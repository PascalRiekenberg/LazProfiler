{
    Copyright (c) 2017 Pascal Riekenberg

    LazProfiler: Core unit

    See the file COPYING.modifiedLGPL.txt, included in this distribution,
    for details about the license.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit LazProfilerCore;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Generics.Collections, Generics.Defaults,
  EpikTimer;

type

  { TLPProc }

  TLPProcList = class;

  TLPProc = class
  private
    fName,
    fNameOfClass,
    fUnitName,
    fFileName: String;
    fRow: Integer;
    fNet,
    fGross: QWord;
    fCount: Integer;
    fCalls,
    fCalledBy: TLPProcList;
    fInstrument: Boolean;
  public
    constructor Create(pName, pNameOfProc, pUnitName, pFileName: String;
      pRow: Integer);
    destructor Destroy; override;
    procedure Init;
    procedure Calls(pProc: TLPProc);
    procedure CalledBy(pProc: TLPProc);
    property Name: String read fName write fName;
    property NameOfClass: String read fNameOfClass write fNameOfClass;
    property UnitName: String read fUnitName write fUnitName;
    property FileName: String read fFileName write fFileName;
    property Row: Integer read fRow write fRow;
    property Count: Integer read fCount write fCount;
    property Net: QWord read fNet write fNet;
    property Gross: QWord read fGross write fGross;
    property Instrument: Boolean read fInstrument write fInstrument;
  end;
  PLPProc = ^TLPProc;

  { TLPProcList }

  TLPCustomProcList = specialize TObjectList<TLPProc>;

  TLPProcList = class(TLPCustomProcList)
  public
    procedure Init;
    procedure Convert(pTicks: ticktype);
    procedure SaveToStringList(pList: TStringList);
    procedure LoadFromStringList(pList: TStringList);
  end;

  { TLPStackFrame }

  TLPStackFrame = class;
  TLPStackFrameList = specialize TObjectList<TLPStackFrame>;

  TLPStackFrame = class
  private
    fProc: TLPProc;
    fChildList: TLPStackFrameList;
    fParent: TLPStackFrame;
    fRunning: Boolean;
    fTicksInit,
    fTicksStart,
    fTicksEnd,
    fTicksExit,
    fNet,
    fGross,
    fOverhead,
    fOff: TickType;
  protected
  public
    procedure Calc;
    procedure CleanupChilds;
    constructor Create(pProc: TLPProc; pParent: TLPStackFrame);
    destructor Destroy; override;
  end;

  { TLPProcUsing }

  TLPProcUsing = class
  private
    fProc: TLPProc;
    fCount: Integer;
  protected
  public
    procedure Inc;
    constructor Create(pProc: TLPProc);
  end;

  { TLPUsingComparer }

  TLPUsingComparer = class(TInterfacedObject, specialize IComparer<TLPProcUsing>)
    function Compare(constref Left, Right: TLPProcUsing): Integer; overload;
  end;

  TLPUsingList = specialize TObjectList<TLPProcUsing>;


  { TCustomLazProfiler }

  TCustomLazProfiler = class
  protected
    fAutoStart: Boolean;
    fNeedsRebuild: Boolean;
    procedure SaveToFile(pFileName: String; pProcList: TLPProcList);
    function LoadFromFile(pFileName: String; pProcList: TLPProcList): Boolean;
  end;

  { TLazProfiler }

  TLazProfiler = class(TCustomLazProfiler)
  private
    fMasterProc: TLPProc;
    fProcList: TLPProcList;
    fName: String;
    fTimer: TEpikTimer;
    fTicks: TickType;
    fThreads: TList;
    fCurStackFrame: array of TLPStackFrame;
    fRunning: Boolean;
    fPauseCount: Integer;
    fPauseStartTicks,
    fOffTicks: TickType;
    fLoaded: Boolean;
    Ticks: function: Ticktype of object;
    fLock: TRTLCriticalSection;
    function GetOffTicks: TickType;
    function HWTicks: Ticktype;
    function SysTicks: Ticktype;
    function ThreadIndex(pThread: TThreadID): Integer;
    procedure Lock;
    procedure UnLock;
  protected
  public
    procedure EnterProfiling(pProcID: Integer);
    procedure ExitProfiling(pProcID: Integer);
    procedure StartProfiling;
    procedure PauseProfiling;
    procedure ContinueProfiling;
    procedure StopProfiling;
    constructor Create(pProgramm: String);
    destructor Destroy; override;
    property OffTicks: TickType read GetOffTicks;
    property PauseStart: TickType read fPauseStartTicks;
  end;

const
  cBackupExtension  = '.lazprofiler_backup';
  cSettingExtension = '.lazprofiler_setting';
  cSettingVersion = 1;
  cCoreFileName = 'LazProfilerCore.pas';
  cRunTimeFileName = 'LazProfilerRunTime.pas';
  cTimerFileName = 'EpikTimer.pas';

implementation

uses
  LazLogger, LazFileUtils, strutils, Dialogs;

{ TCustomLazProfiler }

procedure TCustomLazProfiler.SaveToFile(pFileName: String;
  pProcList: TLPProcList);
var
  lFile,
  lLine: TStringList;
  i: Integer;
begin
  lFile := TStringList.Create;
  lLine := TStringList.Create;
  try
    lLine.Delimiter := ';';
    lLine.StrictDelimiter := True;
    lLine.Clear;
    lLine.Add(IntToStr(cSettingVersion));
    lLine.Add(ifthen(fAutoStart, 'AutoStart', ''));
    lLine.Add(ifthen(fNeedsRebuild, 'NeedsRebuild', ''));
    lFile.Add(lLine.DelimitedText);
    pProcList.SaveToStringList(lFile);
    lFile.SaveToFile(pFileName);
  finally
    lLine.Free;
    lFile.Free;
  end;
end;

function TCustomLazProfiler.LoadFromFile(pFileName: String;
  pProcList: TLPProcList): Boolean;
var
  lFile, lLine: TStringList;
  lVersion: LongInt;
begin
  //DebugLn('   LoadFormFile: '+pFileName);
  if not FileExists(pFileName) then
    Exit(False);
  Result := True;
  lFile := TStringList.Create;
  lLine := TStringList.Create;
  pProcList.Clear;
  try
    lFile.LoadFromFile(pFileName);
    if lFile.Count >= 1 then begin
      lLine.Delimiter := ';';
      lLine.StrictDelimiter := True;
      lLine.DelimitedText := lFile[0];
      if lLine.Count >= 3 then begin
        lVersion := StrToInt(lLine[0]);
        If UpperCase(lLine[1]) = 'AUTOSTART' then
          fAutoStart := True;
        If UpperCase(lLine[2]) = 'NEEDSREBUILD' then
          fNeedsRebuild := True;
      end else
        Exit;
      lFile.Delete(0);
      if lFile.Count > 0 then
        pProcList.LoadFromStringList(lFile);
    end;
    //DebugLn('   Count='+IntToStr(pProcList.Count));
  finally
    lLine.Free;
    lFile.Free;
  end;
end;

{ TLPUsingComparer }

function TLPUsingComparer.Compare(constref Left, Right: TLPProcUsing): Integer;
begin
  if @Left.fProc < @Right.fProc then
    Result := -1
  else if @Left.fProc > @Right.fProc then
    Result := 1
  else
    Result := 0;
end;

{ TLPProcUsing }

procedure TLPProcUsing.Inc;
begin
  fCount := fCount + 1;
end;

constructor TLPProcUsing.Create(pProc: TLPProc);
begin
  fProc := pProc;
  fCount := 1;
end;

{ TLPStackFrame }

procedure TLPStackFrame.Calc;
var
  i: Integer;
begin
  if fNet <> -1 then
    exit;
  { calc times }
  fNet := fTicksEnd - fTicksStart;
  fGross := fTicksExit - fTicksInit;
  fOverhead := fGross - fNet;
  { handle Childs }
  for i := 0 to fChildList.Count - 1 do begin
    fChildList[i].Calc;
    fNet := fNet - fChildList[i].fGross;
    fOverhead := fOverhead + fChildList[i].fOverhead;
  end;
  FreeAndNil(fChildList);
  { store infos }
  if Assigned(fProc) and fRunning then begin
    fProc.Count := fProc.Count + 1;
    fProc.fNet := fProc.fNet + fNet - fOff;
    fProc.fGross := fProc.fGross + fGross - fOverhead - fOff;
  end;
end;

procedure TLPStackFrame.CleanupChilds;
var
  i: Integer;
begin
  for i := 0 to fChildList.Count - 1 do
    fChildList[i].Calc;
end;

constructor TLPStackFrame.Create(pProc: TLPProc; pParent: TLPStackFrame);
begin
  fProc := pProc;
  fParent := pParent;
  if Assigned(fParent) then
    fParent.fChildList.Add(Self);
  fNet := -1;
  fGross := -1;
  fChildList := TLPStackFrameList.Create;
end;

destructor TLPStackFrame.Destroy;
begin
  fChildList.Free;
  inherited Destroy;
end;

{ TLPProcList }

procedure TLPProcList.Init;
var
  i: Integer;
begin
  for i := 0 to Count -1 do
    Items[i].Init;
end;

procedure TLPProcList.Convert(pTicks: TickType);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do begin
    Items[i].Net := trunc(Items[i].Net / pTicks * 1000000000);
    Items[i].Gross := trunc(Items[i].Gross / pTicks * 1000000000);
  end;
end;

procedure TLPProcList.SaveToStringList(pList: TStringList);
var
  lLine: TStringList;
  i: Integer;
begin
  lLine := TStringList.Create;
  try
    lLine.Delimiter := ';';
    lLine.StrictDelimiter := True;
    for i := 0 to Count - 1 do begin
      lLine.Clear;
      lLine.Add(Items[i].Name);
      lLine.Add(Items[i].NameOfClass);
      lLine.Add(Items[i].UnitName);
      lLine.Add(Items[i].FileName);
      lLine.Add(IntToStr(Items[i].Row));
      lLine.Add(IntToStr(Items[i].Count));
      lLine.Add(IntToStr(Items[i].Net));
      lLine.Add(IntToStr(Items[i].Gross));
      lLine.Add(BoolToStr(Items[i].Instrument, True));
      pList.Add(lLine.DelimitedText);
    end;
  finally
    lLine.Free;
  end;
end;

procedure TLPProcList.LoadFromStringList(pList: TStringList);
var
  lLine: TStringList;
  i: Integer;
begin
  lLine := TStringList.Create;
  try
    lLine.Delimiter := ';';
    lLine.StrictDelimiter := True;
    if pList.Count = 0 then
      exit;
    for i := 0 to pList.Count - 1 do begin
      lLine.DelimitedText := pList[i];
      if lLine.Count >= 9 then begin
        Add(TLPProc.Create(lLine[0], lLine[1], lLine[2], lLine[3], StrToInt(lLine[4])));
        Last.Count := StrToInt(lLine[5]);
        Last.Net := StrToQWord(lLine[6]);
        Last.Gross := StrToQWord(lLine[7]);
        Last.Instrument := StrToBool(lLine[8]);
        //if not Last.Instrument then
        //  DebugLn('*** do not instrument '+Last.Name);
      end;
    end;
  finally
    lLine.Free;
  end;
end;

{ TLazProfiler }

function TLazProfiler.GetOffTicks: TickType;
begin
  if fPauseCount = 0 then begin
    Result := fOffTicks;
    fOffTicks := 0;
  end else
    Result := 0;
end;

function TLazProfiler.HWTicks: Ticktype;
begin
  Result := fTimer.HWTimebase.Ticks() - fTimer.HWTimebase.TicksOverhead;
end;

function TLazProfiler.SysTicks: Ticktype;
begin
  Result := fTimer.SysTimebase.Ticks() - fTimer.SysTimebase.TicksOverhead;
end;

function TLazProfiler.ThreadIndex(pThread: TThreadID): Integer;
var
  lCurStackFrame: TLPStackFrame;
begin
  Result := fThreads.IndexOf(Pointer(pThread));
  if Result = -1 then begin
    Result := fThreads.Add(Pointer(pThread));
    if fThreads.Count > Length(fCurStackFrame) then
      SetLength(fCurStackFrame, fThreads.Count + 100);
    lCurStackFrame := TLPStackFrame.Create(fMasterProc, nil);
    lCurStackFrame.fTicksInit := Ticks();
    lCurStackFrame.fTicksStart := lCurStackFrame.fTicksInit;
    fCurStackFrame[Result] := lCurStackFrame;
  end;
end;

procedure TLazProfiler.Lock;
begin
  EnterCriticalsection(fLock);
end;

procedure TLazProfiler.UnLock;
begin
  LeaveCriticalsection(fLock);
end;

procedure TLazProfiler.EnterProfiling(pProcID: Integer);
var
  lTimeStamp: TickType;
  lCurStackFrame: TLPStackFrame;
  lIdx: Integer;
begin
  lTimeStamp := Ticks();
  Lock;
  try
    if pProcID >= fProcList.Count then begin
      ShowMessage('TLazProfiler.EnterProfiling: pProcID >= fProcList.Count: '+IntToStr(pProcID)+'>='+IntToStr(fProcList.Count));
      Exit;
    end;
    lIdx := ThreadIndex(ThreadID);
    lCurStackFrame := fCurStackFrame[lIdx];
    lCurStackFrame := TLPStackFrame.Create(fProcList[pProcID], lCurStackFrame);
    fCurStackFrame[lIdx] := lCurStackFrame;
    lCurStackFrame.fRunning := fRunning;
    lCurStackFrame.fTicksInit := lTimeStamp;
  finally
    UnLock;
  end;
  lCurStackFrame.fTicksStart := Ticks();
end;

procedure TLazProfiler.ExitProfiling(pProcID: Integer);
var
  lTimeStamp: TickType;
  lCurStackFrame: TLPStackFrame;
  lIdx: Integer;
begin
  lTimeStamp := Ticks();
  Lock;
  try
    lIdx := ThreadIndex(ThreadID);
    lCurStackFrame := fCurStackFrame[lIdx];
    lCurStackFrame.fTicksEnd := lTimeStamp;
    if pProcID >= fProcList.Count then begin
      ShowMessage('TLazProfiler.ExitProfiling: pProcID >= fProcList.Count: '+IntToStr(pProcID)+'>='+IntToStr(fProcList.Count));
      Exit;
    end;
    if lCurStackFrame.fProc <> fProcList[pProcID] then
      ShowMessage('TLazProfiler.ExitProfiling: Stack mismatch: '+lCurStackFrame.fProc.Name+'<->'+fProcList[pProcID].name);
    lCurStackFrame.fOff := OffTicks;
    lCurStackFrame.CleanupChilds;
    fCurStackFrame[lIdx] := lCurStackFrame.fParent;
  finally
    UnLock;
  end;
  lCurStackFrame.fTicksExit := Ticks();
end;

procedure TLazProfiler.StartProfiling;
begin
  fRunning := True;
  DebugLn('### LazProfiler: Start');
end;

procedure TLazProfiler.PauseProfiling;
begin
  if fPauseCount = 0 then
    fPauseStartTicks := Ticks();
  inc(fPauseCount);
end;

procedure TLazProfiler.ContinueProfiling;
begin
  dec(fPauseCount);
  if fRunning then begin
    if fPauseCount = 0 then begin
      if fPauseStartTicks <> 0 then
        fOffTicks := fOffTicks + (Ticks() - fPauseStartTicks)
      else
        fOffTicks := 0;
      fPauseStartTicks := 0;
    end;
  end else begin
    fOffTicks := 0;
    fPauseStartTicks := 0;
  end;
end;

procedure TLazProfiler.StopProfiling;
begin
  fRunning := False;
  DebugLn('### LazProfiler: Stop');
end;

constructor TLazProfiler.Create(pProgramm: String);
var
  lCurStackFrame: TLPStackFrame;
begin
  inherited Create;
  InitCriticalSection(fLock);
  fName := AppendPathDelim(ExtractFileDir(pProgramm))+ExtractFileNameOnly(pProgramm);
  fProcList := TLPProcList.Create(True);
  fTimer := TEpikTimer.Create(nil);
  if fTimer.HWCapabilityDataAvailable and fTimer.HWTickSupportAvailable then begin
    Ticks := @HWTicks;
    fTicks := fTimer.HWTimebase.TicksFrequency;
    DebugLn('### LazProfiler: HWTicks used. Freq='+IntToStr(fTicks));
  end else begin
    Ticks := @SysTicks;
    fTicks := fTimer.SysTimebase.TicksFrequency;
    DebugLn('### LazProfiler: SysTicks used. Freq='+IntToStr(fTicks));
  end;
  fMasterProc := TLPProc.Create('Master', '', '', '', -1);
  fOffTicks := 0;
  fThreads := TList.Create;
  lCurStackFrame := TLPStackFrame.Create(fMasterProc, nil);
  lCurStackFrame.fTicksInit := Ticks();
  lCurStackFrame.fTicksStart := lCurStackFrame.fTicksInit;
  SetLength(fCurStackFrame, 100);
  fCurStackFrame[fThreads.Add(Pointer(ThreadID))] := lCurStackFrame;
  fLoaded := LoadFromFile(fName+cSettingExtension, fProcList);
  fRunning := fAutoStart;
end;

destructor TLazProfiler.Destroy;
var
  i: Integer;
  lCurStackFrame: TLPStackFrame;
begin
  fMasterProc.Free;
  for i := 0 to fThreads.Count - 1 do begin
    lCurStackFrame := fCurStackFrame[i];
    lCurStackFrame.fTicksEnd := Ticks();
    lCurStackFrame.fTicksExit := lCurStackFrame.fTicksEnd;
    lCurStackFrame.Calc;
    lCurStackFrame.Free;
  end;
  { calc percentages }

  { free resources }
  fThreads.Free;
  SetLength(fCurStackFrame, 0);
  fTimer.Free;
  if fTicks > 0 then
    fProcList.Convert(fTicks)
  else
    DebugLn('***** LazProfiler: fTicks='+IntToStr(fTicks));
  if fLoaded then
    SaveToFile(fName+cSettingExtension, fProcList);
  fProcList.Free;
  DoneCriticalSection(fLock);
  inherited Destroy;
end;

{ TLPProc }

procedure TLPProc.Init;
begin
  fCount := 0;
  fNet := 0;
  fGross := 0;
end;

constructor TLPProc.Create(pName, pNameOfProc, pUnitName, pFileName: String; pRow: Integer
  );
begin
  fName := pName;
  fNameOfClass := pNameOfProc;
  fUnitName := pUnitName;
  fFileName := pFileName;
  fRow := pRow;
  fInstrument := True;
  Init;
end;

destructor TLPProc.Destroy;
begin
  fCalls.Free;
  fCalledBy.Free;
  inherited Destroy;
end;

procedure TLPProc.Calls(pProc: TLPProc);
begin
  if not Assigned(fCalls) then
    fCalls := TLPProcList.Create(False);
end;

procedure TLPProc.CalledBy(pProc: TLPProc);
begin
  if not Assigned(fCalledBy) then
    fCalledBy := TLPProcList.Create(False);
end;

end.

