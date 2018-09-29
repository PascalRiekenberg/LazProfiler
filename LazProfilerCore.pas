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
  Classes,
  EpikTimer,
  Generics.Collections,
  Generics.Defaults,
  SysUtils;

type

  TIntegerList = specialize TList<Integer>;


  { TLPBaseType }

  TLPBaseType = class
  private
  protected
    fExpanded: Boolean;
  public
    property Expanded: Boolean read fExpanded write fExpanded;
  end;


  { TLPPasUnit }

  TLPPasUnit = class(TLPBaseType)
  private
    procedure SetFileName(pFileName: string);
    procedure SetUnitName(pUnitName: String);
  protected
    fUnitName,
    fFileName,
    fUnitNameUp,
    fFileNameUp: String;
  public
    constructor Create(pUnitName, pFileName: String);
    property UnitName: String read fUnitName write SetUnitName;
    property FileName: string read fFileName write SetFileName;
    property UnitNameUp: String read fUnitNameUp;
    property FileNameUp: String read fFileNameUp;
  end;


  { TLPPasUnitList }

  TLPPasUnitList = class(specialize TObjectList<TLPPasUnit>)
  private
  protected
  public
    function IndexOf(pUnitName, pFileName: String): SizeInt; overload;
    procedure SaveToStringList(pList: TStringList);
    procedure LoadFromStringList(pList: TStringList; pVersion: Integer);
  end;


  { TLPPasClass }

  TLPPasClass = class(TLPPasUnit)
  private
    procedure SetNameOfClass(pNameOfClass: String);
  protected
    fNameOfClass,
    fNameOfClassUp: String;
    fPasUnit: TLPPasUnit;
  public
    constructor Create(pNameOfClass, pUnitName, pFileName: String);
    property PasUnit: TLPPasUnit read fPasUnit write fPasUnit;
    property NameOfClass: String read fNameOfClass write SetNameOfClass;
    property NameOfClassUp: String read fNameOfClassUp;
  end;


  { TLPPasClassList }

  TLPPasClassList = class(specialize TObjectList<TLPPasClass>)
  private
  protected
  public
    function IndexOf(pNameOfClass, pUnitName, pFileName: String): SizeInt; overload;
    procedure SaveToStringList(pList: TStringList);
    procedure LoadFromStringList(pList: TStringList; pVersion: Integer);
  end;


  { TLPProc }

  TLPPasProcList = class;

  { TLPPasProc }

  TLPPasProc = class(TLPPasClass)
  private
    fName,
    fNameUp: String;
    fRow: Integer;
    fNet,
    fGross: QWord;
    fCount: Integer;
    fKind: Integer;
    fCalls,
    fCalledBy: TLPPasProcList;
    fCallsCount,
    fCalledByCount: TIntegerList;
    fInstrument: Boolean;
    fPasClass: TLPPasClass;
    procedure SetFileName(pFileName: String);
    procedure SetName(pName: String);
  public
    constructor Create(pName: String; pKind: Integer; pNameOfClass, pUnitName, pFileName: String; pRow: Integer);
    destructor Destroy; override;
    procedure Init;
    procedure Calls(pProc: TLPPasProc);
    procedure CalledBy(pProc: TLPPasProc);
    property Name: String read fName write SetName;
    property Row: Integer read fRow write fRow;
    property Count: Integer read fCount write fCount;
    property Kind: Integer read fKind write fKind;
    property Net: QWord read fNet write fNet;
    property Gross: QWord read fGross write fGross;
    property Instrument: Boolean read fInstrument write fInstrument;
    property NameUp: string read fNameUp;
    property PasClass: TLPPasClass read fPasClass write fPasClass;
  end;
  PLPPasProc = ^TLPPasProc;


  { TLPPasProcList }

  TLPPasProcList = class(specialize TObjectList<TLPPasProc>)
  private
    fCallCount: Integer;
  public
    constructor Create(pOwnsObjects: Boolean = True); overload;
    procedure Init;
    procedure Convert(pTicks: ticktype);
    function IndexOf(pName, pNameOfClass, pUnitName, pFileName: String): SizeInt;
      overload;
    procedure SaveToStringList(pList: TStringList);
    procedure LoadFromStringList(pList: TStringList; pVersion: Integer);
    property CallCount: Integer read fCallCount write fCallCount;
  end;


  { TLPStackFrame }

  TLPStackFrame = class;
  TLPStackFrameList = specialize TObjectList<TLPStackFrame>;

  TLPStackFrame = class
  private
    fPasProc: TLPPasProc;
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
    constructor Create(pProc: TLPPasProc; pParent: TLPStackFrame);
    destructor Destroy; override;
  end;


  { TCustomLazProfiler }

  TCustomLazProfiler = class
  protected
    fUnitList: TLPPasUnitList;
    fClassList: TLPPasClassList;
    fProcList: TLPPasProcList;
    fAutoStart: Boolean;
    fNeedsRebuild: Boolean;
    fSortColumn: Integer;
    fSortDirection: Integer;
    procedure SaveToFile(pFileName: String);
    function LoadFromFile(pFileName: String): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    property ProcList: TLPPasProcList read fProcList;
    property SortColumn: Integer read fSortColumn write fSortColumn;
    property SortDirection: Integer read fSortDirection write fSortDirection;
  end;


  { TLazProfiler }

  TLazProfiler = class(TCustomLazProfiler)
  private
    fMasterProc: TLPPasProc;
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

  cCoreFileName = 'LazProfilerCore.pas';
  cRunTimeFileName = 'LazProfilerRunTime.pas';
  cTimerFileName = 'EpikTimer.pas';

  cSettingExtension = '.lazprofiler_setting';
  cSettingVersion = 2;

implementation

uses
  LazLogger, LazFileUtils, strutils, Dialogs;

{ TLPPasUnitList }

function TLPPasUnitList.IndexOf(pUnitName, pFileName: String): SizeInt;
var
  i: Integer;
  lPasUnit: TLPPasUnit;
begin
  pUnitName := UpperCase(pUnitName);
  pFileName := UpperCase(pFileName);
  for i := 0 to Count - 1 do begin
    lPasUnit := Items[i];
    if (lPasUnit.UnitNameUp = pUnitName)
    and (lPasUnit.FileNameUp = pFileName) then
      Exit(i);
  end;
  Result := -1;
end;

procedure TLPPasUnitList.SaveToStringList(pList: TStringList);
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
      lLine.Add(Items[i].UnitName);
      lLine.Add(Items[i].FileName);
      lLine.Add(BoolToStr(Items[i].Expanded, True));
      pList.Add(lLine.DelimitedText);
    end;
  finally
    lLine.Free;
  end;
end;

procedure TLPPasUnitList.LoadFromStringList(pList: TStringList; pVersion: Integer);
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
      if lLine.Count >= 3 then begin
        Add(TLPPasUnit.Create(lLine[0], lLine[1]));
        Last.Expanded := StrToBool(lLine[2]);
      end;
    end;
  finally
    lLine.Free;
  end;
end;


{ TLPPasClassList }

function TLPPasClassList.IndexOf(pNameOfClass, pUnitName, pFileName: String): SizeInt;
var
  i: Integer;
  lPasClass: TLPPasClass;
begin
  pNameOfClass := UpperCase(pNameOfClass);
  pUnitName := UpperCase(pUnitName);
  pFileName := UpperCase(pFileName);
  for i := 0 to Count - 1 do begin
    lPasClass := Items[i];
    if (lPasClass.fNameOfClassUp = pNameOfClass)
    and (lPasClass.fUnitNameUp = pUnitName)
    and (lPasClass.fFileNameUp = pFileName) then
      Exit(i);
  end;
  Result := -1;
end;

procedure TLPPasClassList.SaveToStringList(pList: TStringList);
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
      lLine.Add(Items[i].NameOfClass);
      lLine.Add(Items[i].UnitName);
      lLine.Add(Items[i].FileName);
      lLine.Add(BoolToStr(Items[i].Expanded, True));
      pList.Add(lLine.DelimitedText);
    end;
  finally
    lLine.Free;
  end;
end;

procedure TLPPasClassList.LoadFromStringList(pList: TStringList; pVersion: Integer);
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
      if lLine.Count >= 4 then begin
        Add(TLPPasClass.Create(lLine[0], lLine[1], lLine[2]));
        Last.Expanded := StrToBool(lLine[3]);
      end;
    end;
  finally
    lLine.Free;
  end;
end;


{ TLPPasClass }

procedure TLPPasClass.SetNameOfClass(pNameOfClass: String);
begin
  if fNameOfClass = pNameOfClass then Exit;
  fNameOfClass := pNameOfClass;
  fNameOfClassUp := UpperCase(pNameOfClass);
end;

constructor TLPPasClass.Create(pNameOfClass, pUnitName, pFileName: String);
begin
  inherited Create(pUnitName, pFileName);
  fNameOfClass := pNameOfClass;
  fNameOfClassUp := UpperCase(pNameOfClass);
end;


{ TLPPasUnit }

procedure TLPPasUnit.SetFileName(pFileName: string);
begin
  if fFileName = pFileName then Exit;
  fFileName := pFileName;
  fFileNameUp := UpperCase(pFileName);
end;

procedure TLPPasUnit.SetUnitName(pUnitName: String);
begin
  if fUnitName = pUnitName then Exit;
  fUnitName := pUnitName;
  fUnitNameUp := UpperCase(pUnitName);
end;

constructor TLPPasUnit.Create(pUnitName, pFileName: String);
begin
  fUnitName := pUnitName;
  fUnitNameUp := UpperCase(pUnitName);
  fFileName := pFileName;
  fFileNameUp := UpperCase(pFileName);
end;


{ TCustomLazProfiler }

procedure TCustomLazProfiler.SaveToFile(pFileName: String);
var
  lFile,
  lLine: TStringList;
  i: Integer;
begin
  lFile := TStringList.Create;
  lLine := TStringList.Create;
  try
    // header
    lLine.Delimiter := ';';
    lLine.StrictDelimiter := True;
    lLine.Clear;
    lLine.Add(IntToStr(cSettingVersion));
    lLine.Add(ifthen(fAutoStart, 'AutoStart', ''));
    lLine.Add(ifthen(fNeedsRebuild, 'NeedsRebuild', ''));
    lLine.Add(IntToStr(fSortColumn));
    lLine.Add(IntToStr(fSortDirection));
    lLine.Add(IntToStr(fUnitList.Count));
    lLine.Add(IntToStr(fClassList.Count));
    lLine.Add(IntToStr(fProcList.Count));
    lFile.Add(lLine.DelimitedText);
    // units
    fUnitList.SaveToStringList(lFile);
    // classes
    fClassList.SaveToStringList(lFile);
    // procs
    fProcList.SaveToStringList(lFile);
    lFile.SaveToFile(pFileName);
  finally
    lLine.Free;
    lFile.Free;
  end;
end;

function TCustomLazProfiler.LoadFromFile(pFileName: String): Boolean;
var
  lFile, lLine, lTemp: TStringList;
  lVersion, lUnitCount, lClassCount, lProcCount: LongInt;
  lPasClass: TLPPasClass;
  lPasUnit: TLPPasUnit;
  lPasProc: TLPPasProc;
  i, j, p: Integer;
begin
  DebugLn('*** LazProfiler: LoadFromFile: '+pFileName);
  if not FileExists(pFileName) then
    Exit(False);
  Result := True;
  lFile := TStringList.Create;
  lLine := TStringList.Create;
  lTemp := TStringList.Create;
  fProcList.Clear;
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
        if lVersion > 1 then begin
          fSortColumn := StrToInt(lLine[3]);
          fSortDirection := StrToInt(lLine[4]);
          lUnitCount := StrToInt(lLine[5]);
          lClassCount := StrToInt(lLine[6]);
          lProcCount := StrToInt(lLine[7]);
        end;
      end else
        Exit;
      lFile.Delete(0);
      if lVersion = 1 then begin
        if lFile.Count > 0 then
          fProcList.LoadFromStringList(lFile, lVersion);
      end else begin
        p := 0;
        if lUnitCount > 0 then begin;
          lTemp.Clear;
          for i := 0 to lUnitCount - 1 do begin
            lTemp.Add(lFile[p]);
            inc(p);
          end;
          fUnitList.LoadFromStringList(lTemp, lVersion);
        end;
        if lClassCount > 0 then begin;
          lTemp.Clear;
          for i := 0 to lClassCount - 1 do begin
            lTemp.Add(lFile[p]);
            inc(p);
          end;
          fClassList.LoadFromStringList(lTemp, lVersion);
        end;
        if lProcCount > 0 then begin;
          lTemp.Clear;
          for i := 0 to lProcCount - 1 do begin
            lTemp.Add(lFile[p]);
            inc(p);
          end;
          fProcList.LoadFromStringList(lTemp, lVersion);
        end;
      end;
      for i := 0 to fProcList.Count - 1 do begin
        lPasProc := fProcList[i];
        // search unit
        j := fUnitList.IndexOf(lPasProc.UnitName, lPasProc.FileName);
        if j >= 0 then begin
          lPasUnit := fUnitList[j]
        end else begin
          lPasUnit := TLPPasUnit.Create(lPasProc.UnitName, lPasProc.FileName);
          fUnitList.Add(lPasUnit);
        end;
        // search class
        j := fClassList.IndexOf(lPasProc.NameOfClass, lPasProc.UnitName, lPasProc.FileName);
        if j >= 0 then begin
          lPasClass := fClassList[j]
        end else begin
          lPasClass := TLPPasClass.Create(lPasProc.NameOfClass, lPasProc.UnitName, lPasProc.FileName);
          fClassList.Add(lPasClass);
        end;
        // connect
        lPasClass.PasUnit := lPasUnit;
        lPasProc.PasUnit := lPasUnit;
        lPasProc.PasClass := lPasClass;
      end;
    end;
    //DebugLn('   Count='+IntToStr(fProcList.Count));
  finally
    lTemp.Free;
    lLine.Free;
    lFile.Free;
  end;
end;

constructor TCustomLazProfiler.Create;
begin
  inherited Create;
  fClassList := TLPPasClassList.Create(True);
  fUnitList := TLPPasUnitList.Create(True);
  fProcList := TLPPasProcList.Create(True);
  fSortColumn := 0;
  fSortDirection := 1;
end;

destructor TCustomLazProfiler.Destroy;
begin
  fProcList.Free;
  fClassList.Free;
  fUnitList.Free;
  inherited Destroy;
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
  if Assigned(fPasProc) and fRunning then begin
    fPasProc.Count := fPasProc.Count + 1;
    fPasProc.fNet := fPasProc.fNet + fNet - fOff;
    fPasProc.fGross := fPasProc.fGross + fGross - fOverhead - fOff;
  end;
end;

procedure TLPStackFrame.CleanupChilds;
var
  i: Integer;
begin
  for i := 0 to fChildList.Count - 1 do
    fChildList[i].Calc;
end;

constructor TLPStackFrame.Create(pProc: TLPPasProc; pParent: TLPStackFrame);
begin
  fPasProc := pProc;
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


{ TLPPasProcList }

constructor TLPPasProcList.Create(pOwnsObjects: Boolean);
begin
  inherited Create(pOwnsObjects);
  fCallCount := 0;
end;

procedure TLPPasProcList.Init;
var
  i: Integer;
begin
  for i := 0 to Count -1 do
    Items[i].Init;
end;

procedure TLPPasProcList.Convert(pTicks: ticktype);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do begin
    Items[i].Net := trunc(Items[i].Net / pTicks * 1000000000);
    Items[i].Gross := trunc(Items[i].Gross / pTicks * 1000000000);
  end;
end;

procedure TLPPasProcList.SaveToStringList(pList: TStringList);
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
      lLine.Add(IntToStr(Items[i].Kind));
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

function TLPPasProcList.IndexOf(pName, pNameOfClass, pUnitName, pFileName: String): SizeInt;
var
  lPasProc: TLPPasProc;
  i: Integer;
begin
  pName := UpperCase(pName);
  pNameOfClass := UpperCase(pNameOfClass);
  pUnitName := UpperCase(pUnitName);
  pFileName := UpperCase(pFileName);
  for i := 0 to Count - 1 do begin
    lPasProc := Self[i];
    if (lPasProc.NameUp = pName)
    and (lPasProc.NameOfClassUp = pNameOfClass)
    and (lPasProc.UnitNameUp = pUnitName)
    and (lPasProc.FileNameUp = pFileName) then
      Exit(i);
  end;
  Result := -1;
end;

procedure TLPPasProcList.LoadFromStringList(pList: TStringList; pVersion: Integer);
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
        if pVersion = 1 then begin
          Add(TLPPasProc.Create(lLine[0], 0, lLine[1], lLine[2], lLine[3], StrToInt(lLine[4])));
          Last.Count := StrToInt(lLine[5]);
          Last.Net := StrToQWord(lLine[6]);
          Last.Gross := StrToQWord(lLine[7]);
          Last.Instrument := StrToBool(lLine[8]);
        end else begin
          Add(TLPPasProc.Create(lLine[0], StrToInt(lLine[1]), lLine[2], lLine[3], lLine[4], StrToInt(lLine[5])));
          Last.Count := StrToInt(lLine[6]);
          Last.Net := StrToQWord(lLine[7]);
          Last.Gross := StrToQWord(lLine[8]);
          Last.Instrument := StrToBool(lLine[9]);
        end;
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
    if lCurStackFrame.fPasProc <> fProcList[pProcID] then
      ShowMessage('TLazProfiler.ExitProfiling: Stack mismatch: '+lCurStackFrame.fPasProc.Name+'<->'+fProcList[pProcID].name);
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
  fMasterProc := TLPPasProc.Create('Master', 0, '', '', '', -1);
  fOffTicks := 0;
  fThreads := TList.Create;
  lCurStackFrame := TLPStackFrame.Create(fMasterProc, nil);
  lCurStackFrame.fTicksInit := Ticks();
  lCurStackFrame.fTicksStart := lCurStackFrame.fTicksInit;
  SetLength(fCurStackFrame, 100);
  fCurStackFrame[fThreads.Add(Pointer(ThreadID))] := lCurStackFrame;
  fLoaded := LoadFromFile(fName+cSettingExtension);
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
    SaveToFile(fName+cSettingExtension);
  DoneCriticalSection(fLock);
  inherited Destroy;
end;


{ TLPPasProc }

procedure TLPPasProc.Init;
begin
  fCount := 0;
  fNet := 0;
  fGross := 0;
end;

procedure TLPPasProc.SetFileName(pFileName: String);
begin
  if fFileName = pFileName then Exit;
  fFileName := pFileName;
  fFileNameUp := UpperCase(fFileName);
end;

procedure TLPPasProc.SetName(pName: String);
begin
  if fName = pName then Exit;
  fName := pName;
  fNameUp := UpperCase(pName);
end;

constructor TLPPasProc.Create(pName: String; pKind: Integer; pNameOfClass, pUnitName, pFileName: String; pRow: Integer);
begin
  inherited Create(pNameOfClass, pUnitName, pFileName);
  fName := pName;
  fNameUp := UpperCase(pName);
  fRow := pRow;
  fInstrument := True;
  fKind := pKind;
  Init;
end;

destructor TLPPasProc.Destroy;
begin
  fCalls.Free;
  fCalledBy.Free;
  fCallsCount.Free;
  fCalledByCount.Free;
  inherited Destroy;
end;

procedure TLPPasProc.Calls(pProc: TLPPasProc);
var
  i: SizeInt;
begin
  if not Assigned(fCalls) then begin
    fCalls := TLPPasProcList.Create(False);
    fCallsCount := TIntegerList.Create;
  end;
  i := fCalls.IndexOf(pProc);
  if i = -1 then begin
    fCalls.Add(pProc);
    fCallsCount.Add(1);
  end else begin
    fCallsCount[i] := fCallsCount[i] + 1;
  end;
end;

procedure TLPPasProc.CalledBy(pProc: TLPPasProc);
var
  i: SizeInt;
begin
  if not Assigned(fCalledBy) then begin
    fCalledBy := TLPPasProcList.Create(False);
    fCalledByCount := TIntegerList.Create;
  end;
  i := fCalledBy.IndexOf(pProc);
  if i = -1 then begin
    fCalledBy.Add(pProc);
    fCalledByCount.Add(1);
  end else begin
    fCalledByCount[i] := fCalledByCount[i] + 1;
  end;
end;

end.

