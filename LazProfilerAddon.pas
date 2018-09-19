{
    Copyright (c) 2017 Pascal Riekenberg

    LazProfiler: IDE Addon

    See the file COPYING.modifiedLGPL.txt, included in this distribution,
    for details about the license.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit LazProfilerAddon;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, LazIDEIntf, ProjectIntf, MenuIntf, LazProfilerCore;

type

  { TProfilerAddon }

  TProfilerAddon = class(TCustomLazProfiler)
  private
    fProfiling: Boolean;
    fSourcesInstrumented: Boolean;
    fFileList: TStringList;
    fProjectDir: String;
    fIncludePath: String;
    fTargetDir: String;
    fTargetName: String;
    fProcList,
    fOldProcList: TLPProcList;
    procedure BuildFileList(pExtensionMask: String);
    function AddProc(pName, pNameOfClass, pUnitName, pFileName: String;
      pRow: Integer): Integer;
  protected
    function ParseSources(pInstrument: Boolean): Boolean;
    function ParseSource(pFileName: String; pInstrument: Boolean): Boolean;
    procedure RestoreSources;
    procedure RestoreSource(pFileName: String);
    function ProjectBuilding(Sender: TObject): TModalResult;
    procedure ProjectBuildingFinished(Sender: TObject; BuildSuccessful: Boolean);
    procedure RunFinished(Sender: TObject);
    procedure LoadResult;
    function ProjectOpened(Sender: TObject; AProject: TLazProject): TModalResult;
    function ProjectClose(Sender: TObject; AProject: TLazProject): TModalResult;
    procedure CreateProfilerWindow(Sender: TObject; aFormName: string; var AForm: TCustomForm; DoDisableAutoSizing: boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Start(Sender: TObject);
    procedure ShowProfilerWindow(Sender: TObject);
    procedure CleanUp(Sender: TObject);
  end;

var
  Addon: TProfilerAddon;

procedure Register;

implementation

uses
  Controls, LazLogger, CompOptsIntf, IDEImagesIntf, Graphics, MacroIntf, CodeToolManager,
  LazFileUtils, PScanner,
  Generics.Collections,
  Dialogs, strutils, IDEMsgIntf,
  IDEExternToolIntf, LResources, IDEWindowIntf, LazProfilerWindow;

type

  { TBlockEntry }

  TBlockEntry = class
  public
    token: TToken;
    name, nameofclass: String;
    level: Integer;
    parent: TBlockEntry;
    gap: String;
    procid: Integer;
    constructor Create(pToken: TToken; pName: String; pLevel: Integer; pParent: TBlockEntry);
    destructor Destroy; override;
  end;

  TBlocList = specialize TObjectStack<TBlockEntry>;

var
  ProfilerWindow: TLazProfilerForm = nil;

const
  cProfilerFormName = 'LazProfilerForm';
  cAutoRestore = True;


procedure Register;
var
  lItem: TIDEMenuCommand;
  lIdx: Integer;
begin
  lItem := TIDEMenuCommand.Create('itmRunMenuRunWithProfiler');
  with lItem do begin
    Caption := 'Profile';
    OnClick := @Addon.Start;
  end;
  lIdx := itmRunnning.IndexByName('itmRunMenuRun');
  //IDEImages.Images_16.Add(TBitmap.Create.LoadFromStream(), nil);
  //lItem.ImageIndex := IDEImages.LoadImage('menu_profile');

  itmRunnning.Insert(lIdx + 1, lItem);

  RegisterIDEMenuCommand(itmViewMainWindows, 'itmViewMainWindowsProfiler','Profiler Results',@Addon.ShowProfilerWindow, nil);
  RegisterIDEMenuCommand(itmRunnning, 'itmRunMenuProfilerCleanUp','Cleanup Profiler and restore original files',@Addon.CleanUp, nil);
  LazarusIDE.AddHandlerOnProjectBuilding(@Addon.ProjectBuilding);
  LazarusIDE.AddHandlerOnProjectBuildingFinished(@Addon.ProjectBuildingFinished);
  LazarusIDE.AddHandlerOnRunFinished(@Addon.RunFinished); { needs trunk 56254 }
  LazarusIDE.AddHandlerOnProjectOpened(@Addon.ProjectOpened);
  LazarusIDE.AddHandlerOnProjectClose(@Addon.ProjectClose);
  IDEWindowCreators.Add(cProfilerFormName, nil, @Addon.CreateProfilerWindow, '100', '10%', '+300', '+50%');
end;

{ TProc }

constructor TBlockEntry.Create(pToken: TToken; pName: String;
  pLevel: Integer; pParent: TBlockEntry);
var
  i: Integer;
begin
  token := pToken;
  name := pName;
  nameofclass := '';
  level := pLevel;
  parent := pParent;
  gap := '';
  for i := 0 to level do
    gap := gap + '   ';
end;

destructor TBlockEntry.Destroy;
begin
  name := '';
  gap := '';
  inherited Destroy;
end;

{ TProfilerAddon }

procedure TProfilerAddon.BuildFileList(pExtensionMask: String);
var
  lFPCSrcDir, lLazSrcDir: String;
  lPathList: TStringList;
  lLengthFPCSrcDir, lLengthLazSrcDir: Integer;

  procedure ScanDir(pDir, pExtensionMask: String);
  var
    Info: TSearchRec;
    ExtensionList: TStrings;
  begin
    pDir := AppendPathDelim(pDir);

    ExtensionList := TStringList.Create;
    ExtensionList.Delimiter := ';';
    ExtensionList.StrictDelimiter := True;
    ExtensionList.DelimitedText := pExtensionMask;

    if FindFirst(pDir + AllFilesMask, faAnyFile and faDirectory, Info) = 0 then
    begin
      repeat
        if ((Info.Attr and faDirectory) = faDirectory) and (Info.Name <> '.') and (Info.Name <> '..') then
          FileSearch(pDir + Info.Name, pExtensionMask);

        if ExtensionList.IndexOf(ExtractFileExt(Info.Name)) <> -1 then
        begin
          if fFileList.IndexOf(pDir + Info.Name) = -1 then
            fFileList.Add(pDir + Info.Name);
        end;
      until FindNext(Info) <> 0;
    end;
    FindClose(Info);

    ExtensionList.Free;
  end;

  procedure CheckDir(pList: TStringList);
  var
    lPath, lPathUp: String;
    i: Integer;
  begin
    for i := 0 to pList.Count - 1 do begin
      lPath := lPathList[i];
      lPathUp := UpperCase(lPath);
      if (lPathUp <> '')
      and (LeftStr(lPathUp, lLengthFPCSrcDir) <> lFPCSrcDir)
      and (LeftStr(lPathUp, lLengthLazSrcDir) <> lLazSrcDir) then
        ScanDir(lPath, pExtensionMask)
    end;
  end;

begin
  fFileList.Clear;
  { get FPC source dir }
  lFPCSrcDir := '$(FPCSrcDir)';
  IDEMacros.SubstituteMacros(lFPCSrcDir);
  lFPCSrcDir := UpperCase(lFPCSrcDir);
  lLengthFPCSrcDir := length(lFPCSrcDir);
  { get FPC source dir }
  lLazSrcDir := '$(LazarusDir)';
  IDEMacros.SubstituteMacros(lLazSrcDir);
  lLazSrcDir := UpperCase(lLazSrcDir);
  lLengthLazSrcDir := length(lLazSrcDir);

  { scan pathes }
  lPathList := TStringList.Create;
  try
    lPathList.Delimiter := ';';
    lPathList.StrictDelimiter := True;
    { scan includes}
    lPathList.DelimitedText := CodeToolBoss.GetIncludePathForDirectory('');
    CheckDir(lPathList);
    { scan includes}
    lPathList.DelimitedText := CodeToolBoss.GetUnitPathForDirectory('');
    CheckDir(lPathList);
  finally
    lPathList.Free;
  end;
end;

function TProfilerAddon.AddProc(pName, pNameOfClass, pUnitName, pFileName: String; pRow: Integer
  ): Integer;
var
  lProc: TLPProc;
  lUpName, lUpFileName, lUpNameOfClass: String;
  i: Integer;
begin
  lProc := nil;
  lUpName := UpperCase(pName);
  lUpNameOfClass := UpperCase(pNameOfClass);
  lUpFileName := UpperCase(pFileName);
  for i := 0 to fOldProcList.Count - 1 do begin
    if (lUpName = UpperCase(fOldProcList[i].Name))
    and (lUpName = UpperCase(fOldProcList[i].Name))
    and (lUpFileName = UpperCase(fOldProcList[i].Filename)) then begin
      lProc := fOldProcList.ExtractIndex(i);
      lProc.Name := pName;
      lProc.NameOfClass := pNameOfClass;
      lProc.UnitName := pUnitName;
      lProc.FileName := pFileName;
      lProc.Row := pRow;
      lProc.Init;
      break;
    end;
  end;
  if Assigned(lProc) then
    Result := fProcList.Add(lProc)
  else begin
    lProc := TLPProc.Create(pName, pNameOfClass, pUnitName, pFileName, pRow);
    Result := fProcList.Add(lProc);
  end;
  if not lProc.Instrument then
    { do not instrument }
    Result := -1;
end;

procedure TProfilerAddon.LoadResult;
begin
  LoadFromFile(fTargetDir + fTargetName + cSettingExtension, fProcList);
  if fProcList.Count = 0 then begin
    ParseSources(False);
  end;
  if not Assigned(ProfilerWindow) then begin
    IDEWindowCreators.CreateForm(ProfilerWindow, TLazProfilerForm, False, Application);
    if Assigned(IDEDockMaster) then
      IDEDockMaster.MakeIDEWindowDockable(ProfilerWindow);
  end;
  ProfilerWindow.Data := fProcList;
end;

function TProfilerAddon.ParseSources(pInstrument: Boolean): Boolean;
var
  i: Integer;
  lLRS: TLazarusResourceStream;
  lList: TLPProcList;
begin
  Result := True;
  RestoreSources;
  DebugLn('*** LazProfiler: InstrumentSources');

  lList := fOldProcList;
  fOldProcList := fProcList;
  fProcList := lList;
  fProcList.Clear;

  BuildFileList('.pp;.pas;.inc;.lpr');

  fAutoStart := True;
  fFileList.Sort;
  for i := 0 to fFileList.Count - 1 do
    if not ParseSource(fFileList[i], pInstrument) then
      Result := False;

  if pInstrument then begin
    fSourcesInstrumented := True;
    LazarusIDE.ActiveProject.Flags := LazarusIDE.ActiveProject.Flags + [pfAlwaysBuild];
    SaveToFile(fTargetDir + fTargetName + cSettingExtension, fProcList);
    { create LazProfiler units }
    lLRS := TLazarusResourceStream.Create('core', nil);
    try
      lLRS.SaveToFile(fProjectDir + cCoreFileName);
    except
      lLRS.Free;
    end;
    lLRS := TLazarusResourceStream.Create('rt', nil);
    try
      lLRS.SaveToFile(fProjectDir + cRunTimeFileName);
    except
      lLRS.Free;
    end;
    lLRS := TLazarusResourceStream.Create('timer', nil);
    try
      lLRS.SaveToFile(fProjectDir + cTimerFileName);
    except
      lLRS.Free;
    end;
  end;
end;

function TProfilerAddon.ParseSource(pFileName: String; pInstrument: Boolean
  ): Boolean;
var
  sl, lIncludeDirs: TStringList;
  fr: TFileResolver;
  pas: TPascalScanner;
  token, lPart: TToken;
  level, lProfilingChangeLevel, i: Integer;
  lImpUsesPos, lIntUsesPos, lImpPos, lIntPos: TPoint;
  lBlockStack: TBlocList;
  lBlock, lTempBlock, lStartProc: TBlockEntry;
  lName, lParents, lUpComment, lNameOfClass, lUnitName, lIncludePath: String;
  lProfiling: Boolean;
  lPos: SizeInt;

  procedure InsertEnter(pRow, pCol: Integer; pProcID: Integer);
  var
    lLine: String;
  begin
    lLine := sl.Strings[pRow];
    Insert(' try LazProfiler.EnterProfiling(' + IntToStr(pProcID) + ');', lLine, pCol);
    sl.Strings[pRow] := lLine;
  end;

  procedure InsertExit(pRow, pCol: Integer; pProcID: Integer);
  var
    lLine: String;
  begin
    lLine := sl.Strings[pRow];
    Insert('finally LazProfiler.ExitProfiling(' + IntToStr(pProcID) + '); end; ', lLine, pCol);
    sl.Strings[pRow] := lLine;
  end;

  procedure InsertStart(pRow, pCol: Integer);
  var
    lLine: String;
  begin
    lLine := sl.Strings[pRow];
    Insert('LazProfiler.StartProfiling; ', lLine, pCol);
    sl.Strings[pRow] := lLine;
  end;

  procedure InsertStop(pRow, pCol: Integer);
  var
    lLine: String;
  begin
    lLine := sl.Strings[pRow];
    Insert('LazProfiler.StopProfiling; ', lLine, pCol);
    sl.Strings[pRow] := lLine;
  end;

  procedure InsertUnit(pRow, pCol: Integer);
  var
    lLine: String;
  begin
    lLine := sl.Strings[pRow];
    Insert(' LazProfilerRunTime, ', lLine, pCol);
    sl.Strings[pRow] := lLine;
  end;

  procedure InsertUses(pRow, pCol: Integer);
  begin
    sl.Insert(pRow, 'Uses LazProfilerRunTime;');
  end;

  function InStruct: Boolean;
  var
    b: TBlockEntry;
  begin
    Result := False;
    b := lBlock;
    while Assigned(b)
    and (Result = False) do begin
      if b.token in [tkclass, tkobject, tkrecord] then
        Result := True;
      b := b.parent;
    end;
  end;

  procedure NewBlock(pToken: TToken; pName: String);
  begin
    inc(level);
    if Assigned(lBlock) then
      lBlockStack.Push(lBlock);
    lBlock := TBlockEntry.Create(pToken, pName, level, lBlock);
  end;

  procedure PopBlock;
  begin
    if Assigned(lBlock) then begin
      lBlock.Free;
      if lBlockStack.Count > 0 then
        lBlock := lBlockStack.Pop
      else
        lBlock := nil;
      dec(level);
    end;
  end;

  procedure ClearStack;
  begin
    while Assigned(lBlock) do
      PopBlock;
  end;

  procedure DoProc;
  begin
    if not (lPart = tkinterface)
    and not InStruct then begin
      NewBlock(pas.CurToken, pas.CurTokenString);
      while pas.FetchToken <> tkIdentifier do;
      lNameOfClass := '';
      lName := pas.CurTokenString;
      while pas.FetchToken in [tkIdentifier, tkDot] do case pas.CurToken of
        tkIdentifier:
          lName := lName + pas.CurTokenString;
        else begin
          if lNameOfClass = '' then begin
            lNameOfClass := lName;
            lName := '';
          end
          else
            lName := lName + '.'
        end;
      end;
      lBlock.name := lName;
      lBlock.nameofclass := lNameOfClass;
    end;
  end;

begin
  Result := True;
  DebugLn('  '+pFileName);
  sl := TStringList.Create;
  try
    fr := TFileResolver.Create;
    lIncludeDirs := TStringList.Create;
    try
      lIncludeDirs.Delimiter := ';';
      lIncludePath := fIncludePath;
      IDEMacros.SubstituteMacros(lIncludePath);
      lIncludeDirs.DelimitedText := lIncludePath;
      for i := 0 to lIncludeDirs.Count - 1 do
        fr.AddIncludePath(lIncludeDirs[i]);
    finally
      lIncludeDirs.Free;
    end;
    pas := TPascalScanner.Create(fr);
    pas.AddDefine('fpc', true);
    lBlockStack := TBlocList.Create(False);
    try
      { parse and modify }
      lImpUsesPos := Point(-1, -1);
      lIntUsesPos := Point(-1, -1);
      lImpPos := Point(-1, -1);
      lIntPos := Point(-1, -1);
      level := 0;
      lPart := tkEOF;
      lBlock := nil;
      lStartProc := nil;
      lProfiling := True;
      lProfilingChangeLevel := 0;
      sl.LoadFromFile(pFileName);
      pas.OpenFile(pFileName);
      while True do case pas.FetchToken of
        tkAsm: if Assigned(lBlock)
        and not (lBlock.token in [tkprocedure, tkfunction]) then
          NewBlock(pas.CurToken, '');
        tkBegin:
        begin
          if Assigned(lBlock)
          and (lBlock.token in [tkprocedure, tkfunction]) then begin
            if lProfiling then begin
              { insert: lazprofiler.enter() }
              lName := lBlock.name;
              lNameOfClass := lBlock.nameofclass;
              lTempBlock := lBlock.parent;
              if Assigned(lTempBlock) and (lTempBlock.token in [tkprocedure, tkfunction]) then begin
                lParents := 'in '+lTempBlock.name;
                lNameOfClass := lTempBlock.nameofclass;
                lTempBlock := lTempBlock.parent;
                while Assigned(lTempBlock) do begin
                  lParents := lTempBlock.name + ' in ' + lParents;
                  lNameOfClass := lTempBlock.nameofclass;
                  lTempBlock := lTempBlock.parent;
                end;
                lName := lName +' (' + lParents + ')';
              end;
              //DebugLn({lBlock.gap + }'-> ' + lName + ' ' + IntToStr(level));
              lBlock.procid := AddProc(lName, lNameOfClass, lUnitName, pFileName, pas.CurRow - 1);
              if lBlock.procid <> -1 then
                InsertEnter(pas.CurRow - 1, pas.CurColumn, lBlock.procid);
            end;
          end;
          NewBlock(pas.CurToken, '');
        end;
        tkCase: if not Assigned(lBlock)
        or not (lBlock.token in [tkclass, tkobject, tkrecord]) then begin
          NewBlock(pas.CurToken, '');
        end;
        tkEnd:
        begin
          PopBlock;
          if Assigned(lBlock)
          and (lBlock.token in [tkprocedure, tkfunction]) then begin
            if lProfiling then begin
              { insert lazprofiler.exit }
              //DebugLn('      ' + lBlock.Name + ' ' + IntToStr(level));
              if lBlock.procid <> -1 then begin
                InsertExit(pas.CurRow - 1, pas.CurTokenPos.Column, lBlock.procid); // CurTokenPos needs FPC trunk 37235
                if lBlock = lStartProc then begin
                  IDEMessagesWindow.AddCustomMessage(mluFatal, 'LazProfiler: Profiler not stopped in ' + lStartProc.name, pFileName, pas.CurRow, pas.CurTokenPos.Column); // CurTokenPos needs FPC trunk 37235
                  exit(False);
                end;
              end;
            end;
            PopBlock;
          end;
        end;
        tktry:
          NewBlock(pas.CurToken, '');
        tkFunction,
        tkProcedure:
          DoProc;
        tkidentifier:
          if UpperCase(pas.CurTokenString) = 'EXTERNAL' then
            PopBlock;
        tkclass,
        tkobject,
        tkRecord: begin
          NewBlock(pas.CurToken, '');
          while pas.FetchToken in [tkTab, tkWhitespace, tkComment, tkLineEnding] do;
          if pas.CurToken in [tkprocedure, tkfunction] then begin
            { class function/procedure }
            PopBlock;
            DoProc;
          end;
        end;
        tkuses: begin
          case lPart of
            tkinterface:
              lIntUsesPos := Point(pas.CurColumn, pas.CurRow - 1);
            else
              { tkimplementation or tkUnknown }
              lImpUsesPos := Point(pas.CurColumn, pas.CurRow - 1);
          end;
        end;
        tkinterface: begin
          lIntPos := Point(0, pas.CurRow);
          lPart := tkinterface;
        end;
        tkimplementation: begin
          ClearStack;
          lImpPos := Point(0, pas.CurRow);
          lPart := tkimplementation;
        end;
        tkComment: begin
          lUpComment := UpperCase(Trim(pas.CurTokenString));
          if lUpComment = 'START-PROFILER' then begin
            if not lProfiling then
              Continue;
            if Assigned(lStartProc) then begin
              IDEMessagesWindow.AddCustomMessage(mluFatal, 'LazProfiler: START-PROFILER: Profiler already started', pFileName, pas.CurRow, pas.CurTokenPos.Column); // CurTokenPos needs FPC trunk 37235
              exit(False);
            end else begin
              lTempBlock := lBlock;
              while Assigned(lTempBlock) and not (lTempBlock.token in [tkprocedure, tkfunction]) do lTempBlock := lTempBlock.parent;
              if Assigned(lTempBlock) then begin
                lStartProc := lTempBlock;
                InsertStart(pas.CurRow - 1, pas.CurTokenPos.Column); // CurTokenPos needs FPC trunk 37235
                fAutoStart := False;
              end else begin
                IDEMessagesWindow.AddCustomMessage(mluFatal, 'LazProfiler: START-PROFILER: not in procedure/function', pFileName, pas.CurRow, pas.CurTokenPos.Column); // CurTokenPos needs FPC trunk 37235
                exit(False);
              end;
            end;
          end else if lUpComment = 'STOP-PROFILER' then begin
            if not lProfiling then
              Continue;
            if not Assigned(lStartProc) then begin
              IDEMessagesWindow.AddCustomMessage(mluFatal, 'LazProfiler: STOP-PROFILER: Profiler not started', pFileName, pas.CurRow, pas.CurTokenPos.Column); // CurTokenPos needs FPC trunk 37235
              exit(False);
            end else begin
              lTempBlock := lBlock;
              while Assigned(lTempBlock) and not (lTempBlock.token in [tkprocedure, tkfunction]) do lTempBlock := lTempBlock.parent;
              if Assigned(lTempBlock) then begin
                if lTempBlock = lStartProc then begin
                  InsertStop(pas.CurRow - 1, pas.CurTokenPos.Column);  // CurTokenPos needs FPC trunk 37235
                  lStartProc := nil;
                end else begin
                  IDEMessagesWindow.AddCustomMessage(mluFatal, 'LazProfiler: STOP-PROFILER: Profiler not started in same procedure/function', pFileName, pas.CurRow, pas.CurTokenPos.Column); // CurTokenPos needs FPC trunk 37235
                  exit(False);
                end;
              end else begin
                IDEMessagesWindow.AddCustomMessage(mluFatal, 'LazProfiler: STOP-PROFILER: not in procedure/function', pFileName, pas.CurRow, pas.CurTokenPos.Column); // CurTokenPos needs FPC trunk 37235
                exit(False);
              end;
            end;
          end else if lUpComment = 'PROFILER-NO' then begin
            if not lProfiling then begin
              IDEMessagesWindow.AddCustomMessage(mluFatal, 'LazProfiler: PROFILER-NO: set twice', pFileName, pas.CurRow, pas.CurTokenPos.Column); // CurTokenPos needs FPC trunk 37235
              exit(False);
            end;
            lProfiling := False;
            lProfilingChangeLevel := level;
          end else if lUpComment = 'PROFILER-YES' then begin
            if lProfiling then begin
              IDEMessagesWindow.AddCustomMessage(mluFatal, 'LazProfiler: PROFILER-YES: set twice', pFileName, pas.CurRow, pas.CurTokenPos.Column); // CurTokenPos needs FPC trunk 37235
              exit(False);
            end;
            if lProfilingChangeLevel <> level then begin
              IDEMessagesWindow.AddCustomMessage(mluFatal, 'LazProfiler: PROFILER-YES: not set on same level as PROFILER-NO', pFileName, pas.CurRow, pas.CurTokenPos.Column); // CurTokenPos needs FPC trunk 37235
              exit(False);
            end;
            lProfiling := True
          end;
        end;
        tkunit, tkprogram: begin
          while pas.FetchToken <> tkIdentifier do;
          lUnitName := pas.CurTokenString;
        end;
        tkEOF:
          break;
      end;
    finally
      lBlockStack.Free;
      pas.Free;
      fr.Free;
    end;

    if not pInstrument then
      exit;

    { insert uses }
    if UpperCase(ExtractFileExt(pFileName)) <> '.INC' then begin
      if lImpUsesPos <> Point(-1, -1) then
        InsertUnit(lImpUsesPos.y, lImpUsesPos.x)
      else if lIntUsesPos <> Point(-1, -1) then
        InsertUnit(lIntUsesPos.y, lIntUsesPos.x)
      else if lImpPos <> Point(-1, -1) then
        InsertUses(lIntPos.y, lIntPos.x)
      else if lIntPos <> Point(-1, -1) then
        InsertUses(lIntPos.y, lIntPos.x)
      else
        InsertUses(0, 0);
    end;
    if FileExists(pFileName+cBackupExtension) then begin
      IDEMessagesWindow.AddCustomMessage(mluFatal, 'LazProfiler: backup file already exists: '+pFileName+cBackupExtension, pFileName, pas.CurRow, pas.CurTokenPos.Column); // CurTokenPos needs FPC trunk 37235
      exit(False);
    end else begin
      { make backup }
      RenameFile(pFileName, pFileName+cBackupExtension);
      if FileExists(pFileName) then begin
        IDEMessagesWindow.AddCustomMessage(mluFatal, 'LazProfiler: could not create backup file: '+pFileName+cBackupExtension, pFileName, pas.CurRow, pas.CurTokenPos.Column); // CurTokenPos needs FPC trunk 37235
        exit(False);
      end else begin
        { write modified source }
        sl.SaveToFile(pFileName);
      end;
    end;
  finally
    sl.Free;
  end;
end;

procedure TProfilerAddon.RestoreSources;
var
  i: Integer;
begin
  DebugLn('*** LazProfiler: RestoreSources');

  { delete LazProfiler units }
  DeleteFile(fProjectDir + cRunTimeFileName);
  DeleteFile(fProjectDir + cCoreFileName);
  DeleteFile(fProjectDir + cTimerFileName);

  BuildFileList(cBackupExtension);

  fFileList.Sort;
  for i := 0 to fFileList.Count - 1 do
    RestoreSource(fFileList[i]);

  fSourcesInstrumented := False;
  LazarusIDE.ActiveProject.Flags := LazarusIDE.ActiveProject.Flags - [pfAlwaysBuild];
end;

procedure TProfilerAddon.RestoreSource(pFileName: String);
var
  lOrigFileName: String;
begin
  DebugLn('  '+pFileName);
  lOrigFileName := LeftStr(pFileName, length(pFileName) - length(cBackupExtension));
  if FileExists(pFileName) then begin
    DeleteFile(lOrigFileName);
    RenameFile(pFileName, lOrigFileName);
    //todo: touch file here to force rebuild
  end;
end;

function TProfilerAddon.ProjectBuilding(Sender: TObject): TModalResult;
begin
  Result := mrOK;
  if fProfiling then begin
    // disable CheckFilesOnDisk
    LazarusIDE.CheckFilesOnDiskEnabled := False; { needs trunk 56204 }
    if not ParseSources(True) then
      Result := mrAbort;
  end;
end;

procedure TProfilerAddon.ProjectBuildingFinished(Sender: TObject;
  BuildSuccessful: Boolean);
begin
  if not fSourcesInstrumented then begin
    if (LazarusIDE.ActiveProject <> nil)
    and (pfAlwaysBuild in LazarusIDE.ActiveProject.Flags) then
      LazarusIDE.ActiveProject.Flags := LazarusIDE.ActiveProject.Flags - [pfAlwaysBuild];
  end;
  if fProfiling then begin
    if BuildSuccessful
    and cAutoRestore then
      RestoreSources;
    // enable CheckFilesOnDisk
    LazarusIDE.CheckFilesOnDiskEnabled := True; { needs trunk 56204 }
  end;
end;

procedure TProfilerAddon.RunFinished(Sender: TObject);
begin
  LoadResult;
  if fProfiling then begin
    // enable CheckFilesOnDisk
    LazarusIDE.CheckFilesOnDiskEnabled := True; { needs trunk 56204 }
    // force rebuild on next run/compile
    if LazarusIDE.ActiveProject <> nil then
      LazarusIDE.ActiveProject.Flags := LazarusIDE.ActiveProject.Flags + [pfAlwaysBuild];
    // show results
    IDEWindowCreators.ShowForm(cProfilerFormName, True);
    fProfiling := False;
  end else begin
    fProcList.Init;
  end;
end;

function TProfilerAddon.ProjectOpened(Sender: TObject; AProject: TLazProject
  ): TModalResult;
var
  lTargetFileName: String;
begin
  Result := mrOK;
  //DebugLn('*** ProjectOpened: '+AProject.ProjectInfoFile);
  fProcList.Clear;
  fOldProcList.Clear;
  { get project path and name }
  lTargetFileName := '$(TargetFile)';
  IDEMacros.SubstituteMacros(lTargetFileName);
  fTargetDir := AppendPathDelim(ExtractFileDir(lTargetFileName));
  fTargetName := ExtractFileNameOnly(lTargetFileName);
  fProjectDir := AppendPathDelim(ExtractFileDir(AProject.ProjectInfoFile));
  fIncludePath := AProject.LazCompilerOptions.IncludePath;
  fProfiling := False;
  AProject.Flags := AProject.Flags - [pfAlwaysBuild];
  LoadResult;
  if fNeedsRebuild then
    AProject.Flags := AProject.Flags + [pfAlwaysBuild];
end;

function TProfilerAddon.ProjectClose(Sender: TObject; AProject: TLazProject
  ): TModalResult;
begin
  if FileExists(fTargetDir + fTargetName + cSettingExtension)
  or (
    Assigned(ProfilerWindow)
    and ProfilerWindow.DataChanged
  ) then begin
    DebugLn('*** LazProfiler - ProjectClose');
    fNeedsRebuild := pfAlwaysBuild in AProject.Flags;
    SaveToFile(fTargetDir + fTargetName + cSettingExtension, fProcList);
  end;
  if Assigned(ProfilerWindow) then
    ProfilerWindow.Data := nil;
  fProcList.Clear;
  fOldProcList.Clear;
end;

constructor TProfilerAddon.Create;
begin
  fFileList := TStringList.Create;
  fProcList := TLPProcList.Create(True);
  fOldProcList := TLPProcList.Create(True);
end;

destructor TProfilerAddon.Destroy;
begin
  fOldProcList.Free;
  fProcList.Free;
  fFileList.Free;
  inherited Destroy;
end;

procedure TProfilerAddon.CreateProfilerWindow(Sender: TObject; aFormName: string; var AForm: TCustomForm; DoDisableAutoSizing: boolean);
//var
  //lNeedsCreate: Boolean;
begin
  if CompareText(aFormName, cProfilerFormName)<>0 then exit;
  //DebugLn('*** CreateProfilerWindow ' + aFormName);
  //lNeedsCreate :=  not Assigned(ProfilerWindow);
  IDEWindowCreators.CreateForm(ProfilerWindow, TLazProfilerForm, DoDisableAutosizing, LazarusIDE.OwningComponent);
  //if lNeedsCreate
  //and Assigned(IDEDockMaster) then
    //IDEDockMaster.MakeIDEWindowDockable(ProfilerWindow);
  AForm:=ProfilerWindow;
end;

procedure TProfilerAddon.Start(Sender: TObject);
var
  lTargetFileName: String;
  lProject: TLazProject;
begin
  //DebugLn('LazProfiler: start');
  if not Assigned(LazarusIDE.ActiveProject) then
    Exit;

  if Assigned(ProfilerWindow) then
    ProfilerWindow.Data := nil;

  lProject := LazarusIDE.ActiveProject;

  { save project }
  LazarusIDE.DoSaveAll([]);
  LazarusIDE.SaveSourceEditorChangesToCodeCache(nil); // nil: commit all source editors
  { get project path and name }
  lTargetFileName := '$(TargetFile)';
  IDEMacros.SubstituteMacros(lTargetFileName);
  fTargetDir := AppendPathDelim(ExtractFileDir(lTargetFileName));
  fTargetName := ExtractFileNameOnly(lTargetFileName);
  //DebugLn('  Project-Path: '+fTargetDir);
  //DebugLn('  Project-Name: '+fTargetName);

  fProjectDir := AppendPathDelim(ExtractFileDir(lProject.ProjectInfoFile));

  RestoreSources;

  if LazarusIDE.DoBuildProject(crRun, [pbfSkipLinking]) <> mrOK then
    Exit;

  if (IDEMessagesWindow <> nil) then
    IDEMessagesWindow.Clear;

  fProfiling := True;
  if LazarusIDE.DoRunProjectWithoutDebug<>mrOK then begin
    fProfiling := False;
    //if fSourcesInstrumented
    //and cAutoRestore then
    //  RestoreSources;
    LazarusIDE.CheckFilesOnDiskEnabled := True; { needs trunk 56204 }
  end;
end;

procedure TProfilerAddon.ShowProfilerWindow(Sender: TObject);
begin
  IDEWindowCreators.ShowForm(cProfilerFormName, True);
end;

procedure TProfilerAddon.CleanUp(Sender: TObject);
var
  lTargetFileName: String;
  lProject: TLazProject;
begin
  //DebugLn('LazProfiler: cleanup');
  if not Assigned(LazarusIDE.ActiveProject) then
    Exit;

  if Assigned(ProfilerWindow) then
    ProfilerWindow.Data := nil;

  lProject := LazarusIDE.ActiveProject;

  { save project }
  LazarusIDE.DoSaveAll([]);
  LazarusIDE.SaveSourceEditorChangesToCodeCache(nil); // nil: commit all source editors
  { get project path and name }
  lTargetFileName := '$(TargetFile)';
  IDEMacros.SubstituteMacros(lTargetFileName);
  fTargetDir := AppendPathDelim(ExtractFileDir(lTargetFileName));
  fTargetName := ExtractFileNameOnly(lTargetFileName);
  //DebugLn('  Project-Path: '+fTargetDir);
  //DebugLn('  Project-Name: '+fTargetName);

  fProjectDir := AppendPathDelim(ExtractFileDir(lProject.ProjectInfoFile));

  RestoreSources;

  LazarusIDE.DoCheckFilesOnDisk;
end;

initialization

  {$I runtime.lrs}
  ProfilerWindow := nil;
  Addon := TProfilerAddon.Create;

finalization

  Addon.Free;

end.

