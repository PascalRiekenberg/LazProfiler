{
    Copyright (c) 2017-18 Pascal Riekenberg

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
  Classes,
  Forms,
  Generics.Collections,
  LazIDEIntf,
  LazProfilerCore,
  MenuIntf,
  ProjectIntf,
  SysUtils,
  VirtualTrees,
  vtvObject;

type

  { TLPFile }

  TLPFile = class
  private
    fFilename: String;
    fChanged: Boolean;
    fText: TStringList;
  protected
  public
    constructor Create(pFilename: String);
    destructor Destroy; override;
    function Save: Boolean;
    property Filename: String read fFilename;
    property Changed: boolean read fChanged write fChanged;
    property Text: TStringList read fText;
  end;


  { TLPFileList }

  TLPFileList = class(specialize TObjectList<TLPFile>)
  private
  protected
  public
    function IndexOf(pFilename: String): SizeInt;
  end;


  { TLPvtvPasProc }

  TLPvtvPasProc = class(TvtvObj)
  private
    fPasProc: TLPPasProc;
  protected
    procedure SetCheckState(pCheckState: TCheckState); override;
  public
    constructor Create(pProc: TLPPasProc);
    procedure UpdateCheckState;
    function CellText(Column: TColumnIndex; TextType: TVSTTextType): String; override;
    function ImageIndex(pColumn: TColumnIndex): Integer; override;
    procedure InitNode(pVST: TBaseVirtualTree; pNode: PVirtualNode); override;
    property PasProc: TLPPasProc read fPasProc;
  end;
  PLPvtvProc = ^TLPvtvPasProc;


  { TLPvtvPasClass }

  TLPvtvPasClass = class(TLPvtvPasProc)
  private
    fPasClass: TLPPasClass;
  protected
  public
    constructor Create(pNameOfClass, pUnitName, pFileName: String);
    destructor Destroy; override;
    function CellText(Column: TColumnIndex; TextType: TVSTTextType): String; override;
    function ImageIndex(pColumn: TColumnIndex): Integer; override;
    function InitialStates: TVirtualNodeInitStates; override;
    procedure UpdateExpanded; override;
    property PasClass: TLPPasClass read fPasClass write fPasClass;
  end;


  { TLPvtvPasUnit }

  TLPvtvPasUnit = class(TLPvtvPasProc)
  private
    fPasUnit: TLPPasUnit;
  protected
  public
    constructor Create(pUnitName, pFileName: String);
    destructor Destroy; override;
    function CellText(Column: TColumnIndex; TextType: TVSTTextType): String; override;
    function ImageIndex(pColumn: TColumnIndex): Integer; override;
    function InitialStates: TVirtualNodeInitStates; override;
    procedure UpdateExpanded; override;
    property PasUnit: TLPPasUnit read fPasUnit write fPasUnit;
  end;


  { TProfilerAddon }

  TProfilerAddon = class(TCustomLazProfiler)
  private
    fProfiling: Boolean;
    fProject: TLazProject;
    fSourcesInstrumented: Boolean;
    fFileList,
    fIncludeList: TLPFileList;
    fProjectDir: String;
    fIncludePath: String;
    fTargetDir: String;
    fTargetName: String;
    fOldModified: Boolean;
    procedure BuildFileList(pExtensionMask: String; pCheckForBelongsToProject: Boolean);
    function AddProc(pName: String; pToken: Integer; pNameOfClass, pUnitName, pFileName: String; pRow: Integer): Integer;
  protected
    fOldUnitList: TLPPasUnitList;
    fOldClassList: TLPPasClassList;
    fOldProcList: TLPPasProcList;
    function ParseSources(pInstrument: Boolean): Boolean;
    function ParseSource(pFile: TLPFile; pInstrument: Boolean): Boolean;
    procedure RestoreSources;
    procedure RestoreSource(pFile: TLPFile);
    function ProjectBuilding(Sender: TObject): TModalResult;
    procedure ProjectBuildingFinished(Sender: TObject; BuildSuccessful: Boolean);
    procedure RunFinished(Sender: TObject);
    procedure LoadResult;
    function ProjectOpened(Sender: TObject; pProject: TLazProject): TModalResult;
    function ProjectClose(Sender: TObject; AProject: TLazProject): TModalResult;
    procedure CreateProfilerWindow(Sender: TObject; pFormName: string; var pForm: TCustomForm; DoDisableAutoSizing: boolean);
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
  Controls,
  LazLogger,
  CompOptsIntf,
  IDEImagesIntf,
  Graphics,
  MacroIntf,
  CodeToolManager,
  LazFileUtils,
  PScanner,
  Dialogs,
  StrUtils,
  IDEMsgIntf,
  IDEExternToolIntf,
  LResources,
  IDEWindowIntf,
  LazProfilerWindow,
  PackageIntf;

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

function CheckStateToStr(pCheckState: TCheckState): String;
begin
  WriteStr(Result, pCheckState);
end;


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


{ TLPvtvPasUnit }

constructor TLPvtvPasUnit.Create(pUnitName, pFileName: String);
begin
  inherited Create(TLPPasProc.Create('', 0, '', pUnitName, pFileName, 1));
end;

destructor TLPvtvPasUnit.Destroy;
begin
  fPasProc.Free;
  inherited Destroy;
end;

function TLPvtvPasUnit.CellText(Column: TColumnIndex; TextType: TVSTTextType): String;
begin
  case Column of
    0: CellText := fPasProc.UnitName;
    1: CellText := '';
    2: CellText := '';
    3: CellText := '';
    4: CellText := '';
    5: CellText := '';
  end;
end;

function TLPvtvPasUnit.ImageIndex(pColumn: TColumnIndex): Integer;
begin
  if pColumn = 0 then
    Result := 0
  else
    Result := -1;
end;

function TLPvtvPasUnit.InitialStates: TVirtualNodeInitStates;
begin
  Result := inherited InitialStates;
  if not Assigned(fPasUnit) then begin
    DebugLn('!!! TLPvtvPasUnit: %s fPasUnit=Nil', [fPasProc.UnitName]);
  end else
  if fPasUnit.Expanded then
    Result := Result + [ivsExpanded];
end;

procedure TLPvtvPasUnit.UpdateExpanded;
begin
  fPasUnit.Expanded := vsExpanded in fNode^.States;
end;


{ TLPFileList }

function TLPFileList.IndexOf(pFilename: String): SizeInt;
var
  i: Integer;
begin
  pFilename := UpperCase(pFilename);
  for i := 0 to Count - 1 do begin
    if UpperCase(Items[i].Filename) = pFilename then
      Exit(i);
  end;
  Result := -1;
end;


{ TLPFile }

constructor TLPFile.Create(pFilename: String);
begin
  fFilename := pFilename;
  fChanged := False;
  fText := TStringList.Create;
  fText.LoadFromFile(fFilename);
end;

destructor TLPFile.Destroy;
begin
  fText.Free;
  inherited Destroy;
end;

function TLPFile.Save: Boolean;
begin
  if FileExists(fFileName + cBackupExtension) then begin
    IDEMessagesWindow.AddCustomMessage(mluFatal, 'LazProfiler: backup file already exists: ' + fFileName + cBackupExtension, fFileName, 1, 1); // CurTokenPos needs FPC trunk 37235
    Exit(False);
  end else begin
    { make backup }
    RenameFile(fFileName, fFileName + cBackupExtension);
    if FileExists(fFileName) then begin
      IDEMessagesWindow.AddCustomMessage(mluFatal, 'LazProfiler: could not create backup file: ' + fFileName + cBackupExtension, fFileName, 1, 1); // CurTokenPos needs FPC trunk 37235
      Exit(False);
    end else begin
      { write modified source }
      fText.SaveToFile(fFileName);
    end;
  end;
  Result := True;
end;


{ TLPvtvPasClass }

constructor TLPvtvPasClass.Create(pNameOfClass, pUnitName, pFileName: String);
begin
  inherited Create(TLPPasProc.Create('', 0, pNameOfClass, pUnitName, pFileName, 1));
end;

destructor TLPvtvPasClass.Destroy;
begin
  fPasProc.Free;
  inherited Destroy;
end;

function TLPvtvPasClass.CellText(Column: TColumnIndex; TextType: TVSTTextType): String;
begin
  case Column of
    0: CellText := fPasProc.NameOfClass;
    1: CellText := '';
    2: CellText := '';
    3: CellText := '';
    4: CellText := '';
    5: CellText := '';
  end;
end;

function TLPvtvPasClass.ImageIndex(pColumn: TColumnIndex): Integer;
begin
  if pColumn = 0 then
    Result := 1
  else
    Result := -1;
end;

function TLPvtvPasClass.InitialStates: TVirtualNodeInitStates;
begin
  Result := inherited InitialStates;
  if not Assigned(fPasClass) then begin
    DebugLn('!!! TLPvtvPasClass: %s fPasClass=Nil', [fPasProc.NameOfClass]);
  end else
  if fPasClass.Expanded then
    Result := Result + [ivsExpanded];
end;

procedure TLPvtvPasClass.UpdateExpanded;
begin
  fPasClass.Expanded := vsExpanded in fNode^.States;
end;


{ TLPvtvPasProc }

procedure TLPvtvPasProc.UpdateCheckState;
var
  i: Integer;
  lChecked: Boolean;
  lChild: TLPvtvPasProc;

  function Check(pObj: TLPvtvPasProc): Boolean;
  var
    i: Integer;
  begin
    Result := True;
    if pObj.Childs.Count = 0 then begin
      if pObj.PasProc.Instrument <> lChecked then begin
        fNode^.CheckState := csMixedNormal;
        Exit(False);
      end;
    end else begin
      for i := 0 to pObj.Childs.Count - 1 do begin
        if not Check(TLPvtvPasProc(pObj.Childs[i])) then
          Exit(False);
      end;
    end;
  end;

begin
  if fChilds.Count = 0 then
    Exit;

  lChild := Self; while lChild.Childs.Count > 0 do lChild := TLPvtvPasProc(lChild.Childs[0]);
  lChecked := lChild.PasProc.Instrument;

  if not Check(Self) then begin
    fVst.InvalidateNode(fNode);
    Exit;
  end;

  if lChecked then
    fNode^.CheckState := csCheckedNormal
  else
    fNode^.CheckState := csUncheckedNormal;
  fVst.InvalidateNode(fNode);
end;

procedure TLPvtvPasProc.SetCheckState(pCheckState: TCheckState);
var
  lParent: TLPvtvPasProc;
  i: Integer;
begin
  inherited SetCheckState(pCheckState);
  fPasProc.Instrument := pCheckState = csCheckedNormal;
  lParent := TLPvtvPasProc(fParent);
  while Assigned(lParent) do begin
    TLPvtvPasProc(lParent).UpdateCheckState;
    lParent := TLPvtvPasProc(lParent.Parent);
  end;
  fVst.InvalidateChildren(fNode, True);
end;

constructor TLPvtvPasProc.Create(pProc: TLPPasProc);
begin
  inherited Create;
  fPasProc := pProc;
end;

function TLPvtvPasProc.CellText(Column: TColumnIndex; TextType: TVSTTextType): String;

  function IntToTime(pVal: Int64): String;
  var
    lVal: Extended;
  begin
    if pVal > 1000000000 then begin
      lVal := pVal / 1000000000.0;
      Result := FormatFloat('##0.000 s', lVal);
    end else if pVal > 1000000 then begin
      lVal := pVal / 1000000.0;
      Result := FormatFloat('##0.000 ms', lVal);
    end else begin
      lVal := pVal / 1000.0;
      Result := FormatFloat('##0.000 μs', lVal);
    end;
  end;

begin
  case Column of
    0: CellText := fPasProc.Name;
    1: CellText := IfThen(TVirtualStringTree(fVst).Header.SortColumn in [1, 2], '', fPasProc.NameOfClass);
    2: CellText := IfThen(TVirtualStringTree(fVst).Header.SortColumn = 2, '', fPasProc.UnitName);
    3: CellText := IfThen(fPasProc.Count = 0, '', IntToStr(fPasProc.Count));
    4: CellText := IfThen(fPasProc.Count = 0, '', IntToTime(fPasProc.Net));
    5: CellText := IfThen(fPasProc.Count = 0, '', IntTotime(fPasProc.Gross));
  end;
end;

function TLPvtvPasProc.ImageIndex(pColumn: TColumnIndex): Integer;
begin
  if pColumn = 0 then begin
    case TToken(fPasProc.Kind) of
      tkconstructor: Result := 2;
      tkdestructor:  Result := 3;
      tkprocedure:   Result := 4;
      tkfunction:    Result := 5;
      else           Result := 6;
    end;
  end else begin
    Result := -1;
  end;
end;

procedure TLPvtvPasProc.InitNode(pVST: TBaseVirtualTree; pNode: PVirtualNode);
begin
  inherited InitNode(pVST, pNode);
  fNode^.CheckType := ctCheckBox;
  if fPasProc.Instrument then
    fNode^.CheckState := csCheckedNormal
  else
    fNode^.CheckState := csUncheckedNormal;
  UpdateCheckState;
end;


{ TBlockEntry }

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

procedure TProfilerAddon.BuildFileList(pExtensionMask: String; pCheckForBelongsToProject: Boolean);
var
  lFPCSrcDir, lLazSrcDir: String;
  lPathList: TStringList;
  lLengthFPCSrcDir, lLengthLazSrcDir: Integer;

  function BelongsToProject(pFileName: string): Boolean;
  var
    Owners: TFPList;
    i: Integer;
    o: TObject;
  begin
    Result := False;
    Owners := PackageEditingInterface.GetPossibleOwnersOfUnit(pFileName, []);
    if not Assigned(Owners) then begin
      // unit is not directly associated with a project/package
      // maybe the unit was for some reason not added, but is reachable
      // search in all unit paths of all projects/packages
      // Beware: This can lead to false hits
      Owners:=PackageEditingInterface.GetPossibleOwnersOfUnit(pFileName, [piosfExcludeOwned,piosfIncludeSourceDirectories]);
    end;
    if not Assigned(Owners) then
      Exit;
    try
      for i := 0 to Owners.Count - 1 do begin
        o := TObject(Owners[i]);
        if o is TIDEPackage then begin
          //fPackageList.Add(TIDEPackage(o).Filename);
          Result := False;
          Exit;
        end else if (o is TLazProject)
        and (TLazProject(o).ProjectInfoFile = fProject.ProjectInfoFile) then begin
          Result := True;
        end;
      end;
    finally
      Owners.Free;
    end;
  end;

  procedure AddFile(pFileName: String);
  begin
    if (fFileList.IndexOf(pFileName) >= 0)
    or (pCheckForBelongsToProject
    and not BelongsToProject(pFileName)) then
      Exit;
    fFileList.Add(TLPFile.Create(pFileName));
  end;

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
        if ((Info.Attr and faDirectory) = faDirectory) then begin
          if (Info.Name <> '.')
          and (Info.Name <> '..') then
            ScanDir(pDir + Info.Name, pExtensionMask);
        end else begin
          if ExtensionList.IndexOf(ExtractFileExt(Info.Name)) <> -1 then
            AddFile(pDir + Info.Name);
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
  fIncludeList.Clear;
  fFileList.Clear;
  { get FPC source dir }
  lFPCSrcDir := '$(FPCSrcDir)';
  IDEMacros.SubstituteMacros(lFPCSrcDir);
  lFPCSrcDir := UpperCase(lFPCSrcDir);
  lLengthFPCSrcDir := length(lFPCSrcDir);
  { get Lazarus source dir }
  lLazSrcDir := '$(LazarusDir)';
  IDEMacros.SubstituteMacros(lLazSrcDir);
  lLazSrcDir := UpperCase(lLazSrcDir);
  lLengthLazSrcDir := length(lLazSrcDir);

  { scan pathes }
  lPathList := TStringList.Create;
  try
    lPathList.Delimiter := ';';
    lPathList.StrictDelimiter := True;
    { scan sources}
    lPathList.DelimitedText := CodeToolBoss.GetUnitPathForDirectory('');
    CheckDir(lPathList);
  finally
    lPathList.Free;
  end;
end;

function TProfilerAddon.AddProc(pName: String; pToken: Integer; pNameOfClass, pUnitName, pFileName: String; pRow: Integer): Integer;
var
  lPasUnit: TLPPasUnit;
  lPasClass: TLPPasClass;
  lPasProc: TLPPasProc;
  lUpName, lUpFileName, lUpNameOfClass: String;
  i: Integer;
begin
  //DebugLn('    AddProc(%s, %s)', [pName, pNameOfClass]);
  // unit
  lPasUnit := Nil;
  i := fUnitList.IndexOf(pUnitName, pFileName);
  if i >= 0 then begin
    lPasUnit := fUnitList[i];
  end else begin
    i := fOldUnitList.IndexOf(pUnitName, pFileName);
    if i >= 0 then begin
      lPasUnit := fOldUnitList.ExtractIndex(i);
    end;
    if not Assigned(lPasUnit) then
      lPasUnit := TLPPasUnit.Create(pUnitName, pFileName);
    fUnitList.Add(lPasUnit);
  end;

  // class
  lPasClass := Nil;
  i := fClassList.IndexOf(pNameOfClass, pUnitName, pFileName);
  if i >= 0 then begin
    lPasClass := fClassList[i];
  end else begin
    i := fOldClassList.IndexOf(pNameOfClass, pUnitName, pFileName);
    if i >= 0 then begin
      lPasClass := fOldClassList.ExtractIndex(i);
    end;
    if not Assigned(lPasClass) then
      lPasClass := TLPPasClass.Create(pNameOfClass, pUnitName, pFileName);
    lPasClass.PasUnit := lPasUnit;
    fClassList.Add(lPasClass);
  end;

  // proc
  lPasProc := Nil;
  i := fOldProcList.IndexOf(pName, pNameOfClass, pUnitName, pFileName);
  if i >= 0 then begin
    lPasProc := fOldProcList.ExtractIndex(i);
    lPasProc.Name := pName;
    lPasProc.Kind := pToken;
    lPasProc.NameOfClass := pNameOfClass;
    lPasProc.UnitName := pUnitName;
    lPasProc.FileName := pFileName;
    lPasProc.Row := pRow;
    lPasProc.Init;
  end;
  if not Assigned(lPasProc) then
    lPasProc := TLPPasProc.Create(pName, pToken, pNameOfClass, pUnitName, pFileName, pRow);
  //if not Assigned(lPasUnit) then
  //  DebugLn('      lPasUnit=Nil');
  //if not Assigned(lPasClass) then
  //  DebugLn('      lPasClass=Nil');
  lPasProc.PasUnit := lPasUnit;
  lPasProc.PasClass := lPasClass;
  Result := fProcList.Add(lPasProc);
  if not lPasProc.Instrument then
    { do not instrument }
    Result := -1;
end;

procedure TProfilerAddon.LoadResult;
begin
  LoadFromFile(fTargetDir + fTargetName + cSettingExtension);
  if fProcList.Count = 0 then begin
    ParseSources(False);
  end;
  if not Assigned(ProfilerWindow) then begin
    IDEWindowCreators.CreateForm(ProfilerWindow, TLazProfilerForm, False, Application);
    if Assigned(IDEDockMaster) then
      IDEDockMaster.MakeIDEWindowDockable(ProfilerWindow);
  end;
  ProfilerWindow.VST.Header.SortColumn := SortColumn;
  ProfilerWindow.VST.Header.SortDirection := TSortDirection(SortDirection);
  ProfilerWindow.Data := fProcList;
end;

function TProfilerAddon.ParseSources(pInstrument: Boolean): Boolean;
var
  i: Integer;
  lLRS: TLazarusResourceStream;
  lList3: TLPPasUnitList;
  lList2: TLPPasClassList;
  lList: TLPPasProcList;
begin
  Result := True;
  RestoreSources;
  DebugLn('*** LazProfiler: InstrumentSources(%s)', [BoolToStr(pInstrument, True)]);

  lList3 := fOldUnitList;
  fOldUnitList := fUnitList;
  fUnitList := lList3;
  fUnitList.Clear;

  lList2 := fOldClassList;
  fOldClassList := fClassList;
  fClassList := lList2;
  fClassList.Clear;

  lList := fOldProcList;
  fOldProcList := fProcList;
  fProcList := lList;
  fProcList.Clear;

  BuildFileList('.pp;.pas;.lpr', True);

  fAutoStart := True;
  fIncludeList.Clear;
  for i := 0 to fFileList.Count - 1 do
    if not ParseSource(fFileList[i], pInstrument) then
      Result := False;

  if pInstrument then begin
    for i := 0 to fIncludeList.Count - 1 do
      if fIncludeList[i].Changed then
        fIncludeList[i].Save;
    fSourcesInstrumented := True;
    fOldModified := LazarusIDE.ActiveProject.Modified;
    LazarusIDE.ActiveProject.Flags := LazarusIDE.ActiveProject.Flags + [pfAlwaysBuild];
    SaveToFile(fTargetDir + fTargetName + cSettingExtension);
    { create LazProfiler units }
    lLRS := TLazarusResourceStream.Create('core', nil);
    try
      lLRS.SaveToFile(fProjectDir + cCoreFileName);
    finally
      lLRS.Free;
    end;
    lLRS := TLazarusResourceStream.Create('rt', nil);
    try
      lLRS.SaveToFile(fProjectDir + cRunTimeFileName);
    finally
      lLRS.Free;
    end;
    lLRS := TLazarusResourceStream.Create('timer', nil);
    try
      lLRS.SaveToFile(fProjectDir + cTimerFileName);
    finally
      lLRS.Free;
    end;
  end;
end;

function TProfilerAddon.ParseSource(pFile: TLPFile; pInstrument: Boolean): Boolean;
var
  lIncludeDirs: TStringList;
  fr: TFileResolver;
  pas: TPascalScanner;
  token, lPart, lLastBlockToken, lToken: TToken;
  level, lProfilingChangeLevel, i: Integer;
  lImpUsesPos, lIntUsesPos, lImpPos, lIntPos: TPoint;
  lBlockStack: TBlocList;
  lBlock, lTempBlock, lStartProc: TBlockEntry;
  lName, lParents, lUpComment, lNameOfClass, lUnitName, lIncludePath: String;
  lProfiling: Boolean;
  lPos: SizeInt;
  lCurFile: TLPFile;

  procedure CheckFile;
  var
    lCurFileNameUp: String;
    i: Integer;
  begin
    lCurFileNameUp := UpperCase(pas.CurFilename);
    if lCurFileNameUp <> UpperCase(lCurFile.Filename) then begin
      if lCurFileNameUp = UpperCase(pFile.Filename) then begin
        lCurFile := pFile
      end else begin
        i := fIncludeList.IndexOf(pas.CurFilename);
        if i >= 0 then begin
          lCurFile := fIncludeList[i];
        end else begin
          //DebugLn('      Added Include %s', [pas.CurFilename]);
          lCurFile := TLPFile.Create(pas.CurFilename);
          fIncludeList.Add(lCurFile);
        end;
      end;
    end;
  end;

  procedure InsertEnter(pRow, pCol: Integer; pProcID: Integer);
  var
    lLine: String;
  begin
    CheckFile;
    lLine := lCurFile.Text[pRow];
    Insert(' try LazProfiler.EnterProfiling(' + IntToStr(pProcID) + ');', lLine, pCol);
    lCurFile.Text[pRow] := lLine;
    lCurFile.Changed := True;
  end;

  procedure InsertExit(pRow, pCol: Integer; pProcID: Integer);
  var
    lLine: String;
  begin
    CheckFile;
    lLine := lCurFile.Text[pRow];
    Insert('finally LazProfiler.ExitProfiling(' + IntToStr(pProcID) + '); end; ', lLine, pCol);
    lCurFile.Text[pRow] := lLine;
    lCurFile.Changed := True;
  end;

  procedure InsertStart(pRow, pCol: Integer);
  var
    lLine: String;
  begin
    CheckFile;
    lLine := lCurFile.Text[pRow];
    Insert('LazProfiler.StartProfiling; ', lLine, pCol);
    lCurFile.Text[pRow] := lLine;
    lCurFile.Changed := True;
  end;

  procedure InsertStop(pRow, pCol: Integer);
  var
    lLine: String;
  begin
    CheckFile;
    lLine := lCurFile.Text[pRow];
    Insert('LazProfiler.StopProfiling; ', lLine, pCol);
    lCurFile.Text[pRow] := lLine;
    lCurFile.Changed := True;
  end;

  procedure InsertUnit(pRow, pCol: Integer);
  var
    lLine: String;
  begin
    CheckFile;
    lLine := lCurFile.Text[pRow];
    Insert(' LazProfilerRunTime, ', lLine, pCol);
    lCurFile.Text[pRow] := lLine;
    lCurFile.Changed := True;
  end;

  procedure InsertUses(pRow, pCol: Integer);
  begin
    CheckFile;
    lCurFile.Text.Insert(pRow, 'Uses LazProfilerRunTime;');
    lCurFile.Changed := True;
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
          end else
            lName := lName + '.'
        end;
      end;
      lBlock.name := lName;
      lBlock.nameofclass := lNameOfClass;
    end;
  end;

begin
  Result := True;
  lCurFile := pFile;
  //DebugLn('  '+pFile.FileName);
  fr := TFileResolver.Create;
  lIncludeDirs := TStringList.Create;
  try
    lIncludeDirs.Delimiter := ';';
    lIncludePath := fIncludePath;
    IDEMacros.SubstituteMacros(lIncludePath);
    lIncludeDirs.DelimitedText := lIncludePath;
    for i := 0 to lIncludeDirs.Count - 1 do
      fr.AddIncludePath(TrimAndExpandDirectory(lIncludeDirs[i], fProjectDir));
    fr.AddIncludePath(fProjectDir);
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
    pas.OpenFile(pFile.FileName);
    while True do case pas.FetchToken of
      tkunit, tkprogram: begin
        while pas.FetchToken <> tkIdentifier do;
        lUnitName := pas.CurTokenString;
      end;
      tkAsm: if Assigned(lBlock)
      and not (lBlock.token in [tkprocedure, tkfunction, tkconstructor, tkdestructor]) then
        NewBlock(pas.CurToken, '');
      tkBegin:
      begin
        if Assigned(lBlock)
        and (lBlock.token in [tkprocedure, tkfunction, tkconstructor, tkdestructor]) then begin
          if lProfiling then begin
            { insert: lazprofiler.enter() }
            lToken := lBlock.token;
            lName := lBlock.name;
            lNameOfClass := lBlock.nameofclass;
            lTempBlock := lBlock.parent;
            if Assigned(lTempBlock) and (lTempBlock.token in [tkprocedure, tkfunction, tkconstructor, tkdestructor]) then begin
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
            CheckFile;
            lBlock.procid := AddProc(lName, Integer(lToken), lNameOfClass, lUnitName, lCurFile.FileName, pas.CurRow - 1);
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
        if Assigned(lBlock) then
          lLastBlockToken := lBlock.token
        else
          lLastBlockToken := tkEOF;
        PopBlock;
        if Assigned(lBlock)
        and (lBlock.token in [tkprocedure, tkfunction, tkconstructor, tkdestructor])
        and (lLastBlockToken = tkbegin) then begin
          if lProfiling then begin
            { insert lazprofiler.exit }
            //DebugLn('      ' + lBlock.Name + ' ' + IntToStr(level));
            if lBlock.procid <> -1 then begin
              InsertExit(pas.CurRow - 1, pas.CurTokenPos.Column, lBlock.procid); // CurTokenPos needs FPC trunk 37235
              if lBlock = lStartProc then begin
                CheckFile;
                IDEMessagesWindow.AddCustomMessage(mluFatal, 'LazProfiler: Profiler not stopped in ' + lStartProc.name, lCurFile.FileName, pas.CurRow, pas.CurTokenPos.Column); // CurTokenPos needs FPC trunk 37235
                Exit(False);
              end;
            end;
          end;
          PopBlock;
        end;
      end;
      tktry:
        NewBlock(pas.CurToken, '');
      tkconstructor,
      tkdestructor,
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
        if pas.CurToken in [tkprocedure, tkfunction, tkconstructor, tkdestructor] then begin
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
        CheckFile;
        lUpComment := UpperCase(Trim(pas.CurTokenString));
        if lUpComment = 'START-PROFILER' then begin
          if not lProfiling then
            Continue;
          if Assigned(lStartProc) then begin
            IDEMessagesWindow.AddCustomMessage(mluFatal, 'LazProfiler: START-PROFILER: Profiler already started', lCurFile.FileName, pas.CurRow, pas.CurTokenPos.Column); // CurTokenPos needs FPC trunk 37235
            exit(False);
          end else begin
            lTempBlock := lBlock;
            while Assigned(lTempBlock) and not (lTempBlock.token in [tkprocedure, tkfunction, tkconstructor, tkdestructor]) do lTempBlock := lTempBlock.parent;
            if Assigned(lTempBlock) then begin
              lStartProc := lTempBlock;
              InsertStart(pas.CurRow - 1, pas.CurTokenPos.Column); // CurTokenPos needs FPC trunk 37235
              fAutoStart := False;
            end else begin
              IDEMessagesWindow.AddCustomMessage(mluFatal, 'LazProfiler: START-PROFILER: not in procedure/function', lCurFile.FileName, pas.CurRow, pas.CurTokenPos.Column); // CurTokenPos needs FPC trunk 37235
              exit(False);
            end;
          end;
        end else if lUpComment = 'STOP-PROFILER' then begin
          if not lProfiling then
            Continue;
          if not Assigned(lStartProc) then begin
            IDEMessagesWindow.AddCustomMessage(mluFatal, 'LazProfiler: STOP-PROFILER: Profiler not started', lCurFile.FileName, pas.CurRow, pas.CurTokenPos.Column); // CurTokenPos needs FPC trunk 37235
            exit(False);
          end else begin
            lTempBlock := lBlock;
            while Assigned(lTempBlock) and not (lTempBlock.token in [tkprocedure, tkfunction, tkconstructor, tkdestructor]) do lTempBlock := lTempBlock.parent;
            if Assigned(lTempBlock) then begin
              if lTempBlock = lStartProc then begin
                InsertStop(pas.CurRow - 1, pas.CurTokenPos.Column);  // CurTokenPos needs FPC trunk 37235
                lStartProc := nil;
              end else begin
                IDEMessagesWindow.AddCustomMessage(mluFatal, 'LazProfiler: STOP-PROFILER: Profiler not started in same procedure/function', lCurFile.FileName, pas.CurRow, pas.CurTokenPos.Column); // CurTokenPos needs FPC trunk 37235
                exit(False);
              end;
            end else begin
              IDEMessagesWindow.AddCustomMessage(mluFatal, 'LazProfiler: STOP-PROFILER: not in procedure/function', lCurFile.FileName, pas.CurRow, pas.CurTokenPos.Column); // CurTokenPos needs FPC trunk 37235
              exit(False);
            end;
          end;
        end else if lUpComment = 'PROFILER-NO' then begin
          if not lProfiling then begin
            IDEMessagesWindow.AddCustomMessage(mluFatal, 'LazProfiler: PROFILER-NO: set twice', lCurFile.FileName, pas.CurRow, pas.CurTokenPos.Column); // CurTokenPos needs FPC trunk 37235
            exit(False);
          end;
          lProfiling := False;
          lProfilingChangeLevel := level;
        end else if lUpComment = 'PROFILER-YES' then begin
          if lProfiling then begin
            IDEMessagesWindow.AddCustomMessage(mluFatal, 'LazProfiler: PROFILER-YES: set twice', lCurFile.FileName, pas.CurRow, pas.CurTokenPos.Column); // CurTokenPos needs FPC trunk 37235
            exit(False);
          end;
          if lProfilingChangeLevel <> level then begin
            IDEMessagesWindow.AddCustomMessage(mluFatal, 'LazProfiler: PROFILER-YES: not set on same level as PROFILER-NO', lCurFile.FileName, pas.CurRow, pas.CurTokenPos.Column); // CurTokenPos needs FPC trunk 37235
            exit(False);
          end;
          lProfiling := True
        end;
      end;
      tkEOF:
        break;
    end;

    if pInstrument
    and pFile.Changed then begin
      { insert uses }
      if UpperCase(ExtractFileExt(pFile.FileName)) <> '.INC' then begin
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
    end;
  finally
    ClearStack;
    lBlockStack.Free;
    pas.Free;
    fr.Free;
  end;

  if pInstrument
  and pfile.Changed
  and not pFile.Save then
    Exit(False);
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

  BuildFileList(cBackupExtension, False);

  //fFileList.Sort;
  for i := 0 to fFileList.Count - 1 do
    RestoreSource(fFileList[i]);

  fSourcesInstrumented := False;
  LazarusIDE.ActiveProject.Flags := LazarusIDE.ActiveProject.Flags - [pfAlwaysBuild];
  LazarusIDE.ActiveProject.Modified := fOldModified;
end;

procedure TProfilerAddon.RestoreSource(pFile: TLPFile);
var
  lOrigFileName: String;
begin
  DebugLn('  ' + pFile.FileName);
  lOrigFileName := LeftStr(pFile.Filename, length(pFile.Filename) - length(cBackupExtension));
  if FileExists(pFile.Filename) then begin
    DeleteFile(lOrigFileName);
    RenameFile(pFile.Filename, lOrigFileName);
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

procedure TProfilerAddon.ProjectBuildingFinished(Sender: TObject; BuildSuccessful: Boolean);
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

function TProfilerAddon.ProjectOpened(Sender: TObject; pProject: TLazProject): TModalResult;
var
  lTargetFileName: String;
begin
  Result := mrOK;
  DebugLn('*** LazProfiler: ProjectOpened: '+pProject.ProjectInfoFile);
  fProject := pProject;
  fProcList.Clear;
  fOldProcList.Clear;
  { get project path and name }
  lTargetFileName := '$(TargetFile)';
  IDEMacros.SubstituteMacros(lTargetFileName);
  fTargetDir := AppendPathDelim(ExtractFileDir(lTargetFileName));
  fTargetName := ExtractFileNameOnly(lTargetFileName);
  fProjectDir := AppendPathDelim(ExtractFileDir(pProject.ProjectInfoFile));
  fIncludePath := pProject.LazCompilerOptions.IncludePath;
  fProfiling := False;
  pProject.Flags := pProject.Flags - [pfAlwaysBuild];
  LoadResult;
  if fNeedsRebuild then
    pProject.Flags := pProject.Flags + [pfAlwaysBuild];
  fOldModified := False;
  pProject.Modified := False;
end;

function TProfilerAddon.ProjectClose(Sender: TObject; AProject: TLazProject): TModalResult;
begin
  Result := mrOK;
  if FileExists(fTargetDir + fTargetName + cSettingExtension)
  or (
    Assigned(ProfilerWindow)
    and ProfilerWindow.DataChanged
  ) then begin
    DebugLn('*** LazProfiler: ProjectClose');
    fNeedsRebuild := pfAlwaysBuild in AProject.Flags;
    SaveToFile(fTargetDir + fTargetName + cSettingExtension);
  end;
  if Assigned(ProfilerWindow) then
    ProfilerWindow.Data := nil;
  fProcList.Clear;
  fOldProcList.Clear;
end;

constructor TProfilerAddon.Create;
begin
  inherited Create;
  fFileList := TLPFileList.Create(True);
  fOldUnitList := TLPPasUnitList.Create(True);
  fOldClassList := TLPPasClassList.Create(True);
  fIncludeList := TLPFileList.Create(True);
  fOldProcList := TLPPasProcList.Create(True);
end;

destructor TProfilerAddon.Destroy;
begin
  fOldProcList.Free;
  fIncludeList.Free;
  fOldClassList.Free;
  fOldUnitList.Free;
  fFileList.Free;
  inherited Destroy;
end;

procedure TProfilerAddon.CreateProfilerWindow(Sender: TObject; pFormName: string; var pForm: TCustomForm; DoDisableAutoSizing: boolean);
begin
  if CompareText(pFormName, cProfilerFormName)<>0 then exit;
  IDEWindowCreators.CreateForm(ProfilerWindow, TLazProfilerForm, DoDisableAutosizing, LazarusIDE.OwningComponent);
  pForm:=ProfilerWindow;
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

