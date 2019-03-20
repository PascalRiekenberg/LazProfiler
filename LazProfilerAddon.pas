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
  Laz.VirtualTrees,
  vtvObject;

type

  { TLPFile }

  TLPFile = class
  private
    fFilename,
    fFilenameUp,
    fPackageName: String;
    fChanged: Boolean;
    fText: TStringList;
    fSaved: Boolean;
  protected
  public
    constructor Create(pFilename, pPackageName: String);
    destructor Destroy; override;
    function Save: Boolean;
    property Filename: String read fFilename;
    property FilenameUp: String read fFilenameUp;
    property PackageName: String read fPackageName;
    property Changed: boolean read fChanged write fChanged;
    property Text: TStringList read fText;
  end;


  { TLPFileList }

  TLPFileList = class(specialize TObjectList<TLPFile>)
  private
  protected
  public
    function IndexOf(pFilename: String): SizeInt; overload;
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
    constructor Create(pNameOfClass, pUnitName, pFileName, pPackageName: String);
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
    constructor Create(pUnitName, pFileName, pPackageName: String);
    destructor Destroy; override;
    function CellText(Column: TColumnIndex; TextType: TVSTTextType): String; override;
    function ImageIndex(pColumn: TColumnIndex): Integer; override;
    function InitialStates: TVirtualNodeInitStates; override;
    procedure UpdateExpanded; override;
    property PasUnit: TLPPasUnit read fPasUnit write fPasUnit;
  end;


  { TLPvtvPasPackage }

  TLPvtvPasPackage = class(TLPvtvPasProc)
  private
    fPasPackage: TLPPasPackage;
  protected
  public
    constructor Create(pPackageName: String);
    destructor Destroy; override;
    function CellText(Column: TColumnIndex; TextType: TVSTTextType): String; override;
    function ImageIndex(pColumn: TColumnIndex): Integer; override;
    function InitialStates: TVirtualNodeInitStates; override;
    procedure UpdateExpanded; override;
    property PasPackage: TLPPasPackage read fPasPackage write fPasPackage;
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
    function AddProc(pName: String; pToken: Integer; pNameOfClass, pUnitName, pFileName, pPackageName: String; pRow: Integer): Integer;
    procedure SetActive(pActive: Boolean);
  protected
    fEpikTimerPath: String;
    fLazProfilerPath: String;
    fUnitOutputDirectory: String;
    fOldPackageList: TLPPasPackageList;
    fOldUnitList: TLPPasUnitList;
    fOldClassList: TLPPasClassList;
    fOldProcList: TLPPasProcList;
    procedure ModifySettings;
    function ParseSources(pInstrument: Boolean): Boolean;
    function ParseSource(pFile: TLPFile; pInstrument: Boolean): Boolean;
    procedure RestoreModifiedSettings;
    procedure RestoreSources;
    procedure RestoreSource(pFile: TLPFile);
    function ProjectRunWithoutDebugBuilding(Sender: TObject; var Handled: boolean): TModalResult;
    procedure ProjectBuildingFinished(Sender: TObject; BuildSuccessful: Boolean);
    procedure RunFinished(Sender: TObject);
    procedure LoadResult;
    function ProjectOpened(Sender: TObject; pProject: TLazProject): TModalResult;
    function ProjectClose(Sender: TObject; AProject: TLazProject): TModalResult;
    function SaveEditorFile(Sender: TObject; aFile: TLazProjectFile; SaveStep: TSaveEditorFileStep; TargetFilename: string): TModalResult;
    procedure CreateProfilerWindow(Sender: TObject; pFormName: string; var pForm: TCustomForm; DoDisableAutoSizing: boolean);
    procedure UpdateUI;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Start(Sender: TObject);
    procedure ShowProfilerWindow(Sender: TObject);
    procedure CleanUp(Sender: TObject);
    property Project: TLazProject read fProject;
    property Active: Boolean read fActive write SetActive;
  end;


var
  Addon: TProfilerAddon;
  RunCmd: TIDEMenuCommand;


procedure Register;

implementation

uses
  Controls,
  LazLogger,
  CompOptsIntf,
  Dialogs,
  IDEImagesIntf,
  Graphics,
  MacroIntf,
  CodeToolManager,
  LazFileUtils,
  PScanner,
  StrUtils,
  IDEMsgIntf,
  IDEExternToolIntf,
  LResources,
  IDEWindowIntf,
  LazProfilerWindow,
  ProjPackIntf,
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

function TokenToStr(TokenType: TToken): String;
begin
  WriteStr(Result, TokenType);
end;

function CheckStateToStr(pCheckState: TCheckState): String;
begin
  WriteStr(Result, pCheckState);
end;


procedure Register;
var
  lIdx: Integer;
begin
  RunCmd := TIDEMenuCommand.Create('itmRunMenuRunWithProfiler');
  with RunCmd do begin
    Caption := 'Profile';
    OnClick := @Addon.Start;
  end;
  lIdx := itmRunnning.IndexByName('itmRunMenuRun');
  //IDEImages.Images_16.Add(TBitmap.Create.LoadFromStream(), nil);
  //lItem.ImageIndex := IDEImages.LoadImage('menu_profile');

  itmRunnning.Insert(lIdx + 1, RunCmd);

  RegisterIDEMenuCommand(itmViewMainWindows, 'itmViewMainWindowsProfiler','Profiler (Results and Configuration)',@Addon.ShowProfilerWindow, nil);
  RegisterIDEMenuCommand(itmRunnning, 'itmRunMenuProfilerCleanUp','Cleanup Profiler and restore original files',@Addon.CleanUp, nil);
  LazarusIDE.AddHandlerOnRunWithoutDebugBuilding(@Addon.ProjectRunWithoutDebugBuilding);
  LazarusIDE.AddHandlerOnProjectBuildingFinished(@Addon.ProjectBuildingFinished);
  LazarusIDE.AddHandlerOnRunFinished(@Addon.RunFinished); { needs trunk 56254 }
  LazarusIDE.AddHandlerOnProjectOpened(@Addon.ProjectOpened);
  LazarusIDE.AddHandlerOnProjectClose(@Addon.ProjectClose);
  LazarusIDE.AddHandlerOnSaveEditorFile(@Addon.SaveEditorFile);

  IDEWindowCreators.Add(cProfilerFormName, nil, @Addon.CreateProfilerWindow, '100', '10%', '+300', '+50%');
end;


{ TLPvtvPasPackage }

constructor TLPvtvPasPackage.Create(pPackageName: String);
begin
  inherited Create(TLPPasProc.Create('', 0, '', '', '', pPackageName, 1));
end;

destructor TLPvtvPasPackage.Destroy;
begin
  fPasProc.Free;
  inherited Destroy;
end;

function TLPvtvPasPackage.CellText(Column: TColumnIndex; TextType: TVSTTextType): String;
begin
  if Column = 0 then begin
    if fPasProc.PackageIsProject then
      CellText := Addon.Project.Title
    else
      CellText := fPasProc.PackageName;
  end else
    CellText := '';
end;

function TLPvtvPasPackage.ImageIndex(pColumn: TColumnIndex): Integer;
begin
  if pColumn = 0 then
    if fPasProc.PackageIsProject then
      Result := 8
    else
      Result := 7
  else
    Result := -1;
end;

function TLPvtvPasPackage.InitialStates: TVirtualNodeInitStates;
begin
  Result := inherited InitialStates;
  if not Assigned(fPasPackage) then begin
    WriteLn('!!! TLPvtvPasPackage: ', fPasProc.PackageName, ' fPasPackage=Nil');
  end else
  if fPasPackage.Expanded then
    Result := Result + [ivsExpanded];
end;

procedure TLPvtvPasPackage.UpdateExpanded;
begin
  fPasPackage.Expanded := vsExpanded in fNode^.States;
end;


{ TLPvtvPasUnit }

constructor TLPvtvPasUnit.Create(pUnitName, pFileName, pPackageName: String);
begin
  inherited Create(TLPPasProc.Create('', 0, '', pUnitName, pFileName, pPackageName, 1));
end;

destructor TLPvtvPasUnit.Destroy;
begin
  fPasProc.Free;
  inherited Destroy;
end;

function TLPvtvPasUnit.CellText(Column: TColumnIndex; TextType: TVSTTextType): String;
begin
  if Column = 0 then
    CellText := fPasProc.UnitName
  else
    CellText := '';
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
    WriteLn('!!! TLPvtvPasUnit: ', fPasProc.UnitName, ' fPasUnit=Nil');
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

constructor TLPFile.Create(pFilename, pPackageName: String);
begin
  fFilename := pFilename;
  fFilenameUp := UpperCase(pFilename);
  fPackageName := pPackageName;
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
  if not fSaved then begin
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
        fSaved := True;
      end;
    end;
  end else begin
    WriteLn('*** LazProfiler: !!! re-saving ', fFilename);
    fText.SaveToFile(fFileName);
  end;
  Result := True;
end;


{ TLPvtvPasClass }

constructor TLPvtvPasClass.Create(pNameOfClass, pUnitName, pFileName, pPackageName: String);
begin
  inherited Create(TLPPasProc.Create('', 0, pNameOfClass, pUnitName, pFileName, pPackageName, 1));
end;

destructor TLPvtvPasClass.Destroy;
begin
  fPasProc.Free;
  inherited Destroy;
end;

function TLPvtvPasClass.CellText(Column: TColumnIndex; TextType: TVSTTextType): String;
begin
  if Column = 0 then
    CellText := fPasProc.NameOfClass
  else
    CellText := '';
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
    WriteLn('!!! TLPvtvPasClass: ', fPasProc.NameOfClass, ' fPasClass=Nil');
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
begin
  case Column of
    cNameCol: CellText := fPasProc.Name;
    cClassCol: CellText := IfThen(TVirtualStringTree(fVst).Header.SortColumn in [1, 2, 3], '', fPasProc.NameOfClass);
    cUnitCol: CellText := IfThen(TVirtualStringTree(fVst).Header.SortColumn in [1, 2, 3], '', fPasProc.UnitName);
    cPackageCol: CellText := IfThen(TVirtualStringTree(fVst).Header.SortColumn in [1, 2, 3], '', IfThen(fPasProc.PackageName = '?', LazarusIDE.ActiveProject.Title, fPasProc.PackageName));
    cCountCol: CellText := fPasProc.CountStr;
    cPerNetCol: CellText := fPasProc.PerNetStr;
    cSumNetCol: CellText := fPasProc.SumNetStr;
    cPerGrossCol: CellText := fPasProc.PerGrossStr;
    cSumGrossCol: CellText := fPasProc.SumGrossStr;
    cAvgNetCol: CellText := fPasProc.AvgNetStr;
    cAvgGrossCol: CellText := fPasProc.AvgGrossStr;
  end;
end;

function TLPvtvPasProc.ImageIndex(pColumn: TColumnIndex): Integer;
begin
  if pColumn = 0 then begin
    case TToken(fPasProc.Kind) of
      tkconstructor: Result := 2;
      tkdestructor:  Result := 3;
      tkoperator,
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
  lFPCSrcDir, lLazSrcDir, lPath: String;
  lPathList: TStringList;
  lLengthFPCSrcDir, lLengthLazSrcDir, i, j: Integer;
  lPkgList: TFPList;
  lUnits: TStrings;

  function BelongsToProject(pFileName: string; var pPackageName: String): Boolean;
  var
    Owners: TFPList;
    i: Integer;
    o: TObject;
  begin
    Result := False;
    pPackageName := '?';
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
      //debugln('*** LazProfiler Owner of ' + pFileName);
      for i := 0 to Owners.Count - 1 do begin
        o := TObject(Owners[i]);
        if o is TIDEPackage then begin
          pPackageName := ExtractFileNameWithoutExt(ExtractFilenameOnly(TIDEPackage(o).Filename));
          //debugln('    ' + pPackageName);
          Exit(True);
        end else if (o is TLazProject)
        and (TLazProject(o).ProjectInfoFile = fProject.ProjectInfoFile) then begin
          //pPackageName := TLazProject(o).Title;
          //debugln('    ' + pPackageName);
          Exit(True);
        end;
      end;
    finally
      Owners.Free;
    end;
  end;

  procedure AddFile(pFileName: String);
  var
    lPackageName: String;
  begin
    //DebugLn('*** LazProfiler:   testing ', ExtractFileName(pFileName));
    if (fFileList.IndexOf(pFileName) >= 0)
    or (pCheckForBelongsToProject
    and not BelongsToProject(pFileName, lPackageName)) then
      Exit;
    //DebugLn('*** LazProfiler:     added ', lPackageName, ' - ', ExtractFileName(pFileName));
    fFileList.Add(TLPFile.Create(pFileName, lPackageName));
  end;

  procedure ScanDir(pDir, pExtensionMask: String);
  var
    Info: TSearchRec;
    ExtensionList: TStrings;
  begin
    pDir := AppendPathDelim(pDir);

    //DebugLn('*** LazProfiler: ScanDir ', pDir);

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
    //lPathList.DelimitedText := CodeToolBoss.GetUnitPathForDirectory('');
    PackageEditingInterface.GetRequiredPackages(fProject, lPkgList, [pirNotRecursive]);
    for i := 0 to lPkgList.Count - 1 do begin
      //debugln('    Package: ' + TIDEPackage(lPkgList[i]).Filename);
      lPath := ExtractFilePath(TIDEPackage(lPkgList[i]).Filename);
      if lPathList.IndexOf(lPath) = -1 then
        lPathList.Add(lPath);
      lUnits := LazarusIDE.FindUnitsOfOwner(TIDEPackage(lPkgList[i]), [fuooListed,fuooUsed]); // add fuooPackages to include units from packages
      try
        for j:=0 to lUnits.Count-1 do begin
          //debugln('      Unit: ' + lUnits[j]);
          lPath := ExtractFilePath(lUnits[j]);
          if lPathList.IndexOf(lPath) = -1 then
            lPathList.Add(lPath);
        end;
      finally
        lUnits.Free;
      end;
    end;
    lPkgList.Free;

    lUnits := LazarusIDE.FindUnitsOfOwner(fProject,[fuooListed,fuooUsed]); // add fuooPackages to include units from packages
    try
      for i:=0 to lUnits.Count-1 do begin
        //debugln('    Unit: ' + lUnits[i]);
        lPath := ExtractFilePath(lUnits[i]);
        if lPathList.IndexOf(lPath) = -1 then
          lPathList.Add(lPath);
      end;
    finally
      lUnits.Free;
    end;

    { scan sources}
    CheckDir(lPathList);
  finally
    lPathList.Free;
  end;
end;

function TProfilerAddon.AddProc(pName: String; pToken: Integer; pNameOfClass,
  pUnitName, pFileName, pPackageName: String; pRow: Integer): Integer;
var
  lPasPackage: TLPPasPackage;
  lPasUnit: TLPPasUnit;
  lPasClass: TLPPasClass;
  lPasProc: TLPPasProc;
  lUpName, lUpFileName, lUpNameOfClass: String;
  i: Integer;
begin
  //DebugLn('    AddProc(%s, %s)', [pName, pNameOfClass]);

  // package
  lPasPackage := Nil;
  i := fPackageList.IndexOf(pPackageName);
  if i >= 0 then begin
    lPasPackage := fPackageList[i];
  end else begin
    i := fOldPackageList.IndexOf(pPackageName);
    if i >= 0 then begin
      lPasPackage := fOldPackageList.ExtractIndex(i);
    end;
    if not Assigned(lPasPackage) then
      lPasPackage := TLPPasPackage.Create(pPackageName);
    fPackageList.Add(lPasPackage);
  end;

  // unit
  lPasUnit := Nil;
  i := fUnitList.IndexOf(pUnitName, pFileName, pPackageName);
  if i >= 0 then begin
    lPasUnit := fUnitList[i];
  end else begin
    i := fOldUnitList.IndexOf(pUnitName, pFileName, pPackageName);
    if i >= 0 then begin
      lPasUnit := fOldUnitList.ExtractIndex(i);
    end;
    if not Assigned(lPasUnit) then
      lPasUnit := TLPPasUnit.Create(pUnitName, pFileName, pPackageName);
    lPasUnit.PasPackage := lPasPackage;
    fUnitList.Add(lPasUnit);
  end;

  // class
  lPasClass := Nil;
  i := fClassList.IndexOf(pNameOfClass, pUnitName, pFileName, pPackageName);
  if i >= 0 then begin
    lPasClass := fClassList[i];
  end else begin
    i := fOldClassList.IndexOf(pNameOfClass, pUnitName, pFileName, pPackageName);
    if i >= 0 then begin
      lPasClass := fOldClassList.ExtractIndex(i);
    end;
    if not Assigned(lPasClass) then
      lPasClass := TLPPasClass.Create(pNameOfClass, pUnitName, pFileName, pPackageName);
    lPasClass.PasPackage := lPasPackage;
    lPasClass.PasUnit := lPasUnit;
    fClassList.Add(lPasClass);
  end;

  // proc
  lPasProc := Nil;
  i := fOldProcList.IndexOf(pName, pNameOfClass, pUnitName, pFileName, pPackageName);
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
    lPasProc := TLPPasProc.Create(pName, pToken, pNameOfClass, pUnitName, pFileName, pPackageName, pRow);
  //if not Assigned(lPasUnit) then
  //  DebugLn('      lPasUnit=Nil');
  //if not Assigned(lPasClass) then
  //  DebugLn('      lPasClass=Nil');
  lPasProc.PasPackage := lPasPackage;
  lPasProc.PasUnit := lPasUnit;
  lPasProc.PasClass := lPasClass;
  Result := fProcList.Add(lPasProc);
  if not lPasProc.Instrument then
    { do not instrument }
    Result := -1;
end;

procedure TProfilerAddon.SetActive(pActive: Boolean);
begin
  if fActive = pActive then Exit;
  fActive := pActive;
  Screen.Cursor := crHourGlass;
  try
    if fActive then begin
      ParseSources(False);
    end;
    UpdateUI;
    if Assigned(ProfilerWindow) then
      ProfilerWindow.DataChanged := True;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TProfilerAddon.ModifySettings;
var
  i: Integer;
  lFiles: TStringList;
  lPkg: TIDEProjPackBase;
  lPasPkg: TLPPasPackage;
begin
  for i := 0 to fPackageList.Count - 1 do begin
    lPasPkg := fPackageList[i];
    if lPasPkg.PackageIsProject then begin
      lPkg := fProject;
    end else begin
      lPkg := PackageEditingInterface.FindPackageWithName(lPasPkg.PackageName);
      //PackageEditingInterface.DoOpenPackageWithName(lPasPkg.PackageName, [pofDoNotOpenEditor], False);
    end;
    lFiles := TStringList.Create;
    try
      lFiles.Delimiter := ';';
      lFiles.StrictDelimiter := True;
      lFiles.DelimitedText := lPkg.LazCompilerOptions.OtherUnitFiles;
      lFiles.Insert(0, fLazProfilerPath);
      lFiles.Insert(1, fEpikTimerPath);
      lPkg.LazCompilerOptions.OtherUnitFiles := lFiles.DelimitedText;
    finally
      lFiles.Free;
    end;
    lPasPkg.UnitOutputDirectory := lPkg.LazCompilerOptions.UnitOutputDirectory;
    lPkg.LazCompilerOptions.UnitOutputDirectory := fUnitOutputDirectory;
  end;
  LazarusIDE.DoSaveProject([]);
  PackageEditingInterface.DoSaveAllPackages([]);
end;

procedure TProfilerAddon.LoadResult;
begin
  LoadFromFile(fTargetDir + fTargetName + cSettingExtension);
  if fProcList.Count = 0 then begin
    ParseSources(False);
  end;
  UpdateUI;
end;

function TProfilerAddon.ParseSources(pInstrument: Boolean): Boolean;
var
  i: Integer;
  lLRS: TLazarusResourceStream;
  lList4: TLPPasPackageList;
  lList3: TLPPasUnitList;
  lList2: TLPPasClassList;
  lList: TLPPasProcList;
  lPkg: TIDEPackage;

begin
  Result := True;
  RestoreSources;
  if not fActive then
    Exit;
  WriteLn('*** LazProfiler: ParseSources(', BoolToStr(pInstrument, True), ')');

  lList4 := fOldPackageList;
  fOldPackageList := fPackageList;
  fPackageList := lList4;
  fPackageList.Clear;

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
    SaveXML(fTargetDir + fTargetName + cSettingExtension);
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
          lCurFile := TLPFile.Create(pas.CurFilename, pFile.PackageName);
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
    //Debugln('    + %s - %d - %s: %d', [lBlock.name, lBlock.level, TokenToStr(lBlock.token), pas.CurRow]);
  end;

  procedure PopBlock;
  begin
    if Assigned(lBlock) then begin
      //Debugln('    - %s - %d - %s', [lBlock.name, lBlock.level, TokenToStr(lBlock.token)]);
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
    //Debugln('    Clear stack');
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
      while not (pas.FetchToken in [tkWhitespace, tkBraceOpen, tkSemicolon]) do case pas.CurToken of
        tkDot: begin
          if lNameOfClass = '' then begin
            lNameOfClass := lName;
            lName := '';
          end else
            lName := lName + '.'
        end;
        tkIdentifier: begin
          lName := lName + pas.CurTokenString;
        end;
        else begin
          lName := lName + Copy(TokenToStr(pas.CurToken), 3);
        end;
      end;
      if lBlock.token = tkoperator then
        lName := 'operator ' + lName;
      lBlock.name := lName;
      lBlock.nameofclass := lNameOfClass;

    end;
  end;

begin
  Result := True;
  lCurFile := pFile;
  //WriteLn('  '+pFile.FileName);
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
  pas.TokenOptions := [toOperatorToken];
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
      and not (lBlock.token in [tkprocedure, tkfunction, tkconstructor, tkdestructor, tkoperator]) then
        NewBlock(pas.CurToken, '');
      tkBegin:
      begin
        if Assigned(lBlock)
        and (lBlock.token in [tkprocedure, tkfunction, tkconstructor, tkdestructor, tkoperator]) then begin
          if lProfiling then begin
            { insert: lazprofiler.enter() }
            lToken := lBlock.token;
            lName := lBlock.name;
            lNameOfClass := lBlock.nameofclass;
            lTempBlock := lBlock.parent;
            if Assigned(lTempBlock) and (lTempBlock.token in [tkprocedure, tkfunction, tkconstructor, tkdestructor, tkoperator]) then begin
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
            lBlock.procid := AddProc(lName, Integer(lToken), lNameOfClass, lUnitName, lCurFile.FileName, lCurFile.PackageName, pas.CurRow - 1);
            if lBlock.procid <> -1 then
              InsertEnter(pas.CurRow - 1, pas.CurColumn, lBlock.procid);
            //WriteLn('    +', lName, ' - ', IntToStr(lBlock.procid), ' - ', lCurFile.PackageName, '.', ExtractFileName(lCurFile.Filename));
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
        and (lBlock.token in [tkprocedure, tkfunction, tkconstructor, tkdestructor, tkoperator])
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
      tkProcedure,
      tkoperator:
        DoProc;
      tkidentifier:
        if UpperCase(pas.CurTokenString) = 'EXTERNAL' then
          PopBlock;
      tkclass,
      tkobject,
      tkRecord: begin
        NewBlock(pas.CurToken, '');
        while pas.FetchToken in [tkTab, tkWhitespace, tkComment, tkLineEnding] do;
        if pas.CurToken in [tkprocedure, tkfunction, tkconstructor, tkdestructor, tkoperator] then begin
          { class function/procedure/operator }
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
        //debugln('      IMPLEMENTATION');
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
            while Assigned(lTempBlock)
            and not (lTempBlock.token in [tkprocedure, tkfunction, tkconstructor, tkdestructor, tkoperator]) do
              lTempBlock := lTempBlock.parent;
            if Assigned(lTempBlock) then begin
              if lTempBlock.procid <> -1 then begin
                //WriteLn('*** LazProfiler: START-PROFILER: ', lCurFile.Filename, ' - ', IntToStr(pas.CurRow));
                lStartProc := lTempBlock;
                InsertStart(pas.CurRow - 1, pas.CurTokenPos.Column); // CurTokenPos needs FPC trunk 37235
                fAutoStart := False;
              end;
            end else begin
              IDEMessagesWindow.AddCustomMessage(mluFatal, 'LazProfiler: START-PROFILER: not in procedure/function', lCurFile.FileName, pas.CurRow, pas.CurTokenPos.Column); // CurTokenPos needs FPC trunk 37235
              exit(False);
            end;
          end;
        end else if lUpComment = 'STOP-PROFILER' then begin
          if not lProfiling then
            Continue;
          lTempBlock := lBlock;
          while Assigned(lTempBlock) and not (lTempBlock.token in [tkprocedure, tkfunction, tkconstructor, tkdestructor, tkoperator]) do lTempBlock := lTempBlock.parent;
          if Assigned(lTempBlock) then begin
            if lTempBlock.procid <> -1 then begin
              if not Assigned(lStartProc) then begin
                IDEMessagesWindow.AddCustomMessage(mluFatal, 'LazProfiler: STOP-PROFILER: Profiler not started', lCurFile.FileName, pas.CurRow, pas.CurTokenPos.Column); // CurTokenPos needs FPC trunk 37235
                exit(False);
              end else begin
                if lTempBlock = lStartProc then begin
                  //WriteLn('*** LazProfiler: STOP-PROFILER: ', lCurFile.Filename, ' - ', IntToStr(pas.CurRow));
                  InsertStop(pas.CurRow - 1, pas.CurTokenPos.Column);  // CurTokenPos needs FPC trunk 37235
                  lStartProc := nil;
                end else begin
                  IDEMessagesWindow.AddCustomMessage(mluFatal, 'LazProfiler: STOP-PROFILER: Profiler not started in same procedure/function', lCurFile.FileName, pas.CurRow, pas.CurTokenPos.Column); // CurTokenPos needs FPC trunk 37235
                  exit(False);
                end;
              end;
            end;
          end else begin
            IDEMessagesWindow.AddCustomMessage(mluFatal, 'LazProfiler: STOP-PROFILER: not in procedure/function', lCurFile.FileName, pas.CurRow, pas.CurTokenPos.Column); // CurTokenPos needs FPC trunk 37235
            exit(False);
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

procedure TProfilerAddon.RestoreModifiedSettings;
var
  i: Integer;
  lPasPkg: TLPPasPackage;
  lFiles: TStringList;
  lPkg: TIDEProjPackBase;
begin
  for i := 0 to fPackageList.Count -1 do begin
    lPasPkg := fPackageList[i];
    if lPasPkg.PackageIsProject then begin
      lPkg := fProject;
    end else begin
      lPkg := PackageEditingInterface.FindPackageWithName(fPackageList[i].PackageName);
    end;
    lFiles := TStringList.Create;
    try
      lFiles.Delimiter := ';';
      lFiles.StrictDelimiter := True;
      lFiles.DelimitedText := lPkg.LazCompilerOptions.OtherUnitFiles;
      if lFiles.Count > 1 then begin
        if lFiles[0] = fLazProfilerPath then
          lFiles.Delete(0);
        if lFiles[0] = fEpikTimerPath then
          lFiles.Delete(0);
        lPkg.LazCompilerOptions.OtherUnitFiles := lFiles.DelimitedText;
      end;
    finally
      lFiles.Free;
    end;
    if lPkg.LazCompilerOptions.UnitOutputDirectory = fUnitOutputDirectory then
      lPkg.LazCompilerOptions.UnitOutputDirectory := lPasPkg.UnitOutputDirectory;
  end;
  // hint: save will only happen if ToolStatus is itNone or itDebugger: needs trunc 60719
  if not (LazarusIDE.ToolStatus in [itNone, itDebugger]) then
    WriteLn('*** LazProfiler: RestoreModifiedSettings: Saving project and packages will not work. Use at least Lazarus trunk r60719.');
  LazarusIDE.DoSaveProject([]);
  PackageEditingInterface.DoSaveAllPackages([]);
end;

procedure TProfilerAddon.RestoreSources;
var
  i: Integer;
  lPkg: TIDEPackage;

  procedure DeleteRT(pDir: String);
  begin
    DeleteFile(pDir + cRunTimeFileName);
    DeleteFile(pDir + cCoreFileName);
    DeleteFile(pDir + cTimerFileName);
  end;

begin
  WriteLn('*** LazProfiler: RestoreSources');

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
  //DebugLn('  ' + pFile.FileName);
  lOrigFileName := LeftStr(pFile.Filename, length(pFile.Filename) - length(cBackupExtension));
  if FileExists(pFile.Filename) then begin
    DeleteFile(lOrigFileName);
    RenameFile(pFile.Filename, lOrigFileName);
    //todo: touch file here to force rebuild
  end;
end;

function TProfilerAddon.ProjectRunWithoutDebugBuilding(Sender: TObject;
  var Handled: boolean): TModalResult;
begin
  Result := mrOK;
  if fProfiling then begin
    ModifySettings;
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
    RestoreModifiedSettings;
  end;
end;

procedure TProfilerAddon.RunFinished(Sender: TObject);
begin
  //WriteLn('*** LazProfiler: RunFinished');
  LoadResult;
  if fProfiling then begin
    // enable CheckFilesOnDisk
    LazarusIDE.CheckFilesOnDiskEnabled := True; { needs trunk 56204 }
    // force rebuild on next run/compile
    if LazarusIDE.ActiveProject <> nil then
      LazarusIDE.ActiveProject.Flags := LazarusIDE.ActiveProject.Flags + [pfAlwaysBuild];
    // show results
    IDEWindowCreators.ShowForm(cProfilerFormName, True);
    if Assigned(ProfilerWindow) then
      ProfilerWindow.PageControl.ActivePageIndex := 0;
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
  //WriteLn('*** LazProfiler: ProjectOpened: '+pProject.ProjectInfoFile);
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
  SetDefaults;
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
    //WriteLn('*** LazProfiler: ProjectClose');
    fNeedsRebuild := pfAlwaysBuild in AProject.Flags;
    SaveXML(fTargetDir + fTargetName + cSettingExtension);
  end;
  if Assigned(ProfilerWindow) then
    ProfilerWindow.Data := nil;
  fProcList.Clear;
  fOldProcList.Clear;
end;

function TProfilerAddon.SaveEditorFile(Sender: TObject; aFile: TLazProjectFile;
  SaveStep: TSaveEditorFileStep; TargetFilename: string): TModalResult;
var
  i: Integer;
  lFile: TLPFile;
begin
  Result := mrOK;
  if fSourcesInstrumented
  and (SaveStep = sefsAfterWrite) then begin
    // prior r60687 ide checks if file on disk has been modified an writes it to disk
    // which destroys the instrumented source, so write it again
    TargetFilename := UpperCase(TargetFilename);
    for i := 0 to fFileList.Count - 1 do begin
      lFile := fFileList[i];
      if lFile.FilenameUp = TargetFilename then
        lFile.Save;
    end;
    for i := 0 to fIncludeList.Count - 1 do begin
      lFile := fIncludeList[i];
      if lFile.FilenameUp = TargetFilename then
        lFile.Save;
    end;
  end;
end;

constructor TProfilerAddon.Create;
begin
  inherited Create;
  fFileList := TLPFileList.Create(True);
  fOldPackageList := TLPPasPackageList.Create(True);
  fOldUnitList := TLPPasUnitList.Create(True);
  fOldClassList := TLPPasClassList.Create(True);
  fIncludeList := TLPFileList.Create(True);
  fOldProcList := TLPPasProcList.Create(True);
  fUnitOutputDirectory := SysUtils.GetTempDir(False) + 'LazProfiler' + PathDelim + '$(TargetCPU)-$(TargetOS)';
end;

destructor TProfilerAddon.Destroy;
begin
  fOldProcList.Free;
  fIncludeList.Free;
  fOldClassList.Free;
  fOldUnitList.Free;
  fOldPackageList.Free;
  fFileList.Free;
  inherited Destroy;
end;

procedure TProfilerAddon.CreateProfilerWindow(Sender: TObject; pFormName: string; var pForm: TCustomForm; DoDisableAutoSizing: boolean);
begin
  if CompareText(pFormName, cProfilerFormName)<>0 then exit;
  IDEWindowCreators.CreateForm(ProfilerWindow, TLazProfilerForm, DoDisableAutosizing, LazarusIDE.OwningComponent);
  pForm:=ProfilerWindow;
end;

procedure TProfilerAddon.UpdateUI;
begin
  if not Assigned(ProfilerWindow) then begin
    IDEWindowCreators.CreateForm(ProfilerWindow, TLazProfilerForm, False, Application);
    if Assigned(IDEDockMaster) then
      IDEDockMaster.MakeIDEWindowDockable(ProfilerWindow);
  end;
  ProfilerWindow.VST.Header.SortColumn := SortColumn;
  ProfilerWindow.VST.Header.SortDirection := TSortDirection(SortDirection);
  RunCmd.Enabled := fActive;
  if fActive then begin
    ProfilerWindow.Data := fProcList;
  end else begin
    ProfilerWindow.Data := Nil;
  end;
  ProfilerWindow.CBActive.Checked := fActive;
end;

procedure TProfilerAddon.Start(Sender: TObject);
var
  lTargetFileName: String;
  lProject: TLazProject;
  lPKg: TIDEPackage;
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

  lPKg := PackageEditingInterface.FindPackageWithName('LazProfiler');
  if Assigned(lPkg) then
    fLazProfilerPath := lPkg.Directory;
  lPKg := PackageEditingInterface.FindPackageWithName('etpackage');
  if Assigned(lPkg) then
    fEpikTimerPath := lPkg.Directory;

  RestoreSources;

  if LazarusIDE.DoBuildProject(crRun, [pbfSkipLinking]) <> mrOK then
    Exit;

  if (IDEMessagesWindow <> nil) then
    IDEMessagesWindow.Clear;

  fProfiling := True;
  if LazarusIDE.DoRunProjectWithoutDebug<>mrOK then begin
    { error while compiling profiled project }
    fProfiling := False;
    LazarusIDE.CheckFilesOnDiskEnabled := True; { needs trunk 56204 }
    RestoreModifiedSettings;
  end;
end;

procedure TProfilerAddon.ShowProfilerWindow(Sender: TObject);
begin
  IDEWindowCreators.ShowForm(cProfilerFormName, True);
  if Assigned(ProfilerWindow) then with ProfilerWindow do begin
    if fActive then
      PageControl.ActivePageIndex := 0
    else begin
      PageControl.ActivePageIndex := 1;
      CBActive.SetFocus;
    end;
  end;
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

  ProfilerWindow := nil;
  Addon := TProfilerAddon.Create;

finalization

  Addon.Free;

end.
