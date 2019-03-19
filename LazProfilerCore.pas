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
  Generics.Defaults, DOM,
  SysUtils;

type

  TIntegerList = specialize TList<Integer>;


  { TLPBaseType }

  TLPBaseType = class
  private
  protected
    fExpanded: Boolean;
    function XMLNodeName: String; virtual; abstract;
    procedure SaveXMLNode(pDoc: TXMLDocument; pNode: TDOMElement); virtual;
    procedure LoadXMLNode(pNode: TDOMNode); virtual;
    procedure InternalCreateFormXML(pNode: TDOMNode); virtual; abstract;
  public
    constructor CreateFromXML(pNode: TDOMNode);
    property Expanded: Boolean read fExpanded write fExpanded;
    procedure SaveXML(pDoc: TXMLDocument; pNode: TDOMNode);
  end;


  { TLPPasPackage }

  TLPPasPackage = class(TLPBaseType)
  private
    procedure SetPackageName(pPackageName: String);
  protected
    fPackageName: String;
    fPackageNameUp: String;
    fIsProject: Boolean;
    fUnitOutputDirectory: String;
    function XMLNodeName: String; override;
    procedure SaveXMLNode(pDoc: TXMLDocument; pNode: TDOMElement); override;
    procedure LoadXMLNode(pNode: TDOMNode); override;
    procedure InternalCreateFormXML(pNode: TDOMNode); override;
  public
    constructor Create(pPackageName: String);
    property PackageName: String read fPackageName write SetPackageName;
    property PackageNameUp: String read fPackageNameUp;
    property PackageIsProject: Boolean read fIsProject;
    property UnitOutputDirectory: String read fUnitOutputDirectory write fUnitOutputDirectory;
  end;


  { TLPPasPackageList }

  TLPPasPackageList = class(specialize TObjectList<TLPPasPackage>)
  private
  protected
  public
    function IndexOf(pPackageName: String): SizeInt; overload;
    procedure SaveToStringList(pList: TStringList);
    procedure LoadFromStringList(pList: TStringList; pVersion: Integer);
    procedure SaveXML(pDoc: TXMLDocument);
    procedure LoadXML(pDoc: TXMLDocument);
  end;


  { TLPPasUnit }

  TLPPasUnit = class(TLPPasPackage)
  private
    procedure SetFileName(pFileName: string);
    procedure SetUnitName(pUnitName: String);
  protected
    fUnitName,
    fFileName,
    fUnitNameUp,
    fFileNameUp: String;
    fPasPackage: TLPPasPackage;
    function XMLNodeName: String; override;
    procedure SaveXMLNode(pDoc: TXMLDocument; pNode: TDOMElement); override;
    procedure InternalCreateFormXML(pNode: TDOMNode); override;
  public
    constructor Create(pUnitName, pFileName, pPackageName: String);
    property PasPackage: TLPPasPackage read fPasPackage write fPasPackage;
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
    function IndexOf(pUnitName, pFileName, pPackageName: String): SizeInt; overload;
    procedure SaveToStringList(pList: TStringList);
    procedure SaveXML(pDoc: TXMLDocument);
    procedure LoadFromStringList(pList: TStringList; pVersion: Integer);
    procedure LoadXML(pDoc: TXMLDocument);
  end;


  { TLPPasClass }

  TLPPasClass = class(TLPPasUnit)
  private
    procedure SetNameOfClass(pNameOfClass: String);
  protected
    fNameOfClass,
    fNameOfClassUp: String;
    fPasUnit: TLPPasUnit;
    function XMLNodeName: String; override;
    procedure SaveXMLNode(pDoc: TXMLDocument; pNode: TDOMElement); override;
    procedure InternalCreateFormXML(pNode: TDOMNode); override;
  public
    constructor Create(pNameOfClass, pUnitName, pFileName, pPackageName: String);
    property PasUnit: TLPPasUnit read fPasUnit write fPasUnit;
    property NameOfClass: String read fNameOfClass write SetNameOfClass;
    property NameOfClassUp: String read fNameOfClassUp;
  end;


  { TLPPasClassList }

  TLPPasClassList = class(specialize TObjectList<TLPPasClass>)
  private
  protected
  public
    function IndexOf(pNameOfClass, pUnitName, pFileName, pPackageName: String): SizeInt; overload;
    procedure SaveToStringList(pList: TStringList);
    procedure SaveXML(pDoc: TXMLDocument);
    procedure LoadFromStringList(pList: TStringList; pVersion: Integer);
    procedure LoadXML(pDoc: TXMLDocument);
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
    fGross,
    fAvgNet,
    fAvgGross: QWord;
    fCount: Integer;
    fKind: Integer;
    fPerNet,
    fPerGross: LongWord;
    fCalls,
    fCalledBy: TLPPasProcList;
    fCallsCount,
    fCalledByCount: TIntegerList;
    fInstrument: Boolean;
    fPasClass: TLPPasClass;
    fCountStr,
    fSumNetStr,
    fSumGrossStr,
    fAvgNetStr,
    fAvgGrossStr,
    fPerNetStr,
    fPerGrossStr: String;
    procedure SetFileName(pFileName: String);
    procedure SetName(pName: String);
  protected
    function XMLNodeName: String; override;
    procedure SaveXMLNode(pDoc: TXMLDocument; pNode: TDOMElement); override;
    procedure LoadXMLNode(pNode: TDOMNode); override;
    procedure InternalCreateFormXML(pNode: TDOMNode); override;
  public
    constructor Create(pName: String; pKind: Integer; pNameOfClass, pUnitName, pFileName, pPackageName: String; pRow: Integer);
    destructor Destroy; override;
    procedure Init;
    procedure Calls(pProc: TLPPasProc);
    procedure CalledBy(pProc: TLPPasProc);
    procedure SetSums(pNetSum, pGrossSum: QWord);
    property Name: String read fName write SetName;
    property Row: Integer read fRow write fRow;
    property Count: Integer read fCount write fCount;
    property Kind: Integer read fKind write fKind;
    property Net: QWord read fNet write fNet;
    property Gross: QWord read fGross write fGross;
    property AvgNet: QWord read fAvgNet;
    property AvgGross: QWord read fAvgGross;
    property Instrument: Boolean read fInstrument write fInstrument;
    property NameUp: string read fNameUp;
    property PerNet: LongWord read fPerNet;
    property PerGross: LongWord read fPerGross;
    property PasClass: TLPPasClass read fPasClass write fPasClass;
    property CountStr: String read fCountStr;
    property SumNetStr: String read fSumNetStr;
    property SumGrossStr: String read fSumGrossStr;
    property AvgNetStr: String read fAvgNetStr;
    property AvgGrossStr: String read fAvgGrossStr;
    property PerNetStr: String read fPerNetStr;
    property PerGrossStr: String read fPerGrossStr;
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
    function IndexOf(pName, pNameOfClass, pUnitName, pFileName, pPackageName: String): SizeInt; overload;
    procedure SaveToStringList(pList: TStringList);
    procedure SaveXML(pDoc: TXMLDocument);
    procedure LoadFromStringList(pList: TStringList; pVersion: Integer);
    procedure LoadXML(pDoc: TXMLDocument);
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
    fPackageList: TLPPasPackageList;
    fUnitList: TLPPasUnitList;
    fClassList: TLPPasClassList;
    fProcList: TLPPasProcList;
    fActive: Boolean;
    fAutoStart: Boolean;
    fNeedsRebuild: Boolean;
    fSortColumn: Integer;
    fSortDirection: Integer;
    fSettingsVersion: Integer;
    procedure SaveConfig(pDoc: TXMLDocument);
    procedure LoadConfig(pDoc: TXMLDocument);
    procedure SaveToFile(pFileName: String);
    procedure SaveXML(pFileName: String);
    function LoadFromFile(pFileName: String): Boolean;
    function LoadXML(pFileName: String): Boolean;
    procedure SetDefaults;
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
    fPauseCount: array of Integer;
    fPauseStartTicks,
    fOffTicks: array of TickType;
    fLoaded: Boolean;
    Ticks: function: Ticktype of object;
    fLock: TRTLCriticalSection;
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
  end;

  function IntToTime(pVal: Int64): String;

const
  cBackupExtension  = '.lazprofiler_backup';

  cCoreFileName = 'LazProfilerCore.pas';
  cRunTimeFileName = 'LazProfilerRunTime.pas';
  cTimerFileName = 'EpikTimer.pas';

  cSettingExtension = '.lazprofiler_setting';
  cSettingVersion = 3;

  { XML node strings }
  cXMLLazProfiler = 'lazprofiler';
  cXMLConfig =        'config';
  cXMLPackages =      'packages';
  cXMLPackage =         'package';
  cXMLUnits =         'units';
  cXMLUnit =            'unit';
  cXMLClasses =       'classes';
  cXMLClass =           'class';
  cXMLProcs =         'procs';
  cXMLProc =            'proc';
  cXMLRuns =          'runs';
  cXMLRun =             'run';
  cXMLTimings =       'timings';
  cXMLTiming =          'timing';
  cXMLUsings =        'usings';
  cXMLUsing =           'using';
  cXMLCalls =             'calls';
  cXMLCalledBy =          'calledby';

  { XML attribute strings }
  cXMLConfig_AutoStart = 'autostart';
  cXMLConfig_NeedsRebuild = 'needsrebuild';
  cXMLConfig_Version = 'version';
  cXMLConfig_SortColumn = 'sortcolumn';
  cXMLConfig_SortDirection = 'sortdirection';
  cXMLConfig_Active = 'active';

  { ProfilerWindow Result TreeView }
  cColumnCount = 11;
  cNameCol     = 0;
  cClassCol    = 1;
  cUnitCol     = 2;
  cPackageCol  = 3;
  cCountCol    = 4;
  cPerNetCol   = 5;
  cSumNetCol   = 6;
  cPerGrossCol = 7;
  cSumGrossCol = 8;
  cAvgNetCol   = 9;
  cAvgGrossCol = 10;

implementation

uses
  LazFileUtils,
  LazUTF8,
  strutils,
  XMLRead,
  XMLWrite;

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


{ TLPBaseType }

procedure TLPBaseType.SaveXMLNode(pDoc: TXMLDocument; pNode: TDOMElement);
begin
  pNode.SetAttribute('expanded', BoolToStr(Expanded, True));
end;

procedure TLPBaseType.LoadXMLNode(pNode: TDOMNode);
var
  lExpandedAttr: TDOMNode;
begin
  lExpandedAttr := pNode.Attributes.GetNamedItem('expanded');
  if Assigned(lExpandedAttr) then
    fExpanded := StrToBool(lExpandedAttr.NodeValue);
end;

constructor TLPBaseType.CreateFromXML(pNode: TDOMNode);
begin
  InternalCreateFormXML(pNode);
  LoadXMLNode(pNode);
end;

procedure TLPBaseType.SaveXML(pDoc: TXMLDocument; pNode: TDOMNode);
var
  lNode: TDOMElement;
begin
  lNode := pDoc.CreateElement(XMLNodeName);
  SaveXMLNode(pDoc, lNode);
  pNode.AppendChild(lNode);
end;

{ TLPPasPackage }

procedure TLPPasPackage.SetPackageName(pPackageName: String);
begin
  if fPackageName = pPackageName then Exit;
  fPackageName := pPackageName;
  fPackageNameUp := UpperCase(pPackageName);
end;

function TLPPasPackage.XMLNodeName: String;
begin
  Result := cXMLPackage;
end;

procedure TLPPasPackage.SaveXMLNode(pDoc: TXMLDocument; pNode: TDOMElement);
begin
  inherited SaveXMLNode(pDoc, pNode);
  pNode.SetAttribute('packagename', fPackageName);
  pNode.SetAttribute('unitoutputdirectory', fPackageName);
end;

procedure TLPPasPackage.LoadXMLNode(pNode: TDOMNode);
var
  lUnitOutputDirectoryAttr: TDOMNode;
begin
  inherited LoadXMLNode(pNode);
  lUnitOutputDirectoryAttr := pNode.Attributes.GetNamedItem('unitoutputdirectory');
  if Assigned(lUnitOutputDirectoryAttr) then
    fUnitOutputDirectory := lUnitOutputDirectoryAttr.NodeValue;
end;

procedure TLPPasPackage.InternalCreateFormXML(pNode: TDOMNode);
var
  lPackageName: DOMString;
  lPackageNameAttr: TDOMNode;
begin;
  lPackageNameAttr := pNode.Attributes.GetNamedItem('packagename');
  if Assigned(lPackageNameAttr) then
    lPackageName := lPackageNameAttr.NodeValue;

  Create(lPackageName);
end;

constructor TLPPasPackage.Create(pPackageName: String);
begin
  fIsProject := pPackageName = '?';
  fPackageName := pPackageName;
  fPackageNameUp := UpperCase(fPackageName);
end;


{ TLPPasPackageList }

function TLPPasPackageList.IndexOf(pPackageName: String): SizeInt;
var
  i: Integer;
  lPasPackage: TLPPasPackage;
begin
  pPackageName := UpperCase(pPackageName);
  for i := 0 to Count - 1 do begin
    lPasPackage := Items[i];
    if (lPasPackage.PackageNameUp = pPackageName) then
      Exit(i);
  end;
  Result := -1;
end;

procedure TLPPasPackageList.SaveToStringList(pList: TStringList);
begin

end;

procedure TLPPasPackageList.LoadFromStringList(pList: TStringList;
  pVersion: Integer);
begin

end;

procedure TLPPasPackageList.SaveXML(pDoc: TXMLDocument);
var
  lNode: TDOMElement;
  i: Integer;
begin
  lNode := pDoc.CreateElement(cXMLPackages);
  pDoc.DocumentElement.Appendchild(lNode);
  for i := 0 to Count - 1 do
    Items[i].SaveXML(pDoc, lNode);
end;

procedure TLPPasPackageList.LoadXML(pDoc: TXMLDocument);
var
  lNodes, lNode: TDOMNode;
  i: Integer;
begin
  Clear;
  lNodes := pDoc.DocumentElement.FindNode(cXMLPackages);
  if Assigned(lNodes) then with lNodes.ChildNodes do begin
    for i := 0 to Count - 1 do begin
      lNode := Item[i];
      Add(TLPPasPackage.CreateFromXML(lNode));
    end;
  end;
end;


{ TLPPasUnitList }

function TLPPasUnitList.IndexOf(pUnitName, pFileName, pPackageName: String): SizeInt;
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

procedure TLPPasUnitList.SaveXML(pDoc: TXMLDocument);
var
  lNode: TDOMElement;
  i: Integer;
begin
  lNode := pDoc.CreateElement(cXMLUnits);
  pDoc.DocumentElement.Appendchild(lNode);
  for i := 0 to Count - 1 do
    Items[i].SaveXML(pDoc, lNode);
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
        Add(TLPPasUnit.Create(lLine[0], lLine[1], '?'));
        Last.Expanded := StrToBool(lLine[2]);
      end;
    end;
  finally
    lLine.Free;
  end;
end;

procedure TLPPasUnitList.LoadXML(pDoc: TXMLDocument);
var
  lNodes, lNode: TDOMNode;
  i: Integer;
begin
  Clear;
  lNodes := pDoc.DocumentElement.FindNode(cXMLUnits);
  if Assigned(lNodes) then with lNodes.ChildNodes do begin
    for i := 0 to Count - 1 do begin
      lNode := Item[i];
      Add(TLPPasUnit.CreateFromXML(lNode));
    end;
  end;
end;


{ TLPPasClassList }

function TLPPasClassList.IndexOf(pNameOfClass, pUnitName, pFileName, pPackageName: String): SizeInt;
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

procedure TLPPasClassList.SaveXML(pDoc: TXMLDocument);
var
  lNode: TDOMElement;
  i: Integer;
begin
  lNode := pDoc.CreateElement(cXMLClasses);
  pDoc.DocumentElement.Appendchild(lNode);
  for i := 0 to Count - 1 do
    Items[i].SaveXML(pDoc, lNode);
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
        Add(TLPPasClass.Create(lLine[0], lLine[1], lLine[2], '?'));
        Last.Expanded := StrToBool(lLine[3]);
      end;
    end;
  finally
    lLine.Free;
  end;
end;

procedure TLPPasClassList.LoadXML(pDoc: TXMLDocument);
var
  lNodes, lNode: TDOMNode;
  i: Integer;
begin
  Clear;
  lNodes := pDoc.DocumentElement.FindNode(cXMLClasses);
  if Assigned(lNodes) then with lNodes.ChildNodes do begin
    for i := 0 to Count - 1 do begin
      lNode := Item[i];
      Add(TLPPasClass.CreateFromXML(lNode));
    end;
  end;
end;


{ TLPPasClass }

procedure TLPPasClass.SetNameOfClass(pNameOfClass: String);
begin
  if fNameOfClass = pNameOfClass then Exit;
  fNameOfClass := pNameOfClass;
  fNameOfClassUp := UpperCase(pNameOfClass);
end;

function TLPPasClass.XMLNodeName: String;
begin
  Result := cXMLClass;
end;

procedure TLPPasClass.SaveXMLNode(pDoc: TXMLDocument; pNode: TDOMElement);
begin
  inherited SaveXMLNode(pDoc, pNode);
  pNode.SetAttribute('nameofclass', fNameOfClass);
end;

procedure TLPPasClass.InternalCreateFormXML(pNode: TDOMNode);
var
  lPackageName, lFileName, lUnitName, lNameOfClass: DOMString;
  lUnitNameAttr, lFileNameAttr, lPackageNameAttr, lNameOfClassAttr: TDOMNode;
begin;
  lNameOfClassAttr := pNode.Attributes.GetNamedItem('nameofclass');
  if Assigned(lNameOfClassAttr) then
    lNameOfClass := lNameOfClassAttr.NodeValue;

  lUnitNameAttr := pNode.Attributes.GetNamedItem('unitname');
  if Assigned(lUnitNameAttr) then
    lUnitName := lUnitNameAttr.NodeValue;

  lFileNameAttr := pNode.Attributes.GetNamedItem('filename');
  if Assigned(lFileNameAttr) then
    lFileName := lFileNameAttr.NodeValue;

  lPackageNameAttr := pNode.Attributes.GetNamedItem('packagename');
  if Assigned(lPackageNameAttr) then
    lPackageName := lPackageNameAttr.NodeValue;

  Create(lNameOfClass, lUnitName, lFileName, lPackageName);
end;

constructor TLPPasClass.Create(pNameOfClass, pUnitName, pFileName, pPackageName: String);
begin
  inherited Create(pUnitName, pFileName, pPackageName);
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

function TLPPasUnit.XMLNodeName: String;
begin
  Result := cXMLUnit;
end;

procedure TLPPasUnit.SaveXMLNode(pDoc: TXMLDocument; pNode: TDOMElement);
begin
  inherited SaveXMLNode(pDoc, pNode);
  pNode.SetAttribute('unitname', fUnitName);
  pNode.SetAttribute('filename', fFileName);
end;

procedure TLPPasUnit.InternalCreateFormXML(pNode: TDOMNode);
var
  lPackageName, lFileName, lUnitName: DOMString;
  lUnitNameAttr, lFileNameAttr, lPackageNameAttr, lExpandedAttr: TDOMNode;
begin;
  lUnitNameAttr := pNode.Attributes.GetNamedItem('unitname');
  if Assigned(lUnitNameAttr) then
    lUnitName := lUnitNameAttr.NodeValue;

  lFileNameAttr := pNode.Attributes.GetNamedItem('filename');
  if Assigned(lFileNameAttr) then
    lFileName := lFileNameAttr.NodeValue;

  lPackageNameAttr := pNode.Attributes.GetNamedItem('packagename');
  if Assigned(lPackageNameAttr) then
    lPackageName := lPackageNameAttr.NodeValue;

  Create(lUnitName, lFileName, lPackageName);
end;

constructor TLPPasUnit.Create(pUnitName, pFileName, pPackageName: String);
begin
  inherited Create(pPackageName);
  fUnitName := pUnitName;
  fUnitNameUp := UpperCase(pUnitName);
  fFileName := pFileName;
  fFileNameUp := UpperCase(pFileName);
end;


{ TCustomLazProfiler }

procedure TCustomLazProfiler.SaveConfig(pDoc: TXMLDocument);
var
  lNode: TDOMElement;
begin
  lNode := pDoc.CreateElement(cXMLConfig);
  pDoc.DocumentElement.AppendChild(lNode);
  lNode.SetAttribute(cXMLConfig_Version, IntTostr(cSettingVersion));
  lNode.SetAttribute(cXMLConfig_AutoStart, BoolToStr(fAutoStart, True));
  lNode.SetAttribute(cXMLConfig_NeedsRebuild, BoolToStr(fNeedsRebuild, True));
  lNode.SetAttribute(cXMLConfig_SortColumn, IntToStr(fSortColumn));
  lNode.SetAttribute(cXMLConfig_SortDirection, IntToStr(fSortDirection));
  lNode.SetAttribute(cXMLConfig_Active, BoolToStr(fActive, True));
end;

procedure TCustomLazProfiler.LoadConfig(pDoc: TXMLDocument);
var
  lSettingsVersionAttr, lAutostartAttr, lNeedsRebuildAttr,
    lSortColumnAttr, lSortDirectionAttr, lNode, lActiveAttr: TDOMNode;
begin;
  lNode := pDoc.DocumentElement.FindNode(cXMLConfig);
  if Assigned(lNode) then begin
    lSettingsVersionAttr := lNode.Attributes.GetNamedItem(cXMLConfig_Version);
    if Assigned(lSettingsVersionAttr) then
      fSettingsVersion := StrToInt(lSettingsVersionAttr.NodeValue);

    lAutostartAttr := lNode.Attributes.GetNamedItem(cXMLConfig_AutoStart);
    if Assigned(lAutostartAttr) then
      fAutoStart := StrToBool(lAutostartAttr.NodeValue);

    lNeedsRebuildAttr := lNode.Attributes.GetNamedItem(cXMLConfig_NeedsRebuild);
    if Assigned(lNeedsRebuildAttr) then
      fNeedsRebuild := StrToBool(lNeedsRebuildAttr.NodeValue);

    lSortColumnAttr := lNode.Attributes.GetNamedItem(cXMLConfig_SortColumn);
    if Assigned(lSortColumnAttr) then
      fSortColumn := StrToInt(lSortColumnAttr.NodeValue);

    lSortDirectionAttr := lNode.Attributes.GetNamedItem(cXMLConfig_SortDirection);
    if Assigned(lSortDirectionAttr) then
      fSortDirection := StrToInt(lSortDirectionAttr.NodeValue);

    lActiveAttr := lNode.Attributes.GetNamedItem(cXMLConfig_Active);
    if Assigned(lActiveAttr) then
      fActive := StrToBool(lActiveAttr.NodeValue);
  end;
end;

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

procedure TCustomLazProfiler.SaveXML(pFileName: String);
var
  lDoc: TXMLDocument;
  lRootNode: TDOMElement;
begin
  lDoc := TXMLDocument.Create;
  try
    lRootNode := lDoc.CreateElement(cXMLLazProfiler);
    lDoc.AppendChild(lRootNode);
    SaveConfig(lDoc);
    fPackageList.SaveXML(lDoc);
    fUnitList.SaveXML(lDoc);
    fClassList.SaveXML(lDoc);
    fProcList.SaveXML(lDoc);
    WriteXMLFile(lDoc, pFileName);
  finally
    lDoc.Free;
  end;
end;

function TCustomLazProfiler.LoadFromFile(pFileName: String): Boolean;
var
  lFile, lLine, lTemp: TStringList;
  lVersion, lUnitCount, lClassCount, lProcCount: LongInt;
  lPasClass: TLPPasClass;
  lPasUnit: TLPPasUnit;
  lPasProc: TLPPasProc;
  lPasPackage: TLPPasPackage;
  i, j, p: Integer;
begin
  if not FileExists(pFileName) then
    Exit(False);
  Result := True;
  fProcList.Clear;
  lFile := TStringList.Create;
  try
    lFile.LoadFromFile(pFileName);
    if lFile.Count >= 1 then begin
      if Pos('<?xml version=', lFile[0]) = 1 then begin
        lFile.Clear;
        LoadXML(pFileName);
      end else begin
        //WriteLn('*** LazProfiler: LoadFromFile: '+pFileName);
        lLine := TStringList.Create;
        lTemp := TStringList.Create;
        try
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
          fPackageList.Add(TLPPasPackage.Create('?'));
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
            // search package
            j := fPackageList.IndexOf(lPasProc.PackageName);
            if j >= 0 then begin
              lPasPackage := fPackageList[j]
            end else begin
              lPasPackage := TLPPasPackage.Create(lPasProc.PackageName);
              fPackageList.Add(lPasPackage);
            end;
            // search unit
            j := fUnitList.IndexOf(lPasProc.UnitName, lPasProc.FileName, lPasProc.PackageName);
            if j >= 0 then begin
              lPasUnit := fUnitList[j]
            end else begin
              lPasUnit := TLPPasUnit.Create(lPasProc.UnitName, lPasProc.FileName, lPasProc.PackageName);
              fUnitList.Add(lPasUnit);
            end;
            // search class
            j := fClassList.IndexOf(lPasProc.NameOfClass, lPasProc.UnitName, lPasProc.FileName, lPasProc.PackageName);
            if j >= 0 then begin
              lPasClass := fClassList[j]
            end else begin
              lPasClass := TLPPasClass.Create(lPasProc.NameOfClass, lPasProc.UnitName, lPasProc.FileName, lPasProc.PackageName);
              fClassList.Add(lPasClass);
            end;
            // connect
            lPasUnit.PasPackage := lPasPackage;
            lPasClass.PasPackage := lPasPackage;
            lPasClass.PasUnit := lPasUnit;
            lPasProc.PasPackage := lPasPackage;
            lPasProc.PasUnit := lPasUnit;
            lPasProc.PasClass := lPasClass;
          end;
        finally
          lTemp.Free;
          lLine.Free;
        end;
      end;
    end;
    //DebugLn('   Count='+IntToStr(fProcList.Count));
  finally
    lFile.Free;
  end;
end;

function TCustomLazProfiler.LoadXML(pFileName: String): Boolean;
var
  lDoc: TXMLDocument;
  lNodes, lNode: TDOMNode;
  i: Integer;
  lPasProc: TLPPasProc;
  lPasPackage: TLPPasPackage;
  lPasUnit: TLPPasUnit;
  lPasClass: TLPPasClass;
  j: SizeInt;
begin
  //WriteLn('*** LazProfiler: LoadFromXMLFile: '+pFileName);
  if not FileExists(pFileName) then
    Exit(False);
  Result := True;
  fProcList.Clear;

  ReadXMLFile(lDoc, pFileName);
  try
    LoadConfig(lDoc);
    fPackageList.LoadXML(lDoc);
    fUnitList.LoadXML(lDoc);
    fClassList.LoadXML(lDoc);
    fProcList.LoadXML(lDoc);
    for i := 0 to fProcList.Count - 1 do begin
      lPasProc := fProcList[i];
      // search package
      j := fPackageList.IndexOf(lPasProc.PackageName);
      if j >= 0 then begin
        lPasPackage := fPackageList[j]
      end else begin
        lPasPackage := TLPPasPackage.Create(lPasProc.PackageName);
        fPackageList.Add(lPasPackage);
      end;
      // search unit
      j := fUnitList.IndexOf(lPasProc.UnitName, lPasProc.FileName, lPasProc.PackageName);
      if j >= 0 then begin
        lPasUnit := fUnitList[j]
      end else begin
        lPasUnit := TLPPasUnit.Create(lPasProc.UnitName, lPasProc.FileName, lPasProc.PackageName);
        fUnitList.Add(lPasUnit);
      end;
      // search class
      j := fClassList.IndexOf(lPasProc.NameOfClass, lPasProc.UnitName, lPasProc.FileName, lPasProc.PackageName);
      if j >= 0 then begin
        lPasClass := fClassList[j]
      end else begin
        lPasClass := TLPPasClass.Create(lPasProc.NameOfClass, lPasProc.UnitName, lPasProc.FileName, lPasProc.PackageName);
        fClassList.Add(lPasClass);
      end;
      // connect
      lPasUnit.PasPackage := lPasPackage;
      lPasClass.PasPackage := lPasPackage;
      lPasClass.PasUnit := lPasUnit;
      lPasProc.PasPackage := lPasPackage;
      lPasProc.PasUnit := lPasUnit;
      lPasProc.PasClass := lPasClass;
    end;

    lNodes := lDoc.FindNode(cXMLRuns);
    if Assigned(lNodes) then with lNodes.ChildNodes do begin
      for i := 0 to Count - 1 do begin
        lNode := Item[i];
        //Add(TprEnv.CreateFromXML(lEnvNode));
      end;
    end;
  finally
    lDoc.Free;
  end;

end;

procedure TCustomLazProfiler.SetDefaults;
begin
  fActive := False;
  fAutoStart := True;
  fNeedsRebuild := False;
  fSortColumn := cPackageCol;
  fSortDirection := 0;
end;

constructor TCustomLazProfiler.Create;
begin
  inherited Create;
  fPackageList := TLPPasPackageList.Create(True);
  fUnitList := TLPPasUnitList.Create(True);
  fClassList := TLPPasClassList.Create(True);
  fProcList := TLPPasProcList.Create(True);
  SetDefaults;
end;

destructor TCustomLazProfiler.Destroy;
begin
  fProcList.Free;
  fClassList.Free;
  fUnitList.Free;
  fPackageList.Free;
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

procedure TLPPasProcList.SaveXML(pDoc: TXMLDocument);
var
  lNode: TDOMElement;
  i: Integer;
begin
  lNode := pDoc.CreateElement(cXMLProcs);
  pDoc.DocumentElement.Appendchild(lNode);
  for i := 0 to Count - 1 do
    Items[i].SaveXML(pDoc, lNode);
end;

function TLPPasProcList.IndexOf(pName, pNameOfClass, pUnitName, pFileName, pPackageName: String): SizeInt;
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
          Add(TLPPasProc.Create(lLine[0], 0, lLine[1], lLine[2], lLine[3], '?', StrToInt(lLine[4])));
          Last.Count := StrToInt(lLine[5]);
          Last.Net := StrToQWord(lLine[6]);
          Last.Gross := StrToQWord(lLine[7]);
          Last.Instrument := StrToBool(lLine[8]);
        end else begin
          Add(TLPPasProc.Create(lLine[0], StrToInt(lLine[1]), lLine[2], lLine[3], lLine[4], '?', StrToInt(lLine[5])));
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

procedure TLPPasProcList.LoadXML(pDoc: TXMLDocument);
var
  lNodes, lNode: TDOMNode;
  i: Integer;
  lNetSum, lGrossSum: QWord;
  lProc: TLPPasProc;
begin
  Clear;
  lNetSum := 0;
  lGrossSum := 0;
  lNodes := pDoc.DocumentElement.FindNode(cXMLProcs);
  if Assigned(lNodes) then begin
    with lNodes.ChildNodes do begin
      for i := 0 to Count - 1 do begin
        lNode := Item[i];
        lProc := TLPPasProc.CreateFromXML(lNode);
        Add(lProc);
        lNetSum := lNetSum + lProc.Net;
        lGrossSum := lGrossSum + lProc.Gross;
      end;
    end;
    for i := 0 to Count - 1 do
      Items[i].SetSums(lNetSum, lGrossSum);
  end;
end;


{ TLazProfiler }

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
  i: SizeInt;
begin
  Result := fThreads.IndexOf(Pointer(pThread));
  if Result = -1 then begin
    Result := fThreads.Add(Pointer(pThread));
    if fThreads.Count > Length(fCurStackFrame) then begin
      SetLength(fCurStackFrame, fThreads.Count + 100);
      SetLength(fPauseCount, fThreads.Count + 100);
      SetLength(fPauseStartTicks, fThreads.Count + 100);
      SetLength(fOffTicks, fThreads.Count + 100);
      for i := fThreads.Count to fThreads.Count + 99 do begin
        fPauseCount[i] := 0;
        fPauseStartTicks[i] := 0;
        fOffTicks[i] := 0;
      end;
    end;
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
      WriteLn('TLazProfiler.EnterProfiling: pProcID >= fProcList.Count: '+IntToStr(pProcID)+'>='+IntToStr(fProcList.Count));
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
      WriteLn('TLazProfiler.ExitProfiling: pProcID >= fProcList.Count: '+IntToStr(pProcID)+'>='+IntToStr(fProcList.Count));
      Exit;
    end;
    if lCurStackFrame.fPasProc <> fProcList[pProcID] then
      WriteLn('TLazProfiler.ExitProfiling: Stack mismatch: '+lCurStackFrame.fPasProc.Name+'<->'+fProcList[pProcID].name);
    if fPauseCount[lIdx] = 0 then begin
      lCurStackFrame.fOff := fOffTicks[lIdx];
      fOffTicks[lIdx] := 0;
    end else
      lCurStackFrame.fOff := 0;
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
  WriteLn('### LazProfiler: Start');
end;

procedure TLazProfiler.PauseProfiling;
var
  lIdx: Integer;
begin
  lIdx := ThreadIndex(ThreadID);
  if fPauseCount[lIdx] = 0 then
    fPauseStartTicks[lIdx] := Ticks();
  fPauseCount[lIdx] := fPauseCount[lIdx] + 1;
end;

procedure TLazProfiler.ContinueProfiling;
var
  lIdx: Integer;
begin
  lIdx := ThreadIndex(ThreadID);
  fPauseCount[lIdx] := fPauseCount[lIdx] - 1;
  if fRunning then begin
    if fPauseCount[lIdx] = 0 then begin
      if fPauseStartTicks[lIdx] <> 0 then
        fOffTicks[lIdx] := fOffTicks[lIdx] + (Ticks() - fPauseStartTicks[lIdx])
      else
        fOffTicks[lIdx] := 0;
      fPauseStartTicks[lIdx] := 0;
    end;
  end else begin
    fOffTicks[lIdx] := 0;
    fPauseStartTicks[lIdx] := 0;
  end;
end;

procedure TLazProfiler.StopProfiling;
begin
  fRunning := False;
  WriteLn('### LazProfiler: Stop');
end;

constructor TLazProfiler.Create(pProgramm: String);
var
  lCurStackFrame: TLPStackFrame;
  i: Integer;
begin
  inherited Create;
  InitCriticalSection(fLock);
  fName := AppendPathDelim(ExtractFileDir(pProgramm))+ExtractFileNameOnly(pProgramm);
  fTimer := TEpikTimer.Create(nil);
  if fTimer.HWCapabilityDataAvailable and fTimer.HWTickSupportAvailable then begin
    Ticks := @HWTicks;
    fTicks := fTimer.HWTimebase.TicksFrequency;
    //WriteLn('### LazProfiler: HWTicks used. Freq='+IntToStr(fTicks));
  end else begin
    Ticks := @SysTicks;
    fTicks := fTimer.SysTimebase.TicksFrequency;
    //WriteLn('### LazProfiler: SysTicks used. Freq='+IntToStr(fTicks));
  end;
  fMasterProc := TLPPasProc.Create('Master', 0, '', '', '', '', -1);
  fThreads := TList.Create;
  lCurStackFrame := TLPStackFrame.Create(fMasterProc, nil);
  lCurStackFrame.fTicksInit := Ticks();
  lCurStackFrame.fTicksStart := lCurStackFrame.fTicksInit;
  SetLength(fCurStackFrame, 100);
  SetLength(fPauseCount, 100);
  SetLength(fPauseStartTicks, 100);
  SetLength(fOffTicks, 100);
  for i := 0 to 99 do begin
    fPauseCount[i] := 0;
    fPauseStartTicks[i] := 0;
    fOffTicks[i] := 0;
  end;
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
  SetLength(fPauseCount, 0);
  SetLength(fPauseStartTicks, 0);
  SetLength(fOffTicks, 0);
  fTimer.Free;
  if fTicks > 0 then
    fProcList.Convert(fTicks)
  else
    WriteLn('*** LazProfiler: fTicks='+IntToStr(fTicks));
  if fLoaded then
    SaveXML(fName + cSettingExtension);
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

constructor TLPPasProc.Create(pName: String; pKind: Integer; pNameOfClass, pUnitName, pFileName, pPackageName: String; pRow: Integer);
begin
  inherited Create(pNameOfClass, pUnitName, pFileName, pPackageName);
  fName := pName;
  fNameUp := UpperCase(pName);
  fRow := pRow;
  fInstrument := pPackageName = '?';
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

procedure TLPPasProc.SetSums(pNetSum, pGrossSum: QWord);
begin
  fPerNet := 0;
  fPerNetStr := '';
  fPerGross := 0;
  fPerGrossStr := '';
  if fCount > 0 then begin
    if (pNetSum > 0) then begin
      fPerNet := fNet * 100 * 1000 div pNetSum;
      fPerNetStr := Format('%2.3n', [fPerNet / 1000]);
    end;
    if (pGrossSum > 0) then begin
      fPerGross := fGross * 100 * 1000 div pGrossSum;
      fPerGrossStr := Format('%2.3n', [fPerGross / 1000]);
    end;
  end;
end;

function TLPPasProc.XMLNodeName: String;
begin
  Result := cXMLProc;
end;

procedure TLPPasProc.SaveXMLNode(pDoc: TXMLDocument; pNode: TDOMElement);
begin
  inherited SaveXMLNode(pDoc, pNode);
  pNode.SetAttribute('name', fName);
  pNode.SetAttribute('kind', IntToStr(fKind));
  pNode.SetAttribute('row', IntToStr(fRow));
  pNode.SetAttribute('count', IntToStr(fCount));
  pNode.SetAttribute('net', IntToStr(fNet));
  pNode.SetAttribute('gross', IntToStr(fGross));
  pNode.SetAttribute('instrument', BoolToStr(fInstrument, True));
end;

procedure TLPPasProc.LoadXMLNode(pNode: TDOMNode);
var
  lCountAttr, lNetAttr, lGrossAttr, lIntrumentAttr: TDOMNode;
begin
  inherited LoadXMLNode(pNode);

  lCountAttr := pNode.Attributes.GetNamedItem('count');
  if Assigned(lCountAttr) then
    fCount := StrToInt(lCountAttr.NodeValue);

  lNetAttr := pNode.Attributes.GetNamedItem('net');
  if Assigned(lNetAttr) then
    fNet := StrToQWord(lNetAttr.NodeValue);

  lGrossAttr := pNode.Attributes.GetNamedItem('gross');
  if Assigned(lGrossAttr) then
    fGross := StrToQWord(lGrossAttr.NodeValue);

  lIntrumentAttr := pNode.Attributes.GetNamedItem('instrument');
  if Assigned(lIntrumentAttr) then
    fInstrument := StrToBool(lIntrumentAttr.NodeValue);

  if fCount = 0 then begin
    fAvgNet := 0;
    fAvgGross := 0;
    fCountStr := '';
    fSumNetStr := '';
    fSumGrossStr := '';
    fAvgNetStr := '';
    fAvgGrossStr := '';
  end else begin
    fAvgNet := fNet div fCount;
    fAvgGross := fGross div fCount;
    fCountStr := IntToStr(fCount);
    fSumNetStr := IntToTime(fNet);
    fSumGrossStr := IntToTime(fGross);
    fAvgNetStr := IntToTime(fAvgNet);
    fAvgGrossStr := IntToTime(fAvgGross);
  end;
end;

procedure TLPPasProc.InternalCreateFormXML(pNode: TDOMNode);
var
  lName, lPackageName, lFileName, lUnitName, lNameOfClass: DOMString;
  lNameAttr, lKindAttr, lUnitNameAttr, lFileNameAttr, lPackageNameAttr, lNameOfClassAttr, lRowAttr: TDOMNode;
  lKind, lRow: LongInt;
begin;
  lNameAttr := pNode.Attributes.GetNamedItem('name');
  if Assigned(lNameAttr) then
    lName := lNameAttr.NodeValue;

  lKindAttr := pNode.Attributes.GetNamedItem('kind');
  if Assigned(lKindAttr) then
    lKind := StrToInt(lKindAttr.NodeValue);

  lNameOfClassAttr := pNode.Attributes.GetNamedItem('nameofclass');
  if Assigned(lNameOfClassAttr) then
    lNameOfClass := lNameOfClassAttr.NodeValue;

  lUnitNameAttr := pNode.Attributes.GetNamedItem('unitname');
  if Assigned(lUnitNameAttr) then
    lUnitName := lUnitNameAttr.NodeValue;

  lFileNameAttr := pNode.Attributes.GetNamedItem('filename');
  if Assigned(lFileNameAttr) then
    lFileName := lFileNameAttr.NodeValue;

  lPackageNameAttr := pNode.Attributes.GetNamedItem('packagename');
  if Assigned(lPackageNameAttr) then
    lPackageName := lPackageNameAttr.NodeValue;

  lRowAttr := pNode.Attributes.GetNamedItem('row');
  if Assigned(lRowAttr) then
    lRow := StrToInt(lRowAttr.NodeValue);

  Create(lName, lKind, lNameOfClass, lUnitName, lFileName, lPackageName, lRow);
end;

end.

