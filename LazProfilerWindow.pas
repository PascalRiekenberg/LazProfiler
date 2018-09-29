{
    Copyright (c) 2017-18 Pascal Riekenberg

    LazProfiler: IDE Addon - Result Window

    See the file COPYING.modifiedLGPL.txt, included in this distribution,
    for details about the license.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit LazProfilerWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  ComCtrls,
  Controls,
  Dialogs,
  FileUtil,
  Forms,
  Generics.Collections,
  Generics.Defaults,
  Graphics,
  LazProfilerCore,
  ProjectIntf,
  SysUtils,
  VirtualTrees,
  vtvObject;

const
  cColCount = 6;

type

  { TLazProfilerForm }

  TLazProfilerForm = class(TForm)
    Icons: TImageList;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    VST: TVirtualStringTree;
    procedure VSTBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure VSTChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTCollapsedExpanded(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure VSTDblClick(Sender: TObject);
    procedure VSTGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure VSTHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
    procedure VSTInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: Cardinal);
    procedure VSTInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure VSTResize(Sender: TObject);
    procedure VSTStructureChange(Sender: TBaseVirtualTree; Node: PVirtualNode; Reason: TChangeReason);
  private
    fData: TLPPasProcList;
    fTreeData: TvtvObjList;
    fDataChanged: Boolean;
    fMaxColSize: array[0..cColCount - 1] of Integer;
    fSort1,
    fSort2,
    fSort3: TvtvObjList;
    procedure SetData(pData: TLPPasProcList);
    procedure PrepareData;
    procedure RebuildTree;
    procedure RebuildLines;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property Data: TLPPasProcList read fData write SetData;
    property DataChanged: Boolean read fDataChanged;
  end;


  { TLPProcObjectComparer }

  TLPProcObjectComparer = class(specialize TComparer<TLPPasProc>)
    function Compare(constref Left, Right: TLPPasProc): Integer; override; overload;
  end;


  { TLPProcUnitComparer }

  TLPProcUnitComparer = class(specialize TComparer<TLPPasProc>)
    function Compare(constref Left, Right: TLPPasProc): Integer; override; overload;
  end;

implementation

uses
  LazIDEIntf,
  LazLogger,
  LazProfilerAddon,
  SrcEditorIntf;

var
  ProcObjectComparer: TLPProcObjectComparer;
  ProcUnitComparer: TLPProcUnitComparer;

{$R *.lfm}

{ TLPProcUnitComparer }

function TLPProcUnitComparer.Compare(constref Left, Right: TLPPasProc): Integer;
begin
  Result := strcomp(PChar(Left.UnitNameUp), PChar(Right.UnitNameUp));
  if Result = 0 then Result := strcomp(PChar(Left.NameOfClassUp), PChar(Right.NameOfClassUp));
  if Result = 0 then Result := strcomp(PChar(Left.NameUp), PChar(Right.NameUp));
end;


{ TLPProcObjectComparer }

function TLPProcObjectComparer.Compare(constref Left, Right: TLPPasProc): Integer;
begin
  Result := strcomp(PChar(Left.NameOfClassUp), PChar(Right.NameOfClassUp));
  if Result = 0 then Result := strcomp(PChar(Left.NameUp), PChar(Right.NameUp));
end;


{ TLazProfilerForm }

procedure TLazProfilerForm.VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
var
  lData: PvtvObj;
begin
  lData := VST.GetNodeData(Node);
  CellText := lData^.CellText(Column, TextType);
end;

procedure TLazProfilerForm.VSTHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
var
  lFullRebuild: Boolean;
begin
  //DebugLn('*** LazProfiler: HeaderClick');
  lFullRebuild := False;
  Screen.Cursor := crHourGlass;
  try
    if VST.Header.SortColumn = HitInfo.Column then begin
      if VST.Header.SortDirection = sdAscending then
        VST.Header.SortDirection := sdDescending
      else
        VST.Header.SortDirection := sdAscending;
    end else begin
      if HitInfo.Column in [3, 4, 5] then
        VST.Header.SortDirection := sdDescending
      else
        VST.Header.SortDirection := sdAscending;
      if (
        (VST.Header.SortColumn in [0, 3, 4, 5])
        and (HitInfo.Column in [1, 2])
      ) or (
        (VST.Header.SortColumn in [1, 2])
        and (HitInfo.Column <> VST.Header.SortColumn)
      ) then begin
        VST.Header.SortColumn := HitInfo.Column;
        //DebugLn('change layout');
        lFullRebuild := True;
      end else begin
        VST.Header.SortColumn := HitInfo.Column;
      end;
    end;
    if lFullRebuild then
      RebuildTree
    else begin
      if fTreeData.Count > 0 then begin
        VST.SortTree(VST.Header.SortColumn, VST.Header.SortDirection);
        RebuildLines;
        VST.Invalidate;
      end;
    end;
    Addon.SortColumn := VST.Header.SortColumn;
    Addon.SortDirection := Integer(VST.Header.SortDirection);
    fDataChanged := True;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TLazProfilerForm.VSTInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: Cardinal);
var
  lData: PvtvObj;
begin
  lData := VST.GetNodeData(Node);
  ChildCount := lData^.InitChildren;
end;

procedure TLazProfilerForm.VSTChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  lData: TvtvObj;
begin
  lData := PvtvObj(VST.GetNodeData(Node))^;
  lData.CheckState := Node^.CheckState;
  fDataChanged := True;
end;

procedure TLazProfilerForm.VSTBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
  BG: TColor;
  lData: TvtvObj;
begin
  if (CellPaintMode = cpmGetContentMargin) then
    Exit;
  BG := TargetCanvas.Brush.Color;
  lData := PvtvObj(Sender.GetNodeData(Node))^;
  if lData.Line mod 2 = 0 then
    BG := clBtnFace;
  TargetCanvas.Brush.Color := BG;
  TargetCanvas.FillRect(CellRect);
end;

procedure TLazProfilerForm.VSTCollapsedExpanded(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  lData: TvtvObj;
begin
  lData := PvtvObj(Sender.GetNodeData(Node))^;
  lData.UpdateExpanded;
  VSTStructureChange(Sender, Node, crIgnore);
end;

procedure TLazProfilerForm.VSTCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  lP1, lP2: TLPPasProc;

  function Compare(pV1, pV2: QWord): Integer;
  begin
    if pV1 > pV2 then
      Result := 1
    else if pV1 < pV2 then
      Result := -1
    else
      Result := 0;
  end;

begin
  lP1 := PLPvtvProc(VST.GetNodeData(Node1))^.PasProc;
  lP2 := PLPvtvProc(VST.GetNodeData(Node2))^.PasProc;
  case Column of
    0: Result := strcomp(PChar(lP1.NameUp), PChar(lP2.NameUp));
    1: begin
      Result := strcomp(PChar(lP1.NameOfClassUp), PChar(lP2.NameOfClassUp));
      if Result = 0 then Result := strcomp(PChar(lP1.NameUp), PChar(lP2.NameUp))
    end;
    2: begin
      Result := strcomp(PChar(lP1.UnitNameUp), PChar(lP2.UnitNameUp));
      if Result = 0 then Result := strcomp(PChar(lP1.NameOfClassUp), PChar(lP2.NameOfClassUp));
      if Result = 0 then Result := strcomp(PChar(lP1.NameUp), PChar(lP2.NameUp))
    end;
    3: Result := lP1.Count - lP2.Count;
    4: Result := Compare(lP1.Net, lP2.Net);
    5: Result := Compare(lP1.Gross, lP2.Gross);
  end;
  //Debugln('compare '+lp1.Name+' - '+lp2.Name+' Col='+IntToStr(Column)+' Result='+IntToStr(Result));
end;

procedure TLazProfilerForm.VSTDblClick(Sender: TObject);
var
  lData: TLPPasProc;
  lNode: PVirtualNode;
  lFile: TLazProjectFile;
  lSrcIntf: TSourceEditorInterface;
begin
  lNode := VST.FocusedNode;
  if vsHasChildren in lNode^.States then
    Exit;
  lData := PLPvtvProc(VST.GetNodeData(lNode))^.PasProc;
  if not (lData is TLPPasProc) then
    Exit;
  if LazarusIDE.DoOpenEditorFile(lData.FileName, -1, -1, [ofAddToRecent]) <> mrOK then
    exit;
  lSrcIntf := SourceEditorManagerIntf.SourceEditorIntfWithFilename(lData.FileName);
  if not Assigned(lSrcIntf) then
    exit;
  lSrcIntf.CursorTextXY := Classes.Point(1, lData.Row + 1);
end;

procedure TLazProfilerForm.VSTGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
var
  lData: PvtvObj;
begin
  lData := VST.GetNodeData(Node);
  ImageIndex := lData^.ImageIndex(Column);
end;

procedure TLazProfilerForm.VSTInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  lData, lParentData: PvtvObj;
begin
  lData := VST.GetNodeData(Node);
  if not Assigned(ParentNode) then begin
    // Root nodes
    lData^ := fTreeData[Node^.Index];
  end else begin
    // Child nodes
    lParentData := VST.GetNodeData(ParentNode);
    lData^ := lParentData^.Childs[Node^.Index];
  end;
  lData^.InitNode(Sender, Node);
  InitialStates := lData^.Initialstates;
end;

procedure TLazProfilerForm.VSTResize(Sender: TObject);
var
  i, lSize, lMaxSize, lMinSize, lCalcSize: Integer;
begin
  with Sender as TVirtualStringTree do begin
    UpdateVerticalScrollBar(False);
    lSize := ClientRect.Right - ClientRect.Left;
    lMaxSize := 0;
    for i := 0 to cColCount - 1 do
      lMaxSize := lMaxSize + fMaxColSize[i];
    if lMaxSize <= lSize then begin
      // width is enough
      for i := cColCount - 1 downto 1 do begin
        Header.Columns[i].Width := fMaxColSize[i];
        lSize := lSize - fMaxColSize[i];
      end;
      Header.Columns[0].Width := lSize;
    end else begin
      // width is to small
      for i := cColCount - 1 downto 3 do begin
        Header.Columns[i].Width := fMaxColSize[i];
        lSize := lSize - fMaxColSize[i];
        lMaxSize := lMaxSize - fMaxColSize[i];
      end;
      lMinSize := 0;
      for i := 0 to 2 do begin
        lMinSize := lMinSize + Header.Columns[i].MinWidth;
      end;
      if lMinSize <= lSize then begin
        // min width is enough -> split space (relative to min width)
        for i := 2 downto 0 do begin
          lCalcSize := round(lSize * Header.Columns[i].MinWidth / lMinSize);
          if (lCalcSize > fMaxColSize[i])
          and (i <> 0) then begin
            Header.Columns[i].Width := fMaxColSize[i];
          end else begin
            Header.Columns[i].Width := lCalcSize;
          end;
          lSize := lSize - Header.Columns[i].Width;
          lMinSize := lMinSize - Header.Columns[i].MinWidth;
        end;
      end else begin
        for i := 0 to 2 do
          Header.Columns[i].Width := Header.Columns[i].MinWidth;
      end;
    end;
  end;
end;

procedure TLazProfilerForm.VSTStructureChange(Sender: TBaseVirtualTree; Node: PVirtualNode; Reason: TChangeReason);
var
  i, lNewColSize: Integer;
begin
  with Sender as TVirtualStringTree do begin
    for i := 0 to cColCount - 1 do begin
      lNewColSize := GetMaxColumnWidth(i);
      if lNewColSize < Header.Columns[i].MinWidth then
          lNewColSize := Header.Columns[i].MinWidth;
      if fMaxColSize[i] <> lNewColSize then begin
        fMaxColSize[i] := lNewColSize;
      end;
    end;
  end;
  VSTResize(Sender);
  RebuildLines;
end;

procedure TLazProfilerForm.SetData(pData: TLPPasProcList);
begin
  fData := pData;
  VST.RootNodeCount := 0;
  if not Assigned(pData) then
    Exit;
  //DebugLn('fData.Count=%d', [fData.Count]);
  PrepareData;
  RebuildTree;
  fDataChanged := False;
end;

procedure TLazProfilerForm.PrepareData;
var
  i: Integer;
  lPasProc: TLPPasProc;
  lList: TLPPasProcList;
  lPasClass: TLPvtvPasClass;
  lPasUnit: TLPvtvPasUnit;
begin
  fSort1.Clear;
  fSort2.Clear;
  fSort3.Clear;
  lList := TLPPasProcList.Create(False);
  try
    for i := 0 to fData.Count - 1 do begin
      lPasProc := fData[i];
      // default
      fSort1.Add(TLPvtvPasProc.Create(lPasProc));
      // temp list
      lList.Add(lPasProc);
    end;

    // object
    lPasClass := Nil;
    lList.Sort(ProcObjectComparer);
    for i := 0 to lList.Count - 1 do begin
      lPasProc := lList[i];
      if not Assigned(lPasClass)
      or (lPasClass.PasProc.NameOfClassUp <> lPasProc.NameOfClassUp) then begin
        if Assigned(lPasClass)
        and (lPasClass.PasProc.NameOfClass = '') then
          lPasClass.Free;
        lPasClass := TLPvtvPasClass.Create(lPasProc.NameOfClass, lPasProc.UnitName, lPasProc.FileName);
        if lPasClass.PasProc.NameOfClass <> '' then begin
          lPasClass.PasClass := lPasProc.PasClass;
          fSort2.Add(lPasClass);
        end;
      end;
      if lPasClass.PasProc.NameOfClass = '' then begin
        fSort2.Add(TLPvtvPasProc.Create(lPasProc));
        lPasClass.PasClass := lPasProc.PasClass;
      end else
        lPasClass.Add(TLPvtvPasProc.Create(lPasProc));
    end;
    if Assigned(lPasClass)
    and (lPasClass.PasProc.NameOfClass = '') then
      lPasClass.Free;

    // unit
    lPasUnit := Nil;
    lPasClass := Nil;
    lList.Sort(ProcUnitComparer);
    for i := 0 to lList.Count - 1 do begin
      lPasProc := lList[i];
      if not Assigned(lPasUnit)
      or (lPasUnit.PasProc.UnitName <> lPasProc.UnitName) then begin
        lPasUnit := TLPvtvPasUnit.Create(lPasProc.UnitName, lPasProc.FileName);
        lPasUnit.PasUnit := lPasProc.PasUnit;
        fSort3.Add(lPasUnit);
      end;
      if not Assigned(lPasClass)
      or (lPasClass.PasProc.NameOfClassUp <> lPasProc.NameOfClassUp) then begin
        if Assigned(lPasClass)
        and (lPasClass.PasProc.NameOfClass = '') then
          lPasClass.Free;
        lPasClass := TLPvtvPasClass.Create(lPasProc.NameOfClass, lPasProc.UnitName, lPasProc.FileName);
        if lPasClass.PasProc.NameOfClass <> '' then begin
          lPasClass.PasClass := lPasProc.PasClass;
          lPasUnit.Add(lPasClass);
        end;
      end;
      if lPasClass.PasProc.NameOfClass = '' then
        lPasUnit.Add(TLPvtvPasProc.Create(lPasProc))
      else
        lPasClass.Add(TLPvtvPasProc.Create(lPasProc));
    end;
    if Assigned(lPasClass)
    and (lPasClass.PasProc.NameOfClass = '') then
      lPasClass.Free;
  finally
    lList.Free;
  end;
end;

procedure TLazProfilerForm.RebuildTree;
begin
  VST.RootNodeCount := 0;
  case VST.Header.SortColumn of
    1: fTreeData := fSort2;
    2: fTreeData := fSort3;
    else
      fTreeData := fSort1;
  end;
  if fTreeData.Count > 0 then begin
    VST.RootNodeCount := fTreeData.Count;
    VST.SortTree(VST.Header.SortColumn, VST.Header.SortDirection);
    VSTStructureChange(VST, nil, crIgnore);
  end;
end;

procedure TLazProfilerForm.RebuildLines;
var
  lNode: PVirtualNode;
  lData: TvtvObj;
  lLine: Integer;
begin
  lLine := 1;
  lNode := VST.GetFirstVisible;
  while Assigned(lNode) do begin
    lData := PvtvObj(VST.GetNodeData(lNode))^;
    lData.Line := lLine;
    inc(lLine);
    lNode := VST.GetNextVisible(lNode);
  end;
end;

constructor TLazProfilerForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  fSort1 := TvtvObjList.Create(True);
  fSort2 := TvtvObjList.Create(True);
  fSort3 := TvtvObjList.Create(True);
  VST.NodeDataSize := SizeOf(PLPPasProc);
end;

destructor TLazProfilerForm.Destroy;
begin
  fSort1.Free;
  fSort2.Free;
  fSort3.Free;
  inherited Destroy;
end;

initialization

  ProcObjectComparer := TLPProcObjectComparer.Create;
  ProcUnitComparer := TLPProcUnitComparer.Create;

finalization;

  ProcObjectComparer.Free;
  ProcUnitComparer.Free;

end.

