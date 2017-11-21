unit LazProfilerWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Generics.Collections,
  FileUtil, Forms, Controls, Graphics,
  Dialogs, ComCtrls, VirtualTrees, LazProfilerCore, ProjectIntf;

const
  cColCount = 6;

type

  { TLazProfilerForm }

  TLazProfilerForm = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    VST: TVirtualStringTree;
    procedure VSTChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure VSTDblClick(Sender: TObject);
    procedure VSTGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: String);
    procedure VSTInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure VSTResize(Sender: TObject);
    procedure VSTStructureChange(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Reason: TChangeReason);
  private
    fData: TLPProcList;
    fDataChanged: Boolean;
    fMaxColSize: array[0..cColCount - 1] of Integer;
    procedure SetData(pData: TLPProcList);
  public
    constructor Create(TheOwner: TComponent); override;
    property Data: TLPProcList read fData write SetData;
    property DataChanged: Boolean read fDataChanged;

  end;

implementation

uses
  LazLogger, LazIDEIntf, SrcEditorIntf;

{$R *.lfm}

{ TLazProfilerForm }

procedure TLazProfilerForm.VSTGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  lData: PLPProc;

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
  lData := VST.GetNodeData(Node);
  CellText := ' ';
  with lData^ do case Column of
    0: CellText := Name;
    1: CellText := NameOfClass;
    2: CellText := UnitName;
    3: CellText := IntToStr(Count);
    4: CellText := IntToTime(Net);
    5: CellText := IntTotime(Gross);
  end;
end;

procedure TLazProfilerForm.VSTChecked(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  lData: PLPProc;
begin
  lData := VST.GetNodeData(Node);
  lData^.Instrument := Node^.CheckState = csCheckedNormal;
  fDataChanged := True;
end;

procedure TLazProfilerForm.VSTCompareNodes(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  lP1, lP2: PLPProc;

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
  lP1 := VST.GetNodeData(Node1);
  lP2 := VST.GetNodeData(Node2);
  case Column of
    0: Result := strcomp(PChar(lP1^.Name), PChar(lP2^.Name));
    1: begin
      Result := strcomp(PChar(lP1^.NameOfClass), PChar(lP2^.NameOfClass));
      if Result = 0 then Result := strcomp(PChar(lP1^.Name), PChar(lP2^.Name))
    end;
    2: begin
      Result := strcomp(PChar(lP1^.UnitName), PChar(lP2^.UnitName));
      if Result = 0 then Result := strcomp(PChar(lP1^.NameOfClass), PChar(lP2^.NameOfClass));
      if Result = 0 then Result := strcomp(PChar(lP1^.Name), PChar(lP2^.Name))
    end;
    3: Result := lP1^.Count - lP2^.Count;
    4: Result := Compare(lP1^.Net, lP2^.Net);
    5: Result := Compare(lP1^.Gross, lP2^.Gross);
  end;
  //Debugln('compare '+lp1.Name+' - '+lp2.Name+' Col='+IntToStr(Column)+' Result='+IntToStr(Result));
end;

procedure TLazProfilerForm.VSTDblClick(Sender: TObject);
var
  lData: PLPProc;
  lNode: PVirtualNode;
  lFile: TLazProjectFile;
  lSrcIntf: TSourceEditorInterface;
begin
  lNode := VST.FocusedNode;
  lData := VST.GetNodeData(lNode);
  if LazarusIDE.DoOpenEditorFile(lData^.FileName, -1, -1, [ofAddToRecent]) <> mrOK then
    exit;
  lSrcIntf := SourceEditorManagerIntf.SourceEditorIntfWithFilename(lData^.FileName);
  if not Assigned(lSrcIntf) then
    exit;
  lSrcIntf.CursorTextXY := Point(1, lData^.Row + 1);
end;

procedure TLazProfilerForm.VSTInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  lData: PLPProc;
begin
  Node^.CheckType := ctCheckBox;
  lData := VST.GetNodeData(Node);
  lData^ := fData[node^.Index];
  if lData^.Instrument then
    Node^.CheckState := csCheckedNormal
  else
    Node^.CheckState := csUncheckedNormal;
end;

procedure TLazProfilerForm.VSTResize(Sender: TObject);
var
  i, lSize, lMaxSize, lMinSize, lCalcSize: Integer;
begin
  with Sender as TVirtualStringTree do begin
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

procedure TLazProfilerForm.VSTStructureChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Reason: TChangeReason);
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
end;

procedure TLazProfilerForm.SetData(pData: TLPProcList);
begin
  fData := pData;
  VST.RootNodeCount := 0;
  fDataChanged := False;
  if Assigned(pData) then
    VST.RootNodeCount := fData.Count;
  VSTStructureChange(VST, nil, crIgnore);
end;

constructor TLazProfilerForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  VST.NodeDataSize := SizeOf(PLPProc);
end;

end.

