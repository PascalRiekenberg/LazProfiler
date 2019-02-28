{
    Copyright (c) 2017-18 Pascal Riekenberg

    VirtualTreeView-Helper-Class

    See the file COPYING.modifiedLGPL.txt, included in this distribution,
    for details about the license.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit vtvObject;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  Generics.Collections,
  SysUtils,
  Laz.VirtualTrees;

type

  { TvtvObj }

  TvtvObj = class;
  TvtvObjList = specialize TObjectList<TvtvObj>;

  TvtvObj = class
  private
    function GetCheckState: TCheckState; inline;
    procedure SetNode(pValue: PVirtualNode);
  protected
    fLine: Integer;
    fVst: TBaseVirtualTree;
    fNode: PVirtualNode;
    fParent, fRoot: TvtvObj;
    fChilds: TvtvObjList;
    procedure SetCheckState(pCheckState: TCheckState); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(pChild: TvtvObj);
    function CellText(Column: TColumnIndex; TextType: TVSTTextType): String; virtual; abstract;
    function InitialStates: TVirtualNodeInitStates; virtual;
    function InitChildren: Cardinal; virtual;
    function ImageIndex(pColumn: TColumnIndex): Integer; virtual;
    procedure InitNode(pVST: TBaseVirtualTree; pNode: PVirtualNode); virtual;
    procedure UpdateExpanded; virtual;
    property Node: PVirtualNode read fNode write SetNode;
    property Childs: TvtvObjList read fChilds;
    property Parent: TvtvObj read fParent;
    property Root: TvtvObj read fRoot;
    property CheckState: TCheckState read GetCheckState write SetCheckState;
    property Line: Integer read fLine write fLine;
  end;
  PvtvObj = ^TvtvObj;

implementation

{ TvtvObj }

function TvtvObj.GetCheckState: TCheckState;
begin
  Result := fNode^.CheckState;
end;

procedure TvtvObj.SetCheckState(pCheckState: TCheckState);
var
  i: Integer;
begin
  fNode^.CheckState := pCheckState;
  for i := 0 to fChilds.Count - 1 do
    fChilds[i].CheckState := pCheckState;
end;

procedure TvtvObj.SetNode(pValue: PVirtualNode);
begin
  if fNode = pValue then Exit;
  fNode := pValue;
end;

constructor TvtvObj.Create;
begin
  fChilds := TvtvObjList.Create(True);
  fParent := nil;
  fRoot := nil;
  fNode := nil;
end;

destructor TvtvObj.Destroy;
begin
  FreeAndNil(fChilds);
  inherited Destroy;
end;

procedure TvtvObj.Add(pChild: TvtvObj);
begin
  pChild.fParent := Self;
  fChilds.Add(pChild);
  pChild.fRoot := Self;
  while Assigned(pChild.fRoot.fParent) do pChild.fRoot := pChild.fRoot.fParent;
end;

function TvtvObj.InitialStates: TVirtualNodeInitStates;
begin
  Result := [];
  if fChilds.Count > 0 then
    Result := Result + [ivsHasChildren];
end;

function TvtvObj.InitChildren: Cardinal;
begin
  Result := fChilds.Count;
end;

function TvtvObj.ImageIndex(pColumn: TColumnIndex): Integer;
begin
  Result := -1;
end;

procedure TvtvObj.InitNode(pVST: TBaseVirtualTree; pNode: PVirtualNode);
begin
  fVst := pVST;
  fNode := pNode;
end;

procedure TvtvObj.UpdateExpanded;
begin
  // do nothing
end;

end.

