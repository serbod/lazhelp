{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Copyright (C) <2005> <Andrew Haines> chmspecialparser.pas
}
unit ChmSpecialParser;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, SysUtils, Controls, ComCtrls, chmsitemap;
  
type

  TContentTreeNode = class(TTreeNode)
  private
    FUrl: String;
  public
    property Url: String read FUrl write FUrl;
  end;

  TIndexItem = class(TListITem)
  private
    FUrl: String;
  public
    property Url: String read FUrl write FUrl;
  end;
  
  
  { TContentsFiller }
  { Fill TTreeView with TChmSiteMap items }

  TContentsFiller = class(TObject)
  private
    FTreeView: TTreeView;
    FSitemap: TChmSiteMap;
    FChm: TObject;
    FBranchCount: DWord;
    FStop: PBoolean;
    FLastNode: TTreeNode;
    procedure AddItem(AItem: TChmSiteMapItem; AParentNode: TTreeNode);
  public
    constructor Create(ATreeView: TTreeView; ASitemap: TChmSiteMap; StopBoolean: PBoolean; AChm: TObject);
    procedure DoFill(ParentNode: TTreeNode);
  end;
  
implementation

uses
  LConvEncoding, LazUTF8, HTMLDefs;

function ToUTF8(const AText: AnsiString): String;
var
  encoding: String;
begin
  encoding := GuessEncoding(AText);
  if (encoding <> EncodingUTF8) then
    Result := ConvertEncoding(AText, encoding, EncodingUTF8)
  else
    Result := AText;
end;

function FixEscapedHTML(const AText: string): string;
var
  i, len: Integer;
  ampstr: string;
  ws: widestring;
  entity: widechar;
begin
  Result := '';
  entity := ' ';
  i := 1;
  len := Length(AText);
  while i <= len do
  begin
    if AText[i]='&' then
    begin
      ampStr := '';
      Inc(i);
      if i > len then Break;
      while AText[i] <> ';' do
      begin
        ampStr := ampStr + AText[i];
        Inc(i);
        if i > len then Break;
      end;
      ws := UTF8Encode(ampStr);
      if ResolveHTMLEntityReference(ws, entity) then
        Result := Result + UnicodeToUTF8(cardinal(entity))
      else
        Result := Result + '?';
    end
    else
      Result := Result + AText[i];
    Inc(i);
  end;
end;


// Replace %20 with space, \ with /
function FixURL(URL: String): String;
var
  X: LongInt;
begin
  X := Pos('%20', Url);
  while X > 0 do
  begin
    Delete(Url, X, 3);
    Insert(' ', Url, X);
    X := Pos('%20', Url);
  end;
  Result := StringReplace(Url, '\', '/', [rfReplaceAll]);
end;

{ TContentsFiller }

procedure TContentsFiller.AddItem(AItem: TChmSiteMapItem; AParentNode: TTreeNode);
var
  NewNode: TContentTreeNode;
  X: Integer;
  txt: string;
begin
  if FStop^ then Exit;
  txt := AItem.KeyWord;
  // Fallback:
  if txt = '' then txt := AItem.Text;
  txt := FixEscapedHTML(ToUTF8(Trim(txt)));
  if not Assigned(FLastNode) or (FLastNode.Text <> txt) then
  begin
    // Add new child node
    FLastNode := AParentNode;
    NewNode := TContentTreeNode(FTreeView.Items.AddChild(AParentNode, txt));
    NewNode.Url := FixURL('/'+AItem.Local);
    NewNode.Data := FChm;
    if FTreeView.Images <> nil then
    begin
      NewNode.ImageIndex := 3;
      NewNode.SelectedIndex := 3;

      if (AParentNode.ImageIndex < 0) or (AParentNode.ImageIndex > 2) then
      begin
        AParentNode.ImageIndex := 1;
        AParentNode.SelectedIndex := 1;
      end;
    end;
  end
  else
    NewNode := TContentTreeNode(FLastNode);

  Inc(FBranchCount);

  if FBranchCount mod 200 = 0 then
    Application.ProcessMessages;

  for X := 0 to AItem.Children.Count-1 do
    AddItem(AItem.Children.Item[X], NewNode);
end;

constructor TContentsFiller.Create(ATreeView: TTreeView; ASitemap: TChmSiteMap; StopBoolean: PBoolean; AChm: TObject);
begin
  inherited Create;
  FTreeView := ATreeView;
  FSitemap := ASitemap;
  FStop := StopBoolean;
  FChm := AChm;
end;

procedure TContentsFiller.DoFill(ParentNode: TTreeNode);
var
  X: Integer;
begin
  FTreeView.BeginUpdate();

  for X := 0 to FSitemap.Items.Count-1 do
    AddItem(FSitemap.Items.Item[X], ParentNode);

  FTreeView.EndUpdate();
end;


end.

