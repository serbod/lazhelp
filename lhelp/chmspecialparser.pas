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
    LocaleID: DWord;
    constructor Create(ATreeView: TTreeView; ASitemap: TChmSiteMap; StopBoolean: PBoolean; AChm: TObject);
    procedure DoFill(ParentNode: TTreeNode);
  end;
  
implementation

uses
  LConvEncoding, LazUTF8, HTMLDefs, lcid_conv;

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
  i, iPosAmpersand, iLenAText: Integer;
  bFoundClosureSemiColon: Boolean;
  ampStr: string;
  ws: widestring;
  entity: widechar;
begin
  Result := '';
  i := 1;
  iLenAText:= Length(AText);
  while i <= iLenAText do
  begin
    if AText[i] = '&' then
    begin
      iPosAmpersand := i;
      ampStr := '';
      Inc(i);
      while (i <= iLenAText) and (AText[i] <> ';') do
      begin
        ampStr := ampStr + AText[i];
        Inc(i);
      end;
      //is there a Char ';', closing a possible HTML entity like '&{#x}~~~{~~};'?
      bFoundClosureSemiColon := False;
      if (i > iLenAText) then
      begin
        if (AText[i-1] = ';') then
          bFoundClosureSemiColon := True;
      end
      else
      begin
        if (AText[i] = ';') then
          bFoundClosureSemiColon := True;
      end;

      if bFoundClosureSemiColon then
      begin
        //only if it's a possible HTML encoded character like "&xxx;" ...
        ws := UTF8Encode(ampStr);
        if ResolveHTMLEntityReference(ws, entity) then
          Result := Result + UnicodeToUTF8(cardinal(entity))
        else
          Result := Result + '?';
      end
      else
      begin
        //it's not an HTML entity; only an ampersand by itself
	Result := Result + RightStr(AText, iLenAText - (iPosAmpersand-1));
      end;
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
  //txt := FixEscapedHTML(ToUTF8(Trim(txt)));
  txt := ConvToUTF8FromLCID(LocaleID, Trim(txt));
  txt := FixEscapedHTML(txt);
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

