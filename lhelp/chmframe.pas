unit chmframe;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, IpHtml, LResources, Forms, Controls, ComCtrls,
  ExtCtrls, StdCtrls, Menus;

type

  { TFrameChm }

  TFrameChm = class(TFrame)
    btnSearch: TButton;
    cboxKeyword: TComboBox;
    edIndexSearch: TLabeledEdit;
    IpHtmlPanel: TIpHtmlPanel;
    lbResults: TLabel;
    lbKeyword: TLabel;
    miCopy: TMenuItem;
    panContents: TPanel;
    pgcNavigation: TPageControl;
    pmHtml: TPopupMenu;
    Splitter1: TSplitter;
    StatusBar: TStatusBar;
    tvSearchResults: TTreeView;
    tsSearch: TTabSheet;
    tvIndex: TTreeView;
    tsIndex: TTabSheet;
    tvContents: TTreeView;
    tsContents: TTabSheet;
    procedure btnSearchClick(Sender: TObject);
    procedure cboxKeywordKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure edIndexSearchChange(Sender: TObject);
    procedure miCopyClick(Sender: TObject);
    procedure tvContentsCollapsed(Sender: TObject; Node: TTreeNode);
    procedure tvContentsCreateNodeClass(Sender: TCustomTreeView;
      var NodeClass: TTreeNodeClass);
    procedure tvContentsExpanded(Sender: TObject; Node: TTreeNode);
    procedure tvContentsSelectionChanged(Sender: TObject);
    procedure tvIndexCollapsing(Sender: TObject; Node: TTreeNode;
      var AllowCollapse: Boolean);
    procedure tvIndexCreateNodeClass(Sender: TCustomTreeView;
      var NodeClass: TTreeNodeClass);
    procedure tvIndexDblClick(Sender: TObject);
    procedure tvSearchResultsDblClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

implementation

{ TFrameChm }

procedure TFrameChm.tvContentsSelectionChanged(Sender: TObject);
begin
  //
end;

procedure TFrameChm.tvIndexCollapsing(Sender: TObject; Node: TTreeNode;
  var AllowCollapse: Boolean);
begin
  //
end;

procedure TFrameChm.tvIndexCreateNodeClass(Sender: TCustomTreeView;
  var NodeClass: TTreeNodeClass);
begin
  //
end;

procedure TFrameChm.tvIndexDblClick(Sender: TObject);
begin
  //
end;

procedure TFrameChm.tvSearchResultsDblClick(Sender: TObject);
begin
  //
end;

procedure TFrameChm.tvContentsExpanded(Sender: TObject; Node: TTreeNode);
begin
  //
end;

procedure TFrameChm.tvContentsCollapsed(Sender: TObject; Node: TTreeNode);
begin
  //
end;

procedure TFrameChm.edIndexSearchChange(Sender: TObject);
var
  ItemName: String;
  SearchText: String;
  Node: TTreeNode;
begin
  if edIndexSearch <> Sender then
    Exit;
  SearchText := LowerCase(edIndexSearch.Text);
  Node := tvIndex.Items.GetFirstNode;
  while Node <> nil do
  begin
    ItemName := LowerCase(Copy(Node.Text, 1, Length(SearchText)));
    if ItemName = SearchText then
    begin
      tvIndex.Items.GetLastNode.MakeVisible;
      Node.MakeVisible;
      Node.Selected:=True;
      Exit;
    end;
    Node := Node.GetNextSibling;
  end;
  tvIndex.Selected := nil;
end;

procedure TFrameChm.miCopyClick(Sender: TObject);
begin
  IpHtmlPanel.CopyToClipboard();
end;

procedure TFrameChm.cboxKeywordKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  //
end;

procedure TFrameChm.btnSearchClick(Sender: TObject);
begin
  //
end;

procedure TFrameChm.tvContentsCreateNodeClass(Sender: TCustomTreeView;
  var NodeClass: TTreeNodeClass);
begin
  //
end;

initialization
  {$I chmframe.lrs}

end.

