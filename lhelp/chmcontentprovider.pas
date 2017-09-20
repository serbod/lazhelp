unit chmcontentprovider;

{
  Graphical CHM help content provider.
  Responsible for loading TOC, providing search etc.
}

{$mode objfpc}{$H+}

{$Note Compiling lhelp with search support}
{$DEFINE CHM_SEARCH}

{$IF FPC_FULLVERSION>=20400}
{$Note Compiling lhelp *with* binary index and toc support}
// CHMs can have both binary and text Table of Contents and index
{$DEFINE CHM_BINARY_INDEX_TOC}
{$endif}


{off $DEFINE CHM_DEBUG_TIME}


interface

uses
  Classes, SysUtils, ChmReader,
  {$ifdef DEBUG}LazLogger, {$else}LazLoggerDummy, {$endif}
  // LCL
  LCLIntf, Forms, StdCtrls, ExtCtrls, ComCtrls, Controls, Menus,
  // LazUtils
  LazFileUtils, LazUTF8, Laz2_XMLCfg,
  // ChmHelp
  IpHtml, BaseContentProvider, FileContentProvider, ChmDataProvider, lhelpstrconsts,
  chmframe;

type

  { TChmContentProvider }

  TChmContentProvider = class(TFileContentProvider)
  private
    FStatusText: string;
    FUpdateURI: String;
    FChmFrame: TFrameChm;
    FContext: THelpContext;
    FHtml: TIpHtmlPanel;
    function GetNavigationVisible: Boolean;
    function GetShowStatusbar: Boolean;
    procedure SetNavigationVisible(AValue: Boolean);
    procedure SetShowStatusbar(AValue: Boolean);
    procedure SetStatusText(AValue: string);
  protected
    FIsUsingHistory: Boolean;
    FChmFileList: TChmFileList;
    FHistory: TStringList;
    FHistoryIndex: Integer;
    FStopTimer: Boolean;
    FFillingToc: Boolean;
    FFillingIndex: Boolean;
    FActiveChmTitle: String;
    FLoadingSearchURL: Boolean; // use this to try to highlight search terms

    function MakeURI(const AUrl: String; AChm: TChmReader): String;

    procedure AddHistory(const AURL: String);
    procedure DoOpenChm(AFile: String; ACloseCurrent: Boolean = True);
    procedure DoCloseChm();
    procedure DoLoadContext(Context: THelpContext);
    procedure DoLoadUri(const AUri: String; AChm: TChmReader = nil);
    procedure DoError({%H-}Error: Integer);
    procedure NewChmOpened(ChmFileList: TChmFileList; Index: Integer);
    procedure LoadingHTMLStream(var AStream: TStream);

    // Queue TOC fill action for later processing
    procedure QueueFillToc(AChm: TChmReader);
    // Fills table of contents (and index for main file)
    procedure FillTOC(Data: PtrInt);
    procedure IpHtmlPanelDocumentOpen(Sender: TObject);
    procedure IpHtmlPanelHotChange(Sender: TObject);
    procedure IpHtmlPanelHotClick(Sender: TObject);
    procedure ContentsTreeSelectionChanged(Sender: TObject);
    procedure IndexViewDblClick(Sender: TObject);
    procedure TreeViewStopCollapse(Sender: TObject; {%H-}Node: TTreeNode; var AllowCollapse: Boolean);
    procedure ViewMenuContentsClick(Sender: TObject);
    procedure UpdateTitle;
    procedure SetTitle(const AValue: String); override;
    procedure TOCExpand(Sender: TObject; Node: TTreeNode);
    procedure TOCCollapse(Sender: TObject; Node: TTreeNode);
    function SelectTreeItemFromURL(const AUrl: String): Boolean;
    {$IFDEF CHM_SEARCH}
    procedure SearchButtonClick(Sender: TObject);
    procedure SearchResultsDblClick(Sender: TObject);
    procedure SearchComboKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure GetTreeNodeClass(Sender: TCustomTreeView; var NodeClass: TTreeNodeClass);
    {$ENDIF}
  public
    procedure LoadPreferences(ACfg: TXMLConfig); override;
    procedure SavePreferences(ACfg: TXMLConfig); override;
    procedure BeginUpdate; override;
    procedure EndUpdate; override;
  public
    function CanGoBack: Boolean; override;
    function CanGoForward: Boolean; override;
    function GetHistory: TStrings; override;
    { URL = 'file://filename.chm://url_string' }
    function LoadURL(const AURL: String; const AContext: THelpContext=-1): Boolean; override;
    procedure GoHome; override;
    procedure GoBack; override;
    procedure GoForward; override;
    //property TabsControl: TPageControl read FTabsControl;
    //property Splitter: TSplitter read FSplitter;
    property ShowStatusbar: Boolean read GetShowStatusbar write SetShowStatusbar;
    property StatusText: string read FStatusText write SetStatusText;
    property NavigationVisible: Boolean read GetNavigationVisible write SetNavigationVisible;
    class function GetProperContentProviderClass(const {%H-}AURL: String): TBaseContentProviderClass; override;

    constructor Create(AParent: TWinControl; AImageList: TImageList); override;
    destructor Destroy; override;
  end;

implementation

uses ChmSpecialParser{$IFDEF CHM_SEARCH}, chmFIftiMain{$ENDIF}, chmsitemap, LCLType, SAX_HTML, Dom, DOM_HTML, HTMWrite;

type

  { THTMLWordHighlighter }

  THTMLWordHighlighter = class
  private
    Doc: THTMLDocument;
    Words: TStrings;
    Color: String;
    procedure ScanSubNodes(ADomNode: TDOMNode);
    procedure CheckTextNode(var ATextNode: TDomNode);
  public
    constructor Create(AHTMLDoc: THTMLDocument);
    procedure HighlightWords(AWords: TStrings; AColor: String);
  end;

{ THTMLWordHighlighter }

procedure THTMLWordHighlighter.ScanSubNodes(ADomNode: TDOMNode);

var
  CurNode: TDomNode;
begin
  CurNode := ADomNode;
  while CurNode <> nil do
  begin
    if CurNode.HasChildNodes then
      ScanSubNodes(CurNode.FirstChild);

    if CurNode.NodeType = TEXT_NODE then
      CheckTextNode(CurNode);

    CurNode := CurNode.NextSibling;
  end;
end;

procedure THTMLWordHighlighter.CheckTextNode(var ATextNode: TDomNode);
var
  i: Integer;
  fPos: Integer;
  WordStart,
  After: TDOMText;
  Span: TDomElement;
  aWord: String;
  Parent: TDomNode;
begin
   Parent := AtextNode.ParentNode;
   for i := 0 to Words.Count-1 do
   begin
     aWord := Words[i];
     fPos := Pos(aWord, LowerCase(ATextNode.TextContent));
     while fpos > 0 do
     begin
       WordStart:= TDOMText(ATextNode).SplitText(fPos-1);
       After := WordStart.SplitText(Length(aword));
       Span := doc.CreateElement('span');
       Span.SetAttribute('style', 'color:'+Color+';background-color:lightgray');
       Parent.InsertBefore(Span, After);
       Span.AppendChild(WordStart);

       // or we'll keep finding our new node again and again
       ATextNode := After;

       fPos := Pos(aWord, ATextNode.TextContent);
     end;
   end;
end;

constructor THTMLWordHighlighter.Create(AHTMLDoc: THTMLDocument);
begin
  Doc := AHTMLDoc;
end;

procedure THTMLWordHighlighter.HighlightWords(AWords: TStrings; AColor: String);
var
  Elem: TDOMNode;
begin
  Words := AWords;
  Color := AColor;
  Elem := Doc.DocumentElement.FirstChild;

  ScanSubNodes(Elem);

end;

function GetURIFileName(const AURI: String): String;
var
  FileStart,
  FileEnd: Integer;
begin
  FileStart := Pos(':', AURI)+1;
  FileEnd := Pos('::', AURI);

  Result := Copy(AURI, FileStart, FileEnd-FileStart);
end;

function GetURIURL(const AURI: String): String;
var
  URLStart: Integer;
begin
  URLStart := Pos('::', AURI) + 2;
  Result := Copy(AURI, URLStart, Length(AURI));
end;

function ChmURI(const AUrl: String; const AFileName: String): String;
var
  FileNameNoPath: String;
begin
  Result := AUrl;
  if Pos('ms-its:', Result) > 0 then
    Exit;
  FileNameNoPath := ExtractFileName(AFileName);

  Result := 'ms-its:'+FileNameNoPath+'::'+AUrl;
end;

{ TChmContentProvider }

function TChmContentProvider.GetShowStatusbar: Boolean;
begin
  Result := FChmFrame.StatusBar.Visible;
end;

function TChmContentProvider.GetNavigationVisible: Boolean;
begin
  Result := FChmFrame.pgcNavigation.Visible;
end;

procedure TChmContentProvider.SetNavigationVisible(AValue: Boolean);
begin
  FChmFrame.pgcNavigation.Visible := AValue;
  FChmFrame.Splitter1.Visible := AValue;
  if not AValue then
    FChmFrame.Splitter1.Left := FChmFrame.pgcNavigation.Left + 4; //for splitter to move righter
end;

procedure TChmContentProvider.SetShowStatusbar(AValue: Boolean);
begin
  FChmFrame.StatusBar.Visible := AValue;
end;

procedure TChmContentProvider.SetStatusText(AValue: string);
begin
  if FStatusText = AValue then Exit;
  FStatusText := AValue;
  FChmFrame.StatusBar.SimpleText := AValue;
end;

function TChmContentProvider.MakeURI(const AUrl: String; AChm: TChmReader): String;
var
  ChmIndex: Integer;
begin
  ChmIndex := FChmFileList.IndexOfObject(AChm);

  Result := ChmURI(AUrl, FChmFileList.FileName[ChmIndex]);
end;

procedure TChmContentProvider.BeginUpdate;
begin
  inherited BeginUpdate;
  FChmFrame.tvContents.BeginUpdate();
  FChmFrame.tvIndex.BeginUpdate();
end;

procedure TChmContentProvider.EndUpdate;
begin
  inherited EndUpdate;
  FChmFrame.tvContents.EndUpdate();
  FChmFrame.tvIndex.EndUpdate();
  if not IsUpdating then
  begin
    if FUpdateURI <> '' then
      DoLoadUri(FUpdateURI);
    FUpdateURI := '';
  end;
end;

procedure TChmContentProvider.AddHistory(const AURL: String);
begin
  if FHistoryIndex < FHistory.Count then
  begin
    while FHistory.Count-1 > FHistoryIndex do
      FHistory.Delete(FHistory.Count-1);
  end;

  FHistory.Add(AURL);
  Inc(FHistoryIndex);
end;

procedure TChmContentProvider.DoOpenChm(AFile: String; ACloseCurrent: Boolean = True);
begin
  if (FChmFileList <> nil) and FChmFileList.IsAnOpenFile(AFile) then Exit;
  if ACloseCurrent then DoCloseChm;
  if not FileExistsUTF8(AFile) or DirectoryExistsUTF8(AFile) then
  begin
    DebugLn('File not exists: ' + AFile);
    Exit;
  end;
  if FChmFileList = nil then
  begin
    try
      FChmFileList := TChmFileList.Create(Utf8ToSys(AFile));
      if (FChmFileList.Count = 0) or (not FChmFileList.ChmReaders[0].IsValidFile) then
      begin
        DebugLn('File not valid: ' + AFile);
        FreeAndNil(FChmFileList);
        //DoError(INVALID_FILE_TYPE);
        Exit;
      end;
      TIpChmDataProvider(FHtml.DataProvider).ChmFileList := FChmFileList;
    except
      FreeAndNil(FChmFileList);
      //DoError(INVALID_FILE_TYPE);
      Exit;
    end;
  end
  else
  begin
    FChmFileList.OpenChmFile(AFile);
    //WriteLn('Loading new chm: ', AFile);
  end;

  if FChmFileList = nil then
  begin
    DebugLn('File not readed: ' + AFile);
    Exit;
  end;

  FHistoryIndex := -1;
  FHistory.Clear;

  // Code here has been moved to the OpenFile handler

  UpdateTitle;
end;

procedure TChmContentProvider.DoCloseChm();
var
  i : integer;
begin
  FStopTimer := True;
  if Assigned(FChmFileList) then
  begin
    for i := FChmFileList.Count -1 downto 0 do
      FChmFileList.ChmReaders[i].Free();
  end;
  FreeAndNil(FChmFileList);
  UpdateTitle();
end;

procedure TChmContentProvider.DoLoadContext(Context: THelpContext);
var
 Str: String;
begin
  if FChmFileList = nil then exit;
  Str := FChmFileList.ChmReaders[0].GetContextUrl(Context);
  if Str <> '' then DoLoadUri(Str, FChmFileList.ChmReaders[0]);
end;

procedure TChmContentProvider.DoLoadUri(const AUri: String; AChm: TChmReader = nil);
var
  ChmIndex: Integer;
  NewUrl: String;
  FilteredURL: String;
  iPos: Integer;
  StartTime: TDateTime;
  EndTime: TDateTime;
  Time: String;
begin
  if (FChmFileList = nil) and (AChm = nil) then exit;
  StatusText := Format(slhelp_Loading, [AUri]);
  StartTime := Now;

  iPos := Pos('#', AUri);
  if iPos > 0 then
    FilteredURL := Copy(AUri, 1, iPos -1)
  else
    FilteredURL := AUri;

  if FChmFileList.ObjectExists(FilteredURL, AChm) = 0 then
  begin
    StatusText := Format(slhelp_NotFound, [AURI]);
    Exit;
  end;

  if (Pos('ms-its', AUri) = 0) and (AChm <> nil) then
  begin
    ChmIndex := FChmFileList.IndexOfObject(AChm);
    NewUrl := ExtractFileName(FChmFileList.FileName[ChmIndex]);
    NewUrl := 'ms-its:' + NewUrl + '::/' + AUri;
  end
  else
    NewUrl := AUri;


  if not IsUpdating() then
  begin

    FIsUsingHistory := True;
    FHtml.OpenURL(NewUrl);
    TIpChmDataProvider(FHtml.DataProvider).CurrentPath := ExtractFileDir(NewUrl)+'/';

    AddHistory(NewUrl);
    EndTime := Now;

    Time := INtToStr(DateTimeToTimeStamp(EndTime).Time - DateTimeToTimeStamp(StartTime).Time);
    StatusText := Format(slhelp_LoadedInMs, [NewUrl, Time]);

  end
  else
  begin
    // We are updating. Save this to load at end of update. or if there is already a request overwrite it so only the last is loaded
    FUpdateURI := NewUrl;
  end;
end;


procedure TChmContentProvider.DoError(Error: Integer);
begin
  //what to do with these errors?
  //INVALID_FILE_TYPE;
end;

procedure TChmContentProvider.NewChmOpened(ChmFileList: TChmFileList;
  Index: Integer);
begin
  if Index = 0 then
  begin
    if FContext > -1 then
    begin
      DoLoadContext(FContext);
      FContext := -1;
    end
    else if ChmFileList.ChmReaders[Index].DefaultPage <> '' then
    begin
      DoLoadUri(MakeURI(ChmFileList.ChmReaders[Index].DefaultPage, ChmFileList.ChmReaders[Index]));
    end;
  end;
  if ChmFileList.ChmReaders[Index].Title = '' then
    ChmFileList.ChmReaders[Index].Title := ExtractFileName(ChmFileList.FileName[Index]);

  // Fill the table of contents.
  if Index <> 0 then
    QueueFillToc(ChmFileList.ChmReaders[Index]);
end;

procedure TChmContentProvider.LoadingHTMLStream(var AStream: TStream);
var
  Doc: THTMLDocument;
  NewStream: TMemoryStream;
  Highlighter: THTMLWordHighlighter;
  Words: TStringList;
  UseOrigStream: Boolean;
begin
  if not FLoadingSearchURL then
    Exit;
  // load html and add tags to highlight words then save back to stream
  NewStream := TMemoryStream.Create();

  Words := TStringList.Create();
  try
    Words.Delimiter := ' ';
    Words.DelimitedText := FChmFrame.cboxKeyword.Text;

    Doc := nil;
    try
      UseOrigStream := True;
      ReadHTMLFile(Doc, AStream);
      Highlighter := THTMLWordHighlighter.Create(Doc);
      Highlighter.HighlightWords(Words, 'red');
      WriteHTMLFile(Doc, NewStream);
      UseOrigStream := False;
    finally
      try
        Doc.Free;
        Highlighter.Free;
      except
        UseOrigStream := True;
      end;
    end;
  finally
    Words.Free();
  end;

  if not UseOrigStream then
  begin
    AStream.Free;
    AStream := NewStream;
    NewStream.Position:=0;
  end
  else
    NewStream.Free;

  AStream.Position := 0;
end;

procedure TChmContentProvider.QueueFillToc(AChm: TChmReader);
begin
  FChmFrame.tvContents.Visible := False;
  FChmFrame.panContents.Caption := slhelp_TableOfContentsLoadingPleaseWait;
  StatusText := slhelp_TableOfContentsLoading;
  Application.QueueAsyncCall(@FillToc, PtrInt(AChm));
end;

procedure TChmContentProvider.FillTOC(Data: PtrInt);
var
  CHMReader: TChmReader;
  ParentNode: TTreeNode;
  i: Integer;
  SM: TChmSiteMap;
  HasSearchIndex: Boolean = False;
  ContentsFiller: TContentsFiller;
  {$IFNDEF CHM_BINARY_INDEX_TOC}
  Stream: TMemoryStream;
  {$ENDIF}
begin
  if FFillingToc or FFillingIndex then
  begin
    Application.QueueAsyncCall(@FillToc, Data);
    exit;
  end;
  FFillingToc := True;
  FChmFrame.tvContents.BeginUpdate();

  CHMReader := TChmReader(Data);
  {$IFDEF CHM_DEBUG_TIME}
  writeln('Start: ',FormatDateTime('hh:nn:ss.zzz', Now));
  {$ENDIF}
  if CHMReader <> nil then
  begin
    ParentNode := FChmFrame.tvContents.Items.AddChildObject(nil, CHMReader.Title, CHMReader);
    ParentNode.ImageIndex := 0;
    ParentNode.SelectedIndex := 0;
    // GetTOCSitemap first tries binary TOC but falls back to text if needed
    SM := TChmSiteMap.Create(stTOC);
    try
      {$IFDEF CHM_BINARY_INDEX_TOC}
      CHMReader.ReadTOCSitemap(SM);
      {$ELSE}
      fFillingIndex := True;
      Stream := TMemoryStream(CHMReader.GetObject(CHMReader.TOCFile));
      if Stream <> nil then
      begin
        SM := TChmSiteMap.Create(stTOC);
        SM.LoadFromStream(Stream);
        Stream.Free;
      end;
      {$ENDIF}

      {$IFDEF CHM_DEBUG_TIME}
      writeln('Stream read: ',FormatDateTime('hh:nn:ss.zzz', Now));
      {$ENDIF}
      ContentsFiller := TContentsFiller.Create(FChmFrame.tvContents, SM, @FStopTimer, CHMReader);
      try
        ContentsFiller.DoFill(ParentNode);
      finally
        ContentsFiller.Free();
      end;
      if (FChmFrame.tvContents.Selected = nil) and (FHistory.Count > 0) then
        SelectTreeItemFromURL(FHistory.Strings[FHistoryIndex]);

    finally
      SM.Free();
    end;

    if ParentNode.Index = 0 then ParentNode.Expanded := True;
    FFillingToc := False;
    FChmFrame.tvContents.EndUpdate();
    FChmFrame.tvContents.Visible := True;
    FChmFrame.panContents.Caption := '';
    FChmFrame.tsContents.TabVisible := FChmFrame.tvContents.Items.Count > 1;
    //Application.ProcessMessages();
    FFillingIndex := True;

    // we fill the index here too but only for the main file
    if FChmFileList.IndexOfObject(CHMReader) < 1 then
    begin
      SM := TChmSiteMap.Create(stTOC);
      try
        {$IFDEF CHM_BINARY_INDEX_TOC}
        CHMReader.ReadIndexSitemap(SM);
        {$ELSE}
        Stream := TMemoryStream(CHMReader.GetObject(CHMReader.IndexFile));
        if Stream <> nil then
        begin
          SM.LoadFromStream(Stream);
          Stream.Free;
        end;
        {$ENDIF}
        StatusText := slhelp_IndexLoading;
        ContentsFiller := TContentsFiller.Create(FChmFrame.tvIndex, SM, @FStopTimer, CHMReader);
        try
          ContentsFiller.DoFill(nil);
        finally
          ContentsFiller.Free();
        end;
        FChmFrame.tvIndex.FullExpand();

      finally
        SM.Free();
      end;
    end;
  end;
  FFillingIndex := False;
  FChmFrame.tsIndex.TabVisible := FChmFrame.tvIndex.Items.Count > 0;

  StatusText:= '';

  {$IFDEF CHM_DEBUG_TIME}
  writeln('End: ',FormatDateTime('hh:nn:ss.zzz', Now));
  {$ENDIF}

  {$IFDEF CHM_SEARCH}
  i := 0;
  while (HasSearchIndex = False) and (i < FChmFileList.Count) do
  begin
    // Look for binary full text search index in CHM file
    HasSearchIndex := FChmFileList.ChmReaders[i].ObjectExists('/$FIftiMain') > 0;
    Inc(i);
  end;

  FChmFrame.tsSearch.TabVisible := HasSearchIndex;
  {$ENDIF}
end;

procedure TChmContentProvider.IpHtmlPanelDocumentOpen(Sender: TObject);
begin
   // StatusBar1.Panels.Items[1] := FHtml.DataProvider.;
 if FIsUsingHistory = False then
   AddHistory(TIpChmDataProvider(FHtml.DataProvider).CurrentPage)
 else FIsUsingHistory := False;
 SelectTreeItemFromURL(TIpChmDataProvider(FHtml.DataProvider).CurrentPage);
end;

procedure TChmContentProvider.IpHtmlPanelHotChange(Sender: TObject);
begin
  StatusText := FHtml.HotURL;
end;

procedure TChmContentProvider.IpHtmlPanelHotClick(Sender: TObject);
var
  HelpFile: String;
  aPos: integer;
  lcURL: String;
begin
  // chm-links look like: mk:@MSITStore:D:\LazPortable\docs\chm\iPro.chm::/html/lh3zs3.htm
  lcURL := Lowercase(FHtml.HotURL);
  if (Pos('javascript:helppopup(''', lcURL) = 1) or
     (Pos('javascript:popuplink(''', lcURL) = 1)
  then begin
    HelpFile := Copy(FHtml.HotURL, 23, Length(FHtml.HotURL) - (23-1));
    HelpFile := Copy(HelpFile, 1, Pos('''', HelpFile)-1);

    if (Pos('/',HelpFile)=0) and (Pos('.chm:',HelpFile)=0) then begin //looks like?: 'xyz.htm'
      aPos := LastDelimiter('/', FHtml.CurURL);
      if aPos>0 then HelpFile := Copy(FHtml.CurURL,1,aPos) + HelpFile;
   end
   else if (Pos('.chm:',HelpFile)=0) then begin //looks like?: 'folder/xyz.htm' or '/folder/xyz.htm'
     if HelpFile[1]<>'/' then HelpFile:='/'+HelpFile;
     aPos := LastDelimiter(':', FHtml.CurURL);
     if aPos>0 then HelpFile := Copy(FHtml.CurURL,1,aPos) + HelpFile;
   end;
   DoLoadUri(HelpFile); //open it in current iphtmlpanel.
 end
 else
   OpenURL(FHtml.HotURL);
end;

procedure TChmContentProvider.ContentsTreeSelectionChanged(Sender: TObject);
var
  ContentTreeNode: TContentTreeNode;
  ARootNode: TTreeNode;
  ChmReader: TChmReader = nil;
  Uri: String;
  n: Integer;
begin
  if (FChmFrame.tvContents.Selected = nil) then Exit;
  if FChmFrame.tvContents.Selected.Parent = nil then
  begin
    ChmReader := TChmReader(FChmFrame.tvContents.Selected.Data);
    FActiveChmTitle:= ChmReader.Title;
    UpdateTitle;
    if ChmReader.DefaultPage <> '' then
    begin
      Uri := MakeURI(ChmReader.DefaultPage, ChmReader);
      if ((FHtml.MasterFrame <> nil) and (MakeURI(FHtml.CurURL, ChmReader)  = Uri)) = False then
        DoLoadUri(Uri);
    end;
    Exit;
  end;

  ContentTreeNode := TContentTreeNode(FChmFrame.tvContents.Selected);

  //find the chm associated with this branch
  ARootNode := ContentTreeNode.Parent;
  while ARootNode.Parent <> nil do
    ARootNode := ARootNode.Parent;

  ChmReader := TChmReader(ARootNode.Data);
  try
    FChmFrame.tvContents.OnSelectionChanged := nil;

    if ContentTreeNode.Url <> '' then
    begin
      // !!!!
      n := FChmFileList.IndexOfObject(ChmReader);
      Uri := 'file://' + FChmFileList.FileName[n] + '://' + ContentTreeNode.Url;
      LoadURL(Uri);
      { !!!
      Uri := MakeURI(ContentTreeNode.Url, ChmReader);
      if ((FHtml.MasterFrame <> nil) and (MakeURI(FHtml.CurURL, ChmReader) = Uri)) = False then
        DoLoadUri(Uri); }
    end;
  finally
    FChmFrame.tvContents.OnSelectionChanged := @ContentsTreeSelectionChanged;
  end;
end;

procedure TChmContentProvider.IndexViewDblClick(Sender: TObject);
var
  ContentTreeNode: TContentTreeNode;
begin
  if FChmFrame.tvIndex.Selected = nil then Exit;
  ContentTreeNode := TContentTreeNode(FChmFrame.tvIndex.Selected);

  // Find the chm associated with this branch
  DoLoadUri(MakeURI(ContentTreeNode.Url, TChmReader(ContentTreeNode.Data)));
end;

procedure TChmContentProvider.TreeViewStopCollapse(Sender: TObject;
  Node: TTreeNode; var AllowCollapse: Boolean);
begin
  AllowCollapse:=False;
end;

procedure TChmContentProvider.ViewMenuContentsClick(Sender: TObject);
begin
  //TMenuItem(Sender).Checked := not TMenuItem(Sender).Checked;
  //FSplitter.Visible := TMenuItem(Sender).Checked;
  //TabPanel.Visible := Splitter1.Visible;
end;

procedure TChmContentProvider.UpdateTitle();
var
  Item: TTreeNode;
  NewTitle: String;
begin
  Item := FChmFrame.tvContents.Items.GetFirstNode();
  NewTitle := FActiveChmTitle +' [';
  while Item <> nil do
  begin
    if ITem.Text <> FActiveChmTitle then
    begin
      NewTitle := NewTitle + Item.Text;
      if (Item.GetNextSibling <> nil)
      and ((Item.GetNextSibling.GetNextSibling <> nil) or (Item.GetNextSibling.Text <> FActiveChmTitle))
      then
        NewTitle := NewTitle + ', ';
    end;
    Item := Item.GetNextSibling;
  end;
  NewTitle := NewTitle + ']';
  Title := NewTitle;
end;

procedure TChmContentProvider.SetTitle(const AValue: String);
begin
  if Assigned(FChmFrame.Parent) and (FChmFrame.Parent is TTabSheet) then
    TTabSheet(FChmFrame.Parent).Caption := AValue;
  inherited SetTitle(AValue);
end;

procedure TChmContentProvider.TOCExpand(Sender: TObject; Node: TTreeNode);
begin
  if Node.Parent <> nil then
  begin
    Node.ImageIndex := 2;
    Node.SelectedIndex := 2;
  end;
end;

procedure TChmContentProvider.TOCCollapse(Sender: TObject; Node: TTreeNode) ;
begin
  if Node.Parent <> nil then
  begin
    Node.ImageIndex := 1;
    Node.SelectedIndex := 1;
  end;
end;

function TChmContentProvider.SelectTreeItemFromURL(const AUrl: String): Boolean;
var
  FileName: String;
  URL: String;
  RootNode,
  FoundNode,
  Node: TTreeNode;
  TmpHolder: TNotifyEvent;
  i: integer;
begin
  Result := False;
  if FChmFrame.tvContents.OnSelectionChanged = nil then
    Exit; // the change was a response to a click and should be ignored
  FileName := GetURIFileName(AUrl);
  URL      := GetURIURL(AUrl);
  FoundNode := nil;
  Node := nil;
  for i := 0 to FChmFileList.Count-1 do
  begin
    if FileName = ExtractFileName(FChmFileList.FileName[i]) then
    begin
      FActiveChmTitle := FChmFileList.ChmReaders[i].Title;
      UpdateTitle();

      RootNode := FChmFrame.tvContents.Items.FindNodeWithData(FChmFileList.ChmReaders[i]);
      if URL = FChmFileList.ChmReaders[i].DefaultPage then
      begin
        FoundNode := RootNode;
        Break;
      end;

      if RootNode <> nil then
        Node := RootNode.GetFirstChild;

      Break;
    end;

  end;

  if RootNode = nil then
    Exit;

  TmpHolder := FChmFrame.tvContents.OnSelectionChanged;
  FChmFrame.tvContents.OnSelectionChanged := nil;

  while (Node<>nil) and (TContentTreeNode(Node).Url<>Url) do
    Node:=Node.GetNext;

  if (Node <> nil) and (TContentTreeNode(Node).Url = Url) then
    FoundNode := Node;

  if FoundNode <> nil then
  begin
    FChmFrame.tvContents.Selected := FoundNode;
    if not FoundNode.IsVisible then
      FoundNode.MakeVisible;
  end
  else
    FChmFrame.tvContents.Selected := nil;

  FChmFrame.tvContents.OnSelectionChanged := TmpHolder;
  Result := True;
end;

{$IFDEF CHM_SEARCH}

procedure TChmContentProvider.SearchComboKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case key of
    VK_RETURN: SearchButtonClick(nil);

  end;
end;

procedure TChmContentProvider.GetTreeNodeClass(Sender: TCustomTreeView;
  var NodeClass: TTreeNodeClass);
begin
  NodeClass := TContentTreeNode;
end;

procedure TChmContentProvider.LoadPreferences(ACfg: TXMLConfig);
begin
  inherited LoadPreferences(ACfg);
  FChmFrame.pgcNavigation.Width := ACfg.GetValue(ClassName+'/TabControlWidth/Value', FChmFrame.pgcNavigation.Width);
end;

procedure TChmContentProvider.SavePreferences(ACfg: TXMLConfig);
begin
  inherited SavePreferences(ACfg);
  ACfg.SetValue(ClassName+'/TabControlWidth/Value', FChmFrame.pgcNavigation.Width);
end;

procedure TChmContentProvider.SearchButtonClick ( Sender: TObject ) ;
type
  TTopicEntry = record
    Topic:Integer;
    Hits: Integer;
    TitleHits: Integer;
    FoundForThisRound: Boolean;
  end;
  TFoundTopics = array of TTopicEntry;
var
  FoundTopics: TFoundTopics;

  procedure DeleteTopic(ATopicIndex: Integer);
  var
    MoveSize: DWord;
  begin
    //WriteLn('Deleting Topic');
    if ATopicIndex < High(FoundTopics) then
    begin
      MoveSize := SizeOf(TTopicEntry) * (High(FoundTopics) - (ATopicIndex+1));
      Move(FoundTopics[ATopicIndex+1], FoundTopics[ATopicIndex], MoveSize);
    end;
    SetLength(FoundTopics, Length(FoundTopics) -1);
  end;

  function GetTopicIndex(ATopicID: Integer): Integer;
  var
    i: Integer;
  begin
    Result := -1;
    for i := 0 to High(FoundTopics) do
    begin
      if FoundTopics[i].Topic = ATopicID then
        Exit(i);
    end;
  end;

  procedure UpdateTopic(TopicID: Integer; NewHits: Integer; NewTitleHits: Integer; AddNewTopic: Boolean);
  var
    TopicIndex: Integer;
  begin
    //WriteLn('Updating topic');
    TopicIndex := GetTopicIndex(TopicID);
    if TopicIndex = -1 then
    begin
      if AddNewTopic = False then
        Exit;
      SetLength(FoundTopics, Length(FoundTopics)+1);
      TopicIndex := High(FoundTopics);
      FoundTopics[TopicIndex].Topic := TopicID;
    end;

    FoundTopics[TopicIndex].FoundForThisRound := True;
    if NewHits > 0 then
      Inc(FoundTopics[TopicIndex].Hits, NewHits);
    if NewTitleHits > 0 then
      Inc(FoundTopics[TopicIndex].TitleHits, NewTitleHits);
  end;

var
  TopicResults: TChmWLCTopicArray;
  TitleResults: TChmWLCTopicArray;
  FIftiMainStream: TMemoryStream;
  SearchWords: TStringList;
  SearchReader: TChmSearchReader;
  DocTitle: String;
  DocURL: String;
  i: Integer;
  j: Integer;
  k: Integer;
  Item: TContentTreeNode;
begin
  //  if FKeywordCombo.Text = '' then Exit;
  SearchWords := TStringList.Create;
  try
    SearchWords.Delimiter := ' ';
    Searchwords.DelimitedText := FChmFrame.cboxKeyword.Text;
    if FChmFrame.cboxKeyword.Items.IndexOf(FChmFrame.cboxKeyword.Text) = -1 then
      FChmFrame.cboxKeyword.Items.Add(FChmFrame.cboxKeyword.Text);
    FChmFrame.tvSearchResults.BeginUpdate;
    FChmFrame.tvSearchResults.Items.Clear;
    //WriteLn('Search words: ', SearchWords.Text);
    for i := 0 to FChmFileList.Count-1 do
    begin
      for j := 0 to SearchWords.Count-1 do
      begin
        if FChmFileList.ChmReaders[i].SearchReader = nil then
        begin
          FIftiMainStream := FChmFileList.ChmReaders[i].GetObject('/$FIftiMain');
          if FIftiMainStream = nil then
            continue;
          SearchReader := TChmSearchReader.Create(FIftiMainStream, True); //frees the stream when done
          FChmFileList.ChmReaders[i].SearchReader := SearchReader;
        end
        else
          SearchReader := FChmFileList.ChmReaders[i].SearchReader;
        TopicResults := SearchReader.LookupWord(SearchWords[j], TitleResults);
        // Body results
        for k := 0 to High(TopicResults) do
          UpdateTopic(TopicResults[k].TopicIndex, High(TopicResults[k].LocationCodes), 0, j = 0);
        // Title results
        for k := 0 to High(TitleResults) do
          UpdateTopic(TitleResults[k].TopicIndex, 0, High(TitleResults[k].LocationCodes), j = 0);

        // Remove documents that don't have results
        k := 0;
        while k <= High(FoundTopics) do
        begin
          if FoundTopics[k].FoundForThisRound = False then
            DeleteTopic(k)
          else
          begin
            FoundTopics[k].FoundForThisRound := False;
            Inc(k);
          end;
        end;
      end;

      // Clear out results that don't contain all the words we are looking for

      Item := nil;
      // Now lookup titles and urls to add to final search results
      for j := 0 to High(FoundTopics) do
      begin
        try
          DocURL := FChmFileList.ChmReaders[i].LookupTopicByID(FoundTopics[j].Topic, DocTitle);
          if (Length(DocURL) > 0) and (DocURL[1] <> '/') then
            Insert('/', DocURL, 1);
          if DocTitle = '' then
            DocTitle := slhelp_Untitled;
          Item := TContentTreeNode(FChmFrame.tvSearchResults.Items.Add(Item, DocTitle));
          Item.Data:= FChmFileList.ChmReaders[i];
          Item.Url:= DocURL;
        except
          //WriteLn('Exception');
          // :)
        end;
      end;

      SetLength(FoundTopics, 0);
    end;
    SetLength(FoundTopics, 0);
  finally
    SearchWords.Free;
  end;

  if FChmFrame.tvSearchResults.Items.Count = 0 then
  begin
    FChmFrame.tvSearchResults.Items.Add(nil, slhelp_NoResults);
  end;
  FChmFrame.tvSearchResults.EndUpdate;
end;

procedure TChmContentProvider.SearchResultsDblClick ( Sender: TObject ) ;
var
  Item: TContentTreeNode;
begin
  Item := TContentTreeNode(FChmFrame.tvSearchResults.Selected);
  if (Item = nil) or (Item.Data = nil) then
    Exit;
  FLoadingSearchURL:= True;
  DoLoadUri(MakeURI(Item.Url, TChmReader(Item.Data)));
  FLoadingSearchURL:= False;
end;
{$ENDIF}


function TChmContentProvider.CanGoBack: Boolean;
begin
  Result := FHistoryIndex > 0;
end;

function TChmContentProvider.CanGoForward: Boolean;
begin
  Result := FHistoryIndex < FHistory.Count-1
end;

function TChmContentProvider.GetHistory: TStrings;
begin
  Result:= FHistory;
end;

function TChmContentProvider.LoadURL(const AURL: String; const AContext: THelpContext=-1): Boolean;
var
  sFile: String;
  sURL: String = '';
  iPos: Integer;
  FileIndex: Integer;
  LoadTOC: Boolean;
  CurCHM: TChmReader;
  ContextURL: String;
begin
  Result := False;

  if Pos('file://', AUrl) = 1 then
  begin
    // AURL = file://
    sFile := Copy(AUrl,8, Length(AURL));
  end
  else
  begin
    sFile := AUrl;
    iPos := Pos('://', sFile);
    if iPos > 0 then
      sFile := Copy(sFile, iPos+3, Length(sFile));
  end;

  iPos := Pos('://', sFile);
  if iPos > 0 then
  begin
    sURL := Copy(sFile, iPos+3, Length(sFile));
    sFile := Copy(sFile, 1, iPos-1);
  end;

  CurCHM := nil;
  // try to find already open CHM
  if Assigned(FChmFileList) then
  begin
    FileIndex := FChmFileList.IndexOf(sFile);
    if FileIndex <> -1 then
      CurCHM := FChmFileList.ChmReaders[FileIndex];
  end;

  if not Assigned(CurCHM) then
  begin
    LoadTOC := (FChmFileList = nil) or (FChmFileList.IndexOf(sFile) < 0);
    DoOpenChm(sFile, False);

    // in case of exception FChmFileList can be still = nil
    if Assigned(FChmFileList) then
      FileIndex := FChmFileList.IndexOf(sFile)
    else
      Exit;

    CurCHM := FChmFileList.ChmReaders[FileIndex];

    if LoadTOC and (FileIndex = 0) then
    begin
      QueueFillToc(CurCHM);
    end;
  end;

  // AContext will override the URL if it is found
  if AContext <> -1 then
  begin
    ContextURL := CurCHM.GetContextUrl(AContext);
    if (Length(ContextURL) > 0) and not (ContextURL[1] in ['/', '\']) then
      Insert('/', ContextURL , 1);
    if Length(ContextURL) > 0 then
      sURL := ContextURL;
  end;

  if sURL <> '' then
    DoLoadUri(MakeURI(sURL, CurCHM))
  else
    DoLoadUri(MakeURI(CurCHM.DefaultPage, CurCHM));
  Result := True;

  FChmFileList.OnOpenNewFile := @NewChmOpened;
end;

procedure TChmContentProvider.GoHome;
begin
  if (FChmFileList <> nil) and (FChmFileList.ChmReaders[0].DefaultPage <> '') then
  begin
    DoLoadUri(MakeURI(FChmFileList.ChmReaders[0].DefaultPage, FChmFileList.ChmReaders[0]));
  end;
end;

procedure TChmContentProvider.GoBack;
begin
  if CanGoBack then
  begin
    Dec(FHistoryIndex);
    FIsUsingHistory := True;
    FHtml.OpenURL(FHistory.Strings[FHistoryIndex]);
  end;
end;

procedure TChmContentProvider.GoForward;
var
  HistoryChm: TChmReader;
begin
  if CanGoForward then
  begin
    Inc(FHistoryIndex);
    FIsUsingHistory := True;
    HistoryChm := TChmReader(FHistory.Objects[FHistoryIndex]);
    FChmFileList.ObjectExists(FHistory.Strings[FHistoryIndex], HistoryChm); // this ensures that the correct chm will be found
    FHtml.OpenURL(FHistory.Strings[FHistoryIndex]);
  end;
end;

class function TChmContentProvider.GetProperContentProviderClass(const AURL: String
  ): TBaseContentProviderClass;
begin
  Result:=TChmContentProvider;
end;

constructor TChmContentProvider.Create(AParent: TWinControl; AImageList: TImageList);
const
  TAB_WIDTH = 215;
begin
  inherited Create(AParent, AImageList);

  FHistory := TStringList.Create;

  FChmFrame := TFrameChm.Create(AParent);
  FChmFrame.Parent := AParent;
  FChmFrame.Align := alClient;

  FChmFrame.tsContents.Caption := slhelp_Contents;

  FChmFrame.tvContents.OnSelectionChanged := @ContentsTreeSelectionChanged;
  FChmFrame.tvContents.OnExpanded := @TOCExpand;
  FChmFrame.tvContents.OnCollapsed := @TOCCollapse;
  FChmFrame.tvContents.OnCreateNodeClass := @GetTreeNodeClass;
  FChmFrame.tvContents.Images := FImageList;

  FChmFrame.tsIndex.Caption := slhelp_Index;
  FChmFrame.edIndexSearch.Caption := slhelp_Search;

  FChmFrame.tvIndex.OnCollapsing := @TreeViewStopCollapse;
  FChmFrame.tvIndex.OnDblClick := @IndexViewDblClick;
  FChmFrame.tvIndex.OnCreateNodeClass := @GetTreeNodeClass;

 // {$IFDEF CHM_SEARCH}
  FChmFrame.tsSearch.Caption := slhelp_Search;

  FChmFrame.lbKeyword.Caption := slhelp_Keyword;

  FChmFrame.cboxKeyword.OnKeyDown  := @SearchComboKeyDown;

  FChmFrame.btnSearch.Caption := slhelp_Find;
  FChmFrame.btnSearch.OnClick := @SearchButtonClick;

  FChmFrame.lbResults.Caption := slhelp_SearchResults;

  FChmFrame.tvSearchResults.OnDblClick := @SearchResultsDblClick;
  FChmFrame.tvSearchResults.OnCollapsing := @TreeViewStopCollapse;
  FChmFrame.tvSearchResults.OnCreateNodeClass := @GetTreeNodeClass;
 // {$ENDIF}

  FHtml := FChmFrame.IpHtmlPanel;
  FChmFrame.IpHtmlPanel.DataProvider := TIpChmDataProvider.Create(FChmFrame.IpHtmlPanel, FChmFileList);
  TIpChmDataProvider(FChmFrame.IpHtmlPanel.DataProvider).OnGetHtmlPage:=@LoadingHTMLStream;
  FChmFrame.IpHtmlPanel.OnDocumentOpen := @IpHtmlPanelDocumentOpen;
  FChmFrame.IpHtmlPanel.OnHotChange := @IpHtmlPanelHotChange;
  FChmFrame.IpHtmlPanel.OnHotClick := @IpHtmlPanelHotClick;

  FChmFrame.miCopy.Caption := slhelp_Copy;
end;

destructor TChmContentProvider.Destroy;
begin
  DoCloseChm;
  FHistory.Free;
  inherited Destroy;
end;

initialization

  RegisterFileType('.chm', TChmContentProvider);

end.

