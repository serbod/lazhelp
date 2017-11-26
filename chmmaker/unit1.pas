{ Copyright (C) <2008> <Andrew Haines> }
unit unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, chmsitemap, chmfilewriter, Forms, Controls,
  Graphics, Dialogs, StdCtrls, ComCtrls, Menus, ExtCtrls, EditBtn, ActnList,
  LazFileUtils, UTF8Process, chmreader, chmtypes, lcid_conv;

type

  { TProjImportThread }

  TProjImportThread = class(TThread)
  protected
    procedure Execute(); override;
    procedure SyncProc();
    procedure SetStatusStr(const AText: string);
  public
    Project: TChmProject;
    ChmFilename: string;
    StatusStr: string;
  end;

  { TCHMForm }

  TCHMForm = class(TForm)
    actCompileAndView: TAction;
    actCompile: TAction;
    actAbout: TAction;
    actExit: TAction;
    actProjectImport: TAction;
    actProjectClose: TAction;
    actProjectSaveAs: TAction;
    actProjectSave: TAction;
    actProjectOpen: TAction;
    actProjectNew: TAction;
    alMain: TActionList;
    AddFilesBtn: TButton;
    AutoAddLinksBtn: TButton;
    AddAllBtn: TButton;
    cbCodepage: TComboBox;
    chkBinaryIndex: TCheckBox;
    chkBinaryTOC: TCheckBox;
    CompileViewBtn: TButton;
    CompileBtn: TButton;
    DefaultPageCombo: TComboBox;
    ChmFileNameEdit: TFileNameEdit;
    edProjectDir: TEdit;
    edTitle: TEdit;
    chkScanHtmlContents: TCheckBox;
    CreateSearchableCHMCheck: TCheckBox;
    CompileTimeOptionsLabel: TLabel;
    FilesNoteLabel: TLabel;
    DefaultPageLabel: TLabel;
    lbContextInfo: TLabel;
    lbCHMFilename: TLabel;
    lbProjectDir: TLabel;
    lbCodepage: TLabel;
    lbTitle: TLabel;
    lvAliases: TListView;
    MemoLog: TMemo;
    miProjectImport: TMenuItem;
    OpenDialog2: TOpenDialog;
    OpenDialogImportChm: TOpenDialog;
    pgcMain: TPageControl;
    RemoveFilesBtn: TButton;
    btnTOCEdit: TButton;
    btnIndexEdit: TButton;
    edIndexFilename: TFileNameEdit;
    gbFiles: TGroupBox;
    FileListBox: TListBox;
    TableOfContentsLabel: TLabel;
    IndexLabel: TLabel;
    MainMenu1: TMainMenu;
    miProjectMenu: TMenuItem;
    miProjectSave: TMenuItem;
    miProjectSaveAs: TMenuItem;
    MenuItem12: TMenuItem;
    miExit: TMenuItem;
    miCompileMenu: TMenuItem;
    miCompile: TMenuItem;
    miCompileAndView: TMenuItem;
    miProjectClose: TMenuItem;
    miHelpMenu: TMenuItem;
    miHelp: TMenuItem;
    MenuItem5: TMenuItem;
    miAbout: TMenuItem;
    miProjectNew: TMenuItem;
    miProjectOpen: TMenuItem;
    MenuItem9: TMenuItem;
    OpenDialog1: TOpenDialog;
    MainPanel: TPanel;
    Panel2: TPanel;
    SaveDialog1: TSaveDialog;
    StatusBar1: TStatusBar;
    edTOCFilename: TFileNameEdit;
    tmrImport: TTimer;
    tsContext: TTabSheet;
    tsLog: TTabSheet;
    tsMain: TTabSheet;
    procedure actAboutExecute(Sender: TObject);
    procedure actCompileAndViewExecute(Sender: TObject);
    procedure actCompileExecute(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actProjectCloseExecute(Sender: TObject);
    procedure actProjectImportExecute(Sender: TObject);
    procedure actProjectNewExecute(Sender: TObject);
    procedure actProjectOpenExecute(Sender: TObject);
    procedure actProjectSaveAsExecute(Sender: TObject);
    procedure actProjectSaveExecute(Sender: TObject);
    procedure AddAllBtnClick(Sender: TObject);
    procedure AddFilesBtnClick(Sender: TObject);
    procedure AutoAddLinksBtnClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ChmFileNameEditAcceptFileName(Sender: TObject; var Value: string);
    procedure FileListBoxDrawItem({%H-}Control: TWinControl; Index: Integer;
      ARect: TRect; {%H-}State: TOwnerDrawState);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure edIndexFilenameAcceptFileName(Sender: TObject; var Value: string);
    procedure btnIndexEditClick(Sender: TObject);
    procedure lvAliasesData(Sender: TObject; Item: TListItem);
    procedure RemoveFilesBtnClick(Sender: TObject);
    procedure edTOCFilenameAcceptFileName(Sender: TObject; var Value: string);
    procedure btnTOCEditClick(Sender: TObject);
    procedure tmrImportTimer(Sender: TObject);
  private
    FModified: Boolean;
    FProjectFileName: string;
    FProjImportThread: TProjImportThread;
    procedure AddItems({%H-}AParentItem: TTreeNode; {%H-}ChmItems: TChmSiteMapItems);

    function GetModified: Boolean;
    procedure SaveProject(AskFilename: Boolean);
    procedure CloseProject();
    function CompileProject(): Boolean;

    procedure AddFilesToProject(Strings: TStrings);
    procedure InitFileDialog(Dlg: TFileDialog);
    procedure ProjectDirChanged();
    procedure UpdateAliasesList();

    procedure SetStatusStr(const AText: string);

    function CreateRelativeProjectFile(Filename: string): string;
    function CreateAbsoluteProjectFile(Filename: string): string;
  public
    Project: TChmProject;
    procedure OpenProject(AFileName: string);
    // Dirty flag: has project been modified since opening?
    property Modified: Boolean read GetModified write FModified;
  end;

var
  CHMForm: TCHMForm;

implementation

{$R *.lfm}

uses CHMSiteMapEditor, LHelpControl, Process;


function StripPath(s: string): string;
begin
  if Copy(s, 1, 1) = '/' then
    Result := Copy(s, 2, MaxInt)
  else
    Result := s;
end;

procedure ChmErrorHandler(Project: TChmProject; ErrorKind: TChmProjectErrorKind;
    Msg: string; DetailLevel: Integer = 0);
var
  s: string;
begin
  s := FormatDateTime('HH:NN:SS.ZZZ ', Now());
  case ErrorKind of
    chmError: s := s + 'ERRR ';
    chmWarning: s := s + 'WARN ';
    chmHint: s := s + 'HINT ';
    chmNote: s := s + 'NOTE ';
    chmNone: s := s + 'NONE ';
  end;
  CHMForm.MemoLog.Append(s + Msg);
end;

{ TProjImportThread }

procedure TProjImportThread.Execute();
var
  ChmReader: TChmReader;
  s, sProjectFileName, sProjectDir: string;
  fsChm, fs: TFileStream;
  SiteMap: TChmSiteMap;
  sl: TStringList;
  i: Integer;
  ContextItem: TContextItem;
begin
  if Terminated then Exit;
  sProjectDir := IncludeTrailingPathDelimiter(ExtractFileDir(ChmFilename));
  sProjectFileName := sProjectDir + ExtractFileNameOnly(ChmFilename)+'.hfp';
  //OpenProject(sProjectFileName);

  fsChm := TFileStream.Create(ChmFilename, fmOpenRead);
  ChmReader := TChmReader.Create(fsChm, False);
  try
    // get Project settings
    Project.FileName := sProjectFileName;
    Project.OutputFileName := ExtractFileName(ChmFilename);
    Project.Title := ChmReader.Title;
    Project.DefaultPage := StripPath(ChmReader.DefaultPage);
    Project.LocaleID := ChmReader.LocaleID;
    Project.DefaultFont := ChmReader.PreferedFont;

    // extract TOC
    Project.TableOfContentsFileName := StripPath(ChmReader.TOCFile);
    if Project.TableOfContentsFileName <> '' then
    begin
      SetStatusStr('Extracting: Table of Contents');
      SiteMap := TChmSiteMap.Create(stTOC);
      try
        ChmReader.ReadTOCSitemap(SiteMap);
        SiteMap.SaveToFile(sProjectDir + Project.TableOfContentsFileName);
      finally
        FreeAndNil(SiteMap);
      end;
    end;

    // extract Index
    Project.IndexFileName := StripPath(ChmReader.IndexFile);
    if Project.IndexFileName <> '' then
    begin
      SetStatusStr('Extracting: Index');
      SiteMap := TChmSiteMap.Create(stIndex);
      try
        ChmReader.ReadIndexSitemap(SiteMap);
        SiteMap.SaveToFile(sProjectDir + Project.IndexFileName);
      finally
        FreeAndNil(SiteMap);
      end;
    end;

    // extract Aliases (Context)
    sl := TStringList.Create();
    //fs := TFileStream.Create(sProjectDir + '_alias.ini', fmCreate);
    try
      SetStatusStr('Extracting: Constext');
      for i := 0 to ChmReader.ContextList.Count-1 do
      begin
        ContextItem := ChmReader.ContextList.GetItem(i);
        if Assigned(ContextItem) then
        begin
          Project.ContextList.AddContext(ContextItem.ContextID, ContextItem.UrlAlias, ContextItem.Url);
          sl.Add(ContextItem.UrlAlias + '=' + ContextItem.Url);
        end;
      end;
      if sl.Count > 0 then
        sl.SaveToFile(sProjectDir + '_context.ali');
    finally
      //FreeAndNil(fs);
      FreeAndNil(sl);
    end;

    // extract files
    sl := TStringList.Create();
    try
      ChmReader.ReadFilesNamesList(sl, False);
      for i := 0 to sl.Count-1 do
      begin
        //s := ExtractFileName(sl[i]);
        s := StripPath(sl[i]);
        if Length(s) > 1 then
        begin
          SetStatusStr('Extracting: ('+IntToStr(i+1) + '/' + IntToStr(sl.Count)+ '): ' + s);
          if Copy(s, Length(s), 1) = '/' then
          begin
            // folder
            s := Copy(s, 1, Length(s)-1);
            CreateDir(sProjectDir + s);
            Continue;
          end;

          if (s = Project.TableOfContentsFileName)
          or (s = Project.IndexFileName) then
            Continue;
          fs := TFileStream.Create(sProjectDir + s, fmCreate);
          try
            ChmReader.ReadFileContent(sl[i], fs);
            if Project.Files.IndexOf(s) = -1 then
              Project.Files.Append(s);
          finally
            FreeAndNil(fs);
          end;
        end;
      end;

    finally
      sl.Free();
    end;

  finally
    ChmReader.Free();
    fsChm.Free();
  end;
  SetStatusStr('Saving project..');
  Project.SaveToFile(sProjectFileName);
  //CloseProject();
  SetStatusStr('');
  Terminate();
end;

procedure TProjImportThread.SyncProc();
begin
  CHMForm.StatusBar1.SimpleText := StatusStr;
end;

procedure TProjImportThread.SetStatusStr(const AText: string);
begin
  StatusStr := AText;
  Synchronize(@SyncProc);
end;

{ TCHMForm }

procedure TCHMForm.AddItems(AParentItem: TTreeNode; ChmItems: TChmSiteMapItems);
begin
{    for I := 0 to ChmItems.Count-1 do begin
      Item := TreeView1.Items.AddChild(AParentItem, ChmItems.Item[I].Text);
      AddItems(Item, ChmItems.Item[I].Children);
    end;
 }
end;

procedure TCHMForm.Button1Click(Sender: TObject);
begin
  {SiteMap := TChmSiteMap.Create(stTOC);
  OpenDialog1.InitialDir := GetCurrentDir;
  if OpenDialog1.Execute = False then Exit;
  SiteMap.LoadFromFile(OpenDialog1.FileName);
  AddItems(nil, sitemap.Items);
  
  Stream := TMemoryStream.Create;
  
  Sitemap.SaveToStream(Stream);
  Stream.Position := 0;
  
  SynEdit1.Lines.LoadFromStream(Stream);
  Stream.Free;
   }
end;

procedure TCHMForm.AddFilesBtnClick(Sender: TObject);
begin
  InitFileDialog(OpenDialog2);
  if OpenDialog2.Execute = False then
    exit;
  Modified := True;
  AddFilesToProject(OpenDialog2.Files);
end;

procedure TCHMForm.AddAllBtnClick(Sender: TObject);
var
  Files: TStrings;

  procedure AddDir(ADir: string);
  var
    SearchRec: TSearchRec;
    FileName: string;
  begin
    // WriteLn('Adding Dir: ', ADir);
    if FindFirst(ADir + '*', faAnyFile or faDirectory, SearchRec) = 0 then
    begin
      repeat
        if (SearchRec.Attr and faDirectory) <> 0 then
        begin
          if Pos('.', SearchRec.Name) = 0 then
          begin
            AddDir(IncludeTrailingPathDelimiter(ADir + SearchRec.Name));
          end;
        end
        else
        begin
          FileName := ADir + SearchRec.Name;
          FileName := ExtractRelativepath(Project.ProjectDir, FileName);
          if Files.IndexOf(FileName) = -1 then
            Files.Add(FileName);
        end;
      until FindNext(SearchRec) <> 0;
      FindClose(SearchRec);
    end;
  end;

begin
  if MessageDlg('This will add all files in the project directory ' +
    LineEnding + 'recursively. Do you want to continue?',
    mtConfirmation, [mbYes, mbNo], 0) = mrNo then
    exit;
  Modified := True;
  Files := TStringList.Create();
  try
    Files.AddStrings(FileListBox.Items);
    AddDir(Project.ProjectDir);
    FileListBox.Items.Assign(Files);
  finally
    Files.Free();
  end;
end;

procedure TCHMForm.actProjectNewExecute(Sender: TObject);
begin
  InitFileDialog(SaveDialog1);
  if SaveDialog1.Execute then
  begin
    if FileExists(SaveDialog1.FileName)
    and (MessageDlg('File Already Exists! Ovewrite?', mtWarning, [mbYes, mbNo], 0) = mrNo) then
      Exit;
    OpenProject(SaveDialog1.FileName);
    Project.SaveToFile(SaveDialog1.FileName);
  end;
end;

procedure TCHMForm.actProjectOpenExecute(Sender: TObject);
begin
  InitFileDialog(OpenDialog1);
  if OpenDialog1.Execute then
  begin
    CloseProject();
    OpenProject(OpenDialog1.FileName);
  end;
end;

procedure TCHMForm.actProjectSaveExecute(Sender: TObject);
begin
  SaveProject(False);
end;

procedure TCHMForm.actProjectSaveAsExecute(Sender: TObject);
begin
  SaveProject(True);
end;

procedure TCHMForm.actProjectCloseExecute(Sender: TObject);
begin
  CloseProject();
end;

procedure TCHMForm.actProjectImportExecute(Sender: TObject);
var
  sProjectFileName, sProjectDir: string;
begin
  MessageDlg('Import CHM notes', 'Place CHM file into empty directory.'
    + sLineBreak + 'New Project will be created in that directory.',
    mtInformation, [mbOk], 0);

  if OpenDialogImportChm.Execute then
  begin
    // create project
    CloseProject();
    sProjectDir := IncludeTrailingPathDelimiter(ExtractFileDir(OpenDialogImportChm.FileName));
    sProjectFileName := sProjectDir + ExtractFileNameOnly(OpenDialogImportChm.FileName)+'.hfp';
    //OpenProject(sProjectFileName);
    if not Assigned(Project) then
      Project := TChmProject.Create();
    Project.OnError := @ChmErrorHandler;
    FProjectFileName := sProjectFileName;

    actProjectNew.Enabled := False;
    actProjectOpen.Enabled := False;
    actProjectImport.Enabled := False;

    FProjImportThread := TProjImportThread.Create(True);
    FProjImportThread.ChmFilename := OpenDialogImportChm.FileName;
    FProjImportThread.Project := Project;
    FProjImportThread.Suspended := False;
    tmrImport.Enabled := True;
  end;
end;

procedure TCHMForm.actExitExecute(Sender: TObject);
begin
  Close();
end;

procedure TCHMForm.actCompileExecute(Sender: TObject);
begin
  CompileProject();
end;

procedure TCHMForm.actCompileAndViewExecute(Sender: TObject);
var
  LHelpName: string;
  LHelpConn: TLHelpConnection;
  Proc: TProcessUTF8;
  ext: string;
  res: Integer;
begin
  if not CompileProject() then Exit;

  // open
  // ...
  ext := ExtractFileExt(Application.ExeName);
  LHelpName := '../../components/chmhelp/lhelp/lhelp' + ext;
  if not FileExists(LHelpName) then
  begin
    if MessageDlg('LHelp could not be located at ' +
      LHelpName + ' Try to build using lazbuild?', mtError, [mbCancel, mbYes], 0) = mrYes then
    begin
      if not FileExists('../../lazbuild' + ext) then
      begin
        MessageDlg('lazbuild coul not be found.', mtError, [mbCancel], 0);
        Exit;
      end;
      Proc := TProcessUTF8.Create(Self);
      try
        Proc.CommandLine := '../../../lazbuild ./lhelp.lpi';
        SetCurrentDir('../../components/chmhelp/lhelp/');
        Proc.Options := [poWaitOnExit];
        Proc.Execute;
        res := Proc.ExitStatus;
      finally
        Proc.Free();
      end;
      SetCurrentDir('../../../tools/chmmaker/');
      if res <> 0 then
      begin
        MessageDlg('lhelp failed to build', mtError, [mbCancel], 0);
        Exit;
      end;
    end
    else
      Exit;
  end;

  LHelpConn := TLHelpConnection.Create();
  try
    LHelpConn.StartHelpServer('chmmaker', LHelpName);
    LHelpConn.OpenFile(ChmFileNameEdit.FileName);
  finally
    LHelpConn.Free();
  end;
end;

procedure TCHMForm.actAboutExecute(Sender: TObject);
begin
  MessageDlg('About chmmaker', 'CHM (Compiled HTML Help) maker'
    + sLineBreak + 'version 1.1'
    + sLineBreak + ''
    + sLineBreak + 'Authors: FreePascal and Lazarus community',
    mtInformation, [mbOk], 0);
end;

procedure TCHMForm.AutoAddLinksBtnClick(Sender: TObject);
begin
  Project.ScanHtml();
  Modified := True;
end;

procedure TCHMForm.Button2Click(Sender: TObject);
begin
    {
  if OpenDialog1.Execute = False then Exit;
  OutStream := TFileStream.Create('/home/andrew/test.chm', fmCreate or fmOpenWrite);
  Chm := TChmWriter.Create(OutStream, False);
  Chm.FilesToCompress.AddStrings(OpenDialog1.Files);
  Chm.GetFileData := @GetData;
  Chm.Title := 'test';
  Chm.DefaultPage := 'index.html';
  Chm.Execute;
  OutStream.Free;
  Chm.Free;
     }

end;

procedure TCHMForm.ChmFileNameEditAcceptFileName(Sender: TObject; var Value: string);
begin
  if ExtractFileExt(Value) = '' then
    Value := Value + '.chm';
end;

procedure TCHMForm.FileListBoxDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
begin
  FileListbox.Canvas.FillRect(ARect);
  if Pos('..', FileListBox.Items.Strings[Index]) > 0 then
  begin
    // These items won't be added to the chm because they are not within the project dir
    // so mark them with a red rectangle
    Dec(ARect.Right);
    Dec(ARect.Bottom);
    FileListBox.Canvas.Pen.Color := clRed;
    FileListBox.Canvas.Frame(ARect);
  end;
  // Draw item text
  FileListBox.Canvas.TextRect(ARect,
    2, (ARect.Top + ARect.Bottom - FileListbox.Canvas.TextHeight('Tg')) div 2,
    FileListBox.Items[Index]
    );
end;

procedure TCHMForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  MResult: Integer;
begin
  if Modified then
  begin
    MResult := MessageDlg('Project is modified would you like to save the changes?',
      mtConfirmation, [mbYes, mbNo, mbCancel], 0);
    case MResult of
      mrYes: SaveProject(False);
      mrNo: CloseAction := caFree;
      mrCancel: CloseAction := caNone;
    else
      CloseAction := caNone;
    end;
  end;
end;

procedure TCHMForm.FormCreate(Sender: TObject);
begin
  pgcMain.ActivePage := tsMain;
  CloseProject();
end;

procedure TCHMForm.FormDestroy(Sender: TObject);
begin
  CloseProject();
end;

procedure TCHMForm.edTOCFilenameAcceptFileName(Sender: TObject; var Value: string);
begin
  Modified := True;
  Project.TableOfContentsFileName := ExtractFileName(Value);
  Value := Project.TableOfContentsFileName;
end;

procedure TCHMForm.edIndexFilenameAcceptFileName(Sender: TObject; var Value: string);
begin
  Modified := True;
  //Value := ExtractRelativepath(Project.ProjectDir, Value);
  //WriteLn(Value);
  Project.IndexFileName := ExtractFileName(Value);
  Value := Project.IndexFileName;
end;

procedure TCHMForm.btnIndexEditClick(Sender: TObject);
var
  Stream: TStream;
  FileName: string;
begin
  FileName := edIndexFilename.FileName;
  if FileName = '' then
  begin
    FileName := '_index.hhk';
    edIndexFilename.FileName := FileName;
  end;
  FileName := IncludeTrailingPathDelimiter(Project.ProjectDir) + FileName;

  if FileExists(FileName) then
  begin
    Stream := TFileStream.Create(FileName, fmOpenReadWrite);
  end
  else
  begin
    Stream := TFileStream.Create(FileName, fmCreate or fmOpenReadWrite);
  end;

  try
    if SitemapEditForm.Execute(Stream, stIndex, FileListBox.Items) then
      edIndexFilename.FileName := FileName;
  finally
    Stream.Free();
  end;
end;

procedure TCHMForm.lvAliasesData(Sender: TObject; Item: TListItem);
var
  ContextItem: TContextItem;
begin
  if Assigned(Project) and Assigned(Item) then
  begin
    ContextItem := Project.ContextList.GetItem(Item.Index);
    if Assigned(ContextItem) then
    begin
      Item.Caption := IntToStr(ContextItem.ContextID);
      Item.SubItems.Clear();
      Item.SubItems.Add(ContextItem.UrlAlias);
      Item.SubItems.Add(ContextItem.Url);
    end;
  end;
end;

procedure TCHMForm.btnTOCEditClick(Sender: TObject);
var
  Stream: TStream;
  FileName: string;
  BDir: string;
begin
  FileName := edTOCFilename.FileName;
  if FileName = '' then
  begin
    FileName := '_table_of_contents.hhc';
    edTOCFilename.FileName := FileName;
  end;
  FileName := IncludeTrailingPathDelimiter(Project.ProjectDir) + FileName;

  if FileExists(FileName) then
  begin
    Stream := TFileStream.Create(FileName, fmOpenReadWrite);
  end
  else
  begin
    Stream := TFileStream.Create(FileName, fmCreate or fmOpenReadWrite);
  end;

  try
    BDir := ExtractFilePath(Project.FileName);
    FileName := ExtractRelativepath(BDir, FileName);
    if SitemapEditForm.Execute(Stream, stTOC, FileListBox.Items) then
      edTOCFilename.FileName := FileName;
  finally
    Stream.Free();
  end;
end;

procedure TCHMForm.tmrImportTimer(Sender: TObject);
begin
  if Assigned(FProjImportThread) then
  begin
    if FProjImportThread.Terminated then
    begin
      FreeAndNil(FProjImportThread);

      OpenProject(FProjectFileName);
      ShowMessage('CHM file ' + ChmFileNameEdit.FileName + ' imported.');

      actProjectNew.Enabled := True;
      actProjectOpen.Enabled := True;
      actProjectImport.Enabled := True;
    end;
  end
  else
    tmrImport.Enabled := False;
end;

procedure TCHMForm.RemoveFilesBtnClick(Sender: TObject);
var
  i: Integer;
begin
  Modified := True;
  for i := FileListBox.Items.Count - 1 downto 0 do
  begin
    if FileListBox.Selected[i] then
      FileListBox.Items.Delete(i);
  end;
  DefaultPageCombo.Items.Assign(FileListBox.Items);
end;

function TCHMForm.GetModified: Boolean;
begin
  Result := (Project <> nil) and FModified;
end;

procedure TCHMForm.SaveProject(AskFilename: Boolean);
var
  sExt: string;
begin
  if AskFilename or (Project.FileName = '') then
  begin
    InitFileDialog(SaveDialog1);
    if SaveDialog1.Execute then
    begin
      //Project.FileName := ChangeFileExt(SaveDialog1.FileName, '.hfp');
      Project.FileName := SaveDialog1.FileName;
      ProjectDirChanged();
    end;
  end;
  Project.Files.Assign(FileListBox.Items);
  Project.Title := Trim(edTitle.Text);
  Project.DefaultPage := DefaultPageCombo.Text;
  Project.TableOfContentsFileName := edTOCFilename.FileName;
  Project.IndexFileName := edIndexFilename.FileName;
  Project.MakeBinaryTOC := chkBinaryTOC.Checked;
  Project.MakeBinaryIndex := chkBinaryIndex.Checked;
  Project.ScanHtmlContents := chkScanHtmlContents.Checked;
  Project.MakeSearchable := CreateSearchableCHMCheck.Checked;
  Project.OutputFileName := CreateRelativeProjectFile(ChmFileNameEdit.FileName);

  case cbCodepage.ItemIndex of
    0: Project.LocaleID := 0;
    1: Project.LocaleID := 1052; // CP: 1250 - Albanian
    2: Project.LocaleID := 1049; // CP: 1251 - Russian
    3: Project.LocaleID := 1033; // CP: 1252 - English - United States
    4: Project.LocaleID := 1032; // CP: 1253 - Greek
    5: Project.LocaleID := 1055; // CP: 1254 - Turkish
    6: Project.LocaleID := 1037; // CP: 1255 - Hebrew
    7: Project.LocaleID := 1025; // CP: 1256 - Arabic - Saudi Arabia
    8: Project.LocaleID := 1061; // CP: 1257 - Estonian
    9: Project.LocaleID := 1066; // CP: 1258 - Vietnamese
  end;

  sExt := ExtractFileExt(Project.FileName);
  if sExt = '.hhp' then
    Project.SaveToHHP(Project.FileName)
  else
    Project.SaveToFile(Project.FileName);

  Modified := False;
end;

procedure TCHMForm.CloseProject();
begin
  FileListBox.Clear();
  DefaultPageCombo.Clear();
  edTOCFilename.Clear();
  edIndexFilename.Clear();
  gbFiles.Enabled := False;
  MainPanel.Enabled := False;
  miCompileMenu.Enabled := False;
  miProjectSaveAs.Enabled := False;
  miProjectSave.Enabled := False;
  miProjectClose.Enabled := False;

  chkScanHtmlContents.Checked := False;
  CreateSearchableCHMCheck.Checked := False;
  FreeAndNil(Project);
end;

function TCHMForm.CompileProject(): Boolean;
var
  OutFile: TFileStream;
begin
  Result := False;
  if ChmFileNameEdit.FileName = '' then
  begin
    MessageDlg('You must set a filename for the output CHM file!',
      mtError, [mbCancel], 0);
    Exit;
  end;

  SaveProject(False);

  OutFile := TFileStream.Create(Project.OutputFileName, fmCreate or fmOpenWrite);
  try
    Project.WriteChm(OutFile);
    ShowMessage('CHM file ' + ChmFileNameEdit.FileName + ' was created.');
    Result := True;
  finally
    OutFile.Free();
  end;
end;

procedure TCHMForm.OpenProject(AFileName: string);
var
  s: string;
  wcp: Word;
begin
  if not Assigned(Project) then
    Project := TChmProject.Create();
  Project.OnError := @ChmErrorHandler;

  s := LowerCase(ExtractFileExt(AFileName));
  if s = '.hfp' then
    Project.LoadFromFile(AFileName)
  else if s = '.hhp' then
    Project.LoadFromHHP(AFileName)
  else
  begin
    ShowMessage('Incorect file: ' + AFileName);
    FreeAndNil(Project);
    Exit;
  end;

  FProjectFileName := AFileName;
  gbFiles.Enabled := True;
  MainPanel.Enabled := True;
  miCompileMenu.Enabled := True;
  miProjectSaveAs.Enabled := True;
  miProjectSave.Enabled := True;
  miProjectClose.Enabled := True;

  FileListBox.Items.Clear();
  FileListBox.Items.AddStrings(Project.Files);
  edTitle.Text := Project.Title;
  edTOCFilename.FileName := Project.TableOfContentsFileName;
  edIndexFilename.FileName := Project.IndexFileName;
  chkBinaryTOC.Checked := Project.MakeBinaryTOC;
  chkBinaryIndex.Checked := Project.MakeBinaryIndex;
  DefaultPageCombo.Items.Assign(FileListBox.Items);
  DefaultPageCombo.Text := Project.DefaultPage;
  chkScanHtmlContents.Checked := Project.ScanHtmlContents;
  CreateSearchableCHMCheck.Checked := Project.MakeSearchable;
  ChmFileNameEdit.FileName := Project.OutputFileName;

  cbCodepage.ItemIndex := 0;
  if Project.LocaleID <> 0 then
  begin
    wcp := LCIDToWinCP(Project.LocaleID);
    if (wcp >= 1250) and (wcp <= 1258) then
      cbCodepage.ItemIndex := (wcp - 1250 + 1);
  end;

  UpdateAliasesList();

  ProjectDirChanged();
end;

procedure TCHMForm.AddFilesToProject(Strings: TStrings);
var
  BDir: string;
  I: Integer;
  RelativePath: string;
  FileName: string;
begin
  Modified := True;
  BDir := ExtractFilePath(Project.FileName);

  for I := 0 to Strings.Count - 1 do
  begin
    FileName := Strings.Strings[I];

    RelativePath := ExtractRelativepath(BDir, FileName);
    if Pos('..', RelativePath) > 0 then
      FileListBox.Items.AddObject(RelativePath, TObject(1))
    else
      FileListBox.Items.AddObject(RelativePath, TObject(0));
  end;
  DefaultPageCombo.Items.Assign(FileListBox.Items);
end;

procedure TCHMForm.InitFileDialog(Dlg: TFileDialog);
var
  Dir: string;
begin
  Dir := '';
  if (Project <> nil) then
    Dir := ExtractFilePath(Project.FileName);
  if not DirPathExists(Dir) then
    Dir := GetCurrentDirUTF8;
  Dlg.InitialDir := Dir;
end;

procedure TCHMForm.ProjectDirChanged();
var
  Dir: string;
begin
  if Project = nil then
    exit;
  Dir := ExtractFilePath(Project.FileName);

  edProjectDir.Text := Dir;
  edTOCFilename.InitialDir := Dir;
  edIndexFilename.InitialDir := Dir;
  ChmFileNameEdit.InitialDir := Dir;
end;

procedure TCHMForm.UpdateAliasesList();
begin
  if Assigned(Project) then
    lvAliases.Items.Count := Project.ContextList.Count
  else
    lvAliases.Items.Count := 0;

  lvAliases.Invalidate();
end;

procedure TCHMForm.SetStatusStr(const AText: string);
begin
  StatusBar1.SimpleText := AText;
end;

function TCHMForm.CreateRelativeProjectFile(Filename: string): string;
begin
  Result := Filename;
  if (Project = nil) or (not FilenameIsAbsolute(Project.FileName)) then
    exit;
  Result := CreateRelativePath(Filename, ExtractFilePath(Project.FileName));
end;

function TCHMForm.CreateAbsoluteProjectFile(Filename: string): string;
begin
  Result := Filename;
  if FilenameIsAbsolute(Result) then
    exit;
  if (Project = nil) or (not FilenameIsAbsolute(Project.FileName)) then
    exit;
  Result := ExtractFilePath(Project.FileName) + Filename;
end;

end.
