{ Copyright (C) 2005-2014  Andrew Haines, Lazarus contributors

  Main form for lhelp. Includes processing/data communication.

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
  Boston, MA 02110-1335, USA.
}
{
Icons from Tango theme:
http://tango.freedesktop.org/Tango_Icon_Library
}

unit lhelpcore;

{$IFDEF LNET_VISUAL}
{$DEFINE USE_LNET} // you must manually add the lnetvisual.lpk package to the dependancy list
{$ELSE}
{$NOTE You can add http capability to lhelp by adding the lnetvisual package v0.6.3 or greater requirement to lhelp.}
{$ENDIF}

{$IFDEF UNIX}
  {$if FPC_FULLVERSION <= 20700}
    {$DEFINE STALE_PIPE_WORKAROUND}
  {$ENDIF}
{$ENDIF}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SimpleIPC, Laz2_XMLCfg,
  // LCL
  Forms, Controls, Dialogs, Buttons, ComCtrls, ExtCtrls, Menus, StdCtrls,
  LCLProc, LCLType, LCLIntf, DefaultTranslator,
  // LazUtils
  LazFileUtils, LazUTF8,
  {$IFDEF DEBUG}
  LazLogger,
  {$ELSE}
  LazLoggerDummy,
  {$ENDIF}
  // ChmHelp
  {$IFDEF USE_LNET}HTTPContentProvider,{$ENDIF}
  BaseContentProvider, ChmContentProvider, lhelpstrconsts;

type

  { TContentTab }

  TContentTab = class(TTabSheet)
  private
    FContentProvider: TBaseContentProvider;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ContentProvider: TBaseContentProvider read FContentProvider write FContentProvider;
  end;

  { THelpForm }
  
  THelpForm = class(TForm)
    ApplicationProperties1: TApplicationProperties;
    FileMenuCloseItem: TMenuItem;
    FileMenuExitItem: TMenuItem;
    FileMenuItem: TMenuItem;
    FileMenuOpenItem: TMenuItem;
    FileSeperater: TMenuItem;
    ImageList1: TImageList;
    ImageListToolbar: TImageList;
    MainMenu1: TMainMenu;
    FileMenuOpenURLItem: TMenuItem;
    HelpMenuItem: TMenuItem;
    AboutItem: TMenuItem;
    FileMenuOpenRecentItem: TMenuItem;
    ViewShowStatus: TMenuItem;
    ViewShowSepTabs: TMenuItem;
    PageControl: TPageControl;
    OpenDialog1: TOpenDialog;
    ToolBar1: TToolBar;
    HomeBttn: TToolButton;
    BackBttn: TToolButton;
    ForwardBttn: TToolButton;
    FileButton: TToolButton;
    ToolButton1: TToolButton;
    ViewMenuContents: TMenuItem;
    ViewMenuItem: TMenuItem;
    procedure AboutItemClick(Sender: TObject);
    procedure BackToolBtnClick(Sender: TObject);
    procedure FileMenuCloseItemClick(Sender: TObject);
    procedure FileMenuExitItemClick(Sender: TObject);
    procedure FileMenuOpenItemClick(Sender: TObject);
    procedure FileMenuOpenURLItemClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure ForwardToolBtnClick(Sender: TObject);
    procedure HomeToolBtnClick(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    procedure PageControlEnter(Sender: TObject);
    procedure ViewMenuContentsClick(Sender: TObject);
    procedure ViewMenuItemClick(Sender: TObject);
    procedure ViewShowSepTabsClick(Sender: TObject);
    procedure ViewShowStatusClick(Sender: TObject);
  private
    { private declarations }
    // SimpleIPC server name (including unique part as per help protocol)
    FServerName: String;
    // Receives commands from IDE
    FInputIPC: TSimpleIPCServer;
    // Sends responses back to IDE
    // only used if lhelp started with --ipcname to indicate
    // IPC communication method should be used
    FOutputIPC: TSimpleIPCClient;
    FInputIPCTimer: TTimer;
    FContext: LongInt; // used once when we are started on the command line with --context
    FConfig: TXMLConfig;
    FShowSepTabs: Boolean;
    FShowStatus: Boolean;
    FHasShowed: Boolean;
    FHide: boolean; //If yes, start with content hidden. Otherwise start normally
    FUpdateCount: Integer;
    // Keep track of whether size/position preferences were loaded and applied to form
    FLayoutApplied: boolean;
    // Applies layout (size/position/fullscreen) preferences once in lhelp lifetime
    // Needs LoadPreference to be run first to get fConfig object.
    procedure ApplyLayoutPreferencesOnce;
    // Load preferences. Preferences are unique for server-lhelp pairs and plain lhelp
    procedure LoadPreferences(AIPCName: String);
    // Saves preferences. Uses existing config loaded by LoadPreferences
    procedure SavePreferences;
    // Add filename to recent files (MRU) list
    procedure AddRecentFile(AFileName: String);
    procedure ContentTitleChange({%H-}sender: TObject);
    procedure OpenRecentItemClick(Sender: TObject);
    // Send response back to server (IDE)
    // Used to acknowledge commands from the server
    procedure SendResponse(Response: DWord);
    // Wait for message from server (IDE) and process
    procedure ServerMessage(Sender: TObject);
    // Parse any given command line options
    procedure ReadCommandLineOptions;
    // Start simple IPC server/client
    // ServerName can be the variable passed to --ipcname
    // It is used both for starting up the local server and to ID the remote server
    procedure StartComms(ServerName: String);
    // Stop simple IPC server/client
    procedure StopComms();
    // Open specified URL in viewer window
    function OpenURL(const AURL: String; AContext: THelpContext=-1): DWord;
    // Open specified URL - presumably used to queue URLs for delayed opening
    procedure LateOpenURL(Url: PStringItem);
    function ActivePage: TContentTab;
    // Update UI visibility
    procedure RefreshState;
    procedure ShowError(AError: String);
    // Set keyup handler for control (and any child controls)
    procedure SetKeyUp(AControl: TControl);
    // BeginUpdate tells each content provider to possibly stop some events
    procedure BeginUpdate;
    // EndUpdate tells each content provider to resume normal behavior
    procedure EndUpdate;
  public
    { public declarations }
  end;
  

var
  HelpForm: THelpForm;

const
  INVALID_FILE_TYPE = 1;
  VERSION_STAMP = '2017-08-13'; // used in displaying version in about form etc

implementation

{$R *.lfm}

uses 
  LHelpControl;

const
  DigitsInPID = 5; // Number of digits in the formatted PID according to the Help Protocol

type
  TRecentMenuItem = class(TMenuItem)
  public
    URL: String;
  end;

{ THelpForm }


procedure THelpForm.BackToolBtnClick(Sender: TObject);
begin
  if Assigned(ActivePage) then ActivePage.ContentProvider.GoBack;
end;

procedure THelpForm.AboutItemClick(Sender: TObject);
var
  f: TForm;
  l: TLabel;
  b: TButton;
begin
  DebugLn('about');
  f := TForm.Create(Application);
  try
    f.Caption := slhelp_About;
    f.BorderStyle := bsDialog;
    f.Position := poMainFormCenter;
    f.Constraints.MinWidth := 150;
    f.Constraints.MaxWidth := 350;
    l := TLabel.Create(f);
    l.Parent := f;;
    l.Align := alTop;
    l.BorderSpacing.Around := 6;
    l.Caption := Format(slhelp_LHelpCHMFileViewerVersionCopyrightCAndrewHainesLaz, [LineEnding, VERSION_STAMP, LineEnding +
      LineEnding, LineEnding]);
    l.AutoSize := True;
    //l.WordWrap := True; {don't wrap author's name}
    b := TButton.Create(f);
    b.Parent := f;
    b.BorderSpacing.Around := 6;
    b.Anchors := [akTop, akLeft];
    b.AnchorSide[akTop].Control := l;
    b.AnchorSide[akTop].Side := asrBottom;
    b.AnchorSide[akLeft].Control := f;
    b.AnchorSide[akLeft].Side := asrCenter;
    b.Caption := slhelp_Ok;
    b.ModalResult := mrOk;
    f.AutoSize := False;
    f.AutoSize := True;
    f.ShowModal;
  finally
    f.free;
  end;
end;


procedure THelpForm.FileMenuCloseItemClick(Sender: TObject);
begin
  if Assigned(ActivePage) then
  begin
    ViewMenuContentsClick(Self);
    ActivePage.Free;
    RefreshState;
  end;
end;

procedure THelpForm.FileMenuExitItemClick(Sender: TObject);
begin
  Close;
end;

procedure THelpForm.FileMenuOpenItemClick(Sender: TObject);
var
  TimerWasOn: boolean;
begin
  // Work around bug 25529: Slow dialog boxes for Windows Vista+ with
  // themes enabled
  // Stop listening to incoming server messages while busy showing dialog
  if Assigned(FInputIPCTimer) Then
  begin
    TimerWasOn := FInputIPCTimer.Enabled;
    FInputIPCTimer.Enabled := False;
  end;

  try
    if OpenDialog1.Execute then
    begin
      if OpenURL('file://'+OpenDialog1.FileName) = Ord(srSuccess) then
        AddRecentFile('file://'+OpenDialog1.FileName);
      RefreshState;
    end;
  finally
    if Assigned(FInputIPCTimer) Then
    begin
      FInputIPCTimer.Enabled := TimerWasOn;
    end;
  end;
end;

procedure THelpForm.FileMenuOpenURLItemClick(Sender: TObject);
var
  sRes: String;
  URLSAllowed: String;
  ProtocolNames: TStringList;
  i: Integer;
begin
  ProtocolNames := TStringList.Create();
  try
    ReadContentProviderList(ProtocolNames);
    URLSAllowed := '';
    for i := 0 to ProtocolNames.Count-1 do
    begin
      if i < 1 then
        URLSAllowed := URLSAllowed + ProtocolNames[i]
      else
        URLSAllowed := URLSAllowed + ', ' +ProtocolNames[i]
    end;
  finally
    ProtocolNames.Free();
  end;

  URLSAllowed := Trim(URLSAllowed);

  sRes:='';
  if InputQuery(slhelp_PleaseEnterAURL, Format(slhelp_SupportedURLTypeS, [URLSAllowed]), sRes) then
  begin
    if OpenURL(sRes) = Ord(srSuccess) then
      AddRecentFile(sRes);
    RefreshState();
  end;
end;

procedure THelpForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  //close all tabs to avoid AV with many tabs
  while Assigned(ActivePage) do
    ActivePage.Free;
  ////was before: close tab
  ////FileMenuCloseItemClick(Sender);

  Visible := False;
  Application.ProcessMessages;
  StopComms();
  SavePreferences();
end;

procedure THelpForm.FormCreate(Sender: TObject);
begin
  {$IFDEF DEBUG}
  DebugLogger.LogName := 'C:\work\laz-projects\lazhelp\lhelp\lhelp.log';
  DebugLogger.CloseLogFileBetweenWrites := True;
  {$ENDIF}

  FileMenuItem.Caption := slhelp_File;
  FileMenuOpenItem.Caption := slhelp_Open;
  FileMenuOpenRecentItem.Caption := slhelp_OpenRecent;
  FileMenuOpenURLItem.Caption := slhelp_OpenURL;
  FileMenuCloseItem.Caption := slhelp_Close;
  FileMenuExitItem.Caption := slhelp_EXit;
  ViewMenuItem.Caption := slhelp_View;
  ViewMenuContents.Caption := slhelp_ShowContents;
  ViewShowStatus.Caption := slhelp_OpenNewTabWithStatusBar;
  ViewShowSepTabs.Caption := slhelp_OpenNewFileInSeparateTab;
  HelpMenuItem.Caption := slhelp_Help;
  AboutItem.Caption := slhelp_About2;

  OpenDialog1.Title := slhelp_OpenExistingFile;
  OpenDialog1.Filter := slhelp_HelpFilesChmChmAllFiles;

  FContext := -1;

  // Safe default:
  FHide := false;
  // ReadCommandLineOptions will set FHide if requested
  ReadCommandLineOptions;
  LoadPreferences(FServerName);
  // Only start IPC if server name passed in --ipcname
  if FServerName <> '' then
  begin
    StartComms(FServerName);
  end;
  // If user wants lhelp to hide, hide entire form.
  if FHide then
    WindowState := wsMinimized
  else
    RefreshState;
  SetKeyUp(Self);
end;

procedure THelpForm.FormDestroy(Sender: TObject);
begin
  StopComms();
end;

procedure THelpForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    Close;
  // Backspace: go to previous page (as if BackBttn were clicked)
  if Key = VK_BACK then
    if Assigned(ActivePage) then ActivePage.ContentProvider.GoBack;
end;

procedure THelpForm.FormShow(Sender: TObject);
begin
  if FHasShowed then
    Exit;
  FHasShowed := True;
end;

procedure THelpForm.ForwardToolBtnClick(Sender: TObject);
begin
  if Assigned(ActivePage) then ActivePage.ContentProvider.GoForward;
end;

procedure THelpForm.HomeToolBtnClick(Sender: TObject);
begin
  if Assigned(ActivePage) then ActivePage.ContentProvider.GoHome;
end;

procedure THelpForm.PageControlChange(Sender: TObject);
begin
  RefreshState;
end;

procedure THelpForm.PageControlEnter(Sender: TObject);
begin
  RefreshState;
end;

procedure THelpForm.ApplyLayoutPreferencesOnce;
begin
  if not(assigned(FConfig)) then exit;
  if (not(FHide)) and
    (not(FLayoutApplied)) then
  begin
    if (FConfig.GetValue('Position/Maximized', false)=true) then
    begin
      Windowstate:=wsMaximized
    end
    else
    begin
      Left   := FConfig.GetValue('Position/Left/Value', Left);
      Top    := FConfig.GetValue('Position/Top/Value', Top);
      Width  := FConfig.GetValue('Position/Width/Value', Width);
      Height := FConfig.GetValue('Position/Height/Value', Height);
    end;
    // Keep track so we do not reapply initial settings as user may have
    // changed size etc in the meantime.
    FLayoutApplied := true;
  end;
end;

procedure THelpForm.ViewMenuContentsClick(Sender: TObject);
begin
  // TabsControl property in TChmContentProvider
  if Assigned(ActivePage) then
  begin
    with TChmContentProvider(ActivePage.ContentProvider) do
    begin
      NavigationVisible := not NavigationVisible;
    end;
  end;
end;

procedure THelpForm.ViewMenuItemClick(Sender: TObject);
begin
  ViewMenuContents.Checked :=
    Assigned(ActivePage) and
    (ActivePage.ContentProvider is TChmContentProvider) and
    (ActivePage.ContentProvider as TChmContentProvider).NavigationVisible;
  ViewShowSepTabs.Checked := FShowSepTabs;
  ViewShowStatus.Checked := FShowStatus;
end;

procedure THelpForm.ViewShowSepTabsClick(Sender: TObject);
begin
  FShowSepTabs := not FShowSepTabs;
end;

procedure THelpForm.ViewShowStatusClick(Sender: TObject);
begin
  FShowStatus := not FShowStatus;
end;

procedure THelpForm.LoadPreferences(AIPCName: String);
var
  PrefFile: String;
  RecentCount: Integer;
  ServerPart: String;
  i: Integer;
begin
  PrefFile := GetAppConfigDirUTF8(False);
  ForceDirectoriesUTF8(PrefFile);
  // --ipcname passes a server ID that consists of a
  // server-dependent constant together with a process ID.
  // Strip out the formatted process ID to get fixed config file names for
  // one server
  ServerPart := Copy(AIPCName, 1, length(AIPCName)-DigitsInPID);
  PrefFile := Format('%slhelp-%s.conf',[IncludeTrailingPathDelimiter(PrefFile), ServerPart]);

  FConfig := TXMLConfig.Create(Self);
  FConfig.Filename := PrefFile;

  // Restore window but only if currently not being asked to hide
  ApplyLayoutPreferencesOnce;
  OpenDialog1.FileName := FConfig.GetValue('LastFileOpen/Value', OpenDialog1.FileName);

  RecentCount:= FConfig.GetValue('Recent/ItemCount/Value', 0);
  // downto since oldest are knocked off the list:
  for i := RecentCount-1 downto 0 do
    AddRecentFile(FConfig.GetValue('Recent/Item'+IntToStr(i)+'/Value',''));

  FShowSepTabs := FConfig.GetValue('OpenSepTabs/Value', False);
  FShowStatus := FConfig.GetValue('OpenWithStatus/Value', True);
end;

procedure THelpForm.SavePreferences;
var
  i: Integer;
begin
  if not(assigned(FConfig)) then
    exit; //silently abort
  if not (WindowState = wsMaximized) then
  begin
    FConfig.SetValue('Position/Maximized', false);
    FConfig.SetValue('Position/Left/Value', Left);
    FConfig.SetValue('Position/Top/Value', Top);
    FConfig.SetValue('Position/Width/Value', Width);
    FConfig.SetValue('Position/Height/Value', Height);
  end
  else
  begin
    FConfig.SetValue('Position/Maximized', true);
  end;

  FConfig.SetValue('LastFileOpen/Value', OpenDialog1.FileName);

  FConfig.SetValue('Recent/ItemCount/Value', FileMenuOpenRecentItem.Count);
  // downto since oldest are knocked off the list:
  for i := 0 to FileMenuOpenRecentItem.Count-1 do
    FConfig.SetValue('Recent/Item'+IntToStr(i)+'/Value', TRecentMenuItem(FileMenuOpenRecentItem.Items[I]).URL);

  FConfig.SetValue('OpenSepTabs/Value', FShowSepTabs);
  FConfig.SetValue('OpenWithStatus/Value', FShowStatus);

  FConfig.Flush;
  FConfig.Free;
end;

procedure THelpForm.AddRecentFile(AFileName: String);
var
  Item : TRecentMenuItem;
  MaxHistory: longint;
  i: Integer;
begin
  for i := FileMenuOpenRecentItem.Count-1 downto 0 do
    if TRecentMenuItem(FileMenuOpenRecentItem.Items[i]).URL = AFileName then
    begin
      FileMenuOpenRecentItem.Delete(i);
    end;
  Item := TRecentMenuItem.Create(FileMenuOpenRecentItem);
  Item.Caption:=ExtractFileNameOnly(AFileName);
  Item.URL:=AFileName;
  Item.OnClick:=@OpenRecentItemClick;
  Item.Hint:=Item.URL;
  FileMenuOpenRecentItem.Insert(0, Item);

  MaxHistory := FConfig.GetValue('Recent/HistoryCount/Value', 5);

  if FileMenuOpenRecentItem.Count > 0 then
    FileMenuOpenRecentItem.Enabled:=True;

  if FileMenuOpenRecentItem.Count > MaxHistory then
    FileMenuOpenRecentItem.Items[MaxHistory-1].Free;
end;

procedure THelpForm.ContentTitleChange(sender: TObject);
begin
  if ActivePage = nil then
    Exit;
  Caption := Format(slhelp_LHelp2, [ActivePage.FContentProvider.Title]);
end;

procedure THelpForm.OpenRecentItemClick(Sender: TObject);
var
  Item: TRecentMenuItem absolute Sender;
begin
  OpenURL(Item.URL);
  AddRecentFile(Item.URL);
end;

procedure THelpForm.SendResponse(Response: DWord);
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    Stream.WriteDWord(Response);
    if Assigned(FOutputIPC) then
    begin
      // reset output IPC, to avoid strange behavior, that other side not receives messages
      // tested on win32
      FOutputIPC.Active := False;
      FOutputIPC.Active := True;
      FOutputIPC.SendMessage(mtUnknown, Stream);
    end;
  finally
    Stream.Free();
  end;
end;

procedure THelpForm.ServerMessage(Sender: TObject);
var
  UrlReq: TUrlRequest;
  FileReq: TFileRequest;
  ConReq: TContextRequest;
  MiscReq: TMiscRequest;
  MustClose: Boolean = False;
  MustShow: Boolean;
  Stream: TStream;
  Res: LongWord;
  Url: String='';
begin
  while FInputIPC.PeekMessage(0, True) do
  begin
    Stream := FInputIPC.MsgData;
    Stream.Position := 0;
    FillByte(FileReq{%H-}, SizeOf(FileReq), 0);
    Stream.Read(FileReq, SizeOf(FileReq));
    Res := Ord(srError); //fail by default
    MustShow := False;
    case FileReq.RequestType of
      rtFile:
      begin
        DebugLn('got rtFile, filename=' + FileReq.FileName);
        Url := 'file://' + FileReq.FileName;
        Res := OpenURL(URL);
        Res := Ord(srSuccess);
      end;
      rtUrl:
      begin
        Res := Ord(srSuccess);
        Stream.Position := 0;
        FillByte(UrlReq{%H-}, SizeOf(UrlReq), 0);
        Stream.Read(UrlReq, SizeOf(UrlReq));
        DebugLn('got rtUrl, filename='+UrlReq.FileRequest.FileName+', url='+UrlReq.Url);
        if UrlReq.FileRequest.FileName <> '' then
        begin
          Url := 'file://' + UrlReq.FileRequest.FileName;
          Res := OpenUrl(URL+'://'+UrlReq.Url)
        end
        else
        begin
          Url := UrlReq.Url;
          Res := OpenURL(Url);
        end;
      end;
      rtContext:
      begin
        Res := Ord(srSuccess);
        Stream.Position := 0;
        FillByte(ConReq{%H-},SizeOf(ConReq),0);
        Stream.Read(ConReq, SizeOf(ConReq));
        DebugLn('got rtContext, filename='+FileReq.FileName+', context='+IntToStr(ConReq.HelpContext));
        Url := 'file://'+FileReq.FileName;
        Res := OpenURL(Url, ConReq.HelpContext);
      end;
      rtMisc:
      begin
        Stream.Position := 0;
        FillByte(MiscReq{%H-}, SizeOf(MiscReq), 0);
        Stream.Read(MiscReq, SizeOf(MiscReq));
        case MiscReq.RequestID of
          mrClose:
          begin
            DebugLn('got rtMisc/mrClose');
            Res := Ord(srSuccess);
            MustClose := True;
          end;
          mrShow:
          begin
            DebugLn('got rtMisc/mrShow');
            Res := Ord(srSuccess);
            FHide := False;
            MustShow := True;
            if WindowState = wsMinimized then
              WindowState := wsNormal;
            RefreshState();
          end;
          mrVersion:
          begin
            DebugLn('got rtMisc/mrVersion');
            // Protocol version encoded in the filename
            // Verify what we support
            if StrToIntDef(FileReq.FileName, 0) = StrToIntDef(PROTOCOL_VERSION, 0) then
              Res := Ord(srSuccess)
            else
              Res := Ord(srError); //version not supported
          end;
          mrBeginUpdate:
          begin
            DebugLn('got rtMisc/mrBeginUpdate');
            Res := Ord(srSuccess);
            BeginUpdate();
          end;
          mrEndUpdate:
          begin
            DebugLn('got rtMisc/mrEndUpdate');
            Res := Ord(srSuccess);
            EndUpdate();
          end
          else {Unknown request}
          begin
            DebugLn('got rtMisc/Unknown');
            Res := Ord(srUnknown);
          end;
        end;
      end; //rtMisc
    end;

    // This may take some time which may allow receiving end to get ready for
    // receiving messages
    if (URL <> '') and (Res = Ord(srSuccess)) then
      AddRecentFile(Url);
    // Receiving end may not yet be ready (observed with an Intel Core i7),
    // so perhaps wait a bit?
    // Unfortunately, the delay time is guesswork=>Sleep(80)?
    SendResponse(Res); //send response again in case first wasn't picked up
    // Keep after SendResponse to avoid timing issues (e.g. writing to log file):
    DebugLn('sent TLHelpResponse code: '+IntToStr(Res));

    if MustClose then
    begin
      Application.ProcessMessages;
      Sleep(10);
      Application.Terminate;
    end;

    // We received mrShow:
    if MustShow and (MustClose = False) and (FHide = False) then
    begin
      Self.SendToBack();
      Self.BringToFront();
      Self.ShowOnTop();
      // If lhelp was run with hidden parameter, we need to apply
      // layout preferences once:
      ApplyLayoutPreferencesOnce();
    end;
  end;
end;

procedure THelpForm.ReadCommandLineOptions;
var
  X: Integer;
  IsHandled: array[0..50] of boolean;
  URL: String;
  StrItem: PStringItem;
  Filename: String;
begin
  FillChar(IsHandled{%H-}, 51, 0);
  X:=1;
  while X<=ParamCount do
  begin
    if LowerCase(ParamStrUTF8(X)) = '--ipcname' then
    begin
      // IPC name; includes unique PID or other identifier
      IsHandled[X] := True;
      inc(X);
      if X <= ParamCount then
      begin
        FServerName := ParamStrUTF8(X);
        IsHandled[X] := True;
        inc(X);
      end;
    end
    else if LowerCase(ParamStrUTF8(X)) = '--context' then
    begin
      IsHandled[X] := True;
      inc(X);
      if (X <= ParamCount) then
        if TryStrToInt(ParamStrUTF8(X), FContext) then
        begin
          IsHandled[X] := True;
          inc(X);
        end;
    end
    else if LowerCase(ParamStrUTF8(X)) = '--hide' then
    begin
      IsHandled[X] := True;
      inc(X);
      FHide:=true;
    end
    else
    begin
      IsHandled[X]:=copy(ParamStrUTF8(X),1,1)='-'; // ignore other parameters
      inc(X);
    end;
  end;

  // Loop through a second time for the URL
  for X := 1 to ParamCount do
    if not IsHandled[X] then
    begin
      //DoOpenChm(ParamStrUTF8(X));
      URL:=ParamStrUTF8(X);
      if Pos('://', URL) = 0 then
        URL := 'file://'+URL;
      Filename:=URL;
      if copy(Filename,1,length('file://'))='file://' then
      begin
        System.Delete(Filename,1,length('file://'));
        Filename:=SetDirSeparators(Filename);
        if not FileExistsUTF8(Filename) then
        begin
          DebugLn('THelpForm.ReadCommandLineOptions file not found "' + Filename + '"');
          continue;
        end;
      end;
      StrItem := New(PStringItem);
      StrItem^.FString := URL;
      Application.QueueAsyncCall(TDataEvent(@LateOpenURL), {%H-}PtrUInt(StrItem));
      Break;
    end;
end;

procedure THelpForm.StartComms(ServerName: String);
// Starts IPC server and client for two-way communication with
// controlling program (e.g. Lazarus IDE).

// Only useful if IPC serverID is passed through the --ipcname
// command.
begin
  FInputIPC := TSimpleIPCServer.Create(nil);
  FInputIPC.ServerID := ServerName;
  FInputIPC.Global := True;
  FInputIPC.Active := True;

  // Use timer to check for incoming messages from the IDE
  FInputIPCTimer := TTimer.Create(nil);
  FInputIPCTimer.OnTimer := @ServerMessage;
  FInputIPCTimer.Interval := 100; // milliseconds
  FInputIPCTimer.Enabled := True;
  ServerMessage(nil);

  FOutputIPC := TSimpleIPCClient.Create(nil);
  FOutputIPC.ServerID := ServerName+'client';
  try
    if FOutputIPC.ServerRunning then
      FOutputIPC.Active := True;
  except
    FOutputIPC.Active := False;
  end;
end;

procedure THelpForm.StopComms;
begin
  if Assigned(FInputIPC) then
  begin
    try
      if FInputIPC.Active then
        FInputIPC.Active := False;
    finally
      FreeAndNil(FInputIPC);
      FreeAndNil(FInputIPCTimer);
    end;
  end;

  if Assigned(FOutputIPC) then
  begin
    try
      if FOutputIPC.Active then
        FOutputIPC.Active := False;
    finally
      FreeAndNil(FOutputIPC);
    end;
  end;
end;

function THelpForm.OpenURL(const AURL: String; AContext: THelpContext): DWord;
  function GetURLPrefix: String;
  var
    iPos: Integer;
  begin
    iPos := Pos('://', AURL);
    Result := Copy(AURL, 1, iPos+2);
  end;

var
  sURLPrefix: String;
  TmpContentProvider: TBaseContentProviderClass;
  RealContentProviderClass: TBaseContentProviderClass;
  ContentTab: TContentTab = nil;
  i: Integer;
begin
  Result := Ord(srInvalidURL);
  sURLPrefix := GetURLPrefix;
  TmpContentProvider := GetContentProviderClass(sURLPrefix);
 
  if TmpContentProvider = nil then
  begin
    ShowError(Format(slhelp_CannotHandleThisTypeOfContentForUrl, [sURLPrefix, LineEnding, AURL]));
    Result := Ord(srInvalidURL);
    Exit;
  end;
  RealContentProviderClass := TmpContentProvider.GetProperContentProviderClass(AURL);
 
  if RealContentProviderClass = nil then
  begin
   ShowError(Format(slhelp_CannotHandleThisTypeOfSubcontentForUrl, [sURLPrefix, LineEnding, AURL]));
   Result := Ord(srInvalidURL);
   Exit;
  end;

  if not FShowSepTabs then
  for i := 0 to PageControl.PageCount-1 do
  begin
    ContentTab := TContentTab(PageControl.Pages[i]);
    if RealContentProviderClass.ClassName = ContentTab.ContentProvider.ClassName then
    begin
      if ContentTab.ContentProvider.LoadURL(AURL, AContext) then
      begin
        PageControl.ActivePage := PageControl.Pages[i];
        Result := Ord(srSuccess);
      end
      else
        Result := Ord(srInvalidFile);
      Exit;
    end;
  end;

  if ContentTab = nil then
  begin
    // no existing page that can handle this content, so create one
    ContentTab := TContentTab.Create(PageControl);
    ContentTab.ContentProvider := RealContentProviderClass.Create(ContentTab, ImageList1);
    ContentTab.ContentProvider.OnTitleChange := @ContentTitleChange;
    ContentTab.Parent := PageControl;
    SetKeyUp(ContentTab);
    ContentTab.ContentProvider.LoadPreferences(FConfig);
    if ContentTab.ContentProvider is TChmContentProvider then
     (ContentTab.ContentProvider as TChmContentProvider).ShowStatusbar := FShowStatus;
  end;

  if FUpdateCount > 0 then
   ContentTab.ContentProvider.BeginUpdate;

  if ContentTab.ContentProvider.LoadURL(AURL, AContext) then
  begin
   PageControl.ActivePage := ContentTab;
   RefreshState;
   Result := Ord(srSuccess);
  end
  else
   Result := Ord(srInvalidURL);

  if not FHide then
   ShowOnTop;
end;


procedure THelpForm.LateOpenURL(Url: PStringItem) ;
begin
  if OpenURL(URL^.FString, FContext) = Ord(srSuccess) then
    AddRecentFile(URL^.FString);
  // we reset the context because at this point the file has been loaded and the
  // context shown
  FContext := -1;

  Dispose(Url);
  RefreshState;
end;

function THelpForm.ActivePage: TContentTab;
begin
  Result := TContentTab(PageControl.ActivePage);
end;

procedure THelpForm.RefreshState;
var
  en: Boolean;
begin
  if FHide then
  begin
    en := false;
    // Hide content page
    if Assigned(ActivePage) then
    begin
      with TChmContentProvider(ActivePage.ContentProvider) do
      begin
        ActivePage.Visible := false;
        Visible := false;
        NavigationVisible := False;
      end;
    end;
  end
  else
  begin
    en := Assigned(ActivePage);
    // Show content page
    if en then
    begin
      with TChmContentProvider(ActivePage.ContentProvider) do
      begin
        ActivePage.Visible := true;
        Visible := true;
        NavigationVisible := True;
      end;
    end;
  end;

  BackBttn.Enabled := en;
  ForwardBttn.Enabled := en;
  HomeBttn.Enabled := en;
  FileMenuCloseItem.Enabled := en;
  ViewMenuContents.Enabled := en;

  if en and not (csDestroying in ActivePage.ComponentState) then
    Caption := Format(slhelp_LHelp2, [ActivePage.FContentProvider.Title])
  else
    Caption := slhelp_LHelp;
end;

procedure THelpForm.ShowError(AError: String);
begin
  ShowMessage(AError);
end;

procedure THelpForm.SetKeyUp(AControl: TControl);
var
  WCont: TWinControl absolute AControl;
  i: Integer;
begin
  if (AControl = nil) or not (AControl.InheritsFrom(TWinControl)) then
    Exit;
  for i := 0 to WCont.ControlCount-1 do
    SetKeyUp(WCont.Controls[i]);
  WCont.OnKeyUp:=@FormKeyUp;
end;

procedure THelpForm.BeginUpdate;
var
  Tab: TContentTab;
  i: Integer;
begin
  Inc(FUpdateCount);
  if FUpdateCount = 1 then
  begin
    for i := 0 to PageControl.PageCount-1 do
    begin
      Tab := TContentTab(PageControl.Pages[I]);
      Tab.ContentProvider.BeginUpdate();
    end;
  end;

end;

procedure THelpForm.EndUpdate;
var
  Tab: TContentTab;
  i: Integer;
begin
  Dec(FUpdateCount);
  if FUpdateCount < 0 then
   FUpdateCount:=0;

  if FUpdateCount = 0 then
  begin
    for i := 0 to PageControl.PageCount-1 do
    begin
      Tab := TContentTab(PageControl.Pages[I]);
      Tab.ContentProvider.EndUpdate;
    end;
  end;
end;

{ TContentTab }

constructor TContentTab.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TContentTab.Destroy;
begin
  FContentProvider.Free;
  inherited Destroy;
end;

end.

