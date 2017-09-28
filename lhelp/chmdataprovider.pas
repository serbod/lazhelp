{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Copyright (C) <2005> <Andrew Haines> chmdataprovider.pas

}
unit ChmDataProvider;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IpHtml, iputils, IpMsg, Graphics, chmreader,
  LCLType, Controls,
  FPImage,
  {$IF FPC_FULLVERSION>=20602} //fpreadgif exists since at least this version
  FPReadgif,
  {$ENDIF}
  FPReadbmp,
  FPReadxpm,
  FPReadJPEG,
  FPReadpng,
  FPWritebmp,
  FPWritePNG,
  IntFGraphics,
  lhelpstrconsts,
  LConvEncoding,
  lcid_conv;


type

  THelpPopupEvent = procedure(HelpFile: String; URL: String);
  THtmlPageLoadStreamEvent = procedure (var AStream: TStream) of object;

  { TIpChmDataProvider }

  TIpChmDataProvider = class(TIpAbstractHtmlDataProvider)
  private
    FChmFileList: TChmFileList;
    FCurrentPage: String;
    FCurrentPath: String;
    FOnGetHtmlPage: THtmlPageLoadStreamEvent;
    FOnHelpPopup: THelpPopupEvent;
    function StripInPageLink(AURL: String): String;
    function DetectHtmlCodepage(AStr: string): Word;
  protected
    function DoGetHtmlStream(const URL: string;
      {%H-}PostData: TIpFormDataEntity) : TStream; override;
    function DoCheckURL(const URL: string;
      var ContentType: string): Boolean; override;
    procedure DoLeave({%H-}Html: TIpHtml); override;
    procedure DoReference(const {%H-}URL: string); override;
    procedure DoGetImage(Sender: TIpHtmlNode; const URL: string;
      var Picture: TPicture); override;
    { Return True if given URL exists in CHM and can be readed }
    function CanHandle(const AUrl: string): Boolean; override;
    function BuildURL(const OldURL, NewURL: string): string; override;
    { Use ReadDirsParents }
    function GetDirsParents(ADir: String): TStringList; deprecated;
    { Split directory path into list of directories names
      /home/user/dir -> 'home', 'user', 'dir' }
    function ReadDirsParents(const ADir: String; AStrings: TStrings): Boolean;
    { Result must be Free() by caller! }
    function DoGetStream(const URL: string): TStream; override;
  public
    constructor Create(AOwner: TComponent; AChmFileList: TChmFileList); reintroduce;
    destructor Destroy; override;
    property ChmFileList: TChmFileList read FChmFileList write FChmFileList;
    property OnHelpPopup: THelpPopupEvent read FOnHelpPopup write FOnHelpPopup;
    property CurrentPage: String read FCurrentPage;
    property CurrentPath: String read FCurrentPath write FCurrentPath;
    property OnGetHtmlPage: THtmlPageLoadStreamEvent read FOnGetHtmlPage write FOnGetHtmlPage;

  end;

implementation

{ TIpChmDataProvider }

function TIpChmDataProvider.StripInPageLink ( AURL: String ) : String;
var
  i: LongInt;
begin
  Result := AURL;
  i := Pos('#', Result);
  if i > 0 then
    Result := Copy(Result, 1, i-1);
end;

function TIpChmDataProvider.DetectHtmlCodepage(AStr: string): Word;
var
  s, sRes: string;
  n, i: Integer;
  IsRec: Boolean;
begin
  Result := 0;
  AStr := LowerCase(AStr);

  while Length(AStr) > 0 do
  begin
    s := AStr;
    n := Pos('<meta', AStr);
    if n > 0 then
    begin
      AStr := Copy(AStr, n + 5, MaxInt);
      n := Pos('>', AStr);
      if n > 0 then
      begin
        s := Copy(AStr, 1, n);
        AStr := Copy(AStr, n+1, MaxInt);

        n := Pos('charset', s);
        if n > 0 then
        begin
          s := Copy(s, n+7, MaxInt);
          IsRec := False;
          sRes := '';
          for i := 1 to Length(s) do
          begin
            case s[i] of
              '=': IsRec := True;
              '"', '>', ';': IsRec := False;
              '0'..'9': if IsRec then sRes := sRes + s[i];
            end;
          end;
          Result := StrTointDef(sRes, 0);
          Exit;
        end;
      end
      else
        AStr := '';
    end
    else
      AStr := '';
  end;
end;

function TIpChmDataProvider.DoGetHtmlStream(const URL: string;
  PostData: TIpFormDataEntity): TStream;
var
 Tmp, sHead: string;
 LCID: Word;
 wcp: Word;
begin
  Result := TMemoryStream.Create();
  // If for some reason we were not able to get the page return something so that
  // we don't cause an AV
  if not FChmFileList.ReadFileContent(StripInPageLink(URL), Result) then
  begin
    Result.Size := 0;
    Tmp := '<HTML>' + slhelp_PageCannotBeFound + '</HTML>';
    Result.Write(PChar(Tmp)^, Length(tmp));
  end
  else
  begin
    // convert encoding
    LCID := 0;
    if Assigned(FChmFileList.LastChm) then
      LCID := FChmFileList.LastChm.LocaleID;

    if (LCID <> 0) and (LCID <> 1033) then
    begin
      // read stream to string
      Result.Position := 0;
      SetLength(Tmp, Result.Size);
      Result.Read(PChar(Tmp)^, Length(Tmp));

      // detect codepage from HTML header
      sHead := Copy(Tmp, 1, 1024);
      wcp := DetectHtmlCodepage(sHead);
      if wcp <> 0 then
      begin
        // convert from HTML codepage
        // 8 = utf-8 or very old encoding
        if wcp <> 8 then
          Tmp := ConvToUTF8FromCP(wcp, Tmp);
      end
      else
      begin
        // convert from locale codepage
        Tmp := ConvToUTF8FromLCID(LCID, Tmp);
      end;
      // write string to stream
      Result.Size := 0;
      Result.Write(PChar(Tmp)^, Length(Tmp));
      Result.Position := 0;
    end;
  end;
  if Assigned(FOnGetHtmlPage) then
    FOnGetHtmlPage(Result);
end;

function TIpChmDataProvider.DoCheckURL(const URL: string;
  var ContentType: string): Boolean;
var
  Reader: TChmReader = nil;
begin
  //DebugLn('RequestedUrl: ',URL);
  Result := FChmFileList.ObjectExists(StripInPageLink(Url), Reader) > 0;
  if Result then begin
    ContentType := 'text/html';
    FCurrentPath := ExtractFilePath(Url);
    Result := True;
    FCurrentPage := URL;
  end;
end;

procedure TIpChmDataProvider.DoLeave(Html: TIpHtml);
begin
  //
//  //DebugLn('Left: ');
end;

procedure TIpChmDataProvider.DoReference(const URL: string);
begin
  //
  ////DebugLn('Reference=',URL);
end;

procedure TIpChmDataProvider.DoGetImage(Sender: TIpHtmlNode; const URL: string;
  var Picture: TPicture);
var
  Stream: TMemoryStream;
  FileExt: String;
begin
  //DebugLn('Getting Image ',(Url));
  Picture := nil;

  FileExt := ExtractFileExt(URL);

  Picture := TPicture.Create;
  Stream := TMemoryStream.Create();
  try
    if FChmFileList.ReadFileContent('/'+URL, Stream) then
    begin
      Stream.Position := 0;
      try
        Picture.LoadFromStreamWithFileExt(Stream, FileExt);
      except
        // only happens if it's an image type we can't handle
      end;
    end;
  finally
    Stream.Free();
  end;
end;

function TIpChmDataProvider.CanHandle(const AUrl: string): Boolean;
var
  HelpFile: String;
  ChmReader: TChmReader = nil;
begin
  Result := True;
  if Pos('Java', AUrl) = 1  then
    Result := False;

  if  (FChmFileList.ObjectExists(StripInPageLink(AUrl), ChmReader)= 0)
  and (FChmFileList.ObjectExists(StripInPageLink(BuildUrl(FCurrentPath, AUrl)), ChmReader) = 0)
  then
    Result := False;
  //DebugLn('CanHandle ',AUrl,' = ', Result);
  //if not Result then if FChmFileList.ObjectExists(BuildURL('', AUrl)) > 0 Then result := true;

  if (not Result) and (Pos('#', AUrl) = 1) then
    Result := True;
end;

function TIpChmDataProvider.BuildURL(const OldURL, NewURL: string): string;
var
  X: LongInt;
  sNewURL: String;
  ParentDirs: TStringList;
  RemoveDirCount: Integer;
begin
  Result := NewURL;

  sNewURL := NewURL;
  if OldURL = '' then
    exit;

  if Pos('ms-its:', NewURL) = 1 then
  begin
    if Pos('#', NewURL) = 0 then
      exit;
    X := Pos('::', NewURL);
    if NewURL[X+2] = '/' then    // NewURL is complete and absolute --> nothing to do
      exit;
    sNewURL := Copy(sNewURL, X+3, MaxInt);
  end;

  ParentDirs := TStringList.Create();
  try
    ReadDirsParents(OldURL, ParentDirs);
    RemoveDirCount := 0;
    repeat
      X := Pos('../', sNewURL);
      if X > 0 then
      begin
        Delete(sNewURL, X, 3);
        Inc(RemoveDirCount);
      end;
    until X = 0;

    repeat
      X := Pos('./', sNewURL);
      if X > 0 then
        Delete(sNewURL, X, 2);
    until X = 0;

    Result := '';
    for X := 0 to ParentDirs.Count-RemoveDirCount-1 do
      Result := Result + ParentDirs[X] + '/';

    Result := Result+sNewURL;

    repeat
      X := Pos('//', Result);
      if X > 0 then
        Delete(Result, X, 1);
    until X = 0;
  finally
    ParentDirs.Free();
  end;
  //WriteLn('res = ', Result);
end;

function TIpChmDataProvider.GetDirsParents(ADir: String): TStringList;
begin
  Result := TStringList.Create;
  ReadDirsParents(ADir, Result);
end;

function TIpChmDataProvider.ReadDirsParents(const ADir: String; AStrings: TStrings): Boolean;
var
  LastName: String;
begin
  Result := True;
  AStrings.Delimiter := '/';
  AStrings.StrictDelimiter := True;
  AStrings.DelimitedText := ADir;

  LastName := ExtractFileName(ADir);
  if LastName <> '' then
    AStrings.Delete(AStrings.Count-1);
  if AStrings[AStrings.Count-1] = '' then
    AStrings.Delete(AStrings.Count-1);
end;

function TIpChmDataProvider.DoGetStream(const URL: string): TStream;
var
 NewURL: String;
begin
  Result := nil;
  if Length(URL) = 0 then
    Exit;
  if not (URL[1] in ['/']) then
    NewURL := BuildUrl(FCurrentPath,URL)
  else
    NewURL := URL;

  Result := FChmFileList.GetObject(NewURL);
end;

constructor TIpChmDataProvider.Create(AOwner: TComponent; AChmFileList: TChmFileList);
begin
  inherited Create(AOwner);
  FChmFileList := AChmFileList;
end;

destructor TIpChmDataProvider.Destroy;
begin
  inherited Destroy;
end;

end.

