{ Copyright (C) <2005> <Andrew Haines> chmfilewriter.pas

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
}
{
  See the file COPYING.FPC, included in this distribution,
  for details about the copyright.
}
unit chmfilewriter;

{$mode objfpc}{$H+}

interface

uses
  Strings, Classes, SysUtils, chmwriter, inifiles, contnrs, chmsitemap, avl_tree,
  {for html scanning } dom, SAX_HTML, dom_html;

type
  TChmProject = class;
  TChmProjectErrorKind = (chmError, chmWarning, chmHint, chmNote, chmNone);

  TChmProgressCB = procedure(Project: TChmProject; CurrentFile: string) of object;
  TChmErrorCB = procedure(Project: TChmProject; ErrorKind: TChmProjectErrorKind;
    Msg: string; DetailLevel: Integer = 0);

  { TChmProject }

  TChmProject = class
  private
    FAutoFollowLinks: Boolean;
    FDefaultFont: string;
    FDefaultPage: string;
    FFiles: TStrings;
    FIndexFileName: string;
    FMakeBinaryTOC: Boolean;
    FMakeBinaryIndex: Boolean;
    FMakeSearchable: Boolean;
    FFileName: string;
    FOnProgress: TChmProgressCB;
    FOnError: TChmErrorCB;
    FOutputFileName: string;
    FTableOfContentsFileName: string;
    FTitle: string;
    FWindows: TObjectList;
    FMergeFiles: TStringList;
    FDefaultWindow: string;
    FScanHtmlContents: Boolean;
    FOtherFiles: TStrings; // Files found in a scan.
    FAllowedExtensions: TStringList;
    FTotalFileList: TStringIndexList;
    FAnchorList: TStringList;
    FSpareString: TStringIndex;
    FBasePath: string;
    // location of the .hhp file. Needed to resolve relative paths
    FReadmeMessage: string;     // readme message
    FToc, FIndex: TCHMSiteMap;
    FTocStream, FIndexStream: TMemoryStream;
    FCores: Integer;
    FLocaleID: word;
  protected
    { TChmWriter.OnGetFileData handler }
    function WriterGetFileData(const ADataName: string; out APathInChm: string;
      out AFileName: string; AStream: TStream): Boolean;
    { TChmWriter.OnLastFile handler }
    procedure WriterLastFileAdded(Sender: TObject);
    procedure ReadIniOptions(KeyValuePairs: TStringList);
    procedure ScanList(ToScan, NewFiles: TStrings; Recursion: Boolean);
    procedure ScanSitemap(SiteMap: TChmSiteMap; NewFiles: TStrings; Recursion: Boolean);
    function FileInTotalList(const s: string): Boolean;
    function SanitizeURL(const BasePath, InString, LocalPath, LocalName: string;
      var OutString: string): Boolean;
  public
    constructor Create(); virtual;
    destructor Destroy(); override;
    { Load settings from XML config file }
    procedure LoadFromFile(AFileName: string); virtual;
    { Load settings from HHP config file }
    procedure LoadFromHHP(AFileName: string; LeaveInclude: Boolean); virtual;
    { Save settings to XML config file }
    procedure SaveToFile(AFileName: string); virtual;
    { Compile CHM file and write to AOutStream }
    procedure WriteChm(AOutStream: TStream); virtual;
    { Show undefined anchors list to Error log }
    procedure ShowUndefinedAnchors();
    { Path to FileName }
    function ProjectDir(): string;
    { Load TOC and Index files }
    procedure LoadSiteMaps();
    { Add FileName to Files list, assign ContextId and ContextName
      with that FileName }
    procedure AddFileWithContext(AContextId: Integer; AFileName: AnsiString;
      AContextName: AnsiString = '');
    { Scan *.html from Files list and add referenced files (images, css, etc..)
      to OtherFiles list }
    procedure ScanHtml();
    { Call OnError, send error, info or debug message }
    procedure Error(ErrorKind: TChmProjectErrorKind; Msg: string; DetailLevel: Integer = 0);
    // though stored in the project file, it is only there for the program that uses the unit
    // since we actually write to a stream
    property OutputFileName: string read FOutputFileName write FOutputFileName;
    { XML config file name }
    property FileName: string read FFileName write FFileName;
    // html help files list
    property Files: TStrings read FFiles;
    // other files (.css, img etc)
    property OtherFiles: TStrings read FOtherFiles;
    // not used
    property AutoFollowLinks: Boolean read FAutoFollowLinks write FAutoFollowLinks; deprecated;
    { Table-Of-Content file name (*.hhc), must be in same directory with FileName }
    property TableOfContentsFileName: string read FTableOfContentsFileName write FTableOfContentsFileName;
    { Search Index file name (*.hhk), must be in same directory with FileName }
    property IndexFileName: string read FIndexFileName write FIndexFileName;
    { Write binary TOC if True }
    property MakeBinaryTOC: Boolean read FMakeBinaryTOC write FMakeBinaryTOC;
    { Write binary Index if True }
    property MakeBinaryIndex: Boolean read FMakeBinaryIndex write FMakeBinaryIndex;
    { Write full-text search index }
    property MakeSearchable: Boolean read FMakeSearchable write FMakeSearchable;
    { CHM title }
    property Title: string read FTitle write FTitle;
    { Default .html file name }
    property DefaultPage: string read FDefaultPage write FDefaultPage;
    { Default font name }
    property DefaultFont: string read FDefaultFont write FDefaultFont;
    { Help Windows settings }
    property Windows: TObjectList read FWindows;
    { Additional CHM files list }
    property MergeFiles: TStringList read FMergeFiles;
    { Default window name }
    property DefaultWindow: string read FDefaultWindow write FDefaultWindow;
    { If True, do scan html help files and add referenced files (images, css, etc..)
      to OtherFiles list }
    property ScanHtmlContents: Boolean read FScanHtmlContents write FScanHtmlContents;
    { Readme message, added after disclaimer }
    property ReadmeMessage: string read FReadmeMessage write FReadmeMessage;
    { Allowed help files extensions (*.html, *.htm by default) }
    property AllowedExtensions: TStringList read FAllowedExtensions;
    { How many simultaneous theads used for compression, (default = 4)}
    property Cores: Integer read FCores write FCores;
    { MS Locale ID for Topic, TOC, Index and HTML without encoding specified
      0 = UTF-8; default $0409 (en-us) }
    property LocaleID: word read FLocaleID write FLocaleID;

    { Triggered when part of file compressed }
    property OnProgress: TChmProgressCB read FOnProgress write FOnProgress;
    { Triggered on Error log message }
    property OnError: TChmErrorCB read FOnError write FOnError;
  end;

  TChmContextNode = class
    URLName: AnsiString;
    ContextNumber: Integer;
    ContextName: AnsiString;
  end;



const
  ChmErrorKindText: array[TCHMProjectErrorKind] of string =
    ('Error', 'Warning', 'Hint', 'Note', '');

implementation

uses XmlCfg, CHMTypes;

type

  { TFirstReference }

  TFirstReference = class
  public
    Location: string;
    constructor Create(const ALocation: string);
  end;

{ TFirstReference }

constructor TFirstReference.Create(const ALocation: string);
begin
  Location := ALocation;
end;

{ TChmProject }

function TChmProject.WriterGetFileData(const ADataName: string; out APathInChm: string;
  out AFileName: string; AStream: TStream): Boolean;
var
  fs: TFileStream;
begin
  Result := False; // Return true to abort compressing files

  if (AStream is TMemoryStream) then
    (AStream as TMemoryStream).LoadFromFile(ProjectDir + ADataName)
  else
  begin
    fs := TFileStream.Create(ProjectDir + ADataName, fmOpenRead or fmShareDenyWrite);
    try
      AStream.Size := fs.Size;
      AStream.Position := 0;
      if fs.Size > 0 then AStream.CopyFrom(fs, fs.Size);
    finally
      fs.Free();
    end;
  end;
  // clean up the Afilename
  AFileName := StringReplace(ExtractFileName(ADataName), '\', '/', [rfReplaceAll]);
  AFileName := StringReplace(AFileName, '//', '/', [rfReplaceAll]);

  APathInChm := '/' + ExtractFilePath(ADataName);
  if Assigned(FOnProgress) then
    FOnProgress(Self, ADataName);
end;

procedure TChmProject.WriterLastFileAdded(Sender: TObject);
var
  Writer: TChmWriter;
begin
  // Assign the TOC and index files
  Writer := TChmWriter(Sender);
  Writer.Cores := FCores;
  {$ifdef chmindex}
  Writeln('binindex filename ', IndexFileName);
  {$endif}
  if Assigned(FIndexStream) then
  begin
    FIndexStream.Position := 0;
    Writer.AppendIndex(FIndexStream);
    if MakeBinaryIndex then
    begin
      {$ifdef chmindex}
      Writeln('into binindex ');
      {$endif}
      Writer.AppendBinaryIndexFromSiteMap(FIndex, False);
    end;
  end;
  if Assigned(FTocStream) then
  begin
    Writer.AppendTOC(FTocStream);
    if MakeBinaryTOC then
    begin
      Writer.AppendBinaryTOCFromSiteMap(FToc);
    end;
  end;
  if not Assigned(Sender) then
    Writer.Free();
end;

constructor TChmProject.Create();
begin
  FFiles := TStringList.Create();
  FOtherFiles := TStringList.Create();
  FAllowedExtensions := TStringList.Create();
  FAllowedExtensions.Add('.HTM');
  FAllowedExtensions.Add('.HTML');
  FWindows := TObjectList.Create(True);
  FMergeFiles := TStringList.Create();
  ScanHtmlContents := False;
  FTotalFileList := TStringIndexList.Create();
  FSpareString := TStringIndex.Create();
  FAnchorList := TStringList.Create();
  FAnchorList.Sorted := True;
  FAnchorList.OwnsObjects := True;
  FLocaleID := 0;
end;

destructor TChmProject.Destroy();
var
  i: Integer;
begin
  FreeAndNil(FAnchorList);
  FreeAndNil(FSpareString);
  FreeAndNil(FTotalFileList);
  FreeAndNil(FMergeFiles);
  FreeAndNil(FWindows);
  FreeAndNil(FAllowedExtensions);
  FreeAndNil(FOtherFiles);
  for i := 0 to FFiles.Count - 1 do
    FFiles.Objects[i].Free;
  FreeAndNil(FFiles);

  if Assigned(FToc) then
    FreeAndNil(FToc);
  if Assigned(FIndex) then
    FreeAndNil(FIndex);
  if Assigned(FTocStream) then
    FreeAndNil(FTocStream);
  if Assigned(FIndexStream) then
    FreeAndNil(FIndexStream);
  inherited Destroy;
end;

type
  THHPSectionEnum = (secOptions, secWindows, secFiles, secMergeFiles,
    secAlias, secMap, secInfoTypes, secTextPopups, secUnknown);

  THHPOptionEnum = (
    OPTAUTO_INDEX,
    OPTAUTO_TOC,
    OPTBINARY_INDEX,
    OPTBINARY_TOC,
    OPTCITATION,
    OPTCOMPRESS,
    OPTCOPYRIGHT,
    OPTCOMPATIBILITY,
    OPTCOMPILED_FILE,
    OPTCONTENTS_FILE,
    OPTCREATE_CHI_FILE,
    OPTDBCS,
    OPTDEFAULT_FONT,
    OPTDEFAULT_WINDOW,
    OPTDEFAULT_TOPIC,
    OPTDISPLAY_COMPILE_NOTES,
    OPTDISPLAY_COMPILE_PROGRESS,
    OPTENHANCED_DECOMPILATION,
    OPTERROR_LOG_FILE,
    OPTFLAT,
    OPTFULL_TEXT_SEARCH_STOP_LIST,
    OPTFULL_TEXT_SEARCH,
    OPTIGNORE,
    OPTINDEX_FILE,
    OPTLANGUAGE,
    OPTPREFIX,
    OPTSAMPLE_STAGING_PATH,
    OPTSAMPLE_LIST_FILE,
    OPTTMPDIR, OPTTITLE,
    OPTCUSTOM_TAB,
    OPTUNKNOWN);

const
  HHPSectionNames: array[THHPSectionEnum] of string =
    ('OPTIONS', 'WINDOWS', 'FILES', 'MERGE FILES', 'ALIAS', 'MAP', 'INFOTYPES',
    'TEXT POPUPS', 'UNKNOWN');

  HHPOptionKeys: array [THHPOptionEnum] of string = (
    'AUTO INDEX',
    'AUTO TOC',
    'BINARY INDEX',
    'BINARY TOC',
    'CITATION',
    'COMPRESS',
    'COPYRIGHT',
    'COMPATIBILITY',
    'COMPILED FILE',
    'CONTENTS FILE',
    'CREATE CHI FILE',
    'DBCS',
    'DEFAULT FONT',
    'DEFAULT WINDOW',
    'DEFAULT TOPIC',
    'DISPLAY COMPILE NOTES',
    'DISPLAY COMPILE PROGRESS',
    'ENHANCED DECOMPILATION',
    'ERROR LOG FILE',
    'FLAT',
    'FULL-TEXT SEARCH STOP LIST',
    'FULL-TEXT SEARCH',
    'IGNORE',
    'INDEX FILE',
    'LANGUAGE',
    'PREFIX',
    'SAMPLE STAGING PATH',
    'SAMPLE LIST FILE',
    'TMPDIR',
    'TITLE',
    'CUSTOM TAB',
    'UNKNOWN');

function FindSectionName(const Name: string): THHPSectionEnum;
begin
  Result := Low(THHPSectionEnum);
  while (Result < secUnknown) and (Name <> HHPSectionNames[Result]) do
    Inc(Result);
end;

function FindOptionName(const Name: string): THHPOptionEnum;

begin
  Result := Low(THHPOptionEnum);
  while (Result < optUnknown) and (Name <> HHPOptionKeys[Result]) do
    Inc(Result);
end;

// hex codes of LCID (Locale IDs) see at http://msdn.microsoft.com/en-us/goglobal/bb964664.aspx
function GetLanguageID(const sValue: string): Word;
const
  DefaultLCID = $0409; // default "English - United States", 0x0409
var
  ACode: Word;
begin
  Result := DefaultLCID;
  if Length(sValue) >= 5 then
  begin
    Val(Trim(Copy(sValue, 1, 6)), Result, ACode);
    if ACode <> 0 then
      Result := DefaultLCID;
  end;
end;

procedure TChmProject.ReadIniOptions(KeyValuePairs: TStringList);
var
  i: Integer;
  Opt: THHPOptionEnum;
  OptVal, OptValUpper: string;
begin
  for i := 0 to KeyValuePairs.Count - 1 do
  begin
    Opt := FindOptionName(UpperCase(KeyValuePairs.Names[i]));
    OptVal := KeyValuePairs.ValueFromIndex[i];
    OptValUpper := UpperCase(OptVal);
    case Opt of
      OPTAUTO_INDEX: ;
      OPTAUTO_TOC: ;
      OPTBINARY_INDEX: MakeBinaryIndex := OptValUpper = 'YES';
      OPTBINARY_TOC: MakeBinaryToc := OptValUpper = 'YES';
      OPTCITATION: ;
      OPTCOMPRESS: ; // Doesn't seem to have effect in workshop
      OPTCOPYRIGHT: ;
      OPTCOMPATIBILITY: ;
      OPTCOMPILED_FILE: OutputFileName := OptVal;
      OPTCONTENTS_FILE: TableOfContentsFileName := OptVal;
      OPTCREATE_CHI_FILE: ;
      OPTDBCS: ; // What this field makes unicode is not known?
      OPTDEFAULT_FONT: DefaultFont := OptVal;
      OPTDEFAULT_WINDOW: DefaultWindow := OptVal;
      OPTDEFAULT_TOPIC: DefaultPage := OptVal;
      OPTDISPLAY_COMPILE_NOTES: ;
      OPTDISPLAY_COMPILE_PROGRESS: ;
      OPTENHANCED_DECOMPILATION: ;
      OPTERROR_LOG_FILE: ;
      OPTFLAT: ;
      OPTFULL_TEXT_SEARCH_STOP_LIST: ;
      OPTFULL_TEXT_SEARCH: MakeSearchable := OptValUpper = 'YES';
      OPTIGNORE: ;
      OPTINDEX_FILE: IndexFileName := OptVal;
      OPTLANGUAGE: LocaleID := GetLanguageID(OptVal);
      OPTPREFIX: ;  // doesn't seem to have effect
      OPTSAMPLE_STAGING_PATH: ;
      OPTSAMPLE_LIST_FILE: ;
      OPTTMPDIR: ;
      OPTTITLE: Title := OptVal;
      OPTCUSTOM_TAB: ;
      OPTUNKNOWN: ;  // can be used for errors on unknown keys
    end;
  end;
end;


procedure TChmProject.LoadFromFile(AFileName: string);
var
  Cfg: TXMLConfig;
  MergeFileCount, WinCount, FileCount: Integer;
  i: Integer;
  ContextNode: TChmContextNode;
  win: TCHMWindow;
  s: string;
begin
  Cfg := TXMLConfig.Create(nil);
  try
    Cfg.Filename := AFileName;
    FileName := AFileName;
    FBasePath := ExtractFilePath(ExpandFileName(AFileName));

    Files.Clear();
    FileCount := Cfg.GetValue('Files/Count/Value', 0);
    for i := 0 to FileCount - 1 do
    begin
      ContextNode := TChmContextNode.Create();
      ContextNode.URLName := Cfg.GetValue('Files/FileName' + IntToStr(i) + '/Value', '');
      ContextNode.ContextNumber := Cfg.GetValue('Files/FileName' + IntToStr(i) + '/ContextNumber', 0);
      ContextNode.ContextName := Cfg.GetValue('Files/FileName' + IntToStr(i) + '/ContextName', '');
      Files.AddObject(ContextNode.URLName, ContextNode);
    end;

    FileCount := Cfg.GetValue('OtherFiles/Count/Value', 0);
    for i := 0 to FileCount - 1 do
    begin
      s := Cfg.GetValue('OtherFiles/FileName' + IntToStr(i) + '/Value', '');
      OtherFiles.Add(s);
    end;

    WinCount := Cfg.GetValue('Windows/Count/Value', 0);
    for i := 0 to WinCount - 1 do
    begin
      win := TCHMWindow.Create();
      win.LoadFromXml(cfg, 'Windows/item' + IntToStr(i) + '/');
      FWindows.Add(win);
    end;

    MergeFileCount := Cfg.GetValue('MergeFiles/Count/Value', 0);
    for i := 0 to MergeFileCount - 1 do
      MergeFiles.Add(Cfg.GetValue('MergeFiles/FileName' + IntToStr(i) + '/value', ''));

    // load some values that changed key backwards compatible.

    IndexFileName := Cfg.GetValue('Files/IndexFile/Value', '');
    if IndexFileName = '' then
      IndexFileName := Cfg.GetValue('Settings/IndexFile/Value', '');

    TableOfContentsFileName := Cfg.GetValue('Files/TOCFile/Value', '');
    if TableOfContentsFileName = '' then
      TableOfContentsFileName := Cfg.GetValue('Settings/TOCFile/Value', '');

    // For chm file merging, bintoc must be false and binindex true. Change defaults in time?
    // OTOH, merging will be mostly done for fpdoc files, and that doesn't care about defaults.

    s := Cfg.GetValue('Files/MakeBinaryTOC/Value', '');
    if s = '' then
      MakeBinaryTOC := Cfg.GetValue('Settings/MakeBinaryTOC/Value', True)
    else
      MakeBinaryTOC := Cfg.GetValue('Files/MakeBinaryTOC/Value', True);

    s := Cfg.GetValue('Files/MakeBinaryIndex/Value', '');
    if s = '' then
      MakeBinaryIndex := Cfg.GetValue('Settings/MakeBinaryIndex/Value', False)
    else
      MakeBinaryIndex := Cfg.GetValue('Files/MakeBinaryIndex/Value', False);

    AutoFollowLinks := Cfg.GetValue('Settings/AutoFollowLinks/Value', False);
    MakeSearchable := Cfg.GetValue('Settings/MakeSearchable/Value', False);
    DefaultPage := Cfg.GetValue('Settings/DefaultPage/Value', '');
    Title := Cfg.GetValue('Settings/Title/Value', '');
    OutputFileName := Cfg.GetValue('Settings/OutputFileName/Value', '');
    DefaultFont := Cfg.GetValue('Settings/DefaultFont/Value', '');
    DefaultWindow := Cfg.GetValue('Settings/DefaultWindow/Value', '');
    ScanHtmlContents := Cfg.GetValue('Settings/ScanHtmlContents/Value', False);

    LocaleID := Cfg.GetValue('Settings/LocaleID/Value', $0409);

  finally
    Cfg.Free();
  end;
end;

function CleanupString(const s: string): string;
var
  i: Integer;
begin
  i := Pos(';', s);
  if i > 0 then
    Result := Trim(Copy(s, 1, i - 1))
  else
    Result := Trim(s);
end;

procedure TChmProject.LoadFromHHP(AFileName: string; LeaveInclude: Boolean);
// leaveinclude=true leaves includefiles includefiles.

  procedure AddAlias(const Key, Value: string);
  var
    i, j: Integer;
    Node: TCHMContextNode;
    KeyUpper, ValueUpper: string;
  begin
    { Defaults other than global }
    MakeBinaryIndex := True;

    {$ifdef hhp_debug} WriteLn('alias entry:', Key, '=', Value); {$endif}
    KeyUpper := UpperCase(Value);
    i := 0;
    j := Files.Count;
    while (i < j) and (UpperCase(TCHMContextnode(Files.Objects[i]).UrlName) <> KeyUpper) do
      Inc(i);
    if i = j then
    begin
      {$ifdef hhp_debug} WriteLn('alias new node:', Key); {$endif}
      Node := TCHMContextNode.Create();
      ValueUpper := StringReplace(Value, '\', '/', [rfReplaceAll]);
      ValueUpper := StringReplace(ValueUpper, '//', '/', [rfReplaceAll]);
      Node.URLName := ValueUpper;
      Node.ContextName := Key;
    end
    else
    begin
      Node := TCHMContextNode(Files.Objects[i]);
      Node.ContextName := Key;
    end;
  end;

  procedure ProcessAlias(sl: TStringList);
  var
    i, j: Integer;
    s: string;
    sl2: TStringList;
  begin
    for i := 0 to sl.Count - 1 do
    begin
      s := CleanupString(sl[i]);
      if UpperCase(Copy(s, 1, 8)) = '#INCLUDE' then
      begin
        Delete(s, 1, 8);
        s := Trim(s);
        if FileExists(s) then
        begin
          sl2 := TStringList.Create();
          try
            sl2.LoadFromFile(s);
            ProcessAlias(sl2);
          finally
            sl2.Free();
          end;
        end;
      end
      else
      begin
        s := CleanupString(s);
        j := Pos('=', s);
        if j > 0 then
          AddAlias(Trim(Copy(s, 1, j - 1)), Copy(s, j + 1, Length(s) - j));
      end;
    end;
  end;

  procedure AddMap(const Key, Value: string);
  var
    i, j: Integer;
    Node: TCHMContextNode;
    KeyUpper: string;
  begin
    {$ifdef hhp_debug} WriteLn('map entry:', Key, '=', Value); {$endif}
    KeyUpper := UpperCase(Key);
    i := 0;
    j := Files.Count;
    while (i < j) and (UpperCase(TCHMContextnode(Files.Objects[i]).ContextName) <> KeyUpper) do
      Inc(i);
    if i = j then
      raise Exception.Create('context "' + Key + '" not found!')
    else
    begin
      Node := TCHMContextNode(Files.Objects[i]);
      Node.ContextNumber := StrToIntDef(Value, 0);
    end;
  end;

  procedure ProcessMap(sl: TStringList);
  var
    i, j: Integer;
    s: string;
    sl2: TStringList;
  begin
    for i := 0 to sl.Count - 1 do
    begin
      s := CleanupString(sl[i]);
      {$ifdef hhp_debug} WriteLn('map item:', s); {$endif}
      if UpperCase(Copy(s, 1, 8)) = '#INCLUDE' then
      begin
        Delete(s, 1, 8);
        s := Trim(s);
        if FileExists(s) then
        begin
          sl2 := TStringList.Create();
          try
            sl2.LoadFromFile(s);
            ProcessMap(sl2);
          finally
            sl2.Free();
          end;
        end;
      end
      else
      begin
        s := CleanupString(s);
        if UpperCase(Copy(s, 1, 7)) = '#DEFINE' then
        begin
          Delete(s, 1, 7);
          s := Trim(s);
          j := Pos(' ', s);
          if j > 0 then
            AddMap(Trim(Copy(s, 1, j - 1)), Copy(s, j + 1, Length(s) - j));
        end
        else
        begin
          {$ifdef hhp_debug} WriteLn('map leftover:', s); {$endif}
        end;
      end;
    end;
  end;

var
  ini: TMemIniFile;
  // TMemInifile is more compatible with Delphi. Delphi's API based TIniFile fails on .hhp files.
  slSections, slValues: TStringList;
  i, j: Integer;
  Section: THHPSectionEnum;
  ContextNode: TChmContextNode;

begin
  { Defaults other than global }
  MakeBinaryIndex := True;
  FBasePath := ExtractFilePath(ExpandFileName(AFileName));
  ini := TMeminiFile.Create(AFileName);
  slSections := TStringList.Create();
  slValues := TStringList.Create();
  try
    ini.ReadSections(slSections);

    // Do the files Section first so that we can emit errors if
    // other sections reference unknown files.
    ini.ReadSectionValues(HHPSectionNames[secFiles], slValues);
    if slValues.Count > 0 then
    begin
      for j := 0 to slValues.Count - 1 do
      begin
        ContextNode := TChmContextNode.Create();
        ContextNode.URLName := StringReplace(slValues[j], '\', '/', [rfReplaceAll]);
        ContextNode.ContextNumber := 0;
        ContextNode.ContextName := '';
        Files.AddObject(ContextNode.URLName, ContextNode);
      end;
    end;

    // aliases also add file nodes.
    ini.ReadSectionValues(HHPSectionNames[secAlias], slValues); // resolve all aliases.
    if slValues.Count > 0 then
      ProcessAlias(slValues);

    // map files only add to existing file nodes.
    ini.ReadSectionValues(HHPSectionNames[secMap], slValues);
    if slValues.Count > 0 then
      ProcessMap(slValues);

    for i := 0 to slSections.Count - 1 do
    begin
      Section := FindSectionName(UpperCase(slSections[i]));
      if Section <> secUnknown then
        ini.ReadSectionValues(slSections[i], slValues);
      case Section of
        secOptions: ReadIniOptions(slValues);
        secWindows:
          for j := 0 to slValues.Count - 1 do
            FWindows.Add(TCHMWindow.Create(slValues[j]));
        secFiles: ; // already done
        secMergeFiles: FMergeFiles.Assign(slValues); // just a filelist
        secAlias: ; // already done
        secMap: ; // already done
        secInfoTypes: ; // unused for now.
        secTextPopups: ; // rarely used.
      end;
    end;

  finally
    slValues.Free();
    slSections.Free();
    ini.Free();
  end;
  ScanHtmlContents := True;
end;

procedure TChmProject.AddFileWithContext(AContextId: Integer; AFileName: AnsiString;
  AContextName: AnsiString = '');
var
  i: Integer;
  ContextNode: TChmContextNode;
begin
  i := Files.IndexOf(AFileName);
  if i = -1 then
  begin
    ContextNode := TChmContextNode.Create();
    ContextNode.URLName := AFileName;
    ContextNode.ContextNumber := AContextId;
    ContextNode.ContextName := AContextName;
    Files.AddObject(ContextNode.URLName, ContextNode);
  end
  else
  begin
    ContextNode := TChmContextNode(Files.Objects[i]);
    if not Assigned(ContextNode) then
    begin
      ContextNode := TChmContextNode.Create();
      ContextNode.URLName := AFileName;
      Files.Objects[i] := ContextNode;
    end;
    ContextNode.ContextNumber := AContextId;
    ContextNode.ContextName := AContextName;
  end;
end;

procedure TChmProject.SaveToFile(AFileName: string);
var
  Cfg: TXMLConfig;
  i: Integer;
  ContextNode: TChmContextNode;
begin
  Cfg := TXMLConfig.Create(nil);
  try
    Cfg.StartEmpty := True;
    Cfg.Filename := AFileName;
    Cfg.Clear();
    Cfg.SetValue('Files/Count/Value', Files.Count);
    for i := 0 to Files.Count - 1 do
    begin
      ContextNode := TChmContextNode(Files.Objects[i]);
      Cfg.SetValue('Files/FileName' + IntToStr(i) + '/Value', Files.Strings[i]);
      if Assigned(ContextNode) then
      begin
        Cfg.SetValue('Files/FileName' + IntToStr(i) + '/ContextNumber', ContextNode.ContextNumber);
        Cfg.SetValue('Files/FileName' + IntToStr(i) + '/ContextName', ContextNode.ContextName);
      end;
    end;

    Cfg.SetValue('OtherFiles/Count/Value', OtherFiles.Count);
    for i := 0 to OtherFiles.Count - 1 do
      Cfg.SetValue('OtherFiles/FileName' + IntToStr(i) + '/Value', OtherFiles.Strings[i]);


    Cfg.SetValue('Windows/Count/Value', FWindows.Count);
    for i := 0 to FWindows.Count - 1 do
      TCHMWindow(FWindows[i]).SaveToXml(Cfg, 'Windows/item' + IntToStr(i) + '/');

    Cfg.SetValue('MergeFiles/Count/Value', FMergeFiles.Count);
    for i := 0 to FMergeFiles.Count - 1 do
      Cfg.SetValue('MergeFiles/FileName' + IntToStr(i) + '/value', FMergeFiles[i]);

    // delete legacy keys.
    Cfg.DeleteValue('Files/IndexFile/Value');
    Cfg.DeleteValue('Files/TOCFile/Value');
    Cfg.DeleteValue('Files/MakeBinaryTOC/Value');
    Cfg.DeleteValue('Files/MakeBinaryIndex/Value');
    Cfg.SetValue('Settings/IndexFile/Value', IndexFileName);
    Cfg.SetValue('Settings/TOCFile/Value', TableOfContentsFileName);
    Cfg.SetValue('Settings/MakeBinaryTOC/Value', MakeBinaryTOC);
    Cfg.SetValue('Settings/MakeBinaryIndex/Value', MakeBinaryIndex);

    Cfg.SetValue('Settings/AutoFollowLinks/Value', AutoFollowLinks);
    Cfg.SetValue('Settings/MakeSearchable/Value', MakeSearchable);
    Cfg.SetValue('Settings/DefaultPage/Value', DefaultPage);
    Cfg.SetValue('Settings/Title/Value', Title);
    Cfg.SetValue('Settings/OutputFileName/Value', OutputFileName);
    Cfg.SetValue('Settings/DefaultFont/Value', DefaultFont);

    Cfg.SetValue('Settings/DefaultWindow/Value', DefaultWindow);
    Cfg.SetValue('Settings/ScanHtmlContents/Value', ScanHtmlContents);

    Cfg.SetValue('Settings/LocaleID/Value', LocaleID);

    Cfg.Flush();
  finally
    Cfg.Free();
  end;
end;

function TChmProject.ProjectDir(): string;
begin
  Result := ExtractFilePath(FileName);
end;

procedure TChmProject.Error(ErrorKind: TChmProjectErrorKind; Msg: string;
  DetailLevel: Integer = 0);
begin
  if Assigned(OnError) then
    OnError(Self, ErrorKind, Msg, DetailLevel);
end;

const
  ProtocolsArr: array[0..4] of string = ('HTTP:', 'HTTPS:', 'FTP:', 'MS-ITS:', 'MAILTO:');
  ProtocolLenArr: array[0..4] of Integer = (5, 6, 4, 7, 7);

function TChmProject.SanitizeURL(const BasePath, InString, LocalPath,
  LocalName: string; var OutString: string): Boolean;
var
  i, j, len: Integer;
  sAnchor: string;
begin
  Result := True;
  OutString := '';
  if InString = '' then
    Exit(False);

  len := Length(InString);
  if len = 0 then
    Exit(False);
  { Check for protocols before adding local path }
  i := 0;
  while (i <= High(ProtocolsArr)) do
  begin
    if strlicomp(@ProtocolsArr[i][1], @InString[1], ProtocolLenArr[i]) = 0 then
      Exit(False);
    Inc(i);
  end;
  OutString := LocalPath + InString;

  i := Pos('#', OutString);
  if i <> 0 then
  begin
    if i > 1 then
      sAnchor := OutString
    else
      sAnchor := LocalName + OutString;
    j := FAnchorList.IndexOf(sAnchor);
    if j < 0 then
    begin
      FAnchorList.AddObject(sAnchor, TFirstReference.Create(LocalName));
      sAnchor := '(new) ' + sAnchor;
    end;
    Error(CHMNote, 'sAnchor found ' + sAnchor + ' while scanning ' + LocalName, 1);
    Delete(OutString, i, Length(OutString) - i + 1);
  end;

  OutString := ExpandFileName(StringReplace(OutString, '%20', ' ', [rfReplaceAll]));
  // ExpandFileName(InString));

  OutString := ExtractRelativePath(BasePath, OutString);
  OutString := StringReplace(OutString, '\', '/', [rfReplaceAll]);
end;

function TChmProject.FileInTotalList(const s: string): Boolean;

begin
  Result := Assigned(FTotalFileList.GetStringIndex(s));
end;

procedure TChmProject.ScanList(ToScan, NewFiles: TStrings; Recursion: Boolean);
// toscan, list to search for htmlfiles to scan.
// newfiles, the resulting list of files.
// totalfilelist, the list that contains all found and specified files to check against.
// localfilelist (local var), files found in this file.
var
  LocalPath: string;

  function FindAttribute(Node: TDomNode; AttributeName: string): string;
  var
    Attributes: TDOMNamedNodeMap;
    AtNode: TDomNode;
    n: Integer;
  begin
    Result := '';
    if Assigned(Node) then
    begin
      Attributes := Node.Attributes;
      if Assigned(Attributes) then
      begin
        for n := 0 to Attributes.Length - 1 do
        begin
          AtNode := Attributes[n];
          if Assigned(AtNode) and (UTF8Encode(UpperCase(AtNode.NodeName)) = AttributeName) then
            Exit(UTF8Encode(AtNode.NodeValue));
        end;
      end;
    end;
  end;

  procedure CheckAttributes(Node: TDomNode; AttributeName: string;
    const LocalName: string; FileList: TStringList);
  var
    fn: string;
    val: string;
  begin
    val := FindAttribute(Node, AttributeName);
    fn := '';
    if SanitizeURL(FBasePath, val, LocalPath, LocalName, fn) then
    begin
      if (Length(fn) > 0) { Skip links to self using named anchors }
      and (not FileInTotalList(UpperCase(fn))) then
        FileList.Add(fn);
    end;
  end;


  function ScanTags(ParentNode: TDomNode; const LocalName: string; FileList: TStringList): TDomNode;
    // Seach first matching tag in siblings
  var
    ChildNode: TDomNode;
    s, att: AnsiString;
    i: Integer;
  begin
    Result := nil;
    if Assigned(ParentNode) then
    begin
      ChildNode := ParentNode.FirstChild;
      while Assigned(ChildNode) do
      begin
        ScanTags(ChildNode, LocalName, FileList);  // depth first.
        if (ChildNode is TDomElement) then
        begin
          s := UTF8Encode(UpperCase(TDOMElement(ChildNode).TagName));
          if s = 'LINK' then
          begin
            //printattributes(ChildNode,'');
            CheckAttributes(ChildNode, 'HREF', LocalName, FileList);
          end;
          if s = 'SCRIPT' then
          begin
            //printattributes(ChildNode,'');
            CheckAttributes(ChildNode, 'SRC', LocalName, FileList);
          end;
          if s = 'IMG' then
          begin
            //printattributes(ChildNode,'');
            CheckAttributes(ChildNode, 'SRC', LocalName, FileList);
          end;
          if s = 'A' then
          begin
            //printattributes(ChildNode,'');
            CheckAttributes(ChildNode, 'HREF', LocalName, FileList);
            att := 'NAME';
            s := FindAttribute(ChildNode, att);
            if s = '' then
            begin
              att := 'ID';
              s := FindAttribute(ChildNode, att);
            end;
            if s <> '' then
            begin
              i := FAnchorList.IndexOf(LocalName + '#' + s);
              if i < 0 then
              begin
                FAnchorList.Add(LocalName + '#' + s);
                Error(chmNote, 'New Anchor with ' + att + ' ' + s +
                  ' found while scanning ' + LocalName, 1);
              end
              else if FAnchorList.Objects[i] = nil then
                Error(chmWarning, 'Duplicate anchor definitions with ' +
                  att + ' ' + s + ' found while scanning ' + LocalName, 1)
              else
              begin
                FAnchorList.Objects[i].Free();
                FAnchorList.Objects[i] := nil;
                Error(chmNote, 'Anchor with ' + att + ' ' + s +
                  ' defined while scanning ' + LocalName, 1);
              end;
            end;
          end;
        end;
        ChildNode := ChildNode.NextSibling;
      end;
    end;
  end;

var
  LocalFileList: TStringList;
  DomDoc: THTMLDocument;
  i, j: Integer;
  fn, s: string;
  TmpLst: TStringList;
  //localpath : string;

  function TryPath(const AFileName: string): Boolean;
  var
    FileNameUpper: string;
  begin
    FileNameUpper := UpperCase(AFileName);
    if FileInTotalList(FileNameUpper) then
    begin
      Error(chmNote, 'Found duplicate file ' + AFileName + ' while scanning ' + fn, 1);
      Exit(True);
    end;

    Result := False;
    if FileExists(AFileName) then  // correct for relative path .html file?
    begin
      Result := True;
      FTotalFileList.AddStringIndex(FileNameUpper, 0);
      NewFiles.Add(AFileName);
      Error(chmNote, 'Found file ' + AFileName + ' while scanning ' + fn, 1);
    end;
  end;

begin
  LocalFileList := TStringList.Create();
  try
    for j := 0 to ToScan.Count - 1 do
    begin
      fn := ToScan[j];
      LocalFileList.Clear();
      if (FAllowedExtensions.IndexOf(UpperCase(ExtractFileDir(fn))) <> -1) then
      begin
        if FileExists(fn) then
        begin
          DomDoc := THtmlDocument.Create();
          try
            Error(chmNote, 'Scanning file ' + fn + '.', 5);
            ReadHtmlFile(DomDoc, fn);
            LocalPath := ExtractFilePath(fn);
            if (Length(LocalPath) > 0)
            and (not (LocalPath[Length(LocalPath)] in ['/', '\'])) then
              LocalPath := LocalPath + PathSep;
            ScanTags(DomDoc, ExtractFileName(fn), LocalFileList);
            for i := 0 to LocalFileList.Count - 1 do
            begin
              s := LocalFileList[i];
              if not TryPath(s) then
                //                     if not trypath(localpath+s) then
                Error(chmWarning, 'Found file ' + s + ' while scanning ' +
                  fn + ', but couldn''t find it on disk', 2);
            end;
          except
            on e: Exception do
              Error(chmError, 'Html parsing ' + fn + ', failed. Please submit a bug.');
          end;
          DomDoc.Free();
        end
        else
        begin
          Error(chmNote, 'Can''t find file ' + fn + ' to scan it.', 5);
        end;
      end
      else
        Error(chmNote, 'Not scanning file because of unknown extension ' + fn, 5);
    end;

  finally
    LocalFileList.Free();
  end;

  if (NewFiles.Count > 0) and Recursion then
  begin
    TmpLst := TStringList.Create();
    try
      ScanList(NewFiles, TmpLst, True);
      NewFiles.AddStrings(TmpLst);
    finally
      TmpLst.Free();
    end;
  end;
end;

procedure TChmProject.ScanSitemap(SiteMap: TChmSiteMap; NewFiles: TStrings;
  Recursion: Boolean);

  procedure ScanItems(AItems: TChmSiteMapItems);
  var
    i: Integer;
    Item: TChmSiteMapItem;
    s: string;
  begin
    for i := 0 to AItems.Count - 1 do
    begin
      Item := AItems.Item[i];
      if SanitizeURL(FBasePath, Item.Local, '', 'Site Map for ' + Item.Text, s) then
        // sanitize, remove stuff etc.
      begin
        if not FileInTotalList(UpperCase(s)) then
        begin
          if FileExists(s) then
          begin
            Error(chmNote, 'Good url: ' + s + '.', 5);
            FTotalFileList.AddStringIndex(UpperCase(s), 0);
            NewFiles.Add(s);
          end
          else
            Error(chmNote, 'duplicate url: ' + s + '.', 5);
        end
        else
          Error(chmNote, 'duplicate url: ' + s + '.', 5);
      end
      else
        Error(chmNote, 'Bad url: ' + s + '.', 5);

      if Assigned(Item.Children) and (Item.Children.Count > 0) then
        ScanItems(Item.Children);
    end;
  end;

var
  LocalFileList: TStringList;

begin
  LocalFileList := TStringList.Create();
  try
    ScanItems(SiteMap.Items);
    ScanList(NewFiles, LocalFileList, True);
    NewFiles.AddStrings(LocalFileList);
  finally
    LocalFileList.Free();
  end;
end;

procedure TChmProject.ScanHtml();
var
  HelpList, LocalFileList: TStringList;
  i: Integer;
begin

  for i := 0 to OtherFiles.Count - 1 do
  begin
    FTotalFileList.AddStringIndex(UpperCase(OtherFiles[i]), 0);
  end;

  for i := 0 to Files.Count - 1 do
  begin
    FTotalFileList.AddStringIndex(UpperCase(Files[i]), 0);
  end;

  LocalFileList := TStringList.Create();
  try
    ScanList(FFiles, LocalFileList, True);
    OtherFiles.AddStrings(LocalFileList);
    LocalFileList.Clear();
    if (FDefaultPage <> '') and (not FileInTotalList(UpperCase(FDefaultPage))) then
    begin
      Error(chmNote, 'Scanning default file : ' + FDefaultPage + '.', 3);
      HelpList := TStringList.Create();
      try
        HelpList.Add(FDefaultPage);
        ScanList(HelpList, LocalFileList, True);
        OtherFiles.AddStrings(LocalFileList);
        LocalFileList.Clear();
      finally
        HelpList.Free();
      end;
    end;
    if Assigned(FToc) then
    begin
      Error(chmNote, 'Scanning TOC file : ' + FTableOfContentsFileName + '.', 3);
      try
        ScanSitemap(FToc, LocalFileList, True);
        OtherFiles.AddStrings(LocalFileList);
      except
        on e: Exception do
          Error(chmError, 'Error scanning TOC file (' + FTableOfContentsFileName + ')');
      end;
    end;
    LocalFileList.Clear();
    if Assigned(FIndex) then
    begin
      Error(chmNote, 'Scanning Index file : ' + FIndexFileName + '.', 3);
      try
        ScanSitemap(FIndex, LocalFileList, True);
        OtherFiles.AddStrings(LocalFileList);
      except
        on e: Exception do
          Error(chmError, 'Error scanning index file (' + FIndexFileName + ')');
      end;
    end;
  finally
    LocalFileList.Free();
  end;
end;


procedure TChmProject.WriteChm(AOutStream: TStream);
var
  Writer: TChmWriter;
  ContextNode: TChmContextNode;
  i: Integer;
begin

  LoadSiteMaps();

  // Scan html for "rest" files.
  if ScanHtmlContents then
    ScanHtml();
  // Since this is slowing we opt to skip this step, and only do this on html load.

  Writer := TChmWriter.Create(AOutStream, False);
  try
    // our callback to get data
    Writer.OnGetFileData := @WriterGetFileData;
    Writer.OnLastFile := @WriterLastFileAdded;

    // give it the list of html files
    Writer.FilesToCompress.AddStrings(Files);

    // give it the list of other files
    Writer.FilesToCompress.AddStrings(OtherFiles);

    // now some settings in the chm
    Writer.DefaultPage := DefaultPage;
    Writer.Title := Title;
    Writer.DefaultFont := DefaultFont;
    Writer.FullTextSearch := MakeSearchable;
    //Writer.HasBinaryTOC := MakeBinaryTOC;
    //Writer.HasBinaryIndex := MakeBinaryIndex;
    Writer.IndexName := IndexFileName;
    Writer.TocName := TableOfContentsFileName;
    Writer.ReadmeMessage := ReadmeMessage;
    Writer.DefaultWindow := FDefaultWindow;
    Writer.LocaleID := LocaleID;
    for i := 0 to Files.Count - 1 do
    begin
      ContextNode := TChmContextNode(Files.Objects[i]);
      if not FileExists(Files[i]) then
        Error(chmWarning, 'File ' + Files[i] + ' does not exist');
      if Assigned(ContextNode) and (ContextNode.ContextNumber <> 0) then
        Writer.AddContext(ContextNode.ContextNumber, Files[i]);
    end;
    if FWindows.Count > 0 then
      Writer.Windows := FWindows;
    if FMergeFiles.Count > 0 then
      Writer.MergeFiles := FMergeFiles;
    if Assigned(FToc) then
      Writer.TocSitemap := FToc;

    // and write!

    Error(chmNone, 'Writing CHM ' + OutputFileName, 0);

    Writer.Execute();

  finally
    Writer.Free();
  end;
end;

procedure TChmProject.ShowUndefinedAnchors();
var
  i: Integer;
begin
  for i := 0 to FAnchorList.Count - 1 do
  begin
    if FAnchorList.Objects[i] <> nil then
      Error(chmError, 'Anchor ' + FAnchorList[i] + ' undefined; first use ' +
        TFirstReference(FAnchorList.Objects[i]).Location);
  end;
end;

procedure TChmProject.LoadSiteMaps();
var
  FullFileName: string;
begin
  // #IDXHDR (merged files) goes into the system file, and need to keep  TOC sitemap around
  if FTableOfContentsFileName <> '' then
  begin
    FullFileName := IncludeTrailingPathDelimiter(ProjectDir()) + ExtractFileName(FTableOfContentsFileName);
    if FileExists(FullFileName) then
    begin
      FreeAndNil(FTocStream);
      FTocStream := TMemoryStream.Create();
      try
        FTocStream.LoadFromFile(FullFileName);
        //WriteLn(FTableOfContentsFileName, ' ' , FTocStream.Size);
        FTocStream.Position := 0;
        FreeAndNil(FToc);
        FToc := TChmSiteMap.Create(stTOC);
        FToc.LoadFromStream(FTocStream);
        //FToc.SaveToFile('bla.something');
      except
        on e: Exception do
        begin
          Error(chmError, 'Error loading TOC file ' + FTableOfContentsFileName);
          FreeAndNil(FToc);
          FreeAndNil(FTocStream);
        end;
      end;
    end
    else
      Error(chmError, 'Can''t find TOC file' + FTableOfContentsFileName);
  end;

  if FIndexFileName <> '' then
  begin
    FullFileName := IncludeTrailingPathDelimiter(ProjectDir()) + ExtractFileName(FIndexFileName);
    if FileExists(FullFileName) then
    begin
      FreeAndNil(FIndexStream);
      FIndexStream := TMemoryStream.Create();
      try
        FIndexStream.LoadFromFile(FullFileName);
        FIndexStream.Position := 0;
        FreeAndNil(FIndex);
        FIndex := TChmSiteMap.Create(stIndex);
        FIndex.LoadFromFile(FullFileName);
      except
        on e: Exception do
        begin
          Error(chmError, 'Error loading index file ' + FIndexFileName);
          FreeAndNil(FIndex);
          FreeAndNil(FIndexStream);
        end;
      end;
    end
    else
      Error(chmError, 'Can''t find index file ' + FIndexFileName);
  end;
end;


end.
