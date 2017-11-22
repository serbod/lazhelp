{ Copyright (C) <2005> <Andrew Haines> chmreader.pas

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
  See the file COPYING.modifiedLGPL, included in this distribution,
  for details about the copyright.
}
unit chmreader;

{$mode objfpc}{$H+}

//{$DEFINE CHM_DEBUG}
{ $DEFINE CHM_DEBUG_CHUNKS}

interface

uses
  Classes, SysUtils, chmbase, chmtypes, paslzx, chmFIftiMain, chmsitemap;

type

  TLZXResetTableArr = array of QWord;

  PContextItem = ^TContextItem;
  TContextItem = record
    Context: THelpContext;
    Url: String;
  end;

  TContextList = class(TList)
  public
    procedure AddContext(Context: THelpContext; const Url: String);
    function GetURL(Context: THelpContext): String;
    procedure Clear(); override;
  end;


  TFileEntryForEach = procedure(Name: String; Offset, UncompressedSize, Section: Integer) of object;

  { TITSFReader }
  { Info-Tech Storage Format reader
    Provide access to CHM internal files directory listing and content. }
  TITSFReader = class(TObject)
  private
    FSectionNames: TStringList;
    FTmpStrings: TStrings;
    function GetDirectoryChunk(Index: Integer; OutStream: TStream): Integer;
    function ReadPMGLchunkEntryFromStream(Stream: TMemoryStream; var PMGLEntry: TPMGListChunkEntry): Boolean;
    function ReadPMGIchunkEntryFromStream(Stream: TMemoryStream; var PMGIEntry: TPMGIIndexChunkEntry): Boolean;
    procedure LookupPMGLchunk(Stream: TMemoryStream; out PMGLChunk: TPMGListChunk);
    procedure LookupPMGIchunk(Stream: TMemoryStream; out PMGIChunk: TPMGIIndexChunk);

    { Read content from ::DataSpace/Storage/ file }
    function ReadBlockFromSection(SectionPrefix: String; StartPos: QWord; BlockLength: QWord; AData: TStream): Boolean;
    { Returns the blocksize }
    function FindBlocksFromUnCompressedAddr(const ResetTableEntry: TPMGListChunkEntry;
       out CompressedSize: QWord; out UnCompressedSize: QWord; out LZXResetTable: TLZXResetTableArr): QWord;

    procedure ForEachHandler(Name: String; Offset, UncompressedSize, Section: Integer);
  protected
    { CHM file stream }
    FStream: TStream;
    FFreeStreamOnDestroy: Boolean;
    { main CHM file header }
    FITSFHeader: TITSFHeader;
    FHeaderSuffix: TITSFHeaderSuffix;
    { CHM directory header }
    FDirectoryHeader: TITSPHeader;
    FDirectoryHeaderPos: QWord;
    FDirectoryHeaderLength: QWord;
    FDirectoryEntriesStartPos: QWord;
    { contains the last entry found by ObjectExists }
    FCachedEntry: TPMGListChunkEntry;
    FDirectoryEntriesCount: LongWord;
    procedure ReadHeader(); virtual;
    procedure ReadHeaderEntries(); virtual;
    function GetChunkType(Stream: TMemoryStream; ChunkIndex: LongInt): TDirChunkType;
    { Sections must be freed by caller! }
    procedure GetSections(out Sections: TStringList); deprecated;
    { Read sections names to strings list }
    function ReadSections(ASections: TStrings): Boolean;
  public
    { Assign CHM file stream, read CHM main header and header entries from beginning of stream }
    constructor Create(AStream: TStream; FreeStreamOnDestroy: Boolean); virtual;
    destructor Destroy; override;
  public
    ChmLastError: LongInt;
    { Check CHM header and return True if data valid }
    function IsValidFile(): Boolean;
    { Read directory listing entries and pass every entry into ForEach callback method }
    procedure GetCompleteFileList(ForEach: TFileEntryForEach; AIncludeInternalFiles: Boolean = True); virtual;
    { Read content files names into string list
      AIncludeInternalFiles - include  }
    procedure ReadFilesNamesList(sl: TStrings; AIncludeInternalFiles: Boolean = False);
    { Returns zero if no. Otherwise it is the size of the object
     Found AName directory entry is set as CachedEntry
     NOTE directories will return zero size even if they exist }
    function ObjectExists(const AName: String): QWord; virtual;
    { Returns memory block from given section name
      !! You MUST Free() the stream! }
    function GetObject(Name: String): TMemoryStream; virtual; deprecated;
    { Seek internal file by his Name and read file content into AData stream, return True on success }
    function ReadFileContent(const AName: String; AData: TStream): Boolean; virtual;
    { Seek internal file by his AEntry and read file content into AData stream }
    function ReadFileContentByEntry(const AEntry: TPMGListChunkEntry; AData: TStream): Boolean; virtual;
    // Chunk entry from last ObjectExists() call
    property CachedEntry: TPMGListChunkEntry read FCachedEntry;
  end;

  { TChmReader }
  { Provide CHM help features:
    - Topics, TOC (Table Of Content), Index,
    - Context ID and full-text search,
    - browser windows appearance (fonts, colors, buttons) }
  TChmReader = class(TITSFReader)
  private
    FSearchReader: TChmSearchReader;
    // last added sitemap item
    FLastSiteMapItem: TChmSiteMapItem;
    // read and parse #WINDOWS
    procedure ReadSectionWindows();
    // read and parse #SYSTEM
    procedure ReadSectionSystem();
    // read and parse #IVB (help context)
    procedure ReadSectionIvb();
    procedure ParseSiteMapListingBlock(SiteMap: TChmSiteMap; p: PByte);
  protected
    FContextList: TContextList;
    FWindowsList: TCHMWindowList;
    FTOPICSStream: TMemoryStream;
    FURLSTRStream: TMemoryStream;
    FURLTBLStream: TMemoryStream;
    FStringsStream: TMemoryStream;
    FDefaultPage: String;
    FIndexFile: String;
    FTOCFile: String;
    FTitle: String;
    FPreferedFont: String;
    FLocaleID: DWord;
    FDefaultWindow: String;
    { Read item from STRINGS section from APosition offset }
    function ReadStringsEntry(APosition: DWord): String;
    function ReadStringsEntryFromStream(strm: TStream): String;
    { Return LocalUrl string from #URLSTR }
    function ReadURLSTR(APosition: DWord): String;
    { Read #WINDOWS section items }
    procedure ReadWindows(mem: TMemoryStream);
  public
    { Read data from CHM to internal cache }
    procedure ReadCommonData();
    { Return True if common data was readed }
    function CheckCommonStreams(): Boolean;
    { Set AStream as CHM file, read common data (TOC, Index, Context)
      Warning! AStream MUST NOT be freed before Destroy()!
      Set FreeStreamOnDestroy = True to automatically Free() AStream on Destroy() }
    constructor Create(AStream: TStream; FreeStreamOnDestroy: Boolean); override;
    destructor Destroy; override;
    { Returns URL by help context ID }
    function GetContextUrl(Context: THelpContext): String;
    { Return TOC topics count }
    function GetTopicsCount(): Integer;
    { Seek TOC topic by Index, read ATitle, returns a LocalURL }
    function LookupTopicByID(ATopicID: Integer; out ATitle: String): String;
    { Result MUST be freed by caller }
    function GetTOCSitemap(ForceXML: Boolean = False): TChmSiteMap; deprecated;
    { Result MUST be freed by caller }
    function GetIndexSitemap(ForceXML: Boolean = False): TChmSiteMap; deprecated;
    { Read TOC (Table Of Content) items into SiteMap object }
    function ReadTOCSitemap(SiteMap: TChmSiteMap; ForceXML: Boolean = False): Boolean;
    { Read Index items into SiteMap object }
    function ReadIndexSitemap(SiteMap: TChmSiteMap; ForceXML: Boolean = False): Boolean;
    { Read Topic items into list }
    function ReadTopicList(ATopicList: TChmTopicItemList): Boolean;
    { Returns True if Context list not empty }
    function HasContextList(): Boolean;

    { Default page URL }
    property DefaultPage: String read FDefaultPage;
    { Index file name (.hhk) }
    property IndexFile: String read FIndexFile;
    { TOC file name (.hhc) }
    property TOCFile: String read FTOCFile;
    { CHM Title }
    property Title: String read FTitle write FTitle;
    { Default font name }
    property PreferedFont: String read FPreferedFont;
    { MS Windows locale ID code }
    property LocaleID: DWord read FLocaleID;
    { Full-text search reader }
    property SearchReader: TChmSearchReader read FSearchReader write FSearchReader;
    { Help context list, contain THelpContext:URL pairs }
    property ContextList: TContextList read FContextList;
    { Help window appearance defenitions (TChmWindow) list }
    property Windows: TCHMWindowList read FWindowsList;
    { not used }
    property DefaultWindow: string read FDefaultWindow;
  end;

  TChmFileList = class;
  TChmFileOpenEvent = procedure(ChmFileList: TChmFileList; Index: Integer) of object;

  { TChmFileList }
  { CHM files list with associated CHM Readers
    - get contents from library URLs - ms-its:name.chm::/topic.htm
    - combine Topics and Index from multiple help files }
  TChmFileList = class(TStringList)
  protected
    FLastChm: TChmReader;
    FUnNotifiedFiles: TList;
    FOnOpenNewFile: TChmFileOpenEvent;
    function GetChmReader(AIndex: Integer): TChmReader;
    function GetFileName(AIndex: Integer): String;
    { Create TChmReader with AFileName and add to list }
    procedure OpenNewFile(AFileName: String); deprecated;
    function CheckOpenFile(AFileName: String): Boolean; deprecated;
    { Extract file name from specified Name, and do OpenChmFile()
      then works as ObjectExists(). Known META file link types:
        ms-its:name.chm::/topic.htm
        mk:@MSITStore:name.chm::/topic.htm }
    function MetaObjectExists(var Name: String): QWord;
    function MetaGetObject(Name: String): TMemoryStream; deprecated;
    function MetaGetObjectData(Name: String; AData: TStream): Boolean;
    procedure SetOnOpenNewFile(AValue: TChmFileOpenEvent);
  public
    constructor Create(PrimaryFileName: String);
    destructor Destroy(); override;
    procedure Delete(Index: Integer); override;
    { If AFileName not in list, create TChmReader with AFileName and add to list.
      Set ChmReader with AFileName as default. }
    function OpenChmFile(AFileName: String): Boolean;
    { See ReadFileContent() }
    function GetObject(const AName: String): TMemoryStream; deprecated;
    { Seek internal file by his Name and read file content into AData stream, return True on success
      From default ChmReader, selected by ObjectExists() }
    function ReadFileContent(const AName: String; AData: TStream): Boolean;
    { Return True if CHM file with AFileName exists in list }
    function IsAnOpenFile(AFileName: String): Boolean;
    { Seek named file in default ChmReader. If AChmReader specified, then it become default }
    function ObjectExists(const AName: String; var AChmReader: TChmReader): QWord;
    //properties
    property ChmReaders[Index: Integer]: TChmReader read GetChmReader;
    property FileName[Index: Integer]: String read GetFileName;
    { default ChmReader }
    property LastChm: TChmReader read FLastChm;
    { Triggered, when CHM file opened and new TChmReader created.
      On assignment, triggers for all previously opened files }
    property OnOpenNewFile: TChmFileOpenEvent read FOnOpenNewFile write SetOnOpenNewFile;
  end;

//ErrorCodes
const
  ERR_NO_ERR = 0;
  ERR_STREAM_NOT_ASSIGNED = 1;
  ERR_NOT_SUPPORTED_VERSION = 2;
  ERR_NOT_VALID_FILE = 3;
  ERR_UNKNOWN_ERROR = 10;

  function ChmErrorToStr(Error: Integer): String;

implementation

function ChmErrorToStr(Error: Integer): String;
begin
  Result := '';
  case Error of
    ERR_STREAM_NOT_ASSIGNED    : Result := 'ERR_STREAM_NOT_ASSIGNED';
    ERR_NOT_SUPPORTED_VERSION  : Result := 'ERR_NOT_SUPPORTED_VERSION';
    ERR_NOT_VALID_FILE         : Result := 'ERR_NOT_VALID_FILE';
    ERR_UNKNOWN_ERROR          : Result := 'ERR_UNKNOWN_ERROR';
  end;
end;

function ChunkType(Stream: TMemoryStream): TDirChunkType;
var
  ChunkID: array[0..3] of Char;
begin
  Result := ctUnknown;
  if Stream.Size < 4 then Exit;
  Move(Stream.Memory^, ChunkId[0], 4);
  if ChunkID = 'PMGL' then Result := ctPMGL
  else if ChunkID = 'PMGI' then Result := ctPMGI
  else if ChunkID = 'AOLL' then Result := ctAOLL
  else if ChunkID = 'AOLI' then Result := ctAOLI;
end;

function FixUrl(const AValue: String): String;
begin
  Result := StringReplace(AValue, '\', '/', [rfReplaceAll]);
end;

{ TITSFReader }

constructor TITSFReader.Create(AStream: TStream; FreeStreamOnDestroy: Boolean);
begin
  FSectionNames := TStringList.Create();
  FStream := AStream;
  FStream.Position := 0;
  FFreeStreamOnDestroy := FreeStreamOnDestroy;
  ReadHeader();
  ReadSections(FSectionNames);
  if not IsValidFile then Exit;
end;

destructor TITSFReader.Destroy;
begin
  if FFreeStreamOnDestroy then FreeAndNil(FStream);

  FreeAndNil(FSectionNames);
  inherited Destroy;
end;

function TITSFReader.GetChunkType(Stream: TMemoryStream; ChunkIndex: LongInt): TDirChunkType;
var
  Sig: array[0..3] of char;
begin
  Result := ctUnknown;
  if ChunkIndex < 0 then Exit;
  Stream.Position := FDirectoryEntriesStartPos + (FDirectoryHeader.ChunkSize * Cardinal(ChunkIndex));

  Stream.Read(Sig, 4);
  if Sig = 'PMGL' then Result := ctPMGL
  else if Sig = 'PMGI' then Result := ctPMGI
  else if Sig = 'AOLL' then Result := ctAOLL
  else if Sig = 'AOLI' then Result := ctAOLI;
end;

function TITSFReader.GetDirectoryChunk(Index: Integer; OutStream: TStream): Integer;
begin
  Result := Index;
  FStream.Position := FDirectoryEntriesStartPos + (FDirectoryHeader.ChunkSize * Cardinal(Index));
  OutStream.Position := 0;
  OutStream.Size := FDirectoryHeader.ChunkSize;
  OutStream.CopyFrom(FStream, FDirectoryHeader.ChunkSize);
  OutStream.Position := 0;
end;

procedure TITSFReader.LookupPMGLchunk(Stream: TMemoryStream; out PMGLChunk: TPMGListChunk);
begin
  //Stream.Position := FDirectoryEntriesStartPos + (FDirectoryHeader.ChunkSize * ChunkIndex);
  Stream.Read(PMGLChunk, SizeOf(PMGLChunk));
  {$IFDEF ENDIAN_BIG}
  with PMGLChunk do
  begin
    UnusedSpace := LEtoN(UnusedSpace);
    //Unknown1
    PreviousChunkIndex := LEtoN(PreviousChunkIndex);
    NextChunkIndex := LEtoN(NextChunkIndex);
  end;
  {$ENDIF}
end;

function TITSFReader.ReadPMGLchunkEntryFromStream(Stream: TMemoryStream; var PMGLEntry: TPMGListChunkEntry): Boolean;
var
Buf: array [0..1023] of char;
NameLength: LongInt;
begin
  Result := False;
  //Stream.Position := FDirectoryEntriesStartPos + (FDirectoryHeader.ChunkSize * ChunkIndex);
  NameLength := LongInt(GetCompressedInteger(Stream));

  if NameLength > 1022 then NameLength := 1022;
  Stream.Read(buf[0], NameLength);
  buf[NameLength] := #0;
  PMGLEntry.Name := buf;
  PMGLEntry.ContentSection := LongWord(GetCompressedInteger(Stream));
  PMGLEntry.ContentOffset := GetCompressedInteger(Stream);
  PMGLEntry.DecompressedLength := GetCompressedInteger(Stream);
  if NameLength = 0 then Exit; // failed GetCompressedInteger sanity check
  Result := True;
end;

procedure TITSFReader.LookupPMGIchunk(Stream: TMemoryStream; out PMGIChunk: TPMGIIndexChunk);
begin
  //Stream.Position := FDirectoryEntriesStartPos + (FDirectoryHeader.ChunkSize * ChunkIndex);
  Stream.Read(PMGIChunk, SizeOf(PMGIChunk));
  {$IFDEF ENDIAN_BIG}
  with PMGIChunk do begin
    UnusedSpace := LEtoN(UnusedSpace);
  end;
  {$ENDIF}
end;

function TITSFReader.ReadPMGIchunkEntryFromStream(Stream: TMemoryStream;
  var PMGIEntry: TPMGIIndexChunkEntry): Boolean;
var
  Buf: array [0..1023] of char;
  NameLength: LongInt;
begin
  Result := False;
  //Stream.Position := FDirectoryEntriesStartPos + (FDirectoryHeader.ChunkSize * ChunkIndex);
  NameLength := LongInt(GetCompressedInteger(Stream));
  if NameLength > 1023 then NameLength := 1023;
  Stream.Read(buf, NameLength);

  buf[NameLength] := #0;
  PMGIEntry.Name := buf;

  PMGIEntry.ListingChunk := GetCompressedInteger(Stream);
  if NameLength = 0 then Exit; // failed GetCompressedInteger sanity check
  Result := True;
end;

function TITSFReader.IsValidFile(): Boolean;
begin
  if (FStream = nil) then ChmLastError := ERR_STREAM_NOT_ASSIGNED
  else if (FITSFHeader.ITSFsig <> 'ITSF') then ChmLastError := ERR_NOT_VALID_FILE
  //else if (FITSFHeader.Version <> 2) and (FITSFHeader.Version <> 3)
  else if not (FITSFHeader.Version in [2..4])
  then
    ChmLastError := ERR_NOT_SUPPORTED_VERSION;
  Result := ChmLastError = ERR_NO_ERR;
end;

procedure TITSFReader.GetCompleteFileList(ForEach: TFileEntryForEach; AIncludeInternalFiles: Boolean = True);
var
  ChunkStream: TMemoryStream;
  I: Integer;
  Entry: TPMGListChunkEntry;
  PMGLChunk: TPMGListChunk;
  CutOffPoint: Integer;
  NameLength: Integer;
  {$IFDEF CHM_DEBUG_CHUNKS}
  PMGIChunk: TPMGIIndexChunk;
  PMGIndex: Integer;
  {$ENDIF}
begin
  if ForEach = nil then Exit;
  ChunkStream := TMemoryStream.Create();
  try
    {$IFDEF CHM_DEBUG_CHUNKS}
    WriteLn('ChunkCount = ', fDirectoryHeader.DirectoryChunkCount);
    {$ENDIF}
    for I := 0 to FDirectoryHeader.DirectoryChunkCount-1 do
    begin
      GetDirectoryChunk(I, ChunkStream);
      case ChunkType(ChunkStream) of
        ctPMGL:
        begin
          LookupPMGLchunk(ChunkStream, PMGLChunk);
          {$IFDEF CHM_DEBUG_CHUNKS}
          WriteLn('PMGL: ', I, ' Prev PMGL: ', PMGLChunk.PreviousChunkIndex, ' Next PMGL: ', PMGLChunk.NextChunkIndex);
          {$ENDIF}
          CutOffPoint := ChunkStream.Size - PMGLChunk.UnusedSpace;
          while ChunkStream.Position <  CutOffPoint do
          begin
            NameLength := GetCompressedInteger(ChunkStream);
            if (ChunkStream.Position > CutOffPoint) then Continue; // we have entered the quickref section
            SetLength(Entry.Name, NameLength);
            ChunkStream.ReadBuffer(Entry.Name[1], NameLength);
            if (Entry.Name = '') or (ChunkStream.Position > CutOffPoint) then Break; // we have entered the quickref section
            Entry.ContentSection := GetCompressedInteger(ChunkStream);
            if ChunkStream.Position > CutOffPoint then Break; // we have entered the quickref section
            Entry.ContentOffset := GetCompressedInteger(ChunkStream);
            if ChunkStream.Position > CutOffPoint then Break; // we have entered the quickref section
            Entry.DecompressedLength := GetCompressedInteger(ChunkStream);
            if ChunkStream.Position > CutOffPoint then Break; // we have entered the quickref section
            FCachedEntry := Entry; // if the caller trys to get this data we already know where it is :)
            if  (Length(Entry.Name) = 1)
            or (AIncludeInternalFiles
                or
               ((Length(Entry.Name) > 1) and (not (Entry.Name[2] in ['#','$',':']))))
            then
              ForEach(Entry.Name, Entry.ContentOffset, Entry.DecompressedLength, Entry.ContentSection);
          end;
        end;
        {$IFDEF CHM_DEBUG_CHUNKS}
        ctPMGI:
        begin
          WriteLn('PMGI: ', I);
          LookupPMGIchunk(ChunkStream, PMGIChunk);
          CutOffPoint := ChunkStream.Size - PMGIChunk.UnusedSpace - 10;
          while ChunkStream.Position <  CutOffPoint do
          begin
            NameLength := GetCompressedInteger(ChunkStream);
            SetLength(Entry.Name, NameLength);
            ChunkStream.ReadBuffer(Entry.Name[1], NameLength);
            PMGIndex := GetCompressedInteger(ChunkStream);
            WriteLn(Entry.Name, '  ', PMGIndex);
          end;
        end;
        ctUnknown: WriteLn('UNKNOWN CHUNKTYPE!' , I);
        {$ENDIF}
      end;
    end;

  finally
    ChunkStream.Free();
  end;
end;

procedure TITSFReader.ReadFilesNamesList(sl: TStrings; AIncludeInternalFiles: Boolean);
begin
  FTmpStrings := sl;
  GetCompleteFileList(@ForEachHandler, AIncludeInternalFiles);
  FTmpStrings := nil;
end;

function TITSFReader.ObjectExists(const AName: String): QWord;
var
  ChunkStream: TMemoryStream;
  QuickRefCount: Word;
  QuickRefIndex: array of Word;
  ItemCount: Integer;

  procedure ReadQuickRefSection();
  var
    OldPosn: QWord;
    Posn: Integer;
    I: Integer;
  begin
    OldPosn := ChunkStream.Position;
    Posn := ChunkStream.Size-SizeOf(Word);
    ChunkStream.Position := Posn;

    ItemCount := LEToN(ChunkStream.ReadWord);
    //WriteLn('Max ITems for next block = ', ItemCount-1);
    QuickRefCount := ItemCount div (1 + (1 shl FDirectoryHeader.Density));
    //WriteLn('QuickRefCount = ' , QuickRefCount);
    SetLength(QuickRefIndex, QuickRefCount+1);
    for I := 1 to QuickRefCount do
    begin
      Dec(Posn, SizeOf(Word));
      ChunkStream.Position := Posn;
      QuickRefIndex[I] := LEToN(ChunkStream.ReadWord);
    end;
    Inc(QuickRefCount);
    ChunkStream.Position := OldPosn;
  end;

  function ReadString(StreamPosition: Integer = -1): String;
  var
    NameLength: Integer;
  begin
    if StreamPosition > -1 then ChunkStream.Position := StreamPosition;

    NameLength := GetCompressedInteger(ChunkStream);
    SetLength(Result, NameLength);
    if NameLength > 0 then
      ChunkStream.Read(PChar(Result)^, NameLength);
  end;

var
  PMGLChunk: TPMGListChunk;
  PMGIChunk: TPMGIIndexChunk;
  //ChunkStream: TMemoryStream; declared above
  Entry: TPMGListChunkEntry;
  NextIndex: Integer;
  EntryName: String;
  CRes: Integer;
  I: Integer;
begin
  Result := 0;
  //WriteLn('Looking for URL : ', Name);
  if AName = '' then Exit;
  if FDirectoryHeader.DirectoryChunkCount = 0 then Exit;

  //WriteLn('Looking for ', Name);
  if AName = FCachedEntry.Name then
    Exit(FCachedEntry.DecompressedLength); // we've already looked it up

  ChunkStream := TMemoryStream.Create;
  try

    NextIndex := FDirectoryHeader.IndexOfRootChunk;
    if NextIndex < 0 then NextIndex := 0; // no PMGI chunks

    while NextIndex > -1 do
    begin
      GetDirectoryChunk(NextIndex, ChunkStream);
      NextIndex := -1;
      ReadQuickRefSection();
      {$IFDEF CHM_DEBUG}
      WriteLn('In Block ', NextIndex);
      {$endif}
      case ChunkType(ChunkStream) of
        ctUnknown: // something is wrong
        begin
          {$IFDEF CHM_DEBUG}WriteLn(NextIndex, ' << Unknown BlockType!');{$ENDIF}
          Break;
        end;

        ctPMGI: // we must follow the PMGI tree until we reach a PMGL block
        begin
          LookupPMGIchunk(ChunkStream, PMGIChunk);

          //QuickRefIndex[0] := ChunkStream.Position;

          I := 0;
          while ChunkStream.Position <= ChunkStream.Size - PMGIChunk.UnusedSpace do
          begin;
            EntryName := ReadString();
            if EntryName = '' then Break;
            if ChunkStream.Position >= ChunkStream.Size - PMGIChunk.UnusedSpace then Break;
            CRes := ChmCompareText(AName, EntryName);
            if CRes = 0 then
            begin
              // no more need of this block. onto the next!
              NextIndex := GetCompressedInteger(ChunkStream);
              Break;
            end;
            if  CRes < 0 then
            begin
              if I = 0 then Break; // File doesn't exist
              // file is in previous entry
              Break;
            end;
            NextIndex := GetCompressedInteger(ChunkStream);
            Inc(I);
          end;
        end;

        ctPMGL:
        begin
          LookupPMGLchunk(ChunkStream, PMGLChunk);
          QuickRefIndex[0] := ChunkStream.Position;
          I := 0;
          while ChunkStream.Position <= ChunkStream.Size - PMGLChunk.UnusedSpace do
          begin
            // we consume the entry by reading it
            Entry.Name := ReadString();
            if Entry.Name = '' then Break;
            if ChunkStream.Position >= ChunkStream.Size - PMGLChunk.UnusedSpace then Break;

            Entry.ContentSection := GetCompressedInteger(ChunkStream);
            Entry.ContentOffset := GetCompressedInteger(ChunkStream);
            Entry.DecompressedLength := GetCompressedInteger(ChunkStream);

            CRes := ChmCompareText(AName, Entry.Name);
            if CRes = 0 then
            begin
              FCachedEntry := Entry;
              Result := Entry.DecompressedLength;
              Break;
            end;
            Inc(I);
          end;
        end; // case
      end;
    end;
  finally
    ChunkStream.Free();
  end;
end;

function TITSFReader.GetObject(Name: String): TMemoryStream;
begin
  Result := TMemoryStream.Create();
  if not ReadFileContent(Name, Result) then
    FreeAndNil(Result);
end;

function TITSFReader.ReadFileContent(const AName: String; AData: TStream): Boolean;
var
  TmpEntry: TPMGListChunkEntry;
begin
  Result := False;
  if not Assigned(AData) then
  begin
    ChmLastError := ERR_STREAM_NOT_ASSIGNED;
    Exit;
  end;

  if ObjectExists(AName) = 0 then
  begin
    //WriteLn('Object ', name,' Doesn''t exist or is zero sized.');
    Exit;
  end;

  // FCachedEntry now contain AName directory entry
  // FCachedEntry can change while reading!
  TmpEntry := FCachedEntry;
  Result := ReadFileContentByEntry(TmpEntry, AData);
  FCachedEntry := TmpEntry;
end;

function TITSFReader.ReadFileContentByEntry(const AEntry: TPMGListChunkEntry;
  AData: TStream): Boolean;
var
 SectionName: String;
begin
  Result := False;
  if not Assigned(AData) then Exit;

  AData.Position := 0;
  if AEntry.ContentSection = 0 then
  begin
    FStream.Position := FHeaderSuffix.Offset + AEntry.ContentOffset;
    AData.CopyFrom(FStream, AEntry.DecompressedLength);
    Result := True;
  end
  else
  begin // we have to get it from ::DataSpace/Storage/[MSCompressed,Uncompressed]/ControlData
    if FSectionNames.Count >= AEntry.ContentSection then
    begin
      FmtStr(SectionName, '::DataSpace/Storage/%s/',[FSectionNames[AEntry.ContentSection]]);
      Result := ReadBlockFromSection(SectionName, AEntry.ContentOffset, AEntry.DecompressedLength, AData);
    end;
  end;
  AData.Position := 0;
end;

procedure TITSFReader.ReadHeader();
begin
  FStream.Read(FITSFHeader, SizeOf(FITSFHeader));

  // Fix endian issues
  {$IFDEF ENDIAN_BIG}
  fITSFHeader.Version := LEtoN(fITSFHeader.Version);
  fITSFHeader.HeaderLength := LEtoN(fITSFHeader.HeaderLength);
  //Unknown_1
  fITSFHeader.TimeStamp := BEtoN(fITSFHeader.TimeStamp);//bigendian
  fITSFHeader.LanguageID := LEtoN(fITSFHeader.LanguageID);
  {$ENDIF}

  if FITSFHeader.Version < 4 then
    FStream.Seek(SizeOf(TGuid)*2, soCurrent);

  if not IsValidFile then Exit;

  ReadHeaderEntries();
end;

procedure TITSFReader.ReadHeaderEntries();
var
  HeaderEntries: array [0..1] of TITSFHeaderEntry;
begin
  // Copy EntryData into memory
  FStream.Read(HeaderEntries[0], SizeOf(HeaderEntries));

  if FITSFHeader.Version = 3 then
    FStream.Read(FHeaderSuffix.Offset, SizeOf(QWord));
  FHeaderSuffix.Offset := LEtoN(FHeaderSuffix.Offset);
  // otherwise this is set in fill directory entries

  FStream.Position := LEtoN(HeaderEntries[1].PosFromZero);
  FDirectoryHeaderPos := LEtoN(HeaderEntries[1].PosFromZero);
  FStream.Read(FDirectoryHeader, SizeOf(FDirectoryHeader));
  {$IFDEF ENDIAN_BIG}
  with FDirectoryHeader do
  begin
    Version := LEtoN(Version);
    DirHeaderLength := LEtoN(DirHeaderLength);
    //Unknown1
    ChunkSize := LEtoN(ChunkSize);
    Density := LEtoN(Density);
    IndexTreeDepth := LEtoN(IndexTreeDepth);
    IndexOfRootChunk := LEtoN(IndexOfRootChunk);
    FirstPMGLChunkIndex := LEtoN(FirstPMGLChunkIndex);
    LastPMGLChunkIndex := LEtoN(LastPMGLChunkIndex);
    //Unknown2
    DirectoryChunkCount := LEtoN(DirectoryChunkCount);
    LanguageID := LEtoN(LanguageID);
    //GUID: TGuid;
    LengthAgain := LEtoN(LengthAgain);
  end;
  {$ENDIF}
  {$IFDEF CHM_DEBUG}
  WriteLn('PMGI depth = ', FDirectoryHeader.IndexTreeDepth);
  WriteLn('PMGI Root =  ', FDirectoryHeader.IndexOfRootChunk);
  Writeln('DirCount  =  ', FDirectoryHeader.DirectoryChunkCount);
  {$ENDIF}
  FDirectoryEntriesStartPos := FStream.Position;
  FDirectoryHeaderLength := LEtoN(HeaderEntries[1].Length);
end;

procedure TITSFReader.GetSections(out Sections: TStringList);
begin
  Sections := TStringList.Create();
  ReadSections(Sections);
end;

function TITSFReader.ReadSections(ASections: TStrings): Boolean;
var
  ms: TMemoryStream;
  EntryCount: Word;
  X: Integer;
  {$IFDEF ENDIAN_BIG}
  I: Integer;
  {$ENDIF}
  WString: array [0..31] of WideChar;
  StrLength: Word;
begin
  Result := False;
  if not Assigned(ASections) then Exit;

  //WriteLn('::DataSpace/NameList Size = ', ObjectExists('::DataSpace/NameList'));
  ms := TMemoryStream.Create();
  try
    if ReadFileContent('::DataSpace/NameList', ms) then
    begin
      ms.Position := 2;
      EntryCount := LEtoN(ms.ReadWord);
      for X := 0 to EntryCount-1 do
      begin
        StrLength := LEtoN(ms.ReadWord);
        if StrLength > 31 then
          StrLength := 31;
        ms.Read(WString, SizeOf(WideChar)*(StrLength+1)); // the strings are stored null terminated
        {$IFDEF ENDIAN_BIG}
        for I := 0 to StrLength-1 do
          WString[I] := WideChar(LEtoN(Ord(WString[I])));
        {$ENDIF}
        ASections.Add(WString);
      end;
      Result := True;
    end;
  finally
    ms.Free();
  end;
end;

function TITSFReader.ReadBlockFromSection(SectionPrefix: String;
  StartPos: QWord; BlockLength: QWord; AData: TStream): Boolean;
var
  Compressed: Boolean;
  Sig: Array [0..3] of char;
  CompressionVersion: LongWord;
  CompressedSize: QWord;
  UnCompressedSize: QWord;
  //LZXResetInterval: LongWord;
  //LZXWindowSize: LongWord;
  //LZXCacheSize: LongWord;
  ResetTableEntry: TPMGListChunkEntry;
  ResetTable: TLZXResetTableArr;
  WriteCount: QWord;
  BlockWriteLength: QWord;
  WriteStart: LongWord;
  ReadCount: LongInt;
  LZXState: PLZXState;
  InBuf: array of Byte;
  OutBuf: array of Byte;
  //OutBuf: PByte;
  BlockSize: QWord;
  X: Integer;
  FirstBlock, LastBlock: LongInt;
  ResultCode: LongInt;

  procedure ReadBlock();
  begin
    if ReadCount > Length(InBuf) then
      SetLength(InBuf, ReadCount);
    FStream.Read(InBuf[0], ReadCount);
  end;

begin
  // okay now the fun stuff ;)
  Result := False;
  if not Assigned(AData) then Exit;
  AData.Position := 0;

  Compressed := ObjectExists(SectionPrefix+'Transform/{7FC28940-9D31-11D0-9B27-00A0C91E9C7C}/InstanceData/ResetTable') > 0;
  // the easy method
  if (not Compressed) then
  begin
    if ObjectExists(SectionPrefix+'Content') > 0 then
    begin
      FStream.Position := FHeaderSuffix.Offset + FCachedEntry.ContentOffset + StartPos;
      AData.CopyFrom(FStream, BlockLength);
      Result := True;
    end;
    Exit;
  end
  else
    ResetTableEntry := FCachedEntry;

  // First make sure that it is a compression we can read
  if ObjectExists(SectionPrefix+'ControlData') > 0 then
  begin
    FStream.Position := FHeaderSuffix.Offset + FCachedEntry.ContentOffset + 4;
    FStream.Read(Sig, 4);
    if Sig <> 'LZXC' then Exit;

    CompressionVersion := LEtoN(FStream.ReadDWord);
    if CompressionVersion > 2 then exit;
    {LZXResetInterval := }LEtoN(FStream.ReadDWord);
    {LZXWindowSize := }LEtoN(FStream.ReadDWord);
    {LZXCacheSize := }LEtoN(FStream.ReadDWord);

    BlockSize := FindBlocksFromUnCompressedAddr(ResetTableEntry, CompressedSize, UnCompressedSize, ResetTable);
    if UncompressedSize > 0 then ; // to avoid a compiler note
    if StartPos > 0 then
      FirstBlock := StartPos div BlockSize
    else
      FirstBlock := 0;
    LastBlock := (StartPos+BlockLength) div BlockSize;

    if ObjectExists(SectionPrefix+'Content') = 0 then Exit;

    //WriteLn('Compressed Data start''s at: ', FHeaderSuffix.Offset + FCachedEntry.ContentOffset,' Size is: ', FCachedEntry.DecompressedLength);
    AData.Size := BlockLength;
    AData.Position := 0;  // some TStream descendants changes Position after resizing
    SetLength(InBuf, BlockSize);
    //OutBuf := GetMem(BlockSize);
    SetLength(OutBuf, BlockSize);
    // First Init a PLZXState
    LZXState := LZXinit(16);
    try
      if LZXState <> nil then
      begin
        // if FirstBlock is odd (1,3,5,7 etc) we have to read the even block before it first.
        if FirstBlock and 1 = 1 then
        begin
          FStream.Position := FHeaderSuffix.Offset + FCachedEntry.ContentOffset + (ResetTable[FirstBLock-1]);
          ReadCount := ResetTable[FirstBlock] - ResetTable[FirstBlock-1];
          BlockWriteLength := BlockSize;
          ReadBlock();
          ResultCode := LZXdecompress(LZXState, @InBuf[0], @OutBuf[0], ReadCount, LongInt(BlockWriteLength));
        end;

        // now start the actual decompression loop
        for X := FirstBlock to LastBlock do
        begin
          FStream.Position := FHeaderSuffix.Offset + FCachedEntry.ContentOffset + (ResetTable[X]);

          if X = FirstBLock then
            WriteStart := StartPos - (X*BlockSize)
          else
            WriteStart := 0;

          if X = High(ResetTable) then
            ReadCount := CompressedSize - ResetTable[X]
          else
            ReadCount := ResetTable[X+1] - ResetTable[X];

          BlockWriteLength := BlockSize;

          if FirstBlock = LastBlock then
            WriteCount := BlockLength
          else if X = LastBlock then
            WriteCount := (StartPos+BlockLength) - (X*BlockSize)
          else
            WriteCount := BlockSize - WriteStart;

          ReadBlock();
          ResultCode := LZXdecompress(LZXState, @InBuf[0], @OutBuf[0], ReadCount, LongInt(BlockWriteLength));

          //now write the decompressed data to the stream
          if ResultCode = DECR_OK then
          begin
            AData.Write(OutBuf[WriteStart], QWord(WriteCount));
            Result := True;
          end
          else
          begin
            {$IFDEF CHM_DEBUG} // windows gui program will cause an exception with writeln's
            WriteLn('Decompress FAILED with error code: ', ResultCode);
            {$ENDIF}
            Result := False;
            AData.Size := 0;
            Break;
          end;

          // if the next block is an even numbered block we have to reset the decompressor state
          if (X < LastBlock) and (X and 1 = 1) then
            LZXreset(LZXState);
        end;
      end;

    finally
      //FreeMem(OutBuf);
      SetLength(ResetTable, 0);
      LZXteardown(LZXState);
    end;
  end;
end;

function TITSFReader.FindBlocksFromUnCompressedAddr(const ResetTableEntry: TPMGListChunkEntry;
  out CompressedSize: QWord; out UnCompressedSize: QWord; out LZXResetTable: TLZXResetTableArr): QWord;
type
  TResetTableEntry = packed record
    Uncnown0: DWord;
    BlockCount: DWord;
    Uncnown1: DWord;
    TableHeaderSize: DWord;
    UnCompressedSize: QWord;
    CompressedSize: QWord;
    BlockSize: QWord;
  end;

var
  BlockCount: LongWord;
  rte: TResetTableEntry;
  {$IFDEF ENDIAN_BIG}
  I: Integer;
  {$ENDIF}
begin
  Result := 0;
  FStream.Position := FHeaderSuffix.Offset + ResetTableEntry.ContentOffset;
  {FStream.ReadDWord;
  BlockCount := LEtoN(FStream.ReadDWord);
  FStream.ReadDWord;
  FStream.ReadDWord; // TableHeaderSize;
  FStream.Read(UnCompressedSize, SizeOf(QWord));
  UnCompressedSize := LEtoN(UnCompressedSize);
  FStream.Read(CompressedSize, SizeOf(QWord));
  CompressedSize := LEtoN(CompressedSize);
  FStream.Read(Result, SizeOf(QWord)); // block size
  Result := LEtoN(Result); }

  FStream.Read(rte, SizeOf(rte));
  BlockCount := LEtoN(rte.BlockCount);
  UnCompressedSize := LEtoN(rte.UnCompressedSize);
  CompressedSize := LEtoN(rte.CompressedSize);
  Result := LEtoN(rte.BlockSize);

  // now we are located at the first block index

  SetLength(LZXResetTable, BlockCount);
  FStream.Read(LZXResetTable[0], SizeOf(QWord)*BlockCount);
  {$IFDEF ENDIAN_BIG}
  for I := 0 to High(LZXResetTable) do
    LZXResetTable[I] := LEtoN(LZXResetTable[I]);
  {$ENDIF}
end;

procedure TITSFReader.ForEachHandler(Name: String; Offset, UncompressedSize,
  Section: Integer);
begin
  if Assigned(FTmpStrings) then
    FTmpStrings.Append(Name);
end;

{ TChmReader }

constructor TChmReader.Create(AStream: TStream; FreeStreamOnDestroy: Boolean);
begin
  FContextList := TContextList.Create;
  FWindowsList := TCHMWindowList.Create();
  FWindowsList.OwnsObjects := True;
  FTOPICSStream := TMemoryStream.Create();
  FURLSTRStream := TMemoryStream.Create();
  FURLTBLStream := TMemoryStream.Create();
  FStringsStream := TMemoryStream.Create();
  FDefaultWindow := '';

  inherited Create(AStream, FreeStreamOnDestroy);
  if not IsValidFile then Exit;

  ReadCommonData();
end;

destructor TChmReader.Destroy;
begin
  FreeAndNil(FContextList);
  FreeAndNil(FWindowsList);
  if Assigned(FSearchReader) then
    FreeAndNil(FSearchReader);
  FreeAndNil(FTOPICSStream);
  FreeAndNil(FURLSTRStream);
  FreeAndNil(FURLTBLStream);
  FreeAndNil(FStringsStream);
  inherited Destroy;
end;

procedure TChmReader.ReadSectionWindows();
var
  msWindows: TMemoryStream;
  EntryCount,
  EntrySize: DWord;
  EntryStart: QWord;
  X: Integer;
  OffSet: QWord;
begin
  msWindows := TMemoryStream.Create();
  try
    ReadFileContent('/#WINDOWS', msWindows);
    if (msWindows.Size > (SizeOf(DWord) * 2)) then
    begin
      EntryCount := LEtoN(msWindows.ReadDWord);
      EntrySize := LEtoN(msWindows.ReadDWord);
      OffSet := msWindows.Position;
      for X := 0 to EntryCount -1 do
      begin
        EntryStart := OffSet + (X*EntrySize);
        if FTitle = '' then
        begin
          msWindows.Position := EntryStart + $14;
          FTitle := '/'+ReadStringsEntry(LEtoN(msWindows.ReadDWord));
        end;
        if FTOCFile = '' then
        begin
          msWindows.Position := EntryStart + $60;
          FTOCFile := '/'+FixUrl(ReadStringsEntry(LEtoN(msWindows.ReadDWord)));
        end;
        if FIndexFile = '' then
        begin
          msWindows.Position := EntryStart + $64;
          FIndexFile := '/'+FixUrl(ReadStringsEntry(LEtoN(msWindows.ReadDWord)));
        end;
        if FDefaultPage = '' then
        begin
          msWindows.Position := EntryStart + $68;
          FDefaultPage := '/'+FixUrl(ReadStringsEntry(LEtoN(msWindows.ReadDWord)));
        end;
      end;
      ReadWindows(msWindows);
    end;
  finally
    msWindows.Free();
  end;
end;

procedure TChmReader.ReadSectionSystem();
var
  //Version: DWord;
  EntryType: Word;
  EntryLength: Word;
  Data: array[0..511] of char;
  ms: TMemoryStream;
  Tmp: String;
begin
  ms := TMemoryStream.Create();
  try
    if ReadFileContent('/#SYSTEM', ms) and (ms.Size >= SizeOf(DWord)) then
    begin
      ms.Position := 0;
      {Version := }LEtoN(ms.ReadDWord);
      while ms.Position < ms.Size do
      begin
        EntryType := LEtoN(ms.ReadWord);
        EntryLength := LEtoN(ms.ReadWord);
        case EntryType of
          0: // Table of contents
          begin
            if EntryLength > 511 then EntryLength := 511;
            ms.Read(Data[0], EntryLength);
            Data[EntryLength] := #0;
            FTOCFile := '/'+Data;
          end;
          1: // Index File
          begin
            if EntryLength > 511 then EntryLength := 511;
            ms.Read(Data[0], EntryLength);
            Data[EntryLength] := #0;
            FIndexFile := '/'+Data;
          end;
          2: // DefaultPage
          begin
            if EntryLength > 511 then EntryLength := 511;
            ms.Read(Data[0], EntryLength);
            Data[EntryLength] := #0;
            FDefaultPage := '/'+Data;
          end;
          3: // Title of chm
          begin
            if EntryLength > 511 then EntryLength := 511;
            ms.Read(Data[0], EntryLength);
            Data[EntryLength] := #0;
            FTitle := Data;
          end;
          4: // Locale ID
          begin
            FLocaleID := LEtoN(ms.ReadDWord);
            ms.Position := (ms.Position + EntryLength) - SizeOf(DWord);
          end;
          6: // chm file name. use this to get the index and toc name
          begin
            if EntryLength > 511 then EntryLength := 511;
            ms.Read(Data[0], EntryLength);
            Data[EntryLength] := #0;
            if (FIndexFile = '') then
            begin
              Tmp := '/'+Data+'.hhk';
              if (ObjectExists(Tmp) > 0) then
              begin
                FIndexFile := Tmp;
              end;
            end;
            if (FTOCFile = '') then
            begin
              Tmp := '/'+Data+'.hhc';
              if (ObjectExists(Tmp) > 0) then
              begin
                FTOCFile := Tmp;
              end;
            end;
          end;
          16: // Prefered font
          begin
            if EntryLength > 511 then EntryLength := 511;
            ms.Read(Data[0], EntryLength);
            Data[EntryLength] := #0;
            FPreferedFont := Data;
            // todo:  the first number is the point size & the last number is the character set
          end;
        else
          // Skip entries we are not interested in
          ms.Position := ms.Position + EntryLength;
        end;
      end;
    end;
  finally
    ms.Free();
  end;
end;

procedure TChmReader.ReadSectionIvb();
var
  msIVB: TMemoryStream;
  Str: String;
  Value: DWord;
  OffSet: DWord;
  //TotalSize: DWord;
begin
  msIVB := TMemoryStream.Create();
  try
    if ReadFileContent('/#IVB', msIVB) and (msIVB.Size > SizeOf(DWord)) then
    begin;
      msIVB.Position := 0;
      {TotalSize := }LEtoN(msIVB.ReadDWord);
      while msIVB.Position < msIVB.Size do
      begin
        Value := LEtoN(msIVB.ReadDWord);
        OffSet := LEtoN(msIVB.ReadDWord);
        Str := '/' + FixUrl(ReadStringsEntry(Offset));
        FContextList.AddContext(Value, Str);
      end;
    end;
  finally
    msIVB.Free();
  end;
end;

procedure TChmReader.ReadCommonData();
begin
  ReadFileContent('/#STRINGS', FStringsStream);
  ReadFileContent('/#TOPICS', FTOPICSStream);
  ReadFileContent('/#URLSTR', FURLSTRStream);
  ReadFileContent('/#URLTBL', FURLTBLStream);
  ReadSectionSystem();
  ReadSectionWindows();
  ReadSectionIvb();
  {$IFDEF CHM_DEBUG}
  WriteLn('TOC=',fTocfile);
  WriteLn('DefaultPage=',fDefaultPage);
  {$ENDIF}
end;

function TChmReader.ReadStringsEntry(APosition: DWord): String;
var
  buf: array[0..49] of Char;
begin
  Result := '';
  if APosition < FStringsStream.Size-1 then
  begin
    FStringsStream.Position := APosition;
    repeat
      FStringsStream.Read(buf, SizeOf(buf));
      Result := Result + buf;
    until IndexByte(buf, 50, 0) <> -1;
    //Result := PChar(FStringsStream.Memory + APosition); // unsafe
  end;
end;

function TChmReader.ReadStringsEntryFromStream (strm: TStream): String;
var
  APosition : DWord;
begin
  APosition := LEtoN(strm.ReadDWord);
  result := ReadStringsEntry(APosition);
end;

function TChmReader.ReadURLSTR(APosition: DWord): String;
var
  URLOffset: DWord;
begin
  if FURLTBLStream.Size = 0 then
    Exit;

  FURLTBLStream.Position := APosition;
  FURLTBLStream.ReadDWord; // unknown
  FURLTBLStream.ReadDWord; // TOPIC index #
  URLOffset := LEtoN(FURLTBLStream.ReadDWord);

  FURLSTRStream.Position := URLOffset;
  FURLSTRStream.ReadDWord; // URL
  FURLSTRStream.ReadDWord; // FrameName
  // LocalUrl
  if FURLSTRStream.Position < FURLSTRStream.Size-1 then
    Result := PChar(FURLSTRStream.Memory + FURLSTRStream.Position);
end;

function TChmReader.CheckCommonStreams(): Boolean;
begin
  Result := (FTOPICSStream.Size > 0) and (FURLSTRStream.Size > 0) and (FURLTBLStream.Size > 0);
end;

procedure TChmReader.ReadWindows(mem: TMemoryStream);
var
  i, cnt, version: integer;
  x: TChmWindow;
begin
  FWindowsList.Clear();
  mem.Position:=0;
  cnt := LEtoN(mem.ReadDWord);
  version := LEtoN(mem.ReadDWord);
  while (cnt > 0) do
  begin
    x := TChmWindow.Create();
    version            := LEtoN(mem.ReadDWord);                        //  0 size of entry.
    mem.readDWord;                                                     //  4 unknown (bool Unicodestrings?)
    x.window_type      := ReadStringsEntryFromStream(mem);             //  8 Arg 0, name of window
    x.flags            := TValidWindowFields(LEtoN(mem.ReadDWord));    //  C valid fields
    x.nav_style        := LEtoN(mem.ReadDWord);                        // 10 arg 10 navigation pane style
    x.title_bar_text   := ReadStringsEntryFromStream(mem);             // 14 Arg 1,  title bar text
    x.styleflags       := LEtoN(mem.ReadDWord);                        // 18 Arg 14, style flags
    x.xtdstyleflags    := LEtoN(mem.ReadDWord);                        // 1C Arg 15, xtd style flags
    x.left             := LEtoN(mem.ReadDWord);                        // 20 Arg 13, rect.left
    x.right            := LEtoN(mem.ReadDWord);                        // 24 Arg 13, rect.top
    x.top              := LEtoN(mem.ReadDWord);                        // 28 Arg 13, rect.right
    x.bottom           := LEtoN(mem.ReadDWord);                        // 2C Arg 13, rect.bottom
    x.window_show_state:= LEtoN(mem.ReadDWord);                        // 30 Arg 16, window show state
    mem.ReadDWord;                                                     // 34  -    , HWND hwndhelp                OUT: window handle"
    mem.ReadDWord;                                                     // 38  -    , HWND hwndcaller              OUT: who called this window"
    mem.ReadDWord;                                                     // 3C  -    , HH_INFO_TYPE paINFO_TYPES    IN: Pointer to an array of Information Types"
    mem.ReadDWord;                                                     // 40  -    , HWND hwndtoolbar             OUT: toolbar window in tri-pane window"
    mem.ReadDWord;                                                     // 44  -    , HWND hwndnavigation          OUT: navigation window in tri-pane window"
    mem.ReadDWord;                                                     // 48  -    , HWND hwndhtml                OUT: window displaying HTML in tri-pane window"
    x.navpanewidth     := LEtoN(mem.ReadDWord);                        // 4C Arg 11, width of nav pane
    mem.ReadDWord;                                                     // 50  -    , rect.left,   OUT:Specifies the coordinates of the Topic pane
    mem.ReadDWord;                                                     // 54  -    , rect.top ,   OUT:Specifies the coordinates of the Topic pane
    mem.ReadDWord;                                                     // 58  -    , rect.right,  OUT:Specifies the coordinates of the Topic pane
    mem.ReadDWord;                                                     // 5C  -    , rect.bottom, OUT:Specifies the coordinates of the Topic pane
    x.toc_file         := ReadStringsEntryFromStream(mem);             // 60 Arg 2,  toc file
    x.index_file       := ReadStringsEntryFromStream(mem);             // 64 Arg 3,  index file
    x.default_file     := ReadStringsEntryFromStream(mem);             // 68 Arg 4,  default file
    x.home_button_file := ReadStringsEntryFromStream(mem);             // 6c Arg 5,  home button file.
    x.buttons          := LEtoN(mem.ReadDWord);                        // 70 arg 12,
    x.navpane_initially_closed    := LEtoN(mem.ReadDWord);             // 74 arg 17
    x.navpane_default  := LEtoN(mem.ReadDWord);                        // 78 arg 18,
    x.navpane_location := LEtoN(mem.ReadDWord);                        // 7C arg 19,
    x.wm_notify_id     := LEtoN(mem.ReadDWord);                        // 80 arg 20,
    for i:=0 to 4 do
      mem.ReadDWord;                                                   // 84  -      byte[20] unknown -  "BYTE tabOrder[HH_MAX_TABS + 1]; // IN/OUT: tab order: Contents, Index, Search, History, Favorites, Reserved 1-5, Custom tabs"
    mem.ReadDWord;                                                     // 94  -      int cHistory; // IN/OUT: number of history items to keep (default is 30)
    x.jumpbutton_1_text:= ReadStringsEntryFromStream(mem);             // 9C Arg 7,  The text of the Jump 1 button.
    x.jumpbutton_2_text:= ReadStringsEntryFromStream(mem);             // A0 Arg 9,  The text of the Jump 2 button.
    x.jumpbutton_1_file:= ReadStringsEntryFromStream(mem);             // A4 Arg 6,  The file shown for Jump 1 button.
    x.jumpbutton_2_file:= ReadStringsEntryFromStream(mem);             // A8 Arg 8,  The file shown for Jump 1 button.
    for i:=0 to 3 do
     mem.ReadDWord;
    Dec(version, 188);                                              // 1.1 specific onesf
    while (version >= 4) do
    begin
      mem.ReadDWord;
      Dec(version, 4);
    end;

    FWindowsList.Add(x);
    Dec(cnt);
  end;
end;

function TChmReader.GetContextUrl(Context: THelpContext): String;
begin
  // will get '' if context not found
 Result := FContextList.GetURL(Context);
end;

function TChmReader.GetTopicsCount: Integer;
begin
  Result := FTOPICSStream.Size div 16;
end;

function TChmReader.LookupTopicByID(ATopicID: Integer; out ATitle: String): String;
var
  TopicURLTBLOffset: DWord;
  TopicTitleOffset: DWord;
begin
  Result := '';
  ATitle := '';
  //WriteLn('Getting topic# ',ATopicID);
  if not CheckCommonStreams then
    Exit;
  FTOPICSStream.Position := ATopicID * 16;
  if FTOPICSStream.Position = ATopicID * 16 then
  begin
    FTOPICSStream.ReadDWord;
    TopicTitleOffset := LEtoN(FTOPICSStream.ReadDWord);
    TopicURLTBLOffset := LEtoN(FTOPICSStream.ReadDWord);
    if TopicTitleOffset <> $FFFFFFFF then
      ATitle := ReadStringsEntry(TopicTitleOffset);
     //WriteLn('Got a title: ', ATitle);
    Result := ReadURLSTR(TopicURLTBLOffset);
  end;
end;

const DefBlockSize = 2048;

function LoadBtreeHeader(m: TStream; var BtreeHdr: TBtreeHeader): Boolean;
begin
  if m.Size < SizeOf(TBtreeHeader) Then
    Exit(False);
  Result := True;
  m.Read(BtreeHdr, SizeOf(TBtreeHeader));
  {$IFDEF ENDIAN_BIG}
     BtreeHdr.Flags         := LEToN(BtreeHdr.Flags);
     BtreeHdr.BlockSize     := LEToN(BtreeHdr.BlockSize);
     BtreeHdr.LastLstBlock  := LEToN(BtreeHdr.LastLstBlock);
     BtreeHdr.IndexRootBlock:= LEToN(BtreeHdr.IndexRootBlock);
     BtreeHdr.NrBlock       := LEToN(BtreeHdr.NrBlock);
     BtreeHdr.TreeDepth     := LEToN(BtreeHdr.TreeDepth);
     BtreeHdr.NrKeyWords    := LEToN(BtreeHdr.NrKeyWords);
     BtreeHdr.CodePage      := LEToN(BtreeHdr.CodePage);
     BtreeHdr.LCID          := LEToN(BtreeHdr.LCID);
     BtreeHdr.IsChm         := LEToN(BtreeHdr.IsChm);
  {$endif}
end;

function ReadWCharString(var head: PByte; tail: PByte; var readv: AnsiString): Boolean;
var
  pw: PWord;
  oldHead: PByte;
  ws: WideString;
  n: Integer;
begin
  oldHead := head;
  pw := PWord(head);
  while (pw < PWord(tail)) and (pw^ <> word(0)) do
    Inc(pw);
  Inc(pw); // skip #0#0.
  head := PByte(pw);
  Result := head < tail;

  n := head - oldHead;
  SetLength(ws, n div SizeOf(widechar));
  Move(oldHead^, ws[1], n);
  for n:=1 to Length(ws) do
    Word(ws[n]) := LEToN(Word(ws[n]));
  readv := ws; // force conversion for now, and hope it doesn't require cwstring
end;

procedure CreateSiteMapEntry(SiteMap: TChmSiteMap; var Item: TChmSiteMapItem; const Name: AnsiString; CharIndex: Integer; const Topic, Title: AnsiString);
var
  SubItem: TChmSiteMapItem;
  sShortName: AnsiString;
  sLongPart: AnsiString;
begin
  if CharIndex = 0 then
  begin
    Item := SiteMap.Items.NewItem();
    Item.KeyWord := Name;
    Item.Local := Topic;
    Item.Text := Title;
  end
  else
  begin
    sShortName := Copy(Name, 1, CharIndex-2);
    sLongPart := Copy(Name, CharIndex, Length(Name)-CharIndex+1);
    if Assigned(Item) and (sShortName = Item.Text) then
    begin
      SubItem := Item.Children.NewItem();
      SubItem.Local := Topic;
      SubItem.KeyWord := sLongPart; // recursively split this? No examples.
      SubItem.Text := Title;
    end
    else
    begin
      Item := SiteMap.Items.NewItem();
      Item.KeyWord := sShortName;
      Item.Local := Topic;
      Item.Text := Title;

      SubItem := Item.Children.NewItem();
      SubItem.KeyWord := sLongPart;
      SubItem.Local := Topic;
      SubItem.Text := Title; // recursively split this? No examples.
    end;
  end;
end;

procedure TChmReader.ParseSiteMapListingBlock(SiteMap: TChmSiteMap; p: PByte);
var
  hdr: PBTreeBlockHeader;
  head, tail: PByte;
  IsSeeAlso, nrPairs: Integer;
  i: Integer;
  PE: PBtreeBlockEntry;
  sTitle: string;
  CharIndex, ind: Integer;
  EntryDepth: Word;
  SeeAlsoStr, Topic, Name: AnsiString;
begin
  //setlength (curitem,10);
  hdr := PBTreeBlockHeader(p);
  hdr^.Length           := LEToN(hdr^.Length);
  hdr^.NumberOfEntries  := LEToN(hdr^.NumberOfEntries);
  hdr^.IndexOfPrevBlock := LEToN(hdr^.IndexOfPrevBlock);
  hdr^.IndexOfNextBlock := LEToN(hdr^.IndexOfNextBlock);

  tail := p + (2048 - hdr^.Length);
  head := p + SizeOf(TBtreeBlockHeader);

  {$ifdef binindex}
  WriteLn('previndex  : ',hdr^.IndexOfPrevBlock);
  WriteLn('nextindex  : ',hdr^.IndexOfNextBlock);
  {$endif}
  while head < tail do
  begin
    if not ReadWCharString(Head, Tail, Name) Then
      Break;
    {$ifdef binindex}
    Writeln('name : ',name);
    {$endif}
    if (head + SizeOf(TBtreeBlockEntry)) >= tail then
      Break;
    PE := PBtreeBlockEntry(head);
    NrPairs    := LEToN(PE^.NrPairs);
    IsSeealso  := LEToN(PE^.IsSeeAlso);
    EntryDepth := LEToN(PE^.EntryDepth);
    CharIndex  := LEToN(PE^.CharIndex);
    {$ifdef binindex}
    Writeln('seealso   :  ', IsSeeAlso);
    Writeln('entrydepth:  ', EntryDepth);
    Writeln('Nrpairs   :  ', NrPairs);
    Writeln('CharIndex :  ', CharIndex);
    {$endif}

    Inc(head, SizeOf(TBtreeBlockEntry));
    if IsSeeAlso > 0 then
    begin
      if not ReadWCharString(Head, Tail, SeeAlsoStr) Then
        Break;
      // have to figure out first what to do with it.
      // is See Also really mutually exclusive with pairs?
      // or is the number of pairs equal to the number of seealso
      // strings?
      {$ifdef binindex}
      WriteLn('seealso: ', SeeAlsoStr);
      {$endif}
    end
    else
    begin
      if NrPairs > 0 then
      begin
        {$ifdef binindex}
        WriteLn('Pairs   : ');
        {$endif}

        for i:=0 to nrPairs-1 do
        begin
          if head < tail then
          begin
            ind := LEToN(PLongint(head)^);
            Topic := LookupTopicByID(ind, sTitle);
            {$ifdef binindex}
              WriteLn(i:3, ' topic: ', Topic);
              WriteLn('    title: ', sTitle);
            {$endif}
            Inc(head, 4);
          end;
        end;
      end;
    end;
    if nrPairs <> 0 then
      CreateSiteMapEntry(SiteMap, FLastSiteMapItem, Name, CharIndex, Topic, sTitle);
    Inc(head, 4); // always 1
    {$ifdef binindex}
    if head < tail then
      WriteLn('Zero based index (13 higher than last) :', PLongint(head)^);
    {$endif}
    Inc(head, 4); // zero based index (13 higher than last
  end;
end;

function TChmReader.GetIndexSitemap(ForceXML: Boolean = False): TChmSiteMap;
begin
  Result := TChmSitemap.Create(StIndex);
  if not ReadIndexSitemap(Result, ForceXML) then
    FreeAndNil(Result);
end;

function TChmReader.ReadIndexSitemap(SiteMap: TChmSiteMap; ForceXML: Boolean
  ): Boolean;
var
  msIndex: TMemoryStream;
  BHdr: TBTreeHeader;
  block: array[0..2047] of Byte;
  i: Integer;
begin
  Result := False;
  if not Assigned(SiteMap) or (SiteMap.SiteMapType <> StIndex) then Exit;
  // First Try Binary
  msIndex := TMemoryStream.Create();
  try
    if (not ReadFileContent('/$WWKeywordLinks/BTree', msIndex)) then
    begin
      ForceXML := True;
    end;
    if not CheckCommonStreams then
    begin
      ForceXML := True;
    end;

    if not ForceXML then
    begin
      ForceXML := True;
      //SiteMap := TChmSitemap.Create(StIndex);
      FLastSiteMapItem := nil;  // cached last created item, in case we need to make a child.

      BHdr.LastLstBlock:=0;
      if LoadBtreeHeader(msIndex, BHdr) then
      begin
        if BHdr.BlockSize = DefBlockSize then
        begin
          for i:=0 to BHdr.LastLstBlock do
          begin
            if (msIndex.Size - msIndex.Position) >= DefBlockSize then
            begin
              msIndex.Read(block, DefBlockSize);
              ParseSiteMapListingBlock(SiteMap, @block);
            end;
          end;
          ForceXML := False;
          Result := True;
        end;
      end;
    end;

    if ForceXML then
    begin
      msIndex.Size := 0;
      // Second Try text Index
      if ReadFileContent(IndexFile, msIndex) then
      begin
        SiteMap.LoadFromStream(msIndex);
        Result := True;
      end;
    end;
  finally
    msIndex.Free();
  end;
end;

function TChmReader.ReadTopicList(ATopicList: TChmTopicItemList): Boolean;
var
  i: Integer;
  sTitle, sLocalUrl: string;
begin
  if not Assigned(ATopicList) then
    Exit;
  for i := 0 to GetTopicsCount()-1 do
  begin
    sLocalUrl := LookupTopicByID(i, sTitle);
    ATopicList.AddItem(sTitle, sLocalUrl);
  end;
  Result := (ATopicList.Count > 0);
end;

function TChmReader.ReadTOCSitemap(SiteMap: TChmSiteMap; ForceXML: Boolean
  ): Boolean;

  function AddTOCItem(AStream: TStream; AItemOffset: DWord; SiteMapITems: TChmSiteMapItems): DWord;
  var
    Props: DWord;
    Item: TChmSiteMapItem;
    NextEntry: DWord;
    TopicsIndex: DWord;
    Title: String;
  begin
    AStream.Position:= AItemOffset + 4;
    Item := SiteMapITems.NewItem();
    Props := LEtoN(AStream.ReadDWord());
    if (Props and TOC_ENTRY_HAS_LOCAL) = 0 then
      Item.Text:= ReadStringsEntry(LEtoN(AStream.ReadDWord()))
    else
    begin
      TopicsIndex := LEtoN(AStream.ReadDWord());
      Item.Local := LookupTopicByID(TopicsIndex, Title);
      Item.Text := Title;
    end;
    AStream.ReadDWord();
    Result := LEtoN(AStream.ReadDWord());
    if Props and TOC_ENTRY_HAS_CHILDREN > 0 then
    begin
      NextEntry := LEtoN(AStream.ReadDWord());
      repeat
        NextEntry := AddTOCItem(AStream, NextEntry, Item.Children);
      until NextEntry = 0;
    end;
  end;

var
  msTOC: TMemoryStream;
  //TOPICSOffset: DWord;
  //EntriesOffset: DWord;
  EntryCount: DWord;
  EntryInfoOffset: DWord;
  NextItem: DWord;
begin
  Result := False;
  if (not Assigned(SiteMap)) or (SiteMap.SiteMapType <> stTOC) then Exit;

  msTOC := TMemoryStream.Create();
  try
    // First Try Binary
    if (not ReadFileContent('/#TOCIDX', msTOC)) then
    begin
      ForceXML := True;
    end;

    // TOPICS URLSTR URLTBL must all exist to read binary mstoc
    // if they don't then try text file
    if not CheckCommonStreams then
    begin
      ForceXML := True;
    end;

    if not ForceXML then
    begin
      // Binary msToc Exists
      EntryInfoOffset := NtoLE(msTOC.ReadDWord);
      {EntriesOffset   :=} NtoLE(msTOC.ReadDWord);
      EntryCount      := NtoLE(msTOC.ReadDWord);
      {TOPICSOffset    :=} NtoLE(msTOC.ReadDWord);

      if EntryCount > 0 then
      begin
        //Result := TChmSiteMap.Create(stTOC);
        NextItem := EntryInfoOffset;
        repeat
          NextItem := AddTOCItem(msToc, NextItem, SiteMap.Items);
        until NextItem = 0;
        Result := True;
      end;
    end;

    if ForceXML then
    begin
      msToc.Size := 0;
      // Second Try text mstoc
      if ReadFileContent(TOCFile, msTOC) then
      begin
        SiteMap.LoadFromStream(msTOC);
        Result := True;
      end;
    end;

  finally
    msTOC.Free();
  end;
end;

function TChmReader.GetTOCSitemap(ForceXML: Boolean = False): TChmSiteMap;
begin
  Result := TChmSiteMap.Create(stTOC);
  if not ReadTOCSitemap(Result, ForceXML) then
    FreeAndNil(Result);
end;

function TChmReader.HasContextList: Boolean;
begin
  Result := FContextList.Count > 0;
end;

{ TContextList }

procedure TContextList.AddContext(Context: THelpContext; const Url: String);
var
  ContextItem: PContextItem;
begin
  New(ContextItem);
  Add(ContextItem);
  ContextItem^.Context := Context;
  ContextItem^.Url := Url;
end;

function TContextList.GetURL(Context: THelpContext): String;
var
  X: Integer;
begin
  Result := '';
  for X := 0 to Count-1 do
  begin
    if PContextItem(Get(X))^.Context = Context then
    begin
      Result := PContextItem(Get(X))^.Url;
      Exit;
    end;
  end;
end;

procedure TContextList.Clear();
var
  X: Integer;
begin
  for X := Count-1 downto 0 do
  begin
    Dispose(PContextItem(Get(X)));
    Delete(X);
  end;
end;


{ TChmFileList }

constructor TChmFileList.Create(PrimaryFileName: String);
begin
  inherited Create;
  FUnNotifiedFiles := TList.Create();
  OpenChmFile(PrimaryFileName);
end;

destructor TChmFileList.Destroy();
begin
  FreeAndNil(FUnNotifiedFiles);
  inherited Destroy;
end;

procedure TChmFileList.Delete(Index: Integer);
begin
  ChmReaders[Index].Free();
  inherited Delete(Index);
end;

function TChmFileList.GetChmReader(AIndex: Integer): TChmReader;
begin
  if (AIndex < 0) or (AIndex >= Count) then
    Result := FLastChm
  else
    Result := TChmReader(Objects[AIndex]);
end;

function TChmFileList.GetFileName(AIndex: Integer): String;
begin
  if (AIndex < 0) or (AIndex >= Count) then
    AIndex := IndexOfObject(FLastChm);

  Result := Strings[AIndex];
end;

procedure TChmFileList.OpenNewFile(AFileName: String);
begin
  OpenChmFile(AFileName);
end;

function TChmFileList.CheckOpenFile(AFileName: String): Boolean;
begin
  Result := OpenChmFile(AFileName);
end;

function TChmFileList.OpenChmFile(AFileName: String): Boolean;
var
  i: Integer;
  sFileNameOnly: string;
  TmpStream: TFileStream;
  ChmReader: TChmReader;
begin
  Result := False;
  sFileNameOnly := ExtractFileName(AFileName);
  for i := 0 to Count-1 do
  begin
    if ExtractFileName(FileName[i]) = sFileNameOnly then
    begin
      FLastChm := ChmReaders[i];
      Result := True;
      Exit;
    end;
  end;
  if not Result then
  begin
    if not FileExists(AFileName) and (Count > 0) then
    begin
      // filename relative to first opened chm
      AFileName := ExtractFilePath(FileName[0]) + sFileNameOnly;
    end;
    if FileExists(AFileName) and (LowerCase(ExtractFileExt(AFileName)) = '.chm') then
    begin
      //OpenNewFile(AFileName);
      TmpStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
      ChmReader := TChmReader.Create(TmpStream, True);
      i := AddObject(AFileName, ChmReader);
      FLastChm := ChmReader;
      if Assigned(FOnOpenNewFile) then
        FOnOpenNewFile(Self, i)
      else
        FUnNotifiedFiles.Add(ChmReader);
      Result := True;
    end;
  end;
end;

function TChmFileList.MetaObjectExists(var Name: String): QWord;
var
  AFileName: String;
  URL: String;
  iStart, iEnd: Integer;
  Found: Boolean;
begin
  Found := False;
  Result := 0;
  //Known META file link types
  //       ms-its:name.chm::/topic.htm
  //mk:@MSITStore:name.chm::/topic.htm
  if Pos('ms-its:', Name) > 0 then
  begin
    iStart := Pos('ms-its:', Name) + Length('ms-its:');
    iEnd := Pos('::', Name) - iStart;
    AFileName := Copy(Name, iStart, iEnd);
    iStart := iEnd + iStart + 2;
    iEnd := Length(Name) - (iStart-1);
    URL := Copy(Name, iStart, iEnd);
    Found := True;
  end
  else if Pos('mk:@MSITStore:', Name) > 0 then
  begin
    iStart := Pos('mk:@MSITStore:', Name)+Length('mk:@MSITStore:');
    iEnd := Pos('::', Name) - iStart;
    AFileName := Copy(Name, iStart, iEnd);
    iStart := iEnd + iStart + 2;
    iEnd := Length(Name) - (iStart-1);
    URL := Copy(Name, iStart, iEnd);
    Found := True;
  end;
  if not Found then Exit;
  //WriteLn('Looking for URL ', URL, ' in ', AFileName);
  if OpenChmFile(AFileName) then
    Result := FLastChm.ObjectExists(URL);
  if Result > 0 then
    Name := Url;
end;

function TChmFileList.MetaGetObject(Name: String): TMemoryStream;
begin
  Result := TMemoryStream.Create();
  if not MetaGetObjectData(Name, Result) then
    FreeAndNil(Result);
end;

function TChmFileList.MetaGetObjectData(Name: String; AData: TStream): Boolean;
begin
  Result := False;
  if Assigned(AData) and (MetaObjectExists(Name) > 0) then
    Result := FLastChm.ReadFileContent(Name, AData);
end;

procedure TChmFileList.SetOnOpenNewFile(AValue: TChmFileOpenEvent);
var
  X: Integer;
begin
  FOnOpenNewFile := AValue;
  if AValue = nil then Exit;
  for X := 0 to FUnNotifiedFiles.Count-1 do
    AValue(Self, X);
  FUnNotifiedFiles.Clear();
end;

function TChmFileList.ObjectExists(const AName: String; var AChmReader: TChmReader): QWord;
var
  s: string;
begin
  Result := 0;
  if Count = 0 then Exit;
  if AChmReader <> nil then
    FLastChm := AChmReader;
  Result := FLastChm.ObjectExists(AName);
  if Result = 0 then
  begin
    Result := ChmReaders[0].ObjectExists(AName);
    if Result > 0 then
      FLastChm := ChmReaders[0];
  end;
  if Result = 0 then
  begin
    s := AName;
    Result := MetaObjectExists(s);
  end;
  if (Result <> 0) and (AChmReader = nil) then
    AChmReader := FLastChm;
end;

function TChmFileList.GetObject(const AName: String): TMemoryStream;
begin
  Result := TMemoryStream.Create();
  if not ReadFileContent(AName, Result) then
    FreeAndNil(Result);
end;

function TChmFileList.ReadFileContent(const AName: String; AData: TStream
  ): Boolean;
begin
  Result := False;
  if Count = 0 then Exit;
  Result := FLastChm.ReadFileContent(AName, AData);
  if not Result then
    Result := MetaGetObjectData(AName, AData);
end;

function TChmFileList.IsAnOpenFile(AFileName: String): Boolean;
var
  X: Integer;
begin
  Result := False;
  for X := 0 to Count-1 do
  begin
    if AFileName = FileName[X] then Exit(True);
  end;
end;

end.

