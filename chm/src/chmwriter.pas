{ Copyright (C) <2005> <Andrew Haines> chmwriter.pas

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation, Inc.,
  51 Franklin Street, Fifth Floor, Boston, MA 02111-1301, USA.
}
{
  See the file COPYING.FPC, included in this distribution,
  for details about the copyright.
}
unit chmwriter;

{$MODE OBJFPC}{$H+}

interface

uses Classes, ChmBase, chmtypes, chmspecialfiles, HtmlIndexer,
  chmsitemap, contnrs, StreamEx, Avl_Tree, lzxcompressthread;

const
  DefaultHHC = 'Default.hhc';
  DefaultHHK = 'Default.hhk';

type
  // TGetDataFunc - Fill Stream with content from given DataName
  //  DataName :  A FileName or whatever so that the getter can find and open the file to add
  //  PathInChm:  This is the absolute path in the archive. i.e. /home/user/helpstuff/
  //              becomes '/' and /home/user/helpstuff/subfolder/ > /subfolder/
  //  FileName :  /home/user/helpstuff/index.html > index.html
  //  Stream   :  the file opened with DataName should be written to this stream
  TGetDataFunc = function(const DataName: string; out PathInChm: string;
    out FileName: string; Stream: TStream): Boolean of object;

type
  TStringIndex = class
    // AVLTree needs wrapping in non automated reference type also used in filewriter.
    TheString: string;
    StrId: Integer;
  end;

  TUrlStrIndex = class
    UrlStr: string;
    UrlStrId: Integer;
  end;

  { TStringIndexList }

  TStringIndexList = class(TAVLTree)
  private
    FSpareStringIndex: TStringIndex;
  public
    constructor Create(); reintroduce;
    procedure BeforeDestruction(); override;
    function AddStringIndex(AString: string; AStrId: Integer): TStringIndex;
    function GetStringIndex(AString: string): TStringIndex;
  end;

  { TITSFWriter }

  TITSFWriter = class(TObject)
    FOnLastFile: TNotifyEvent;
  private
    ForceExit: Boolean;
    FInternalFiles: TFileEntryList;
    // Contains a complete list of files in the chm including
    FFrameSize: LongWord;
    // uncompressed files and special internal files of the chm
    FCurrentStream: TStream; // used to buffer the files that are to be compressed
    FCurrentIndex: Integer;
    FOnGetFileData: TGetDataFunc;
    // uncompressed data
    FSection0: TMemoryStream;
    // Compressed Stream
    FSection1: TStream;
    FSection1Size: QWord;
    FSection1ResetTable: TMemoryStream;
    // has a list of frame positions NOT window positions
    FDirectoryListings: TStream;
    FOutStream: TStream;
    FFileNames: TStrings;
    FDestroyStream: Boolean;
    FTempStream: TStream;
    FPostStream: TStream;
    FWindowSize: LongWord;
    FReadCompressedSize: QWord;
    // Current Size of Uncompressed data that went in Section1 (compressed)
    FPostStreamActive: Boolean;
    // Linear order of file
    ITSFHeader: TITSFHeader;
    HeaderSection0Table: TITSFHeaderEntry;  // points to HeaderSection0
    HeaderSection1Table: TITSFHeaderEntry; // points to HeaderSection1
    HeaderSuffix: TITSFHeaderSuffix; //contains the offset of CONTENTSection0 from zero
    HeaderSection0: TITSPHeaderPrefix;
    HeaderSection1: TITSPHeader; // DirectoryListings header
    FReadmeMessage: string;
    FCores: Integer;
    // DirectoryListings
    // CONTENT Section 0 (section 1 is contained in section 0)
    // EOF
    // end linear header parts
    procedure InitITSFHeader();
    procedure InitHeaderSectionTable();
    procedure SetTempRawStream(const AValue: TStream);
    procedure WriteHeader(Stream: TStream);
    procedure CreateDirectoryListings();
    procedure WriteDirectoryListings(Stream: TStream);
    procedure StartCompressingStream();
    procedure WriteREADMEFile();
    procedure WriteSection0();
    procedure WriteSection1();
    // This procedure will write all files starting with ::
    procedure WriteDataSpaceFiles(const AStream: TStream);

    // callbacks for lzxcomp
    function AtEndOfData(): LongBool;
    function GetData(Count: LongInt; Buffer: PByte): LongInt;
    function WriteCompressedData(Count: LongInt; Buffer: Pointer): LongInt;
    // when lzx frame compressed
    procedure MarkFrame(UnCompressedTotal, CompressedTotal: LongWord);
    // end callbacks

    // callbacks for lzx compress threads
    function LTGetData(Sender: TLZXCompressor; WantedByteCount: Integer;
      Buffer: Pointer): Integer;
    function LTIsEndOfFile(Sender: TLZXCompressor): Boolean;
    procedure LTChunkDone(Sender: TLZXCompressor; CompressedSize: Integer;
      UncompressedSize: Integer; Buffer: Pointer);
    procedure LTMarkFrame(Sender: TLZXCompressor; CompressedTotal: Integer;
      UncompressedTotal: Integer);
    // end callbacks
  protected
    procedure WriteInternalFilesBefore(); virtual;
    procedure WriteInternalFilesAfter(); virtual;
    procedure WriteFinalCompressedFiles(); virtual;
    { Executed when file added to resulting stream, before compression }
    procedure FileAdded(AStream: TStream; const AEntry: TFileEntryRec); virtual;
  public
    { NOTE: AOutStream MUST NOT be destroyed before TITSFWriter destructor
      Set FreeStreamOnDestroy to True, and AOutStream will be freed in Destroy() }
    constructor Create(AOutStream: TStream; FreeStreamOnDestroy: Boolean); virtual;
    destructor Destroy; override;
    { Execute CHM file creation:
      - WriteInternalFilesBefore() (Readme by default)
      - compress and write user files from FilesToCompress list
      - compress and write FPostStream
      - WriteInternalFilesAfter()
      - creates all special files in the archive that start with ::DataSpace
      - creates all directory listings including header
      - fill and write CHM header }
    procedure Execute();
    { This procedure is used to manually add files to compress to an internal stream that is
      processed before FileToCompress is called. Files added this way should not be
      duplicated in the FilesToCompress property. }
    procedure AddStreamToArchive(AFileName, APath: string; AStream: TStream;
      Compress: Boolean = True);
    { Add file to OutStream after user files }
    procedure PostAddStreamToArchive(AFileName, APath: string;
      AStream: TStream; Compress: Boolean = True);

    property WindowSize: LongWord read FWindowSize write FWindowSize default 2;
    // in $8000 blocks
    property FrameSize: LongWord read FFrameSize write FFrameSize default 1;
    // in $8000 blocks
    property FilesToCompress: TStrings read FFileNames;
    { Triggers when next file from FilesToCompress need to be compressed and
      appended to CHM, provide content for that file as TStream }
    property OnGetFileData: TGetDataFunc read FOnGetFileData write FOnGetFileData;
    { Triggers after compressing last file from FilesToCompress list
      TChmProject used it to append TOC and Index }
    property OnLastFile: TNotifyEvent read FOnLastFile write FOnLastFile;
    property OutStream: TStream read FOutStream;
    property TempRawStream: TStream read FTempStream write SetTempRawStream;
    property ReadmeMessage: string read FReadmeMessage write FReadmeMessage;
    { CPU cores number, for multi-threaded compression }
    property Cores: Integer read FCores write FCores;
    { MS Locale ID code }
    property LocaleID: DWord read ITSFHeader.LanguageID write ITSFHeader.LanguageID;
  end;

  { TChmWriter }

  TChmWriter = class(TITSFWriter)
  private
    FDefaultFont: string;
    FDefaultPage: string;
    FFullTextSearch: Boolean;
    FFullTextSearchAvailable: Boolean;
    FSearchTitlesOnly: Boolean;
    FStringsStream: TMemoryStream; // the #STRINGS file
    FTopicsStream: TMemoryStream;  // the #TOPICS file
    FURLTBLStream: TMemoryStream;  // the #URLTBL file. has offsets of strings in URLSTR
    FURLSTRStream: TMemoryStream;  // the #URLSTR file
    FFiftiMainStream: TMemoryStream;
    FContextStream: TMemoryStream; // the #IVB file
    FIDXHdrStream: TMemoryStream; // the #IDXHDR and chunk 13 in #SYSTEM
    FTitle: string;
    FHasTOC: Boolean;
    FHasIndex: Boolean;
    FIndexedFiles: TIndexedWordList;
    FAvlStrings: TStringIndexList;    // dedupe strings
    // ?not used? Topic deduping, if we load it both from hhp and TOC
    //FAVLTopicDedupe: TStringIndexList;
    FAvlURLStr: TAVLTree;    // dedupe urltbl + binindex must resolve URL to topicid
    //SpareString: TStringIndex;
    SpareUrlStr: TUrlStrIndex;
    FWindows: TCHMWindowList;
    FDefaultWindow: string;
    FTocName: string;
    FIndexName: string;
    FMergeFiles: TStringList;
    FTocSM: TCHMSitemap;
    FHasKLinks: Boolean;
    FNrTopics: Integer;
  protected
    FHasBinaryTOC: Boolean;
    FHasBinaryIndex: Boolean;

    procedure WriteInternalFilesBefore(); override;
    procedure WriteInternalFilesAfter(); override;
    procedure WriteFinalCompressedFiles(); override;
    { Executed when file added to resulting stream, before compression.
      If FullTextSearch enabled, fill Search index from this file contents }
    procedure FileAdded(AStream: TStream; const AEntry: TFileEntryRec); override;
    { Write AStream to internal file '#TOCIDX' }
    procedure AppendBinaryTOCStream(AStream: TStream);
    { Write binary Index contents to CHM internal files
      IndexStream - '/$WWKeywordLinks/BTree'
      DataStream - '/$WWKeywordLinks/Data'
      MapStream - '/$WWKeywordLinks/Map'
      PropertyStream - '/$WWKeywordLinks/Property'
      chw - if True then convert names to upper case (CHW format) }
    procedure AppendBinaryIndexStream(IndexStream, DataStream, MapStream, PropertyStream: TStream; chw: Boolean);
  private
    procedure WriteSYSTEM();
    procedure WriteITBITS();
    procedure WriteSTRINGS();
    procedure WriteTOPICS();
    procedure WriteIVB(); // context ids
    procedure CreateIDXHDRStream();
    procedure WriteIDXHDR();
    procedure WriteURL_STR_TBL();
    procedure WriteOBJINST();
    procedure WriteFiftiMain();
    procedure WriteWindows();

    function AddString(AString: string): LongWord;
    function AddURL(AURL: string; TopicsIndex: DWord): LongWord;
    procedure CheckFileMakeSearchable(AStream: TStream; const AFileEntry: TFileEntryRec);
    function AddTopic(ATitle, AnUrl: AnsiString; code: Integer = -1): Integer;
    procedure ScanSitemap(ASiteMap: TCHMSiteMap);
    function NextTopicIndex: Integer;
    procedure SetWindows(AWindowList: TCHMWindowList);
    procedure SetMergeFiles(Src: TStringList);
  public
    constructor Create(AOutStream: TStream; FreeStreamOnDestroy: Boolean); override;
    destructor Destroy; override;
    { Write AStream to internal file TOCName, 'Default.hhc' by default }
    procedure AppendTOC(AStream: TStream);
    { Write ASiteMap as binary TOC }
    procedure AppendBinaryTOCFromSiteMap(ASiteMap: TChmSiteMap);
    { Write ASiteMap as binary Index }
    procedure AppendBinaryIndexFromSiteMap(ASiteMap: TChmSiteMap; chw: Boolean);
    { Write stream to internal file IndexName, 'Default.HHK' by default }
    procedure AppendIndex(AStream: TStream);
    { Write AStream to internal file AName }
    procedure AppendSearchDB(AName: string; AStream: TStream);
    { Add ContextID and local Url to context list }
    procedure AddContext(AContext: DWord; AUrl: string);
    { Write empty internal file '/$WWAssociativeLinks/Property' }
    procedure AddDummyALink();

    property Title: string read FTitle write FTitle;
    { Enable full-text search indexation for HTML files }
    property FullTextSearch: Boolean read FFullTextSearch write FFullTextSearch;
    { Enable full-text search indexation only for HTML Titles }
    property SearchTitlesOnly: Boolean read FSearchTitlesOnly write FSearchTitlesOnly;
    { Binary TOC present flag, used for CHM header.
      Set automatically when AppendBinaryTOCFromSiteMap() }
    property HasBinaryTOC: Boolean read FHasBinaryTOC;
    { Binary Index present flag, used for CHM header.
      Set automatically when AppendBinaryIndexFromSiteMap() }
    property HasBinaryIndex: Boolean read FHasBinaryIndex;
    property DefaultFont: string read FDefaultFont write FDefaultFont;
    property DefaultPage: string read FDefaultPage write FDefaultPage;
    { TCHMWindow items }
    property Windows: TCHMWindowList read FWindows write SetWindows;
    property TOCName: string read FTocName write FTocName;
    property IndexName: string read FIndexName write FIndexName;
    property DefaultWindow: string read FDefaultWindow write FDefaultWindow;
    property MergeFiles: TStringList read FMergeFiles write SetMergefiles;
    property TOCSitemap: TChmSitemap read FTocSM write FTocSM;
  end;

function CompareStrings(Node1, Node2: Pointer): Integer; // also used in filewriter

implementation

uses dateutils, SysUtils, paslzxcomp, chmFiftiMain;

const
  LZX_WINDOW_SIZE = 16; // 16 = 2 frames = 1 shl 16
  LZX_FRAME_SIZE = $8000;

  {$ifdef binindex}
procedure logentry(s: string);
begin
  Writeln(s);
  flush(stdout);
end;

  {$endif}
{$I chmobjinstconst.inc}


function CompareStrings(Node1, Node2: Pointer): Integer;
var
  n1, n2: TStringIndex;
begin
  n1 := TStringIndex(Node1);
  n2 := TStringIndex(Node2);
  Result := CompareText(n1.TheString, n2.TheString);
  if Result < 0 then
    Result := -1
  else if Result > 0 then
    Result := 1;
end;


function CompareUrlStrs(Node1, Node2: Pointer): Integer;
var
  n1, n2: TUrlStrIndex;
begin
  n1 := TUrlStrIndex(Node1);
  n2 := TUrlStrIndex(Node2);
  Result := CompareText(n1.UrlStr, n2.UrlStr);
  if Result < 0 then
    Result := -1
  else if Result > 0 then
    Result := 1;
end;

{ TStringIndexList }

constructor TStringIndexList.Create();
begin
  inherited Create(@CompareStrings);
  FSpareStringIndex := TStringIndex.Create();
end;

procedure TStringIndexList.BeforeDestruction();
begin
  FreeAndClear();
  FreeAndNil(FSpareStringIndex);
  inherited BeforeDestruction;
end;

function TStringIndexList.AddStringIndex(AString: string; AStrId: Integer
  ): TStringIndex;
begin
  Result := TStringIndex.Create();
  Result.TheString := AString;
  Result.StrId := AStrId;
  Add(Result);
end;

function TStringIndexList.GetStringIndex(AString: string): TStringIndex;
var
  Node: TAVLTreeNode;
begin
  FSpareStringIndex.TheString := AString;
  Node := FindKey(FSpareStringIndex, @CompareStrings);
  if Assigned(Node) then
    Result := TStringIndex(Node.Data)
  else
    Result := nil;
end;

{ TChmWriter }

procedure TITSFWriter.InitITSFHeader();
begin
  with ITSFHeader do
  begin
    ITSFsig := ITSFFileSig;
    Version := NToLE(DWord(3));
    // we fix endian order when this is written to the stream
    HeaderLength := NToLE(DWord(SizeOf(TITSFHeader) + (SizeOf(TGuid) * 2) +
      (SizeOf(TITSFHeaderEntry) * 2) + SizeOf(TITSFHeaderSuffix)));
    Unknown_1 := NToLE(DWord(1));
    TimeStamp := NToBE(MilliSecondOfTheDay(Now)); //bigendian
    if LanguageID = 0 then
      LanguageID := NToLE(DWord($0409)); // English / English_US
  end;
end;

procedure TITSFWriter.InitHeaderSectionTable();
begin
  // header section 0
  HeaderSection0Table.PosFromZero := LEToN(ITSFHeader.HeaderLength);
  HeaderSection0Table.Length := SizeOf(TITSPHeaderPrefix);
  // header section 1
  HeaderSection1Table.PosFromZero :=
    HeaderSection0Table.PosFromZero + HeaderSection0Table.Length;
  HeaderSection1Table.Length := SizeOf(TITSPHeader) + FDirectoryListings.Size;

  //contains the offset of CONTENT Section0 from zero
  HeaderSuffix.Offset := HeaderSection1Table.PosFromZero + HeaderSection1Table.Length;

  // now fix endian stuff
  HeaderSection0Table.PosFromZero := NToLE(HeaderSection0Table.PosFromZero);
  HeaderSection0Table.Length := NToLE(HeaderSection0Table.Length);
  HeaderSection1Table.PosFromZero := NToLE(HeaderSection1Table.PosFromZero);
  HeaderSection1Table.Length := NToLE(HeaderSection1Table.Length);

  with HeaderSection0 do
  begin // TITSPHeaderPrefix;
    Unknown1 := NToLE(DWord($01FE));
    Unknown2 := 0;
    // at this point we are putting together the headers. content sections 0 and 1 are complete
    FileSize := NToLE(HeaderSuffix.Offset + FSection0.Size + FSection1Size);
    Unknown3 := 0;
    Unknown4 := 0;
  end;
  with HeaderSection1 do
  begin // TITSPHeader; // DirectoryListings header
    ITSPsig := ITSPHeaderSig;
    Version := NToLE(DWord(1));
    DirHeaderLength := NToLE(DWord(SizeOf(TITSPHeader)));
    // Length of the directory header
    Unknown1 := NToLE(DWord($0A));
    ChunkSize := NToLE(DWord($1000));
    Density := NToLE(DWord(2));
    // updated when directory listings were created
    //IndexTreeDepth := 1 ; // 1 if there is no index 2 if there is one level of PMGI chunks. will update as
    //IndexOfRootChunk := -1;// if no root chunk
    //FirstPMGLChunkIndex,
    //LastPMGLChunkIndex: LongWord;

    Unknown2 := NToLE(LongInt(-1));
    //DirectoryChunkCount: LongWord;
    LanguageID := NToLE(DWord(LocaleID));
    GUID := ITSPHeaderGUID;
    LengthAgain := NToLE(DWord($54));
    Unknown3 := NToLE(LongInt(-1));
    Unknown4 := NToLE(LongInt(-1));
    Unknown5 := NToLE(LongInt(-1));
  end;

  // more endian stuff
  HeaderSuffix.Offset := NToLE(HeaderSuffix.Offset);
end;

procedure TITSFWriter.SetTempRawStream(const AValue: TStream);
begin
  if (FCurrentStream.Size > 0) or (FSection1.Size > 0) then
    raise Exception.Create(
      'Cannot set the TempRawStream once data has been written to it!');
  if AValue = nil then
    raise Exception.Create('TempRawStream cannot be nil!');
  if FCurrentStream = AValue then
    Exit;
  FCurrentStream.Free();
  FCurrentStream := AValue;
end;

procedure TITSFWriter.WriteHeader(Stream: TStream);
begin
  Stream.Write(ITSFHeader, SizeOf(TITSFHeader));

  if ITSFHeader.Version < 4 then
  begin
    Stream.Write(ITSFHeaderGUID, SizeOf(TGuid));
    Stream.Write(ITSFHeaderGUID, SizeOf(TGuid));
  end;
  Stream.Write(HeaderSection0Table, SizeOf(TITSFHeaderEntry));
  Stream.Write(HeaderSection1Table, SizeOf(TITSFHeaderEntry));
  Stream.Write(HeaderSuffix, SizeOf(TITSFHeaderSuffix));
  Stream.Write(HeaderSection0, SizeOf(TITSPHeaderPrefix));

end;

procedure TITSFWriter.CreateDirectoryListings();
type
  TFirstListEntry = record
    Entry: array[0..511] of Byte;
    Size: Integer;
  end;
var
  Buffer: array [0..511] of Byte;
  IndexBlock: TPMGIDirectoryChunk;
  ListingBlock: TDirectoryChunk;
  I: Integer;
  Size: Integer;
  FESize: Integer;
  FileName: string;
  FileNameSize: Integer;
  LastListIndex: Integer;
  FirstListEntry: TFirstListEntry;
  ChunkIndex: Integer;
  ListHeader: TPMGListChunk;
const
  PMGL = 'PMGL';
  PMGI = 'PMGI';

  procedure UpdateLastListChunk();
  var
    Tmp: QWord;
  begin
    if ChunkIndex < 1 then
    begin
      Exit;
    end;
    Tmp := FDirectoryListings.Position;
    FDirectoryListings.Position := (LastListIndex) * $1000;
    FDirectoryListings.Read(ListHeader, SizeOf(TPMGListChunk));
    FDirectoryListings.Position := (LastListIndex) * $1000;
    ListHeader.NextChunkIndex := NToLE(ChunkIndex);
    FDirectoryListings.Write(ListHeader, SizeOf(TPMGListChunk));
    FDirectoryListings.Position := Tmp;
  end;

  procedure WriteIndexChunk(ShouldFinish: Boolean = False);
  var
    IndexHeader: TPMGIIndexChunk;
    ParentIndex, TmpIndex: TPMGIDirectoryChunk;
  begin
    IndexHeader.PMGIsig := PMGI;
    IndexHeader.UnusedSpace := NToLE(IndexBlock.FreeSpace);

    IndexBlock.WriteHeader(@IndexHeader);
    IndexBlock.WriteChunkToStream(FDirectoryListings, ChunkIndex, ShouldFinish);
    IndexBlock.Clear;
    if HeaderSection1.IndexOfRootChunk < 0 then
      HeaderSection1.IndexOfRootChunk := ChunkIndex;
    if ShouldFinish then
    begin
      HeaderSection1.IndexTreeDepth := 2;
      ParentIndex := IndexBlock.ParentChunk;
      if ParentIndex <> nil then
      begin
        repeat // the parent index is notified by our child index when to write
          HeaderSection1.IndexOfRootChunk := ChunkIndex;
          TmpIndex := ParentIndex;
          ParentIndex := ParentIndex.ParentChunk;
          TmpIndex.Free;
          Inc(HeaderSection1.IndexTreeDepth);
          Inc(ChunkIndex);
        until ParentIndex = nil;
      end;
    end;
    Inc(ChunkIndex);
  end;

  procedure WriteListChunk();
  begin
    with ListHeader do
    begin
      PMGLsig := PMGL;
      UnusedSpace := NToLE(ListingBlock.FreeSpace);
      Unknown1 := 0;
      PreviousChunkIndex := NToLE(LastListIndex);
      NextChunkIndex := NToLE(longint(-1));
      // we update this when we write the next chunk
    end;
    if HeaderSection1.FirstPMGLChunkIndex <= 0 then
      HeaderSection1.FirstPMGLChunkIndex := NToLE(ChunkIndex);
    HeaderSection1.LastPMGLChunkIndex := NToLE(ChunkIndex);
    ListingBlock.WriteHeader(@ListHeader);
    ListingBlock.WriteChunkToStream(FDirectoryListings);
    ListingBlock.Clear();
    UpdateLastListChunk();

    LastListIndex := ChunkIndex;
    Inc(ChunkIndex);
    // now add to index
    if not IndexBlock.CanHold(FirstListEntry.Size) then
      WriteIndexChunk();
    IndexBlock.WriteEntry(FirstListEntry.Size, @FirstListEntry.Entry[0]);
  end;

begin
  // first sort the listings
  FInternalFiles.Sort();
  HeaderSection1.IndexTreeDepth := 1;
  HeaderSection1.IndexOfRootChunk := -1;

  ChunkIndex := 0;

  IndexBlock := TPMGIDirectoryChunk.Create(SizeOf(TPMGIIndexChunk));
  ListingBlock := TDirectoryChunk.Create(SizeOf(TPMGListChunk));
  try
    LastListIndex := -1;

    // add files to a pmgl block until it is full.
    // after the block is full make a pmgi block and add the first entry of the pmgl block
    // repeat until the index block is full and start another.
    // the pmgi chunks take care of needed parent chunks in the tree
    for I := 0 to FInternalFiles.Count - 1 do
    begin
      Size := 0;
      FileName := FInternalFiles.FileEntry[I].Path + FInternalFiles.FileEntry[I].Name;
      FileNameSize := Length(FileName);
      // filename length
      Inc(Size, WriteCompressedInteger(@Buffer[Size], FileNameSize));
      // filename
      Move(FileName[1], Buffer[Size], FileNameSize);
      Inc(Size, FileNameSize);
      FESize := Size;
      // File is compressed...
      Inc(Size, WriteCompressedInteger(@Buffer[Size],
        Ord(FInternalFiles.FileEntry[I].Compressed)));
      // Offset from section start
      Inc(Size, WriteCompressedInteger(@Buffer[Size],
        FInternalFiles.FileEntry[I].DecompressedOffset));
      // Size when uncompressed
      Inc(Size, WriteCompressedInteger(@Buffer[Size],
        FInternalFiles.FileEntry[I].DecompressedSize));

      if not ListingBlock.CanHold(Size) then
        WriteListChunk();

      ListingBlock.WriteEntry(Size, @Buffer[0]);

      if ListingBlock.ItemCount = 1 then
      begin // add the first list item to the index
        Move(Buffer[0], FirstListEntry.Entry[0], FESize);
        FirstListEntry.Size := FESize + WriteCompressedInteger(
          @FirstListEntry.Entry[FESize], ChunkIndex);
      end;
    end;
    if ListingBlock.ItemCount > 0 then
      WriteListChunk();

    if ChunkIndex > 1 then
    begin
      if (IndexBlock.ItemCount > 1)
      or ((IndexBlock.ItemCount > 0) and (HeaderSection1.IndexOfRootChunk > -1)) then
        WriteIndexChunk(True);
    end;

    HeaderSection1.DirectoryChunkCount := NToLE(DWord(FDirectoryListings.Size div $1000));

  finally
    ListingBlock.Free();
    IndexBlock.Free();
  end;

  //now fix some endian stuff
  HeaderSection1.IndexOfRootChunk := NToLE(HeaderSection1.IndexOfRootChunk);
  HeaderSection1.IndexTreeDepth := NtoLE(HeaderSection1.IndexTreeDepth);
end;

procedure TITSFWriter.WriteDirectoryListings(Stream: TStream);
begin
  Stream.Write(HeaderSection1, SizeOf(HeaderSection1));
  FDirectoryListings.Position := 0;
  Stream.CopyFrom(FDirectoryListings, FDirectoryListings.Size);
  FDirectoryListings.Position := 0;
  //TMemoryStream(FDirectoryListings).SaveToFile('dirlistings.pmg');
end;

procedure TITSFWriter.WriteInternalFilesBefore();
begin
  // written to Section0 (uncompressed)
  WriteREADMEFile();
end;

procedure TITSFWriter.WriteInternalFilesAfter();
begin
end;

procedure IterateWord(AWord: TIndexedWord; State: pointer);
var
  i, cnt: Integer;
begin
  cnt := PInteger(State)^;
  for i := 0 to AWord.DocumentCount - 1 do
    Inc(cnt, AWord.GetLogicalDocument(i).NumberOfIndexEntries);
  // was commented in original procedure, seems to list index entries per doc.
  //WriteLn(AWord.TheWord,'             documents = ', AWord.DocumentCount, ' h
  PInteger(State)^ := cnt;
end;

procedure TITSFWriter.WriteREADMEFile();
const
  DISCLAIMER_STR =
    'This archive was not made by the MS HTML Help Workshop(r)(tm) program, but by Free Pascal''s chm package '
    +
    CHMPackageVersion + '.'#13#10;
var
  Entry: TFileEntryRec;
begin
  // This procedure puts a file in the archive that says it wasn't compiled with the MS compiler
  Entry.Compressed := False;
  Entry.DecompressedOffset := FSection0.Position;
  FSection0.Write(DISCLAIMER_STR, SizeOf(DISCLAIMER_STR));
  if Length(FReadmeMessage) > 0 then
    FSection0.Write(FReadmeMessage[1], Length(FReadmeMessage));
  Entry.DecompressedSize := FSection0.Position - Entry.DecompressedOffset;
  Entry.Path := '/';
  Entry.Name := '_#_README_#_'; //try to use a name that won't conflict with normal names
  FInternalFiles.AddEntry(Entry);
end;

procedure TITSFWriter.WriteFinalCompressedFiles();
begin

end;


procedure TITSFWriter.WriteSection0();
begin
  FSection0.Position := 0;
  FOutStream.CopyFrom(FSection0, FSection0.Size);
end;

procedure TITSFWriter.WriteSection1();
begin
  WriteContentToStream(FOutStream, FSection1);
end;

procedure TITSFWriter.WriteDataSpaceFiles(const AStream: TStream);
var
  Entry: TFileEntryRec;
begin
  // This procedure will write all files starting with ::
  Entry.Compressed := False; // None of these files are compressed

  //  ::DataSpace/NameList
  Entry.DecompressedOffset := FSection0.Position;
  Entry.DecompressedSize := WriteNameListToStream(FSection0,
    [snUnCompressed, snMSCompressed]);
  Entry.Path := '::DataSpace/';
  Entry.Name := 'NameList';
  FInternalFiles.AddEntry(Entry, False);

  //  ::DataSpace/Storage/MSCompressed/ControlData
  Entry.DecompressedOffset := FSection0.Position;
  Entry.DecompressedSize := WriteControlDataToStream(FSection0, 2, 2, 1);
  Entry.Path := '::DataSpace/Storage/MSCompressed/';
  Entry.Name := 'ControlData';
  FInternalFiles.AddEntry(Entry, False);

  //  ::DataSpace/Storage/MSCompressed/SpanInfo
  Entry.DecompressedOffset := FSection0.Position;
  Entry.DecompressedSize := WriteSpanInfoToStream(FSection0, FReadCompressedSize);
  Entry.Path := '::DataSpace/Storage/MSCompressed/';
  Entry.Name := 'SpanInfo';
  FInternalFiles.AddEntry(Entry, False);

  //  ::DataSpace/Storage/MSCompressed/Transform/List
  Entry.DecompressedOffset := FSection0.Position;
  Entry.DecompressedSize := WriteTransformListToStream(FSection0);
  Entry.Path := '::DataSpace/Storage/MSCompressed/Transform/';
  Entry.Name := 'List';
  FInternalFiles.AddEntry(Entry, False);

  //  ::DataSpace/Storage/MSCompressed/Transform/{7FC28940-9D31-11D0-9B27-00A0C91E9C7C}/
  //  ::DataSpace/Storage/MSCompressed/Transform/{7FC28940-9D31-11D0-9B27-00A0C91E9C7C}/InstanceData/ResetTable
  Entry.DecompressedOffset := FSection0.Position;
  Entry.DecompressedSize := WriteResetTableToStream(FSection0, FSection1ResetTable);
  Entry.Path :=
    '::DataSpace/Storage/MSCompressed/Transform/{7FC28940-9D31-11D0-9B27-00A0C91E9C7C}/InstanceData/';
  Entry.Name := 'ResetTable';
  FInternalFiles.AddEntry(Entry, True);


  //  ::DataSpace/Storage/MSCompressed/Content do this last
  Entry.DecompressedOffset := FSection0.Position;
  Entry.DecompressedSize := FSection1Size;
  // we will write it directly to FOutStream later
  Entry.Path := '::DataSpace/Storage/MSCompressed/';
  Entry.Name := 'Content';
  FInternalFiles.AddEntry(Entry, False);

end;

procedure TITSFWriter.FileAdded(AStream: TStream; const AEntry: TFileEntryRec);
begin
  // do nothing here
end;

function _AtEndOfData(arg: Pointer): LongBool; cdecl;
begin
  Result := TITSFWriter(arg).AtEndOfData;
end;

function TITSFWriter.AtEndOfData: LongBool;
begin
  Result := ForceExit or (FCurrentIndex >= FFileNames.Count - 1);
  if Result then
    Result := Integer(FCurrentStream.Position) >= Integer(FCurrentStream.Size) - 1;
end;

function _GetData(arg: pointer; Count: LongInt; Buffer: Pointer): LongInt; cdecl;
begin
  Result := TITSFWriter(arg).GetData(Count, PByte(Buffer));
end;

function TITSFWriter.GetData(Count: LongInt; Buffer: PByte): LongInt;
var
  FileEntry: TFileEntryRec;
begin
  Result := 0;
  while (Result < Count) and (not AtEndOfData) do
  begin
    Inc(Result, FCurrentStream.Read(Buffer[Result], Count - Result));
    if (Result < Count) and (not AtEndOfData) then
    begin
      // the current file has been read. move to the next file in the list
      FCurrentStream.Position := 0;
      FCurrentStream.Size := 0;
      Inc(FCurrentIndex);
      ForceExit := OnGetFileData(FFileNames[FCurrentIndex], FileEntry.Path,
        FileEntry.Name, FCurrentStream);
      FileEntry.DecompressedSize := FCurrentStream.Size;
      FileEntry.DecompressedOffset := FReadCompressedSize;
      //269047723;//to test writing really large numbers
      FileEntry.Compressed := True;

      FileAdded(FCurrentStream, FileEntry);

      FInternalFiles.AddEntry(FileEntry);
      // So the next file knows it's offset
      Inc(FReadCompressedSize, FileEntry.DecompressedSize);
      FCurrentStream.Position := 0;
    end;

    // this is intended for programs to add perhaps a file
    // after all the other files have been added.
    if (AtEndOfData) and (FCurrentStream <> FPostStream) then
    begin
      FPostStreamActive := True;
      if Assigned(FOnLastFile) then
        FOnLastFile(Self);
      FCurrentStream.Free;
      WriteFinalCompressedFiles();
      FCurrentStream := FPostStream;
      FCurrentStream.Position := 0;
      Inc(FReadCompressedSize, FCurrentStream.Size);
    end;
  end;
end;

function _WriteCompressedData(arg: pointer; Count: LongInt;
  Buffer: Pointer): LongInt; cdecl;
begin
  Result := TITSFWriter(arg).WriteCompressedData(Count, Buffer);
end;

function TITSFWriter.WriteCompressedData(Count: LongInt; Buffer: Pointer): LongInt;
begin
  // we allocate a MB at a time to limit memory reallocation since this
  // writes usually 2 bytes at a time
  if (FSection1 is TMemoryStream) and (FSection1.Position >= FSection1.Size - 1) then
  begin
    FSection1.Size := FSection1.Size + $100000;
  end;
  Result := FSection1.Write(Buffer^, Count);
  Inc(FSection1Size, Result);
end;

procedure _MarkFrame(arg: pointer; UncompressedTotal, CompressedTotal: LongWord); cdecl;
begin
  TITSFWriter(arg).MarkFrame(UncompressedTotal, CompressedTotal);
end;

procedure TITSFWriter.MarkFrame(UnCompressedTotal, CompressedTotal: LongWord);

  procedure WriteQWord(Value: QWord);
  begin
    FSection1ResetTable.Write(NToLE(Value), 8);
  end;

  procedure IncEntryCount();
  var
    OldPos: QWord;
    Value: DWord;
  begin
    OldPos := FSection1ResetTable.Position;
    FSection1ResetTable.Position := $4;
    Value := LeToN(FSection1ResetTable.ReadDWord) + 1;
    FSection1ResetTable.Position := $4;
    FSection1ResetTable.WriteDWord(NToLE(Value));
    FSection1ResetTable.Position := OldPos;
  end;

  procedure UpdateTotalSizes();
  var
    OldPos: QWord;
  begin
    OldPos := FSection1ResetTable.Position;
    FSection1ResetTable.Position := $10;
    WriteQWord(FReadCompressedSize); // size of read data that has been compressed
    WriteQWord(CompressedTotal);
    FSection1ResetTable.Position := OldPos;
  end;

begin
  if FSection1ResetTable.Size = 0 then
  begin
    // Write the header
    FSection1ResetTable.WriteDWord(NtoLE(DWord(2)));
    FSection1ResetTable.WriteDWord(0);
    // number of entries. we will correct this with IncEntryCount
    FSection1ResetTable.WriteDWord(NtoLE(DWord(8))); // Size of Entries (qword)
    FSection1ResetTable.WriteDWord(NtoLE(DWord($28))); // Size of this header
    WriteQWord(0); // Total Uncompressed Size
    WriteQWord(0); // Total Compressed Size
    WriteQWord(NtoLE($8000)); // Block Size
    WriteQWord(0); // First Block start
  end;
  IncEntryCount();
  UpdateTotalSizes();
  WriteQWord(CompressedTotal); // Next Block Start
  // We have to trim the last entry off when we are done because there is no next block in that case
end;

function TITSFWriter.LTGetData(Sender: TLZXCompressor; WantedByteCount: Integer;
  Buffer: Pointer): Integer;
begin
  Result := GetData(WantedByteCount, Buffer);
  //WriteLn('Wanted ', WantedByteCount, ' got ', Result);
end;

function TITSFWriter.LTIsEndOfFile(Sender: TLZXCompressor): Boolean;
begin
  Result := AtEndOfData;
end;

procedure TITSFWriter.LTChunkDone(Sender: TLZXCompressor; CompressedSize: Integer;
  UncompressedSize: Integer; Buffer: Pointer);
begin
  WriteCompressedData(CompressedSize, Buffer);
end;

procedure TITSFWriter.LTMarkFrame(Sender: TLZXCompressor;
  CompressedTotal: Integer; UncompressedTotal: Integer);
begin
  MarkFrame(UncompressedTotal, CompressedTotal);
  //WriteLn('Mark Frame C = ', CompressedTotal, ' U = ', UncompressedTotal);
end;

constructor TITSFWriter.Create(AOutStream: TStream; FreeStreamOnDestroy: Boolean);
begin
  if AOutStream = nil then
    raise Exception.Create('TITSFWriter.OutStream Cannot be nil!');
  FOutStream := AOutStream;
  FCurrentIndex := -1;
  FCurrentStream := TMemoryStream.Create();
  FInternalFiles := TFileEntryList.Create();
  FSection0 := TMemoryStream.Create();
  FSection1 := TMemoryStream.Create();
  FSection1ResetTable := TMemoryStream.Create();
  FDirectoryListings := TMemoryStream.Create();
  FPostStream := TMemoryStream.Create();

  FDestroyStream := FreeStreamOnDestroy;
  FFileNames := TStringList.Create();
  ITSFHeader.LanguageID := 0;
end;

destructor TITSFWriter.Destroy();
begin
  if FDestroyStream then
    FreeAndNil(FOutStream);
  FreeAndNil(FInternalFiles);
  FreeAndNil(FCurrentStream);
  FreeAndNil(FSection0);
  FreeAndNil(FSection1);
  FreeAndNil(FSection1ResetTable);
  FreeAndNil(FDirectoryListings);
  FreeAndNil(FFileNames);
  inherited Destroy;
end;

procedure TITSFWriter.Execute();
begin
  InitITSFHeader();
  FOutStream.Position := 0;
  FSection1Size := 0;

  // write any internal files to FCurrentStream that we want in the compressed section
  WriteInternalFilesBefore();

  // move back to zero so that we can start reading from zero :)
  FReadCompressedSize := FCurrentStream.Size;
  FCurrentStream.Position := 0;
  // when compressing happens, first the FCurrentStream is read
  // before loading user files. So we can fill FCurrentStream with
  // internal files first.

  // this gathers ALL files that should be in section1 (the compressed section)
  StartCompressingStream();
  FSection1.Size := FSection1Size;

  WriteInternalFilesAfter();

  //this creates all special files in the archive that start with ::DataSpace
  WriteDataSpaceFiles(FSection0);

  // creates all directory listings including header
  CreateDirectoryListings();

  // do this after we have compressed everything so that we know the values that must be written
  InitHeaderSectionTable();

  // Now we can write everything to FOutStream
  WriteHeader(FOutStream);
  WriteDirectoryListings(FOutStream);
  WriteSection0(); //does NOT include section 1 even though section0.content IS section1
  WriteSection1(); // writes section 1 to FOutStream
end;

// this procedure is used to manually add files to compress to an internal stream that is
// processed before FileToCompress is called. Files added this way should not be
// duplicated in the FilesToCompress property.
procedure TITSFWriter.AddStreamToArchive(AFileName, APath: string;
  AStream: TStream; Compress: Boolean = True);
var
  TargetStream: TStream;
  Entry: TFileEntryRec;
begin
  // in case AddStreamToArchive is used after we should be writing to the post stream
  if FPostStreamActive then
  begin
    PostAddStreamToArchive(AFileName, APath, AStream, Compress);
    Exit;
  end;
  if AStream = nil then
    Exit;
  if Compress then
    TargetStream := FCurrentStream
  else
    TargetStream := FSection0;

  Entry.Name := AFileName;
  Entry.Path := APath;
  Entry.Compressed := Compress;
  Entry.DecompressedOffset := TargetStream.Position;
  Entry.DecompressedSize := AStream.Size;
  FileAdded(AStream, Entry);
  FInternalFiles.AddEntry(Entry);
  AStream.Position := 0;
  TargetStream.CopyFrom(AStream, AStream.Size);
end;

procedure TITSFWriter.PostAddStreamToArchive(AFileName, APath: string;
  AStream: TStream; Compress: Boolean);
var
  TargetStream: TStream;
  Entry: TFileEntryRec;
begin
  if AStream = nil then
    Exit;
  if Compress then
    TargetStream := FPostStream
  else
    TargetStream := FSection0;

  Entry.Name := AFileName;
  Entry.Path := APath;
  Entry.Compressed := Compress;
  if not Compress then
    Entry.DecompressedOffset := TargetStream.Position
  else
    Entry.DecompressedOffset := FReadCompressedSize + TargetStream.Position;
  Entry.DecompressedSize := AStream.Size;
  FInternalFiles.AddEntry(Entry);
  AStream.Position := 0;
  TargetStream.CopyFrom(AStream, AStream.Size);
  FileAdded(AStream, Entry);
end;

procedure TITSFWriter.StartCompressingStream();
var
  LZXdata: Plzx_data;
  WSize: LongInt;
  Compressor: TLZXCompressor;
begin
  if FCores = 0 then
  begin
    lzx_init(@LZXdata, LZX_WINDOW_SIZE, @_GetData, Self, @_AtEndOfData,
      @_WriteCompressedData, Self, @_MarkFrame, Self);

    WSize := 1 shl LZX_WINDOW_SIZE;
    while not AtEndOfData do
    begin
      lzx_reset(LZXdata);
      lzx_compress_block(LZXdata, WSize, True);
    end;

    //we have to mark the last frame manually
    MarkFrame(LZXdata^.len_uncompressed_input, LZXdata^.len_compressed_output);

    lzx_finish(LZXdata, nil);
  end
  else
  begin
    if FCores = 0 then
      FCores := 4;
    Compressor := TLZXCompressor.Create(FCores);
    try
      Compressor.OnChunkDone := @LTChunkDone;
      Compressor.OnGetData := @LTGetData;
      Compressor.OnIsEndOfFile := @LTIsEndOfFile;
      Compressor.OnMarkFrame := @LTMarkFrame;
      Compressor.Execute(True);
      //Sleep(20000);
    finally
      Compressor.Free();
    end;
  end;
end;


procedure TChmWriter.WriteSYSTEM();
var
  Entry: TFileEntryRec;
  TmpStr, TmpTitle: string;
const
  VersionStr = 'HHA Version 4.74.8702'; // does this matter?
begin

  // this creates the /#SYSTEM file
  Entry.Name := '#SYSTEM';
  Entry.Path := '/';
  Entry.Compressed := False;
  Entry.DecompressedOffset := FSection0.Position;

 { if FileExists('#SYSTEM') then
  begin
    TmpStream := TMemoryStream.Create;
    TmpStream.LoadFromFile('#SYSTEM');
    TmpStream.Position := 0;
    FSection0.CopyFrom(TmpStream, TmpStream.Size);
  end;                                    }
  // EntryCodeOrder: 10 9 4 2 3 16 6 0 1 5
  FSection0.WriteDWord(NToLE(Word(3))); // Version
  if Title <> '' then
    TmpTitle := Title
  else
    TmpTitle := 'default';

  // Code -> Length -> Data
  // 10
  FSection0.WriteWord(NToLE(Word(10)));
  FSection0.WriteWord(NToLE(Word(SizeOf(DWord))));
  FSection0.WriteDWord(NToLE(MilliSecondOfTheDay(Now)));
  // 9
  FSection0.WriteWord(NToLE(Word(9)));
  FSection0.WriteWord(NToLE(Word(SizeOf(VersionStr) + 1)));
  FSection0.Write(VersionStr, SizeOf(VersionStr));
  FSection0.WriteByte(0);
  // 4 A struct that is only needed to set if full text search is on.
  FSection0.WriteWord(NToLE(Word(4)));
  FSection0.WriteWord(NToLE(Word(36))); // size

  FSection0.WriteDWord(NToLE(DWord(LocaleID)));
  FSection0.WriteDWord(0);
  FSection0.WriteDWord(NToLE(DWord(Ord(FFullTextSearch and FFullTextSearchAvailable))));

  FSection0.WriteDWord(NToLE(DWord(Ord(FHasKLinks)))); // klinks
  FSection0.WriteDWord(0); // alinks

  // two for a QWord
  FSection0.WriteDWord(0);
  FSection0.WriteDWord(0);

  FSection0.WriteDWord(0);
  FSection0.WriteDWord(0);


  ////////////////////////<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  // 2  default page to load
  if FDefaultPage <> '' then
  begin
    FSection0.WriteWord(NToLE(Word(2)));
    FSection0.WriteWord(NToLE(Word(Length(FDefaultPage) + 1)));
    FSection0.Write(FDefaultPage[1], Length(FDefaultPage));
    FSection0.WriteByte(0);
  end;
  // 3  Title
  if FTitle <> '' then
  begin
    FSection0.WriteWord(NToLE(Word(3)));
    FSection0.WriteWord(NToLE(Word(Length(FTitle) + 1)));
    FSection0.Write(FTitle[1], Length(FTitle));
    FSection0.WriteByte(0);
  end;

  // 16 Default Font
  if FDefaultFont <> '' then
  begin
    FSection0.WriteWord(NToLE(word(16)));
    FSection0.WriteWord(NToLE(word(Length(FDefaultFont) + 1)));
    FSection0.Write(FDefaultFont[1], Length(FDefaultFont));
    FSection0.WriteByte(0);
  end;

  // 6
  // unneeded. if output file is :  /somepath/OutFile.chm the value here is outfile(lowercase)
  {FSection0.WriteWord(6);
  FSection0.WriteWord(Length('test1')+1);
  Fsection0.Write('test1', 5);
  FSection0.WriteByte(0);}

  // 0 Table of contents filename
  if FHasTOC then
  begin
    if FTocName = '' then
      TmpStr := DefaultHHC
    else
      TmpStr := FTocName;
    FSection0.WriteWord(0);
    FSection0.WriteWord(NToLE(Word(Length(TmpStr) + 1)));
    FSection0.Write(TmpStr[1], Length(TmpStr));
    FSection0.WriteByte(0);
  end;
  // 1
  // hhk Index
  if FHasIndex then
  begin
    if FIndexName = '' then
      TmpStr := DefaultHHK
    else
      TmpStr := FIndexName;
    FSection0.WriteWord(NToLE(Word(1)));
    FSection0.WriteWord(NToLE(Word(Length(TmpStr) + 1)));
    FSection0.Write(TmpStr[1], Length(TmpStr));
    FSection0.WriteByte(0);
  end;
  // 5 Default Window

  if FDefaultWindow <> '' then
  begin
    FSection0.WriteWord(NtoLE(Word(5)));
    TmpStr := FDefaultWindow;
    FSection0.WriteWord(NToLE(Word(Length(TmpStr) + 1)));
    FSection0.Write(TmpStr[1], Length(TmpStr));
    FSection0.WriteByte(0);
  end;

  // 7 Binary Index
  if FHasBinaryIndex then
  begin
    {$ifdef binindex}
    logentry('binary index!');
    {$endif}
    FSection0.WriteWord(NToLE(Word(7)));
    FSection0.WriteWord(NToLE(Word(4)));
    FSection0.WriteDWord(DWord(0)); // what is this number to be?
  end;

  // 11 Binary TOC
  if FHasBinaryTOC then
  begin
    FSection0.WriteWord(NToLE(Word(11)));
    FSection0.WriteWord(NToLE(Word(4)));
    FSection0.WriteDWord(DWord(0)); // what is this number to be?
  end;


  // 13
  if FIDXHdrStream.Size > 0 then
  begin
    FSection0.WriteWord(NToLE(Word(13)));
    FSection0.WriteWord(NToLE(Word(FIDXHdrStream.Size)));
    FSection0.CopyFrom(FIDXHdrStream, 0);
  end;

  Entry.DecompressedSize := FSection0.Position - Entry.DecompressedOffset;
  FInternalFiles.AddEntry(Entry);
end;

procedure TChmWriter.WriteITBITS();
var
  Entry: TFileEntryRec;
begin
  // This is an empty and useless file
  Entry.Name := '#ITBITS';
  Entry.Path := '/';
  Entry.Compressed := False;
  Entry.DecompressedOffset := 0;// FSection0.Position;
  Entry.DecompressedSize := 0;

  FInternalFiles.AddEntry(Entry);
end;

procedure TChmWriter.WriteSTRINGS();
begin
  if FStringsStream.Size = 0 then;
  FStringsStream.WriteByte(0);
  FStringsStream.Position := 0;
  PostAddStreamToArchive('#STRINGS', '/', FStringsStream);
end;

procedure TChmWriter.WriteTOPICS();
begin
  if FTopicsStream.Size = 0 then
    Exit;
  if tocname <> '' then
    AddTopic('', self.TOCName, 2);
  if indexname <> '' then
    AddTopic('', self.IndexName, 2);

  FTopicsStream.Position := 0;
  PostAddStreamToArchive('#TOPICS', '/', FTopicsStream);
  // I commented the code below since the result seemed unused
  // FHits:=0;
  //   FIndexedFiles.ForEach(@IterateWord,FHits);
end;

procedure TChmWriter.WriteIDXHDR();
begin
  if FIDXHdrStream.Size = 0 then
    Exit;
  FIDXHdrStream.Position := 0;
  PostAddStreamToArchive('#IDXHDR', '/', FIDXHdrStream);
end;

procedure TChmWriter.WriteIVB();
begin
  if FContextStream = nil then
    exit;

  FContextStream.Position := 0;
  // the size of all the entries
  FContextStream.WriteDWord(NToLE(DWord(FContextStream.Size - SizeOf(DWord))));

  FContextStream.Position := 0;
  AddStreamToArchive('#IVB', '/', FContextStream);
end;

const
  IdxHdrMagic = 'T#SM';

procedure TChmWriter.CreateIDXHDRStream();
var
  i: Integer;
  IdxHdr: TIdxHdr;
begin
  if FMergeFiles.Count = 0 then
    // I assume text/site properties could also trigger idxhdr
    Exit;

  FillChar(IdxHdr, SizeOf(IdxHdr), #0);
  IdxHdr.IdxHdrSig := IdxHdrMagic;
  IdxHdr.Unknown_04 := NtoLE(1);
  IdxHdr.Unknown_08 := NtoLE(1);
  IdxHdr.TopicsCount := NtoLE(FNrTopics);

  if Assigned(FTocSM) then
  begin
    if (FTocSM.ImageList <> '') then
      IdxHdr.ImageListStr := NtoLE(AddString(FTocSM.ImageList));
    if (FTocSM.UseFolderImages) then
      IdxHdr.ImageType := NtoLE(1);
    IdxHdr.Background := NtoLE(FTocSM.BackgroundColor);
    IdxHdr.Foreground := NtoLE(FTocSM.ForegroundColor);
    if (FTocSM.Font <> '') then
      IdxHdr.FontStr := NtoLE(AddString(FTocSM.Font));
    IdxHdr.WindowStyles := NtoLE(FTocsm.WindowStyles);
    IdxHdr.ExWindowStyles := NtoLE(FTocSm.ExWindowStyles);
    if (FTocSM.FrameName <> '') then
      IdxHdr.FrameNameStr := NtoLE(AddString(FTocSM.FrameName));
    if (FTocSM.WindowName <> '') then
      IdxHdr.WindowNameStr := NtoLE(AddString(FTocSM.WindowName));
  end;

  IdxHdr.MergeFilesCount := NtoLE(FMergeFiles.Count);
  if FMergeFiles.Count > 0 then
    IdxHdr.Unknown_4C := NtoLE(1);

  for i := 0 to FMergeFiles.Count - 1 do
    IdxHdr.MergeFilesList[i] := NtoLE(AddString(FMergeFiles[i]));

  FIDXHdrStream.SetSize(4096);
  FIDXHdrStream.Position := 0;
  FIDXHdrStream.Write(IdxHdr, SizeOf(IdxHdr));
end;

procedure TChmWriter.WriteURL_STR_TBL();
begin
  if FURLSTRStream.Size <> 0 then
  begin
    FURLSTRStream.Position := 0;
    PostAddStreamToArchive('#URLSTR', '/', FURLSTRStream);
  end;
  if FURLTBLStream.Size <> 0 then
  begin
    FURLTBLStream.Position := 0;
    PostAddStreamToArchive('#URLTBL', '/', FURLTBLStream);
  end;
end;

procedure TChmWriter.WriteOBJINST();
var
  i: Integer;
  ObjStream: TMemoryStream;
  //Flags: Word;
begin
  ObjStream := TMemoryStream.Create();
  try
    // this file is needed to enable searches for the ms reader
    ObjStream.WriteDWord(NtoLE($04000000));
    ObjStream.WriteDWord(NtoLE(DWord(2))); // two entries

    ObjStream.WriteDWord(NtoLE(DWord(24))); // offset into file of entry
    ObjStream.WriteDWord(NtoLE(DWord(2691))); // size

    ObjStream.WriteDWord(NtoLE(DWord(2715))); // offset into file of entry
    ObjStream.WriteDWord(NtoLE(DWord(36))); // size

    // first entry
    // write guid 4662DAAF-D393-11D0-9A56-00C04FB68BF7
    ObjStream.WriteDWord(NtoLE($4662DAAF));
    ObjStream.WriteWord(NtoLE($D393));
    ObjStream.WriteWord(NtoLE(Word($11D0)));
    ObjStream.WriteWord(NtoLE(Word($569A)));
    ObjStream.WriteByte($00);
    ObjStream.WriteByte($C0);
    ObjStream.WriteByte($4F);
    ObjStream.WriteByte($B6);
    ObjStream.WriteByte($8B);
    ObjStream.WriteByte($F7);

    ObjStream.WriteDWord(NtoLE($04000000));
    ObjStream.WriteDWord(NtoLE(11));  // bit flags
    ObjStream.WriteDWord(NtoLE(DWord(1252)));
    ObjStream.WriteDWord(NtoLE(DWord(1033)));
    ObjStream.WriteDWord(NtoLE($00000000));
    ObjStream.WriteDWord(NtoLE($00000000));
    ObjStream.WriteDWord(NtoLE($00145555));
    ObjStream.WriteDWord(NtoLE($00000A0F));
    ObjStream.WriteWord(NtoLE($0100));
    ObjStream.WriteDWord(NtoLE($00030005));
    for i := 0 to 5 do
      ObjStream.WriteDWord($00000000);
    ObjStream.WriteWord($0000);
    // okay now the fun stuff
    for i := 0 to $FF do
      ObjStream.Write(ObjInstEntries[i], SizeOF(TObjInstEntry));
    {begin
      if i = 1 then
        Flags := 7
      else
        Flags := 0;
      if (i >= $41) and (i <= $5A) then
        Flags := Flags or 2;
      if (i >= $61) and (i <= $7A) then
        Flags := Flags or 1;
      if i = $27 then
        Flags := Flags or 6;
      ObjStream.WriteWord(NtoLE(Flags));
      ObjStream.WriteWord(NtoLE(Word(i)));
      if (i >= $41) and (i <= $5A) then
        ObjStream.WriteByte(NtoLE(i+$20))
      else
        ObjStream.WriteByte(NtoLE(i));
      ObjStream.WriteByte(NtoLE(i));
      ObjStream.WriteByte(NtoLE(i));
      ObjStream.WriteByte(NtoLE(i));
      ObjStream.WriteWord(NtoLE($0000));
    end;}
    ObjStream.WriteDWord(NtoLE($E66561C6));
    ObjStream.WriteDWord(NtoLE($73DF6561));
    ObjStream.WriteDWord(NtoLE($656F8C73));
    ObjStream.WriteWord(NtoLE(Word($6F9C)));
    ObjStream.WriteByte($65);
    // third bit of second entry
    // write guid 8FA0D5A8-DEDF-11D0-9A61-00C04FB68BF7
    ObjStream.WriteDWord(NtoLE($8FA0D5A8));
    ObjStream.WriteWord(NtoLE($DEDF));
    ObjStream.WriteWord(NtoLE(Word($11D0)));
    ObjStream.WriteWord(NtoLE(Word($619A)));
    ObjStream.WriteByte($00);
    ObjStream.WriteByte($C0);
    ObjStream.WriteByte($4F);
    ObjStream.WriteByte($B6);
    ObjStream.WriteByte($8B);
    ObjStream.WriteByte($F7);

    ObjStream.WriteDWord(NtoLE($04000000));
    ObjStream.WriteDWord(NtoLE(DWord(1)));
    ObjStream.WriteDWord(NtoLE(DWord(1252)));
    ObjStream.WriteDWord(NtoLE(DWord(1033)));
    ObjStream.WriteDWord(NtoLE(DWord(0)));

    // second entry
    // write guid 4662DAB0-D393-11D0-9A56-00C04FB68B66
    ObjStream.WriteDWord(NtoLE($4662DAB0));
    ObjStream.WriteWord(NtoLE($D393));
    ObjStream.WriteWord(NtoLE(Word($11D0)));
    ObjStream.WriteWord(NtoLE(Word($569A)));
    ObjStream.WriteByte($00);
    ObjStream.WriteByte($C0);
    ObjStream.WriteByte($4F);
    ObjStream.WriteByte($B6);
    ObjStream.WriteByte($8B);
    ObjStream.WriteByte($66);

    ObjStream.WriteDWord(NtoLE(DWord(666))); // not kidding
    ObjStream.WriteDWord(NtoLE(DWord(1252)));
    ObjStream.WriteDWord(NtoLE(DWord(1033)));
    ObjStream.WriteDWord(NtoLE(DWord(10031)));
    ObjStream.WriteDWord(NtoLE(DWord(0)));

    ObjStream.Position := 0;
    AddStreamToArchive('$OBJINST', '/', ObjStream, True);

  finally
    ObjStream.Free();
  end;

end;

procedure TChmWriter.WriteFiftiMain();
var
  SearchWriter: TChmSearchWriter;
begin
  if FTopicsStream.Size = 0 then
    Exit;
  SearchWriter := TChmSearchWriter.Create(FFiftiMainStream, FIndexedFiles);
  try
    FFullTextSearchAvailable := SearchWriter.HasData;
    // do not add an empty $FIftiMain
    if FFullTextSearchAvailable then
    begin
      SearchWriter.WriteToStream();
    end;
  finally
    SearchWriter.Free();
  end;

  if FFiftiMainStream.Size = 0 then
    Exit;

  FFiftiMainStream.Position := 0;
  PostAddStreamToArchive('$FIftiMain', '/', FFiftiMainStream);
end;

procedure TChmWriter.WriteWindows();
var
  WindowStream: TMemoryStream;
  i: Integer;
  Win: TChmWindow;
  we: TChmWindowEntry;
begin
  if FWindows.Count = 0 then
    Exit;

  WindowStream := TMemoryStream.Create();
  try
    // #WINDOWS header
    WindowStream.WriteDword(NToLE(DWord(FWindows.Count)));
    WindowStream.WriteDword(NToLE(DWord(196))); // 1.1 or later. 188 is old style.
    // #WINDOWS entries
    for i := 0 to Windows.Count - 1 do
    begin
      Win := Windows[i];

      FillChar(we, SizeOf(we), #0);
      we.EntrySize := NtoLE(196);
      //we.IsUnicode := ;
      we.WindowTypeStr := NToLE(AddString(Win.window_type));
      we.WindowFlags := NToLE(DWord(Win.flags));
      we.NavStyleFlags := NToLE(DWord(Win.nav_style));
      we.TitleBarStr := NToLE(AddString(Win.title_bar_text));
      we.StyleFlags := NToLE(DWord(Win.styleflags));
      we.ExStyleFlags := NToLE(DWord(Win.xtdstyleflags));
      we.WindowPosition[0] := NToLE(DWord(Win.left));
      we.WindowPosition[1] := NToLE(DWord(Win.top));
      we.WindowPosition[2] := NToLE(DWord(Win.right));
      we.WindowPosition[3] := NToLE(DWord(Win.bottom));
      we.WinShowState := NToLE(DWord(Win.window_show_state));
      //we.HelpHandle := 0;
      //we.CallerHandle := 0;
      //we.InfoTypesPtr := 0;
      //we.ToolBarHandle := 0;
      //we.NavHandle := 0;
      //we.HtmlHandle := 0;
      we.NavWidth := NToLE(DWord(Win.navpanewidth));
      //we.TopicPaneRect[0] := 0;
      we.TocFileStr := NToLE(AddString(Win.toc_file));
      we.IndexFileStr := NToLE(AddString(Win.index_file));
      we.DefaultFileStr := NToLE(AddString(Win.default_file));
      we.HomeFileStr := NToLE(AddString(Win.home_button_file));
      we.ButtonsFlags := NToLE(DWord(Win.Buttons));
      we.IsNavPaneClosed := NToLE(DWord(Win.navpane_initially_closed));
      we.NavPaneDefault := NToLE(DWord(Win.navpane_default));
      we.NavPaneLocation := NToLE(DWord(Win.navpane_location));
      we.WmNotifyId := NToLE(DWord(Win.wm_notify_id));
      //we.TabOrder[0] := 0;
      we.HistoryDepth := NToLE(DWord(30));
      we.Jump1TextStr := NToLE(AddString(Win.Jumpbutton_1_Text));
      we.Jump2TextStr := NToLE(AddString(Win.Jumpbutton_2_Text));
      we.Jump1FileStr := NToLE(AddString(Win.Jumpbutton_1_File));
      we.Jump2FileStr := NToLE(AddString(Win.Jumpbutton_2_File));
      //we.MinWindowSize[0] := 0;
      // CHM 1.1 and later
      //we.InfoTypeSize := 0;
      //we.CustomTabs := 0;
      WindowStream.Write(we, SizeOf(we));
    end;
    WindowStream.Position := 0;
    AddStreamToArchive('#WINDOWS', '/', WindowStream, True);
  finally
    WindowStream.Free();
  end;
end;

procedure TChmWriter.WriteInternalFilesAfter();
begin
  // This creates and writes the #ITBITS (empty) file to section0
  WriteITBITS();
  // This creates and writes the #SYSTEM file to section0
  WriteSystem();
  if Assigned(FTocSM) then
    ScanSitemap(FTocSM);
end;

procedure TChmWriter.WriteFinalCompressedFiles();
begin
  inherited WriteFinalCompressedFiles();
  WriteTOPICS();
  WriteURL_STR_TBL();
  WriteWINDOWS();
  CreateIDXHDRStream();
  WriteIDXHDR();
  WriteSTRINGS();
  WriteFiftiMain();
end;

procedure TChmWriter.FileAdded(AStream: TStream; const AEntry: TFileEntryRec);
begin
  inherited FileAdded(AStream, AEntry);
  if FullTextSearch then
    CheckFileMakeSearchable(AStream, AEntry);
end;

procedure TChmWriter.WriteInternalFilesBefore();
begin
  inherited WriteInternalFilesBefore();
  WriteIVB();
  WriteOBJINST();
end;

constructor TChmWriter.Create(AOutStream: TStream; FreeStreamOnDestroy: Boolean);
begin
  inherited Create(AOutStream, FreeStreamOnDestroy);
  FStringsStream := TmemoryStream.Create();
  FTopicsStream := TMemoryStream.Create();
  FURLSTRStream := TMemoryStream.Create();
  FURLTBLStream := TMemoryStream.Create();
  FFiftiMainStream := TMemoryStream.Create();
  FIndexedFiles := TIndexedWordList.Create();
  //FAVLTopicDedupe := TStringIndexList.Create();  // dedupe filenames in topics.
  FAvlStrings := TStringIndexList.Create();    // dedupe strings
  FAvlURLStr := TAVLTree.Create(@CompareUrlStrs);
  // dedupe urltbl + binindex must resolve URL to topicid
  // We need an object to search in avltree
  SpareUrlStr := TUrlStrIndex.Create();
  //    to avoid create/free circles we keep one in spare
  FIDXHdrStream := TMemoryStream.Create();
  // the #IDXHDR and chunk 13 in #SYSTEM
  //    for searching purposes
  FWindows := TCHMWindowList.Create();
  FDefaultWindow := '';
  FMergeFiles := TStringList.Create();
  FNrTopics := 0;
end;

destructor TChmWriter.Destroy();
begin
  if Assigned(FContextStream) then
    FreeAndNil(FContextStream);
  FreeAndNil(FMergeFiles);
  FreeAndNil(FIndexedFiles);
  FreeAndNil(FStringsStream);
  FreeAndNil(FTopicsStream);
  FreeAndNil(FURLSTRStream);
  FreeAndNil(FURLTBLStream);
  FreeAndNil(FFiftiMainStream);
  FreeAndNil(FIDXHdrStream);
  FreeAndNil(SpareUrlStr);
  FAvlUrlStr.FreeAndClear();
  FreeAndNil(FAvlUrlStr);
  FreeAndNil(FAvlStrings);
  //FreeAndNil(FAVLTopicDedupe);
  FreeAndNil(FWindows);

  inherited Destroy;
end;


function TChmWriter.AddString(AString: string): LongWord;
var
  NextBlock: DWord;
  Pos: DWord;
  StrItem: TStringIndex;
begin
  // #STRINGS starts with a null char
  if FStringsStream.Size = 0 then
    FStringsStream.WriteByte(0);

  StrItem := FAvlStrings.GetStringIndex(AString);
  if Assigned(StrItem) then
    Exit(StrItem.StrId);

  // each entry is a null terminated string
  Pos := DWord(FStringsStream.Position);

  // Strings are contained in $1000 byte blocks and cannot cross blocks
  NextBlock := ($0000F000 and Pos) + $00001000;
  if Length(AString) + 1 > NextBlock then
  begin
    FStringsStream.Size := NextBlock;
    FStringsStream.Position := NextBlock;
  end;

  Result := FStringsStream.Position;
  if Length(AString) > 0 then
    FStringsStream.WriteBuffer(AString[1], Length(AString));
  FStringsStream.WriteByte(0);
  FAvlStrings.AddStringIndex(AString, Result);
end;

function TChmWriter.AddURL(AURL: string; TopicsIndex: DWord): LongWord;

  procedure CheckURLStrBlockCanHold(const AString: string);
  var
    Rem: LongWord;
    Len: LongWord;
  begin
    Rem := $4000 - (FURLSTRStream.Size mod $4000);
    Len := 9 + Length(AString);  // 2 dwords the string and NT
    if Rem < Len then
    begin
      while Rem > 0 do
      begin
        FURLSTRStream.WriteByte(0);
        Dec(Rem);
      end;
    end;
  end;

  function AddURLString(const AString: string): DWord;
  var
    UrlStrRec: TUrlStrIndex;
  begin
    CheckURLStrBlockCanHold(AString);
    if FURLSTRStream.Size mod $4000 = 0 then
      FURLSTRStream.WriteByte(0);
    Result := FURLSTRStream.Position;
    UrlStrRec := TUrlStrIndex.Create;
    UrlStrRec.UrlStr := AString;
    UrlStrRec.UrlStrid := Result;
    FAvlUrlStr.Add(UrlStrRec);
    FURLSTRStream.WriteDWord(NToLE(DWord(0)));
    // URL Offset for topic after the the "Local" value
    FURLSTRStream.WriteDWord(NToLE(DWord(0))); // Offset of FrameName??
    if Length(AString) > 0 then
      FURLSTRStream.Write(AString[1], Length(AString));
    FURLSTRStream.WriteByte(0); //NT
  end;

  function LookupUrlString(const AUrl: string): DWord;
  var
    Node: TAvlTreeNode;
  begin
    SpareUrlStr.UrlStr := AUrl;
    Node := FAvlUrlStr.FindKey(SpareUrlStr, @CompareUrlStrs);
    if Assigned(Node) then
      Result := TUrlStrIndex(Node.Data).UrlStrId
    else
      Result := AddUrlString(AUrl);
  end;

var
  UrlIndex: Integer;

begin
  if (Length(AURL) > 0) and (AURL[1] = '/') then
    Delete(AURL, 1, 1);
  UrlIndex := LookupUrlString(AUrl);

  //if $1000 - (FURLTBLStream.Size mod $1000) = 4 then // we are at 4092
  if FURLTBLStream.Size and $FFC = $FFC then // faster :)
    FURLTBLStream.WriteDWord(0);
  Result := FURLTBLStream.Position;
  FURLTBLStream.WriteDWord(0);//($231e9f5c); //unknown
  FURLTBLStream.WriteDWord(NtoLE(TopicsIndex)); // Index of topic in #TOPICS
  FURLTBLStream.WriteDWord(NtoLE(UrlIndex));
end;

procedure TChmWriter.CheckFileMakeSearchable(AStream: TStream;
  const AFileEntry: TFileEntryRec);
var
  ATitle: string;
begin
  if Pos('.ht', AFileEntry.Name) > 0 then
  begin
    ATitle := FIndexedFiles.IndexFile(AStream, NextTopicIndex, FSearchTitlesOnly);
    AddTopic(ATitle, AFileEntry.Path + AFileEntry.Name, -1);
  end;
end;

function TChmWriter.AddTopic(ATitle, AnUrl: ansistring; code: Integer = -1): Integer;
var
  TopicEntry: TTopicEntry;
begin
  AnUrl := StringReplace(AnUrl, '\', '/', [rfReplaceAll]);
  if ATitle <> '' then
    TopicEntry.StringsOffset := AddString(ATitle)
  else
    TopicEntry.StringsOffset := $FFFFFFFF;
  Result := NextTopicIndex;
  TopicEntry.URLTableOffset := AddURL(AnUrl, Result);
  if code = -1 then
  begin
    if ATitle <> '' then
      TopicEntry.InContents := 6
    else
      TopicEntry.InContents := 2;
    if Pos('#', AnUrl) > 0 then
      TopicEntry.InContents := 0;
  end
  else
    TopicEntry.InContents := code;

  Inc(FNrTopics);
  TopicEntry.Unknown := 0;
  TopicEntry.TocOffset := 0;
  FTopicsStream.WriteDWord(LEtoN(TopicEntry.TocOffset));
  FTopicsStream.WriteDWord(LEtoN(TopicEntry.StringsOffset));
  FTopicsStream.WriteDWord(LEtoN(TopicEntry.URLTableOffset));
  FTopicsStream.WriteWord(LEtoN(TopicEntry.InContents));
  FTopicsStream.WriteWord(LEtoN(TopicEntry.Unknown));
end;

procedure TChmWriter.ScanSitemap(ASiteMap: TCHMSiteMap);

  procedure ScanItems(it: TChmSiteMapItems);
  var
    i: Integer;
    x: TChmSiteMapItem;
    //s: string;
    //StrRec: TStringIndex;
  begin
    for i := 0 to it.Count - 1 do
    begin
      x := it.item[i];
      //      if sanitizeurl(fbasepath, x.local, S) then   // sanitize, remove stuff etc.
      //        begin
      //          writeln(x.text,' : ', x.local, ' ', x.url, ' ' , x.merge);

      if Assigned(x.Children) and (x.Children.Count > 0) then
        ScanItems(x.Children);
    end;
  end;

begin
  ScanItems(ASiteMap.Items);
end;

function TChmWriter.NextTopicIndex: Integer;
begin
  Result := FTopicsStream.Size div 16;
end;

procedure TChmWriter.AppendTOC(AStream: TStream);
var
  tmpStr: string;
begin
  FHasTOC := True;
  if FTocName = '' then
    tmpStr := DefaultHHC
  else
    tmpStr := FTocName;
  PostAddStreamToArchive(tmpStr, '/', AStream, True);
end;

procedure TChmWriter.AppendBinaryTOCFromSiteMap(ASiteMap: TChmSiteMap);
var
  Header: TTOCIdxHeader;
  Entry: TTocEntry;
  EntryInfo: TTOCEntryPageBookInfo;

  EntryInfoStream, EntryTopicOffsetStream, EntryStream: TMemoryStream;

  TOCIDXStream: TMemoryStream;

  NextLevelItems, CurrentLevelItems: TFPList;
  i, j: Integer;
  MenuItem: TChmSiteMapItem;
  MenuItems: TChmSiteMapItems;
  TopicEntry: TTopicEntry;
  EntryCount: DWord = $29A;

  procedure FixParentBookFirstChildOffset(AChildOffset: DWord);
  var
    ParentEntry: TTOCEntryPageBookInfo;
  begin
    // read parent entry
    EntryInfoStream.Position := MenuItems.InternalData;
    EntryInfoStream.Read(ParentEntry, SizeOf(ParentEntry));
    // update child offset
    ParentEntry.FirstChildOffset := NtoLE(DWord(4096 + AChildOffset));
    // write back to stream
    EntryInfoStream.Position := MenuItems.InternalData;
    EntryInfoStream.Write(ParentEntry, SizeOf(ParentEntry));
    // move to end of stream
    EntryInfoStream.Position := AChildOffset;
  end;

begin
  FillChar(Header, SizeOf(Header), 0);
  // create streams
  TOCIDXStream := TMemoryStream.Create();
  EntryInfoStream := TMemoryStream.Create();
  EntryTopicOffsetStream := TMemoryStream.Create();
  EntryStream := TMemoryStream.Create();

  try
    NextLevelItems := TFPList.Create();
    NextLevelItems.Add(ASiteMap.Items);

    if NextLevelItems.Count = 0 then
      FreeAndNil(NextLevelItems);

    while NextLevelItems <> nil do
    begin
      CurrentLevelItems := NextLevelItems;
      NextLevelItems := TFPList.Create;

      for i := 0 to CurrentLevelItems.Count - 1 do
      begin
        MenuItems := TChmSiteMapItems(CurrentLevelItems.Items[i]);

        for j := 0 to MenuItems.Count - 1 do
        begin
          MenuItem := MenuItems.Item[j];
          // first figure out the props
          EntryInfo.Props := 0;
          if MenuItem.Children.Count > 0 then
            EntryInfo.Props := EntryInfo.Props or TOC_ENTRY_HAS_CHILDREN;
          if Length(MenuItem.Local) > 0 then
            EntryInfo.Props := EntryInfo.Props or TOC_ENTRY_HAS_LOCAL;


          if EntryInfo.Props and TOC_ENTRY_HAS_LOCAL > 0 then
          begin
            // Write #TOPICS entry
            TopicEntry.TocOffset := NtoLE(DWord(4096 + EntryInfoStream.Position));
            TopicEntry.StringsOffset := NtoLE(AddString(MenuItem.Text));
            TopicEntry.URLTableOffset := NtoLE(AddURL(MenuItem.Local, NextTopicIndex));
            TopicEntry.InContents := NtoLE(word(2));
            TopicEntry.Unknown := 0;
            EntryInfo.TopicsIndexOrStringsOffset := NtoLE(Dword(NextTopicIndex));

            FTopicsStream.Write(TopicEntry, SizeOf(TopicEntry));
            EntryTopicOffsetStream.WriteDWord(EntryInfo.TopicsIndexOrStringsOffset);

            // write TOCEntry
            Entry.PageBookInfoOffset := NtoLE(4096 + EntryInfoStream.Position);
            Entry.IncrementedInt := NtoLE(EntryCount);
            EntryStream.Write(Entry, SizeOf(Entry));
            Inc(EntryCount);

          end
          else
          begin
            EntryInfo.TopicsIndexOrStringsOffset := NtoLE(AddString(MenuItem.Text));
          end;


          // write TOCEntryInfo

          EntryInfo.Unknown1 := 0;
          EntryInfo.EntryIndex := NtoLE(word(EntryCount - $29A));
          //who knows how useful any of this is

          if MenuItems.InternalData <> maxLongint then
            EntryInfo.ParentPageBookInfoOffset := MenuItems.InternalData
          else
            EntryInfo.ParentPageBookInfoOffset := 0;

          if j = MenuItems.Count - 1 then
            EntryInfo.NextPageBookOffset := 0
          else if (EntryInfo.Props and TOC_ENTRY_HAS_CHILDREN) > 0 then
            EntryInfo.NextPageBookOffset := 4096 + EntryInfoStream.Position + 28
          else
            EntryInfo.NextPageBookOffset := 4096 + EntryInfoStream.Position + 20;

          // Only if TOC_ENTRY_HAS_CHILDREN is set are these written
          EntryInfo.FirstChildOffset := 0; // we will update this when the child is written
          // in fact lets update the *parent* of this item now if needed
          if (j = 0) and (MenuItems.InternalData <> maxLongint) then
            FixParentBookFirstChildOffset(EntryInfoStream.Position);

          EntryInfo.Unknown3 := 0;

          // fix endian order
          EntryInfo.Props := NtoLE(EntryInfo.Props);
          EntryInfo.ParentPageBookInfoOffset := NtoLE(EntryInfo.ParentPageBookInfoOffset);
          EntryInfo.NextPageBookOffset := NtoLE(EntryInfo.NextPageBookOffset);

          if MenuItem.Children.Count > 0 then
          begin
            NextLevelItems.Add(MenuItem.Children);
            MenuItem.Children.InternalData := EntryInfoStream.Position;
          end;

          // write to stream
          EntryInfoStream.Write(EntryInfo, PageBookInfoRecordSize(@EntryInfo));
        end;
      end;

      FreeAndNil(CurrentLevelItems);
      if NextLevelItems.Count = 0 then
        FreeAndNil(NextLevelItems);
    end;

    // write all streams to TOCIdxStream and free everything
    EntryInfoStream.Position := 0;
    EntryTopicOffsetStream.Position := 0;
    EntryStream.Position := 0;

    Header.BlockSize := NtoLE(DWord(4096));
    Header.EntriesCount := NtoLE(DWord(EntryCount - $29A));
    Header.EntriesOffset := NtoLE(DWord(4096 + EntryInfoStream.Size +
      EntryTopicOffsetStream.Size));
    Header.TopicsOffset := NtoLE(DWord(4096 + EntryInfoStream.Size));

    TOCIDXStream.Write(Header, SizeOf(Header));

    TOCIDXStream.CopyFrom(EntryInfoStream, EntryInfoStream.Size);
    EntryInfoStream.Size := 0;
    TOCIDXStream.CopyFrom(EntryTopicOffsetStream, EntryTopicOffsetStream.Size);
    EntryTopicOffsetStream.Size := 0;
    TOCIDXStream.CopyFrom(EntryStream, EntryStream.Size);
    EntryStream.Size := 0;

    TOCIDXStream.Position := 0;
    AppendBinaryTOCStream(TOCIDXStream);
    FHasBinaryTOC := True;

  finally
    EntryInfoStream.Free();
    EntryTopicOffsetStream.Free();
    EntryStream.Free();
    TOCIDXStream.Free();
  end;
end;

const
  //BinIndexIdent: array[0..1] of char = (CHR($3B), CHR($29));
  AlwaysX44: array[0..15] of char = ('X', '4', '4', #0, #0, #0, #0, #0,
    #0, #0, #0, #0, #0, #0, #0, #0);
  DataEntry: array[0..12] of byte =
    ($00, $00, $00, $00, $05, $00, $00, $00, $80, $00, $00, $00, $00);
{
  IndexStream := TMemoryStream.Create;
  IndexStream.Write(BinIndexIdent,2);
  IndexStream.Write(NToLE(word(2)),2);
  IndexStream.Write(NToLE(word(2048)),2);
  IndexStream.Write(AlwaysX44,sizeof(AlwaysX44));
  IndexStrem.Write (dword(0),2);
}

const
  DefBlockSize = 2048;

type
  TIndexBlock = array[0..DefBlockSize - 1] of byte;

procedure WriteWord(var p: PByte; w: Word); inline;
begin
  PWord(p)^ := NToLE(w);
  Inc(PWord(p));
end;

procedure WriteDWord(var p: PByte; d: DWord); inline;
begin
  PDWord(p)^ := NToLE(d);
  Inc(PDWord(p));
end;

procedure TChmWriter.AppendBinaryIndexFromSiteMap(ASiteMap: TChmSiteMap; chw: Boolean);
var
  IndexStream: TMemoryStream;
  //n           : Integer;
  CurBlock: TIndexBlock;   // current listing block being built
  TestBlock: TIndexBlock;  // each entry is first built here. then moved to curblock
  CurInd: Integer;         // next byte to write in testblock.
  BlockNr: Integer;        // blocknr of block in testblock;
  LastBlockNr: Integer;    // blocknr of last block.
  Entries: Integer;        // Number of entries in this block so far
  TotalEntries: Integer;   // Total number of entries
  MapEntries: Integer;
  IndexBlockNr: Integer;
  BlockInd: Integer;        // next byte to write in blockn[blocknr]
  BlockEntries: Integer;    // entries so far ins blockn[blocknr]
  BlockN: array of TIndexBlock;
  BlockNPlus1: array of TIndexBlock;
  Mod13Value: Integer;      // A value that is increased by 13 for each entry. (?!?!)
  EntryToIndex: Boolean;
  // helper var to make sure the first block is always indexed.
  BlockNPlusIndex: Integer;  // blocks in level n+1 (second part)
  BlockNPlusEntries: Integer;  // The other blocks indexed on creation.
  DataStream, MapStream, PropertyStream: TMemoryStream;

  procedure PrepareCurrentBlock(force: Boolean);
  var
    p: PBTreeBlockHeader;
  begin
  {$ifdef binindex}
    writeln('prepcurblock ', Entries, ' ', lastblock, ' ', blocknr, ' ', indexstream.position);
  {$endif}
    p := @CurBlock[0];
    FillChar(p^, SizeOf(TBtreeBlockHeader), #0);
    p^.Length := NToLE(DefBlockSize - CurInd);
    p^.NumberOfEntries := Entries;
    p^.IndexOfPrevBlock := Cardinal(LastBlockNr); // lastblock can be -1, avoid rangecheck
    p^.IndexOfNextBlock := BlockNr;
    if force and (BlockNr = 0) then   // only one listblock -> no indexblocks.
      p^.IndexOfNextBlock := DWord(-1);
    IndexStream.Write(CurBlock[0], DefBlockSize);
    FillChar(CurBlock[0], DefBlockSize, #0);
    MapStream.Write(NToLE(MapEntries), SizeOf(DWord));
    MapStream.Write(NToLE(BlockNr), Sizeof(DWord));
    MapEntries := TotalEntries;
    CurInd := SizeOf(TBtreeBlockHeader);   // index into current block;
    LastBlockNr := BlockNr;
    Inc(BlockNr);
    Entries := 0;
  {$ifdef binindex}
    writeln('prepcurblock post', indexstream.position);
  {$endif}
  end;

  procedure PrepareIndexBlockN(ListingBlockNr: Integer);
  var
    p: PBTreeIndexBlockHeader;
  begin
    {$ifdef binindex} WriteLn('PrepareIndexBlockN(', ListingBlockNr, ')'); {$endif}
    p := @Blockn[IndexBlockNr];
    p^.Length := DefBlockSize - BlockInd;
    p^.NumberOfEntries := BlockEntries;

    // p^.IndexOfChildBlock  // already entered on block creation, since of first entry, not last.
    Inc(IndexBlockNr);
    BlockEntries := 0;
    BlockInd := 0;
    if IndexBlockNr >= Length(BlockN) then
    begin
      SetLength(BlockN, Length(BlockN) + 1);
      // larger increments also possible. #blocks is kept independantly.
      FillChar(BlockN[0][0], SizeOf(BlockN[0]), #0);
    end;
    p := @Blockn[IndexBlockNr];
    p^.IndexOfChildBlock := ListingBlockNr;
    BlockInd := SizeOf(TBTreeIndexBlockHeader);
  end;

  procedure FinalizeIndexBlockN(p: PByte; var Ind: Integer; xEntries: Integer);
  var
    ph: PBTreeIndexBlockHeader;
  begin
    ph := PBTreeIndexBlockHeader(p);
    ph^.Length := DefBlockSize - Ind;
    ph^.NumberOfEntries := xEntries;
    // p^.IndexOfChildBlock  // already entered on block creation, since of first entry, not last.
    //  Inc(Ind);
  end;

  procedure CurEntryToIndex(EntrySize: Integer);
  var
    p, pEntry: PByte;
    IndexEntrySize: Integer;
  begin
    {$ifdef binindex} WriteLn('CurEntryToIndex(', EntrySize, ')'); {$endif}

    IndexEntrySize := EntrySize - SizeOf(DWord);
    // index entry is 4 bytes shorter, and only the last dword differs
    if (BlockInd + IndexEntrySize) >= DefBlockSize then
      PrepareIndexBlockN(BlockNr);
    p := @BlockN[IndexBlockNr][BlockInd];
    Move(TestBlock[0], p^, IndexEntrySize);
    pEntry := @p[IndexEntrySize - SizeOf(DWord)];         // ptr to last dword
    WriteDWord(pEntry, BlockNr);                      // patch up the "index of child field"
    Inc(BlockInd, IndexEntrySize);
  end;

  procedure CreateEntry(Item: TChmSiteMapItem; Str: WideString; CommaAtPosition: Integer);
  var
    p: PByte;
    TopicID: Integer;
    SeeAlsoID: Integer;
    EntrySize: Integer;
    i: Integer;
  begin
    Inc(TotalEntries);
    FillChar(TestBlock[0], DefBlockSize, #0);
    p := @TestBlock[0];
    for i := 1 to Length(Str) do
      WriteWord(p, Word(Str[i]));   // write the wstr in little endian
    WriteWord(p, 0);                // NT
    //  if item.seealso = '' then    // no seealso for now
    SeeAlsoID := 0;
    // else
    //    SeeAlsoID := 2;
    WriteWord(p, SeeAlsoID);        // =0 not a see also 2 =seealso
    WriteWord(p, 0);                // Entrydepth.  We can't know it, so write 2.
    WriteDWord(p, CommaAtPosition); // position of the comma
    WriteDWord(p, 0);               // unused 0
    WriteDWord(p, 1);               // for now only local pair.
    TopicID := AddTopic(Item.Text, Item.Local);
    WriteDWord(p, TopicID);
    // if seealso then _here_ a wchar NT string with seealso?
    WriteDWord(p, 1);               // always 1 (unknown);
    WriteDWord(p, Mod13Value);      //a value that increments with 13.
    Mod13Value := Mod13Value + 13;
    EntrySize := p - PByte(@TestBlock[0]);
    {$ifdef binindex}
    WriteLn(CurInd, ' ', EntrySize, ' ', DefBlockSize);
    WriteLn('curstr ', str, ' ', CommaAtPosition);
    {$endif}
    if (CurInd + EntrySize) >= DefBlockSize then
    begin
      {$ifdef binindex} WriteLn('larger!'); {$endif}
      PrepareCurrentBlock(False);
      EntryToIndex := True;
    end;
    if EntryToIndex then
    begin
      {$ifdef binindex} WriteLn('entrytoindex'); {$endif}
      CurEntryToIndex(EntrySize);
      EntryToIndex := False;
    end;
    Move(TestBlock[0], CurBlock[CurInd], EntrySize);
    Inc(CurInd, EntrySize);
    DataStream.Write(DataEntry, SizeOf(DataEntry));
    Inc(Entries);
  end;

  procedure MoveIndexEntry(nr: Integer; ABytes: Integer; AChildBlock: Integer);
  var
    pSrc, pDest: PByte;
  begin
    {$ifdef binindex}
    WriteLn(' MoveIndexEntry(', nr, ', bytes:', ABytes, ', ChildBlock:', AChildBlock, ')');
    Flush(stdout);
    {$endif}

    if ((BlockInd + ABytes) >= DefBlockSize) then
    begin
      {$ifdef binindex}
      WriteLn(' in scalecheck  ', BlockInd);
      Flush(stdout);
      {$endif}

      FinalizeIndexBlockN(@BlockNPlus1[BlockNPlusIndex][0], BlockInd, BlockNPlusEntries);
      Inc(BlockNPlusIndex);
      if BlockNPlusIndex >= Length(BlockNPlus1) then
      begin
        SetLength(BlockNPlus1, Length(BlockNPlus1) + 1);
        FillChar(BlockNPlus1[Length(BlockNPlus1) - 1][0], SizeOf(BlockNPlus1[0]), #0);
      end;
      BlockInd := Sizeof(TBTreeIndexBlockHeader);
      PDWord(@BlockNPlus1[BlockNPlusIndex][0])[4] := NToLE(AChildBlock);
      /// init 2nd level index to first 1st level index block
    end;
    {$ifdef binindex}
    WriteLn(' len:', Length(BlockNPlus1), ' BlockInd:', BlockInd, ' Index:', BlockNPlusIndex);
    Flush(stdout);
    {$endif}

    // copy entry from one indexblock to another
    pSrc := @BlockN[nr][SizeOf(TBtreeIndexBlockHeader)];
    pDest := @BlockNPlus1[BlockNPlusIndex][BlockInd];
    Move(pSrc^, pDest^, ABytes);
    PDWord(@pDest[ABytes - SizeOf(DWord)])^ := NToLE(AChildBlock);
    // correcting the childindex
    Inc(BlockInd, ABytes);
    Inc(BlockNPlusEntries);
    // not needed for writing, but used to check if something has been written. End condition
  end;

  function ScanIndexBlock(blk: PByte): Integer;
  var
    start: PByte;
    n: Integer;
    i: Integer;
  begin
    start := @blk[SizeOf(TBtreeIndexBlockHeader)];
    blk := start;
    while PWord(blk)^ <> 0 do   // skip wchar
      Inc(PWord(blk));
    Inc(PWord(blk));          // skip NT
    Inc(PWord(blk));          // skip see also
    Inc(PWord(blk));          // skip depth
    Inc(PDWord(blk));         // skip Character Index.
    Inc(PDWord(blk));          // skip always  0
    n := LEToN(PDWord(blk)^);
    Inc(PDWord(blk));          // skip nr of pairs.
    for i := 1 to n do
      Inc(PDWord(blk));          // skip <n> topicids
    Inc(PDWord(blk));          // skip childindex
    Result := blk - start;
  end;

  procedure CombineWithChildren(ParentItem: TChmSiteMapItem; Str: WideString;
    CommaAtPosition: Integer; First: Boolean);
  var
    i: Integer;
    Item: TChmSiteMapItem;
  begin
    if ParentItem.Children.Count = 0 then
    begin
      // comment/fix next
      //   if CommaAtPosition = Length(str) then CommaAtPosition := 0;
      if First then
        CreateEntry(ParentItem, Str, 0)
      else
        CreateEntry(ParentItem, Str, CommaAtPosition);
    end
    else
      for i := 0 to ParentItem.Children.Count - 1 do
      begin
        Item := TChmSiteMapItem(ParentItem.Children.Item[i]);
        if First then
          CombineWithChildren(Item, Str + ', ' + Item.Text, CommaAtPosition + 2, False)
        else
          CombineWithChildren(Item, Str + ', ' + Item.Text, CommaAtPosition, False);
      end;
  end;

var
  i: Integer;
  Key: WideString;
  Item: TChmSiteMapItem;
  ListingBlocks: Integer;
  EntryBytes: Integer;
  Hdr: TBTreeHeader;
  TreeDepth: Integer;

{$ifdef binindex}
  procedure PrintLoopVars(i: Integer);
  begin
    Writeln('location :', i, ' blocknr :', blocknr, ' level:', TreeDepth);
    Writeln('blockn      length: ', length(blockn), ' indexblocknr: ',
      indexblocknr, ' blockind ', blockind);
    Writeln('blocknplus1 length: ', length(blocknplus1), ' blocknplusindex:',
      blocknplusindex, ' entries:', blocknplusentries);
    Flush(stdout);
  end;
{$endif}

begin
  {$ifdef binindex} WriteLn('starting index'); {$endif}
  IndexStream := TMemoryStream.Create();
  DataStream := TMemoryStream.Create();
  MapStream := TMemoryStream.Create();
  PropertyStream := TMemoryStream.Create();
  try
    IndexStream.Size := SizeOf(TBTreeHeader);
    IndexStream.Position := SizeOf(TBTreeHeader);
    MapStream.Size := 2;
    MapStream.Position := 2;
    PropertyStream.Write(NToLE(0), SizeOf(4));
    // we iterate over all entries and write listingblocks directly to the stream.
    // and the first (and maybe last) level is written to blockn.
    // we can't do higher levels yet because we don't know how many listblocks we get
    BlockNr := 0;   // current block number
    LastBlockNr := -1;  // previous block nr or -1 if none.
    Entries := 0;   // entries in this block
    TotalEntries := 0;   // entries so far.
    Mod13Value := 0;   // value that increments by 13 entirely.
    IndexBlockNr := 0;   // nr of first index block.
    BlockEntries := 0;   // entries into current block;
    MapEntries := 0;   // entries before the current listing block, for MAP file
    TreeDepth := 0;

    FillChar(TestBlock[0], DefBlockSize, #0);
    FillChar(CurBlock[0], DefBlockSize, #0);
    CurInd := SizeOf(TBTreeBlockHeader);      // index into current listing block;
    BlockInd := SizeOf(TBtreeIndexBlockHeader); // index into current index block

    SetLength(BlockN, 1);
    FillChar(BlockN[0][0], SizeOf(BlockN[0]), #0);
    PDWord(@BlockN[0][4])^ := NToLE(0);
    /// init first listingblock nr to 0 in the first index block
    EntryToIndex := True;
    {$ifdef binindex} WriteLn('items:', ASiteMap.Items.Count); {$endif}
    for i := 0 to ASiteMap.Items.Count - 1 do
    begin
      Item := TChmSiteMapItem(ASiteMap.Items.Item[i]);
      Key := Item.Text;
      {$ifdef binindex} WriteLn('item: ', i, ' ', Key); {$endif}

      {$ifdef chm_windowsbinindex}
      // append 2 to all index level 0 entries. This
      // so we can see if Windows loads the binary or textual index.
      CombineWithChildren(Item, Key + '2', Length(Key) + 1, True);
      {$else}
      CombineWithChildren(Item, Key, Length(Key), True);
      {$endif}
    end;
    PrepareCurrentBlock(True);     // flush last listing block.

    ListingBlocks := BlockNr;
    // blocknr is from now on the number of the first block in blockn.
    // we still need the # of listingblocks for the header though
    {$ifdef binindex}
    WriteLn('binindex: listingblocks : ' + IntToStr(ListingBlocks),
      ' indexblocks: ', IndexBlockNr, ' entries:', BlockEntries);
    {$endif}

    // we have now created and written the listing blocks, and created the first level of index in <blockn>
    // the following loop uses <blockn> to calculate the next level (in blocknplus1), then write out blockn,
    // and repeat until we have no entries left.

    // First we finalize the current set of blocks
    if BlockNr > 1 then
    begin
      if BlockInd <> SizeOf(TBtreeIndexBlockHeader) then
      begin
        {$ifdef binindex} WriteLn('finalizing level 1 index'); {$endif}
        FinalizeIndexBlockN(@BlockN[IndexBlockNr][0], BlockInd, BlockEntries);
        // also increasing indexblocknr
        Inc(IndexBlockNr);
      end;
      {$ifdef binindex}
      WriteLn('binindex: listingblocks : ' + IntToStr(ListingBlocks),
        ' indexblocks: ', IndexBlockNr, ' entries:', BlockEntries);
      {$endif}


      while (IndexBlockNr > 1) do
      begin
        {$ifdef binindex} PrintLoopVars(1); {$endif}

        BlockInd := SizeOf(TBtreeIndexBlockHeader);
        PDWord(@BlockN[0][4])^ := NToLE(ListingBlocks);
        /// init 2nd level index to first 1st level index block
        BlockNPlusIndex := 0;
        BlockNPlusEntries := 0;
        if Length(BlockNPlus1) < 1 then
        begin
          SetLength(BlockNPlus1, 1);
          FillChar(BlockNPlus1[0][0], SizeOf(BlockNPlus1[0]), #0);
        end;

        EntryToIndex := True;
        {$ifdef binindex} PrintLoopVars(2); {$endif}
        for i := 0 to IndexBlockNr - 1 do
        begin
          EntryBytes := ScanIndexBlock(@BlockN[i][0]);
          // writeln('after scan ,',i, ' bytes: ',entrybytes,' blocknr:',blocknr,' indexblocknr:',indexblocknr,' to:',blocknr+i);
          MoveIndexEntry(i, EntryBytes, BlockNr + i);
          IndexStream.Write(BlockN[i][0], DefBlockSize);
        end;

        {$ifdef binindex} PrintLoopVars(3); {$endif}

        if BlockInd <> SizeOf(TBtreeIndexBlockHeader) then
        begin
          {$ifdef binindex} LogEntry('finalizing'); {$endif}
          FinalizeIndexBlockn(@BlockNPlus1[BlockNPlusIndex][0], BlockInd, BlockNPlusEntries);
          Inc(BlockNPlusIndex);
        end;

        Inc(BlockNr, IndexBlockNr);

        IndexBlockNr := BlockNPlusIndex;
        BlockN := Copy(BlockNPlus1);
        SetLength(BlockNPlus1, 1);
        {$ifdef binindex} PrintLoopVars(5); {$endif}

        Inc(TreeDepth);
      end;
      IndexStream.Write(BlockN[0][0], DefBlockSize);
      Inc(BlockNr);
    end;
    // Fixup header.
    hdr.Ident[0] := chr($3B);
    hdr.Ident[1] := chr($29);
    hdr.Flags := NToLE(Word($2));
    // bit $2 is always 1, bit $0400 1 if dir? (always on)
    hdr.BlockSize := NToLE(Word(DefBlockSize)); // size of blocks (2048)
    hdr.DataFormat := AlwaysX44;           // "X44" always the same, see specs.
    hdr.Unknown0 := NToLE(0);            // always 0
    hdr.LastLstBlock := NToLE(DWord(ListingBlocks - 1));
    // index of last listing block in the file;
    hdr.IndexRootBlock := NToLE(DWord(BlockNr - 1));
    // Index of the root block in the file.
    hdr.Unknown1 := NToLE(DWord(-1));           // always -1
    hdr.NrBlock := NToLE(BlockNr);      // Number of blocks
    hdr.TreeDepth := NToLE(Word(TreeDepth));
    // The depth of the tree of blocks (1 if no index blocks, 2 one level of index blocks, ...)
    hdr.NrKeyWords := NToLE(TotalEntries); // number of keywords in the file.
    hdr.CodePage := NToLE(DWord(1252));
    // Windows code page identifier (usually 1252 - Windows 3.1 US (ANSI))
    hdr.LCID := NToLE(LocaleID);            //  ???? LCID from the HHP file.
    if not chw then
      hdr.IsChm := NToLE(DWord(1))
      // 0 if this a BTREE and is part of a CHW file, 1 if it is a BTree and is part of a CHI or CHM file
    else
      hdr.IsChm := NToLE(0);
    hdr.Unknown2 := NToLE(DWord(10031));
    // Unknown. Almost always 10031. Also 66631 (accessib.chm, ieeula.chm, iesupp.chm, iexplore.chm, msoe.chm, mstask.chm, ratings.chm, wab.chm).
    hdr.Unknown3 := NToLE(0);            // unknown 0
    hdr.Unknown4 := NToLE(0);            // unknown 0
    hdr.Unknown5 := NToLE(0);            // unknown 0

    IndexStream.Position := 0;
    IndexStream.Write(hdr, SizeOf(hdr));
    {$ifdef binindex}
    LogEntry('before append');
    {$endif}

    AppendBinaryIndexStream(IndexStream, DataStream, MapStream, PropertyStream, chw);
    FHasBinaryIndex := True;

  finally
    IndexStream.Free();
    PropertyStream.Free();
    MapStream.Free();
    DataStream.Free();
  end;
  FHasKLinks := TotalEntries > 0;
  {$ifdef binindex}
  WriteLn('end index');
  {$endif}

end;

procedure TChmWriter.AppendBinaryTOCStream(AStream: TStream);
begin
  AddStreamToArchive('#TOCIDX', '/', AStream, True);
end;

procedure TChmWriter.AppendBinaryIndexStream(
  IndexStream, DataStream, MapStream, PropertyStream: TStream; chw: Boolean);

  procedure StAdd(fn: string; Stream: TStream);
  begin
    Stream.Position := 0;
    if CHW then
      fn := UpperCase(fn);
  {$ifdef binindex}
    logentry('before append ' + fn);
  {$endif}
    AddStreamToArchive(fn, '/$WWKeywordLinks/', Stream, True);
  end;

begin
  AddDummyALink();
  StAdd('BTree', IndexStream);
  StAdd('Data', DataStream);
  StAdd('Map', MapStream);
  StAdd('Property', PropertyStream);
end;

procedure TChmWriter.AppendIndex(AStream: TStream);
var
  tmpStr: string;
begin
  FHasIndex := True;
  if FIndexName = '' then
    tmpStr := DefaultHHK
  else
    tmpStr := FIndexName;
  PostAddStreamToArchive(tmpStr, '/', AStream, True);
end;

procedure TChmWriter.AppendSearchDB(AName: string; AStream: TStream);
begin
  PostAddStreamToArchive(AName, '/', AStream);
end;


procedure TChmWriter.AddContext(AContext: DWord; AUrl: string);
var
  Offset: DWord;
begin
  if not Assigned(FContextStream) then
  begin
    FContextStream := TMemoryStream.Create();
    // #IVB starts with a dword which is the size of the stream - sizeof(dword)
    FContextStream.WriteDWord(0);
    // we will update this when we write the file to the final stream
  end;
  // an entry is a context id and then the offset of the name of the topic in the strings file
  FContextStream.WriteDWord(NToLE(AContext));
  Offset := NToLE(AddString(AUrl));
  FContextStream.WriteDWord(Offset);
end;

procedure TChmWriter.AddDummyALink();
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create();
  try
    Stream.WriteDWord(0);
    Stream.Position := 0;
    AddStreamToArchive('Property', '/$WWAssociativeLinks/', Stream, True);
  finally
    Stream.Free();
  end;
end;

procedure TChmWriter.SetWindows(AWindowList: TCHMWindowList);
var
  i: Integer;
  x: TCHMWindow;
begin
  Windows.Clear();
  for i := 0 to AWindowList.Count - 1 do
  begin
    x := TChmWindow.Create();
    x.Assign(AWindowList[i]);
    Windows.Add(x);
  end;
end;

procedure TChmWriter.SetMergeFiles(Src: TStringList);
var
  i: Integer;
begin
  FMergeFiles.Clear();
  for i := 0 to Src.Count - 1 do
    FMergefiles.Add(Src[i]);
end;

end.
