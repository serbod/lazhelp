{ Copyright (C) <2005> <Andrew Haines> chmtypes.pas

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
unit chmtypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, xmlcfg;

type
  TSectionName = (snMSCompressed, snUnCompressed);

  TSectionNames = set of TSectionName;

   { TDirectoryChunk }

  TDirectoryChunk = class(TObject)
  private
    FHeaderSize: Integer;
    FQuickRefEntries: Word;
    Buffer: array[0..$1000-1] of byte;
    CurrentPos: Integer;
    FItemCount: Word;
    FClearCount: Integer;
  public
    function CanHold(ASize: Integer): Boolean;
    function FreeSpace: Integer;
    procedure WriteHeader(AHeader: Pointer);
    procedure WriteEntry(Size: Integer; Data: Pointer);
    procedure WriteChunkToStream(Stream: TStream); overload;
    procedure Clear;
    property ItemCount: Word read FItemCount;
    constructor Create(AHeaderSize: Integer);
  end;

  { TPMGIDirectoryChunk }

  TPMGIDirectoryChunk = class(TDirectoryChunk)
  private
    FChunkLevelCount: Integer;
    FParentChunk: TPMGIDirectoryChunk;
  public
    procedure WriteChunkToStream(Stream: TStream; var AIndex: Integer; Final: Boolean = False); overload;
    property ParentChunk: TPMGIDirectoryChunk read FParentChunk write FParentChunk;
    property ChunkLevelCount: Integer read FChunkLevelCount write FChunkLevelCount;
  end;

  PFileEntryRec = ^TFileEntryRec;
  TFileEntryRec = record
    Path: String;
    Name: String;
    DecompressedOffset: QWord;
    DecompressedSize: QWord;
    Compressed: Boolean; // True means it goes in section1 False means section0
  end;

  { TFileEntryList }

  TFileEntryList = class(TList)
  private
    FPaths: TStringList;
    function GetFileEntry(Index: Integer): TFileEntryRec;
    procedure SetFileEntry(Index: Integer; const AValue: TFileEntryRec);
  public
    function AddEntry(AFileEntry: TFileEntryRec; CheckPathIsAdded: Boolean = True): Integer;
    procedure Delete(Index: Integer);
    property FileEntry[Index: Integer]: TFileEntryRec read GetFileEntry write SetFileEntry;
    procedure Sort;
    constructor Create;
    destructor Destroy; override;

  end;

  TChmWindowEntry = packed record
    EntrySize: LongWord;       // Size of the entry (188 in CHMs compiled with Compatibility set to 1.0, 196 in CHMs compiled with Compatibility set to 1.1 or later)
    IsUnicode: LongWord;       // 0 (unknown) - but htmlhelp.h indicates that this is "BOOL fUniCodeStrings; // IN/OUT: TRUE if all strings are in UNICODE"
    WindowTypeStr: LongWord;   // The window type. Offset in #STRINGS file.
    WindowFlags: LongWord;     // Which window properties are valid & are to be used for this window.
    NavStyleFlags: LongWord;   // A bit feild of navigation pane styles.
    TitleBarStr: LongWord;     // The title bar text. Offset in #STRINGS file.
    StyleFlags: LongWord;      // Style Flags. As set in the Win32 SetWindowLong & CreateWindow APIs.
    ExStyleFlags: LongWord;    // Extended Style Flags. As set in the Win32 SetWindowLong & CreateWindowEx APIs
    WindowPosition: array [0..3] of LongWord; // Initial position of the window on the screen: [left, top, right, bottom].
    WinShowState: LongWord;    // Window show state. As set in the Win32 ShowWindow API.
    HelpHandle: LongWord;      // Help window handle
    CallerHandle: LongWord;    // who called this window
    InfoTypesPtr: LongWord;    // Pointer to an array of Information Types
    ToolBarHandle: LongWord;   // toolbar window in tri-pane window
    NavHandle: LongWord;       // navigation window in tri-pane window
    HtmlHandle: LongWord;      // window displaying HTML in tri-pane window
    NavWidth: LongWord;        // Width of the navigation pane in pixels.
    TopicPaneRect: array [0..3] of LongWord; // Specifies the coordinates of the Topic pane.
    TocFileStr: LongWord;      // The TOC file. Offset in #STRINGS file.
    IndexFileStr: LongWord;    // The Index file. Offset in #STRINGS file.
    DefaultFileStr: LongWord;  // The Default file. Offset in #STRINGS file.
    HomeFileStr: LongWord;     // The file shown when the Home button is pressed. Offset in #STRINGS file.
    ButtonsFlags: LongWord;    // A bit field of the buttons to show.
    IsNavPaneClosed: LongWord; // Whether or not the navigation pane is initially closed. 1 = closed, 0 = open
    NavPaneDefault: LongWord;  // The default navigation pane. 0 = TOC, 1 = Index, 2 = Search, 3 = Favorites, 4 = History (not implemented by HH), 5 = Author, 11-19 = Custom panes.
    NavPaneLocation: LongWord; // Where the navigation pane tabs should be. 0 = Top, 1 = Left, 2 = Bottom & anything else makes the tabs appear to be behind the pane on Win95.
    WmNotifyId: LongWord;      // ID to send in WM_NOTIFY messages.
    TabOrder: array [0..4] of LongWord; // tab order: Contents, Index, Search, History, Favorites, Reserved 1-5, Custom tabs
    HistoryDepth: LongWord;    // number of history items to keep (default is 30)
    Jump1TextStr: LongWord;    // The text of the Jump 1 button. Offset in #STRINGS file.
    Jump2TextStr: LongWord;    // The text of the Jump 2 button. Offset in #STRINGS file.
    Jump1FileStr: LongWord;    // The file shown when the Jump 1 button is pressed. Offset in #STRINGS file.
    Jump2FileStr: LongWord;    // The file shown when the Jump 2 button is pressed. Offset in #STRINGS file.
    MinWindowSize: array [0..3] of LongWord; // Minimum size for window (ignored in version 1)
    // CHM 1.1 and later
    InfoTypeSize: LongWord;    // size of paInfoTypes
    CustomTabs: LongWord;      // multiple zero-terminated strings
  end;

  TValidWindowFieldsEnum = (valid_Unknown1 {:=1},
                            valid_Navigation_pane_style {:= 2},
                            valid_Window_style_flags {:= 4},
                            valid_Window_extended_style_flags {:= 8},
                            valid_Initial_window_position    {:= $10},
                            valid_Navigation_pane_width {:= $20},
                            valid_Window_show_state {:= $40},
                            valid_Info_types {:= $80},
                            valid_Buttons {:= $100},
                            valid_Navigation_Pane_initially_closed_state {:= $200},
                            valid_Tab_position {:= $400},
                            valid_Tab_order {:= $800},
                            valid_History_count{ := $1000},
                            valid_Default_Pane {:= $2000});

  TValidWindowFields     = Set Of TValidWindowFieldsEnum;

  { TCHMWindow }

  TCHMWindow = class
  public
    window_type,
    Title_bar_text,
    Toc_file,
    index_file,
    Default_File,
    Home_button_file,
    Jumpbutton_1_File,
    Jumpbutton_1_Text,
    Jumpbutton_2_File,
    Jumpbutton_2_Text : string;
    nav_style    : integer;  // overlay with bitfields (next 2 also)
    navpanewidth : integer;
    buttons      : integer;
    left,
    top,
    right,
    bottom       : integer;
    styleflags   ,
    xtdstyleflags,
    window_show_state,
    navpane_initially_closed,
    navpane_default,
    navpane_location,
    wm_notify_id : integer;
    flags : TValidWindowFields; // bitset that keeps track of which fields are filled.
                                // of certain fields. Needs to be inserted into #windows stream
    constructor Create(s: string = '');
    procedure load_from_ini(txt: string);
    procedure SaveToXml(cfg: TXMLConfig; key: string);
    procedure LoadFromXml(cfg: TXMLConfig; key: string);
    procedure Assign(obj: TCHMWindow);
  end;

  { TCHMWindowList }

  TCHMWindowList = class(TList)
  protected
    procedure PutItem(Index: Integer; AValue: TCHMWindow);
    function GetItem(Index: Integer): TCHMWindow;
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    OwnsObjects: Boolean;
    property Items[Index: Integer]: TCHMWindow read GetItem write PutItem; default;
  end;

  TTOCIdxHeader = record
    BlockSize: DWord; // 4096
    EntriesOffset: DWord;
    EntriesCount: DWord;
    TopicsOffset: DWord;
    EmptyBytes: array[0..4079] of byte;
  end;

const
  TOC_ENTRY_HAS_NEW      = 2;
  TOC_ENTRY_HAS_CHILDREN = 4;
  TOC_ENTRY_HAS_LOCAL    = 8;

type
  PTOCEntryPageBookInfo = ^TTOCEntryPageBookInfo;
  TTOCEntryPageBookInfo = record
    Unknown1: Word; //  = 0
    EntryIndex: Word; // multiple entry info's can have this value but the TTocEntry it points to points back to the first item with this number. Wierd.
    Props: DWord; // BitField. See TOC_ENTRY_*
    TopicsIndexOrStringsOffset: DWord; // if TOC_ENTRY_HAS_LOCAL is in props it's the Topics Index
                                       // else it's the Offset In Strings of the Item Text
    ParentPageBookInfoOffset: DWord;
    NextPageBookOffset: DWord; // same level of tree only

    // Only if TOC_ENTRY_HAS_CHILDREN is set are these written
    FirstChildOffset: DWord;
    Unknown3: DWord; // = 0
  end;

  TTocEntry = record
    PageBookInfoOffset: DWord;
    IncrementedInt: DWord; // first is $29A
    TopicsIndex: DWord; // Index of Entry in #TOPICS file
  end;

  TTopicEntry = record
    TocOffset: DWord;
    StringsOffset: DWord;
    URLTableOffset: DWord;
    InContents: Word;// 2 = in contents 6 = not in contents
    Unknown: Word; // 0,2,4,8,10,12,16,32
  end;

  TBtreeHeader = packed record
    Ident          : array[0..1] of AnsiChar; // $3B $29
    Flags          : Word;	// bit $2 is always 1, bit $0400 1 if dir? (always on)
    BlockSize      : Word;  // size of blocks (2048)
    DataFormat     : array[0..15] of AnsiChar;  // "X44" always the same, see specs.
    Unknown0       : DWord; // always 0
    LastLstBlock   : DWord; // index of last listing block in the file;
    IndexRootBlock : DWord; // Index of the root block in the file.
    Unknown1       : DWord; // always -1
    NrBlock        : DWord; // Number of blocks
    TreeDepth      : Word;  // The depth of the tree of blocks (1 if no index blocks, 2 one level of index blocks, ...)
    NrKeyWords     : DWord; // number of keywords in the file.
    CodePage       : DWord; // Windows code page identifier (usually 1252 - Windows 3.1 US (ANSI))
    LCID           : DWord; // LCID from the HHP file.
    IsChm          : DWord; // 0 if this a BTREE and is part of a CHW file, 1 if it is a BTree and is part of a CHI or CHM file
    Unknown2       : DWord; // Unknown. Almost always 10031. Also 66631 (accessib.chm, ieeula.chm, iesupp.chm, iexplore.chm, msoe.chm, mstask.chm, ratings.chm, wab.chm).
    Unknown3       : DWord; // unknown 0
    Unknown4       : DWord; // unknown 0
    Unknown5       : DWord; // unknown 0
  end;

  PBTreeBlockHeader = ^TBtreeBlockHeader;
  TBtreeBlockHeader = packed record
    Length             : Word;  // Length of free space at the end of the block.
    NumberOfEntries    : Word;  // Number of entries in the block.
    IndexOfPrevBlock   : DWord; // Index of the previous block. -1 if this is the first listing block.
    IndexOfNextBlock   : DWord; // Index of the next block. -1 if this is the last listing block.
  end;

  PBtreeBlockEntry = ^TBtreeBlockEntry;
  TBtreeBlockEntry = packed record
    IsSeeAlso  : Word; // 2 if this keyword is a See Also keyword, 0 if it is not.
    EntryDepth : Word; // Depth of this entry into the tree.
    CharIndex  : DWord;// Character index of the last keyword in the ", " separated list.
    Unknown0   : DWord;// 0 (unknown)
    NrPairs    : DWord;// Number of Name, Local pairs
  end;

  PBtreeIndexBlockHeader = ^TBtreeIndexBlockHeader;
  TBtreeIndexBlockHeader = packed record
    Length             : Word;  // Length of free space at the end of the block.
    NumberOfEntries    : Word;  // Number of entries in the block.
    IndexOfChildBlock  : DWord; // Index of Child Block
  end;

  PBtreeIndexBlockEntry = ^TBtreeIndexBlockEntry;
  TBtreeIndexBlockEntry = packed record
    IsSeeAlso  : Word; // 2 if this keyword is a See Also keyword, 0 if it is not.
    EntryDepth : Word; // Depth of this entry into the tree.
    CharIndex  : DWord;// Character index of the last keyword in the ", " separated list.
    Unknown0   : DWord;// 0 (unknown)
    NrPairs    : DWord;// Number of Name, Local pairs
  end;

const
  IdxHdrMagic = 'T#SM';

type
  // #IDXHDR
  TIdxHdr = packed record
    IdxHdrSig: array [0..3] of AnsiChar; // 'T#SM'
    Unknown_04: LongWord;    // Unknown timestamp/checksum
    Unknown_08: LongWord;    // 1 (unknown)
    TopicsCount: LongWord;   // Number of topic nodes including the contents & index files
    Unknown_10: LongWord;    // 0 (unknown)
    ImageListStr: LongWord;  // Offset in the #STRINGS file of the ImageList param of the "text/site properties" object of the sitemap contents (0/-1 = none)
    Unknown_18: LongWord;    // 0 (unknown)
    ImageType: LongWord;     // 1 if the value of the ImageType param of the "text/site properties" object of the sitemap contents is Folder. 0 otherwise.
    Background: LongWord;    // The value of the Background param of the "text/site properties" object of the sitemap contents
    Foreground: LongWord;    // The value of the Foreground param of the "text/site properties" object of the sitemap contents
    FontStr: LongWord;       // Offset in the #STRINGS file of the Font param of the "text/site properties" object of the sitemap contents (0/-1 = none)
    WindowStyles: LongWord;  // The value of the Window Styles param of the "text/site properties" object of the sitemap contents
    ExWindowStyles: LongWord; // The value of the ExWindow Styles param of the "text/site properties" object of the sitemap contents
    Unknown_34: LongWord;    // Unknown. Often -1. Sometimes 0.
    FrameNameStr: LongWord;  // Offset in the #STRINGS file of the FrameName param of the "text/site properties" object of the sitemap contents (0/-1 = none)
    WindowNameStr: LongWord; // Offset in the #STRINGS file of the WindowName param of the "text/site properties" object of the sitemap contents (0/-1 = none)
    InfoTypesCount: LongWord; // Number of information types
    Unknown_44: LongWord;    // Unknown. Often 1. Also 0, 3.
    MergeFilesCount: LongWord; // Number of files in the [MERGE FILES] list.
    Unknown_4C: LongWord;    // Unknown. Often 0. Non-zero mostly in files with some files in the merge files list.
    MergeFilesList: array [0..1003] of LongWord; // List of offsets in the #STRINGS file that are the [MERGE FILES] list. Zero terminated, but don't count on it.
  end;

function PageBookInfoRecordSize(ARecord: PTOCEntryPageBookInfo): Integer;

const DefValidFlags = [valid_Navigation_pane_style, valid_Window_style_flags, valid_Initial_window_position, valid_Navigation_pane_width, valid_Buttons, valid_Tab_position];

implementation

uses chmbase;

function PageBookInfoRecordSize(ARecord: PTOCEntryPageBookInfo): Integer;
begin
  if (TOC_ENTRY_HAS_CHILDREN and ARecord^.Props) > 0 then
    Result := 28
  else
    Result := 20;
end;

{ TCHMWindowList }

procedure TCHMWindowList.PutItem(Index: Integer; AValue: TCHMWindow);
begin
  Put(Index, AValue);
end;

procedure TCHMWindowList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited Notify(Ptr, Action);
  if (Action = lnDeleted) and OwnsObjects then
    TCHMWindow(Ptr).Free();
end;

function TCHMWindowList.GetItem(Index: Integer): TCHMWindow;
begin
  Result := TCHMWindow(Get(Index));
end;

{ TDirectoryChunk }

function TDirectoryChunk.CanHold(ASize: Integer): Boolean;
begin
  Result := CurrentPos < $1000 - ASize - (SizeOf(Word) * (FQuickRefEntries+2));
end;

function TDirectoryChunk.FreeSpace: Integer;
begin
  Result := $1000 - CurrentPos;
end;

procedure TDirectoryChunk.WriteHeader(AHeader: Pointer);
begin
  Move(AHeader^, Buffer[0], FHeaderSize);
end;

procedure TDirectoryChunk.WriteEntry(Size: Integer; Data: Pointer);
var
  ReversePos: Integer;
  Value: Word;
begin
  if not CanHold(Size) then Raise Exception.Create('Trying to write past the end of the buffer');
  Move(Data^, Buffer[CurrentPos], Size);
  Inc(CurrentPos, Size);
  Inc(FItemCount);

  // now put a quickref entry if needed
  if ItemCount mod 5 = 0 then
  begin
    Inc(FQuickRefEntries);
    ReversePos := ($1000) - SizeOf(Word) - (SizeOf(Word)*FQuickRefEntries);
    Value := NtoLE(Word(CurrentPos - Size - FHeaderSize));
    Move(Value, Buffer[ReversePos], SizeOf(Word));
  end;
end;

procedure TDirectoryChunk.WriteChunkToStream(Stream: TStream);
var
  ReversePos: Integer;
  TmpItemCount: Word;
begin
  ReversePos := $1000 - SizeOf(Word);
  TmpItemCount := NtoLE(Word(FItemCount));
  Move(TmpItemCount, Buffer[ReversePos], SizeOf(Word));

  Stream.Write(Buffer[0], $1000);
  {$IFDEF DEBUG_CHM_CHUNKS}
  WriteLn('Writing ', Copy(PChar(@Buffer[0]),0,4),' ChunkToStream');
  {$ENDIF}
end;

procedure TDirectoryChunk.Clear();
begin
  FillChar(Buffer, $1000, 0);
  FItemCount := 0;
  CurrentPos := FHeaderSize;
  FQuickRefEntries := 0;
  Inc(FClearCount);
end;

constructor TDirectoryChunk.Create(AHeaderSize: Integer);
begin
  FHeaderSize := AHeaderSize;
  CurrentPos := FHeaderSize;
end;

{ TFileEntryList }

function TFileEntryList.GetFileEntry(Index: Integer): TFileEntryRec;
begin
  Result := PFileEntryRec(Items[Index])^;
end;

procedure TFileEntryList.SetFileEntry(Index: Integer; const AValue: TFileEntryRec);
begin
  PFileEntryRec(Items[Index])^ := AValue;
end;

function TFileEntryList.AddEntry(AFileEntry: TFileEntryRec; CheckPathIsAdded: Boolean = True): Integer;
var
  TmpEntry: PFileEntryRec;
begin
  New(TmpEntry);
  //WriteLn('Adding: ', AFileEntry.Path+AFileEntry.Name,' Size = ', AFileEntry.DecompressedSize,' Offset = ', AFileEntry.DecompressedOffset);
  if CheckPathIsAdded and (FPaths.IndexOf(AFileEntry.Path) < 0) then
  begin
    // all paths are included in the list of files in section 0 with a size and offset of 0
    FPaths.Add(AFileEntry.Path);
    TmpEntry^.Path := AFileEntry.Path;
    TmpEntry^.Name := '';
    TmpEntry^.DecompressedOffset := 0;
    TmpEntry^.DecompressedSize := 0;
    TmpEntry^.Compressed := False;
    (Self as TList).Add(TmpEntry);
    New(TmpEntry);
  end;
  TmpEntry^ := AFileEntry;
  Result := (Self as TList).Add(TmpEntry);
end;

procedure TFileEntryList.Delete(Index: Integer);
begin
  Dispose(PFileEntryRec(Items[Index]));
  Inherited Delete(Index);
end;

function FileEntrySortFunc(Item1, Item2: PFileEntryRec): Integer;
var
  Str1, Str2: String;
begin
  Str1 := Item1^.Path + Item1^.Name;
  Str2 := Item2^.Path + Item2^.Name;
  Result := ChmCompareText(Str1, Str2);
end;

procedure TFileEntryList.Sort;
begin
  inherited Sort(TListSortCompare(@FileEntrySortFunc));
end;

constructor TFileEntryList.Create;
begin
  inherited Create;
  FPaths := TStringList.Create;
end;

destructor TFileEntryList.Destroy;
var
  I: Integer;
begin
  for I := Count-1 downto 0 do
    Delete(I);
  FPaths.Free;
  inherited Destroy;
end;

{ TPMGIDirectoryChunk }
procedure TPMGIDirectoryChunk.WriteChunkToStream(Stream: TStream; var AIndex: Integer
  ; Final: Boolean = False);
var
  NewBuffer: array[0..512] of byte;
  EntryLength,
  WriteSize: Integer;
  OldPos, NewPos, NewStart: Int64;

  procedure FinishBlock();
  var
    Header: TPMGIIndexChunk;
  begin
    Inc(AIndex);
    Header.PMGIsig := 'PMGI';
    Header.UnusedSpace := FParentChunk.FreeSpace;
    FParentChunk.WriteHeader(@Header);
    FParentChunk.WriteChunkToStream(Stream, AIndex, Final);
    FParentChunk.Clear();
  end;

begin
  if FItemCount < 1 then
  begin
    {$ifdef chm_debug}
    WriteLn('WHAT ARE YOU DOING!!');
    {$endif}
    Dec(AIndex);
    Exit;
  end;
  OldPos := Stream.Position;
  WriteChunkToStream(Stream);
  NewPos := Stream.Position;
  Inc(FChunkLevelCount);

  if Final and (ChunkLevelCount < 2) then
  begin
    FParentChunk.Free();
    FParentChunk := nil;
    Exit;
  end;
  if FParentChunk = nil then FParentChunk := TPMGIDirectoryChunk.Create(FHeaderSize);

  NewStart := OldPos+FHeaderSize;
  Stream.Position := NewStart;
  EntryLength := GetCompressedInteger(Stream);
  WriteSize := (Stream.Position - NewStart) + EntryLength;
  Move(Buffer[FHeaderSize], NewBuffer[0], WriteSize);
  Inc(WriteSize, WriteCompressedInteger(@NewBuffer[WriteSize], AIndex));

  Stream.Position := NewPos;

  if not FParentChunk.CanHold(WriteSize) then
  begin
    FinishBlock();
  end;

  FParentChunk.WriteEntry(WriteSize, @NewBuffer[0]);
  if Final then FinishBlock();
  //WriteLn(ChunkLevelCount);
end;

function GetNext(const s: string; var i: Integer; len: Integer): string;
var
  ind: integer;
begin
  if i > len then Exit('');
  ind := i;
  if s[ind] = '"' then
  begin
    Inc(ind);
    while (ind <= len) and (s[ind] <> '"') do Inc(ind);
    Result := Copy(s, i+1, ind-i-1);
    Inc(ind); // skip "
  end
  else
  begin
    while (ind <= len) and (s[ind] <> ',') do Inc(ind);
    Result := Copy(s, i, ind-i);
  end;
  i := ind + 1; // skip ,
end;

function GetNextInt(const txt: string; var ind: Integer; len: Integer;
                    var flags: TValidWindowFields; x: TValidWindowFieldsEnum): Integer;
var
  s: string;
  i: Integer;
begin
  i := ind;
  s := GetNext(txt, ind, len);
  // set a flag if the field was empty (,,)
  if (ind = (i+1)) and (x <> valid_Unknown1) then
    Include(flags, x);
  Result := StrToIntDef(s, 0);  // I think this does C style hex, if not fixup here.
end;

procedure TCHMWindow.load_from_ini(txt: string);
var ind,len,
    j,k     : integer;
    arr     : array[0..3] of integer;
    s2      : string;
    bArr    : Boolean;
begin
  j := Pos('=', txt);
  if j > 0 then
    txt[j] := ',';

  ind := 1;
  len := Length(txt);
  window_type       := GetNext(txt, ind, len);
  Title_bar_text    := GetNext(txt, ind, len);
  Toc_file          := GetNext(txt, ind, len);
  index_file        := GetNext(txt, ind, len);
  Default_File      := GetNext(txt, ind, len);
  Home_button_file  := GetNext(txt, ind, len);
  Jumpbutton_1_File := GetNext(txt, ind, len);
  Jumpbutton_1_Text := GetNext(txt, ind, len);
  Jumpbutton_2_File := GetNext(txt, ind, len);
  Jumpbutton_2_Text := GetNext(txt, ind, len);
  nav_style         := GetNextInt(txt, ind, len, flags, valid_navigation_pane_style);
  navpanewidth      := GetNextInt(txt, ind, len, flags, valid_navigation_pane_width);
  buttons           := GetNextInt(txt, ind, len, flags, valid_buttons);
  
  (* initialize arr[] *)
  arr[0] := 0;
  arr[1] := 0;
  arr[2] := 0;
  arr[3] := 0;
  k := 0;
  bArr := False;
  (* "[" int,int,int,int "]", |,  *)
  s2 := GetNext(txt, ind, len);
  if Length(s2) > 0 then
  begin
    (* check if first chart is "[" *)
    if (s2[1] = '[') then
    begin
      Delete(s2, 1, 1);
      bArr := True;
    end;
    (* looking for a max 4 int followed by a closing "]" *)
    repeat
      if k > 0 then s2 := GetNext(txt, ind, len);
      
      j := Pos(']', s2);
      if j > 0 then Delete(s2, j, 1);
      if Length(Trim(s2)) > 0 then
        Include(flags, valid_tab_position);
      arr[k] := StrToIntDef(s2, 0);
      Inc(k);
    until (bArr <> True) or (j <> 0) or (ind > len);
  end;
   
  left   := arr[0];
  top    := arr[1];
  right  := arr[2];
  bottom := arr[3];
  styleflags               := GetNextInt(txt, ind, len, flags, valid_buttons);
  xtdstyleflags            := GetNextInt(txt, ind, len, flags, valid_window_style_flags);
  window_show_state        := GetNextInt(txt, ind, len, flags, valid_window_extended_style_flags);
  navpane_initially_closed := GetNextInt(txt, ind, len, flags, valid_navigation_pane_initially_closed_state);
  navpane_default          := GetNextInt(txt, ind, len, flags, valid_default_pane);
  navpane_location         := GetNextInt(txt, ind, len, flags, valid_tab_position);
  wm_notify_id             := GetNextInt(txt, ind, len, flags, valid_unknown1);
end;

procedure TCHMWindow.SaveToXml(cfg: TXMLConfig; key: string);
begin
  cfg.SetValue(key+'window_type', window_type);
  cfg.SetValue(key+'title_bar_text', title_bar_text);
  cfg.SetValue(key+'toc_file', Toc_file);
  cfg.SetValue(key+'index_file', index_file);
  cfg.SetValue(key+'default_file', Default_File);
  cfg.SetValue(key+'home_button_file', Home_button_file);
  cfg.SetValue(key+'jumpbutton_1_file', Jumpbutton_1_File);
  cfg.SetValue(key+'jumpbutton_1_text', Jumpbutton_1_Text);
  cfg.SetValue(key+'jumpbutton_2_file', Jumpbutton_2_File);
  cfg.SetValue(key+'jumpbutton_2_text', Jumpbutton_2_Text);
  cfg.SetValue(key+'nav_style', nav_style);
  cfg.SetValue(key+'navpanewidth', navpanewidth);
  cfg.SetValue(key+'buttons', buttons);
  cfg.SetValue(key+'left', left);
  cfg.SetValue(key+'top', top);
  cfg.SetValue(key+'right', right);
  cfg.SetValue(key+'bottom', bottom);
  cfg.SetValue(key+'styleflags', styleflags);
  cfg.SetValue(key+'xtdstyleflags', xtdstyleflags);
  cfg.SetValue(key+'window_show_state', window_show_state);
  cfg.SetValue(key+'navpane_initially_closed', navpane_initially_closed);
  cfg.SetValue(key+'navpane_default', navpane_default);
  cfg.SetValue(key+'navpane_location', navpane_location);
  cfg.SetValue(key+'wm_notify_id', wm_notify_id);
end;

procedure TCHMWindow.LoadFromXml(cfg: TXMLConfig; key: string);
begin
  window_type           := cfg.GetValue(key+'window_type','');
  Title_bar_text        := cfg.GetValue(key+'title_bar_text','');
  Toc_file              := cfg.GetValue(key+'toc_file','');
  Index_file            := cfg.GetValue(key+'index_file','');
  Default_File          := cfg.GetValue(key+'default_file','');
  Home_button_file      := cfg.GetValue(key+'home_button_file','');
  Jumpbutton_1_File     := cfg.GetValue(key+'jumpbutton_1_file','');
  Jumpbutton_1_Text     := cfg.GetValue(key+'jumpbutton_1_text','');
  Jumpbutton_2_File     := cfg.GetValue(key+'jumpbutton_2_file','');
  Jumpbutton_2_Text     := cfg.GetValue(key+'jumpbutton_2_text','');
  nav_style             := cfg.GetValue(key+'nav_style',0);
  navpanewidth          := cfg.GetValue(key+'navpanewidth',0);
  buttons               := cfg.GetValue(key+'buttons',0);
  left                  := cfg.GetValue(key+'left',0);
  top                   := cfg.GetValue(key+'top',0);
  right                 := cfg.GetValue(key+'right',0);
  bottom                := cfg.GetValue(key+'bottom',0);
  styleflags            := cfg.GetValue(key+'styleflags',0);
  xtdstyleflags         := cfg.GetValue(key+'xtdstyleflags',0);
  window_show_state     := cfg.GetValue(key+'window_show_state',0);
  navpane_initially_closed := cfg.GetValue(key+'navpane_initially_closed',0);
  navpane_default       := cfg.GetValue(key+'navpane_default',0);
  navpane_location      := cfg.GetValue(key+'navpane_location',0);
  wm_notify_id          := cfg.GetValue(key+'wm_notify_id',0);
end;

constructor TCHMWindow.Create(s: string = '');
begin
 flags := DefValidFlags;
 if s <> '' then
   load_from_ini(s);
end;


procedure TCHMWindow.Assign(obj: TCHMWindow);

begin
  window_type      := obj.window_type;
  Title_bar_text   := obj.Title_bar_text;
  Toc_file         := obj.Toc_file;
  Index_file       := obj.Index_file;
  Default_File     := obj.Default_File;
  Home_button_file := obj.Home_button_file;
  Jumpbutton_1_File:= obj.Jumpbutton_1_File;
  Jumpbutton_1_Text:= obj.Jumpbutton_1_Text;
  Jumpbutton_2_File:= obj.Jumpbutton_2_File;
  Jumpbutton_2_Text:= obj.Jumpbutton_2_Text;
  nav_style        := obj.nav_style;
  navpanewidth     := obj.navpanewidth;
  buttons          := obj.buttons;
  left             := obj.left;
  top              := obj.top;
  right            := obj.right;
  bottom           := obj.bottom;
  styleflags       := obj.styleflags;
  xtdstyleflags    := obj.xtdstyleflags;
  window_show_state:= obj.window_show_state;
  navpane_initially_closed := obj.navpane_initially_closed;
  navpane_default  := obj.navpane_default;
  navpane_location := obj.navpane_location;
  wm_notify_id     := obj.wm_notify_id;
end;


end.
