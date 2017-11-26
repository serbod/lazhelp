{ Copyright (C) <2008> <Andrew Haines> htmlindexer.pas

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
unit HTMLIndexer;

{$MODE OBJFPC}{$H+}
interface

uses Classes, SysUtils, FastHTMLParser{$ifdef userb}, fos_redblacktree_gen{$else},
  avl_tree{$endif};

type

  { TIndexDocument }
  TIndexDocument = class(TObject)
  private
    FDocumentIndex: Integer;
    FLastEntry: Integer;
    FWordIndex: array of Integer;
    function GetIndExentries(): Integer;
  public
    constructor Create(ADocumentIndex: Integer);
    function GetWordIndex(i: Integer): Integer; inline;
    procedure AddWordIndex(AIndex: Integer);
    property DocumentIndex: Integer read FDocumentIndex;
    property IndexEntry[i: Integer]: Integer read GetWordIndex;
    property NumberOfIndexEntries: Integer read GetIndExentries;
  end;

  { TIndexDocumentList }

  TIndexDocumentList = class(TList)
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    function GetItem(Index: Integer): TIndexDocument;
  end;

  { TIndexedWord }
  TIndexedWord = class(TObject)
  private
    FIsTitle: Boolean;
    FTheWord: string;
    FCachedTopic: TIndexDocument;
    FDocumentList: TIndexDocumentList;
    function GetDocument(TopicIndexNum: Integer): TIndexDocument;
    function GetDocumentCount(): Integer;
  public
    constructor Create(AWord: string; AIsTitle: Boolean);
    destructor Destroy; override;
    function GetLogicalDocument(AIndex: Integer): TIndexDocument;
    property TheWord: string read FTheWord write FTheWord; // Always lowercase
    property DocumentTopic[TopicIndexNum: Integer]: TIndexDocument read GetDocument;
    property DocumentCount: Integer read GetDocumentCount;
    { True if word was in document Title }
    property IsTitle: Boolean read FIsTitle write FIsTitle;
  end;

  { TIndexedWordList }

  {$ifdef userb}
  TRBIndexTree = specialize TGFOS_RBTree<string, TIndexedWord>;
  {$endif}

  TForEachMethod = procedure(AWord: TIndexedWord) of object;
  TForEachProcedure = procedure(AWord: TIndexedWord; state: pointer);

  TIndexedWordList = class(TObject)
  private
    FIndexTitlesOnly: Boolean;
    FIndexedFileCount: DWord;
    //vars while processing page
    FInTitle, FInBody: Boolean;
    FWordCount: Integer; // only words in body
    FDocTitle: string;
    FTopicIndex: Integer;
    //end vars
    FTotalDifferentWordLength: DWord;
    FTotalDifferentWords: DWord;
    FTotalWordCount: DWord;
    FTotalWordLength: DWord;
    FLongestWord: DWord;
    FParser: THTMLParser;
    {$ifdef userb}
    FAVLTree: TRBIndexTree;
    {$else}
    FAVLTree: TAVLTree;
    FTmpWord: TIndexedWord;
    {$endif}
    { only lowerased words allowed }
    function AddGetWord(const AWord: string; IsTitle: Boolean): TIndexedWord;
    // callbacks
    procedure CBFoundTag(NoCaseTag, ActualTag: string);
    procedure CBFountText(Text: string);
    { only lowerased text allowed }
    procedure EatWords(const Words: string; IsTitle: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    function IndexFile(AStream: TStream; ATOPICIndex: Integer;
      AIndexOnlyTitles: Boolean): string; // returns the documents <Title>
    procedure Clear;
    procedure AddWord(const AWord: TIndexedWord);
    procedure ForEach(Proc: TForEachMethod);
    procedure ForEach(Proc: TForEachProcedure; pState: Pointer);
    property IndexedFileCount: DWord read FIndexedFileCount;
    property LongestWord: DWord read FLongestWord;
    property TotalWordCount: DWord read FTotalWordCount;
    property TotalDIfferentWords: DWord read FTotalDifferentWords;
    property TotalWordLength: DWord read FTotalWordLength;
    property TotalDifferentWordLength: DWord read FTotalDifferentWordLength;
    //property Words[AWord: string; IsTitle: Boolean]: TIndexedWord read AddGetWord;
  end;

implementation

const
  GrowSpeed = 10;

const
  TitleXlat: array [Boolean] of Char = ('0', '1');

function MakeKey(const s: string; IsTitle: Boolean): string; inline;
begin
  Result := s + '___' + TitleXlat[IsTitle];
end;

function CompareProcObj(Node1, Node2: Pointer): Integer;
var
  n1, n2: TIndexedWord;
begin
  n1 := TIndexedWord(Node1);
  n2 := TIndexedWord(Node2);
  Result := CompareText(n1.TheWord, n2.TheWord);
  if Result = 0 then
  begin
    Result := Ord(n2.IsTitle) - Ord(n1.IsTitle);
  end;
  if Result < 0 then
    Result := -1
  else if Result > 0 then
    Result := 1;
end;

{ TIndexDocumentList }

procedure TIndexDocumentList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited Notify(Ptr, Action);
  if Action = lnDeleted then
    TIndexDocument(Ptr).Free();
end;

function TIndexDocumentList.GetItem(Index: Integer): TIndexDocument;
begin
  Result := TIndexDocument(Get(Index));
end;

{ TIndexedWordList }
function TIndexedWordList.AddGetWord(const AWord: string; IsTitle: Boolean): TIndexedWord;
var
{$ifdef userb}
  key: string;
{$else}
  n: TAVLTreeNode;
{$endif}
  iLen: Integer;
begin
  Result := nil;
 {$ifdef userb}
  key := MakeKey(AWord, IsTitle);
  if not FAVLTree.Find(key, Result) then
    Result := nil;
  ;
  {$else}
  if not Assigned(FTmpWord) then
    FTmpWord := TIndexedWord.Create(AWord, IsTitle)
  else
  begin
    FTmpWord.TheWord := AWord;
    FTmpWord.IsTitle := IsTitle;
  end;

  n := FAVLTree.FindKey(FTmpWord, @CompareProcObj);
  if Assigned(n) then
    Result := TIndexedWord(n.Data);
  {$endif}

  iLen := Length(AWord);
  if Result = nil then
  begin
    Inc(FTotalDifferentWordLength, iLen);
    Inc(FTotalDifferentWords);
    {$ifdef  userb}
    Result := TIndexedWord.Create(AWord, IsTitle);
    FAVLTree.Add(key, Result);
    {$else}
    Result := FTmpWord; // TIndexedWord.Create(AWord,IsTitle);
    FTmpWord := nil;
    AddWord(Result);
    {$endif}

    //  if IsTitle then
    //WriteLn('Creating word: ', AWord);
    if iLen > FLongestWord then
      FLongestWord := iLen;
  end;
  Inc(FTotalWordLength, iLen);
  Inc(FTotalWordCount);
end;

procedure TIndexedWordList.CBFoundTag(NoCaseTag, ActualTag: string);
begin
  if FInBody then
  begin
    if NoCaseTag = '</BODY>' then
      FInBody := False;
  end
  else
  begin
    //WriteLn('"',NoCaseTag,'"');
    if NoCaseTag = '<TITLE>' then
      FInTitle := True
    else if NoCaseTag = '</TITLE>' then
      FInTitle := False
    else if NoCaseTag = '<BODY>' then
      FInBody := True
    else;
  end;
  if FInBody and FIndexTitlesOnly then
    FParser.Done := True;
end;

procedure TIndexedWordList.CBFountText(Text: string);
begin
  if Length(Text) < 1 then
    Exit;
  EatWords(LowerCase(Text), FInTitle and (not FInBody));
end;

procedure TIndexedWordList.EatWords(const Words: string; IsTitle: Boolean);
var
  WordPtr: PChar;
  WordStart: PChar;
  InWord: Boolean;
  IsNumberWord: Boolean;

  function IsEndOfWord(): Boolean;
  begin
    Result := not (WordPtr^ in ['a'..'z', '0'..'9', #01, #$DE, #$FE]);
    if Result and IsNumberWord then
      Result := Result and (WordPtr[0] <> '.');
    if Result and InWord then
      Result := Result and (WordPtr[0] <> '''');
  end;

var
  IndexedWord: TIndexedWord;
  WordName: string;
  FPos: Integer;
begin
  if IsTitle then
    FDocTitle := Words;
  WordStart := PChar(Words);
  WordPtr := WordStart;
  IsNumberWord := False;
  InWord := False;
  repeat
    if InWord and IsEndOfWord() then
    begin
      WordName := Copy(WordStart, 0, (WordPtr - WordStart));
      FPos := Pos('''', WordName);
      while FPos > 0 do
      begin
        Delete(WordName, FPos, 1);
        FPos := Pos('''', WordName);
      end;
      IndexedWord := AddGetWord(WordName, IsTitle);
      InWord := False;
      IsNumberWord := False;
      IndexedWord.DocumentTopic[FTopicIndex].AddWordIndex(FWordCount);
      //if not IsTitle then
      Inc(FWordCount);

    end
    else if (not InWord) and (not IsEndOfWord()) then
    begin
      InWord := True;
      WordStart := WordPtr;
      IsNumberWord := WordPtr^ in ['0'..'9'];
    end;
    Inc(WordPtr);
  until WordPtr^ = #0;

  if InWord then
  begin
    WordName := Copy(WordStart, 0, (WordPtr - WordStart));
    try
      IndexedWord := AddGetWord(WordName, IsTitle); // Self.Words[WordName, IsTitle];
    except
      on e: Exception do
        WriteLn(WordName);
    end;
    IndexedWord.DocumentTopic[FTopicIndex].AddWordIndex(FWordCount);
    InWord := False;
    //if IsNumberWord then WriteLn('Following is NUMBER WORD: "', (WordStart[0]),'"'); ;
    IsNumberWord := False;
    //WriteLn(FWordCount, ' "', WordName,'"');
    if not IsTitle then
      Inc(FWordCount);
  end;
end;

function DefaultIndexedWord: TIndexedWord;
begin
  Result := TIndexedWord.Create('', False);
end;

constructor TIndexedWordList.Create;
begin
  inherited;
  {$ifdef userb}
  FAVLTree := TRBIndexTree.Create(@default_rb_string_compare,
    @DefaultIndexedWord, @default_rb_string_undef);
  {$else}
  FAVLTree := TAVLTree.Create(@CompareProcObj);
  FTmpWord := nil;
  {$endif}
end;

procedure FreeObject(const Obj: TIndexedWord);
begin
  Obj.Free();
end;


destructor TIndexedWordList.Destroy;
begin
  Clear();
  {$ifndef userb}
  if Assigned(FTmpWord) then
    FreeAndNil(FTmpWord);
  {$endif}
  FreeAndNil(FAVLTree);
  inherited Destroy;
end;

function TIndexedWordList.IndexFile(AStream: TStream; ATOPICIndex: Integer;
  AIndexOnlyTitles: Boolean): string;
var
  DataStr: string;
begin
  FInBody := False;
  FInTitle := False;
  FIndexTitlesOnly := AIndexOnlyTitles;
  FWordCount := 0;
  FTopicIndex := ATOPICIndex;
  FIndexedFileCount := FIndexedFileCount + 1;

  SetLength(DataStr, AStream.Size);
  AStream.Position := 0;
  AStream.Read(PChar(DataStr)^, AStream.Size);

  FParser := THTMLParser.Create(PChar(DataStr));
  try
    FParser.OnFoundTag := @CBFoundTag;
    FParser.OnFoundText := @CBFountText;
    FParser.Exec();
  finally
    FreeAndNil(FParser);
  end;

  Result := FDocTitle;
  FDocTitle := '';
  FInBody := False;
  FInTitle := False;
  FWordCount := 0;
  FTopicIndex := -1;

  AStream.Position := 0;
end;

procedure TIndexedWordList.Clear();
begin
  {$ifdef userb}
  FAVLTree.ClearN(@FreeObject);
  {$else}
  FAVLTree.FreeAndClear();
  {$endif}
end;

procedure TIndexedWordList.AddWord(const AWord: TIndexedWord);
begin
 {$ifdef userb}
  FAVLTree.Add(MakeKey(AWord.TheWord, AWord.IsTitle), AWord);
 {$else}
  FAVLTree.Add(AWord);
 {$endif}
end;

procedure TIndexedWordList.ForEach(Proc: TForEachMethod);
{$ifdef userb}
var
  key: string;
  val: TIndexedWord;
{$else}
var
  AVLNode: TAVLTreeNode;
{$endif}
begin
 {$ifdef userb}
  if FAVLTree.FirstNode(key, val) then
  begin  // Scan it forward
    repeat
      Proc(val);
    until not FAVLTree.FindNext(key, val);
  end;
 {$else}
  AVLNode := FAVLTree.FindLowest();
  while (AVLNode <> nil) do
  begin
    Proc(TIndexedWord(AVLNode.Data));
    AVLNode := FAVLTree.FindSuccessor(AVLNode);
  end;
 {$endif}
end;

procedure TIndexedWordList.ForEach(Proc: TForEachProcedure; pState: Pointer);

{$ifdef userb}
var
  key: string;
  val: TIndexedWord;
{$else}
var
  AVLNode: TAVLTreeNode;
{$endif}
begin
 {$ifdef userb}
  if FAVLTree.FirstNode(key, val) then
  begin  // Scan it forward
    repeat
      Proc(val, pState);
    until not FAVLTree.FindNext(key, val);
  end;
 {$else}
  AVLNode := FAVLTree.FindLowest();
  while (AVLNode <> nil) do
  begin
    Proc(TIndexedWord(AVLNode.Data), pState);
    AVLNode := FAVLTree.FindSuccessor(AVLNode);
  end;
  {$endif}
end;

{ TIndexedWord }
function TIndexedWord.GetDocument(TopicIndexNum: Integer): TIndexDocument;
var
  i: Integer;
begin
  Result := nil;
  if (FCachedTopic <> nil) and (FCachedTopic.FDocumentIndex = TopicIndexNum) then
    Exit(FCachedTopic);

  for i := 0 to FDocumentList.Count-1 do
  begin
    Result := FDocumentList.GetItem(i);
    if Result.FDocumentIndex = TopicIndexNum then
    begin
      FCachedTopic := Result;
      Exit;
    end;
  end;

  Result := TIndexDocument.Create(TopicIndexNum);
  FDocumentList.Add(Result);
  FCachedTopic := Result;
end;

function TIndexedWord.GetDocumentCount: Integer;
begin
  Result := FDocumentList.Count;
end;

constructor TIndexedWord.Create(AWord: string; AIsTitle: Boolean);
begin
  FDocumentList := TIndexDocumentList.Create();
  FTheWord := AWord;
  FIsTitle := AIsTitle;
end;

destructor TIndexedWord.Destroy;
begin
  // here the word removed itself from the linked list. But it can't
  // touch the AVL tree here.
  FreeAndNil(FDocumentList);
  inherited Destroy;
end;

function TIndexedWord.GetLogicalDocument(AIndex: Integer): TIndexDocument;
begin
  Result := FDocumentList.GetItem(AIndex);
end;

{ TIndexDocument }
procedure TIndexDocument.AddWordIndex(AIndex: Integer);
begin
  if FLastEntry >= Length(FWordIndex) then
    SetLength(FWordIndex, Length(FWordIndex) + GrowSpeed);
  FWordIndex[FLastEntry] := AIndex;
  Inc(FLastEntry);
end;

constructor TIndexDocument.Create(ADocumentIndex: Integer);
begin
  FDocumentIndex := ADocumentIndex;
  FLastEntry := 0;
end;

function TIndexDocument.GetWordIndex(i: Integer): Integer;
begin
  Result := FWordIndex[i];
end;

function TIndexDocument.GetIndExentries: Integer;
begin
  Result := FLastEntry - 1;
end;

end.
