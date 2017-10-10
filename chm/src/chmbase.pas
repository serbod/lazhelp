{ Copyright (C) <2005> <Andrew Haines> chmbase.pas

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
{
CHM format reference
http://www.russotto.net/chm/chmformat.html
Copyright 2001-2003 Matthew T. Russotto
}
unit chmbase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const 
  CHMPackageVersion = '3.1.1'; // to be put in readme
  
type
  {$PACKRECORDS C}
  { The .chm file begins with a short ($38 byte) initial header.
    This is followed by the header section table and the offset to the content. }
  TITSFHeader = record
    ITSFsig: array [0..3] of char; // 'ITSF'
    Version: LongWord;      // 3 (Version number)
    HeaderLength: LongWord; // Total header length, including header section table and following data.
    Unknown_1: LongWord;    // 1 (unknown)
    TimeStamp: LongWord;    { a timestamp.
                              Considered as a big-endian DWORD, it appears to contain
                              seconds (MSB) and fractional seconds (second byte).
                              The third and fourth bytes may contain even more fractional
                              bits.  The 4 least significant bits in the last byte are
                              constant. }
    LanguageID: LongWord;   // Windows Language ID.
  end;

  { header section table, which is 2 entries, where each entry is $10 bytes long }
  TITSFHeaderEntry = record
    PosFromZero: QWord;            // Offset of section from beginning of file
    Length: QWord;                 // Length of section
  end;
  
  // Version 3 has this qword. 2 does not
  TITSFHeaderSuffix = record
    Offset: QWord; // offset within file of content section 0
  end;
  
  { This section contains the total size of the file, and not much else }
  TITSPHeaderPrefix = record
    Unknown1: LongWord; // = $01FE
    Unknown2: LongWord; // = 0
    FileSize: QWord;    // File Size
    Unknown3: LongWord; // =0
    Unknown4: LongWord; // =0
  end;
  
  { The directory starts with a header }
  TITSPHeader = record
    ITSPsig: array [0..3] of char; // = 'ITSP'
    Version: LongWord;             // =1 Version number
    DirHeaderLength: Longword;     // Length of the directory header
    Unknown1: LongWord;            // =$0a
    ChunkSize: LongWord;           // $1000 Directory chunk size
    Density: LongWord;             // "Density" of quickref section, usually = 2
    IndexTreeDepth: LongWord;      // Depth of the index tree, 1 if there is no index 2 if there is one level of PMGI chunks
    IndexOfRootChunk: LongInt;     // Chunk number of root index chunk, -1 if there is none
    FirstPMGLChunkIndex,           // Chunk number of first PMGL (listing) chunk
    LastPMGLChunkIndex: LongWord;  // Chunk number of last PMGL (listing) chunk
    Unknown2: LongInt;             // = -1
    DirectoryChunkCount: LongWord; // Number of directory chunks (total)
    LanguageID: LongWord;          // Windows language ID
    GUID: TGuid;                   // {5D02926A-212E-11D0-9DF9-00A0C922E6EC}
    LengthAgain: LongWord;         // $54 (This is the length again)
    Unknown3: LongInt;             // = -1
    Unknown4: LongInt;             // = -1
    Unknown5: LongInt;             // = -1
  end;
  
  TDirChunkType = (ctPMGL, ctPMGI, ctAOLL, ctAOLI, ctUnknown);

  { The header is directly followed by the directory chunks.
    There are two types of directory chunks -- index chunks, and listing chunks.
    The index chunk will be omitted if there is only one listing chunk. }
  TPMGListChunk = record
    PMGLsig: array [0..3] of char; // 'PMGL'
    UnusedSpace: Longword;         // Length of free space and/or (!!) quickref area at end of directory chunk
    Unknown1: Longword;            // always 0
    PreviousChunkIndex: LongInt;   { chunk number of the prev listing chunk when reading dir in sequence
                                    (-1 if this is the first listing chunk) }
    NextChunkIndex: LongInt;       // chunk number of the next listing chunk (-1 if this is the last chunk)
  end;

  { directory listing entry }
  TPMGListChunkEntry = record
    //NameLength: LongInt; we don't need this permanantly so I've moved it to a temp var
    Name: String;
    ContentSection: LongWord;  // QWord content section
    ContentOffset: QWord;      // offset
    DecompressedLength: QWord; // length
  end;
  PPMGListChunkEntry = ^TPMGListChunkEntry;

  { index chunk }
  TPMGIIndexChunk = record
    PMGIsig: array [0..3] of char; // 'PMGI'
    UnusedSpace: LongWord;         // Length of quickref / free area at end of directory chunk
  end;
  
  { directory index entry }
  TPMGIIndexChunkEntry = record
    //NameLength: LongInt; // length of name
    Name: String;        //  (UTF-8 encoded)
    ListingChunk: DWord; // directory listing chunk which starts with name
  end;

  
const
  ITSFHeaderGUID : TGuid = '{7C01FD10-7BAA-11D0-9E0C-00A0C922E6EC}';
  ITSFFileSig: array [0..3] of char = 'ITSF';
  
  ITSPHeaderGUID : TGuid = '{5D02926A-212E-11D0-9DF9-00A0C922E6EC}';
  ITSPHeaderSig: array [0..3] of char = 'ITSP';

  // this function will advance the stream to the end of the compressed integer
  // and return the value
  function GetCompressedInteger(const Stream: TStream): DWord;
  // returns the number of bytes written to the stream
  function WriteCompressedInteger(const Stream: TStream; ANumber: DWord): DWord;
  function WriteCompressedInteger(Buffer: Pointer; ANumber: DWord): DWord;
  
  // stupid needed function
  function ChmCompareText(const S1, S2: String): Integer; inline;


implementation

function GetCompressedInteger(const Stream: TStream): DWord;
var
  total: QWord = 0;
  temp: Byte;
  Sanity: Integer = 0;
begin
  try
    temp := Stream.ReadByte();
    while temp >= $80 do
    begin
      total := total shl 7;
      total := total + temp and $7f;
      temp := Stream.ReadByte();
      Inc(Sanity);
      if Sanity > 8 then
      begin
        Result := 0;
        Exit;
      end;
    end;
    Result := (total shl 7) + temp;
  except
    Result := 0;
  end;
end;

// returns how many bytes were written
function WriteCompressedInteger(const Stream: TStream; ANumber: DWord): DWord;
var
  Buffer: QWord; // Easily large enough
begin
  Result := WriteCompressedInteger(@Buffer, ANumber);
  Result := Stream.Write(Buffer, Result);
end;

// returns how many bytes were written
function WriteCompressedInteger(Buffer: Pointer; ANumber: DWord): DWord;
var
  bit: dword;
  mask: QWord;
  buf: PByte;
  Value: QWord = 0;
  TheEnd: DWord = 0;
begin
  bit := 28; //((sizeof(dWord)*8)div 7)*7; // = 28
  buf := @Value;
  {$push}
  {$R-}
  while True do
  begin
    mask := $7f shl bit;
    if (bit = 0) or ((ANumber and mask) <> 0) then break;
    Dec(bit, 7);
  end;
  while True do
  begin
    buf^ := Byte(((ANumber shr bit) and $7f));
    if (bit = 0) then break;
    buf^ := buf^ or $80;
    Inc(buf);
    Dec(bit, 7);
    Inc(TheEnd);
  end;

  {$pop}
  
  buf := @Value;
  Result := TheEnd+1;
  Move(Value, Buffer^, Result);
  {$ifdef chm_debug}
  if Result > 8 then WriteLn(' ', ANumber,' WRITE_COMPRESSED_INTEGER too big!: ', Result, ' ');
  {$endif}
end;

function ChmCompareText(const S1, S2: String): Integer; inline;
begin
  // for our purposes the CompareText function will not work.
  Result := CompareStr(LowerCase(S1), Lowercase(S2));
end;

end.

