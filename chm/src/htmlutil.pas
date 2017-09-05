{ Copyright (C) <2005> <Andrew Haines> htmlutil.pas

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
{ modified from jsFastHtmlParser  for use with freepascal 
  
 Original Author:
  James Azarja

 Contributor:
  Lars aka L505
  http://z505.com 

 Note: this isn't perfect, it needs to be improved.. see comments  }

unit HTMLUtil;
{$ifdef fpc} {$MODE Delphi} {$H+}{$endif}

interface

uses
  SysUtils;

{ most commonly used }
function GetVal(const tag, attribname_ci: string): string;
function GetTagName(const Tag: string): string;

{ less commonly used, but useful }
function GetUpTagName(const tag: string): string;
function GetNameValPair(const tag, attribname_ci: string): string;
function GetValFromNameVal(const namevalpair: string): string;

{ old buggy code}
function GetVal_JAMES(tag, attribname_ci: string): string;
function GetNameValPair_JAMES(tag, attribname_ci: string): string;

{ rarely needed NAME= case sensitivity }
function GetNameValPair_cs(tag, attribname: string): string;


implementation

function CopyBuffer(StartIndex: PChar; Length: Integer): string;
begin
  // works faster, than SetString() and Move()
  SetLength(Result, Length);
  StrLCopy(PChar(Result), StartIndex, Length);
end;

{ Return tag name, case preserved }
function GetTagName(const Tag: string): string;
var
  P: PChar;
  S: PChar;
begin
  P := PChar(Tag);
  while P^ in ['<', ' ', #9] do
    Inc(P);
  S := P;
  while not (P^ in [' ', '>', #0]) do
    Inc(P);
  if P > S then
    Result := CopyBuffer(S, P - S)
  else
    Result := '';
end;

{ Return tag name in uppercase }
function GetUpTagName(const tag: string): string;
var
  P: PChar;
  S: PChar;
begin
  P := PChar(uppercase(Tag));
  while P^ in ['<', ' ', #9] do
    Inc(P);
  S := P;
  while not (P^ in [' ', '>', #0]) do
    Inc(P);
  if P > S then
    Result := CopyBuffer(S, P - S)
  else
    Result := '';
end;


{ Return name=value pair ignoring case of NAME, preserving case of VALUE
  Lars' fixed version }
function GetNameValPair(const tag, attribname_ci: string): string;
var
  P: PChar;
  S: PChar;
  UpperTag, UpperAttrib: string;
  Start: Integer;
  L: Integer;
  C: char;
begin
  // must be space before case insensitive NAME, i.e. <a HREF="" STYLE=""
  UpperAttrib := ' ' + Uppercase(attribname_ci);
  UpperTag := Uppercase(Tag);
  P := PChar(UpperTag);
  S := StrPos(P, PChar(UpperAttrib));

  if S <> nil then
  begin
    Inc(S); // skip space
    P := S;

    // Skip
    while not (P^ in ['=', ' ', '>', #0]) do
      Inc(P);

    if (P^ = '=') then
      Inc(P);

    while not (P^ in [' ', '>', #0]) do
    begin
      if (P^ in ['"', '''']) then
      begin
        C := P^;
        Inc(P); { Skip quote }
      end
      else
        C := ' ';

      { thanks to Dmitry [mail@vader.ru] }
      while not (P^ in [C, '>', #0]) do
        Inc(P);

      if (P^ <> '>') then
        Inc(P); { Skip current character, except '>' }

      break;
    end;

    L := P - S;
    Start := S - PChar(UpperTag);
    P := PChar(Tag);
    S := P;
    Inc(S, Start);

    Result := CopyBuffer(S, L);
  end;
end;


{ Get value of attribute, e.g WIDTH=36 -return-> 36, preserves case sensitive }
function GetValFromNameVal(const namevalpair: string): string;
var
  P: PChar;
  S: PChar;
  C: char;
begin
  P := PChar(namevalpair);
  S := StrPos(P, '=');

  if S <> nil then
  begin
    Inc(S); // skip equal
    P := S;  // set P to a character after =

    if (P^ in ['"', '''']) then
    begin
      C := P^;
      Inc(P); { Skip current character }
    end
    else
      C := ' ';

    S := P;
    while not (P^ in [C, #0]) do
      Inc(P);

    if (P <> S) then { Thanks to Dave Keighan (keighand@yahoo.com) }
      Result := CopyBuffer(S, P - S)
    else
      Result := '';
  end;
end;


{ return value of an attribute (attribname_ci), case ignored for NAME portion, but return value case is preserved }

function GetVal(const tag, attribname_ci: string): string;
var
  namevalpair: string;
begin
  // returns full name=value pair
  namevalpair := GetNameValPair(tag, attribname_ci);
  // extracts value portion only
  Result := GetValFromNameVal(namevalpair);
end;


{ ----------------------------------------------------------------------------
  BELOW FUNCTIONS ARE OBSOLETE OR RARELY NEEDED SINCE THEY EITHER CONTAIN BUGS
  OR THEY ARE TOO CASE SENSITIVE (FOR THE TAG NAME PORTION OF THE ATTRIBUTE  }

{ James old buggy code for testing purposes.
  Bug: when finding 'ID', function finds "width", even though width <> "id" }
function GetNameValPair_JAMES(tag, attribname_ci: string): string;
var
  P: PChar;
  S: PChar;
  UT, UA: string;
  Start: Integer;
  L: Integer;
  C: char;
begin
  UA := Uppercase(attribname_ci);
  UT := Uppercase(Tag);
  P := PChar(UT);
  S := StrPos(P, PChar(UA));
  if S <> nil then
  begin

    P := S;

    // Skip attribute name
    while not (P^ in ['=', ' ', '>', #0]) do
      Inc(P);

    if (P^ = '=') then
      Inc(P);

    while not (P^ in [' ', '>', #0]) do
    begin

      if (P^ in ['"', '''']) then
      begin
        C := P^;
        Inc(P); { Skip current character }
      end
      else
        C := ' ';

      { thanks to Dmitry [mail@vader.ru] }
      while not (P^ in [C, '>', #0]) do
        Inc(P);

      if (P^ <> '>') then
        Inc(P); { Skip current character, except '>' }
      break;
    end;

    L := P - S;
    Start := S - PChar(UT);
    P := PChar(Tag);
    S := P;
    Inc(S, Start);
    Result := CopyBuffer(S, L);
  end;
end;


{ James old buggy code for testing purposes }
function GetVal_JAMES(tag, attribname_ci: string): string;
var
  namevalpair: string;
begin
  namevalpair := GetNameValPair_JAMES(tag, attribname_ci);
  Result := GetValFromNameVal(namevalpair);
end;

{ return name=value portion, case sensitive, case preserved }
function GetNameValPair_cs(Tag, attribname: string): string;
var
  P: PChar;
  S: PChar;
  C: char;
begin
  P := PChar(Tag);
  S := StrPos(P, PChar(attribname));
  if S <> nil then
  begin
    P := S;

    // Skip attribute name
    while not (P^ in ['=', ' ', '>', #0]) do
      Inc(P);

    if (P^ = '=') then
      Inc(P);

    while not (P^ in [' ', '>', #0]) do
    begin

      if (P^ in ['"', '''']) then
      begin
        C := P^;
        Inc(P); { Skip current character }
      end
      else
        C := ' ';

      { thanks to Dmitry [mail@vader.ru] }
      while not (P^ in [C, '>', #0]) do
        Inc(P);

      if (P^ <> '>') then
        Inc(P); { Skip current character, except '>' }
      break;
    end;

    if P > S then
      Result := CopyBuffer(S, P - S)
    else
      Result := '';
  end;
end;


end.
(* alternative, not needed

{ return value (case preserved) from a name=value pair, ignores case in given NAME= portion }
function GetValFromNameVal(namevalpair: string): string;

  type
    TAttribPos = record
      startpos: longword; // start pos of value
      len: longword;      // length of value
    end;

  { returns case insensitive start position and length of just the value
    substring in name=value pair}
  function ReturnPos(attribute: string): TAttribPos;
  var
    P    : Pchar;
    S    : Pchar;
    C    : Char;
  begin
    result.startpos:= 0;
    result.len:= 0;
    P:= Pchar(uppercase(Attribute));
    // get substring including and everything after equal
    S:= StrPos(P, '=');
    result.startpos:= pos('=', P);

    if S <> nil then
    begin
      inc(S);
      // set to character after =
      inc(result.startpos);
      P:= S;

      if (P^ in ['"','''']) then
      begin
        C:= P^;
        // skip quote
        inc(P);
        inc(result.startpos);
      end else
        C:= ' ';

      S:= P;
      // go to end quote or end of value
      while not (P^ in [C, #0]) do
        inc(P);

      if (P <> S) then
      begin
        result.len:= p - s;
      end;
    end;

  end;

var
  found: TAttribPos;
begin
  found:= ReturnPos(namevalpair);
  // extract using coordinates
  result:= MidStr(namevalpair, found.startpos, found.len);
end;

*)
