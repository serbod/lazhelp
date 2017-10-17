{ Copyright (C) <2005> <Andrew Haines> chmcmd.pas

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
  See the file COPYING, included in this distribution,
  for details about the copyright.
}
program chmcmd;

{$mode objfpc}{$H+}

uses
 {$ifdef Unix}cthreads,
 {$endif} Classes,
  SysUtils,
  chmfilewriter,
  GetOpts;

const
  CHMCMDVersion = '3.1.1';

  procedure Usage();
  begin
    WriteLn(StdErr, 'Usage: chmcmd [options] <filename>');
    WriteLn(StdErr);
    WriteLn(StdErr, 'The following options are available :');
    WriteLn(StdErr, ' --html-scan       : scan html for missing files or alinks  ');
    WriteLn(StdErr, ' --no-html-scan    : don''t scan html for missing files or alinks ');
    WriteLn(StdErr, ' -h, --help        : print this text');
    WriteLn(StdErr, '--verbosity number : set verbosity level 0..5, 0 is least');
    WriteLn(StdErr, '--generate-xml     : (if .hhp file), also generate a xml project from .hhp');
    WriteLn(StdErr);
    WriteLn(StdErr, ' .hhp projects are default scanned for html, .xml not');
    Halt(1);
  end;

var
  TheOpts: array[1..7] of TOption;
  Cores: Integer = 0;

  procedure InitOptions();
  begin
    with TheOpts[1] do
    begin
      Name := 'html-scan';
      Has_arg := 0;
      Flag := nil;
      Value := #0;
    end;
    with TheOpts[2] do
    begin
      Name := 'no-html-scan';
      Has_arg := 0;
      Flag := nil;
      Value := #0;
    end;
    with TheOpts[3] do
    begin
      Name := 'verbosity';
      Has_arg := 1;
      Flag := nil;
      Value := #0;
    end;
    with TheOpts[4] do
    begin
      Name := 'generate-xml';
      Has_arg := 0;
      Flag := nil;
      Value := #0;
    end;
    with TheOpts[5] do
    begin
      Name := 'help';
      Has_arg := 0;
      Flag := nil;
    end;
    with TheOpts[6] do
    begin
      Name := 'cores';
      Has_arg := 1;
      Flag := nil;
    end;
    with TheOpts[7] do
    begin
      Name := '';
      Has_arg := 0;
      Flag := nil;
    end;
  end;

type
  THtmlScanEnum = (scanDefault, scanForce, scanForcedNo);

var
  GenerateXMLForHHP: Boolean = False;
  AllowedDetailLevel: Integer = 0;     // show if msg.detaillevel<=allowdetaillevel
  HtmlScan: THtmlScanEnum = scanDefault;

  procedure OnError(Project: TChmProject; ErrorKind: TChmProjectErrorKind;
    Msg: string; DetailLevel: Integer = 0);
  begin
    if (DetailLevel <= AllowedDetailLevel) or (ErrorKind < chmNote) then
    begin
      if ErrorKind <> chmNone then
        WriteLn(ChmErrorKindText[ErrorKind], ': ', Msg)
      else
        WriteLn(Msg);
    end;
  end;

  procedure ProcessFile(Name: string);
  var
    OutStream: TFileStream;
    Project: TChmProject;
    XmlName: string;
    IsHHP: Boolean;
  begin
    IsHHP := UpperCase(ExtractFileExt(Name)) = '.HHP';
    Project := TChmProject.Create();
    try
      Project.ReadMeMessage := 'Compiled by CHMCmd ' + CHMCMDVersion;
      if IsHHP then
      begin
        XmlName := ChangeFileExt(Name, '.hhp.xml');
        Project.OnError := @OnError;
        try
          Project.LoadFromHHP(Name, False);
          // we need a param for this second param later
        except
          on e: Exception do
          begin
            WriteLn('This HHP CHM project seems corrupt, please check it ', Name, ' (',
              e.Message, ')');
            halt(1);
          end;
        end;
        Project.ScanHtmlContents := (HtmlScan <> scanForcedNo);  // .hhp default SCAN
      end
      else
      begin
        try
          Project.ScanHtmlContents := (HtmlScan = scanForce);  // .hhp default SCAN
          Project.LoadFromFile(Name);
        except
          on e: Exception do
          begin
            WriteLn('This XML CHM project seems corrupt, please check it ', Name);
            halt(1);
          end;
        end;
      end;
      OutStream := TFileStream.Create(Project.OutputFileName, fmCreate);
      try
        Project.WriteChm(OutStream);
        if Project.ScanHtmlContents then
          Project.ShowUndefinedAnchors();
        if IsHHP and GenerateXMLForHHP then
        begin
          WriteLn('Generating XML ', XmlName, '.');
          Project.SaveToFile(XmlName);
        end;
      finally
        OutStream.Free();
      end;
    finally
      Project.Free();
    end;
  end;

var
  Name: string;
  OptionIndex: Integer;
  c: char;
  VerbTemp: Integer;
  VerbBool: Boolean;

begin
  InitOptions();
  WriteLn(StdErr, 'chmcmd, a CHM compiler. (c) 2010 Free Pascal core.');
  WriteLn(StdErr);
  repeat
    c := GetLongOpts('h', @TheOpts[1], OptionIndex);
    case c of
      #0:
      begin
        case OptionIndex of
          1: HtmlScan := scanForce;
          2: HtmlScan := scanForcedNo;
          3:
          begin
            VerbBool := TryStrToInt(OptArg, VerbTemp);
            if VerbBool then
              VerbBool := (VerbTemp >= 0) and (VerbTemp < 6);
            if VerbBool then
              AllowedDetailLevel := VerbTemp
            else
            begin
              WriteLn('Illegal value for switch --verbosity :', OptArg);
              Usage();
            end;
          end;
          4: GenerateXMLForHHP := True;
          5: Usage();
          6:
          begin
            if not TryStrToInt(OptArg, Cores) then
            begin
              WriteLn('Illegal value for switch --cores :', OptArg);
              Usage();
            end;
          end;
        end;
      end;
      '?':
      begin
        WriteLn('unknown option', OptOpt);
        Usage();
        halt;
      end;
    end; { case }
  until c = EndOfOptions;

  if (ParamCount - OptInd) = 0 then  // if equal, then 1 parameter
  begin
    Name := ParamStr(OptInd);
    if not FileExists(Name) then
    begin
      WriteLn('Can''t find project file ', Name);
      halt;
    end;
    ProcessFile(Name);
  end
  else
  begin
    Writeln('Invalid number of parameters :', ParamCount - OptInd + 1);
    Usage();
    halt;
  end;
end.
