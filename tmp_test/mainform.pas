unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, chmreader, chm_doc;

type

  { TFormMain }

  TFormMain = class(TForm)
    lvEntries: TListView;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure lvEntriesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
  private
    procedure FileEntryCallback(AName: String; AOffset, AUncompressedSize, ASection: Integer);
  public
    ChmStream: TStream;
    ChmReader: TChmReader;

    ChmEntryList: TChmEntryList;
    procedure Test();
  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
begin
  ChmEntryList := TChmEntryList.Create();
  Test();
end;

procedure TFormMain.lvEntriesSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
var
  ss: TStringStream;
  ChmEntry: TChmEntry;
begin
  if Selected and Assigned(Item) then
  begin
    ChmEntry := TChmEntry(Item.Data);
    ss := TStringStream.Create('');
    try
      ChmReader.ReadFileContent(ChmEntry.Name, ss);
      Memo1.Text := ss.DataString;
    finally
      ss.Free();
    end;
  end;
end;

procedure TFormMain.FileEntryCallback(AName: String; AOffset,
  AUncompressedSize, ASection: Integer);
var
  //s: string;
  li: TListItem;
  ChmEntry: TChmEntry;
begin
  //s := IntToStr(Memo1.Lines.Count+1) + ': ' + AName + ':'
  //   + ' offs=' + IntToStr(AOffset)
  //   + ' size=' + IntToStr(AUncompressedSize)
  //   + ' sect=' + IntToStr(ASection);
  //Memo1.Lines.Append(s);

  ChmEntry := TChmEntry.Create();
  ChmEntry.Name := AName;
  ChmEntry.ContentOffset := AOffset;
  ChmEntry.DecompressedLength := AUncompressedSize;
  ChmEntry.ContentSection := ASection;
  ChmEntryList.Add(ChmEntry);

  li := lvEntries.Items.Add();
  li.Data := ChmEntry;
  li.Caption := ChmEntry.Name;
  li.SubItems.Append(IntToStr(AUncompressedSize));
end;

procedure TFormMain.Test;
begin
  Memo1.Lines.Clear();
  ChmStream := TFileStream.Create('rtl.chm', fmOpenRead);
  ChmReader := TChmReader.Create(ChmStream, True);

  lvEntries.BeginUpdate();
  ChmReader.GetCompleteFileList(@FileEntryCallback);
  lvEntries.EndUpdate();
end;

end.

