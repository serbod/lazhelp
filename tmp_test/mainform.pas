unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, chmreader, chm_doc, chmsitemap;

type

  { TFormMain }

  TFormMain = class(TForm)
    ButtonTestIndex: TButton;
    ButtonTestTOC: TButton;
    ButtonTestListing: TButton;
    Label1: TLabel;
    ListBoxTestFiles: TListBox;
    lvEntries: TListView;
    Memo1: TMemo;
    procedure ButtonTestIndexClick(Sender: TObject);
    procedure ButtonTestListingClick(Sender: TObject);
    procedure ButtonTestTOCClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lvEntriesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
  private
    procedure FileEntryCallback(AName: String; AOffset, AUncompressedSize, ASection: Integer);
    procedure FillTestFiles();
  public
    Mode: Integer;
    ChmStream: TStream;
    ChmReader: TChmReader;

    ChmEntryList: TChmEntryList;
    ChmToc: TChmSitemap;
    ChmIndex: TChmSitemap;
    procedure TestDirListing(AFileName: string);
    procedure TestTOC(AFileName: string);
    procedure TestIndex(AFileName: string);
  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

procedure StreamToFile(AStream: TStream; AFileName: string);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(AFileName, fmCreate);
  try
    fs.CopyFrom(AStream, AStream.Size - AStream.Position);
  finally
    fs.Free();
  end;
end;

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
begin
  ChmEntryList := TChmEntryList.Create();
  FillTestFiles();
end;

procedure TFormMain.ButtonTestListingClick(Sender: TObject);
begin
  Mode := 1;
  TestDirListing(ListBoxTestFiles.GetSelectedText());
  ButtonTestListing.Enabled := False;
  ButtonTestTOC.Enabled := False;
  ButtonTestIndex.Enabled := False;
end;

procedure TFormMain.ButtonTestIndexClick(Sender: TObject);
begin
  Mode := 3;
  TestIndex(ListBoxTestFiles.GetSelectedText());
  ButtonTestListing.Enabled := False;
  ButtonTestTOC.Enabled := False;
  ButtonTestIndex.Enabled := False;
end;

procedure TFormMain.ButtonTestTOCClick(Sender: TObject);
begin
  Mode := 2;
  TestTOC(ListBoxTestFiles.GetSelectedText());
  ButtonTestListing.Enabled := False;
  ButtonTestTOC.Enabled := False;
  ButtonTestIndex.Enabled := False;
end;

procedure TFormMain.lvEntriesSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
var
  ss: TStringStream;
  ChmEntry: TChmEntry;
  TOCItem: TChmSiteMapItem;
begin
  if Selected and Assigned(Item) then
  begin
    case Mode of
      1:
      begin
        Memo1.Text := '';
        ChmEntry := TChmEntry(Item.Data);
        Memo1.Lines.Add('Name=' + ChmEntry.Name);
        Memo1.Lines.Add('Offset=' + IntToStr(ChmEntry.ContentOffset));
        Memo1.Lines.Add('Size=' + IntToStr(ChmEntry.DecompressedLength));
        Memo1.Lines.Add('Section=' + IntToStr(ChmEntry.ContentSection));
        ss := TStringStream.Create('');
        try
          ChmReader.ReadFileContent(ChmEntry.Name, ss);
          Memo1.Text := Memo1.Text + ss.DataString;
          StreamToFile(ss, 'tmp_file.txt');
        finally
          ss.Free();
        end;
      end;

      2, 3:
      begin
        TOCItem := TChmSiteMapItem(Item.Data);
        Memo1.Text := '';
        Memo1.Lines.Add('Text=' + TOCItem.Text);
        Memo1.Lines.Add('KeyWord=' + TOCItem.KeyWord);
        Memo1.Lines.Add('Local=' + TOCItem.Local);
        Memo1.Lines.Add('URL=' + TOCItem.URL);
        Memo1.Lines.Add('SeeAlso=' + TOCItem.SeeAlso);
        Memo1.Lines.Add('ImageNumber=' + IntToStr(TOCItem.ImageNumber));
        //Memo1.Lines.Add('IncreaseImageIndex=' + IntToStr(TOCItem.IncreaseImageIndex));
        Memo1.Lines.Add('Comment=' + TOCItem.Comment);
        Memo1.Lines.Add('FrameName=' + TOCItem.FrameName);
        Memo1.Lines.Add('WindowName=' + TOCItem.WindowName);
        Memo1.Lines.Add('Merge=' + TOCItem.Merge);

        ss := TStringStream.Create('');
        try
          ChmReader.ReadFileContent('/'+TOCItem.Local, ss);
          Memo1.Text := Memo1.Text + ss.DataString;
          StreamToFile(ss, 'tmp_file.txt');
        finally
          ss.Free();
        end;
      end;
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

procedure TFormMain.FillTestFiles();
begin
  FindAllFiles(ListBoxTestFiles.Items, '..', '*.chm');
end;

procedure TFormMain.TestDirListing(AFileName: string);
begin
  Memo1.Lines.Clear();
  ChmStream := TFileStream.Create(AFileName, fmOpenRead);
  ChmReader := TChmReader.Create(ChmStream, True);

  lvEntries.BeginUpdate();
  ChmReader.GetCompleteFileList(@FileEntryCallback);
  lvEntries.EndUpdate();
end;

procedure AddSitemapItemToList(lv: TListView; TOCItem: TChmSiteMapItem);
var
  li: TListItem;
  i: Integer;
begin
  li := lv.Items.Add();
  li.Data := TOCItem;
  li.Caption := TOCItem.Text;
  li.SubItems.Append(TOCItem.URL);
  for i := 0 to TOCItem.Children.Count-1 do
  begin
    AddSitemapItemToList(lv, TOCItem.Children.Item[i]);
  end;
end;

procedure TFormMain.TestTOC(AFileName: string);
var
  //s: string;
  i: Integer;
begin
  Memo1.Lines.Clear();
  ChmStream := TFileStream.Create(AFileName, fmOpenRead);
  ChmReader := TChmReader.Create(ChmStream, True);

  ChmToc := TChmSitemap.Create(stTOC);
  ChmReader.ReadTOCSitemap(ChmToc);

  lvEntries.BeginUpdate();
  for i := 0 to ChmToc.Items.Count-1 do
  begin
    AddSitemapItemToList(lvEntries, ChmToc.Items.Item[i]);
  end;
  lvEntries.EndUpdate();
end;

procedure TFormMain.TestIndex(AFileName: string);
var
  //s: string;
  i: Integer;
begin
  Memo1.Lines.Clear();
  ChmStream := TFileStream.Create(AFileName, fmOpenRead);
  ChmReader := TChmReader.Create(ChmStream, True);

  ChmIndex := TChmSitemap.Create(stIndex);
  ChmReader.ReadIndexSitemap(ChmIndex);

  lvEntries.BeginUpdate();
  for i := 0 to ChmIndex.Items.Count-1 do
  begin
    AddSitemapItemToList(lvEntries, ChmIndex.Items.Item[i]);
  end;
  lvEntries.EndUpdate();
end;

end.

