unit chm_doc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TChmEntry = class(TObject)
    Name: string;
    ContentSection: LongWord;
    ContentOffset: QWord;
    DecompressedLength: QWord;
  end;

  { TChmEntryList }

  TChmEntryList = class(TList)
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    function GetItem(AIndex: Integer): TChmEntry;
  end;

implementation

{ TChmEntryList }

procedure TChmEntryList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited Notify(Ptr, Action);
  if Action = lnDeleted then
  begin
    TChmEntry(Ptr).Free();
  end;
end;

function TChmEntryList.GetItem(AIndex: Integer): TChmEntry;
begin
  Result := TChmEntry(inherited Get(AIndex));
end;

end.

