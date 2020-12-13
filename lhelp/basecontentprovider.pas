unit BaseContentProvider;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Laz2_XMLCfg;
  
type

  { TBaseContentProvider }

  TBaseContentProviderClass = Class of TBaseContentProvider;
  TBaseContentProvider = class(TObject)
  private
    FOnTitleChange: TNotifyEvent;
    FParent: TWinControl;
    FTitle: String;
    FConfig: TXMLConfig;
    FUpdateCount: Integer;
  protected
    FImageList: TImageList;
    function GetTitle: String; virtual;
    procedure SetTitle(const AValue: String); virtual;
    function IsUpdating: Boolean;
  public
    function CanGoBack: Boolean; virtual; abstract;
    function CanGoForward: Boolean; virtual; abstract;
    function GetHistory: TStrings; virtual; abstract;
    function LoadURL(const AURL: String; const AContext: THelpContext = -1): Boolean; virtual; abstract;
    procedure GoHome; virtual; abstract;
    procedure GoBack; virtual; abstract;
    procedure GoForward; virtual; abstract;
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
    procedure LoadPreferences(ACfg: TXMLConfig); virtual;
    procedure SavePreferences({%H-}ACfg: TXMLConfig); virtual;
    class function GetProperContentProviderClass(const AURL: String): TBaseContentProviderClass; virtual; abstract;
    constructor Create(AParent: TWinControl; AImageList: TImageList); virtual;
    destructor Destroy; override;
    property Parent: TWinControl read FParent;
    property Title: String read GetTitle write SetTitle;
    property OnTitleChange: TNotifyEvent read FOnTitleChange write FOnTitleChange;
  end;
  



  // returns false if the protocol has already been registered
  function RegisterContentProvider(const Protocol: String; ContentProvider: TBaseContentProviderClass): Boolean;
  // example: RegisterContentProvider('chm://', TChmContentProvider);
  
  function GetContentProviderClass(const Protocol: String): TBaseContentProviderClass;

  procedure ReadContentProviderList(AValue: TStrings);

implementation

var
  ContentProviders: TStringList;

function RegisterContentProvider(const Protocol: String;
  ContentProvider: TBaseContentProviderClass): Boolean;
begin
  Result := False;
  if ContentProviders.IndexOf(Protocol) > -1 then exit;
  ContentProviders.AddObject(Protocol, TObject(ContentProvider));
end;

function GetContentProviderClass(const Protocol: String): TBaseContentProviderClass;
var
  iIndex: Integer;
begin
  Result := nil;
  iIndex := ContentProviders.IndexOf(Protocol);
  if iIndex = -1 then Exit;
  
  Result := TBaseContentProviderClass(ContentProviders.Objects[iIndex]);
end;

procedure ReadContentProviderList(AValue: TStrings);
begin
  AValue.AddStrings(ContentProviders);
end;



{ TBaseContentProvider }

function TBaseContentProvider.GetTitle: String;
begin
  Result := FTitle;
end;

procedure TBaseContentProvider.SetTitle(const AValue: String);
begin
  FTitle := AValue;
  if Assigned(FOnTitleChange) then
    FOnTitleChange(Self);
end;

function TBaseContentProvider.IsUpdating: Boolean;
begin
  Result := FUpdateCount <> 0;
end;

procedure TBaseContentProvider.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TBaseContentProvider.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount < 0 then
    FUpdateCount := 0;
end;

procedure TBaseContentProvider.LoadPreferences(ACfg: TXMLConfig);
begin
  FConfig := ACfg;
end;

procedure TBaseContentProvider.SavePreferences(ACfg: TXMLConfig);
begin

end;

constructor TBaseContentProvider.Create(AParent: TWinControl; AImageList: TImageList);
begin
  FParent:= AParent;
  FImageList:= AImageList;
end;

destructor TBaseContentProvider.Destroy;
begin
  SavePreferences(FConfig);
  inherited Destroy;
end;

initialization
  ContentProviders := TStringList.Create;

finalization

  ContentProviders.Free;

end.

