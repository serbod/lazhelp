program chmmaker;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, LazFileUtils,
  unit1, CHMSiteMapEditor, lhelpcontrolpkg;

var
  i: Integer;
  Filename: String;

{$R *.res}

begin
  {$if declared(useHeapTrace)}
  globalSkipIfNoLeaks := true; // supported as of debugger version 3.2.0
  setHeapTraceOutput('chmmaker_trace.log'); // supported as of debugger version 3.2.0
  {$endIf}
  Application.Initialize;
  Application.CreateForm(TCHMForm, CHMForm);
  Application.CreateForm(TSitemapEditForm, SitemapEditForm);
  for i:=1 to Application.ParamCount do
  begin
    Filename:=ParamStr(i);
    if (Filename='') or (Filename[1]='-') then continue;
    CHMForm.OpenProject(CleanAndExpandFilename(Filename));
    break;
  end;
  Application.Run;
end.

