unit lcid_conv;

{ MS Locale ID to Windows Code Page converter }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LConvEncoding;

function LCIDToWinCP(ALCID: Word): Word;
function ConvToUTF8(ALCID: Word; AStr: string): string;

implementation

type
  T_LCID_CP = record
    LCID: Word;
    CP: Word;
  end;

const
  lcid_cp_list: array [0..93] of T_LCID_CP = (
    (LCID: 1052; CP: 1250),  // Albanian
    (LCID: 1050; CP: 1250),  // Croatian
    (LCID: 1029; CP: 1250),  // Czech
    (LCID: 1038; CP: 1250),  // Hungarian
    (LCID: 1045; CP: 1250),  // Polish
    (LCID: 1048; CP: 1250),  // Romanian - Romania
    (LCID: 2074; CP: 1250),  // Serbian - Latin
    (LCID: 1051; CP: 1250),  // Slovak
    (LCID: 1060; CP: 1250),  // Slovenian

    (LCID: 2092; CP: 1251),  // Azeri - Cyrillic
    (LCID: 1059; CP: 1251),  // Belarusian
    (LCID: 1026; CP: 1251),  // Bulgarian
    (LCID: 1087; CP: 1251),  // Kazakh
    (LCID: 1088; CP: 1251),  // Kyrgyz - Cyrillic
    (LCID: 1104; CP: 1251),  // Mongolian
    (LCID: 1049; CP: 1251),  // Russian
    (LCID: 2073; CP: 1251),  // Russian - Moldova
    (LCID: 3098; CP: 1251),  // Serbian - Cyrillic
    (LCID: 1092; CP: 1251),  // Tatar
    (LCID: 1058; CP: 1251),  // Ukrainian
    (LCID: 2115; CP: 1251),  // Uzbek - Cyrillic

    (LCID: 1078; CP: 1252),  // Afrikaans
    (LCID: 1069; CP: 1252),  // Basque
    (LCID: 1027; CP: 1252),  // Catalan
    (LCID: 1030; CP: 1252),  // Danish
    (LCID: 2067; CP: 1252),  // Dutch - Belgium
    (LCID: 1043; CP: 1252),  // Dutch - Netherlands
    (LCID: 3081; CP: 1252),  // English - Australia
    (LCID: 10249; CP: 1252), // English - Belize
    (LCID: 4105; CP: 1252),  // English - Canada
    (LCID: 9225; CP: 1252),  // English - Caribbean
    (LCID: 2057; CP: 1252),  // English - Great Britain
    (LCID: 16393; CP: 1252), // English - India
    (LCID: 6153; CP: 1252),  // English - Ireland
    (LCID: 8201; CP: 1252),  // English - Jamaica
    (LCID: 5129; CP: 1252),  // English - New Zealand
    (LCID: 13321; CP: 1252), // English - Phillippines
    (LCID: 7177; CP: 1252),  // English - Southern Africa
    (LCID: 11273; CP: 1252), // English - Trinidad
    (LCID: 1033; CP: 1252),  // English - United States
    (LCID: 12297; CP: 1252), // English - Zimbabwe
    (LCID: 1080; CP: 1252),  // Faroese
    (LCID: 1035; CP: 1252),  // Finnish
    (LCID: 2060; CP: 1252),  // French - Belgium
    (LCID: 11276; CP: 1252), // French - Cameroon
    (LCID: 3084; CP: 1252),  // French - Canada
    (LCID: 9228; CP: 1252),  // French - Congo
    (LCID: 12300; CP: 1252), // French - Cote d'Ivoire
    (LCID: 1036; CP: 1252),  // French - France
    (LCID: 5132; CP: 1252),  // French - Luxembourg
    (LCID: 13324; CP: 1252), // French - Mali
    (LCID: 6156; CP: 1252),  // French - Monaco
    (LCID: 14348; CP: 1252), // French - Morocco
    (LCID: 10252; CP: 1252), // French - Senegal
    (LCID: 4108; CP: 1252),  // French - Switzerland
    (LCID: 7180; CP: 1252),  // French - West Indies
    (LCID: 1110; CP: 1252),  // Galician
    (LCID: 3079; CP: 1252),  // German - Austria
    (LCID: 1031; CP: 1252),  // German - Germany
    (LCID: 5127; CP: 1252),  // German - Liechtenstein
    (LCID: 4103; CP: 1252),  // German - Luxembourg
    (LCID: 2055; CP: 1252),  // German - Switzerland
    (LCID: 1039; CP: 1252),  // Icelandic
    (LCID: 1057; CP: 1252),  // Indonesian
    (LCID: 1040; CP: 1252),  // Italian - Italy
    (LCID: 2064; CP: 1252),  // Italian - Switzerland
    (LCID: 2110; CP: 1252),  // Malay - Brunei
    (LCID: 1086; CP: 1252),  // Malay - Malaysia
    (LCID: 1044; CP: 1252),  // Norwegian - Bokml
    (LCID: 2068; CP: 1252),  // Norwegian - Nynorsk
    (LCID: 1046; CP: 1252),  // Portuguese - Brazil
    (LCID: 2070; CP: 1252),  // Portuguese - Portugal
    (LCID: 11274; CP: 1252), // Spanish - Argentina
    (LCID: 16394; CP: 1252), // Spanish - Bolivia
    (LCID: 13322; CP: 1252), // Spanish - Chile
    (LCID: 9226; CP: 1252),  // Spanish - Colombia
    (LCID: 5130; CP: 1252),  // Spanish - Costa Rica
    (LCID: 7178; CP: 1252),  // Spanish - Dominican Republic
    (LCID: 12298; CP: 1252), // Spanish - Ecuador
    (LCID: 17418; CP: 1252), // Spanish - El Salvador
    (LCID: 4106; CP: 1252),  // Spanish - Guatemala
    (LCID: 18442; CP: 1252), // Spanish - Honduras
    (LCID: 2058; CP: 1252),  // Spanish - Mexico
    (LCID: 19466; CP: 1252), // Spanish - Nicaragua
    (LCID: 6154; CP: 1252),  // Spanish - Panama
    (LCID: 15370; CP: 1252), // Spanish - Paraguay
    (LCID: 10250; CP: 1252), // Spanish - Peru
    (LCID: 20490; CP: 1252), // Spanish - Puerto Rico
    (LCID: 1034; CP: 1252),  // Spanish - Spain (Traditional)
    (LCID: 14346; CP: 1252), // Spanish - Uruguay
    (LCID: 8202; CP: 1252),  // Spanish - Venezuela
    (LCID: 1089; CP: 1252),  // Swahili
    (LCID: 2077; CP: 1252),  // Swedish - Finland
    (LCID: 1053; CP: 1252)   // Swedish - Sweden

  );

function LCIDToWinCP(ALCID: Word): Word;
var
  i: Integer;
begin
  Result := 1252; // Latin
  for i := Low(lcid_cp_list) to High(lcid_cp_list) do
  begin
    if lcid_cp_list[i].LCID = ALCID then
    begin
      Result := lcid_cp_list[i].CP;
      Exit;
    end;
  end;
end;

function ConvToUTF8(ALCID: Word; AStr: string): string;
var
  wcp: Word;
begin
  if (ALCID <> 0) or (ALCID <> 1033) then
  begin
    wcp := LCIDToWinCP(ALCID);
    // convert to codepage
    case wcp of
      1250: Result := CP1250ToUTF8(AStr);
      1251: Result := CP1251ToUTF8(AStr);
      1252: Result := CP1252ToUTF8(AStr);
    else
      Result := AStr;
    end;
  end
  else
    Result := AStr;
end;

end.

