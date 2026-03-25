[Setup]
AppName=CocoScript
AppVersion=0.1.0
AppPublisher=AppleJuiceStudios
DefaultDirName={autopf}\CocoScript
DefaultGroupName=CocoScript
OutputDir=installer_output
OutputBaseFilename=cocoscript-setup
SetupIconFile=assets\icon.ico
Compression=lzma2
SolidCompression=yes
ChangesEnvironment=yes

[Files]
Source: "_build\default\src\main.exe"; DestDir: "{app}"; DestName: "cocoscript.exe"; Flags: ignoreversion
Source: "tools\nasm.exe"; DestDir: "{app}\tools"; Flags: ignoreversion
Source: "tools\gcc.exe"; DestDir: "{app}\tools"; Flags: ignoreversion
Source: "tools\*.dll"; DestDir: "{app}\tools"; Flags: ignoreversion skipifsourcedoesntexist
Source: "examples\*"; DestDir: "{app}\examples"; Flags: ignoreversion
Source: "README.md"; DestDir: "{app}"; Flags: ignoreversion
Source: "LICENSE"; DestDir: "{app}"; Flags: ignoreversion

[Icons]
Name: "{group}\CocoScript Examples"; Filename: "{app}\examples"
Name: "{group}\Uninstall CocoScript"; Filename: "{uninstallexe}"

[Registry]
Root: HKLM; Subkey: "SYSTEM\CurrentControlSet\Control\Session Manager\Environment"; ValueType: expandsz; ValueName: "Path"; ValueData: "{olddata};{app};{app}\tools"; Check: NeedsAddPath('{app}')

[Code]
function NeedsAddPath(Param: string): Boolean;
var
  OrigPath: string;
begin
  if not RegQueryStringValue(HKLM, 'SYSTEM\CurrentControlSet\Control\Session Manager\Environment', 'Path', OrigPath) then
  begin
    Result := True;
    exit;
  end;
  Result := Pos(';' + Param + ';', ';' + OrigPath + ';') = 0;
end;
