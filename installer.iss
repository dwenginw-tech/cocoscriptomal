[Setup]
AppName=CocoScript
AppVersion=0.4.0
AppPublisher=AppleJuiceStudios
DefaultDirName={autopf}\CocoScript
DefaultGroupName=CocoScript
OutputDir=installer_output
OutputBaseFilename=cocoscript-0.4.0-setup
SetupIconFile=assets\icon.ico
Compression=lzma2
SolidCompression=yes
ChangesEnvironment=yes

[Files]
Source: "_build\default\src\driver\main.exe"; DestDir: "{app}"; DestName: "cocoscript.exe"; Flags: ignoreversion
Source: "tools\nasm.exe"; DestDir: "{app}\tools"; Flags: ignoreversion
Source: "tools\gcc.exe"; DestDir: "{app}\tools"; Flags: ignoreversion
Source: "tools\ld.exe"; DestDir: "{app}\tools"; Flags: ignoreversion
Source: "tools\as.exe"; DestDir: "{app}\tools"; Flags: ignoreversion
Source: "tools\*.dll"; DestDir: "{app}\tools"; Flags: ignoreversion skipifsourcedoesntexist
Source: "tools\lib\gcc\x86_64-w64-mingw32\15.2.0\*"; DestDir: "{app}\tools\lib\gcc\x86_64-w64-mingw32\15.2.0"; Flags: ignoreversion
Source: "tools\x86_64-w64-mingw32\lib\*"; DestDir: "{app}\tools\x86_64-w64-mingw32\lib"; Flags: ignoreversion
Source: "examples\*.coco"; DestDir: "{app}\examples"; Flags: ignoreversion
Source: "examples\*.asm"; DestDir: "{app}\examples"; Flags: ignoreversion skipifsourcedoesntexist
Source: "README.md"; DestDir: "{app}"; Flags: ignoreversion
Source: "LICENSE"; DestDir: "{app}"; Flags: ignoreversion
Source: "CHANGELOG.md"; DestDir: "{app}"; Flags: ignoreversion
Source: "STDLIB_REFERENCE.md"; DestDir: "{app}"; Flags: ignoreversion
Source: "src\lib\*.coco"; DestDir: "{app}\lib"; Flags: ignoreversion

[Icons]
Name: "{group}\CocoScript Examples"; Filename: "{app}\examples"
Name: "{group}\Documentation"; Filename: "{app}\README.md"
Name: "{group}\Standard Library Reference"; Filename: "{app}\STDLIB_REFERENCE.md"
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
