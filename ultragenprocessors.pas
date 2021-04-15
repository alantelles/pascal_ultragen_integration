unit UltraGenProcessors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UltraGenInterfaceClass, ARClass, ASTCLass;

function ProcessTemplate(Adapter: TUltraAdapter):string;

implementation

uses
  dos;

function ProcessTemplate(Adapter: TUltraAdapter):string;
var
  PreludeLines: TStringList;
  UHome: string;
begin
  UHome := ParamStr(2);
  PreludeLines := TStringList.Create;
  PreludeLines.Add('addModulePath(["'+UHome + '", "modules"].path())');
  PreludeLines.Add('include @Core');
  Result := TUltraInterface.InterpretScript('templates/index.ultra', PreludeLines, Adapter);
end;

end.

