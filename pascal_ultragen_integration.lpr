program pascal_ultragen_integration;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, dos,
  { you can add units after this }
  { Web units }
  fphttpapp, httpdefs, httproute,
  { application }
  RouteHandlers;

procedure TurnOffServer(ARequest: TRequest; AResponse: TResponse);
begin
  Application.Terminate;
end;

begin
  if ParamCount < 2 then
  begin
    Writeln('Insufficient parameters');
    WriteLn('Usage: program [int]port [str]ultragen_home');
  end
  else
  begin
    HTTPRouter.RegisterRoute('/', rmGet, @RouteHandlers.index);
    HTTPRouter.RegisterRoute('/film/:id', rmGet, @RouteHandlers.show);
    HTTPRouter.RegisterRoute('/doc', rmGet, @RouteHandlers.documentation);
    if GetEnv('CAN_TURN_OFF') <> '' then
      HTTPRouter.RegisterRoute('/off', rmDelete, @TurnOffServer);

    Application.Port := StrToInt(ParamStr(1));
    Application.Title := 'Integration demo';
    Application.Threaded := True;
    Application.Initialize;
    WriteLn('Serving application');
    WriteLn('UltraGen home: ', ParamStr(2));
    WriteLn('Port: ', ParamStr(1));
    Application.Run;
  end;
end.

