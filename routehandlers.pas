unit RouteHandlers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  UltraGenProcessors,
  { UltraGen units }
  ARCLass, UltraGenInterfaceClass,
  { web units }
  httpdefs, fphttpapp, httproute;

const APIURL = 'https://ghibliapi.herokuapp.com/';

procedure index(ARequest: TRequest; AResponse: TResponse);
procedure show(ARequest: TRequest; AResponse: TResponse);
procedure documentation(ARequest: TRequest; AResponse: TResponse);

implementation

uses fphttpclient;

procedure index(ARequest: TRequest; AResponse: TResponse);
var
  Output: string;
  Adapter: TUltraAdapter;
  Response: string;
begin
  Response := TFpHttpClient.SimpleGet(APIURL + 'films');
  Adapter := TUltraAdapter.Create('fromApp');
  Adapter.AddMember('template', 'list_films');
  Adapter.AddMember('response', Response);
  Output := ProcessTemplate(Adapter);
  AResponse.Content := Output;
end;

procedure documentation(ARequest: TRequest; AResponse: TResponse);
var
  Output: string;
  Adapter: TUltraAdapter;
begin
  Adapter := TUltraAdapter.Create('fromApp');
  Adapter.AddMember('template', 'documentation');
  Output := ProcessTemplate(Adapter);
  AResponse.Content := Output;
end;

procedure show(ARequest: TRequest; AResponse: TResponse);
var
  Output: string;
  Adapter: TUltraAdapter;
  Response: string;
begin
  Response := TFpHttpClient.SimpleGet(APIURL + 'films/' + ARequest.RouteParams['id']);
  Adapter := TUltraAdapter.Create('fromApp');
  Adapter.AddMember('template', 'show_film');
  Adapter.AddMember('id', ARequest.RouteParams['id']);
  Adapter.AddMember('response', Response);
  Output := ProcessTemplate(Adapter);
  AResponse.Content := Output;
end;

end.

