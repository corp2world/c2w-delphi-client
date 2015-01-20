unit C2WHttpService;

interface
uses SysUtils, Classes, C2WDataModels,
{$IFDEF INDY_STD}
     IdHTTP, IdIOHandlerSocket, IdSSLOpenSSL, IdHTTPHeaderInfo
{$ELSE}
     CWHTTP, CWIOHandlerSocket, CWSSLOpenSSL, CWHTTPHeaderInfo
{$ENDIF}
     , SuperObject ;

type
  TC2WProxyParams = class(TPersistent)
  protected
    FProxyParams: TIdProxyConnectionInfo;

    function GetProxyServer: AnsiString;
    procedure SetProxyServer(AValue: AnsiString);
    function GetProxyPort: Integer;
    procedure SetProxyPort(AValue: Integer);
    function GetProxyUserName: AnsiString;
    procedure SetProxyUserName(AValue: AnsiString);
    function GetProxyPassword: AnsiString;
    procedure SetProxyPassword(AValue: AnsiString);
  public
    property Params: TIdProxyConnectionInfo read FProxyParams
      write FProxyParams;
  published
    property ProxyServer: AnsiString read GetProxyServer write SetProxyServer;
    property ProxyPort: Integer read GetProxyPort write SetProxyPort;
    property ProxyUserName: AnsiString read GetProxyUserName write SetProxyUserName;
    property ProxyPassword: AnsiString read GetProxyPassword write SetProxyPassword;
    end;

  (*****************************************************)
  (*    Class HTTP-based Corp2World                    *)
  (*     message service implementation                *)
  (*****************************************************)
  TC2WHttpService = class(TComponent)
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  protected
    FHttpUrl: AnsiString;
    FProxyParams: TC2WProxyParams;

    FHttp: TIdHTTP;
    FSSLIOHandlerSocketOpenSSL: TIdSSLIOHandlerSocketOpenSSL;

    function GetUserName: AnsiString;
    procedure SetUserName(AValue: AnsiString);
    function GetPassword: AnsiString;
    procedure SetPassword(AValue: AnsiString);

    function Utf8Decode(const S: AnsiString): WideString;
    function Utf8ToUnicode(Dest: PWideChar; Source: PChar; MaxChars: Integer): Integer;  overload;
    function Utf8ToUnicode(Dest: PWideChar; MaxDestChars: Cardinal; Source: PChar; SourceBytes: Cardinal): Cardinal;   overload;
    function Utf8Encode(const WS: WideString): AnsiString;
    function UnicodeToUtf8(Dest: PChar; MaxDestBytes: Cardinal; Source: PWideChar; SourceChars: Cardinal): Cardinal;
    function Utf8ToAnsi(const S: AnsiString): AnsiString;
  published
    property HttpUrl: AnsiString read FHttpUrl write FHttpUrl;
    property UserName: AnsiString read GetUserName write SetUserName;
    property Password: AnsiString read GetPassword write SetPassword;
    property ProxyParams: TC2WProxyParams read FProxyParams;
  public
    procedure Send(AMessage: TC2WMessage; var AHttpResult: TC2WHttpResult);
    end;


implementation

{ TC2WProxyParams }
{$WARNINGS OFF}
function TC2WProxyParams.GetProxyServer: AnsiString;
begin
  Result:=FProxyParams.ProxyServer;
  end;

procedure TC2WProxyParams.SetProxyServer(AValue: AnsiString);
begin
  FProxyParams.ProxyServer:=AValue;
  end;

function TC2WProxyParams.GetProxyPort: Integer;
begin
  Result:=FProxyParams.ProxyPort;
  end;

procedure TC2WProxyParams.SetProxyPort(AValue: Integer);
begin
  FProxyParams.ProxyPort:=AValue;
  end;

function TC2WProxyParams.GetProxyUserName: AnsiString;
begin
  Result:=FProxyParams.ProxyUserName;
  end;

procedure TC2WProxyParams.SetProxyUserName(AValue: AnsiString);
begin
  FProxyParams.ProxyUserName:=AValue;
  FProxyParams.BasicAuthentication:=Length(AValue)>0;
  end;

function TC2WProxyParams.GetProxyPassword: AnsiString;
begin
  Result:=FProxyParams.ProxyPassword;
  end;

procedure TC2WProxyParams.SetProxyPassword(AValue: AnsiString);
begin
  FProxyParams.ProxyPassword:=AValue;
  end;
{$WARNINGS ON}
{ TC2WHttpService }

{    Create class    }

constructor TC2WHttpService.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHttp:= TIdHTTP.Create;
  FHttp.Request.CharSet:='utf-8';
  FHttp.Request.ContentType:='application/json';
  FHttp.ProtocolVersion:=pv1_1;
  FHttp.HTTPOptions:=FHttp.HTTPOptions+[hoInProcessAuth];
  FSSLIOHandlerSocketOpenSSL:=TIdSSLIOHandlerSocketOpenSSL.Create;
  FHttp.IOHandler := FSSLIOHandlerSocketOpenSSL;
  FProxyParams:=TC2WProxyParams.Create;
  FProxyParams.Params:=FHttp.ProxyParams;
  end;

{    Destroy class    }

destructor TC2WHttpService.Destroy;
begin
  FreeAndNil(FHttp);
  FreeAndNil(FSSLIOHandlerSocketOpenSSL);
  FreeAndNil(FProxyParams);
  inherited;
  end;

{    Send messege to Corp2World service   }
{$WARNINGS OFF}
procedure TC2WHttpService.Send(AMessage: TC2WMessage;
  var AHttpResult: TC2WHttpResult);
var
  XStream: TStringStream;
  XResultStream: TStringStream;
  XReceive: AnsiString;
  XObj: ISuperObject;
begin
  XStream:=TStringStream.Create(AMessage.AsJsonString);
  XResultStream:=TStringStream.Create('');
  try
    FHttp.Post(FHttpUrl,XStream,XResultStream);
    XResultStream.Seek(0, soFromBeginning);
    XReceive:=XResultStream.DataString;
{$IFNDEF INDY_STD}
    XReceive:=Utf8Decode(XResultStream.DataString);
{$ENDIF}
    XObj := SO(XReceive);
    if Assigned(AHttpResult) then begin
      AHttpResult.Status:=XObj['status'].AsString;
      AHttpResult.Response:=XObj['response'].AsString;
      end;
  finally
    XStream.Free;
    XResultStream.Free;
    end;
  end;


function TC2WHttpService.Utf8Encode(const WS: WideString): AnsiString;
var
  L: Integer;
  Temp: AnsiString;
begin
  Result := '';
  if WS = '' then Exit;
  SetLength(Temp, Length(WS) * 3); // SetLength includes space for null terminator

  L := UnicodeToUtf8(PChar(Temp), Length(Temp)+1, PWideChar(WS), Length(WS));
  if L > 0 then
    SetLength(Temp, L-1)
  else
    Temp := '';
  Result := Temp;
  end;
{$WARNINGS ON}
function TC2WHttpService.UnicodeToUtf8(
  Dest: PChar; MaxDestBytes: Cardinal; Source: PWideChar; SourceChars: Cardinal): Cardinal;
var
  i, count: Cardinal;
  c: Cardinal;
begin
  Result := 0;
  if Source = nil then Exit;
  count := 0;
  i := 0;
  if Dest <> nil then
  begin
    while (i < SourceChars) and (count < MaxDestBytes) do
    begin
      c := Cardinal(Source[i]);
      Inc(i);
      if c <= $7F then
      begin
        Dest[count] := Char(c);
        Inc(count);
      end
      else if c > $7FF then
      begin
        if count + 3 > MaxDestBytes then
          break;
        Dest[count] := Char($E0 or (c shr 12));
        Dest[count+1] := Char($80 or ((c shr 6) and $3F));
        Dest[count+2] := Char($80 or (c and $3F));
        Inc(count,3);
      end
      else //  $7F < Source[i] <= $7FF
      begin
        if count + 2 > MaxDestBytes then
          break;
        Dest[count] := Char($C0 or (c shr 6));
        Dest[count+1] := Char($80 or (c and $3F));
        Inc(count,2);
      end;
    end;
    if count >= MaxDestBytes then count := MaxDestBytes-1;
    Dest[count] := #0;
  end
  else
  begin
    while i < SourceChars do
    begin
      c := Integer(Source[i]);
      Inc(i);
      if c > $7F then
      begin
        if c > $7FF then
          Inc(count);
        Inc(count);
      end;
      Inc(count);
    end;
  end;
  Result := count+1;  // convert zero based index to byte count
  end;
{$WARNINGS OFF}
function TC2WHttpService.Utf8Decode(const S: AnsiString): WideString;
var
  L: Integer;
  Temp: WideString;
begin
  Result := '';
  if S = '' then Exit;
  SetLength(Temp, Length(S));

  L := Utf8ToUnicode(PWideChar(Temp), Length(Temp)+1, PChar(S), Length(S));
  if L > 0 then
    SetLength(Temp, L-1)
  else
    Temp := '';
  Result := Temp;
end;

function TC2WHttpService.Utf8ToAnsi(const S: AnsiString): AnsiString;
begin
  Result := Utf8Decode(S);
  end;
{$WARNINGS ON}
function TC2WHttpService.Utf8ToUnicode(Dest: PWideChar; Source: PChar; MaxChars: Integer): Integer;
var
  len: Cardinal;
begin
  len := 0;
  if Source <> nil then
    while Source[len] <> #0 do
      Inc(len);
  Result := Utf8ToUnicode(Dest, MaxChars, Source, len);
  end;

function TC2WHttpService.Utf8ToUnicode(Dest: PWideChar; MaxDestChars: Cardinal; Source: PChar; SourceBytes: Cardinal): Cardinal;
var
  i, count: Cardinal;
  c: Byte;
  wc: Cardinal;
begin
  if Source = nil then
  begin
    Result := 0;
    Exit;
  end;
  Result := Cardinal(-1);
  count := 0;
  i := 0;
  if Dest <> nil then
  begin
    while (i < SourceBytes) and (count < MaxDestChars) do
    begin
      wc := Cardinal(Source[i]);
      Inc(i);
      if (wc and $80) <> 0 then
      begin
        if i >= SourceBytes then Exit;          // incomplete multibyte char
        wc := wc and $3F;
        if (wc and $20) <> 0 then
        begin
          c := Byte(Source[i]);
          Inc(i);
          if (c and $C0) <> $80 then Exit;      // malformed trail byte or out of range char
          if i >= SourceBytes then Exit;        // incomplete multibyte char
          wc := (wc shl 6) or (c and $3F);
        end;
        c := Byte(Source[i]);
        Inc(i);
        if (c and $C0) <> $80 then Exit;       // malformed trail byte

        Dest[count] := WideChar((wc shl 6) or (c and $3F));
      end
      else
        Dest[count] := WideChar(wc);
      Inc(count);
    end;
    if count >= MaxDestChars then count := MaxDestChars-1;
    Dest[count] := #0;
  end
  else
  begin
    while (i < SourceBytes) do
    begin
      c := Byte(Source[i]);
      Inc(i);
      if (c and $80) <> 0 then
      begin
        if i >= SourceBytes then Exit;          // incomplete multibyte char
        c := c and $3F;
        if (c and $20) <> 0 then
        begin
          c := Byte(Source[i]);
          Inc(i);
          if (c and $C0) <> $80 then Exit;      // malformed trail byte or out of range char
          if i >= SourceBytes then Exit;        // incomplete multibyte char
        end;
        c := Byte(Source[i]);
        Inc(i);
        if (c and $C0) <> $80 then Exit;       // malformed trail byte
      end;
      Inc(count);
    end;
  end;
  Result := count+1;
  end;


{$WARNINGS OFF}
function TC2WHttpService.GetUserName: AnsiString;
begin
  Result:=FHttp.Request.UserName;
  end;

procedure TC2WHttpService.SetUserName(AValue: AnsiString);
begin
  FHttp.Request.UserName:=AValue;
  end;

function TC2WHttpService.GetPassword: AnsiString;
begin
  Result:=FHttp.Request.Password;
  end;

procedure TC2WHttpService.SetPassword(AValue: AnsiString);
begin
  FHttp.Request.Password:=AValue;
  end;

{$WARNINGS ON}

end.
