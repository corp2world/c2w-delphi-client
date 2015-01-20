unit C2WClient;

interface

uses  SysUtils,  Classes,  SuperObject,  C2WDataModels, C2WHttpService;

procedure ExecuteExample;

implementation

const
  c2w_SystemFileName = 'c2wclient.sys';
  c2w_sysHttpUrl = 'httpurl';
  c2w_sysUserName = 'username';
  c2w_sysPassword = 'password';
  c2w_sysProxyServer = 'proxyserver';
  c2w_sysProxyPort = 'proxyport';
  c2w_sysProxyUserName = 'proxyusername';
  c2w_sysProxyPassword = 'proxypassword';
{$WARNINGS OFF}
procedure LoadSysParams(var AHttpUrl, AUserName, APassword: AnsiString;
  var AProxyServer: AnsiString; var AProxyPort: integer;
  var AProxyUserName, AProxyPassword: AnsiString);
var
  XCnfFile: AnsiString;
  XCnfParams: TStringList;
  XPortStr: AnsiString;
begin
  WriteLn('Load config params');
  XCnfFile:=ExtractFileDir(ParamStr(0))+'\'+c2w_SystemFileName;
  XCnfParams:= TStringList.Create;
  try
    if FileExists(XCnfFile) then begin
      XCnfParams.LoadFromFile(XCnfFile);
      AHttpUrl:=XCnfParams.Values[c2w_sysHttpUrl];
      AUserName:=XCnfParams.Values[c2w_sysUserName];
      APassword:=XCnfParams.Values[c2w_sysPassword];

      AProxyServer:=XCnfParams.Values[c2w_sysProxyServer];
      XPortStr:=XCnfParams.Values[c2w_sysProxyPort];
      if Length(XPortStr)>0 then begin
        try
          AProxyPort:=StrToInt(XPortStr);
        except
          WriteLn('Error!!! Parameter proxy port setting is wrong.');
          AProxyPort:=0;
          end;
        end
      else AProxyPort:=0;

      AProxyUserName:=XCnfParams.Values[c2w_sysProxyUserName];
      AProxyPassword:=XCnfParams.Values[c2w_sysProxyPassword];
      end
    else begin
      WriteLn('Config file not found');
      AHttpUrl:='https://corp2world.com:9443/rest/message/post';
      XCnfParams.Add(c2w_sysHttpUrl+'='+AHttpUrl);
      AUserName:='c2w_test_user';
      XCnfParams.Add(c2w_sysUserName+'='+AUserName);
      APassword:='123456';
      XCnfParams.Add(c2w_sysPassword+'='+APassword);

      AProxyServer:='';
      AProxyPort:=0;
      AProxyUserName:='';
      AProxyPassword:='';

      XCnfParams.Add(c2w_sysProxyServer+'=');
      XCnfParams.Add(c2w_sysProxyPort+'=');
      XCnfParams.Add(c2w_sysProxyUserName+'=');
      XCnfParams.Add(c2w_sysProxyPassword+'=');

      XCnfParams.SaveToFile(XCnfFile);
      WriteLn('Created config file with the default settings');
      end;
  finally
    XCnfParams.Free;
    end;
  end;

procedure SendMessage(
  AHttpUrl, AUserName, APassword: AnsiString;
  AProxyServer: AnsiString; var AProxyPort: integer;
  AProxyUserName, AProxyPassword: AnsiString);
var
  XTopic: AnsiString;
  XText: AnsiString;
  XMessage: TC2WMessage;
  XHttpService: TC2WHttpService;
  XResult: TC2WHttpResult;
  i: integer;
  XParamStr: string;
  XPos: integer;
  XPropertyName: string;
  XPropertyValue: string;
  XChannelTypeIdStr: string;
  XChannelTypeId: integer;
  XRecipentIds: string;
  XRecipentIdsList: TStringList;
begin
  if ParamCount<2 then begin
    WriteLn('Two arguments are required: message topic and message test');
    exit;
    end;
  WriteLn('Send message');
  XTopic:= ParamStr(1);
  XText:= ParamStr(2);

  XMessage:= TC2WMessage.Create(XTopic,XText);
  // If message has dynamic recipients
  // in format of <channel_type_id>=[<recipient1>,<recipient2>...]
  // or properties
  // in format of <p_propertyName>=<propertyValue>
  // parse them and assign to message
  for i:=3 to ParamCount do begin
    XParamStr:=ParamStr(i);
    XPos:=Pos('=',XParamStr);
    if XPos>0 then begin
      if XParamStr[1]='p' then begin
        XPropertyName:=Copy(XParamStr,3,XPos-3);
        XPropertyValue:=Copy(XParamStr,XPos+1,Length(XParamStr));
        XMessage.AddProperty(XPropertyName, XPropertyValue);
        end
      else begin
        XChannelTypeIdStr:=Copy(XParamStr,1,XPos-1);
        // Try to parse channel type ID
        try
          XChannelTypeId:=StrToInt(XChannelTypeIdStr);
        except
          WriteLn('Cannot parse channel type ID: ' + XChannelTypeIdStr);
          XChannelTypeId:=-1;
          end;
        if XChannelTypeId<0 then exit;
        XRecipentIds:=Copy(XParamStr,XPos+1,Length(XParamStr));
        XRecipentIdsList:= TStringList.Create;
        // Parse recipients list
        ExtractStrings([','], [], PChar(XRecipentIds), XRecipentIdsList);
        XMessage.AddRecipient(XChannelTypeId, XRecipentIdsList);
        end;
      end else
    if XParamStr='test' then XMessage.Test:=true;
    end;

  XHttpService:= TC2WHttpService.Create(nil);
  XHttpService.HttpUrl:=AHttpUrl;
  XHttpService.UserName:=AUserName;
  XHttpService.Password:=APassword;

  if Length(AProxyServer)>0 then begin
    XHttpService.ProxyParams.ProxyServer:=AProxyServer;
    XHttpService.ProxyParams.ProxyPort:=AProxyPort;
    if Length(AProxyUserName)>0 then begin
      XHttpService.ProxyParams.ProxyUserName:=AProxyUserName;
      XHttpService.ProxyParams.ProxyPassword:=AProxyPassword;
      end;
    end;

  XResult:=TC2WHttpResult.Create;
  try
    XHttpService.Send(XMessage,XResult);
    WriteLn('Result - '+XResult.Status);
    WriteLn('Response - '+XResult.Response);
  finally
    XHttpService.Free;
    XMessage.Free;
    XResult.Free;
    end;

  end;
{$WARNINGS ON}
procedure ExecuteExample;
var
  XHttpUrl: AnsiString;
  XUserName: AnsiString;
  XPassword: AnsiString;
  XProxyServer: AnsiString;
  XProxyPort: integer;
  XProxyUserName: AnsiString;
  XProxyPassword: AnsiString;
begin
  LoadSysParams(XHttpUrl, XUserName, XPassword,
    XProxyServer,XProxyPort,XProxyUserName,XProxyPassword);
  SendMessage(XHttpUrl, XUserName, XPassword,
    XProxyServer,XProxyPort,XProxyUserName,XProxyPassword);
  WriteLn('Press any key to quit');
  ReadLn;
  end;




end.
