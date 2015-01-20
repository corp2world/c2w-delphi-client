unit C2WDataModels;

interface
uses SysUtils, Classes, SuperObject;

type
  (*****************************************************)
  (*    Class represent Property object.               *)
  (*    Property has name and value                    *)
  (*****************************************************)
  TC2WProperty = class(TCollectionItem)
  protected
    FName: AnsiString;
    FValue: AnsiString;
  public
    property Name: AnsiString read FName write FName;
    property Value: AnsiString read FValue write FValue;
    end;


  (*****************************************************)
  (*    Class represent Recipient object.              *)
  (*    Property has channel type and recipients list  *)
  (*****************************************************)
  TC2WRecipient = class(TCollectionItem)
    destructor Destroy; override;
  protected
    FChannelType: integer;
    FRecipientIds: TStringList;
  public
    property ChannelType: integer read FChannelType write FChannelType;
    property RecipientIds: TStringList read FRecipientIds write FRecipientIds;
    end;

  TC2WProperties = class(TCollection)
  private
    function GetItem(Index: Integer): TC2WProperty;
    procedure SetItem(Index: Integer; const Value: TC2WProperty);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create;
    function Add: TC2WProperty;
    function AddProperty(AName, AValue: ansistring): TC2WProperty;
    property Items[Index: Integer]: TC2WProperty
      read GetItem write SetItem; default;
  end;

  TC2WRecipients = class(TCollection)
  private
    function GetItem(Index: Integer): TC2WRecipient;
    procedure SetItem(Index: Integer; const Value: TC2WRecipient);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create;
    function Add: TC2WRecipient;
    function AddRecipient(AChannelType:integer;
      ARecipientIds: TStringList): TC2WRecipient;
    property Items[Index: Integer]: TC2WRecipient
      read GetItem write SetItem; default;
  end;

  (*****************************************************)
  (*    Class represent TC2WMessage object.            *)
  (*    Message has topic, text (main message content) *)
  (*       and additional properties and recipients.   *)
  (*****************************************************)
  TC2WMessage = class(TObject)
  public
    constructor Create(ATopic,AText: AnsiString);
    destructor Destroy; override;
  protected
    FTopic: AnsiString;
    FText: AnsiString;
    FTimestamp: Int64;
    FTest: boolean;
    FProperties: TC2WProperties;
    FRecipients: TC2WRecipients;

    function GetCurrentTimestamp: Int64;
    function GetJsonString: AnsiString;
  public
    property Topic: AnsiString read FTopic write FTopic;
    property Text: AnsiString read FText write FText;
    property Timestamp: Int64 read FTimestamp write FTimestamp;
    property Test: boolean read FTest write FTest;
    property Properties: TC2WProperties read FProperties write FProperties;
    property Recipients: TC2WRecipients read FRecipients write FRecipients;

    property AsJsonString: AnsiString read GetJsonString;
    procedure AddProperty(AName, AValue: AnsiString);
    procedure AddRecipient(AChannelType: integer; ARecipientIds: TStringList);
    end;

  (*****************************************************)
  (*    Class represent TC2WHttpResult object.         *)
  (*    Result has status, response                    *)
  (*    and additional properties.                     *)
  (*****************************************************)
  TC2WHttpResult = class(TObject)
  public
    constructor Create;
    destructor Destroy; override;
  protected
    FStatus: AnsiString;
    FResponse: AnsiString;
    FProperties: TC2WProperties;
  public
    property Status: AnsiString read FStatus write FStatus;
    property Response: AnsiString read FResponse write FResponse;
    property Properties: TC2WProperties read FProperties write FProperties;
    end;

implementation

{ TC2WProperties }

function TC2WProperties.Add: TC2WProperty;
begin
  Result:=TC2WProperty(inherited Add)
  end;

function TC2WProperties.AddProperty(AName, AValue: AnsiString): TC2WProperty;
begin
  Result:=TC2WProperty(Add);
  Result.Name:=AName;
  Result.Value:=AValue;
  end;

constructor TC2WProperties.Create;
begin
  inherited Create(TC2WProperty);
  end;

function TC2WProperties.GetItem(Index: Integer): TC2WProperty;
begin
  Result := TC2WProperty(inherited GetItem(Index))
  end;


procedure TC2WProperties.SetItem(Index: Integer; const Value: TC2WProperty);
begin
  inherited SetItem(Index, Value)
  end;

procedure TC2WProperties.Update(Item: TcollectionItem);
begin
  inherited Update(Item);
  end;

{ TC2WRecipient }

{    Destroy class    }

destructor TC2WRecipient.Destroy;
begin
  if Assigned(FRecipientIds) then FreeAndNil(FRecipientIds);
  inherited;
  end;

{ TC2WRecipients }

function TC2WRecipients.Add: TC2WRecipient;
begin
  Result:=TC2WRecipient(inherited Add)
  end;

function TC2WRecipients.AddRecipient(AChannelType: integer;
  ARecipientIds: TStringList): TC2WRecipient;
begin
  Result:=TC2WRecipient(Add);
  Result.ChannelType:=AChannelType;
  Result.RecipientIds:=ARecipientIds;
  end;

constructor TC2WRecipients.Create;
begin
  inherited Create(TC2WRecipient);
  end;

function TC2WRecipients.GetItem(Index: Integer): TC2WRecipient;
begin
  Result:=TC2WRecipient(inherited GetItem(Index))
  end;

procedure TC2WRecipients.SetItem(Index: Integer;
  const Value: TC2WRecipient);
begin
  inherited SetItem(Index, Value)
  end;

procedure TC2WRecipients.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  end;

{ TC2WMessage }

{    Create class    }

constructor TC2WMessage.Create(ATopic,AText: AnsiString);
begin
  FProperties:=TC2WProperties.Create;
  FRecipients:=TC2WRecipients.Create;
  FTopic:=ATopic;
  FText:=AText;
  FTest:= false;

  FTimeStamp:= GetCurrentTimeStamp;
  end;

{    Destroy class    }

destructor TC2WMessage.Destroy;
begin
  FreeAndNil(FProperties);
  FreeAndNil(FRecipients);
  inherited;
  end;

function TC2WMessage.GetCurrentTimestamp: Int64;
var
  XTimeStamp: TTimeStamp;
  XTimeJ : int64;
begin
  XTimeStamp:=DateTimeToTimeStamp(Now);
  XTimeJ:=XTimeStamp.Date;
  XTimeJ:=XTimeJ-719163;
  XTimeJ:=XTimeJ*86400000-3600000*2; //смещение часового пояса
  Result:=XTimeJ+XTimeStamp.Time;
  end;
{$WARNINGS OFF}
function TC2WMessage.GetJsonString: AnsiString;
var
  XObj, XObjPr: ISuperObject;
  i,j: integer;
  XRecipients: AnsiString;
begin
  XObjPr:=  SO();
  XObj:= SO();
  for i:=0 to FProperties.Count-1 do begin
    XObjPr.PutS(FProperties[i].Name,FProperties[i].Value)
    end;
  XObj.PutS('topic',FTopic);
  XObj.PutS('text',FText);
  XObj.PutI('timestamp',FTimeStamp);
  //obj.PutI('ttl',FTTL);
  XObj.PutO('properties',XObjPr);
  Result:=XObj.AsJSon;
  XRecipients:='';
  if FRecipients.Count<>0 then begin
    XRecipients:='"channelRecipients":{';
    for i:=0 to FRecipients.Count-1 do begin
      XRecipients:=XRecipients+'"'+IntToStr(FRecipients[i].ChannelType)+'":[';
      for j:=0 to FRecipients[i].RecipientIds.Count-1 do begin
        XRecipients:=XRecipients+'"'+FRecipients[i].FRecipientIds.strings[j]+'"';
        if j<FRecipients[i].FRecipientIds.Count-1 then  XRecipients:=XRecipients+',';
        end;
      XRecipients:=XRecipients+']';
      if i<FRecipients.Count-1 then  XRecipients:=XRecipients+',';
      end;
    XRecipients:=XRecipients+'}';
    end;
  if Length(XRecipients)>0 then begin
    Result[Length(Result)]:=',';
    Result:=Result+XRecipients+'}';
    end;
  end;
{$WARNINGS ON}
procedure TC2WMessage.AddProperty(AName, AValue: AnsiString);
begin
  FProperties.AddProperty(AName,AValue);
  end;

procedure TC2WMessage.AddRecipient(AChannelType: integer; ARecipientIds: TStringList);
begin
  FRecipients.AddRecipient(AChannelType, ARecipientIds);
  end;


{ TC2WHttpResult }

{    Create class    }

constructor TC2WHttpResult.Create;
begin
  FProperties:=TC2WProperties.Create;
  end;

{    Destroy class    }

destructor TC2WHttpResult.Destroy;
begin
  FreeAndNil(FProperties);
  inherited;
  end;

end.
