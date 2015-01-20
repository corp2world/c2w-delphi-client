unit CWHeaderCoderBase;

interface

{$i CWCompilerDefines.inc}

uses
  Classes, CWGlobal, CWException;

type
  TIdHeaderDecodingNeededEvent = procedure(const ACharSet: AnsiString; const AData: TIdBytes; var VResult: AnsiString; var VHandled: Boolean) of object;
  TIdHeaderEncodingNeededEvent = procedure(const ACharSet, AData: AnsiString; var VResult: TIdBytes; var VHandled: Boolean) of object;

  TIdHeaderCoder = class(TObject)
  public
    class function Decode(const ACharSet: AnsiString; const AData: TIdBytes): AnsiString; virtual;
    class function Encode(const ACharSet, AData: AnsiString): TIdBytes; virtual;
    class function CanHandle(const ACharSet: AnsiString): Boolean; virtual;
  end;

  TIdHeaderCoderClass = class of TIdHeaderCoder;

  EIdHeaderEncodeError = class(EIdException);

var
  GHeaderEncodingNeeded: TIdHeaderEncodingNeededEvent = nil;
  GHeaderDecodingNeeded: TIdHeaderDecodingNeededEvent = nil;

function HeaderCoderByCharSet(const ACharSet: AnsiString): TIdHeaderCoderClass;
function DecodeHeaderData(const ACharSet: AnsiString; const AData: TIdBytes; var VResult: AnsiString): Boolean;
function EncodeHeaderData(const ACharSet, AData: AnsiString): TIdBytes;
procedure RegisterHeaderCoder(const ACoder: TIdHeaderCoderClass);
procedure UnregisterHeaderCoder(const ACoder: TIdHeaderCoderClass);

implementation

uses
  {$IFDEF VCL_XE3_OR_ABOVE}
  System.Types,
  {$ENDIF}
  SysUtils, CWResourceStringsProtocols;

type
  TIdHeaderCoderList = class(TList)
  public
    function ByCharSet(const ACharSet: AnsiString): TIdHeaderCoderClass;
  end;

var
  GHeaderCoderList: TIdHeaderCoderList = nil;

{ TIdHeaderCoder }

class function TIdHeaderCoder.Decode(const ACharSet: AnsiString; const AData: TIdBytes): AnsiString;
begin
  Result := '';
end;

class function TIdHeaderCoder.Encode(const ACharSet, AData: AnsiString): TIdBytes;
begin
  Result := nil;
end;

class function TIdHeaderCoder.CanHandle(const ACharSet: AnsiString): Boolean;
begin
  Result := False;
end;

{ TIdHeaderCoderList }

function TIdHeaderCoderList.ByCharSet(const ACharSet: AnsiString): TIdHeaderCoderClass;
var
  I: Integer;
  LCoder: TIdHeaderCoderClass;
begin
  Result := nil;
  // loop backwards so that user-defined coders can override native coders
  for I := Count-1 downto 0 do begin
    LCoder := TIdHeaderCoderClass(Items[I]);
    if LCoder.CanHandle(ACharSet) then begin
      Result := LCoder;
      Exit;
    end;
  end;
end;

function HeaderCoderByCharSet(const ACharSet: AnsiString): TIdHeaderCoderClass;
begin
  if Assigned(GHeaderCoderList) then begin
    Result := GHeaderCoderList.ByCharSet(ACharSet);
  end else begin
    Result := nil;
  end;
end;

function DecodeHeaderData(const ACharSet: AnsiString; const AData: TIdBytes; var VResult: AnsiString): Boolean;
var
  LCoder: TIdHeaderCoderClass;
begin
  LCoder := HeaderCoderByCharSet(ACharSet);
  if LCoder <> nil then begin
    VResult := LCoder.Decode(ACharSet, AData);
    Result := True;
  end else
  begin
    VResult := '';
    Result := False;
    if Assigned(GHeaderDecodingNeeded) then begin
      GHeaderDecodingNeeded(ACharSet, AData, VResult, Result);
    end;
    { RLebeau: TODO - enable this?
    if not LDecoded then begin
      raise EIdHeaderDecodeError.Create(RSHeaderDecodeError, [ACharSet]);
    end;
    }
  end;
end;

function EncodeHeaderData(const ACharSet, AData: AnsiString): TIdBytes;
var
  LCoder: TIdHeaderCoderClass;
  LEncoded: Boolean;
begin
  LCoder := HeaderCoderByCharSet(ACharSet);
  if LCoder <> nil then begin
    Result := LCoder.Encode(ACharSet, AData);
  end else
  begin
    Result := nil;
    LEncoded := False;
    if Assigned(GHeaderEncodingNeeded) then begin
      GHeaderEncodingNeeded(ACharSet, AData, Result, LEncoded);
    end;
    if not LEncoded then begin
      raise EIdHeaderEncodeError.CreateFmt(RSHeaderEncodeError, [ACharSet]);
    end;
  end;
end;

procedure RegisterHeaderCoder(const ACoder: TIdHeaderCoderClass);
begin
  if Assigned(ACoder) and Assigned(GHeaderCoderList) and (GHeaderCoderList.IndexOf(TObject(ACoder)) = -1) then begin
    GHeaderCoderList.Add(TObject(ACoder));
  end;
end;

procedure UnregisterHeaderCoder(const ACoder: TIdHeaderCoderClass);
begin
  if Assigned(GHeaderCoderList) then begin
    GHeaderCoderList.Remove(TObject(ACoder));
  end;
end;

initialization
  GHeaderCoderList := TIdHeaderCoderList.Create;
finalization
  FreeAndNil(GHeaderCoderList);

end.
