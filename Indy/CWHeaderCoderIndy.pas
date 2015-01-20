unit CWHeaderCoderIndy;

interface

{$i CWCompilerDefines.inc}

uses
  CWGlobal, CWHeaderCoderBase;

type
  TIdHeaderCoderIndy = class(TIdHeaderCoder)
  public
    class function Decode(const ACharSet: AnsiString; const AData: TIdBytes): AnsiString; override;
    class function Encode(const ACharSet, AData: AnsiString): TIdBytes; override;
    class function CanHandle(const ACharSet: AnsiString): Boolean; override;
  end;

  // RLebeau 4/17/10: this forces C++Builder to link to this unit so
  // RegisterHeaderCoder can be called correctly at program startup...
  (*$HPPEMIT '#pragma link "IdHeaderCoderIndy"'*)

implementation

uses
  CWGlobalProtocols;

class function TIdHeaderCoderIndy.Decode(const ACharSet: AnsiString; const AData: TIdBytes): AnsiString;
var
  LEncoding: TIdTextEncoding;
begin
  Result := '';
  try
    LEncoding := CharsetToEncoding(ACharSet);
    {$IFNDEF DOTNET}
    try
    {$ENDIF}
      Result := LEncoding.GetString(AData);
    {$IFNDEF DOTNET}
    finally
      LEncoding.Free;
    end;
    {$ENDIF}
  except
  end;
end;

class function TIdHeaderCoderIndy.Encode(const ACharSet, AData: AnsiString): TIdBytes;
var
  LEncoding: TIdTextEncoding;
begin
  Result := nil;
  try
    LEncoding := CharsetToEncoding(ACharSet);
    {$IFNDEF DOTNET}
    try
    {$ENDIF}
      Result := LEncoding.GetBytes(AData);
    {$IFNDEF DOTNET}
    finally
      LEncoding.Free;
    end;
    {$ENDIF}
  except
  end;
end;

class function TIdHeaderCoderIndy.CanHandle(const ACharSet: AnsiString): Boolean;
var
  LEncoding: TIdTextEncoding;
begin
  try
    LEncoding := CharsetToEncoding(ACharSet);
    Result := Assigned(LEncoding);
    {$IFNDEF DOTNET}
    if Result then begin
      LEncoding.Free;
    end;
    {$ENDIF}
  except
    Result := False;
  end;
end;

initialization
  RegisterHeaderCoder(TIdHeaderCoderIndy);
finalization
  UnregisterHeaderCoder(TIdHeaderCoderIndy);

end.
