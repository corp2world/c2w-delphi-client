unit CWHeaderCoderPlain;

interface

{$i CWCompilerDefines.inc}

uses
  CWGlobal, CWHeaderCoderBase;

type
  TIdHeaderCoderPlain = class(TIdHeaderCoder)
  public
    class function Decode(const ACharSet: AnsiString; const AData: TIdBytes): AnsiString; override;
    class function Encode(const ACharSet, AData: AnsiString): TIdBytes; override;
    class function CanHandle(const ACharSet: AnsiString): Boolean; override;
  end;

  // RLebeau 4/17/10: this forces C++Builder to link to this unit so
  // RegisterHeaderCoder can be called correctly at program startup...
  (*$HPPEMIT '#pragma link "IdHeaderCoderPlain"'*)

implementation

uses
  SysUtils;

class function TIdHeaderCoderPlain.Decode(const ACharSet: AnsiString; const AData: TIdBytes): AnsiString;
begin
  Result := BytesToStringRaw(AData);
end;

class function TIdHeaderCoderPlain.Encode(const ACharSet, AData: AnsiString): TIdBytes;
begin
  Result := ToBytes(AData, Indy8BitEncoding);
end;

class function TIdHeaderCoderPlain.CanHandle(const ACharSet: AnsiString): Boolean;
begin
  Result := TextStartsWith(ACharSet, 'ISO'); {do not localize}
  if Result then begin
    // 'ISO-2022-JP' is handled by TIdHeaderCoder2022JP
    Result := not TextIsSame(ACharSet, 'ISO-2022-JP'); {do not localize}
    Exit;
  end;
  if not Result then begin
    Result := TextStartsWith(ACharSet, 'WINDOWS'); {do not localize}
    if not Result then begin
      Result := TextStartsWith(ACharSet, 'KOI8'); {do not localize}
      if not Result then begin
        Result := TextStartsWith(ACharSet, 'GB2312'); {do not localize}
        if not Result then begin
          Result := TextIsSame(ACharSet, 'US-ASCII');
        end;
      end;
    end;
  end;
end;

initialization
  RegisterHeaderCoder(TIdHeaderCoderPlain);
finalization
  UnregisterHeaderCoder(TIdHeaderCoderPlain);

end.