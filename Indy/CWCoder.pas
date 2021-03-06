{
  $Project$
  $Workfile$
  $Revision$
  $DateUTC$
  $Id$

  This file is part of the Indy (Internet Direct) project, and is offered
  under the dual-licensing agreement described on the Indy website.
  (http://www.indyproject.org/)

  Copyright:
   (c) 1993-2005, Chad Z. Hower and the Indy Pit Crew. All rights reserved.


  $Log$


   Rev 1.18    27.08.2004 22:03:20  Andreas Hausladen
 Optimized encoders
 speed optimization ("const" for AnsiString parameters)


   Rev 1.17    7/23/04 7:00:14 PM  RLebeau
 Added extra exception handling to DecodeString() and Encode()


   Rev 1.16    2004.06.14 9:23:06 PM  czhower
 Bug fix.


   Rev 1.15    22/05/2004 12:05:20  CCostelloe
 Bug fix


   Rev 1.14    2004.05.20 1:39:20 PM  czhower
 Last of the IdStream updates


   Rev 1.13    2004.05.20 11:37:08 AM  czhower
 IdStreamVCL


   Rev 1.12    2004.05.20 11:13:10 AM  czhower
 More IdStream conversions


   Rev 1.11    2004.05.19 3:06:48 PM  czhower
 IdStream / .NET fix


   Rev 1.10    2004.02.03 5:44:56 PM  czhower
 Name changes


   Rev 1.9    1/27/2004 3:58:16 PM  SPerry
 StringStream ->IdStringStream


   Rev 1.8    27/1/2004 1:57:58 PM  SGrobety
 Additional bug fix


   Rev 1.6    11/10/2003 7:39:22 PM  BGooijen
 Did all todo's ( TStream to TIdStream mainly )


   Rev 1.5    2003.10.02 10:52:48 PM  czhower
 .Net


   Rev 1.4    2003.06.24 12:02:08 AM  czhower
 Coders now decode properly again.


   Rev 1.3    2003.06.13 6:57:08 PM  czhower
 Speed improvement


   Rev 1.2    2003.06.13 3:41:18 PM  czhower
 Optimizaitions.


   Rev 1.1    2003.06.13 2:24:06 PM  czhower
 Speed improvement


   Rev 1.0    11/14/2002 02:14:30 PM  JPMugaas
}
unit CWCoder;

interface

{$i CWCompilerDefines.inc}

uses
  Classes,
  CWBaseComponent,
  CWGlobal;

type
  TIdEncoder = class(TIdBaseComponent)
  public
    function Encode(const AIn: AnsiString; AByteEncoding: TIdTextEncoding = nil
      {$IFDEF STRING_IS_ANSI}; ASrcEncoding: TIdTextEncoding = nil{$ENDIF}
      ): AnsiString; overload;
    procedure Encode(const AIn: AnsiString; ADestStrings: TStrings; AByteEncoding: TIdTextEncoding = nil
      {$IFDEF STRING_IS_ANSI}; ASrcEncoding: TIdTextEncoding = nil{$ENDIF}
      ); overload;
    procedure Encode(const AIn: AnsiString; ADestStream: TStream; AByteEncoding: TIdTextEncoding = nil
      {$IFDEF STRING_IS_ANSI}; ASrcEncoding: TIdTextEncoding = nil{$ENDIF}
      ); overload;

    function Encode(ASrcStream: TStream; const ABytes: Integer = -1): AnsiString; overload;
    procedure Encode(ASrcStream: TStream; ADestStrings: TStrings; const ABytes: Integer = -1); overload;
    procedure Encode(ASrcStream: TStream; ADestStream: TStream; const ABytes: Integer = -1); overload; virtual; abstract;

    class function EncodeString(const AIn: AnsiString; AByteEncoding: TIdTextEncoding = nil
      {$IFDEF STRING_IS_ANSI}; ASrcEncoding: TIdTextEncoding = nil{$ENDIF}
      ): AnsiString; overload;
    class procedure EncodeString(const AIn: AnsiString; ADestStrings: TStrings;
      AByteEncoding: TIdTextEncoding = nil
      {$IFDEF STRING_IS_ANSI}; ASrcEncoding: TIdTextEncoding = nil{$ENDIF}
      ); overload;
    class procedure EncodeString(const AIn: AnsiString; ADestStream: TStream;
      AByteEncoding: TIdTextEncoding = nil
      {$IFDEF STRING_IS_ANSI}; ASrcEncoding: TIdTextEncoding = nil{$ENDIF}
      ); overload;

    class function EncodeBytes(const ABytes: TIdBytes): AnsiString; overload;
    class procedure EncodeBytes(const ABytes: TIdBytes; ADestStrings: TStrings); overload;
    class procedure EncodeBytes(const ABytes: TIdBytes; ADestStream: TStream); overload;

    class function EncodeStream(ASrcStream: TStream; const ABytes: Integer = -1): AnsiString; overload;
    class procedure EncodeStream(ASrcStream: TStream; ADestStrings: TStrings; const ABytes: Integer = -1); overload;
    class procedure EncodeStream(ASrcStream: TStream; ADestStream: TStream; const ABytes: Integer = -1); overload;
  end;

  TIdEncoderClass = class of TIdEncoder;

  TIdDecoder = class(TIdBaseComponent)
  protected
    FStream: TStream;
  public
    procedure DecodeBegin(ADestStream: TStream); virtual;
    procedure DecodeEnd; virtual;

    procedure Decode(const AIn: AnsiString); overload;
    procedure Decode(ASrcStream: TStream; const ABytes: Integer = -1); overload; virtual; abstract;

    class function DecodeString(const AIn: AnsiString; AByteEncoding: TIdTextEncoding = nil
      {$IFDEF STRING_IS_ANSI}; ADestEncoding: TIdTextEncoding = nil{$ENDIF}
      ): AnsiString;
    class function DecodeBytes(const AIn: AnsiString): TIdBytes;
    class procedure DecodeStream(const AIn: AnsiString; ADestStream: TStream);
  end;

  TIdDecoderClass = class of TIdDecoder;

implementation

uses
  {$IFDEF DOTNET}
  CWStreamNET,
  {$ELSE}
  CWStreamVCL,
  {$ENDIF}
  CWGlobalProtocols, SysUtils;

{ TIdDecoder }

procedure TIdDecoder.DecodeBegin(ADestStream: TStream);
begin
  FStream := ADestStream;
end;

procedure TIdDecoder.DecodeEnd;
begin
  FStream := nil;
end;

procedure TIdDecoder.Decode(const AIn: AnsiString);
var
  LStream: TMemoryStream;
begin
  LStream := TMemoryStream.Create;
  try
    WriteStringToStream(LStream, AIn, Indy8BitEncoding{$IFDEF STRING_IS_ANSI}, Indy8BitEncoding{$ENDIF});
    LStream.Position := 0;
    Decode(LStream);
  finally
    LStream.Free;
  end;
end;

class function TIdDecoder.DecodeString(const AIn: AnsiString; AByteEncoding: TIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ADestEncoding: TIdTextEncoding = nil{$ENDIF}
  ): AnsiString;
var
  LStream: TMemoryStream;
begin
  LStream := TMemoryStream.Create;
  try
    DecodeStream(AIn, LStream);
    LStream.Position := 0;
    if AByteEncoding = nil then begin
      AByteEncoding := Indy8BitEncoding;
    end;
    Result := ReadStringFromStream(LStream, -1, AByteEncoding{$IFDEF STRING_IS_ANSI}, ADestEncoding{$ENDIF});
  finally
    LStream.Free;
  end;
end;

class function TIdDecoder.DecodeBytes(const AIn: AnsiString): TIdBytes;
var
  LStream: TMemoryStream;
begin
  Result := nil;
  LStream := TMemoryStream.Create;
  try
    DecodeStream(AIn, LStream);
    LStream.Position := 0;
    ReadTIdBytesFromStream(LStream, Result, -1);
  finally
    FreeAndNil(LStream);
  end;
end;

class procedure TIdDecoder.DecodeStream(const AIn: AnsiString; ADestStream: TStream);
begin
  with Create(nil) do
  try
    DecodeBegin(ADestStream);
    try
      Decode(AIn);
    finally
      DecodeEnd;
    end;
  finally
    Free;
  end;
end;

{ TIdEncoder }

function TIdEncoder.Encode(const AIn: AnsiString; AByteEncoding: TIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ASrcEncoding: TIdTextEncoding = nil{$ENDIF}
  ): AnsiString;
var
  LStream: TMemoryStream;
begin
  if AIn <> '' then begin
    LStream := TMemoryStream.Create;
    try
      if AByteEncoding = nil then begin
        AByteEncoding := Indy8BitEncoding;
      end;
      WriteStringToStream(LStream, AIn, AByteEncoding{$IFDEF STRING_IS_ANSI}, ASrcEncoding{$ENDIF});
      LStream.Position := 0;
      Result := Encode(LStream);
    finally
      FreeAndNil(LStream);
    end;
  end else begin
    Result := '';
  end;
end;

procedure TIdEncoder.Encode(const AIn: AnsiString; ADestStrings: TStrings;
  AByteEncoding: TIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ASrcEncoding: TIdTextEncoding = nil{$ENDIF}
  );
var
  LStream: TMemoryStream;
begin
  LStream := TMemoryStream.Create;
  try
    if AByteEncoding = nil then begin
      AByteEncoding := Indy8BitEncoding;
    end;
    WriteStringToStream(LStream, AIn, AByteEncoding{$IFDEF STRING_IS_ANSI}, ASrcEncoding{$ENDIF});
    LStream.Position := 0;
    Encode(LStream, ADestStrings);
  finally
    FreeAndNil(LStream);
  end;
end;

procedure TIdEncoder.Encode(const AIn: AnsiString; ADestStream: TStream;
  AByteEncoding: TIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ASrcEncoding: TIdTextEncoding = nil{$ENDIF}
  );
var
  LStream: TMemoryStream;
begin
  LStream := TMemoryStream.Create;
  try
    if AByteEncoding = nil then begin
      AByteEncoding := Indy8BitEncoding;
    end;
    WriteStringToStream(LStream, AIn, AByteEncoding{$IFDEF STRING_IS_ANSI}, ASrcEncoding{$ENDIF});
    LStream.Position := 0;
    Encode(LStream, ADestStream);
  finally
    FreeAndNil(LStream);
  end;
end;

function TIdEncoder.Encode(ASrcStream: TStream; const ABytes: Integer = -1) : AnsiString;
var
  LStream: TMemoryStream;
begin
  LStream := TMemoryStream.Create;
  try
    Encode(ASrcStream, LStream, ABytes);
    LStream.Position := 0;
    Result := ReadStringFromStream(LStream, -1, Indy8BitEncoding);
  finally
    FreeAndNil(LStream);
  end;
end;

procedure TIdEncoder.Encode(ASrcStream: TStream; ADestStrings: TStrings; const ABytes: Integer = -1);
var
  LStream: TMemoryStream;
begin
  ADestStrings.Clear;
  LStream := TMemoryStream.Create;
  try
    Encode(ASrcStream, LStream, ABytes);
    LStream.Position := 0;
    ADestStrings.LoadFromStream(LStream);
  finally
    FreeAndNil(LStream);
  end;
end;

class function TIdEncoder.EncodeString(const AIn: AnsiString; AByteEncoding: TIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ASrcEncoding: TIdTextEncoding = nil{$ENDIF}
  ): AnsiString;
begin
  with Create(nil) do
  try
    Result := Encode(AIn, AByteEncoding{$IFDEF STRING_IS_ANSI}, ASrcEncoding{$ENDIF});
  finally
    Free;
  end;
end;

class procedure TIdEncoder.EncodeString(const AIn: AnsiString; ADestStrings: TStrings;
  AByteEncoding: TIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ASrcEncoding: TIdTextEncoding = nil{$ENDIF}
  );
begin
  with Create(nil) do
  try
    Encode(AIn, ADestStrings, AByteEncoding{$IFDEF STRING_IS_ANSI}, ASrcEncoding{$ENDIF});
  finally
    Free;
  end;
end;

class procedure TIdEncoder.EncodeString(const AIn: AnsiString; ADestStream: TStream;
  AByteEncoding: TIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ASrcEncoding: TIdTextEncoding = nil{$ENDIF}
  );
begin
  with Create(nil) do
  try
    Encode(AIn, ADestStream, AByteEncoding{$IFDEF STRING_IS_ANSI}, ASrcEncoding{$ENDIF});
  finally
    Free;
  end;
end;

class function TIdEncoder.EncodeBytes(const ABytes: TIdBytes): AnsiString;
var
  LStream: TMemoryStream;
begin
  if ABytes <> nil then begin
    LStream := TMemoryStream.Create;
    try
      WriteTIdBytesToStream(LStream, ABytes);
      LStream.Position := 0;
      Result := EncodeStream(LStream);
    finally
      FreeAndNil(LStream);
    end;
  end else begin
    Result := '';
  end;
end;

class procedure TIdEncoder.EncodeBytes(const ABytes: TIdBytes; ADestStrings: TStrings);
var
  LStream: TMemoryStream;
begin
  if ABytes <> nil then begin
    LStream := TMemoryStream.Create;
    try
      WriteTIdBytesToStream(LStream, ABytes);
      LStream.Position := 0;
      EncodeStream(LStream, ADestStrings);
    finally
      FreeAndNil(LStream);
    end;
  end;
end;

class procedure TIdEncoder.EncodeBytes(const ABytes: TIdBytes; ADestStream: TStream);
var
  LStream: TMemoryStream;
begin
  if ABytes <> nil then begin
    LStream := TMemoryStream.Create;
    try
      WriteTIdBytesToStream(LStream, ABytes);
      LStream.Position := 0;
      EncodeStream(LStream, ADestStream);
    finally
      FreeAndNil(LStream);
    end;
  end;
end;

class function TIdEncoder.EncodeStream(ASrcStream: TStream; const ABytes: Integer = -1): AnsiString;
begin
  if ASrcStream <> nil then begin
    with Create(nil) do
    try
      Result := Encode(ASrcStream, ABytes);
    finally
      Free;
    end;
  end else begin
    Result := '';
  end;
end;

class procedure TIdEncoder.EncodeStream(ASrcStream: TStream; ADestStrings: TStrings; const ABytes: Integer = -1);
begin
  if ASrcStream <> nil then begin
    with Create(nil) do
    try
      Encode(ASrcStream, ADestStrings, ABytes);
    finally
      Free;
    end;
  end;
end;

class procedure TIdEncoder.EncodeStream(ASrcStream: TStream; ADestStream: TStream; const ABytes: Integer = -1);
begin
  if ASrcStream <> nil then begin
    with Create(nil) do
    try
      Encode(ASrcStream, ADestStream, ABytes);
    finally
      Free;
    end;
  end;
end;

end.


