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
}
{
  $Log$
}

unit CWStream;

interface

{$I CWCompilerDefines.inc}

uses
{$IFDEF DOTNET}
  CWStreamNET
{$ELSE}
  CWStreamVCL
{$ENDIF};

type
{$IFDEF DOTNET}
  TIdStreamHelper = TIdStreamHelperNET;
{$ELSE}
  TIdStreamHelper = TIdStreamHelperVCL;
{$ENDIF}

implementation

end.

