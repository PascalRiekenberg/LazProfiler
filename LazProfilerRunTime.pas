{
    Copyright (c) 2017 Pascal Riekenberg

    LazProfiler: Runtime unit

    See the file COPYING.modifiedLGPL.txt, included in this distribution,
    for details about the license.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit LazProfilerRunTime;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazProfilerCore;

var
  LazProfiler: TLazProfiler;

implementation

initialization

  LazProfiler := TLazProfiler.Create(ParamStr(0));

finalization

  FreeAndNil(LazProfiler);

end.

