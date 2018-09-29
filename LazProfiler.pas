{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit LazProfiler;

{$warn 5023 off : no warning about unused units}
interface

uses
  LazProfilerAddon, LazProfilerCore, LazProfilerRunTime, LazProfilerWindow, 
  vtvObject, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('LazProfilerAddon', @LazProfilerAddon.Register);
end;

initialization
  RegisterPackage('LazProfiler', @Register);
end.
