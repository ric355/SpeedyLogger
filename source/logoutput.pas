unit logoutput;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Console, GlobalTypes, MonitorShell;

var
  ConsoleActivated : boolean = false;
  logfileopen : boolean = false;
  WindowHandle : TWindowHandle;
  logfile : Textfile;

procedure Log(str : string);

implementation

procedure Log(str : string);
var
  s : string;
begin
  s := DateTimeToStr(Now) +': ' + str;

  if (ConsoleActivated) then
    ConsoleWindowWriteLn(WindowHandle, s);

  if (logfileopen) then
  begin
    writeln(logfile, s);
    flush(logfile);
  end;

  moncmd.log(s);
end;


end.

