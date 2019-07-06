unit SpeeduinoShell;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Shell,
  inifiles,
  speeduinomessagehandler;

type
  //Create a class for our shell command, descended from TShellCommand
  TSpeeduinoShellCommand = class(TShellCommand)
  public
   constructor Create;
  private

  public
   speedymessage : tspeeduinomessagehandler;
   //Override the DoHelp, DoInfo and DoCommand methods
   function DoHelp(AShell:TShell;ASession:TShellSession):Boolean; override;
   function DoInfo(AShell:TShell;ASession:TShellSession):Boolean; override;
   function DoCommand(AShell:TShell;ASession:TShellSession;AParameters:TStrings):Boolean; override;
  end;

  var
   cmd : TSpeeduinoShellCommand;

implementation

constructor TSpeeduinoShellCommand.Create;
begin
 {}
 inherited Create;

 //In the Create() method we have to set the name of our commmand

 //Name is the name of the command, eg what the user has to type
 Name:='DATALOG';

 //Flags tell the shell if this command provides help or info
 Flags:=SHELL_COMMAND_FLAG_INFO or SHELL_COMMAND_FLAG_HELP;
end;

function TSpeeduinoShellCommand.DoHelp(AShell:TShell;ASession:TShellSession):Boolean;
begin
 //The DoHelp method is called when someone types HELP <COMMAND>
 Result:=False;

 if AShell = nil then Exit;

 AShell.DoOutput(ASession,'DATALOG - control the logging functions');
 AShell.DoOutput(ASession,'DATALOG RESUME - start logging (retain file number)');
 AShell.DoOutput(ASession,'DATALOG PAUSE - stop logging (retain file number)');
 AShell.DoOutput(ASession,'DATALOG NEXT - move to next log file');
 AShell.DoOutput(ASession,'DATALOG CLEANUP - remove all files prior to current file number');
 AShell.DoOutput(ASession,'DATALOG RESET - restore the file nuber back to 0. WARNING: files will be overwritten');
 AShell.DoOutput(ASession,'DATALOG WEBON - Enable the web server (requires a restart)');
 AShell.DoOutput(ASession,'DATALOG WEBOFF - Disable the web server (requires a restart)');
 AShell.DoOutput(ASession,'RESTART - restart the system after config changes');
 Result := True;
end;

function TSpeeduinoShellCommand.DoInfo(AShell:TShell;ASession:TShellSession):Boolean;
begin
 //The DoInfo method is called when someone types INFO or INFO <COMMAND>
 Result:=False;

 if AShell = nil then Exit;

 AShell.DoOutput(ASession,'No info for DATALOG command at present');
 Result := True;
end;

function TSpeeduinoShellCommand.DoCommand(AShell:TShell;ASession:TShellSession;AParameters:TStrings):Boolean;
var
 Value:String;
 Parameter:String;
 ini: TIniFile;
begin
 //The DoCommand method is called when someone types our command in the shell
 //We also get any parameters they added in the AParameters object
 Result:=False;

 if AShell = nil then Exit;

 Value:=' ';

 //Get the parameter (if any)
 Parameter:=AShell.ParameterIndex(0,AParameters);
 if Length(Parameter) > 0 then
  begin
   Value:=' ' + Parameter + ' ';
   if (upcase(Parameter) = 'PAUSE') then
     begin
      //stop logging
     SpeedyMessage.PauseLogging;
     Result:=AShell.DoOutput(ASession,'Logging stopped');
    end
   else
   if (upcase(Parameter) = 'RESUME') then
   begin
    //start logging going again
    SpeedyMessage.ResumeLogging;
    Result:=AShell.DoOutput(ASession,'Logging started');
   end
   else
   if (upcase(Parameter) = 'NEXT') then
   begin
    SpeedyMessage.EndLogging;
    Result:=AShell.DoOutput(ASession,'Starting next log file');
   end
   else
   if (upcase(Parameter) = 'CLEANUP') then
   begin
     speedymessage.DeleteLogs;
     Result:=AShell.DoOutput(ASession,'Old logs removed; current one left active.');
   end
   else
   if (upcase(Parameter) = 'RESET') then
   begin
     speedymessage.ResetLogs;
     Result:=AShell.DoOutput(ASession,'Log file number set to zero. Logs will be overwritten.');
   end
   else
   if (upcase(Parameter) = 'WEBON') then
   begin
     ini := TIniFile.Create('c:\speedylog.ini');
     ini.WriteString('webserver','enabled', '1');
     ini.Free;
   end
   else
   if (upcase(Parameter) = 'WEBOFF') then
   begin
    ini := TIniFile.Create('c:\speedylog.ini');
    ini.WriteString('webserver','enabled', '0');
    ini.Free;
   end;
  end;

end;

initialization
 //Register our new shell command so it is available in any shell
   cmd := TSpeeduinoShellCommand.Create;
 ShellRegisterCommand(cmd);

end.
