unit monitorshell;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Shell,
  GlobalTypes,
  Threads;

const
  MAX_ACTIVE_MESSAGES = 1000;

type
  TMonitorShellCommand = class(TShellCommand)
    fcs : trtlcriticalsection;
    FMessageReady : TSemaphoreHandle;
    FMessageList : TStringList;
    FCommandActive : boolean;
    FLastMessage : string;
  public
    constructor Create;
    destructor Destroy; override;
  private
    function GetLastMessage : string;
  public
    function DoHelp(AShell:TShell;ASession:TShellSession):Boolean; override;
    function DoInfo(AShell:TShell;ASession:TShellSession):Boolean; override;
    function DoCommand(AShell:TShell;ASession:TShellSession;AParameters:TStrings):Boolean; override;
    procedure Log(amsg : string);
    property Message : string write Log;
    property LastMessage : string read GetLastMessage;
  end;


var
  moncmd : TMonitorShellCommand;

implementation

uses
  RemoteShell,
  services,
  strutils,
  winsock2,
  GlobalConst;


constructor TMonitorShellCommand.Create;
begin
  inherited Create;
  Name:='MON';
  Flags:=SHELL_COMMAND_FLAG_INFO or SHELL_COMMAND_FLAG_HELP;

  initcriticalsection(fcs);

  FMessageReady := SemaphoreCreate(1);

  FMessageList := TStringList.Create;

  FCommandActive := False;
  FLastMessage:='';
end;

destructor TMonitorShellCommand.Destroy;
begin
  SemaphoreDestroy(FMessageReady);

  inherited Destroy;
end;

function TMonitorShellCommand.DoHelp(AShell:TShell;ASession:TShellSession):Boolean;
begin
 Result:=False;

 if AShell = nil then Exit;

 AShell.DoOutput(ASession,'Monitor logging output from your application.');
 AShell.DoOutput(ASession,'');
 AShell.DoOutput(ASession,'MON - monitor output indefinitely.');
 AShell.DoOutput(ASession,'      Use the escape sequence and quit telnet to terminate.');
 AShell.DoOutput(ASession,'MON <timeout> - monitor for <timeout> seconds');
 Result := True;
end;

function TMonitorShellCommand.DoInfo(AShell:TShell;ASession:TShellSession):Boolean;
begin
 Result:=False;

 if AShell = nil then Exit;

 AShell.DoOutput(ASession,'Monitor logging output from your application.');
 Result := True;
end;


function TMonitorShellCommand.DoCommand(AShell:TShell;ASession:TShellSession;AParameters:TStrings):Boolean;
var
  CommandSecs : String;
  msg : string;
  starttime : qword;
  timeused : integer;
  timeout : integer;
  StopNow : boolean;

begin
  Result:=False;

  if AShell = nil then Exit;

  CommandSecs := AShell.ParameterIndex(0,AParameters);

  StopNow := False;
  FCommandActive := true;

  if (CommandSecs <> '') then
  begin
    try
    timeout := strtoint(CommandSecs);
    except
      timeout := MAXLONGINT;
    end;
    AShell.DoOutput(ASession, 'Monitoring for ' + inttostr(timeout) + ' seconds.');
  end
  else
  begin
    timeout := MAXLONGINT;
    AShell.DoOutput(ASession, 'Monitoring until session closed (usually use <ctrl> ] and then type quit to close the session)');
  end;


  timeused := 0;
  starttime := gettickcount64;

  while (timeused < timeout) and (not StopNow) do
  begin
    // wait here until we have something to do.
    SemaphoreWaitEx(FMessageReady, 1000);

    //now lock the fmessage list so we can safely remove the
    //first message from it.
    EnterCriticalSection(fcs);

    While (FMessageList.Count > 0) do
    begin
      msg := FMessageList[0];
      FMessageList.Delete(0);

      LeaveCriticalSection(fcs);
      // and send that message to the output.
      StopNow := not AShell.DoOutput(ASession, msg);

      EnterCriticalSection(fcs);
    end;

    LeaveCriticalSection(fcs);

    // calculate time we have been runnin for in seconds.
    timeused := (gettickcount64 - starttime) div 1000;
  end;

  Result := true;
  FCommandActive := False;
end;

procedure TMonitorShellCommand.Log(amsg : string);
begin
  // if not active, we can still store the last n messages.
  if (not FCommandActive) then
    if (FMessageList.Count > MAX_ACTIVE_MESSAGES) then
       FMessageList.Delete(0);

  entercriticalsection(fcs);
  FMessageList.Add(amsg);
  FLastMessage := amsg;
  leavecriticalsection(fcs);

  // signal the semaphore to make the thread run.
  SemaphoreSignal(FMessageReady);
end;

function TMonitorShellCommand.GetLastMessage : string;
begin
  entercriticalsection(fcs);
  Result := FLastMessage;
  leavecriticalsection(fcs);
end;

initialization

  moncmd := TMonitorShellCommand.Create;
  ShellRegisterCommand(moncmd);

end.

