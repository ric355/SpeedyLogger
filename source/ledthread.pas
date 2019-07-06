unit ledthread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, platform;

type
  TLEDThread = class(TThread)
    private
      FRate : integer;
    public
      constructor Create;
      property Rate : integer read FRate write FRate;
      procedure Execute; override;
  end;

implementation

constructor TLEDThread.Create;
begin
  FRate := 100;
  inherited Create(False);
end;

procedure TLEDThread.Execute;
begin
  ActivityLEDEnable;

  while not Terminated do
  begin
    ActivityLEDOn;
    sleep(FRate);
    ActivityLEDOff;
    sleep(FRate);
  end;

end;

end.

