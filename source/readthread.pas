unit readthread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, threads, callstack,
  Ultibo, GlobalConst;

type
  TReadThread = class(TThread)
  protected
    FActive: boolean;
    FRecordSize : Longword;
    FBytesRead : Longword;
    FFirstMessageSent : Boolean;
    FRecordBufferP : pchar;
    FUpdateBufferP : pchar;
    FDeviceCloseRequested : boolean;
    FExecutionIsOver : boolean;
  protected
    procedure SetActive(state: boolean);
    procedure DeviceOpen; virtual; abstract;
    procedure DeviceClose; virtual; abstract;
    procedure ReadData; virtual; abstract;
    procedure WriteData(data: string); virtual; abstract;
    procedure OnRecordAvailable(recP : pchar; bufsize : integer); virtual; abstract;
    procedure OnConnectionEstablished; virtual; abstract;
    procedure SetRecordSize(ASize : longword); virtual;
    function ExtraBufferByteCount : integer; virtual; abstract;
  public
    SystemBooted : boolean;
    constructor Create(aRecordSize : Integer; CreateSuspended: Boolean);
    destructor Destroy; override;
    procedure Execute; override;
    procedure FastForward(secs : integer); virtual abstract;
    property Active: boolean read FActive write SetActive;
    property Terminated;
    property RecordSize : Longword read FRecordSize write SetRecordSize;
    property ExecutionIsOver : boolean read FExecutionIsOver;
  end;

implementation

uses
  logoutput;

constructor TReadThread.Create(aRecordSize : Integer; CreateSuspended : boolean);
begin
  inherited Create(CreateSuspended);

  FExecutionIsOver := false;
  SystemBooted := true;
  FActive := False;
  FRecordSize := aRecordSize;
  FBytesRead := 0;
  log('getting ' + inttostr(frecordsize + extrabufferbytecount) + ' bytes for record buffer');
  FRecordBufferP := Getmem(FRecordSize + ExtraBufferByteCount);
  fillchar(FRecordBufferP^, FRecordSize + ExtraBufferByteCount, 0);
  FUpdateBufferP := FRecordBufferP;
  FFirstMessageSent := False;

  FDeviceCloseRequested := False;
end;

destructor TReadThread.Destroy;
begin
  log('TReadThread.Destroy');
  DeviceClose;

  inherited Destroy;
end;


procedure TReadThread.SetActive(state: boolean);
begin
  if state=FActive then exit;

  if state then
    DeviceOpen
  else
    DeviceClose;

  FActive:=state;
end;

procedure TReadThread.SetRecordSize(ASize : longword);
begin
  if (ASize <> FRecordSize) then
  begin
    if (FRecordBufferP <> nil) then
       FreeMem(FRecordBufferP);

    FRecordSize := ASize;

    log('setrecordsize getting ' + inttostr(asize + ExtraBufferByteCount) + ' bytes for record buffer');

    getmem(FRecordBufferP, ASize + ExtraBufferByteCount);
    fillchar(FRecordBufferP^, ASize, 0);
    FUpdateBufferP := FRecordBufferP;

    FBytesRead := 0;
  end;
end;

procedure TReadThread.Execute;
begin
  ThreadSetName(GetCurrentThreadId, 'SPEEDY MSG HANDLER');
  {$ifdef RPI3}
  ThreadSetAffinity(GetCurrentThread, CPU_AFFINITY_3);
  ThreadMigrate(GetCurrentThreadId, 3);
  {$endif}

  try
    try
    while not Terminated do
    begin
      if (Active) then
      begin
         ReadData;

         if (FBytesRead = FRecordSize) then
         begin
            if (not FFirstMessageSent) then
            begin
              OnConnectionEstablished;
              FFirstMessageSent := True;
            end;

            OnRecordAvailable(FRecordBufferP, FRecordSize + ExtraBufferByteCount);

            FBytesRead := 0;
            FUpdateBufferP := FRecordBufferP;
         end;

         if (FDeviceCloseRequested) then
         begin
            Active := False;
            FDeviceCloseRequested := False;
         end;
      end;
    end;

    except
          on e:exception do
          begin
            log('Exception occurred: ' + e.message + ' at ' + inttohex(longword(exceptaddr), 8));
            dumpcallstack;
          end;
    end;
  finally
    log('Execution of read thread has completed');
    FreeMem(FRecordBufferP);
    FRecordBufferP := nil;
    FExecutionIsOver := true;
  end;
end;

end.

