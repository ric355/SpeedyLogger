unit Serial3Comms;

{$mode objfpc}{$H+}

interface

uses
  Classes, Platform, GlobalConst, Serial;

const
   SERIAL_3_RECORD_SIZE = 76;

type
  TConf = record
    coBaudRate, coParity, coDataBits, coStopBits, coFlowControl : LongWord;
  end;

type
  TComPortReadThread=class(TThread)
  private
    FActive: boolean;
    FConfi : TConf;
    FRecordSize : Longword;
    FRecordBufferP : pchar;
    FDeviceCloseRequested : boolean;
    FBytesRead : Longword;
    FUpdateBufferP : pchar;
    FFirstMessageSent : Boolean;

    procedure DeviceOpen;
    procedure DeviceClose;
    procedure ReadData;

  public
    MustDie: boolean;

    procedure Open;
    procedure Close;

    function DataAvailable: boolean;
    function ReadChar: Char;

    function WriteData(data: string): integer;
    function WriteBuffer(var buf; size: integer): integer;

    procedure Config(cBaudRate : LongWord; cParity : Char; cDataBits : LongWord; cStopBits : LongWord; cFlowControl : LongWord); virtual;
    constructor Create(RecordSize : Integer; CreateSuspended: Boolean);

  protected
    procedure Execute; override;
    procedure OnRecordAvailable(recP : pointer); virtual;
    procedure OnConnectionEstablished; virtual;
    procedure SetActive(state: boolean);
    procedure RequestDeviceClose;
  published
    property Terminated;
    property Active: boolean read FActive write SetActive;
    property IsReceivingData : boolean read FFirstMessageSent;
  end;




implementation

procedure TComPortReadThread.Close;
begin
  Active:=false;
end;

procedure TComPortReadThread.DeviceClose;
begin
  SerialClose;
end;

function TComPortReadThread.DataAvailable: boolean;
begin

  result:=SerialAvailable;
end;

procedure TComPortReadThread.Open;
begin
  Active:=true;
end;

procedure TComPortReadThread.Config(cBaudRate : LongWord; cParity : Char; cDataBits : LongWord; cStopBits : LongWord; cFlowControl : LongWord);
begin

   FConfi.coBaudRate := cBaudRate;

   FConfi.coParity := SERIAL_PARITY_NONE;
   case cParity of
    'N', 'n': FConfi.coParity := SERIAL_PARITY_NONE;
    'O', 'o': FConfi.coParity := SERIAL_PARITY_ODD;
    'E', 'e': FConfi.coParity := SERIAL_PARITY_EVEN;
    'M', 'm': FConfi.coParity := SERIAL_PARITY_MARK;
    'S', 's': FConfi.coParity := SERIAL_PARITY_SPACE;
   end;

  FConfi.coDataBits := SERIAL_DATA_8BIT;
  case cDataBits of
    7: FConfi.coDataBits := SERIAL_DATA_7BIT;
    6: FConfi.coDataBits := SERIAL_DATA_6BIT;
    5: FConfi.coDataBits := SERIAL_DATA_5BIT;
  end;

  FConfi.coStopBits := SERIAL_STOP_1BIT;
  case cStopBits of
    2: FConfi.coStopBits := SERIAL_STOP_2BIT;
    15: FConfi.coStopBits := SERIAL_STOP_1BIT5;
  end;

  FConfi.coFlowControl := SERIAL_FLOW_NONE;
  case cFlowControl of
    0: FConfi.coFlowControl := SERIAL_FLOW_NONE;
    1: FConfi.coFlowControl := SERIAL_FLOW_RTS_CTS;
    2: FConfi.coFlowControl := SERIAL_FLOW_DSR_DTR;
  end;

end;

procedure TComPortReadThread.DeviceOpen;
begin

  SerialOpen(FConfi.coBaudRate,FConfi.coDataBits,FConfi.coStopBits,FConfi.coParity,FConfi.coFlowControl,0,0);
end;

procedure TComPortReadThread.OnConnectionEstablished;
begin
end;

function TComPortReadThread.ReadChar: Char;
var
  uCharac : Char;
  Count1 : LongWord;

begin
  Count1:=0;
  SerialRead(@uCharac,SizeOf(uCharac),Count1);
  result:=uCharac;
end;

procedure TComPortReadThread.ReadData;
var
  ResultCode : integer;
  Count : LongWord;
begin
   ResultCode:=SerialDeviceRead(SerialDeviceGetDefault,FUpdateBufferP,
                     FRecordSize - FBytesRead,SERIAL_READ_NON_BLOCK,
                     Count);

   if (Count > 0) then
   begin
     FBytesRead := FBytesRead + Count;
     FUpdateBufferP := FUpdateBufferP + Count;
   end;
end;

procedure TComPortReadThread.SetActive(state: boolean);
begin
  if state=FActive then exit;

  if state then DeviceOpen
  else DeviceClose;

  FActive:=state;
end;

function TComPortReadThread.WriteBuffer(var buf; size: integer): integer;
var
  Count2 : Longword;
begin
  Count2 := 0;
  SerialWrite(Pointer(@buf), size, Count2);
  result :=  Count2;
end;

function TComPortReadThread.WriteData(data: string): integer;
var
  Count3 : Longword;
begin
  Count3 := 0;
  SerialWrite(PChar(data),Length(data),Count3);
  result := Count3;
end;


constructor TComPortReadThread.Create(RecordSize : Integer; CreateSuspended : boolean);
begin
  inherited Create(CreateSuspended);

  FActive := False;
  FRecordSize := RecordSize;
  FBytesRead := 0;
  FRecordBufferP := Getmem(FRecordSize);
  FUpdateBufferP := FRecordBufferP;
  FFirstMessageSent := False;

  FDeviceCloseRequested := False;

  FConfi.coBaudRate := 9600;
  FConfi.coParity := SERIAL_PARITY_NONE;
  FConfi.coDataBits := SERIAL_DATA_8BIT;
  FConfi.coStopBits := SERIAL_STOP_1BIT;
  FConfi.coFlowControl := SERIAL_FLOW_NONE;
end;

procedure TComPortReadThread.Execute;
begin
  try
    while not Terminated do begin
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

            OnRecordAvailable(FRecordBufferP);
            FBytesRead := 0;
            FUpdateBufferP := FRecordBufferP;
         end;

         if (FDeviceCloseRequested) then
         begin
            Active := False;
            FDeviceCloseRequested := False;
         end;
      end;
      sleep(10);
    end;
  finally
    Active := False;
    FreeMem(FRecordBufferP);
    Terminate;
  end;

end;

procedure TComPortReadThread.OnRecordAvailable(recP : pointer);
begin
end;

procedure TComPortReadThread.RequestDeviceClose;
begin
  FDeviceCloseRequested := True;
end;

end.
