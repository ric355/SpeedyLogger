unit speeduinomessagehandler;

{$mode objfpc}{$H+}

interface

uses
  Ultibo, Classes, Platform, GlobalConst, Serial, SysUtils, threads,
  readthread,
  Serial3comms,
  bufferedwrites,
  shell,
  globaltypes;

const
  BIT_STATUS1_DFCO = 16;     // bit 4 (2^4) in speeduino code this is defined as the bit number.
  SERIAL_3_FULL_RECORD_SIZE = 76;      // uses an A request. Includes the 'A' in the response.

type
  TRealTimeStatus = record
    response : Char;
    secl : byte;
    status1 : byte;
    engine : byte;
    dwell : byte;
    maplo : byte;
    maphi : byte;
    iat : byte;
    clt : byte;
    batcorrection : byte;
    batteryv : byte;
    o2 : byte;
    egocorrection : byte;
    iatcorrection : byte;
    wue : byte;
    rpmlo : byte;
    rpmhi : byte;
    taeamount : byte;
    gammae : byte;
    ve : byte;
    afrtarget : byte;
    pw1lo : byte;
    pw1hi : byte;
    tpsdot : byte;
    advance : byte;
    tps : byte;
    loopslo : byte;
    loopshi : byte;
    freeramlo : byte;
    freeramhi : byte;
    boosttarget : byte;
    boostduty : byte;
    spark : byte;
    rpmdotlo : byte;
    rpmdothi : byte;
    ethanolpct : byte;
    flexcorrection : byte;
    flexigncorrection : byte;
    idleload : byte;
    testoutputs : byte;
    o2_2 : byte;
    baro : byte;
    candata : array[1..32] of byte;
    tpsadc : byte;
    errors : byte;
  end;


  TSpeeduinoMessageHandler = class(TReadThread)
  private
    FRootFolder : string;
    FOutputFilename : string;
    FLoggingPaused : Boolean;
    FLogfileOpen : boolean;
    FLoggingEnabled : boolean;
    FByteCount : integer;
    filenumber : integer;
    FLogStartTime : Integer;
    Ftimeoflastmessage : qword;
    FStreamCS : TRTLCriticalSection;
    FMarkNumber : Word;
    FMarkerRequested : Boolean;
    FMsgTimeSpin : TSpinHandle;
    FWriteBuffer : TBufferedWriter;
  private
    function GetByteCount : Longint;
    function GetFilename : string;
    function GetFileSizeStr : string;
    function GetTimeOfLastMessage : qword;
  public
    rtStatus : TRealTimeStatus;
    DataIsReady : procedure;
    constructor Create;
    destructor destroy; override;
    procedure requestFullImage;
    procedure DumpRealTimeData; virtual;
    procedure OnRecordAvailable(recP : pchar; bufsize : integer); override;
    procedure OnConnectionEstablished; override;
    procedure DeleteLogs;
    procedure ResetLogs;    // resets file number to zero
    procedure PauseLogging;  // pause logging, keep data coming in
    procedure ResumeLogging; // resume logging
    procedure StartLogging; // start logging (makes first request to ecu)
    procedure EndLogging;    // stop this log completely, terminate thread
    procedure RequestMarker;
    procedure SendTimeUpdate; virtual;
    property IsPaused : boolean read FLoggingPaused;
    property TimeOfLastMessage : qword read GetTimeOfLastMessage;
    property ByteCount : longint read GetByteCount;
    property OutputFilename : string read GetFilename;
    property FileSizeStr : string read GetFileSizeStr;
  end;


  TComPortReadThread=class(TSpeeduinoMessageHandler)
  private
    FConfi : TConf;

  protected
    procedure Config(cBaudRate : LongWord; cParity : Char; cDataBits : LongWord; cStopBits : LongWord; cFlowControl : LongWord); virtual;
    procedure DeviceOpen; override;
    procedure DeviceClose; override;
    procedure ReadData; override;
    procedure WriteData(data: string); override;
    procedure RequestDeviceClose;
    function ExtraBufferByteCount : integer; override;

  public
    procedure FastForward(secs : integer); override;
    constructor Create;
  published
  end;

implementation

uses
  logoutput;

constructor TComPortReadThread.Create; //(aRecordSize : Integer; CreateSuspended : boolean);
begin
  inherited Create; //(aRecordSize, CreateSuspended);

  Config(115200,'N',8,1,0);
end;

function TComPortReadThread.ExtraBufferByteCount : integer;
begin
  Result := 0;
end;

procedure TComPortReadThread.DeviceOpen;
begin
  SerialOpen(FConfi.coBaudRate,FConfi.coDataBits,FConfi.coStopBits,FConfi.coParity,FConfi.coFlowControl,0,0);
end;

procedure TComPortReadThread.DeviceClose;
begin
  SerialClose;
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

procedure TComPortReadThread.WriteData(data: string);
var
  Count3 : Longword;
begin
  Count3 := 0;
  if (SerialWrite(PChar(data),Length(data),Count3) <> ERROR_SUCCESS) then
    log('error writing to serial port');
end;

procedure TComPortReadThread.RequestDeviceClose;
begin
  FDeviceCloseRequested := True;
end;

procedure TComPortReadThread.FastForward(secs : integer);
begin
  // no FF function available on a serial connection
end;



constructor TSpeeduinoMessageHandler.Create;
var
  infoFile : TextFile;
begin
  inherited create(SERIAL_3_FULL_RECORD_SIZE, true);

  FRootFolder := 'c:\datalogs\';
  FMarkNumber := 1;
  FMarkerRequested := false;
  filenumber := 0;
  FLogStartTime := GetTickCount64;
  FLoggingPaused := True;
  InitializeCriticalSection(FStreamCS);
  FOutputFilename := 'Not Connected';
  FMsgTimeSpin := SpinCreate;

  FLogfileOpen := false;
  FLoggingEnabled := false;

  FTimeOfLastMessage := FLogStartTime; // this may need to change later

  assignfile(infofile, 'c:\datalog.inf');
  if (fileexists('c:\datalog.inf')) then
  begin
     reset(infofile);
     readln(infofile, filenumber);
     closefile(infofile);
  end;

  FWriteBuffer := TBufferedWriter.Create(30*1024, 5*1024, true, FRootFolder);
end;

destructor TSpeeduinoMessageHandler.destroy;
begin
  SpinDestroy(FMsgTimeSpin);

  try

  EnterCriticalSection(FStreamCS);
  try
    FWriteBuffer.Free;
  finally
    LeaveCriticalSection(FStreamCS);
  end;

  DeleteCriticalSection(FStreamCS);

  except
  end;

  inherited destroy;
end;

procedure TSpeeduinoMessageHandler.RequestMarker;
begin
  FMarkerRequested := true;
end;

function TSpeeduinoMessageHandler.GetFilename : string;
begin
  Result := FOutputFileName;
end;

function TSpeeduinoMessageHandler.GetFileSizeStr : string;
var
  b : longint;
begin
  b := ByteCount;
  if (b > 0) then
    Result := formatfloat('##0.0#', ByteCount/1024/1024)+' Mb'
  else
    Result := '0.0 Mb';
end;

function TSpeeduinoMessageHandler.GetByteCount : longint;
begin
  Result := FByteCount;
end;

function TSpeeduinoMessageHandler.GetTimeOfLastMessage : qword;
begin
  try
    SpinLock(FMsgTimeSpin);
    Result := Ftimeoflastmessage;

  finally
    SpinUnlock(FMsgTimeSpin);
  end;
end;

procedure TSpeeduinoMessageHandler.OnRecordAvailable(recP : pchar; bufsize : integer);
begin
  try
    SpinLock(FMsgTimeSpin);
    FTimeOfLastMessage := gettickcount64;
  finally
    SpinUnLock(FMsgTimeSpin);
  end;

  if (recp^ = 'A')then
  begin
    move(pchar(recp)^, rtstatus, bufsize);  //SERIAL_3_FULL_RECORD_SIZE

    // this has to be done regardless of whether logging is paused.
    if assigned(DataIsReady) then
       DataIsReady;

    if (not FLoggingPaused) then
    begin
      DumpRealTimeData;
    end;

    // again, even if logging is paused we still want the data for the gui.
    if (Active) then
    begin
       RequestFullImage;
    end;
  end;
end;

procedure TSpeeduinoMessageHandler.OnConnectionEstablished;
var
  row : string;
  infofile : TextFile;

begin
  log('Connection established');

  // assign the output filename first, because we want it assigned even if we
  // don't open the file, since it is also used for saving RTT data.

  FOutputFilename := 'dl'+format('%.6d',[filenumber])+'.msl';

  if (FLoggingPaused) or (not FLoggingEnabled) then
  begin
    log('Not opening log file because logging is paused or disabled.');
    exit;
  end
  else
  if (FLogFileOpen) then
  begin
    log('Not opening log file because it is already open');
    exit;           // no need to do anything if already open.
  end;

  // we need to work out what filename to use.
  // first, look in the file datalo.inf for the file number

  try
  assignfile(infofile, 'c:\datalog.inf');
  if (fileexists('c:\datalog.inf')) then
  begin
     reset(infofile);
     readln(infofile, filenumber);
     closefile(infofile);
  end;
  except
    on e: exception do log('exception writing datalog.inf' + e.message);
  end;

  //open the file for writing

  try
    log('starting new file ' + fOutputfilename);
    FWriteBuffer.StartNewFile(fOutputFilename);
  except
    on e: exception do log('exception starting new file '+fOutputFilename+' ' + e.message);
  end;

  // update the file number for the next iteration

  try
  filenumber := filenumber + 1;
  rewrite(infofile);
  writeln(infofile, filenumber);
  closefile(infofile);
  except
    on e: exception do log('exception old rewrite infofile '+e.message);
  end;

  //write the headers to the file
  row := 'time'
    + #9 + 'RPM'
    + #9 + 'MAP'
    + #9 + 'Engine'
    + #9 + 'Secl'
    + #9 + 'status1'
    + #9 + 'dwell'
    + #9 + 'IAT'
    + #9 + 'CLT'
    + #9 + 'GammaBat'
    + #9 + 'BatteryV'
    + #9 + 'AFR'
    + #9 + 'GammaEGO'
    + #9 + 'GammaAir'
    + #9 + 'GammaWarm'
    + #9 + 'Accel Enrich'
    + #9 + 'GammaE'
    + #9 + 'VE'
    + #9 + 'AFR Target'
    + #9 + 'PW1'
    + #9 + 'TPSdot'
    + #9 + 'Advance'
    + #9 + 'TPS'
    + #9 + 'Loops'
    + #9 + 'Boost Target'
    + #9 + 'Boost Duty'
    + #9 + 'Spark'
    + #9 + 'RPMdot'
    + #9 + 'Ethanol'
    + #9 + 'Flex Corr'
    + #9 + 'Flex Ign Corr'
    + #9 + 'Idle Load'
    + #9 + 'O2_2'
    + #9 + 'Baro'
    + #9 + 'Error#'
    ;
    row := row + #13 + #10;

  try
    FWriteBuffer.AddRow(row)
  except
    on e: exception do log('exception fwritebuffer.addrow '+e.message);

  end;

  row := #9 + 'rpm'
    + #9 + 'psi'
    + #9 + 'bits'
    + #9 + 'sec'
    + #9 + ''
    + #9 + 'ms'
    + #9 + 'deg'
    + #9 + 'deg'
    + #9 + '%'
    + #9 + 'v'
    + #9 + 'AFR'
    + #9 + '%'
    + #9 + '%'
    + #9 + '%'
    + #9 + '%'
    + #9 + '%'
    + #9 + '%'
    + #9 + 'AFR'
    + #9 + 'ms'
    + #9 + '%/s'
    + #9 + 'deg'
    + #9 + '%'
    + #9 + 'Loops'
    + #9 + ''
    + #9 + '%'
    + #9 + ''
    + #9 + 'rpm/s'
    + #9 + '%'
    + #9 + '%'
    + #9 + '%'
    + #9 + 'steps'
    + #9 + 'AFR'
    + #9 + 'psi'
    + #9 + ''
    ;

    row := row + #13 + #10;

  FWriteBuffer.AddRow(row);

  FLogfileOpen := true;
end;

procedure TSpeeduinoMessageHandler.RequestFullImage;
begin
  RecordSize := SERIAL_3_FULL_RECORD_SIZE;
  WriteData('A');
end;

procedure TSpeeduinoMessageHandler.DumpRealTimeData;

var
  row : string;
  row2 : string;

begin
  // if the log file is not open, even though logging has been enabled,
  // then we can open it here.

  if (not FLogFileOpen) then
    OnConnectionEstablished;

  with rtstatus do
  begin
    row := floattostr((FTimeOfLastMessage - FLogStartTime)/1000)
     + #9 + inttostr(rpmhi * 256 + rpmlo)
     + #9 + inttostr(maphi * 256 + maplo)
     + #9 + inttostr(engine)

     + #9 + inttostr(secl)
     + #9 + inttostr(status1)
     + #9 + floattostr(dwell/10)
     + #9 + inttostr(iat - 40)
     + #9 + inttostr(clt - 40)
     + #9 + inttostr(batcorrection)
     + #9 + floattostr(batteryv/10)
     + #9 + floattostr(o2/10)
     + #9 + inttostr(egocorrection)
     + #9 + inttostr(iatcorrection)
     + #9 + inttostr(wue)
     + #9 + inttostr(taeamount)
     + #9 + inttostr(gammae)
     + #9 + inttostr(ve)
     + #9 + floattostr(afrtarget/10)
     + #9 + floattostr((pw1hi * 256 + pw1lo)/1000)
     + #9 + inttostr(tpsdot*10)
     + #9 + inttostr(advance)
     + #9 + inttostr(tps)
     + #9 + inttostr(loopshi*256+loopslo)
     + #9 + inttostr(boosttarget)
     + #9 + inttostr(boostduty)
     + #9 + inttostr(spark)
     + #9 + inttostr(rpmdothi*256+rpmdotlo)
     + #9 + inttostr(ethanolpct)
     + #9 + inttostr(flexcorrection)
     + #9 + inttostr(flexigncorrection)
     + #9 + inttostr(idleload * 2)     // idle load is divided by 2 by the ecu so that it fits into a byte
     + #9 + floattostr(o2_2/10)
     + #9 + inttostr(baro)
     + #9 + inttostr(errors)
     ;

      row := row + #13 + #10;
  end;

  FByteCount := FByteCount + Length(row);

  // write the data to the log

  try
    EnterCriticalSection(FStreamCS);

    FWriteBuffer.AddRow(row);

    if (FMarkerRequested) then
    begin
      row2 := 'MARK ' + Format('%.3d', [FMarkNumber]) + ' - manual - no date';
      FMarkNumber := FMarkNumber + 1;

      FWriteBuffer.AddRow(row2);

      FMarkerRequested := False;
      FByteCount := FByteCount + Length(row2);
    end;
  finally
    LeaveCriticalSection(FStreamCS);
  end;
end;

procedure TSpeeduinoMessageHandler.DeleteLogs;
var
  i : Integer;

begin
  // we don't delete the current one, and filenumber always points to the
  // NEXT file, hence the -2 below.

  for i := 0 to filenumber - 2 do
  begin
    try
      DeleteFile('c:\dl'+format('%.6d',[i])+'.msl');
    except
    end;
  end;
end;

procedure TSpeeduinoMessageHandler.ResetLogs;
var
  infofile : textfile;

begin
  filenumber := 0;
  assignfile(infofile, 'c:\datalog.inf');
  rewrite(infofile);

  try
    EnterCriticalSection(FStreamCS);

    //open the new text file for writing.
    FOutputFilename := 'dl'+format('%.6d',[filenumber])+'.msl';

    FWriteBuffer.StartNewFile(FOutputFilename)
  finally
    LeaveCriticalSection(FStreamCS);
  end;

  // update the file number for the next iteration
  filenumber := filenumber + 1;
  writeln(infofile, filenumber);
  closefile(infofile);
end;

procedure TSpeeduinoMessageHandler.PauseLogging;
begin
//  RequestDeviceClose;
  FLoggingPaused := True;
end;

procedure TSpeeduinoMessageHandler.ResumeLogging;
begin
  FLoggingPaused := False;
  FLoggingEnabled := True;
end;

procedure TSpeeduinoMessageHandler.StartLogging;
begin
  log('Starting logging');
  FLoggingEnabled := true;
  FLoggingPaused := False;
  Ftimeoflastmessage:= GetTickCount64;
  RequestFullImage;
end;

procedure TSpeeduinoMessageHandler.EndLogging;
begin
  // the device will be closed. From here, restarting must set active to true
  // and then call 'request' to kickstart the process again.
  FLoggingPaused := True;
  FLoggingEnabled := False;
  FLogfileOpen := false;
  FWriteBuffer.CloseLogFile;
end;

procedure TSpeeduinoMessageHandler.SendTimeUpdate;
begin
end;


end.

