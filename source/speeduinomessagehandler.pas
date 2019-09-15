unit speeduinomessagehandler;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Serial3comms,
  shell;

const
  BIT_STATUS1_DFCO = 16;     // bit 4 (2^4) in speeduino code this is defined as the bit number.

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


  TSpeeduinoMessageHandler = class(TComPortReadThread)
  private
    FFileStream : TFileStream;
    baseptr : PChar;
    ptr : PChar;
    filenumber : integer;
    FLogStartTime : Integer;
    FPaused : Boolean;
    Ftimeoflastmessage : longint;
    FStreamCS : TRTLCriticalSection;
    FMarkNumber : Word;
    FMarkerRequested : Boolean;
  private
    function GetByteCount : Longint;
    function GetFilename : string;
  public
    rtStatus : TRealTimeStatus;
    screenwriter : procedure;
    constructor Create;
    destructor destroy; override;
    procedure request;
    procedure DumpRealTimeData;
    procedure OnRecordAvailable(recP : pointer); override;
    procedure OnConnectionEstablished; override;
    function MAPValue : integer;
    function VEValue : integer;
    function AdvanceValue : integer;
    procedure DeleteLogs;
    procedure ResetLogs;
    procedure PauseLogging;
    procedure ResumeLogging;
    procedure EndLogging;
    procedure RequestMarker;
    property IsPaused : boolean read FPaused;
    property TimeOfLastMessage : Integer read FTimeOfLastMessage;
    property ByteCount : longint read GetByteCount;
    property Filename : string read GetFilename;
  end;

implementation

constructor TSpeeduinoMessageHandler.Create;
var
  infoFile : TextFile;
begin
  inherited create(SERIAL_3_RECORD_SIZE, true);

  FFileStream := nil;
  FMarkNumber := 1;
  FMarkerRequested := false;
  Config(115200,'N',8,1,0);
  baseptr := @rtStatus;
  ptr := baseptr;
  filenumber := 0;
  FLogStartTime := GetTickCount64;
  FPaused := True;
  InitCriticalSection(FStreamCS);

  FTimeOfLastMessage := FLogStartTime; // this may need to change later

  assignfile(infofile, 'c:\datalog.inf');
  if (fileexists('c:\datalog.inf')) then
  begin
     reset(infofile);
     readln(infofile, filenumber);
     closefile(infofile);
  end;

end;

destructor TSpeeduinoMessageHandler.destroy;
begin
  FFileStream.Free;
  DoneCriticalSection(FStreamCS);
  inherited destroy;
end;

procedure TSpeeduinoMessageHandler.RequestMarker;
begin
  FMarkerRequested := true;
end;

function TSpeeduinoMessageHandler.GetFilename : string;
begin
  EnterCriticalSection(FStreamCS);
  try
  if FFileStream <> nil then
     Result := FFileStream.FileName
  else
     Result := 'Not Logging';
  finally
    LeaveCriticalSection(FStreamCS);
  end;
end;

function TSpeeduinoMessageHandler.GetByteCount : longint;
begin
  Result := 0;
  try
    EnterCriticalSection(FStreamCS);
    if (assigned(FFileStream)) then
       Result := FFileStream.Position;
  finally
    LeaveCriticalSection(FStreamCS);
  end;
end;

procedure TSpeeduinoMessageHandler.OnRecordAvailable(recP : Pointer);
begin
  // temporary move until I sort out buffer passing to the thread.
  move(pchar(recp)^, rtstatus, SERIAL_3_RECORD_SIZE);

  if assigned(screenwriter) then
     screenwriter;

  DumpRealTimeData;
  if (Active) then
     Request;
end;

procedure TSpeeduinoMessageHandler.OnConnectionEstablished;
var
  row : string;
  infofile : TextFile;

begin
  // we need to work out what filename to use.
  // first, look in the file datalo.inf for the file number
  assignfile(infofile, 'c:\datalog.inf');
  if (fileexists('c:\datalog.inf')) then
  begin
     reset(infofile);
     readln(infofile, filenumber);
     closefile(infofile);
  end;

  //open the stream (for now - going to change this to a textfile as the stream is overkill)
  FFileStream := TFileStream.Create('dl'+format('%.6d',[filenumber])+'.msl',fmCreate);

  // update the file number for the next iteration
  filenumber := filenumber + 1;
  rewrite(infofile);
  writeln(infofile, filenumber);
  closefile(infofile);

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

    + #13 + #10;

  FFileStream.Write(row[1], length(row));
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

    + #13 + #10;

  FFileStream.Write(row[1], length(row));

end;

procedure TSpeeduinoMessageHandler.Request;
begin
  FPaused := False;
  WriteData('A');
end;

procedure TSpeeduinoMessageHandler.DumpRealTimeData;

var
  row : string;
  row2 : string;

begin
  FTimeOfLastMessage := gettickcount64;

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
     + #9 + inttostr(idleload)
     + #9 + floattostr(o2_2/10)
     + #9 + inttostr(baro)
     + #9 + inttostr(errors)

     + #13 + #10;
  end;

  // write the data to the log

  try
    EnterCriticalSection(FStreamCS);
    FFileStream.Write(row[1], length(row));

    if (FMarkerRequested) then
    begin
       row2 := 'MARK ' + Format('%.3d', [FMarkNumber]) + ' - manual - no date';
       FMarkNumber := FMarkNumber + 1;
      FFileStream.Write(row2[1], length(row2));
      FMarkerRequested := False;
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
    if assigned(FFileStream) then
        FFileStream.Free;

    //open the stream (for now - going to change this to a textfile now I think
    FFileStream := TFileStream.Create('dl'+format('%.6d',[filenumber])+'.msl',fmCreate);
  finally
    LeaveCriticalSection(FStreamCS);
  end;

  // update the file number for the next iteration
  filenumber := filenumber + 1;
  writeln(infofile, filenumber);
  closefile(infofile);
end;

function TSpeeduinoMessageHandler.MAPValue : integer;
begin
  Result := rtstatus.maphi * 256 + rtstatus.maplo;
end;

function TSpeeduinoMessageHandler.VEValue : integer;
begin
  Result := rtstatus.ve;
end;

function TSpeeduinoMessageHandler.AdvanceValue : integer;
begin
  Result := rtstatus.Advance;
end;

procedure TSpeeduinoMessageHandler.PauseLogging;
begin
  RequestDeviceClose;
  FPaused := True;
end;

procedure TSpeeduinoMessageHandler.ResumeLogging;
begin
   if (FPaused) then
   begin
      FPaused := False;
      Active := True;
      FTimeOfLastMessage := GetTickCount64;
      Request;
   end;
end;

procedure TSpeeduinoMessageHandler.EndLogging;
begin
  // the device will be closed. From here, restarting must set active to true
  // and then call 'request' to kickstart the process again.
  FPaused := True;
  Terminate;
end;

end.

