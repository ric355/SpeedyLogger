program lcdgauge;

{
To do list:
1. Change the processing so that we only draw the page when there is new data from the serial feed.
Need to think about what happens when there is no serial feed. Suppose just need an initial screen that
doesn't update thereafter.
}



{$mode objfpc}{$H+}

uses
  initoverrides,
  {$ifdef RPI1}
  RaspberryPi,
  BCM2835,
  BCM2708,
  {$endif}
  {$ifdef ZERO}
  RaspberryPi,
  BCM2835,
  BCM2708,         {driver for the Raspberry Pi SD host}
  {$endif}
  {$ifdef RPI2}
  RaspberryPi2,
  BCM2836,
  BCM2709,         {driver for the Raspberry Pi SD host}
  {$endif}
  {$ifdef RPI3}
  RaspberryPi3,
  BCM2837,
  BCM2710,
  {$endif}
  Platform,
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Threads,
  SysUtils,
  serial3comms,
  Classes,
  Console,
  strutils,
  MMC,             {Include the MMC/SD unit for access to the SD card}
  HTTP,            {Include the HTTP unit for the server classes}
  Winsock2,        {Include the Winsock2 unit so we can get the IP address}
  FileSystem,      {Include the File system so we have some files to serve up}
  FATFS,           {Plus the FAT file system unit}
  SMSC95XX,        {And the driver for the Raspberry Pi network adapter}
  DWCOTG,          {As well as the driver for the Raspberry Pi USB host}
  Shell,           {Add the Shell unit just for some fun}
  ShellFilesystem, {Plus the File system shell commands}
  ShellUpdate,
  RemoteShell,
  Ultibo,
  speeduinomessagehandler,
  SpeeduinoShell,
  ledthread,
  inifiles,
  framebuffer,
  devices,
  font,

  gauges,
  VGShapes,
  OpenVG,
  VC4,
  PiTFT28,
  PXL.TypeDef,
  PXL.Types,
  PXL.Boards.Types,
  PXL.Boards.Ultibo,
  PXL.Displays.Types,
  PXL.Displays.ILI9340,
  PXL.Bitmaps

  ;

type
  TLCDWriteMode = (wmFrameBuffer, wmAPI);

const
   NUM_GAUGES = 4;
   PinDC = Integer(GPIO_PIN_25);
   PinRST = Integer(GPIO_PIN_UNKNOWN);


type
  PathStruct = record
    x : int16_t;
    y : int16_t;
  end;


var
  LCDWriteMode : TLCDWriteMode;
  WindowHandle : TWindowHandle;
  SpeeduinoMsg : TSpeeduinoMessageHandler;
  IPAddress : String;
  HTTPListener : THTTPListener;
  HTTPFolder : THTTPFolder;
  Winsock2TCPClient : TWinsock2TCPClient;
  led : TLEDThread;
  iniFile : TIniFile;
  webenabled : string;
  xpos, ypos : longword;
  Width : Integer;
  Height : Integer;

//  runsecs : longint;
  fps : byte;
  startoffpstime : longint;
  loopcount : integer;
  gaugelist : array[1..NUM_GAUGES] of TGauge;

  FSoftwareBitmap: TBitmap;
  FDisplay: TCustomDisplay;
  FSystemCore: TUltiboSystemCore;
  FGPIO: TUltiboGPIO;
  FDataPort: TCustomDataPort;
  tempafr : byte;
  buf2 : array[0..319, 0..239] of longword;
  newbuf : array[0..(240*320)-1] of word;
  AFRPath : VGPath;
  AFRDataSize : Integer;
  AFRGraphCommands : array[0..640] of VGPathCommand;
  AFRGraphCoordinates : array[0..640] of PathStruct;
  FramebufferDevice:PFramebufferDevice;

  //TFTHandle : THandle;
  FrameBufferProperties : TFrameBufferProperties;
  NightMode : Boolean;
  tempval : byte;




procedure Log(str : string);
begin
  ConsoleWindowWriteLn(WindowHandle, DateTimeToStr(Now) + ': ' + str);
end;


procedure InitWebServer;
begin
  // the web server can be enabled by changing the ini file
  // see the datalog command on the console.
  // Once enabled, you can download the data logs
  // via your browser, but you won't be able to download the file that is currently
  // being written to.
  // You obviously need a network connection for this to work, so it's only useful
  // for bench testing.
  // WiFI is NOT supported.
  // It is not recommended that this be enabled when the pi is running on a vehicle

  Winsock2TCPClient:=TWinsock2TCPClient.Create;

  Log('Host name is ' + Winsock2TCPClient.LocalHost);

  IPAddress:=Winsock2TCPClient.LocalAddress;

  // wait for IP address to be assigned, then bind the web server to it

  if (IPAddress = '') or (IPAddress = '0.0.0.0') or (IPAddress = '255.255.255.255') then
   begin
    Log('IP address is ' + IPAddress);
    Log('Waiting for a valid IP address, make sure the network is connected');

    while (IPAddress = '') or (IPAddress = '0.0.0.0') or (IPAddress = '255.255.255.255') do
     begin
      Sleep(10);

      IPAddress:=Winsock2TCPClient.LocalAddress;
     end;
   end;

  Log('IP address is ' + IPAddress);

  Log('Creating HTTP listener');
  HTTPListener:=THTTPListener.Create;

  HTTPListener.Active:=True;

  Log('Creating HTTP folder / for C:\');
  HTTPFolder:=THTTPFolder.Create;
  HTTPFolder.Name:='/';
  HTTPFolder.Folder:='C:\';

  Log('Registering HTTP folder');
  HTTPListener.RegisterDocument('',HTTPFolder);

  Log('Web Server ready, point your browser to http://' + Winsock2TCPClient.LocalAddress + '/');

  Winsock2TCPClient.Free;
end;

procedure DrawGauges;
var
  i : integer;
  rnd : byte;

begin

  VGShapesStart(Width, Height);
  //vgshapesscale(320/1920, 240/1080);

  if (NightMode) then
     VGShapesBackground(0, 0, 0);

  if (LCDWriteMode = wmFrameBuffer) then
  begin
    // rotate 90 degrees
    VGRotate(-90);
    // move the origin up 320
   // vgtranslate(-320, 0);

    vgscale(-1.0, 1.0);

    //everything we write from here is rotated 90
    //so when we grab the page we can get a 240 wide by 320 tall section
    //and this will match the framebuffer shape. This in turn allows us to
    //move directly into the framebuffer with a dma transfer.
  end;

  if (LCDWriteMode = wmAPI) then
  begin
    // these are for mirroring when using buf2 and asphire
    vgtranslate(0, 240);
    vgscale(1.0, -1.0);
  end;

//  runsecs := trunc(gettickcount / 1000);
//  VGShapesText(0, Height - 30, 'secs: ' + inttostr(runsecs),VGShapesSansTypeface,30);

//      VGShapesText(20, 100, 'w: ' + inttostr(Width),VGShapesSansTypeface,15);
//      VGShapesText(20, 150, 'h: ' + inttostr(Height),VGShapesSansTypeface,15);
//      VGShapesText(20, 180, 'ang: ' + inttostr(angle),VGShapesSansTypeface,15);


  //simulated afr for development purposes

{  rnd := random(3) - 1;
  tempval := tempval + rnd;
  if (tempval > 100) then
    tempval := 100
  else
  if (tempval < 0) then
    tempval := 0;
  SpeeduinoMsg.rtStatus.tps := tempval;
  SpeeduinoMsg.rtStatus.o2 := tempval + 100;

  if (tempval mod 2) = 1 then
    SpeeduinoMsg.rtStatus.status1:=SpeeduinoMsg.rtStatus.status1 or BIT_STATUS1_DFCO
  else
    SpeeduinoMsg.rtStatus.status1 := SpeeduinoMsg.rtStatus.status1 and (not BIT_STATUS1_DFCO );
}
  //pull the real AFR from the status object
  tempafr := round(SpeeduinoMsg.rtStatus.o2 * 0.6);

  //are we all of the way across the page with the diagram yet?
  if (AFRDataSize < 319) then
  begin
    if (AFRDataSize = 0) then
    begin
      //generate move coordinates

     AFRGraphCoordinates[AFRDataSize].x := AFRDataSize;
     AFRGraphCoordinates[AFRDataSize].y := tempafr;

     AFRDataSize := AFRDataSize + 1;
    end
    else
    begin
      //must be at least one
      //generate line coordinates

      AFRGraphCoordinates[AFRDataSize].x := AFRDataSize;
      AFRGraphCoordinates[AFRDataSize].y := tempafr;
      AFRDataSize := AFRDataSize + 1;

      //generate move coordinates same ones

      AFRGraphCoordinates[AFRDataSize].x := AFRDataSize-1;
      AFRGraphCoordinates[AFRDataSize].y := tempafr;
      AFRDataSize := AFRDataSize + 1;
    end;

  end
  else
  begin
    // now we have to start pushing data around to make it scroll.
    move(AFRGraphCoordinates[2], AFRGraphCoordinates[0], 318 * SizeOf(PathStruct));
    for i := 0 to 317 do
       AFRGraphCoordinates[i].x := AFRGraphCoordinates[i].x - 2;

    // and put in the latest data item at the end.
    AFRGraphCoordinates[AFRDataSize-2].x := 319;
    AFRGraphCoordinates[AFRDataSize-2].y := tempafr;
    AFRGraphCoordinates[AFRDataSize-1].x := 319;
    AFRGraphCoordinates[AFRDataSize-1].y := tempafr;
  end;

  // O2 - this needs to be drawn before the path because we want the path to
  // go over the top of it.
  gaugelist[1].draw;


  // clear the path and append all of the data onto it. This makes the line start
  // to build up on the screen from the left hand side.
  VGClearPath(AFRPath, VG_PATH_CAPABILITY_ALL);

  VGAppendPathData(AFRPath, AFRDataSize * 4, @AFRGraphCommands[0], @AFRGraphCoordinates[0]);
  vgSetf(VG_STROKE_LINE_WIDTH, 2);

  if (NightMode) then
  begin
     VGShapesStroke(128, 128, 0, 1);
     VGShapesFill(128, 128, 0, 1);
  end
  else
  begin
     VGShapesStroke(0, 128, 0, 1);
     VGShapesFill(0, 128, 0, 1);
  end;
  VGDrawPath(AFRPath, VG_STROKE_PATH);

  if (assigned(SpeeduinoMsg)) and (SpeeduinoMsg.IsReceivingData) then
     VGShapesText(20, 220, formatfloat('###.##', SpeeduinoMsg.ByteCount/1024/1024)+' Mb',VGShapesSansTypeface,15)
  else
     VGShapesText(20, 220, '0.0 Mb',VGShapesSansTypeface,15);
  //write the name of the lof file


  VGShapesText(220, 20, 'fps: ', VGShapesSansTypeface, 15);

 // FPS
 gaugelist[2].draw;

 // RPM
 gaugelist[3].draw;

 if (NightMode) then
   gaugelist[4].SetColour(128, 128, 0)
 else
   gaugelist[4].SetColour(0, 128, 0);
 gaugelist[4].draw;

 VGShapesText(20, 205, SpeeduinoMsg.Filename, VGShapesSansTypeface, 8);

  if (SpeeduinoMsg.IsPaused) then
  begin
     VGShapesStroke(255, 0, 0, 1);
     VGShapesCircleOutline(300, 225, 15);
  end
  else
  begin
     VGShapesStroke(255, 0, 0, 1);
     VGShapesFill(255, 0, 0, 1);
     VGShapesCircle(300, 225, 15);
  end;

  //DFCO indicator (blue daytime, blue with yellow outline nighttime
  if (NightMode) then
  begin
   if (SpeeduinoMsg.rtStatus.status1 and BIT_STATUS1_DFCO) = BIT_STATUS1_DFCO then
   begin
      VGShapesStroke(128, 128, 0, 1);
      VGShapesFill(0, 0, 128, 1);
      VGShapesCircle(270, 225, 15);
   end
   else
   begin
     VGShapesStroke(128, 128, 0, 1);
     VGShapesCircleOutline(270, 225, 15);
   end;
  end
  else
  begin
    if (SpeeduinoMsg.rtStatus.status1 and BIT_STATUS1_DFCO) = BIT_STATUS1_DFCO  then
    begin
      VGShapesStroke(0, 0, 128, 1);
      VGShapesFill(0, 0, 128, 1);
      VGShapesCircle(270, 225, 15);
    end
    else
    begin
      VGShapesStroke(0, 0, 128, 1);
      VGShapesCircleOutline(270, 225, 15);
    end;
  end;


  vgtranslate(320,0);
  vgrotate(0);
  VGShapesEnd;

  if (LCDWriteMode = wmAPI) then
     VGReadPixels(@buf2[0,0], 320*4, VG_sARGB_8888, 0, 0, 320, 240)
  else
  begin
      // the framebuffer write must be byte swapped before it can be dma transferred
      // unfortunately this slows down the frame rate a little bit but it is not too
      // much of an impact.
      VGReadPixels(@newbuf[0], 240*2, VG_sRGB_565, 0, 0, 240, 320);
      for i := 0 to (240*320)-1 do
         newbuf[i] := SwapEndian(newbuf[i]);
  end;
end;


procedure initgraphics;

var
  i : Integer;

begin
  {Initialize OpenVG and the VGShapes unit}
  VGShapesInitWindowSize(0, 0, 320, 320);
  VGShapesInit(Width,Height);

  NightMode := False;

  gaugelist[1] := TValueGauge.Create(300, 80, 300, 160, 0, 200, 15, dtByte, 10, 100);
  TValueGauge(gaugelist[1]).RightJ := true;

  gaugelist[2] := TValueGauge.Create(280, 20, 50, 20, 0, 100, 0, dtByte, 0, 15);

  gaugelist[3] := TValueGauge.Create(200, 220, 50, 20, 0, 100, 0, dtWord, 0, 15);

  gaugelist[4] := THorizontalGauge.Create(20, 20, 140, 15, 0, 100, 0, dtByte);

  //Create a path for the updating AFR graph
  AFRPath := vgCreatePath(VG_PATH_FORMAT_STANDARD, VG_PATH_DATATYPE_S_16, 1, 0, 340, 340, 0); //VG_PATH_CAPABILITY_ALL);
  AFRDataSize := 0;

  vgSetf(VG_STROKE_LINE_WIDTH, 2);

  for i := 0 to 319 do
  begin
    AFRGraphCommands[i*2] := VG_MOVE_TO;
    AFRGraphCommands[i*2+1] := VG_LINE_TO;
  end;
end;

procedure RebootButton(Data : Pointer; Pin, Trigger : LongWord);
begin
 SystemRestart(100);
end;

procedure startstoplogging(data : Pointer; pin, trigger : longword);
begin
  if (SpeeduinoMsg.IsPaused) then
     SpeeduinoMsg.ResumeLogging
  else
     SpeeduinoMsg.PauseLogging;

  sleep(500);
  GPIOInputEvent(GPIO_PIN_17,GPIO_TRIGGER_LOW,INFINITE,@startstoplogging,nil);
end;

procedure nightmodetoggle(Data : Pointer; pin, trigger : longword);
begin
   NightMode := not NightMode;
   sleep(500);
   GPIOInputEvent(GPIO_PIN_27,GPIO_TRIGGER_LOW,INFINITE,@nightmodetoggle,nil);
end;

procedure smallerfont(Data : Pointer; pin, trigger : longword);
begin
  sleep(500);
   GPIOInputEvent(GPIO_PIN_23,GPIO_TRIGGER_LOW,INFINITE,@smallerfont,nil);
end;

procedure initlcdstuff;
begin
  if (LCDWriteMode = wmAPI) then
  begin
    FSystemCore := TUltiboSystemCore.Create;
    FGPIO := TUltiboGPIO.Create(FSystemCore);
    FDataPort := TUltiboSPI.Create(nil, TUltiboSPI.DefaultChipSelect, 32000000);

    // Create and configure embedded display.
    FDisplay := TDisplay.Create(FGPIO, FDataPort, PinDC, PinRST);
    FDisplay.LogicalOrientation := TDisplay.TOrientation.Landscape;

    // Display-bound bitmap that uses software rendering.
    FSoftwareBitmap := TBitmap.Create(FDisplay.Device);

    FDisplay.Initialize;

    FSoftwareBitmap.SetSize(320, 240);
    fsoftwarebitmap.PixelFormat:= TPixelFormat.A8R8G8B8;
  end;


  //adjust font size buttons
  GPIOPullSelect(GPIO_PIN_23,GPIO_PULL_UP);
  GPIOFunctionSelect(GPIO_PIN_23,GPIO_FUNCTION_IN);
  GPIOInputEvent(GPIO_PIN_23,GPIO_TRIGGER_LOW,INFINITE,@smallerfont,nil);

  GPIOPullSelect(GPIO_PIN_27,GPIO_PULL_UP);
  GPIOFunctionSelect(GPIO_PIN_27,GPIO_FUNCTION_IN);
  GPIOInputEvent(GPIO_PIN_27,GPIO_TRIGGER_LOW,INFINITE,@nightmodetoggle,nil);

  //gpio pin 17 (bottom button) is the start stop logging button.
  GPIOPullSelect(GPIO_PIN_17,GPIO_PULL_UP);
  GPIOFunctionSelect(GPIO_PIN_17,GPIO_FUNCTION_IN);
  GPIOInputEvent(GPIO_PIN_17,GPIO_TRIGGER_LOW,INFINITE,@startstoplogging,nil);
end;

procedure AttachGauges;
begin
  //gaugelist[1].AttachValue(@tempafr);
  gaugelist[1].AttachValue(@SpeeduinoMsg.rtStatus.o2);
  gaugelist[2].AttachValue(@fps);
  gaugelist[3].AttachValue(@SpeeduinoMsg.rtStatus.rpmlo);
  gaugelist[4].AttachValue(@SpeeduinoMsg.rtStatus.tps);
end;

procedure updatelcd;
begin
  if (LCDWriteMode = wmFrameBuffer) then
     FramebufferDeviceWrite(FramebufferDevice, 0, 0, @newbuf[0], 320*240, FRAMEBUFFER_TRANSFER_DMA)
  else
  begin
     // this is for when using buf2 with mirrored. Much slower than framebuffer.
     move(buf2[0,0], FSoftwareBitmap.Surface.Bits^, 320*240*4);

     // Draw software bitmap on display.
     FDisplay.Canvas.UseImage(FSoftwareBitmap);
     FDisplay.Canvas.TexQuad(FloatRect4(0.0, 0.0, FSoftwareBitmap.Width, FSoftwareBitmap.Height), IntColorWhite4);

     // Present picture on the display.
     FDisplay.Present;
      end;
end;

begin
  LCDWriteMode := wmFrameBuffer;
  LED := TLEDThread.Create;
  LED.FreeonTerminate := true;
  tempval := 0;

  //WindowHandle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_FULL,True);

  FramebufferDevice:=FramebufferDeviceFindByDescription('Adafruit PiTFT 2.8" LCD');
  //TFTHandle:=ConsoleWindowCreate(ConsoleDeviceFindByDescription('Framebuffer Console (' + DeviceGetName(@FramebufferDevice^.Device) + ')'),CONSOLE_POSITION_FULL,False);
  //ConsoleWindowWrite(TFTHandle,'LCD Gauge Started');

  FramebufferDeviceGetProperties(FrameBufferDevice, @FrameBufferProperties);

  initlcdstuff;

  initgraphics;

  // wait until the file system is ready
  while not DirectoryExists('C:\') do
   begin
    Sleep(10);
   end;
  Log('File system is ready');

  iniFile := TIniFile.Create('c:\speedylog.ini');
  webenabled := iniFile.ReadString('webserver', 'enabled', '0');
  if (webenabled = '1') then
    initwebserver;

  ConsoleWindowGetCursorXY(WindowHandle, xpos, ypos);
  fps := 0;
  tempafr := 0;

  //setup a reboot button
  GPIOPullSelect(GPIO_PIN_22,GPIO_PULL_UP);
  GPIOFunctionSelect(GPIO_PIN_22,GPIO_FUNCTION_IN);
  GPIOInputEvent(GPIO_PIN_22,GPIO_TRIGGER_LOW,INFINITE,@RebootButton,nil);


  while true do
  begin
    loopcount := 0;

    //create the handler thread

    SpeeduinoMsg := TSpeeduinoMessageHandler.Create;

    AttachGauges;

    //uncomment this line if you want to see data on the console. You
    //can change what is printed (per message) in the function itself. See the
    //TRealTimeStatus structure
    SpeeduinoMsg.screenwriter := nil;
    SpeeduinoMsg.Active := True;
    SpeeduinoMsg.Start;

    //begin requesting data.
    //the thread will re-request data automatically until terminated or paused.

    SpeeduinoMsg.Request;
    cmd.speedymessage := SpeeduinoMsg;

    SpeeduinoMsg.rtStatus.o2:=133;

    startoffpstime := gettickcount;


    while (not SpeeduinoMsg.Terminated) do
    begin
      drawgauges;

      // in case the connection is dropped, we have a 1s timeout.
      // this needs testing for when tunerstudio is connected via usb, as that causes a reset on the arduino.

      if (SpeeduinoMsg.timeoflastmessage < (gettickcount - 2000)) and (not SpeeduinoMsg.IsPaused) then
      begin
        SpeeduinoMsg.EndLogging;
        LED.Rate := 100;
      end;

      loopcount := loopcount + 1;
      if (loopcount = 5) then
      begin
        fps := round(5 / ((gettickcount - startoffpstime) / 1000));
        startoffpstime := gettickcount;
        loopcount := 0;
      end;

      updatelcd;
    end;

    SpeeduinoMsg.Free;

    sleep(100);
  end;
end.

