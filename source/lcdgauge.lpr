program lcdgauge;

{
To do list:
1. Change the processing so that we only draw the page when there is new data from the serial feed.
Need to think about what happens when there is no serial feed.

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
  lastbuttonticks : integer;
  currentbuttonticks : integer;
  MarkerRequested : boolean;
  markerrequesttime : longint;



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
    //move directly into the framebuffer with a dma transfer, once the bytes
    //have been swapped into the correct order.
  end;

  if (LCDWriteMode = wmAPI) then
  begin
    // these are for mirroring when using buf2 and asphire
    vgtranslate(0, 240);
    vgscale(1.0, -1.0);
  end;

//  runsecs := trunc(gettickcount / 1000);
//  VGShapesText(0, Height - 30, 'secs: ' + inttostr(runsecs),VGShapesSansTypeface,30);

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


  //pull the real AFR from the status object, and calculate a y position from it.
  tempafr := round(SpeeduinoMsg.rtStatus.o2 * 0.6);

  //are we all of the way across the page with the graph yet?
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
    // now we have to start pushing data around and adjusting x values to make it scroll.
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

  // FPS
  VGShapesText(220, 20, 'fps: ', VGShapesSansTypeface, 15);
  gaugelist[2].draw;

  // RPM
  gaugelist[3].draw;

  if (NightMode) then
    gaugelist[4].SetColour(128, 128, 0)
  else
    gaugelist[4].SetColour(0, 128, 0);

  // TPS
  gaugelist[4].draw;

  // current log file name
  VGShapesText(20, 205, SpeeduinoMsg.Filename, VGShapesSansTypeface, 8);

  // data logging indicator
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

  //DFCO indicator (blue daytime, blue with yellow outline nighttime)
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

  if (MarkerRequested) and ((gettickcount - markerrequesttime) < 1000) then
  begin
    VGShapesText(150, 220, 'M', VGShapesSansTypeface, 15);
  end
  else
     MarkerRequested := False;

  // clean up
  vgtranslate(320,0);
  vgrotate(0);
  VGShapesEnd;

  if (LCDWriteMode = wmAPI) then
     VGReadPixels(@buf2[0,0], 320*4, VG_sARGB_8888, 0, 0, 320, 240)
  else
  begin
      // the framebuffer data must be byte swapped before it can be dma transferred
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

  //
  gaugelist[1] := TValueGauge.Create(300, 80, 300, 160, 0, 200, 15, dtByte, 10, 100);
  TValueGauge(gaugelist[1]).RightJ := true;

  //FPS (will be removed eventually)
  gaugelist[2] := TValueGauge.Create(280, 20, 50, 20, 0, 100, 0, dtByte, 0, 15);

  //RPM
  gaugelist[3] := TValueGauge.Create(200, 220, 50, 20, 0, 100, 0, dtWord, 0, 15);

  //TPS %
  gaugelist[4] := THorizontalGauge.Create(20, 20, 140, 15, 0, 100, 0, dtByte);

  //Create a path for the updating AFR graph
  AFRPath := vgCreatePath(VG_PATH_FORMAT_STANDARD, VG_PATH_DATATYPE_S_16, 1, 0, 340, 340, 0); //VG_PATH_CAPABILITY_ALL);
  AFRDataSize := 0;

  vgSetf(VG_STROKE_LINE_WIDTH, 2);

  // set up stroke data for the chart. Only needs to be done once, then the data
  // values can be shifted around to make the chart scroll sideways.
  for i := 0 to 319 do
  begin
    AFRGraphCommands[i*2] := VG_MOVE_TO;
    AFRGraphCommands[i*2+1] := VG_LINE_TO;
  end;
end;

procedure RebootButton(Data : Pointer; Pin, Trigger : LongWord);
begin
 // reboot the logger. Really only for development purposes when wanting to
 // restart the firmware after updating.
 SystemRestart(100);
end;

procedure startstoplogging(data : Pointer; pin, trigger : longword);
begin
 // Toggle the logging state.
  if (SpeeduinoMsg.IsPaused) then
     SpeeduinoMsg.ResumeLogging
  else
     SpeeduinoMsg.PauseLogging;

  sleep(500);
  GPIOInputEvent(GPIO_PIN_17,GPIO_TRIGGER_LOW,INFINITE,@startstoplogging,nil);
end;

procedure nightmodetoggle(Data : Pointer; pin, trigger : longword);
begin
   // toggle night mode, to reduce glare when it's dark.
   NightMode := not NightMode;
   sleep(500);
   GPIOInputEvent(GPIO_PIN_27,GPIO_TRIGGER_LOW,INFINITE,@nightmodetoggle,nil);
end;

procedure addlogfilemarker(Data : Pointer; pin, trigger : longword);
begin
  // add a marker to the log file, so it shows in megalogviewer.
  currentbuttonticks := gettickcount;
  if ((currentbuttonticks - lastbuttonticks) > 500) then
  begin
     // we have detected a button press that is at least 500ms after the last one (debounce)
     lastbuttonticks := currentbuttonticks;
     if (Assigned(SpeeduinoMsg)) then
        SpeeduinoMsg.RequestMarker;
     MarkerRequested := True;
     markerrequesttime := currentbuttonticks;
  end;

  GPIOInputEvent(GPIO_PIN_23,GPIO_TRIGGER_LOW,INFINITE,@addlogfilemarker,nil);
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


  //add log file marker button (second from top button)
  GPIOPullSelect(GPIO_PIN_23,GPIO_PULL_UP);
  GPIOFunctionSelect(GPIO_PIN_23,GPIO_FUNCTION_IN);
  GPIOInputEvent(GPIO_PIN_23,GPIO_TRIGGER_LOW,INFINITE,@addlogfilemarker,nil);

  // night mode toggle button (top button)
  GPIOPullSelect(GPIO_PIN_27,GPIO_PULL_UP);
  GPIOFunctionSelect(GPIO_PIN_27,GPIO_FUNCTION_IN);
  GPIOInputEvent(GPIO_PIN_27,GPIO_TRIGGER_LOW,INFINITE,@nightmodetoggle,nil);

  //gpio pin 17 (bottom button) is the start stop logging button.
  GPIOPullSelect(GPIO_PIN_17,GPIO_PULL_UP);
  GPIOFunctionSelect(GPIO_PIN_17,GPIO_FUNCTION_IN);
  GPIOInputEvent(GPIO_PIN_17,GPIO_TRIGGER_LOW,INFINITE,@startstoplogging,nil);

  //pin 22 is a reboot button (second button from bottom)
  GPIOPullSelect(GPIO_PIN_22,GPIO_PULL_UP);
  GPIOFunctionSelect(GPIO_PIN_22,GPIO_FUNCTION_IN);
  GPIOInputEvent(GPIO_PIN_22,GPIO_TRIGGER_LOW,INFINITE,@RebootButton,nil);

end;

procedure AttachGauges;
begin
  gaugelist[1].AttachValue(@SpeeduinoMsg.rtStatus.o2);
  gaugelist[2].AttachValue(@fps);
  gaugelist[3].AttachValue(@SpeeduinoMsg.rtStatus.rpmlo);
  gaugelist[4].AttachValue(@SpeeduinoMsg.rtStatus.tps);
end;

procedure updatelcd;
begin
  // we are using the wmframebuffer mode as this is now perfected. The other mode is left in for backup

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

  // open the ini file and check the configuration
  // this will be expanded in due course to add gauge positions, pages etc.

  iniFile := TIniFile.Create('c:\speedylog.ini');
  webenabled := iniFile.ReadString('webserver', 'enabled', '0');
  if (webenabled = '1') then
    initwebserver;

  //ConsoleWindowGetCursorXY(WindowHandle, xpos, ypos);
  fps := 0;
  tempafr := 0;

  // we loop forever; this loop steps forward when connection is lost, at
  // which point we create a new message handler object

  while true do
  begin
    currentbuttonticks := 0;
    lastbuttonticks := 0;
    loopcount := 0;
    MarkerRequested := False;
    markerrequesttime := 0;

    //create the handler thread

    SpeeduinoMsg := TSpeeduinoMessageHandler.Create;

    AttachGauges;

    // no consonle output for this implementation.
    SpeeduinoMsg.screenwriter := nil;

    //kick off the thread.
    SpeeduinoMsg.Active := True;
    SpeeduinoMsg.Start;

    //begin requesting data.
    //the thread will re-request data automatically until terminated or paused.

    SpeeduinoMsg.Request;
    cmd.speedymessage := SpeeduinoMsg;

    // initialize the o2 reading to a default value so that we don't get 0.0 on the screen
    SpeeduinoMsg.rtStatus.o2:=133;

    startoffpstime := gettickcount;

    // the message handler thread terminates when logging is stopped either via
    // lost connection or via the start/stop button.

    while (not SpeeduinoMsg.Terminated) do
    begin
      // this writes the guages into the video memory on the GPU
      // if conneced via hdmi they will display at this point without any further processing,
      // with the exception that they will be rotated (see comments in the function)
      drawgauges;

      // in case the connection is dropped, we have a 2s timeout.
      // this needs testing for when tunerstudio is connected via usb, as that causes a reset on the arduino.

      if (SpeeduinoMsg.timeoflastmessage < (gettickcount - 2000)) and (not SpeeduinoMsg.IsPaused) then
      begin
        SpeeduinoMsg.EndLogging;
        LED.Rate := 100;
      end;

      // fps calcs are development only, really.
      loopcount := loopcount + 1;
      if (loopcount = 5) then
      begin
        fps := round(5 / ((gettickcount - startoffpstime) / 1000));
        startoffpstime := gettickcount;
        loopcount := 0;
      end;

      // this copies the graphics from the GPU to the framebuffer.
      updatelcd;
    end;

    SpeeduinoMsg.Free;

    sleep(100);
  end;
end.

