program SpeedyLogger;

{$mode objfpc}{$H+}

uses
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
  inifiles
  {$ifdef graphics}
  ,
  gauges,
  VGShapes,
  OpenVG,
  VC4
  {$endif}
  ;

const
   NUM_GAUGES = 15;

var
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
  {$ifdef graphics}
  Width : Integer;
  Height : Integer;
  ShapeColor : TVGShapesColor;
  runsecs : longint;
  fps : real;
  startoffpstime : longint;
  loopcount : integer;
  gaugelist : array[1..NUM_GAUGES] of TGauge;
  {$endif}



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

procedure DebugUpdateMessage;

begin
  // this function is called from a different thread, so may result in some visual
  // corruption if you also print things out from the main loop, but it is only
  // for debug purposes so it doesn't matter.
  {$ifndef graphics}
  with SpeeduinoMsg.RTStatus do
  begin
    ConsoleWindowWriteEx(WindowHandle, 'secl    : ' + inttostr(secl) + '    ',
       1, ypos, COLOR_BLACK, COLOR_WHITE);
    ConsoleWindowWriteEx(WindowHandle, 'rpm     : ' + inttostr(rpmhi * 256 + rpmlo) + '    ',
       1, ypos + 1, COLOR_BLACK, COLOR_WHITE);
    ConsoleWindowWriteEx(WindowHandle, 'TPS     : ' + inttostr(tps) + '    ',
       1, ypos + 2, COLOR_BLACK, COLOR_WHITE);
    ConsoleWindowWriteEx(WindowHandle, 'MAP     : ' + inttostr(maphi * 256 + maplo) + '    ',
       1, ypos + 3, COLOR_BLACK, COLOR_WHITE);
    ConsoleWindowWriteEx(WindowHandle, 'VE      : ' + inttostr(ve) + '    ',
       1, ypos + 4, COLOR_BLACK, COLOR_WHITE);
    ConsoleWindowWriteEx(WindowHandle, 'Advance : ' + inttostr(advance) + '    ',
       1, ypos + 5, COLOR_BLACK, COLOR_WHITE);
  end;
  {$endif}
  LED.Rate := 250;

end;

procedure DrawGauges;
var
  val : integer;
  i : integer;

begin
  {$ifdef graphics}
  VGShapesStart(Width, Height);

//  VGShapesBackground(0,0,0);

  VGShapesFill(128, 0, 0, 1);

  // paint all of the objects
  for i := 1 to NUM_GAUGES do
  begin
    gaugelist[i].draw;
  end;

  if (fps > 0) then
  begin
    val := trunc(fps);
    VGShapesTextEnd(Width-100,Height-70, 'fps: ' + inttostr(val),VGShapesSansTypeface,30);
  end;

  runsecs := trunc(gettickcount / 1000);
  VGShapesTextEnd(Width-300, Height-70, 'secs: ' + inttostr(runsecs),VGShapesSansTypeface,30);



  VGShapesEnd;
  {$endif}
end;


procedure initgraphics;

begin
  {$ifdef graphics}
  {Initialize OpenVG and the VGShapes unit}
  VGShapesInit(Width,Height);

  {Convert the RGB color to use for our shapes into a TVGShapesColor record}
  VGShapesRGB(202,225,255,Shapecolor);

  gaugelist[1] := TVerticalGauge.Create(100, 50, 50, 300, 0, 250, 100, dtWord);
  gaugelist[2] := TVerticalGauge.Create(200, 50, 50, 300, 0, 100, 100, dtByte);
  gaugelist[3] := TVerticalGauge.Create(300, 50, 50, 300, 0, 50, 100, dtByte);
  gaugelist[4] := THorizontalGauge.Create(400, 50, 150, 50, 0, 1000, 0, dtWord); //loopsLo, dtSwappedWord);
  gaugelist[5] := TValueGauge.Create(Width - 600, Height - 70, 150, 50, 0, 1000, 0, dtByte);

    gaugelist[6] := TVerticalGauge.Create(100, 400, 50, 300, 0, 250, 100, dtWord);
    gaugelist[7] := TVerticalGauge.Create(200, 400, 50, 300, 0, 250, 100, dtWord);
    gaugelist[8] := TVerticalGauge.Create(300, 400, 50, 300, 0, 250, 100, dtWord);
    gaugelist[9] := TVerticalGauge.Create(400, 400, 50, 300, 0, 250, 100, dtWord);
    gaugelist[10] := TVerticalGauge.Create(500, 400, 50, 300, 0, 250, 100, dtWord);
    gaugelist[11] := TVerticalGauge.Create(600, 400, 50, 300, 0, 250, 100, dtWord);
    gaugelist[12] := TVerticalGauge.Create(700, 400, 50, 300, 0, 250, 100, dtWord);
    gaugelist[13] := TVerticalGauge.Create(800, 400, 50, 300, 0, 250, 100, dtWord);
    gaugelist[14] := TVerticalGauge.Create(900, 400, 50, 300, 0, 250, 100, dtWord);
    gaugelist[15] := TCircularGauge.Create(1000, 400, 120, 0, 250, 100, dtWord);

  {$endif}
end;

procedure AttachGauges;
begin
  {$ifdef graphics}
  gaugelist[1].AttachValue(@SpeeduinoMsg.rtStatus.maplo);
  gaugelist[2].AttachValue(@SpeeduinoMsg.rtStatus.ve);
  gaugelist[3].AttachValue(@SpeeduinoMsg.rtStatus.advance);
  gaugelist[4].AttachValue(@SpeeduinoMsg.rtStatus.loopslo);
  gaugelist[5].AttachValue(@SpeeduinoMsg.rtStatus.secl);

  gaugelist[6].AttachValue(@SpeeduinoMsg.rtStatus.maplo);
  gaugelist[7].AttachValue(@SpeeduinoMsg.rtStatus.maplo);
  gaugelist[8].AttachValue(@SpeeduinoMsg.rtStatus.maplo);
  gaugelist[9].AttachValue(@SpeeduinoMsg.rtStatus.maplo);
  gaugelist[10].AttachValue(@SpeeduinoMsg.rtStatus.maplo);
  gaugelist[11].AttachValue(@SpeeduinoMsg.rtStatus.maplo);
  gaugelist[12].AttachValue(@SpeeduinoMsg.rtStatus.maplo);
  gaugelist[13].AttachValue(@SpeeduinoMsg.rtStatus.maplo);
  gaugelist[14].AttachValue(@SpeeduinoMsg.rtStatus.maplo);
  gaugelist[15].AttachValue(@SpeeduinoMsg.rtStatus.maplo);

  {$endif}
end;

begin
  LED := TLEDThread.Create;
  LED.FreeonTerminate := true;

  WindowHandle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_FULL,True);

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

  while true do
  begin
    //create the handler thread

    SpeeduinoMsg := TSpeeduinoMessageHandler.Create;

    AttachGauges;

    //uncomment this line if you want to see data on the console. You
    //can change what is printed (per message) in the function itself. See the
    //TRealTimeStatus structure
    SpeeduinoMsg.screenwriter := @DebugUpdateMessage;
    SpeeduinoMsg.Active := True;
    SpeeduinoMsg.Start;

    //begin requesting data.
    //the thread will re-request data automatically until terminated or paused.

    SpeeduinoMsg.Request;
    cmd.speedymessage := SpeeduinoMsg;

    {$ifdef graphics}
    startoffpstime := gettickcount;
    fps := 0;
    {$endif}


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

      {$ifdef graphics}
      loopcount := loopcount + 1;
      if (loopcount = 40) then
      begin
        fps := 40 / ((gettickcount - startoffpstime) / 1000);
        startoffpstime := gettickcount;
        loopcount := 0;
      end;

      {$endif}

    end;

    SpeeduinoMsg.Free;

    sleep(100);
  end;
end.

