program SpeedyLogger;

{$mode objfpc}{$H+}

uses
  {$ifdef RPI2}
  RaspberryPi2,
  {$endif}
  {$ifdef RPI3}
  RaspberryPi3,
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
  {$ifdef RPI2}
  BCM2836,
  {$endif}
  {$ifdef RPI3}
  BCM2837,
  {$endif}
  MMC,             {Include the MMC/SD unit for access to the SD card}
  {$ifdef RPI2}
  BCM2709,         {And the driver for the Raspberry Pi SD host}
  {$endif}
  {$ifdef RPI3}
  BCM2710,
  {$endif}
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
  inifiles;


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


procedure Log(str : string);
begin
  ConsoleWindowWriteLn(WindowHandle, DateTimeToStr(Now) + ': ' + str);
end;


procedure InitWebServer;
begin
  // the web server can be enabled by uncommenting out the call to this function
  // in the main program further down. Once enabled, you can download the data logs
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
var
  x,y : longword;
begin
  // this function is called from a different thread, so may result in some visual
  // corruption if you also print things out from the main loop, but it is only
  // for debug purposes so it doesn't matter.

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

  LED.Rate := 250;

end;

procedure DrawGauges;
begin
end;


procedure initgraphics;

begin
end;

procedure AttachGauges;
begin
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

    end;

    SpeeduinoMsg.Free;

    sleep(100);
  end;
end.

