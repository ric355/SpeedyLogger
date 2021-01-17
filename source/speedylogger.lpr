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
  inifiles,
  devices,
  logoutput;

var
  SpeeduinoMsg : TSpeeduinoMessageHandler;
  IPAddress : String;
  HTTPListener : THTTPListener;
  HTTPFolder : THTTPFolder;
  Winsock2TCPClient : TWinsock2TCPClient;
  led : TLEDThread;
  iniFile : TIniFile;
  webenabled : string;
  ypos : longword;
  currentbuttonticks : qword = 0;
  lastbuttonticks : qword = 0;
  commsretries : qword = 0;
  shellpath : string;
  InfoWindowHandle : THandle;


procedure logfiletidy;
var
  f : longint;
  SearchRec : TRawbyteSearchRec;
  FileList : TStringList;
  MaxLogFiles : integer;
  MinFreeSpace : QWord;
begin
  {
  Look at the log files and delete any until the amount of free disk space we have is
  more than the configured Mb. Delete oldest first using the filename sorted as the
  ids are numeric.

  There are now two options:
     - keepfreemb - defines a number of mb to keep free - files deleted until
                                                   this amount is available.
     - maxlogfiles - defines a count of the number of files allowed at once.
                   files will be deleted until maxlogfiles-1 files are left, ready
                   for the next one to be created when logging starts.

    if both are defined, keepfreemb takes priority and maxlogfiles is ignored.
  }

  Log('Tidying up log files');

  MinFreeSpace := iniFile.ReadInteger('general', 'keepfreemb', 0);
  if (MinFreeSpace = 0) then
  begin
    log('keepfreemb not defined; testing for maxlogfiles');
    MaxLogFiles := iniFile.ReadInteger('general', 'maxlogfiles', 0);
    if (MaxLogFiles = 0) then
    begin
      log('maxlogfiles is not configured - not doing any file deletion.');
      exit;
    end;
    log('Max log files = ' +inttostr(MaxLogFiles));
  end
  else
  begin
    log('keepfreemb = ' + inttostr(MinFreeSpace) + 'Mb');
    MinFreeSpace := MinFreeSpace * 1024 * 1024;   // convert to bytes
  end;

  FileList := TStringList.Create;
  FileList.Sorted:=true;

  // find all files in the datalogs directory. Does *not* include subdirectories.
  f := FindFirst('c:\datalogs\*.msl', 0, SearchRec);
  while (f <> -1) do
  begin
    FileList.Add(SearchRec.Name);
    f := FindNext(SearchRec);
  end;

  log('There are ' + inttostr(FileList.Count) + ' log files in the datalogs folder.');
  if (MinFreeSpace > 0) then
  begin
    // if minfreespace is defined, we delete files until we have at least minfreespace mb free
    log('Current free space on disk = '+ inttostr(SysUtils.DiskFree(0) div 1024 div 1024) + 'Mb');

    while (SysUtils.DiskFree(0) < MinFreeSpace) and (filelist.count > 0) do
    begin
      log('Deleting file ' + filelist[0]);
      SysUtils.DeleteFile('c:\datalogs\'+FileList[0]);
      FileList.Delete(0);
      log('Free space is now ' + inttostr(sysutils.diskfree(0)));
    end;

    if (SysUtils.DiskFree(0) < MinFreeSpace) then
      log('Warning: Could not free up enough space on this drive!');
  end
  else
  begin
    // oterwise if maxlogfiles is defined, we delete until we are back under the limit.
    while (FileList.Count > MaxLogFiles) do
    begin
      log('Deleting file ' + FileList[0]);
      SysUtils.DeleteFile('c:\datalogs\'+FileList[0]);
      FileList.Delete(0);
    end;
  end;

  log('Log file tidying completed. Free space='+inttostr(SysUtils.DiskFree(0) div 1024 div 1024) + 'Mb');
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
  with SpeeduinoMsg.RTStatus do
  begin
    ConsoleWindowWriteEx(InfoWindowHandle, 'secl          : ' + inttostr(secl) + '    ',
       1, ypos, COLOR_BLACK, COLOR_WHITE);
    ConsoleWindowWriteEx(InfoWindowHandle, 'rpm           : ' + inttostr(rpmhi * 256 + rpmlo) + '    ',
       1, ypos + 1, COLOR_BLACK, COLOR_WHITE);
    ConsoleWindowWriteEx(InfoWindowHandle, 'TPS           : ' + inttostr(tps) + '    ',
       1, ypos + 2, COLOR_BLACK, COLOR_WHITE);
    ConsoleWindowWriteEx(InfoWindowHandle, 'MAP           : ' + inttostr(maphi * 256 + maplo) + '    ',
       1, ypos + 3, COLOR_BLACK, COLOR_WHITE);
    ConsoleWindowWriteEx(InfoWindowHandle, 'VE            : ' + inttostr(ve) + '    ',
       1, ypos + 4, COLOR_BLACK, COLOR_WHITE);
    ConsoleWindowWriteEx(InfoWindowHandle, 'Advance       : ' + inttostr(advance) + '    ',
       1, ypos + 5, COLOR_BLACK, COLOR_WHITE);
    ConsoleWindowWriteEx(InfoWindowHandle, 'Comms Retries : ' + inttostr(commsretries) + '    ',
       1, ypos + 5, COLOR_BLACK, COLOR_WHITE);
  end;
  LED.Rate := 750;

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
  end;

  GPIOInputEvent(GPIO_PIN_23,GPIO_TRIGGER_LOW,INFINITE,@addlogfilemarker,nil);
end;


begin
  LED := TLEDThread.Create;
  LED.FreeonTerminate := true;

  ConsoleActivated := true;

  WindowHandle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_BOTTOM,True);

  InfoWindowHandle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_TOP,True);


  //add log file marker button (second from top button)
  GPIOPullSelect(GPIO_PIN_23,GPIO_PULL_UP);
  GPIOFunctionSelect(GPIO_PIN_23,GPIO_FUNCTION_IN);
  GPIOInputEvent(GPIO_PIN_23,GPIO_TRIGGER_LOW,INFINITE,@addlogfilemarker,nil);

  // wait until the file system is ready
  while not DirectoryExists('C:\') do
   begin
    Sleep(10);
   end;
  Log('File system is ready');

  if (not DirectoryExists('c:\datalogs')) then
    CreateDirectory('c:\datalogs', nil);

  iniFile := TIniFile.Create('c:\speedylog.ini');
  webenabled := iniFile.ReadString('webserver', 'enabled', '0');
  if (webenabled = '1') then
    initwebserver;

  shellpath := inifile.ReadString('general', 'shellupdatelocalpath', '');
  if (shellpath <> '') then
    SHELL_UPDATE_LOCAL_PATH := shellpath;

  LogFileTidy;

  inifile.Free;

  ypos := 1;

  while true do
  begin

    //create the handler thread

    log('Create message handler');
    LED.Rate := 750;
    SpeeduinoMsg := TComPortReadThread.Create;

    //uncomment this line if you want to see data on the console. You
    //can change what is printed (per message) in the function itself. See the
    //TRealTimeStatus structure
    SpeeduinoMsg.DataIsReady := @DebugUpdateMessage;
    SpeeduinoMsg.Active := True;
    SpeeduinoMsg.Start;

    //begin requesting data.
    //the thread will re-request data automatically until terminated or paused.

    SpeeduinoMsg.StartLogging;
    cmd.speedymessage := SpeeduinoMsg;

    while (not SpeeduinoMsg.Terminated) do
    begin
      sleep(100);

      // in case the connection is dropped, we have a 2s timeout.

      if (SpeeduinoMsg.timeoflastmessage < (gettickcount - 2000)) and (not SpeeduinoMsg.IsPaused) then
      begin
        SpeeduinoMsg.EndLogging;
        SpeeduinoMsg.Terminate;
        commsretries := commsretries + 1;
        LED.Rate := 100;
      end;

    end;

    try
      SpeeduinoMsg.Free;
    except
      on e : exception do
        log('Exception on message free : ' + e.message);
    end;

    sleep(100);
  end;
end.

