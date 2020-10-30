unit bufferedwrites;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, threads, globaltypes;

type
  TBufferedWriter = class
  private
    FBufferSize : integer;
    FBufferP : PChar;
    FBufferPosP : PChar;
    FMutex : TMutexHandle;
    FBatchSize : integer;
    FBytesUsed : integer;
    FLogFile : file of byte;
    FOpen : Boolean;
    FRootFolder : string;
  public
    constructor Create(BufferSize : integer;
                       BatchSize : integer;
                       CreateSuspended: Boolean;
                       ARootFolder : string);
    destructor Destroy; override;
    procedure AddRow(arow : string);
    procedure StartNewFile(afilename : string);
    procedure CloseLogFile;
  end;


implementation

uses
  logoutput;

constructor TBufferedWriter.Create(BufferSize : integer;
                                BatchSize : integer;
                                CreateSuspended : Boolean;
                                ARootFolder : string);
begin
  FBufferSize := BufferSize;
  FBufferP := GetMem(FBufferSize);
  FBatchSize := BatchSize;
  FRootFolder := aRootFolder;

  // start off with an empty buffer.
  FBytesUsed := 0;
  FBufferPosP := FBufferP;

  FMutex := MutexCreate;

  FOpen := False;
end;

destructor TBufferedWriter.Destroy;
begin
  try
  FreeMem(FBufferP);

  MutexDestroy(FMutex);

  if (FOpen) then
    CloseFile(FLogFile);


  except
    log('exception 5');
//    threadhalt(0);
  end;
  inherited Destroy;
end;

procedure TBufferedWriter.AddRow(arow : string);
var
  l : integer;
begin
  // we grab mutex here as we don't want the thread execute to alter the
  // bytes used while we are looking at it.

  MutexLock(FMutex);
  try
    l := Length(ARow);

    if ((l + FBytesUsed) < FBufferSize) then
    begin
      move(arow[1], FBufferPosP^, l);
      FBufferPosP := FBufferPosP + l;
      FBytesUsed := FBytesUsed + l;

      // note we don't release the lock until we have finished checking shared
      // resources and ths if statement is part of that.
      if (FBytesUsed > FBatchSize) then
      begin
        // release the semphore to signal a write is required.

        if (FOpen) then
          BlockWrite(FLogFile, FBufferP^, FBytesUsed);
        FBytesUsed := 0;
        FBufferPosP := FBufferP;
      end;
    end
    else
    begin
      // here there isn't enough room so we need to write the buffer now.
      if (FOpen) then
        BlockWrite(FLogFile, FBufferP^, FBytesUsed);
      FBytesUsed := 0;
      FBufferPosP := FBufferP;

      // now we should be able to write our data to the buffer.
      move(arow[1], FBufferPosP, l);
      FBufferPosP := FBufferPosP + l;
      FBytesUsed := FBytesUsed + l;
    end;

  finally
    MutexUnlock(FMutex);
  end;
end;

procedure TBufferedWriter.StartNewFile(afilename : string);
begin
  MutexLock(FMutex);
  try
    if (FOpen) then
      CloseFile(FLogFile);

    Assignfile (FLogFile, FRootFolder + afilename);
    Rewrite(FLogFile);
    FOpen := True;

  finally
    MutexUnlock(FMutex);
  end;
end;

procedure TBufferedWriter.CloseLogFile;
begin
  MutexLock(FMutex);
  try

  if (FOpen) then
  begin
    CloseFile(FLogFile);
    FOpen := False;

    //reset buffer.
    FBytesUsed := 0;
    FBufferPosP := FBufferP;
  end;

  finally
    MutexUnLock(FMutex);
  end;
end;


end.

