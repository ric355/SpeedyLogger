unit initunit;

{$mode objfpc}{$H+}

interface

uses
  GlobalConfig;

implementation

initialization
  FAT_INFO_SECTOR_ENABLE := FALSE;

end.

