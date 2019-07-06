unit gauges;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VGShapes;

type
  TGaugeDataType = (dtByte,
                 dtSwappedWord,
                 dtWord,
                 dtString);

  TGauge = class
  private
    scalex, scaley : integer;            // scale of screen to design coordinates
    FDataP : pointer;
    FDataType : TGaugeDataType;
  protected
    fx, fy : integer;             // location, bottom left corner
    fw, fh : integer;              // width and height
    fvalue : integer;
    fmin : integer;
    fmax : integer;
    function ValueString : String;
    function ValueInt : Integer;
  public
    constructor Create(x, y, w, h, min, max, value : integer; dataType : TGaugeDataType);
    procedure draw; virtual; //(latestvalue : integer);
    procedure AttachValue(dataP : Pointer);
  end;

  TVerticalGauge = class(TGauge)
  public
    procedure draw; override;
  end;

  THorizontalGauge = class(TGauge)
    procedure draw; override; //(latestvalue : integer);
  end;

  TCircularGauge = class(TGauge)
    procedure draw; override;
  end;

  TValueGauge = class(TGauge)
    procedure draw; override;
  end;

implementation

constructor TGauge.Create(x, y, w, h, min, max, value : integer; dataType : TGaugeDataType);
begin
  FDataP := nil;
  FDataType := dataType;
  fx := x;
  fy := y;
  fw := w;
  fh := h;
  fmin := min;
  fmax := max;
  fvalue := value;
end;

procedure TGauge.AttachValue(dataP : pointer);
begin
  FDataP := dataP;
end;

procedure TGauge.Draw;
begin
end;

function TGauge.ValueString : String;
var
  a, b : Byte;
  w : Word;
begin
  if (FDataP = nil) then
  begin
    Result := '';
    exit;
  end;

  case FDataType of
    dtByte:
       begin
         b := PByte(FDataP)^;
         Result := IntToStr(b);
       end;

   dtWord:
      begin
        w := PByte(FDataP+1)^ * 256 + PByte(FDataP)^;
        Result := IntToStr(w);
      end;

   dtSwappedWord:
        Result := '100';

   dtString :
      Result := PString(FDataP)^;
  end;
end;

function TGauge.ValueInt : Integer;
var
  a, b : Byte;
  w : Word;
begin
  if (FDataP = nil) then
  begin
    Result := 0;
    exit;
  end;

  case FDataType of
    dtByte:
    begin
      b := PByte(FDataP)^;
      Result := b;
    end;

   dtWord:
      begin
        w := PByte(FDataP+1)^ * 256 + PByte(FDataP)^;
        Result := w;
      end;

   dtSwappedWord :
      Result := 100;

   dtString :
      Result := 0;
  end;
end;

procedure TVerticalGauge.Draw;
var
  height : integer;
  latestvalue : longint;
begin
  VGShapesFill(128, 0, 0, 1);

  latestvalue := ValueInt;

  height := trunc(latestvalue / fmax * fh);
  VGShapesRect(fx, fy, fw, height);
  VGShapesTextEnd(fx+fw, height + 50, ValueString, VGShapesSansTypeface,30);
end;

procedure THorizontalGauge.Draw;
var
  width : integer;
  latestvalue : integer;
begin
  VGShapesFill(128, 0, 0, 1);
  latestvalue := ValueInt;

  width := trunc(latestvalue / fmax * fw);
  VGShapesRect(fx, fy, width, fh);
  VGShapesText(fx+width, fy, inttostr(latestvalue), VGShapesSansTypeface,30);
end;


procedure TCircularGauge.draw;
var
  degrees : integer;
  latestvalue : integer;
begin
  VGShapesFill(128, 0, 0, 1);

  VGShapesCircle(fx, fy, fw);

  latestvalue := ValueInt;

  // convert the value into a number of degrees

  degrees := trunc(latestvalue / fmax * 360);

end;

procedure TValueGauge.draw;
begin
  VGShapesFill(128, 0, 0, 1);
  VGShapesText(fx, fy, ValueString, VGShapesSansTypeface,30);
end;

end.

