unit gauges;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VGShapes, openvg;

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
    Fr : Byte;
    Fg : Byte;
    Fb : Byte;
    function ValueString : String; virtual;
    function ValueInt : Integer;
  public
    constructor Create(x, y, w, h, min, max, value : integer; dataType : TGaugeDataType);
    procedure draw; virtual; //(latestvalue : integer);
    procedure AttachValue(dataP : Pointer);
    procedure SetColour(r, g, b : Byte);
    property r : byte read Fr write Fr;
    property g : byte read Fg write Fg;
    property b : byte read Fb write Fb;
  end;

  TVerticalGauge = class(TGauge)
  public
    procedure draw; override;
  end;

  THorizontalGauge = class(TGauge)
    procedure draw; override; //(latestvalue : integer);
  end;

  TCircularGauge = class(TGauge)
    private
      fradius : longint;
    public
    constructor Create(x, y, rad, min, max, value : integer; dataType : TGaugeDataType);
    procedure draw; override;
  end;

  TValueGauge = class(TGauge)
  private
    FFactor : Integer;
    FFontHeight : Integer;
    FRightJ : Boolean;
    function ValueString : String; override;
  public
    property fontsize : integer read FFontHeight write ffontheight;
    property RightJ : boolean read FRightJ write FRightJ;
    constructor Create(x, y, w, h, min, max, value : integer; dataType : TGaugeDataType; factor : integer; fontheight : integer);
    procedure draw; override;
    procedure SetFactor(factor : integer);
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
  byteval : Byte;
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
         byteval := PByte(FDataP)^;
         Result := IntToStr(byteval);
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
  byteval : Byte;
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
      byteval := PByte(FDataP)^;
      Result := byteval;
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

procedure TGauge.SetColour(r, g, b : Byte);
begin
  Fr := r;
  Fg := g;
  Fb := b;
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
  VGShapesTextEnd(fx+fw, height + 10 + fy, ValueString, VGShapesSansTypeface,30);
end;

procedure THorizontalGauge.Draw;
var
  width : integer;
  latestvalue : integer;
begin
  VGShapesFill(Fr, Fg, Fb, 1);
  VGShapesStroke(Fr, Fg, Fb, 1);
  latestvalue := ValueInt;

  width := trunc(latestvalue / fmax * fw);
  VGShapesRectOutline(fx, fy, fw, fh);
  VGShapesRect(fx, fy, width, fh);
  //VGShapesText(fx+width, fy, inttostr(latestvalue), VGShapesSansTypeface,30);
end;


constructor TCircularGauge.Create(x, y, rad, min, max, value : integer; dataType : TGaugeDataType);
begin
  inherited Create(x, y, r*2, r*2, min, max, value, datatype);

  fradius := r;
end;


procedure TCircularGauge.draw;
var
  degrees : VGFloat;
  latestvalue : integer;
  tx, ty : longint;
begin
  VGShapesFill(0, 0, 128, 1);

  VGShapesCircle(fx, fy, fradius*2+10);

  VGShapesFill(255, 0, 0, 1);

  latestvalue := ValueInt;


  degrees := 240 - (trunc(latestvalue / fmax * 300));

  tx := fx;
  ty := fy;

  // translate to the origin so we can rotate
  VGShapesTranslate(tx, ty);
  VGShapesRotate(degrees);

  VGShapesRect(0, -5, fradius, 10);
  VGShapesFill(0,0,0,1);
  VGShapesCircle(5,0,5);

  // can we do another translate rotate here?
  VGShapesTranslate(fradius + 10, -15);
  VGShapesRotate(270);
  VGShapesText(0, 0, ValueString, VGShapesSansTypeface, 30);
  VGShapesRotate(-270);
  VGShapesTranslate(-(fradius + 10), 15);

  //restore rotation and translate back to correct position
  VGShapesRotate(-degrees);
  VGShapesTranslate(-tx, -ty);

  //now for kicks we'll make the number rotate on the end of the pointer.
  {
  VGShapesTranslate(tx + fradius + 10, ty-15);
  VGRotate(degrees+90);
  VGShapesText(0, 0, ValueString, VGShapesSansTypeface,30);
  VGRotate(-degrees-90);
  VGShapesTranslate(-(tx + fradius + 10), -(ty-15));  }
end;

constructor TValueGauge.Create(x, y, w, h, min, max, value : integer; dataType : TGaugeDataType; factor : integer; fontheight : integer);
begin
  FFactor := factor;
  FRightJ := false;
  FFontHeight := fontheight;
  inherited Create(x, y, w, h, min, max, value, dataType);
end;

procedure TValueGauge.draw;
begin
  VGShapesFill(128, 0, 0, 1);
  if (FRightJ) then
    VGShapesTextEnd(fx, fy, ValueString, VGShapesSansTypeface,FFontHeight)
  else
    VGShapesText(fx, fy, ValueString, VGShapesSansTypeface,FFontHeight);
end;

procedure TValueGauge.SetFactor(factor : integer);
begin
  FFactor := factor;
end;

function TValueGauge.ValueString : string;
var
  rval : real;
begin
  if (Ffactor > 0) then
  begin
    rval := ValueInt / Ffactor;
    //Result := FloatToStr(rval);
    Result := FormatFloat('#0.0', rval);
  end
  else
  begin
    Result := inherited ValueString;
  end;
end;

end.
