unit CastleJoystickManager;

{TODO:

 * TJoyState has max 8 axes [0..7], however, database has as much as 14 axes \
 * I have no idea what "~" means in the database for axes, simply removing the symbol
 * Data[0] is GUID and we can't use it at the moment, however, maybe we can get it from backend?
   it seems like this is a joystick driver-specific value
 * [CRITICAL] Note that in Windows we get a wrong reported joystick name as Microsoft PC-joystick driver
   while the joystick name reported by SDL2 is G-Shark GS-GP702 and the true name is Esperanza EG102
 * Defatul X-Box like gamepad layout
 * What is the difference between ButtonPress and ButtonDown events?
 * Button press/release based on Value for axis-driven buttons

}

interface

uses
  SysUtils, Classes,
  CastleJoysticks, CastleInternalJoystickRecord;

type
  TCastleJoystickManager = class
  strict private
    FDefaultJoystickRecord: TJoystickRecord;
    function GetJoystickRecord(const Joy: TJoystick): TJoystickRecord; inline;
    procedure SayJoystickEvent(const Joy: TJoystick; const Prefix: String; const JE: TJoystickEvent; const Value: Single = 0);
    function DefaultJoystickRecord: TJoystickRecord;
  public
    procedure DoAxisMove(const Joy: TJoystick; const Axis: Byte; const Value: Single);
    procedure DoButtonDown(const Joy: TJoystick; const Button: Byte);
    procedure DoButtonUp(const Joy: TJoystick; const Button: Byte);
    procedure DoButtonPress(const Joy: TJoystick; const Button: Byte);
    destructor Destroy; override;
  end;

function JoystickManager: TCastleJoystickManager;
implementation
uses
  Generics.Collections,
  CastleLog;

{ TJoystickManager ---------------------------------------------------------}

function TCastleJoystickManager.GetJoystickRecord(const Joy: TJoystick): TJoystickRecord; inline;
begin
  //if not Database.TryGetValue(Joy, Result) then
  Result := DefaultJoystickRecord;
end;

procedure TCastleJoystickManager.SayJoystickEvent(const Joy: TJoystick; const Prefix: String; const JE: TJoystickEvent; const Value: Single = 0);
begin
  if JE in AxisEvents then
    WriteLnLog(Joy.Info.Name, Prefix + ':' + FloatToStr(Value))
  else
    WriteLnLog(Joy.Info.Name, Prefix);
end;

procedure TCastleJoystickManager.DoAxisMove(const Joy: TJoystick; const Axis: Byte; const Value: Single);
var
  R: TJoystickRecord;
  JE: TJoystickEvent;
begin
  R := GetJoystickRecord(Joy);
  JE := R.AxisEvent(Axis, Value);
  SayJoystickEvent(Joy, R.JoystickEventToStr(JE), JE, Value);
end;
procedure TCastleJoystickManager.DoButtonDown(const Joy: TJoystick; const Button: Byte);
var
  R: TJoystickRecord;
  JE: TJoystickEvent;
begin
  R := GetJoystickRecord(Joy);
  JE := R.ButtonEvent(Button);
  SayJoystickEvent(Joy, R.JoystickEventToStr(JE), JE);
end;
procedure TCastleJoystickManager.DoButtonUp(const Joy: TJoystick; const Button: Byte);
var
  R: TJoystickRecord;
  JE: TJoystickEvent;
begin
  R := GetJoystickRecord(Joy);
  JE := R.ButtonEvent(Button);
  SayJoystickEvent(Joy, R.JoystickEventToStr(JE), JE);
end;
procedure TCastleJoystickManager.DoButtonPress(const Joy: TJoystick; const Button: Byte);
var
  R: TJoystickRecord;
  JE: TJoystickEvent;
begin
  R := GetJoystickRecord(Joy);
  JE := R.ButtonEvent(Button);
  SayJoystickEvent(Joy, R.JoystickEventToStr(JE), JE);
end;

function TCastleJoystickManager.DefaultJoystickRecord: TJoystickRecord;
begin
  if FDefaultJoystickRecord = nil then
  begin
    FDefaultJoystickRecord := TJoystickRecord.Create;
    //FDefaultJoystickRecord.Parse('03000000790000000600000010010000,Microntek USB Joystick,a:b2,b:b1,x:b3,y:b0,back:b8,start:b9,leftshoulder:b4,rightshoulder:b5,dpup:h0.1,dpdown:h0.4,dpleft:h0.8,dpright:h0.2,leftx:a0,lefty:a1,rightx:a2,righty:a3,lefttrigger:b6,righttrigger:b7,rightstick:b11,leftstick:b10,platform:Linux,');
  end;
  Result := FDefaultJoystickRecord;
end;

destructor TCastleJoystickManager.Destroy;
begin
  FreeAndNil(FDefaultJoystickRecord);
  inherited;
end;

{------------------------------------------------------------------------}

var
  FJoystickManager: TCastleJoystickManager;

function JoystickManager: TCastleJoystickManager;
begin
  if FJoystickManager = nil then
    FJoystickManager := TCastleJoystickManager.Create;
  Result := FJoystickManager;
end;

finalization
  FreeAndNil(FJoystickManager);
end.

