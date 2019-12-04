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
  TCastleJoysticks = class
  strict private
    FDefaultJoystickRecord: TJoystickRecord;

    function GetJoystickRecord(const Joy: TJoystick): TJoystickRecord; inline;
    procedure SayJoystickEvent(const Joy: TJoystick; const Prefix: String; const JE: TJoystickEvent; const Value: Single = 0);
    function DefaultJoystickRecord: TJoystickRecord;

    procedure DoAxisMove(const Joy: TJoystick; const Axis: Byte; const Value: Single);
    procedure DoButtonDown(const Joy: TJoystick; const Button: Byte);
    procedure DoButtonUp(const Joy: TJoystick; const Button: Byte);
    procedure DoButtonPress(const Joy: TJoystick; const Button: Byte);
  public
    procedure Initialize;
  end;

function JoysticksNew: TCastleJoysticks;

implementation
uses
  {$ifdef Linux}CastleInternalJoystickDatabaseLinux,{$endif}
  {$ifdef Windows}CastleInternalJoystickDatabaseWindows{$endif}
  CastleLog;

{ TJoystickManager ---------------------------------------------------------}

function TCastleJoysticks.GetJoystickRecord(const Joy: TJoystick): TJoystickRecord; inline;
begin
  //if not Database.TryGetValue(Joy, Result) then
  Result := DefaultJoystickRecord;
end;

procedure TCastleJoysticks.SayJoystickEvent(const Joy: TJoystick; const Prefix: String; const JE: TJoystickEvent; const Value: Single = 0);
begin
  if JE in AxisEvents then
    WriteLnLog(Joy.Info.Name, Prefix + ':' + FloatToStr(Value))
  else
    WriteLnLog(Joy.Info.Name, Prefix);
end;

procedure TCastleJoysticks.DoAxisMove(const Joy: TJoystick; const Axis: Byte; const Value: Single);
var
  R: TJoystickRecord;
  JE: TJoystickEvent;
begin
  R := GetJoystickRecord(Joy);
  JE := R.AxisEvent(Axis, Value);
  SayJoystickEvent(Joy, R.JoystickEventToStr(JE), JE, Value);
end;
procedure TCastleJoysticks.DoButtonDown(const Joy: TJoystick; const Button: Byte);
var
  R: TJoystickRecord;
  JE: TJoystickEvent;
begin
  R := GetJoystickRecord(Joy);
  JE := R.ButtonEvent(Button);
  SayJoystickEvent(Joy, R.JoystickEventToStr(JE), JE);
end;
procedure TCastleJoysticks.DoButtonUp(const Joy: TJoystick; const Button: Byte);
var
  R: TJoystickRecord;
  JE: TJoystickEvent;
begin
  R := GetJoystickRecord(Joy);
  JE := R.ButtonEvent(Button);
  SayJoystickEvent(Joy, R.JoystickEventToStr(JE), JE);
end;
procedure TCastleJoysticks.DoButtonPress(const Joy: TJoystick; const Button: Byte);
var
  R: TJoystickRecord;
  JE: TJoystickEvent;
begin
  R := GetJoystickRecord(Joy);
  JE := R.ButtonEvent(Button);
  SayJoystickEvent(Joy, R.JoystickEventToStr(JE), JE);
end;

function TCastleJoysticks.DefaultJoystickRecord: TJoystickRecord;
begin
  if FDefaultJoystickRecord = nil then
  begin
    FDefaultJoystickRecord := JoystickDatabase['Microntek USB Joystick'];
    WriteLnLog(FDefaultJoystickRecord.LogJoystickFeatures);
  end;
  Result := FDefaultJoystickRecord;
end;

procedure TCastleJoysticks.Initialize;
begin
  Joysticks.Initialize;
  Joysticks.OnAxisMove := @DoAxisMove;
  Joysticks.OnButtonDown := @DoButtonDown;
  Joysticks.OnButtonUp := @DoButtonUp;
  Joysticks.OnButtonPress := @DoButtonPress;
end;

{------------------------------------------------------------------------}

var
  FJoysticks: TCastleJoysticks;

function JoysticksNew: TCastleJoysticks;
begin
  if FJoysticks = nil then
    FJoysticks := TCastleJoysticks.Create;
  Result := FJoysticks;
end;

finalization
  FreeAndNil(FJoysticks);
end.

