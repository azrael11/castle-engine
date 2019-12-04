unit CastleJoystickManager;

{TODO:

 * [CRITICAL] Note that in Windows we get a wrong reported joystick name as Microsoft PC-joystick driver
   while the joystick name reported by SDL2 is G-Shark GS-GP702 and the true name is Esperanza EG102
 * TJoyState has max 8 axes [0..7], however, database has as much as 14 axes \
 * Windows and Linux backends handle axes in an ambiguous way,
   AxesMap tries to mimic something that this database should do,
   but eventually it does so only for one type of axes layout.
   Overall, currently this makes this database pretty useless.
   However, I'm not sure if our current backends return valid axes indexes,
   as we're using legacy backends both on Linux and Windows and
   axes layouts theoretically can be different from those provided by
   newer backends.
 * Data[0] is GUID and we can't use it at the moment, however, maybe we can get it from backend?
   In SDL it is used to detect the joystick
 * I have no idea what "~" means in the database for axes, simply removing the symbol
 * [TODO] Defatul X-Box like gamepad layout.
   Note, that currently we have Right stick flipped X and Y axes for X-Box gamepad,
   possibly due to AxesMap messing with original axes order,
   or original axes order reported wrong.
   (it is also possible, thou very unlikely, that it's fault of the
   database, as it does contain errors; however, not a single gamepad
   reports Y axis before X axis - Y axis always follows X axis, never preceeds,
   like we have now on X-Box layout)
 * What is the difference between ButtonPress and ButtonDown events?
   Looks like there is none on Linux, however, on Windows event ButtonDown
   fires constantly while the button is hold pressed, but ButtonPress
   only once, when the button is pressed.
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

