{ See also https://github.com/spurious/SDL-mirror/blob/master/src/joystick/SDL_gamecontroller.c
  (same thing done in SDL) }

unit CastleJoystickManager;

{TODO:

 * We have a lot of duplicate joystick names
   Also duplicate GUIDs are found for "different OSes"
   TODO: rework storage and searching for joystick records.
   Note: duplicate records seem to have identical layouts, only different GUIDs
   This way current "overwriting" of the equally named joysticks isn't a critical
   problem, however, it most certainly has to be fixed in the future
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
 * [TODO] I have no idea what "~" means in the database for axes, simply removing the symbol
   UPD: https://github.com/spurious/SDL-mirror/blob/master/src/joystick/SDL_gamecontroller.c#L585
   It means "inverted input" on the axis
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
 * TODO: see https://github.com/spurious/SDL-mirror/blob/master/src/joystick/SDL_gamecontrollerdb.h
   they have a "default,Standard Gamepad" something we should use too.
 * Note (TODO): Some GUIDS are not actual GUIDS, but notification, such as XInput

}

interface

uses
  SysUtils, Classes, Generics.Collections,
  CastleJoysticks, CastleInternalJoystickRecord;

type
  TJoystickDictionary = specialize TObjectDictionary<TJoystick, TJoystickRecord>;

type
  TCastleJoysticks = class
  strict private
    JoysticksRecords: TJoystickDictionary;
    FDefaultJoystickRecord: TJoystickRecord;

    procedure SayJoystickEvent(const Joy: TJoystick; const Prefix: String; const JE: TJoystickEvent; const Value: Single = 0);

    procedure DoAxisMove(const Joy: TJoystick; const Axis: Byte; const Value: Single);
    //procedure DoButtonDown(const Joy: TJoystick; const Button: Byte);
    procedure DoButtonUp(const Joy: TJoystick; const Button: Byte);
    procedure DoButtonPress(const Joy: TJoystick; const Button: Byte);
  public
    procedure Initialize;
    destructor Destroy; override;
  end;

function JoysticksNew: TCastleJoysticks;

implementation
uses
  {$ifdef Linux}CastleInternalJoystickDatabaseLinux,{$endif}
  {$ifdef Windows}CastleInternalJoystickDatabaseWindows{$endif}
  CastleLog;

{ TJoystickManager ---------------------------------------------------------}

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
  R := JoysticksRecords.Items[Joy];
  JE := R.AxisEvent(Axis, Value);
  SayJoystickEvent(Joy, R.JoystickEventToStr(JE), JE, Value);
end;
{procedure TCastleJoysticks.DoButtonDown(const Joy: TJoystick; const Button: Byte);
var
  R: TJoystickRecord;
  JE: TJoystickEvent;
begin
  R := JoysticksRecords.Items[Joy];
  JE := R.ButtonEvent(Button);
  SayJoystickEvent(Joy, R.JoystickEventToStr(JE), JE);
end;}
procedure TCastleJoysticks.DoButtonUp(const Joy: TJoystick; const Button: Byte);
var
  R: TJoystickRecord;
  JE: TJoystickEvent;
begin
  R := JoysticksRecords.Items[Joy];
  JE := R.ButtonEvent(Button);
  SayJoystickEvent(Joy, R.JoystickEventToStr(JE), JE);
end;
procedure TCastleJoysticks.DoButtonPress(const Joy: TJoystick; const Button: Byte);
var
  R: TJoystickRecord;
  JE: TJoystickEvent;
begin
  R := JoysticksRecords.Items[Joy];
  JE := R.ButtonEvent(Button);
  SayJoystickEvent(Joy, R.JoystickEventToStr(JE), JE);
end;

procedure TCastleJoysticks.Initialize;

  function TrimJoystickName(const AJoystickName: String): String;
  begin
    Result := Trim(AJoystickName);
    while Pos('  ', Result) > 0 do
      Result := StringReplace(Result, '  ', ' ', [rfReplaceAll]);
    WriteLnLog(Result);
  end;

var
  I: Integer;
  J: TJoystick;
  R: TJoystickRecord;
  JoyName: String;
begin
  if JoysticksRecords = nil then
    JoysticksRecords := TJoystickDictionary.Create //owns nothing
  else
    JoysticksRecords.Clear;

  Joysticks.Initialize;
  Joysticks.OnAxisMove := @DoAxisMove;
  //Joysticks.OnButtonDown := @DoButtonDown;
  Joysticks.OnButtonUp := @DoButtonUp;
  Joysticks.OnButtonPress := @DoButtonPress;

  for I := 0 to Pred(Joysticks.Count) do
  begin
    J := Joysticks[I];
    WriteLnLog('Joystick Name', J.Info.Name);
    WriteLnLog('Joystick Buttons', IntToStr(J.Info.Count.Buttons));
    WriteLnLog('Joystick Axes', IntToStr(J.Info.Count.Axes));
    WriteLnLog('Joystick Caps', IntToStr(J.Info.Caps));
    //try autodetect the joystick
    JoyName := TrimJoystickName(J.Info.Name);
    if JoystickRecordsByName.ContainsKey(JoyName) then
    begin
      R := JoystickRecordsByName[JoyName];
      WriteLnLog('Joystick autodetected by name successfully!');
    end else
    begin
      JoyName := 'Microntek USB Joystick';
      R := JoystickRecordsByName[JoyName];
      WriteLnLog('Joystick failed to autodetect. Usind default record for ' + JoyName + '.');
    end;
    WriteLnLog(R.LogJoystickFeatures);
    JoysticksRecords.Add(J, R);

    WriteLnLog('--------------------');
  end;
end;

destructor TCastleJoysticks.Destroy;
begin
  FreeAndNil(JoysticksRecords);
  inherited;
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

