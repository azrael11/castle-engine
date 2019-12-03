unit CastleInternalJoystickRecord;

interface

uses
  Generics.Collections;

type
  TJoystickEvent = (
    { The event was not detected, in other words, it means an error }
    unknownEvent, unknownAxisEvent, unknownButtonEvent,

    { Primary pad buttons }
    padX, padY, padA, padB,

    { Utility buttons }
    buttonBack, buttonStart,

    { Shoulder buttons }
    buttonRightShoulder, buttonLeftShoulder,
    buttonRightTrigger, buttonLeftTrigger,

    { Primary axes }
    axisLeftX, axisLeftY, axisRightX, axisRightY,
    axisLeftXPlus, axisLeftXMinus, axisLeftYPlus, axisLeftYMinus,
    axisRightYPlus, axisRightYMinus,

    { Pressing on the sticks }
    buttonLeftStick, buttonRightStick,

    { Hat buttons/axes }
    dpadLeft, dpadRight, dpadUp, dpadDown,

    { X-Box button }
    buttonGuide

    );

const
  AxisEvents = [axisLeftX, axisLeftY, axisRightX, axisRightY,
    axisLeftXPlus, axisLeftXMinus, axisLeftYPlus, axisLeftYMinus,
    axisRightYPlus, axisRightYMinus];

type
  TJoystickDictionary = specialize TDictionary<Byte, TJoystickEvent>;

type
  TJoystickRecord = class
  public
    { GUID of the joystick in the database, unused for now }
    Guid: String;
    { Which number of event (button/axis/pad) corresponds to which joystick event
      Note that some joysticks are tricky and have buttons assigned to axes
      or D-Pad to buttons }
    Buttons, AxesPlus, AxesMinus, DPad: TJoystickDictionary;
    { Reported name of the joystick. Note, that currently our backend reports
      different joystick names, especially on Windows, where it often simply
      reports 'Microsoft PC-joystick driver' }
    JoystickName: String;
    function IsJoystickName(const AName: String): Boolean;
    function AxisEvent(const AxisID: Byte; const AxisValue: Single): TJoystickEvent;
    function ButtonEvent(const ButtonID: Byte): TJoystickEvent;
    function JoystickEventToStr(const Event: TJoystickEvent): String; {todo: move to JoystickParser?}
    constructor Create; //override;
    destructor Destroy; override;
  end;

  TJoystickDatabase = specialize TObjectDictionary<String, TJoystickRecord>;

var
  { Database of joysticks by name,
    A database corresponding to the current OS will be loaded
    As different OS report different GUIDs and names for the same joystick
    (note, that you can have only one database loaded simultaneously)}
  JoystickDatabase: TJoystickDatabase;
  { todo: When we have the autodetection pipeline, we'll have to use
  JoystickGUIDDatabase: TJoystickDatabase;
    and detect the joystick by its GUID.
    Note that some joysticks use the same GUID, but have different axis/buttons layout.
    That includes my Esperanza EG102, with (Linux) reported name
    'Microntek USB Joystick' and sharing GUID '03000000780000000600000010010000'
    with several other joysticks with different layouts.
    Here we can also try to "surpass" the available database
    and allow joysticks be detected not only by GUID but also by name
    or at least leave a warning for the user in log about ambiguous joystick GUID }

implementation
uses
  Classes, SysUtils,
  CastleLog;

{ TJoystickRecord ---------------------------------------------------------}

constructor TJoystickRecord.Create;
begin
  inherited; //parent is empty
  Buttons := TJoystickDictionary.Create;
  AxesPlus := TJoystickDictionary.Create;
  AxesMinus := TJoystickDictionary.Create;
  DPad := TJoystickDictionary.Create;
end;

destructor TJoystickRecord.Destroy;
begin
  FreeAndNil(Buttons);
  FreeAndNil(AxesPlus);
  FreeAndNil(AxesMinus);
  FreeAndNil(DPad);
  inherited;
end;

function TJoystickRecord.IsJoystickName(const AName: String): Boolean;
begin
  //we might have additional information in JoystickName
  Result := Pos(AName, JoystickName) > 0;
end;

function TJoystickRecord.AxisEvent(const AxisID: Byte; const AxisValue: Single): TJoystickEvent;
begin
  if AxisValue >= 0 then
  begin
    if not AxesPlus.TryGetValue(AxisID, Result) then
      Result := unknownAxisEvent;
  end else
    if not AxesMinus.TryGetValue(AxisID, Result) then
      Result := unknownAxisEvent;

  //todo
  if Result = unknownAxisEvent then
  begin
    //try interpret the event as D-Pad - see bugs section above
    if AxisID = 6 then
    begin
      if AxisValue >= 0 then
        Result := dpadLeft
      else
        Result := dpadRight;
    end else
    if AxisID = 7 then
    begin
      if AxisValue >= 0 then
        Result := dpadUp
      else
        Result := dpadDown;
    end;
    //DPad.TryGetValue()
  end;
end;

function TJoystickRecord.ButtonEvent(const ButtonID: Byte): TJoystickEvent;
begin
  if not Buttons.TryGetValue(ButtonID, Result) then
    Result := unknownButtonEvent;
end;

function TJoystickRecord.JoystickEventToStr(const Event: TJoystickEvent): String;
begin
  WriteStr(Result, Event);
end;

end.

