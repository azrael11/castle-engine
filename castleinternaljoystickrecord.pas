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
    axisLeftX, axisLeftY,
    axisRightX, axisRightY,
    axisLeftXPlus, axisLeftXMinus, axisLeftYPlus, axisLeftYMinus,
    axisRightYPlus, axisRightYMinus, {note: there are no RightXPlus/minux events in the database }

    { Pressing on the sticks }
    buttonLeftStick, buttonRightStick,

    { D-Pad buttons/axes }
    dpadLeft, dpadRight, dpadUp, dpadDown,

    { X-Box button }
    buttonGuide

    );

const
  AxisEvents = [axisLeftX, axisLeftY, axisRightX, axisRightY,
    axisLeftXPlus, axisLeftXMinus, axisLeftYPlus, axisLeftYMinus,
    axisRightYPlus, axisRightYMinus];

type
  TGuid = String[32];

type
  TJoystickDictionary = specialize TDictionary<Byte, TJoystickEvent>;

type
  TJoystickRecord = class
  strict private
    FCacheNotReady: Boolean;
    JoystickHasEvents: set of TJoystickEvent; //maybe it's better to publish it as a property so that the user may easily check if the joystick has an event?
    FHasLeftStick: Boolean;
    FHasRightStick: Boolean;
    FHasDPad: Boolean;
    FHasAbyx: Boolean;
    { Caches which event the joystick has }
    procedure Cache;
  public
    { GUID of the joystick in the database, unused for now }
    Guid: TGuid;
    { Which number of event (button/axis/pad) corresponds to which joystick event
      Note that some joysticks are tricky and have buttons assigned to axes
      or D-Pad to buttons }
    Buttons, AxesPlus, AxesMinus, DPad: TJoystickDictionary;
    { Reported name of the joystick. Note, that currently our backend reports
      different joystick names, especially on Windows, where it often simply
      reports 'Microsoft PC-joystick driver' }
    JoystickName: String;
    { This joystick was reported as "buggy",
      sharing the same GUID with other joysticks with different axes/buttons layouts }
    BuggyGuid: Boolean;
    function IsJoystickName(const AName: String): Boolean;
    { Translate axis, D-Pads and button events reported by Backend to TJoystickEvent }
    function AxisEvent(const AxisID: Byte; const AxisValue: Single): TJoystickEvent;
    function ButtonEvent(const ButtonID: Byte): TJoystickEvent;
    //function DPadEvent(const DPadParameters): TJoystickEvent;
    { Report if the joystick has a specific feature }
    function HasLeftStick: Boolean;
    function HasRightStick: Boolean;
    function HasDPad: Boolean;
    function HasAbyx: Boolean;

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
  BuggyGuid := false;
  FCacheNotReady := true;
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

procedure TJoystickRecord.Cache;
  procedure ScanDictionary(const ADictionary: TJoystickDictionary);
  var
    B: Byte;
  begin
    for B in ADictionary.Keys do
      JoystickHasEvents := JoystickHasEvents + [ADictionary.Items[B]];
  end;
begin
  JoystickHasEvents := [];
  ScanDictionary(Buttons);
  ScanDictionary(AxesPlus);
  ScanDictionary(AxesMinus);
  ScanDictionary(DPad);

  FHasLeftStick := (axisLeftX in JoystickHasEvents) and
                   (axisLeftY in JoystickHasEvents);
  FHasRightStick := (axisRightX  in JoystickHasEvents) and
                    (axisRightY in JoystickHasEvents);
  FHasDPad := (dpadLeft in JoystickHasEvents) and
              (dpadRight in JoystickHasEvents) and
              (dpadUp in JoystickHasEvents) and
              (dpadDown in JoystickHasEvents);
  FHasAbyx := (padX in JoystickHasEvents) and
              (padY in JoystickHasEvents) and
              (padA in JoystickHasEvents) and
              (padB in JoystickHasEvents);

  FCacheNotReady := false;
end;

function TJoystickRecord.HasLeftStick: Boolean;
begin
  if FCacheNotReady then
    Cache;
  Result := FHasLeftStick;
end;

function TJoystickRecord.HasRightStick: Boolean;
begin
  if FCacheNotReady then
    Cache;
  Result := FHasRightStick;
end;

function TJoystickRecord.HasDPad: Boolean;
begin
  if FCacheNotReady then
    Cache;
  Result := FHasDPad;
end;

function TJoystickRecord.HasAbyx: Boolean;
begin
  if FCacheNotReady then
    Cache;
  Result := FHasAbyx;
end;

end.

