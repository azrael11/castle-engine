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
  TInvertAxes = specialize TList<Byte>;

type
  TJoystickLayout = class
  strict private
    type
      TSetOfJoystickEvents = set of TJoystickEvent;
    var
      FJoystickHasEvents: TSetOfJoystickEvents;
  public
    { GUID of the joystick in the database, unused for now }
    Guid: TGuid;
    { Which number of event (button/axis/pad) corresponds to which joystick event
      Note that some joysticks are tricky and have buttons assigned to axes
      or D-Pad to buttons }
    Buttons, AxesPlus, AxesMinus, DPad: TJoystickDictionary;
    {}
    InvertAxes: TInvertAxes;
    { Reported name of the joystick. Note, that currently our backend reports
      different joystick names, especially on Windows, where it often simply
      reports 'Microsoft PC-joystick driver' }
    JoystickName: String;
    { This joystick was reported as "buggy",
      sharing the same GUID with other joysticks with different axes/buttons layouts }
    BuggyGuid: Boolean;
    { The database for this joystick has duplicate entries for the same event
      e.g. axis[0] is reported as both LeftX and RightX.
      This means the data for this joystick is unreliable }
    BuggyDuplicateEvents: Boolean;
    function MakeCopy: TJoystickLayout;
    { Translate axis, D-Pads and button events reported by Backend to TJoystickEvent }
    function InvertAxis(const AxisID: Byte): Boolean;
    function AxisEvent(const AxisID: Byte; const AxisValue: Single): TJoystickEvent;
    function ButtonEvent(const ButtonID: Byte): TJoystickEvent;
    //function DPadEvent(const DPadParameters): TJoystickEvent;

    { Report if the joystick has a specific feature }
    property JoystickHasEvents: TSetOfJoystickEvents read FJoystickHasEvents;
    function HasLeftStick: Boolean;
    function HasRightStick: Boolean;
    function HasDPad: Boolean;
    function HasAbyx: Boolean;
    procedure CacheJoystickEvents;
    function LogJoystickFeatures: String;

    constructor Create; //override;
    destructor Destroy; override;
  end;

  TJoystickDatabase = specialize TObjectDictionary<String, TJoystickLayout>;

  { Database of joysticks by name/GUID,
    A database corresponding to the current OS will be loaded
    As different OS report different GUIDs and names for the same joystick }
var
  JoystickRecordsByName, JoystickRecordsByGuid: TJoystickDatabase;

function JoystickEventToStr(const Event: TJoystickEvent): String; {todo: move to JoystickParser?}
implementation
uses
  Classes, SysUtils,
  CastleLog, CastleUtils;

{ TJoystickLayout ---------------------------------------------------------}

constructor TJoystickLayout.Create;
begin
  inherited; //parent is empty
  BuggyGuid := false;
  BuggyDuplicateEvents := false;
  Buttons := TJoystickDictionary.Create;
  AxesPlus := TJoystickDictionary.Create;
  AxesMinus := TJoystickDictionary.Create;
  DPad := TJoystickDictionary.Create;
end;

destructor TJoystickLayout.Destroy;
begin
  FreeAndNil(Buttons);
  FreeAndNil(AxesPlus);
  FreeAndNil(AxesMinus);
  FreeAndNil(DPad);
  FreeAndNil(InvertAxes);
  inherited;
end;

function TJoystickLayout.MakeCopy: TJoystickLayout;
var
  B: Byte;
begin
  Result := TJoystickLayout.Create;
  Result.JoystickName := JoystickName;
  Result.Guid := Guid;
  for B in Buttons.Keys do
    Result.Buttons.Add(B, Buttons[B]);
  for B in AxesPlus.Keys do
    Result.AxesPlus.Add(B, AxesPlus[B]);
  for B in AxesMinus.Keys do
    Result.AxesMinus.Add(B, AxesMinus[B]);
  for B in DPad.Keys do
    Result.DPad.Add(B, DPad[B]);
  if InvertAxes <> nil then
  begin
    Result.InvertAxes := TInvertAxes.Create;
    for B in InvertAxes do
      Result.InvertAxes.Add(B);
  end;
  Result.CacheJoystickEvents;
end;

function TJoystickLayout.InvertAxis(const AxisID: Byte): Boolean;
begin
  if (InvertAxes <> nil) and (InvertAxes.Contains(AxisID)) then
    Result := true
  else
    Result := false;
end;

function TJoystickLayout.AxisEvent(const AxisID: Byte; const AxisValue: Single): TJoystickEvent;
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

function TJoystickLayout.ButtonEvent(const ButtonID: Byte): TJoystickEvent;
begin
  if not Buttons.TryGetValue(ButtonID, Result) then
    Result := unknownButtonEvent;
end;

procedure TJoystickLayout.CacheJoystickEvents;
  procedure ScanDictionary(const ADictionary: TJoystickDictionary);
  var
    B: Byte;
  begin
    for B in ADictionary.Keys do
      FJoystickHasEvents := FJoystickHasEvents + [ADictionary.Items[B]];
  end;
begin
  FJoystickHasEvents := [];
  ScanDictionary(Buttons);
  ScanDictionary(AxesPlus);
  ScanDictionary(AxesMinus);
  ScanDictionary(DPad);
end;

function TJoystickLayout.HasLeftStick: Boolean;
begin
  Result :=
    ((axisLeftX in JoystickHasEvents) or
    ((axisLeftXPlus in JoystickHasEvents) and (axisLeftXMinus in JoystickHasEvents))) and
    ((axisLeftY in JoystickHasEvents) or
    ((axisLeftYPlus in JoystickHasEvents) and (axisLeftYMinus in JoystickHasEvents)));
end;

function TJoystickLayout.HasRightStick: Boolean;
begin
  Result :=
    (axisRightX  in JoystickHasEvents) and
    ((axisRightY in JoystickHasEvents) or
    ((axisRightYPlus in JoystickHasEvents) and (axisRightYMinus in JoystickHasEvents)));
end;

function TJoystickLayout.HasDPad: Boolean;
begin
  Result :=
    (dpadLeft in JoystickHasEvents) and
    (dpadRight in JoystickHasEvents) and
    (dpadUp in JoystickHasEvents) and
    (dpadDown in JoystickHasEvents);
end;

function TJoystickLayout.HasAbyx: Boolean;
begin
  Result :=
    (padX in JoystickHasEvents) and
    (padY in JoystickHasEvents) and
    (padA in JoystickHasEvents) and
    (padB in JoystickHasEvents);
end;

function TJoystickLayout.LogJoystickFeatures: String;
var
  J: TJoystickEvent;
begin
  Result := 'Joystick ' + JoystickName + ' has the following features:' + NL;
  Result += 'GUID: ' + Guid + NL;
  if BuggyGuid then
    Result += 'This joystick GUID is known to be shared by different joyticks with different layouts' + NL;
  if BuggyDuplicateEvents then
    Result += 'The database for this joystick contains duplicate events, which makes mapping unreliable.' + NL;
  for J in TJoystickEvent do
    if not (J in [unknownEvent, unknownAxisEvent, unknownButtonEvent]) then
      Result += JoystickEventToStr(J) + ': ' + (J in JoystickHasEvents).ToString(TUseBoolStrs.True) + NL;
end;

function JoystickEventToStr(const Event: TJoystickEvent): String;
begin
  WriteStr(Result, Event);
end;

end.

