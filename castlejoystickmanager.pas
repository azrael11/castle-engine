unit CastleJoystickManager;

interface

uses
  SysUtils, Classes, Generics.Collections,
  CastleWindow, CastleKeysMouse, CastleVectors, CastleJoysticks,
  CastleInternalJoystickRecord;

const
  { Joystick buttons }
  joyA = keyPadB; {WARNING: inverting}
  joyB = keyPadA; {WARNING: inverting}
  joyY = keyPadX; {WARNING: inverting}
  joyX = keyPadY; {WARNING: inverting}
  joyBack = keyPadMinus;
  joyStart = keyPadPlus;
  joyLeftShoulder = keyPadL;
  joyRightShoulder = keyPadR;
  joyLeftTrigger = keyPadZL;
  joyRightTrigger = keyPadZR;
  joyLeftStick = keyPadL; {WARNING: duplicating}
  joyRightStick = keyPadR; {WARNING: duplicating}
  joyGuide = keyPadPlus; {WARNING: duplicating}
  joyLeft = keyPadLeft;
  joyRight = keyPadRight;
  joyUp = keyPadUp;
  joyDown = keyPadDown;

  {
  joyFakeLeft = joyLeft;
  joyFakeRight = joyRight;
  joyFakeUp = joyUp;
  joyFakeDown = joyDown;
  }

type
  { Temporary: these are the routines that need to go into new TJoystick }
  TJoystickAdditionalData = class
    LeftAxis, RightAxis, DPad: TVector2;
  end;

type
  TJoystickDictionary = specialize TObjectDictionary<TJoystick, TJoystickRecord>;
  TJoystickAdditionalDataDictionary = specialize TObjectDictionary<TJoystick, TJoystickAdditionalData>;

type
  TCastleJoysticks = class
  strict private
    const
      JoystickEpsilon = 0.3;
    var
      JoysticksRecords: TJoystickDictionary;
      JoysticksAdditionalData: TJoystickAdditionalDataDictionary;

    procedure SayJoystickEvent(const JoyName: String; const Prefix: String; const JE: TJoystickEvent; const Value: Single);
    procedure SendJoystickEvent(const Joy: TJoystick; const JE: TJoystickEvent; const Value: Single);

    procedure DoAxisMove(const Joy: TJoystick; const Axis: Byte; const Value: Single);
    //procedure DoButtonDown(const Joy: TJoystick; const Button: Byte);
    procedure DoButtonUp(const Joy: TJoystick; const Button: Byte);
    procedure DoButtonPress(const Joy: TJoystick; const Button: Byte);
  public
    Container: TWindowContainer;
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

procedure TCastleJoysticks.SayJoystickEvent(const JoyName: String; const Prefix: String; const JE: TJoystickEvent; const Value: Single);
begin
  if JE in AxisEvents then
    WriteLnLog(JoyName, Prefix + ':' + FloatToStr(Value))
  else
    WriteLnLog(JoyName, Prefix);
end;

procedure TCastleJoysticks.SendJoystickEvent(const Joy: TJoystick; const JE: TJoystickEvent; const Value: Single);

  function KeyToStr(const AKey: TKey): String;
  begin
    WriteStr(Result, AKey);
  end;

  procedure JoystickKey(const AKey: TKey; const AValue: Single);
  var
    UnusedStringVariable: String;
  begin
    if AValue > JoystickEpsilon then
    begin
      Container.EventPress(InputKey(TVector2.Zero, AKey, ''));
      Container.Pressed.KeyDown(AKey, '');
      WriteLnLog('Pressed', KeyToStr(AKey));
    end else
    begin
      Container.EventRelease(InputKey(TVector2.Zero, AKey, ''));
      Container.Pressed.KeyUp(AKey, UnusedStringVariable);
      WriteLnLog('Released', KeyToStr(AKey));
    end;
  end;

begin
  case JE of
    padA: JoystickKey(joyA, Value);
    padB: JoystickKey(joyB, Value);
    padY: JoystickKey(joyY, Value);
    padX: JoystickKey(joyX, Value);
    buttonBack: JoystickKey(joyBack, Value);
    buttonStart: JoystickKey(joyStart, Value);
    buttonLeftShoulder: JoystickKey(joyLeftShoulder, Value);
    buttonRightShoulder: JoystickKey(joyRightShoulder, Value);
    buttonLeftTrigger: JoystickKey(joyLeftTrigger, Value);
    buttonRightTrigger: JoystickKey(joyRightTrigger, Value);
    buttonLeftStick: JoystickKey(joyLeftStick, Value);
    buttonRightStick: JoystickKey(joyRightStick, Value);
    buttonGuide: JoystickKey(joyGuide, Value);
    dpadLeft: JoystickKey(joyLeft, Value);
    dpadRight: JoystickKey(joyRight, Value);
    dpadUp: JoystickKey(joyUp, Value);
    dpadDown: JoystickKey(joyDown, Value);

    {{ Primary axes }
    axisLeftX, axisLeftY,
    axisRightX, axisRightY,
    axisLeftXPlus, axisLeftXMinus, axisLeftYPlus, axisLeftYMinus,
    axisRightYPlus, axisRightYMinus, {note: there are no RightXPlus/minux events in the database }}

    else
      ;
  end;
  //SayJoystickEvent(Joy.Info.Name, JoystickEventToStr(JE), JE, Value);
end;

procedure TCastleJoysticks.DoAxisMove(const Joy: TJoystick; const Axis: Byte; const Value: Single);
begin
  //if not AxisInverted[Axis] then
  SendJoystickEvent(Joy, JoysticksRecords.Items[Joy].AxisEvent(Axis, Value), Value);
end;
{procedure TCastleJoysticks.DoButtonDown(const Joy: TJoystick; const Button: Byte);
begin
  SendJoystickEvent(Joy, JoysticksRecords.Items[Joy].ButtonEvent(Button), 1.0);
end;}
procedure TCastleJoysticks.DoButtonPress(const Joy: TJoystick; const Button: Byte);
begin
  SendJoystickEvent(Joy, JoysticksRecords.Items[Joy].ButtonEvent(Button), 1.0);
end;
procedure TCastleJoysticks.DoButtonUp(const Joy: TJoystick; const Button: Byte);
begin
  SendJoystickEvent(Joy, JoysticksRecords.Items[Joy].ButtonEvent(Button), 0.0);
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
  D: TJoystickAdditionalData;
  JoyName: String;
begin
  if JoysticksRecords = nil then
  begin
    JoysticksRecords := TJoystickDictionary.Create; //owns nothing
    JoysticksAdditionalData := TJoystickAdditionalDataDictionary.Create([doOwnsValues]);
  end else
  begin
    JoysticksRecords.Clear;
    JoysticksAdditionalData.Clear;
  end;

  InitJoysticksDatabase;

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

    D := TJoystickAdditionalData.Create;
    JoysticksAdditionalData.Add(J, D);

    WriteLnLog('--------------------');
  end;

  { we shouould free databases here, however, they own joystick records,
    i.e. first we should make copies }
end;

destructor TCastleJoysticks.Destroy;
begin
  FreeAndNil(JoysticksRecords);
  FreeAndNil(JoysticksAdditionalData);
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

