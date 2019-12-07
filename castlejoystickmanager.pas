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
  joyLeftShoulder = keyPadZL;
  joyRightShoulder = keyPadZR;
  joyLeftTrigger = keyPadL;
  joyRightTrigger = keyPadR;
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
    TrimmedName: String;
    LeftAxis, RightAxis, DPad: TVector2;
  end;

type
  { Temporary: additional routines required for TJoysticks }
  TJoysticksHelper = class helper for TJoysticks
    function IndexOf(const Joy: TJoystick): Integer;
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
  CastleLog, CastleUtils;

function TJoysticksHelper.IndexOf(const Joy: TJoystick): Integer;
var
  I: Integer;
begin
  //Result := FList.IndexOf(Joy); //invisible private field
  Result := -1;
  for I := 0 to Pred(Count) do
    if Items[I] = Joy then
      Exit(I);
end;

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
    ButtonEvent: TInputPressRelease;
  begin
    ButtonEvent := InputKey(TVector2.Zero, AKey, '');
    ButtonEvent.FingerIndex := Joysticks.IndexOf(Joy);
    if AValue > JoystickEpsilon then
    begin
      Container.EventPress(ButtonEvent);
      Container.Pressed.KeyDown(ButtonEvent.Key, ButtonEvent.KeyString);
      WriteLnLog('Pressed', KeyToStr(ButtonEvent.Key));
    end else
    begin
      Container.EventRelease(ButtonEvent);
      Container.Pressed.KeyUp(ButtonEvent.Key, UnusedStringVariable);
      WriteLnLog('Released', KeyToStr(ButtonEvent.Key));
    end;
  end;

  //todo: optimize?
  procedure JoystickLeftXAxis(const AValue: Single);
  begin
    JoysticksAdditionalData.Items[Joy].LeftAxis :=
      Vector2(AValue, JoysticksAdditionalData.Items[Joy].LeftAxis.Y);
    WriteLnLog('LeftAxis:', JoysticksAdditionalData.Items[Joy].LeftAxis.ToString);
  end;
  procedure JoystickLeftYAxis(const AValue: Single);
  begin
    JoysticksAdditionalData.Items[Joy].LeftAxis :=
      Vector2(JoysticksAdditionalData.Items[Joy].LeftAxis.X, AValue);
    WriteLnLog('LeftAxis:', JoysticksAdditionalData.Items[Joy].LeftAxis.ToString);
  end;
  procedure JoystickRightXAxis(const AValue: Single);
  begin
    JoysticksAdditionalData.Items[Joy].RightAxis :=
      Vector2(AValue, JoysticksAdditionalData.Items[Joy].RightAxis.Y);
    WriteLnLog('RightAxis:', JoysticksAdditionalData.Items[Joy].RightAxis.ToString);
  end;
  procedure JoystickRightYAxis(const AValue: Single);
  begin
    JoysticksAdditionalData.Items[Joy].RightAxis :=
      Vector2(JoysticksAdditionalData.Items[Joy].RightAxis.X, AValue);
    WriteLnLog('RightAxis:', JoysticksAdditionalData.Items[Joy].RightAxis.ToString);
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

    axisLeftX, axisLeftXPlus: JoystickLeftXAxis(Value);
    axisLeftXMinus: JoystickLeftXAxis(-Value); //WARNING: I don't really know if it works this way as I don't have a joystick with this feature to test if the value should be inverted
    axisLeftY, axisLeftYPlus: JoystickLeftYAxis(Value);
    axisLeftYMinus: JoystickLeftYAxis(-Value);

    axisRightX: JoystickRightXAxis(Value);
    axisRightY, axisRightYPlus: JoystickRightYAxis(Value);
    axisRightYMinus: JoystickRightYAxis(-Value);

    else
      raise EInternalError.CreateFmt('Unknown joystick event received by SendJoystickEvent: %s.',
        [JoystickEventToStr(JE)]);
  end;
  //SayJoystickEvent(Joy.Info.Name, JoystickEventToStr(JE), JE, Value);
end;

procedure TCastleJoysticks.DoAxisMove(const Joy: TJoystick; const Axis: Byte; const Value: Single);
var
  JE: TJoystickEvent;
begin
  JE := JoysticksRecords.Items[Joy].AxisEvent(Axis, Value);
  if JE <> unknownAxisEvent then
  begin
    //if not AxisInverted[Axis] then
    SendJoystickEvent(Joy, JE, Value);
    //else SendJoystickEvent(Joy, JE, -Value);
  end else
    WriteLnLog('Warning', Format('Unknown "%s" (detected as "%s") joystick event at axis [%s]',
      [JoysticksAdditionalData.Items[Joy].TrimmedName,
       JoysticksRecords.Items[Joy].JoystickName, IntToStr(Axis)]));
end;
{procedure TCastleJoysticks.DoButtonDown(const Joy: TJoystick; const Button: Byte);
var
  JE: TJoystickEvent;
begin
  JE := JoysticksRecords.Items[Joy].ButtonEvent(Button);
  if JE <> unknownButtonEvent then
    SendJoystickEvent(Joy, JE, 1.0)
  else
    WriteLnLog('Warning', Format('Unknown "%s" (detected as "%s") joystick event at button [%s]',
      [JoysticksAdditionalData.Items[Joy].TrimmedName,
       JoysticksRecords.Items[Joy].JoystickName, IntToStr(Button)]));
end;}
procedure TCastleJoysticks.DoButtonPress(const Joy: TJoystick; const Button: Byte);
var
  JE: TJoystickEvent;
begin
  JE := JoysticksRecords.Items[Joy].ButtonEvent(Button);
  if JE <> unknownButtonEvent then
    SendJoystickEvent(Joy, JE, 1.0)
  else
    WriteLnLog('Warning', Format('Unknown "%s" (detected as "%s") joystick event at button [%s]',
      [JoysticksAdditionalData.Items[Joy].TrimmedName,
       JoysticksRecords.Items[Joy].JoystickName, IntToStr(Button)]));
end;
procedure TCastleJoysticks.DoButtonUp(const Joy: TJoystick; const Button: Byte);
var
  JE: TJoystickEvent;
begin
  JE := JoysticksRecords.Items[Joy].ButtonEvent(Button);
  if JE <> unknownButtonEvent then
    SendJoystickEvent(Joy, JE, 0.0)
  else
    WriteLnLog('Warning', Format('Unknown "%s" (detected as "%s") joystick event at button [%s]',
      [JoysticksAdditionalData.Items[Joy].TrimmedName,
       JoysticksRecords.Items[Joy].JoystickName, IntToStr(Button)]));
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
    JoyName := TrimJoystickName(J.Info.Name) + 'aaaa';
    if JoystickRecordsByName.ContainsKey(JoyName) then
    begin
      R := JoystickRecordsByName[JoyName];
      WriteLnLog('Joystick autodetected by name successfully!');
    end else
    begin
      {WARNING: this is my buggy Esperanza EG102, replace by X-Box in release}
      {$ifdef Windows}
      R := JoystickRecordsByGuid['03000000790000000600000000000000'];
      {$else}
      R := JoystickRecordsByGuid['03000000780000000600000010010000'];
      {$endif}
      WriteLnLog('Joystick failed to autodetect. Using default record for ' + R.JoystickName + '.');
    end;
    WriteLnLog(R.LogJoystickFeatures);
    JoysticksRecords.Add(J, R);

    D := TJoystickAdditionalData.Create;
    D.TrimmedName := JoyName;
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

