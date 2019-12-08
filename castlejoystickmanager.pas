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
  TJoystickDictionary = specialize TObjectDictionary<TJoystick, TJoystickLayout>;
  TJoystickAdditionalDataDictionary = specialize TObjectDictionary<TJoystick, TJoystickAdditionalData>;

type
  TCastleJoysticks = class
  strict private
    const
      JoystickEpsilon = 0.3;
    var
      JoysticksLayouts: TJoystickDictionary;
      JoysticksAdditionalData: TJoystickAdditionalDataDictionary;

    procedure SayJoystickEvent(const JoyName: String; const Prefix: String; const JE: TJoystickEvent; const Value: Single);
    { Current defaults are:
      Windows: 030000005e0400000a0b000000000000, Xbox Adaptive Controller
      Linux: 030000006f0e00001304000000010000, Generic X-Box pad }
    function DefaultJoystickGuid: String;

    procedure SendJoystickEvent(const Joy: TJoystick; const JE: TJoystickEvent; const Value: Single);

    procedure DoAxisMove(const Joy: TJoystick; const Axis: Byte; const Value: Single);
    //procedure DoButtonDown(const Joy: TJoystick; const Button: Byte);
    procedure DoButtonUp(const Joy: TJoystick; const Button: Byte);
    procedure DoButtonPress(const Joy: TJoystick; const Button: Byte);
  public
    { Window container that will receive joystick buttons press events }
    Container: TWindowContainer;
    { Determines if the joystick layouts database freed immediately
      after the joysticks have been autodetected.
      If you need to propose the player to choose a joystick manually
      this value should be set to false
      Default: true }
    FreeJoysticksDatabaseAfterInitialization: Boolean;
    { Initialize the joysticks connected to the system
      and tries to autodetect joysticks layouts }
    procedure Initialize;
    { Initializes/frees joysticks database to save up memory
      Use in case you want to manually manage the joystick database
      (e.g. in case you want the player to be able to select
      joystick model manually in the game menu)
      @groupbegin }
    procedure InitializeDatabase;
    procedure FreeDatabase;
    { @groupend }

    function JoysticksLayoutsNames: TStringList;
    function JoysticksLayoutsGuids: TStringList;
    procedure AssignJoystickLayoutByName(const Joy: TJoystick; const JoystickName: String);
    procedure AssignJoystickLayoutByGuid(const Joy: TJoystick; const JoystickGuid: String);

    constructor Create; //override;
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
    WriteLnLog('LeftAxis', JoysticksAdditionalData.Items[Joy].LeftAxis.ToString);
  end;
  procedure JoystickLeftYAxis(const AValue: Single);
  begin
    JoysticksAdditionalData.Items[Joy].LeftAxis :=
      Vector2(JoysticksAdditionalData.Items[Joy].LeftAxis.X, AValue);
    WriteLnLog('LeftAxis', JoysticksAdditionalData.Items[Joy].LeftAxis.ToString);
  end;
  procedure JoystickRightXAxis(const AValue: Single);
  begin
    JoysticksAdditionalData.Items[Joy].RightAxis :=
      Vector2(AValue, JoysticksAdditionalData.Items[Joy].RightAxis.Y);
    WriteLnLog('RightAxis', JoysticksAdditionalData.Items[Joy].RightAxis.ToString);
  end;
  procedure JoystickRightYAxis(const AValue: Single);
  begin
    JoysticksAdditionalData.Items[Joy].RightAxis :=
      Vector2(JoysticksAdditionalData.Items[Joy].RightAxis.X, AValue);
    WriteLnLog('RightAxis', JoysticksAdditionalData.Items[Joy].RightAxis.ToString);
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
      raise EInternalError.CreateFmt('$s sent an unknown joystick event received by SendJoystickEvent: %s.',
        [Joy.Info.Name, JoystickEventToStr(JE)]);
  end;
  //SayJoystickEvent();
end;

procedure TCastleJoysticks.DoAxisMove(const Joy: TJoystick; const Axis: Byte; const Value: Single);
var
  JL: TJoystickLayout;
  JE: TJoystickEvent;
begin
  JL := JoysticksLayouts.Items[Joy];
  JE := JL.AxisEvent(Axis, Value);
  if JE <> unknownAxisEvent then
  begin
    if not JL.InvertAxis(Axis) then
      SendJoystickEvent(Joy, JE, Value)
    else
      SendJoystickEvent(Joy, JE, -Value);
  end else
    WriteLnLog('Warning', Format('Unknown "%s" (detected as "%s") joystick event at axis [%s]',
      [JoysticksAdditionalData.Items[Joy].TrimmedName,
       JoysticksLayouts.Items[Joy].JoystickName, IntToStr(Axis)]));
end;
{procedure TCastleJoysticks.DoButtonDown(const Joy: TJoystick; const Button: Byte);
var
  JE: TJoystickEvent;
begin
  JE := JoysticksLayouts.Items[Joy].ButtonEvent(Button);
  if JE <> unknownButtonEvent then
    SendJoystickEvent(Joy, JE, 1.0)
  else
    WriteLnLog('Warning', Format('Unknown "%s" (detected as "%s") joystick event at button [%s]',
      [JoysticksAdditionalData.Items[Joy].TrimmedName,
       JoysticksLayouts.Items[Joy].JoystickName, IntToStr(Button)]));
end;}
procedure TCastleJoysticks.DoButtonPress(const Joy: TJoystick; const Button: Byte);
var
  JE: TJoystickEvent;
begin
  JE := JoysticksLayouts.Items[Joy].ButtonEvent(Button);
  if JE <> unknownButtonEvent then
    SendJoystickEvent(Joy, JE, 1.0)
  else
    WriteLnLog('Warning', Format('Unknown "%s" (detected as "%s") joystick event at button [%s]',
      [JoysticksAdditionalData.Items[Joy].TrimmedName,
       JoysticksLayouts.Items[Joy].JoystickName, IntToStr(Button)]));
end;
procedure TCastleJoysticks.DoButtonUp(const Joy: TJoystick; const Button: Byte);
var
  JE: TJoystickEvent;
begin
  JE := JoysticksLayouts.Items[Joy].ButtonEvent(Button);
  if JE <> unknownButtonEvent then
    SendJoystickEvent(Joy, JE, 0.0)
  else
    WriteLnLog('Warning', Format('Unknown "%s" (detected as "%s") joystick event at button [%s]',
      [JoysticksAdditionalData.Items[Joy].TrimmedName,
       JoysticksLayouts.Items[Joy].JoystickName, IntToStr(Button)]));
end;

function TCastleJoysticks.DefaultJoystickGuid: String;
begin
  {$ifdef Windows}
  Result := '030000005e0400000a0b000000000000';
  {$else}
  {$ifdef Linux}
  Result := '030000006f0e00001304000000010000';
  {$endif}
  {$endif}
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
  JL: TJoystickLayout;
  JAD: TJoystickAdditionalData;
  JoyName: String;
begin
  if JoysticksLayouts = nil then
  begin
    JoysticksLayouts := TJoystickDictionary.Create([doOwnsValues]);
    JoysticksAdditionalData := TJoystickAdditionalDataDictionary.Create([doOwnsValues]);
  end else
  begin
    JoysticksLayouts.Clear;
    JoysticksAdditionalData.Clear;
  end;

  InitializeDatabase;

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

    //try autodetect the joystick by GUID
    //if autodetect by GUID failed then try
    JoyName := TrimJoystickName(J.Info.Name);
    if JoystickLayoutsByName.ContainsKey(JoyName) then
    begin
      JL := JoystickLayoutsByName[JoyName].MakeCopy;
      WriteLnLog('Joystick autodetected by name successfully!');
    end else
    //if autodetect by name failed then
    begin
      JL := JoystickLayoutsByGuid[DefaultJoystickGuid].MakeCopy;
      WriteLnLog('Joystick failed to autodetect. Using default layout for ' + JL.JoystickName + '.');
    end;

    WriteLnLog(JL.LogJoystickFeatures);
    JoysticksLayouts.Add(J, JL);

    JAD := TJoystickAdditionalData.Create;
    JAD.TrimmedName := JoyName;
    JoysticksAdditionalData.Add(J, JAD);

    WriteLnLog('--------------------');
  end;

  if FreeJoysticksDatabaseAfterInitialization then
    FreeDatabase;
end;

function TCastleJoysticks.JoysticksLayoutsNames: TStringList;
var
  S: String;
begin
  if JoystickLayoutsByName = nil then
  begin
    Result := nil;
    WriteLnLog('Warning', 'Joysticks layouts database not initialized, use FreeJoysticksDatabaseAfterInitialization = false to avoid freeing joysticks layouts database or initialize it manually through InitializeDatabase. Unable to JoysticksLayoutsNames.')
  end else
  begin
    Result := TStringList.Create;
    for S in JoystickLayoutsByName.Keys do
      Result.Add(S);
  end;
end;

function TCastleJoysticks.JoysticksLayoutsGuids: TStringList;
var
  S: String;
begin
  if JoystickLayoutsByGuid = nil then
  begin
    Result := nil;
    WriteLnLog('Warning', 'Joysticks layouts database not initialized, use FreeJoysticksDatabaseAfterInitialization = false to avoid freeing joysticks layouts database or initialize it manually through InitializeDatabase. Unable to JoysticksLayoutsGuids.')
  end else
  begin
    Result := TStringList.Create;
    for S in JoystickLayoutsByGuid.Keys do
      Result.Add(S);
  end;
end;

procedure TCastleJoysticks.AssignJoystickLayoutByName(const Joy: TJoystick; const JoystickName: String);
var
  NewJoystickLayout: TJoystickLayout;
begin
  if JoysticksLayouts = nil then
    WriteLnLog('Warning', 'Joysticks not initialized. Unable to AssignJoystickLayoutByName.')
  else
  begin
    if JoystickLayoutsByName = nil then
      WriteLnLog('Warning', 'Joysticks layouts database not initialized, use FreeJoysticksDatabaseAfterInitialization = false to avoid freeing joysticks layouts database or initialize it manually through InitializeDatabase. Unable to AssignJoystickLayoutByName.')
    else
    begin
      if JoystickLayoutsByName.ContainsKey(JoystickName) then
      begin
        NewJoystickLayout := JoystickLayoutsByName[JoystickName].MakeCopy;
        JoysticksLayouts.AddOrSetValue(Joy, NewJoystickLayout);
        WriteLnLog(Format('Joystick "%s" layout has been successfully changed to "%s".', [JoysticksAdditionalData[Joy].TrimmedName, NewJoystickLayout.JoystickName]));
        WriteLnLog(NewJoystickLayout.LogJoystickFeatures);
      end else
        WriteLnLog('Warning', Format('Joystick name "%s" not found in the database. No changes made.', [JoystickName]));
    end;
  end;
end;

procedure TCastleJoysticks.AssignJoystickLayoutByGuid(const Joy: TJoystick; const JoystickGuid: String);
var
  NewJoystickLayout: TJoystickLayout;
begin
  if JoysticksLayouts = nil then
    WriteLnLog('Warning', 'Joysticks not initialized. Unable to AssignJoystickLayoutByGuid.')
  else
  begin
    if JoystickLayoutsByGuid = nil then
      WriteLnLog('Warning', 'Joysticks layouts database not initialized, use FreeJoysticksDatabaseAfterInitialization = false to avoid freeing joysticks layouts database or initialize it manually through InitializeDatabase. Unable to AssignJoystickLayoutByGuid.')
    else
    begin
      if JoystickLayoutsByGuid.ContainsKey(JoystickGuid) then
      begin
        NewJoystickLayout := JoystickLayoutsByGuid[JoystickGuid].MakeCopy;
        JoysticksLayouts.AddOrSetValue(Joy, NewJoystickLayout);
        WriteLnLog(Format('Joystick "%s" layout has been successfully changed to "%s".', [JoysticksAdditionalData[Joy].TrimmedName, NewJoystickLayout.JoystickName]));
        WriteLnLog(NewJoystickLayout.LogJoystickFeatures);
      end else
        WriteLnLog('Warning', Format('Joystick GUID "%s" not found in the database. No changes made.', [JoystickGuid]));
    end;
  end;
end;

procedure TCastleJoysticks.InitializeDatabase;
begin
  InitJoysticksDatabase;
end;

procedure TCastleJoysticks.FreeDatabase;
begin
  FreeJoysticksDatabase;
end;

constructor TCastleJoysticks.Create;
begin
  inherited; //parent is empty
  FreeJoysticksDatabaseAfterInitialization := true;
end;

destructor TCastleJoysticks.Destroy;
begin
  FreeAndNil(JoysticksLayouts);
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

