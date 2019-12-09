{
  Copyright 2019-2019 Yevhen Loza, Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Joystick event manager:
  - Reads joysticks layouts database;
  - Tries to autodetect initialized joysticks by name;
  - Converts joystick events to Press events or Axes events. }
unit CastleJoystickManager;

interface

uses
  SysUtils, Classes, Generics.Collections,
  CastleWindow, CastleKeysMouse, CastleVectors, CastleJoysticks,
  CastleInternalJoystickLayout;

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

  joyFakeLeft = joyLeft;
  joyFakeRight = joyRight;
  joyFakeUp = joyUp;
  joyFakeDown = joyDown;

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
  { Temporary: to be merged with TJoysticks }
  TCastleJoysticks = class
  private //we need to access some of these properties in TFakeJoysitckEventHandler
    const
      { "Dead zone" of the joystick axes, that do not trigger button events }
      JoystickEpsilon = 0.3;
    type
      { Converts joystick axis events into "fake" buttons events
        Note, due to the specific way the joystick axes events are handled,
        Update needs to be called every frame.
        If FakeJoystickEventsHandler is not going to be used for a long time
        ReleaseAllFakeEvents should be called to avoid Press-related bugs.
        Note, that this class is designed to handle only simple cases,
        such as menu navigation. }
      TFakeJoystickEventsHandler = class
      strict private

      public
        IsActive: Boolean;
        procedure Update;
        procedure ReleaseAllFakeEvents;
        constructor Create; //override;
        destructor Destroy; override;
      end;
    var
      FakeEventsHandler: TFakeJoystickEventsHandler;
      JoysticksLayouts: TJoystickDictionary;
      JoysticksAdditionalData: TJoystickAdditionalDataDictionary;

    { Current defaults are:
      Windows: 030000005e0400000a0b000000000000, Xbox Adaptive Controller
      Linux: 030000006f0e00001304000000010000, Generic X-Box pad }
    function DefaultJoystickGuid: String;
    { Correctly send this joystick event to Container.Pressed or
      TJoystick.LeftAxis, TJoystick.RightAxis }
    procedure SendJoystickEvent(const Joy: TJoystick; const JEP: TJoystickEventPair; const Value: Single);

    procedure DoAxisMove(const Joy: TJoystick; const Axis: Byte; const Value: Single);
    //procedure DoButtonDown(const Joy: TJoystick; const Button: Byte);
    procedure DoButtonUp(const Joy: TJoystick; const Button: Byte);
    procedure DoButtonPress(const Joy: TJoystick; const Button: Byte);

    function GetGenerateFakeEvents: Boolean;
    procedure SetGenerateFakeEvents(const AValue: Boolean);
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
      @groupBegin }
    procedure InitializeDatabase;
    procedure FreeDatabase;
    { @groupEnd }

    { Return a list of names of joystick layouts available in currently loaded database
      nil if no database had been loaded }
    function JoysticksLayoutsNames: TStringList;
    { Return a list of GUIDs of joystick layouts available in currently loaded database
      nil if no database had been loaded }
    function JoysticksLayoutsGuids: TStringList;
    { Assign Joy to use layout named JoystickName
      The joystick database has to be loaded and record named JoystickName must be available
      Note, that different OSes report the same physical gamepad by different names }
    procedure AssignJoystickLayoutByName(const Joy: TJoystick; const JoystickName: String);
    { Assign Joy to use layout with GUID=JoystickGuid
      The joystick database has to be loaded and record with GUID=JoystickGuid must be available
      Note, that different OSes report the same physical gamepad by different GUIDs }
    procedure AssignJoystickLayoutByGuid(const Joy: TJoystick; const JoystickGuid: String);

    procedure UpdateFakeEvents;
    property GenerateFakeEvents: Boolean read GetGenerateFakeEvents write SetGenerateFakeEvents;

    constructor Create; //override;
    destructor Destroy; override;
  end;

function JoysticksNew: TCastleJoysticks;

implementation
uses
  {$ifdef Linux}CastleInternalJoystickDatabaseLinux, {$endif}
  {$ifdef Windows}CastleInternalJoystickDatabaseWindows, {$endif}
  {$ifdef MSWINDOWS} CastleInternalJoysticksWindows, {$endif} //needed to report a buggy library name, nothing more
  CastleLog, CastleUtils;

function TJoysticksHelper.IndexOf(const Joy: TJoystick): Integer;
var
  I: Integer;
begin
  //Result := FList.IndexOf(Joy); //unaccessible private field
  Result := -1;
  for I := 0 to Pred(Count) do
    if Items[I] = Joy then
      Exit(I);
end;

{ TJoystickManager ---------------------------------------------------------}

procedure TCastleJoysticks.SendJoystickEvent(const Joy: TJoystick; const JEP: TJoystickEventPair; const Value: Single);

  function KeyToStr(const AKey: TKey): String;
  begin
    WriteStr(Result, AKey);
  end;

  function JoystickEventToKey(const AJoystickEvent: TJoystickEvent): TKey;
  begin
    case AJoystickEvent of
      padA: Result := joyA;
      padB: Result := joyB;
      padY: Result := joyY;
      padX: Result := joyX;
      buttonBack: Result := joyBack;
      buttonStart: Result := joyStart;
      buttonLeftShoulder: Result := joyLeftShoulder;
      buttonRightShoulder: Result := joyRightShoulder;
      buttonLeftTrigger: Result := joyLeftTrigger;
      buttonRightTrigger: Result := joyRightTrigger;
      buttonLeftStick: Result := joyLeftStick;
      buttonRightStick: Result := joyRightStick;
      buttonGuide: Result := joyGuide;

      dpadLeft: Result := joyLeft;
      dpadRight: Result := joyRight;
      dpadUp: Result := joyUp;
      dpadDown: Result := joyDown;
      else
        raise EInternalError.Create('Error: Received an unexpected joystick event ' + JoystickEventToStr(AJoystickEvent) + ' in TCastleJoysticks.SendJoystickEvent.JoystickEventToKey.');
    end;
  end;

  procedure JoystickKey(const AValue: Single);
  var
    UnusedStringVariable: String;
    ButtonEvent: TInputPressRelease;
    EventReleased: Boolean;
  begin
    ButtonEvent := InputKey(TVector2.Zero, JoystickEventToKey(JEP.Primary), '');
    ButtonEvent.FingerIndex := Joysticks.IndexOf(Joy);
    if Abs(AValue) > JoystickEpsilon then
    begin
      Container.EventPress(ButtonEvent);
      Container.Pressed.KeyDown(ButtonEvent.Key, ButtonEvent.KeyString);
      WriteLnLog('Pressed', KeyToStr(ButtonEvent.Key));
    end else
    begin
      EventReleased := false;
      if Container.Pressed[ButtonEvent.Key] then
      begin
        Container.EventRelease(ButtonEvent);
        Container.Pressed.KeyUp(ButtonEvent.Key, UnusedStringVariable);
        WriteLnLog('Released', KeyToStr(ButtonEvent.Key));
        EventReleased := true;
      end;
      if JEP.Inverse <> JEP.Primary then
      begin
        ButtonEvent := InputKey(TVector2.Zero, JoystickEventToKey(JEP.Inverse), '');
        ButtonEvent.FingerIndex := Joysticks.IndexOf(Joy);
        if Container.Pressed[ButtonEvent.Key] then
        begin
          Container.EventRelease(ButtonEvent);
          Container.Pressed.KeyUp(ButtonEvent.Key, UnusedStringVariable);
          WriteLnLog('Released', KeyToStr(ButtonEvent.Key));
          EventReleased := true;
        end;
      end;
      if not EventReleased then
        WriteLnLog('Warning', Format('Received a releasing event, however neither %s nor %s have been pressed',
          [JoystickEventToStr(JEP.Primary), JoystickEventToStr(JEP.Inverse)]));
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
    { Y axes should be 1 when pointing up, -1 when pointing down.
      This is consistent with CGE 2D coordinate system
      (and standard math 2D coordinate system). }
    JoysticksAdditionalData.Items[Joy].LeftAxis :=
      Vector2(JoysticksAdditionalData.Items[Joy].LeftAxis.X, -AValue);
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
    { Y axes should be 1 when pointing up, -1 when pointing down.
      This is consistent with CGE 2D coordinate system
      (and standard math 2D coordinate system). }
    JoysticksAdditionalData.Items[Joy].RightAxis :=
      Vector2(JoysticksAdditionalData.Items[Joy].RightAxis.X, -AValue);
    WriteLnLog('RightAxis', JoysticksAdditionalData.Items[Joy].RightAxis.ToString);
  end;

begin
  case JEP.Primary of
    padA, padB, padY, padX,
    buttonBack, buttonStart,
    buttonLeftShoulder, buttonRightShoulder,
    buttonLeftTrigger, buttonRightTrigger,
    buttonLeftStick, buttonRightStick,
    buttonGuide,
    dpadLeft, dpadRight, dpadUp, dpadDown: JoystickKey(Value);

    axisLeftX, axisLeftXPlus: JoystickLeftXAxis(Value);
    axisLeftXMinus: JoystickLeftXAxis(-Value); //WARNING: I don't really know if it works this way as I don't have a joystick with this feature to test if the value should be inverted
    axisLeftY, axisLeftYPlus: JoystickLeftYAxis(Value);
    axisLeftYMinus: JoystickLeftYAxis(-Value);

    axisRightX: JoystickRightXAxis(Value);
    axisRightY, axisRightYPlus: JoystickRightYAxis(Value);
    axisRightYMinus: JoystickRightYAxis(-Value);

    else
      raise EInternalError.CreateFmt('%s sent an unknown joystick event received by SendJoystickEvent: %s.',
        [Joy.Info.Name, JoystickEventToStr(JEP.Primary)]);
  end;
end;

procedure TCastleJoysticks.DoAxisMove(const Joy: TJoystick; const Axis: Byte; const Value: Single);
var
  JL: TJoystickLayout;
  JEP: TJoystickEventPair;
begin
  JL := JoysticksLayouts.Items[Joy];
  if (Axis <> 6) and (Axis <> 7) then //if event is not D-Pad
  begin
    JEP := JL.AxisEvent(Axis, Value);
    if JEP.Primary <> unknownAxisEvent then
    begin
      if (JL.InvertAxis(Axis)) {temp} xor (Axis = JOY_AXIS_Y) {/temp} then //temporary: counteract the backend inverting JOY_AXIS_Y
        SendJoystickEvent(Joy, JEP, -Value)
      else
        SendJoystickEvent(Joy, JEP, Value);
    end else
      WriteLnLog('Warning', Format('Unknown "%s" (detected as "%s") joystick event at axis [%s]',
        [JoysticksAdditionalData.Items[Joy].TrimmedName,
         JoysticksLayouts.Items[Joy].JoystickName, IntToStr(Axis)]));
  end else
  begin
    JEP := JL.DPadEvent(Axis, Value);
    if JEP.Primary <> unknownAxisEvent then
      SendJoystickEvent(Joy, JEP, Value)
    else
      WriteLnLog('Warning', Format('Unknown "%s" (detected as "%s") joystick event at D-Pad axis [%s]',
        [JoysticksAdditionalData.Items[Joy].TrimmedName,
         JoysticksLayouts.Items[Joy].JoystickName, IntToStr(Axis)]));
  end;
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
  JEP: TJoystickEventPair;
begin
  JEP.Primary := JoysticksLayouts.Items[Joy].ButtonEvent(Button);
  JEP.Inverse := JEP.Primary;
  if JEP.Primary <> unknownButtonEvent then
    SendJoystickEvent(Joy, JEP, 1.0)
  else
    WriteLnLog('Warning', Format('Unknown "%s" (detected as "%s") joystick event at button [%s]',
      [JoysticksAdditionalData.Items[Joy].TrimmedName,
       JoysticksLayouts.Items[Joy].JoystickName, IntToStr(Button)]));
end;
procedure TCastleJoysticks.DoButtonUp(const Joy: TJoystick; const Button: Byte);
var
  JEP: TJoystickEventPair;
begin
  JEP.Primary := JoysticksLayouts.Items[Joy].ButtonEvent(Button);
  JEP.Inverse := JEP.Primary;
  if JEP.Primary <> unknownButtonEvent then
    SendJoystickEvent(Joy, JEP, 0.0)
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
    {$ifdef Windows}
    if JoyName = 'Microsoft PC-joystick driver' then
      WriteLnLog(Format('%s reported a generic joystick name. Unfortunately autodetect will fail.', [WINMMLIB]));
    {$endif}
    if JoystickLayoutsByName.ContainsKey(JoyName) then
    begin
      JL := JoystickLayoutsByName[JoyName].MakeCopy;
      if not JL.BuggyDuplicateName then
        WriteLnLog('Joystick autodetected by name successfully!')
      else
        WriteLnLog('Warning: Joystick was autodetected by name, however, there are several different layouts available for this joystick name.');
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
        if not NewJoystickLayout.BuggyDuplicateName then
          WriteLnLog(Format('Joystick "%s" layout has been successfully changed to "%s".', [JoysticksAdditionalData[Joy].TrimmedName, NewJoystickLayout.JoystickName]))
        else
          WriteLnLog(Format('Warning: Joystick "%s" layout has been successfully changed to "%s". However, there are several different layouts for this joystick name in the databse.', [JoysticksAdditionalData[Joy].TrimmedName, NewJoystickLayout.JoystickName]));
        WriteLnLog(NewJoystickLayout.LogJoystickFeatures);
      end else
        WriteLnLog('Warning', Format('Joystick name "%s" not found in the database. No changes were made.', [JoystickName]));
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
        WriteLnLog('Warning', Format('Joystick GUID "%s" not found in the database. No changes were made.', [JoystickGuid]));
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

procedure TCastleJoysticks.UpdateFakeEvents;
begin
  if GenerateFakeEvents then
    FakeEventsHandler.Update;
end;

function TCastleJoysticks.GetGenerateFakeEvents: Boolean;
begin
  Result := (FakeEventsHandler <> nil) and (FakeEventsHandler.IsActive);
end;

procedure TCastleJoysticks.SetGenerateFakeEvents(const AValue: Boolean);
begin
  if FakeEventsHandler = nil then
  begin
    if AValue = false then
      Exit //do not create anything if FakeEventsHandler is not requested
    else
      FakeEventsHandler := TFakeJoystickEventsHandler.Create;
  end;
  FakeEventsHandler.IsActive := AValue;
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
  FreeAndNil(FakeEventsHandler);
  inherited;
end;

{ TCastleJoysticks.TFakeJoystickEventsHandler --------------------------------}

procedure TCastleJoysticks.TFakeJoystickEventsHandler.Update;
begin
  //update events every frame
end;

procedure TCastleJoysticks.TFakeJoystickEventsHandler.ReleaseAllFakeEvents;
begin
  //release all events
end;

constructor TCastleJoysticks.TFakeJoystickEventsHandler.Create;
begin
  inherited; //parent is empty
  IsActive := true;
end;

destructor TCastleJoysticks.TFakeJoystickEventsHandler.Destroy;
begin
  ReleaseAllFakeEvents;
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

