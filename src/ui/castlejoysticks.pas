{
  Copyright 2015-2019 Tomasz Wojtyś, Michalis Kamburelis.
  Based on zgl_joystick.pas by Andrey Kemka.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.
  This file is based on ZenGL, with "zlib" license
  ( http://www.zengl.org/license.html ) which is fully compatible with
  Castle Game Engine "LGPL with static linking exception" / GPL licensing.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Cross-platform joystick and gamepad handling. }
unit CastleJoysticks;

{$I castleconf.inc}

interface

uses
  Generics.Collections, Classes,
  CastleVectors, CastleKeysMouse, CastleApplicationProperties,
  CastleUiControls, CastleTimeUtils,
  CastleInternalJoystickLayout;

const
  { Joystick buttons
    TODO: move into CastleKeysMouse?
    TODO: assign special key events }
  joySouth = keyPadB; {WARNING: inverting}
  joyEast = keyPadA; {WARNING: inverting}
  joyNorth = keyPadX; {WARNING: inverting}
  joyWest = keyPadY; {WARNING: inverting}
  joyBack = keyPadMinus;
  joyStart = keyPadPlus;
  joyLeftShoulder = keyPadL;
  joyRightShoulder = keyPadR;
  joyLeftTrigger = keyPadZL;
  joyRightTrigger = keyPadZR;
  joyLeftStick = keyReserved_178;
  joyRightStick = keyReserved_179;
  joyGuide = keyReserved_180;
  joyLeft = keyPadLeft;
  joyRight = keyPadRight;
  joyUp = keyPadUp;
  joyDown = keyPadDown;

  joyFakeLeft = joyLeft; {WARNING: duplicating}
  joyFakeRight = joyRight;
  joyFakeUp = joyUp;
  joyFakeDown = joyDown;
  { Note, duplicate events will cause minor bugs with Release event }

  itJoystick = itKey; //a small temporary hack to send correct joystick press events

type
  PJoyInfo = ^TJoyInfo;
  { Joystick information.
    TODO: Deprecate at some point, in favor of simpler joystick API like @link(TJoystick.Axis). }
  TJoyInfo = record
    Name   : String;
    Count  : record
      Axes    : Integer;
      Buttons : Integer;
             end;
    Caps   : LongWord;
  end;

  PJoyState = ^TJoyState;
  { Joystick state.
    TODO: Deprecate at some point, in favor of simpler joystick API like @link(TJoystick.Axis). }
  TJoyState = record
    Axis        : array[ 0..17 ] of Single;
    BtnUp       : array[ 0..31 ] of Boolean;
    BtnDown     : array[ 0..31 ] of Boolean;
    BtnPress    : array[ 0..31 ] of Boolean;
    BtnCanPress : array[ 0..31 ] of Boolean;
  end;

  { Properties of a given joystick, use by accessing @link(TJoysticks.Items Joysticks[Index]).
    Do not construct instances of this yourself, TJoysticks creates
    this automatically when necessary. }
  TJoystick = class
  strict private
    FLayout: TJoystickLayout;
    procedure SetLayout(const NewLayout: TJoystickLayout);
  public
    { Implementation-specific information. }
    InternalBackendInfo: TObject;
    { Information.
      TODO: Deprecate at some point, in favor of simpler joystick API like @link(TJoystick.Axis). }
    Info    : TJoyInfo;
    { State.
      TODO: Deprecate at some point, in favor of simpler joystick API like @link(TJoystick.Axis). }
    State   : TJoyState;

    TrimmedName: String;
    LeftAxis, RightAxis, DPad: TVector2;

    property Layout: TJoystickLayout read FLayout write SetLayout;

    function Axis: TVector2; deprecated 'use LeftAxis and RightAxis';
    destructor Destroy; override;
  end;

  PJoy = TJoystick deprecated 'use TJoystick';

  TJoystickList = {$ifdef CASTLE_OBJFPC}specialize{$endif} TObjectList<TJoystick>;

const
  { TODO: Deprecate these constants at some point, in favor of simpler joystick API like @link(TJoystick.Axis). }

  JOY_HAS_Z   = $000001;
  JOY_HAS_R   = $000002;
  JOY_HAS_U   = $000004;
  JOY_HAS_V   = $000008;
  JOY_HAS_POV = $000010;

  JOY_AXIS_X = 0;
  JOY_AXIS_Y = 1;
  JOY_AXIS_Z = 2;
  JOY_AXIS_R = 3;
  JOY_AXIS_U = 4;
  JOY_AXIS_V = 5;
  JOY_POVX   = 6;
  JOY_POVY   = 7;

  JOY_NEWPOVX = 16;
  JOY_NEWPOVY = 17;

type
  TJoysticks = class;

  { Internal class to provide different implementations of joystick events reading.
    @exclude }
  TJoysticksBackend = class abstract
    { Detect and add joysticks to given list. }
    procedure Initialize(const List: TJoystickList); virtual; abstract;
    { Update state of joysticks on given list. }
    procedure Poll(const List: TJoystickList;
      const EventContainer: TJoysticks); virtual; abstract;
  end;

  { Joystick axis move event. }
  TOnJoyAxisMove = procedure(const Joy: TJoystick; const Axis: Byte; const Value: Single) of object;
  { Joystick button action event. Used on button press/up/down. }
  TOnJoyButtonEvent = procedure(const Joy: TJoystick; const Button: Byte) of object;

  { TJoysticks is a class for joysticks and gamepads management }
  TJoysticks = class
  private
    FOnAxisMove: TOnJoyAxisMove;
    FOnButtonDown: TOnJoyButtonEvent;
    FOnButtonUp: TOnJoyButtonEvent;
    FOnButtonPress: TOnJoyButtonEvent;
    Backend: TJoysticksBackend;
    FInitialized: Boolean;
    FOnChange: TNotifyEvent;
    function IndexOf(const Joy: TJoystick): Integer;
    function GetItems(const Index: Integer): TJoystick;
    { Get (creating if necessary) joystick's explicit backend.
      Always returns TExplicitJoystickBackend, but cannot be declared as such. }
    function ExplicitBackend: TJoysticksBackend;
  protected
    FList: TJoystickList;
    { See OnChange. }
    procedure DoChange;
  public
    constructor Create;
    destructor Destroy; override;
    { Check state of every connected joystick and run event procedures.
      This is internal, called automatically by CastleUIControls unit,
      user code does not need to call this.
      @exclude }
    procedure InternalPoll;
    { @exclude }
    procedure Poll; deprecated 'do not call this, it is not necessary';

    function  GetInfo( JoyID : Byte ) : PJoyInfo;
    function  AxisPos( JoyID, Axis : Byte ): Single;
    function  Down( JoyID, Button : Byte ): Boolean;
    function  Up( JoyID, Button : Byte ): Boolean;
    function  Press( JoyID, Button : Byte ): Boolean;
    procedure ClearState;
    function GetJoy(const JoyID: Integer): TJoystick; deprecated 'use Joysticks[xxx] instead of Joysticks.GetJoy(xxx)';

    property OnAxisMove: TOnJoyAxisMove read FOnAxisMove write FOnAxisMove;
    property OnButtonDown: TOnJoyButtonEvent read FOnButtonDown write FOnButtonDown;
    property OnButtonUp: TOnJoyButtonEvent read FOnButtonUp write FOnButtonUp;
    property OnButtonPress: TOnJoyButtonEvent read FOnButtonPress write FOnButtonPress;
    function JoyCount: Integer; deprecated 'use Count';
    function Count: Integer;

    property Items[const Index: Integer]: TJoystick read GetItems; default;

    { Detect connected joysticks.
      On some platforms, you need to call this to search for connected joysticks,
      otherwise you will always have zero joysticks.
      Calling this again is allowed, it searches for connected joysticks again. }
    procedure Initialize; virtual;

    property Initialized: Boolean read FInitialized;

    { Used by CASTLE_WINDOW_LIBRARY when
      an external API notifies us about the joysticks state.
      @exclude }
    procedure InternalSetJoystickCount(const JoystickCount: Integer);
    { @exclude }
    procedure InternalSetJoystickAxis(const JoystickIndex: Integer; const Axis: TVector2);

    { Called after TJoystick instances on the list change (some are added, destroyed).
      In case of some backends, this is only called at the end of @link(Initialize),
      but it may be called in other cases (e.g. "explicit" joystick backend,
      used by Nintendo Switch, may call this at any moment). }
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

type
  TUiContainerList = specialize TObjectList<TUiContainer>;

type
  { Joystick event manager:
    - Reads joysticks layouts database;
    - Tries to autodetect initialized joysticks by name;
    - Converts joystick events to Press events or Axes events;
    - Generates "Fake" buttons events based on joystick axes data }
  TCastleJoysticks = class(TJoysticks)
  private //we need to access some of these properties in TFakeJoysitckEventHandler
    const
      { "Dead zone" of the joystick axes, that do not trigger button events
        Todo = remake it into a variable}
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
      private const
        DefaultFakeEventsPause = 0.3; { seconds // todo - remake into a variable }
        FakeEventSenderJoystick = 255;
      strict private
        LastFakeEventTimerX, LastFakeEventTimerY: TTimerResult;
        { A workaround being unable to set TTimerResult value directly }
        Never: TTimerResult;
        function SumAllAxes: TVector2;
        procedure PressKey(const AKey: TKey);
        procedure ReleaseKey(const AKey: TKey);
        procedure ReleaseXAxes;
        procedure ReleaseYAxes;
      public
        FakeEventsPause: Single;
        FakeEventsDelay: TFloatTime;
        IsActive: Boolean;
        procedure Update;
        procedure ReleaseAllFakeEvents;
        constructor Create; //override;
        destructor Destroy; override;
      end;
    var
      FakeEventsHandler: TFakeJoystickEventsHandler;

    { Current defaults are:
      Windows: 030000005e0400000a0b000000000000, Xbox Adaptive Controller
      Linux: 030000006f0e00001304000000010000, Generic X-Box pad
      otherwise: "minimalistic" (a joystick with only left stick) }
    function DefaultJoystickGuid: String;
    { Correctly send this joystick event to Container.Pressed or
      TJoystick.LeftAxis, TJoystick.RightAxis }
    procedure SendJoystickEvent(const Joy: TJoystick; const JEP: TJoystickEventPair; const Value: Single);

    procedure DoAxisMove(const Joy: TJoystick; const Axis: Byte; const Value: Single);
    //procedure DoButtonDown(const Joy: TJoystick; const Button: Byte);
    procedure DoButtonUp(const Joy: TJoystick; const Button: Byte);
    procedure DoButtonPress(const Joy: TJoystick; const Button: Byte);

    procedure SendPressEventToAllContainers(const AEvent: TInputPressRelease);
    procedure SendReleaseEventToAllContainers(const AEvent: TInputPressRelease);

    function GetGenerateFakeEvents: Boolean;
    procedure SetGenerateFakeEvents(const AValue: Boolean);
    function GetFakeEventsPause: TFloatTime;
    procedure SetFakeEventsPause(const AValue: TFloatTime);
  public
    { A list of Ui containers that receive joystick buttons press events
      every Ui container is automatically added on TUiContainer.Create }
    UiContainers: TUiContainerList;
    { Determines if the joystick layouts database freed immediately
      after the joysticks have been autodetected.
      If you need to propose the player to choose a joystick manually
      this value should be set to false
      Default: true }
    FreeJoysticksDatabaseAfterInitialization: Boolean;
    { Initialize the joysticks connected to the system
      and tries to autodetect joysticks layouts }
    procedure Initialize; override;
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

    { Set this to true to start generating "fake" buttons events by joysticks axes movement.
      If FakeJoystickEventsHandler is not going to be used for a long time
      GenerateFakeEvents should be set to "false" avoid Press-related bugs.
      Note, that FakeEvents is designed to handle only simple cases,
      such as menu navigation. }
    property GenerateFakeEvents: Boolean read GetGenerateFakeEvents write SetGenerateFakeEvents;
    { If using FakeEvents - call this every frame in Window.OnUpdate
      or inside any other object, that updates every frame, e.g. TUiState }
    procedure UpdateFakeEvents(Sender: TObject);
    { Delay between two sequential "fake" press events generated,
      I.e. in case the player holds axis downwards for some time,
      the fake events will be generated every FakeEventsPause seconds }
    property FakeEventsPause: TFloatTime read GetFakeEventsPause write SetFakeEventsPause;

    constructor Create; //override;
    destructor Destroy; override;
  end;

{ Detect connected joysticks. }
procedure EnableJoysticks; deprecated 'use Joysticks.Initialize';

{ Global joystick manager object. Singleton, automatically created when being accessed.
  Remember to call @link(TJoysticks.Initialize), this is necessary on some platforms
  to detect joysticks. }
function Joysticks: TCastleJoysticks;

implementation

{$ifdef LINUX}{$define DatabaseLoaded}{$endif}
{$ifdef MSWINDOWS}{$define DatabaseLoaded}{$endif}

uses SysUtils, Math,
  CastleLog, CastleUtils,
  {$ifdef LINUX} CastleInternalJoysticksLinux, {$endif}
  {$ifdef MSWINDOWS} CastleInternalJoysticksWindows, {$endif}
  {$ifdef LINUX}CastleInternalJoystickDatabaseLinux, {$endif}
  {$ifdef MSWINDOWS}CastleInternalJoystickDatabaseWindows, {$endif}
  {$ifndef DatabaseLoaded} CastleInternalJoystickDatabaseGeneric, {$endif}
  CastleInternalJoysticksExplicit;

{ TJoystick ------------------------------------------------------------------ }

destructor TJoystick.Destroy;
begin
  FreeAndNil(InternalBackendInfo);
  FreeAndNil(FLayout);
  inherited;
end;

procedure TJoystick.SetLayout(const NewLayout: TJoystickLayout);
begin
  FreeAndNil(FLayout);
  FLayout := NewLayout;
end;

function TJoystick.Axis: TVector2;
begin
  Result := LeftAxis;
  {Result := Vector2(
    State.Axis[JOY_AXIS_X],
    State.Axis[JOY_AXIS_Y]
  );}
end;

{ TJoysticks ----------------------------------------------------------------- }

constructor TJoysticks.Create;
begin
  inherited;
  FList := TJoystickList.Create(true);
  {$if defined(MSWINDOWS)}
  Backend := TWindowsJoysticksBackend.Create;
  {$elseif defined(LINUX)}
  Backend := TLinuxJoysticksBackend.Create;
  {$else}
  // This way Backend is non-nil always
  Backend := TExplicitJoystickBackend.Create;
  {$endif}
end;

destructor TJoysticks.Destroy;
begin
  FreeAndNil(FList);
  FreeAndNil(Backend);
  inherited;
end;

procedure TJoysticks.Initialize;
begin
  FInitialized := true;

  { In case of TExplicitJoystickBackend,
    do not clear the list and call Backend.Initialize.
    Instead leave existing joysticks (set by TExplicitJoystickBackend.SetJoystickCount).
    That's because Backend.Initialize doesn't have a way to get new joystick information
    (in case of TExplicitJoystickBackend, it is CGE that is informed by external code
    about joystick existence). }
  if Backend is TExplicitJoystickBackend then
    Exit;

  FList.Clear;
  Backend.Initialize(FList);
  ClearState;
  DoChange;
end;

procedure TJoysticks.Poll;
begin
  InternalPoll;
end;

procedure TJoysticks.InternalPoll;
begin
  if FInitialized then
    Backend.Poll(FList, Self);
end;

function TJoysticks.GetInfo(JoyID: Byte): PJoyInfo;
begin
  Result := nil;
  if JoyID >= Count then Exit;

  Result := @(FList[JoyID].Info);
end;

function TJoysticks.AxisPos(JoyID, Axis: Byte): Single;
begin
  Result := 0;
  if ( JoyID >= Count ) or ( Axis > JOY_POVY ) then Exit;

  Result := FList[JoyID].State.Axis[ Axis ];
end;

function TJoysticks.Down(JoyID, Button: Byte): Boolean;
begin
  Result := False;
  if ( JoyID >= Count ) or ( Button >= FList[JoyID].Info.Count.Buttons ) then Exit;

  Result := FList[JoyID].State.BtnDown[ Button ];
end;

function TJoysticks.Up(JoyID, Button: Byte): Boolean;
begin
  Result := False;
  if ( JoyID >= Count ) or ( Button >= FList[JoyID].Info.Count.Buttons ) then Exit;

  Result := FList[JoyID].State.BtnUp[ Button ];
end;

function TJoysticks.Press(JoyID, Button: Byte): Boolean;
begin
  Result := False;
  if ( JoyID >= Count ) or ( Button >= FList[JoyID].Info.Count.Buttons ) then Exit;

  Result := FList[JoyID].State.BtnPress[ Button ];
end;

procedure TJoysticks.ClearState;
var
  i, j  : Integer;
  state : PJoyState;
begin
  for i := 0 to Count - 1 do
    for j := 0 to FList[I].Info.Count.Buttons - 1 do
      begin
        state := @FList[I].State;
        state^.BtnUp[ j ]       := False;
        state^.BtnDown[ j ]     := False;
        state^.BtnPress[ j ]    := False;
        state^.BtnCanPress[ j ] := True;
      end;
end;

function TJoysticks.GetJoy(const JoyID: Integer): TJoystick;
begin
  Result := nil;
  if JoyID >= Count then Exit;
  Result := FList[JoyID];
end;

function TJoysticks.ExplicitBackend: TJoysticksBackend;
begin
  Assert(Backend <> nil);
  if not (Backend is TExplicitJoystickBackend) then
  begin
    FreeAndNil(Backend);
    Backend := TExplicitJoystickBackend.Create;
    { Although TExplicitJoystickBackend.Initialize doesn't do anything for now,
      but call it, to make sure Initialized = true. }
    Initialize;
  end;
  Result := Backend;
end;

procedure TJoysticks.InternalSetJoystickCount(const JoystickCount: Integer);
begin
  TExplicitJoystickBackend(ExplicitBackend).SetJoystickCount(FList, JoystickCount);
  DoChange;
end;

procedure TJoysticks.InternalSetJoystickAxis(const JoystickIndex: Integer; const Axis: TVector2);
begin
  TExplicitJoystickBackend(ExplicitBackend).SetJoystickAxis(FList, JoystickIndex, Axis);
end;

function TJoysticks.GetItems(const Index: Integer): TJoystick;
begin
  Result := FList[Index];
end;

function TJoysticks.JoyCount: Integer;
begin
  Result := FList.Count;
end;

function TJoysticks.Count: Integer;
begin
  Result := FList.Count;
end;

procedure TJoysticks.DoChange;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

function TJoysticks.IndexOf(const Joy: TJoystick): Integer;
begin
  Result := FList.IndexOf(Joy);
end;

{ temporary: to-do: move to CastleKeysMouse}

function InputJoystick(const Key: TKey; const JoystickIndex: Byte): TInputPressRelease;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.Position := TVector2.Zero;
  Result.EventType := itJoystick;
  Result.Key := Key;
  Result.ModifiersDown := [];
  Result.KeyString := '';
  Result.FingerIndex := JoystickIndex;
end;

{ TJoystickManager ---------------------------------------------------------}

procedure TCastleJoysticks.SendPressEventToAllContainers(const AEvent: TInputPressRelease);
var
  C: TUiContainer;
begin
  for C in UiContainers do
  begin
    C.EventPress(AEvent);
    C.Pressed.KeyDown(AEvent.Key, AEvent.KeyString);
  end;
end;

procedure TCastleJoysticks.SendReleaseEventToAllContainers(const AEvent: TInputPressRelease);
var
  C: TUiContainer;
  UnusedStringVariable: String;
begin
  for C in UiContainers do
    if C.Pressed[AEvent.Key] then
    begin
      C.EventRelease(AEvent);
      C.Pressed.KeyUp(AEvent.Key, UnusedStringVariable);
    end;
end;

procedure TCastleJoysticks.SendJoystickEvent(const Joy: TJoystick; const JEP: TJoystickEventPair; const Value: Single);

  function KeyToStr(const AKey: TKey): String;
  begin
    WriteStr(Result, AKey);
  end;

  function JoystickEventToKey(const AJoystickEvent: TJoystickEvent): TKey;
  begin
    case AJoystickEvent of
      padA: Result := joySouth;
      padB: Result := joyEast;
      padY: Result := joyNorth;
      padX: Result := joyWest;
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
    ButtonEvent: TInputPressRelease;
  begin
    ButtonEvent := InputJoystick(JoystickEventToKey(JEP.Primary), IndexOf(Joy));
    if Abs(AValue) > JoystickEpsilon then
      SendPressEventToAllContainers(ButtonEvent)
    else
    begin
      SendReleaseEventToAllContainers(ButtonEvent);
      if JEP.Inverse <> JEP.Primary then
      begin
        ButtonEvent := InputJoystick(JoystickEventToKey(JEP.Inverse), IndexOf(Joy));
        SendReleaseEventToAllContainers(ButtonEvent);
      end;
    end;
  end;

  //todo: optimize?
  procedure JoystickLeftXAxis(const AValue: Single);
  begin
    Joy.LeftAxis := Vector2(AValue, Joy.LeftAxis.Y);
  end;
  procedure JoystickLeftYAxis(const AValue: Single);
  begin
    { Y axes should be 1 when pointing up, -1 when pointing down.
      This is consistent with CGE 2D coordinate system
      (and standard math 2D coordinate system). }
    Joy.LeftAxis := Vector2(Joy.LeftAxis.X, -AValue);
  end;
  procedure JoystickRightXAxis(const AValue: Single);
  begin
    Joy.RightAxis := Vector2(AValue, Joy.RightAxis.Y);
  end;
  procedure JoystickRightYAxis(const AValue: Single);
  begin
    { Y axes should be 1 when pointing up, -1 when pointing down.
      This is consistent with CGE 2D coordinate system
      (and standard math 2D coordinate system). }
    Joy.RightAxis := Vector2(Joy.RightAxis.X, -AValue);
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
  JEP: TJoystickEventPair;
begin
  if (Axis <> JOY_NEWPOVX) and (Axis <> JOY_NEWPOVY) then //if event is not D-Pad
  begin
    JEP := Joy.Layout.AxisEvent(Axis, Value);
    if JEP.Primary <> unknownAxisEvent then
    begin
      if (Joy.Layout.InvertAxis(Axis)) then
        SendJoystickEvent(Joy, JEP, -Value)
      else
        SendJoystickEvent(Joy, JEP, Value);
    end else
      WriteLnLog('Warning', Format('Unknown "%s" (detected as "%s") joystick event at axis [%s]',
        [Joy.TrimmedName,
         Joy.Layout.JoystickName, IntToStr(Axis)]));
  end else
  begin
    JEP := Joy.Layout.DPadEvent(Axis, Value);
    if JEP.Primary <> unknownAxisEvent then
      SendJoystickEvent(Joy, JEP, Value)
    else
      WriteLnLog('Warning', Format('Unknown "%s" (detected as "%s") joystick event at D-Pad axis [%s]',
        [Joy.TrimmedName, Joy.Layout.JoystickName, IntToStr(Axis)]));
  end;
end;
{procedure TCastleJoysticks.DoButtonDown(const Joy: TJoystick; const Button: Byte);
var
  JE: TJoystickEvent;
begin
  JE := Joy.Layout.ButtonEvent(Button);
  if JE <> unknownButtonEvent then
    SendJoystickEvent(Joy, JE, 1.0)
  else
    WriteLnLog('Warning', Format('Unknown "%s" (detected as "%s") joystick event at button [%s]',
      [Joy.TrimmedName, Joy.Layout.JoystickName, IntToStr(Button)]));
end;}
procedure TCastleJoysticks.DoButtonPress(const Joy: TJoystick; const Button: Byte);
var
  JEP: TJoystickEventPair;
begin
  JEP.Primary := Joy.Layout.ButtonEvent(Button);
  JEP.Inverse := JEP.Primary;
  if JEP.Primary <> unknownButtonEvent then
    SendJoystickEvent(Joy, JEP, 1.0)
  else
    WriteLnLog('Warning', Format('Unknown "%s" (detected as "%s") joystick event at button [%s]',
      [Joy.TrimmedName, Joy.Layout.JoystickName, IntToStr(Button)]));
end;
procedure TCastleJoysticks.DoButtonUp(const Joy: TJoystick; const Button: Byte);
var
  JEP: TJoystickEventPair;
begin
  JEP.Primary := Joy.Layout.ButtonEvent(Button);
  JEP.Inverse := JEP.Primary;
  if JEP.Primary <> unknownButtonEvent then
    SendJoystickEvent(Joy, JEP, 0.0)
  else
    WriteLnLog('Warning', Format('Unknown "%s" (detected as "%s") joystick event at button [%s]',
      [Joy.TrimmedName, Joy.Layout.JoystickName, IntToStr(Button)]));
end;

function TCastleJoysticks.DefaultJoystickGuid: String;
begin
  {$ifdef Windows}
  Result := '030000005e0400000a0b000000000000';
  {$endif}
  {$ifdef Linux}
  Result := '030000006f0e00001304000000010000';
  {$endif}

  //this is a "minimalistic" joystick used on non Windows/Linux systems
  {$ifndef DatabaseLoaded}
  Result := 'minimalistic';
  {$endif}
end;

procedure TCastleJoysticks.Initialize;

  function TrimJoystickName(const AJoystickName: String): String;
  begin
    Result := Trim(AJoystickName);
    while Pos('  ', Result) > 0 do
      Result := StringReplace(Result, '  ', ' ', [rfReplaceAll]);
  end;

var
  I: Integer;
  Joy: TJoystick;
  JL: TJoystickLayout;
  JoyName: String;
begin
  InitializeDatabase;

  inherited;

  OnAxisMove := @DoAxisMove;
  //OnButtonDown := @DoButtonDown;
  OnButtonUp := @DoButtonUp;
  OnButtonPress := @DoButtonPress;

  for I := 0 to Pred(Count) do
  begin
    Joy := FList[I];
    WriteLnLog('Joystick Name', Joy.Info.Name);
    WriteLnLog('Joystick Buttons', IntToStr(Joy.Info.Count.Buttons));
    WriteLnLog('Joystick Axes', IntToStr(Joy.Info.Count.Axes));
    WriteLnLog('Joystick Caps', IntToStr(Joy.Info.Caps));

    //try autodetect the joystick by GUID
    //if autodetect by GUID failed then try
    JoyName := TrimJoystickName(Joy.Info.Name);
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
    Joy.Layout := JL;
    Joy.TrimmedName := JoyName;

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
  if not Initialized then
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
        Joy.Layout := NewJoystickLayout;
        if not NewJoystickLayout.BuggyDuplicateName then
          WriteLnLog(Format('Joystick "%s" layout has been successfully changed to "%s".', [Joy.TrimmedName, NewJoystickLayout.JoystickName]))
        else
          WriteLnLog(Format('Warning: Joystick "%s" layout has been successfully changed to "%s". However, there are several different layouts for this joystick name in the databse.', [Joy.TrimmedName, NewJoystickLayout.JoystickName]));
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
  if not Initialized then
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
        Joy.Layout := NewJoystickLayout;
        WriteLnLog(Format('Joystick "%s" layout has been successfully changed to "%s".', [Joy.TrimmedName, NewJoystickLayout.JoystickName]));
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

procedure TCastleJoysticks.UpdateFakeEvents(Sender: TObject);
begin
  if GenerateFakeEvents then
    FakeEventsHandler.Update;
end;

function TCastleJoysticks.GetFakeEventsPause: TFloatTime;
begin
  if FakeEventsHandler <> nil then
    Result := FakeEventsHandler.FakeEventsPause
  else
    Result := -1;
end;
procedure TCastleJoysticks.SetFakeEventsPause(const AValue: TFloatTime);
begin
  if FakeEventsHandler = nil then
    FakeEventsHandler := TFakeJoystickEventsHandler.Create;
  FakeEventsHandler.FakeEventsPause := AValue;
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
  UiContainers := TUiContainerList.Create(false);
  ApplicationProperties.OnUpdate.Add(@UpdateFakeEvents);
end;

destructor TCastleJoysticks.Destroy;
begin
  FreeAndNil(FakeEventsHandler);
  FreeAndNil(UiContainers);
  inherited;
end;

{ TCastleJoysticks.TFakeJoystickEventsHandler --------------------------------}

function TCastleJoysticks.TFakeJoystickEventsHandler.SumAllAxes: TVector2;
var
  I: Integer;
begin
  Result := TVector2.Zero;
  for I := 0 to Pred(Joysticks.Count) do
  begin
    Result += Joysticks[I].LeftAxis;
    Result += Joysticks[I].RightAxis;
  end;
  Result.Normalize;
end;

procedure TCastleJoysticks.TFakeJoystickEventsHandler.Update;
var
  FakeAxes: TVector2;
begin
  { We have no smarter idea than simply to sum both left and right axes
    of all available joysticks to generate fake events.
    Note, that this approach is fail-proof, in case player has several
    joysticks connected to the computer, or prefers to use different axes
    for menu navigation.
    This issue has to be addressed differently in hot-seat multiplayer,
    however, TFakeJoystickEventsHandler was only designed to handle simple menus }
  FakeAxes := SumAllAxes;

  if Abs(FakeAxes.X) < Joysticks.JoystickEpsilon then
    ReleaseXAxes
  else
  begin
    if TimerSeconds(Timer, LastFakeEventTimerX) > FakeEventsPause then
    begin
      if FakeAxes.X > 0 then
      begin
        ReleaseKey(joyFakeLeft);
        ReleaseKey(joyFakeRight);
        PressKey(joyFakeRight);
      end else
      begin
        ReleaseKey(joyFakeLeft);
        ReleaseKey(joyFakeRight);
        PressKey(joyFakeLeft);
      end;
      LastFakeEventTimerX := Timer;
    end;
  end;

  if Abs(FakeAxes.Y) < Joysticks.JoystickEpsilon then
    ReleaseYAxes
  else
  begin
    if TimerSeconds(Timer, LastFakeEventTimerY) > FakeEventsPause then
    begin
      if FakeAxes.Y > 0 then
      begin
        ReleaseKey(joyFakeUp);
        ReleaseKey(joyFakeDown);
        PressKey(joyFakeUp);
      end else
      begin
        ReleaseKey(joyFakeUp);
        ReleaseKey(joyFakeDown);
        PressKey(joyFakeDown);
      end;
      LastFakeEventTimerY := Timer;
    end;
  end;
end;

procedure TCastleJoysticks.TFakeJoystickEventsHandler.ReleaseAllFakeEvents;
begin
  ReleaseXAxes;
  ReleaseYAxes;
end;

procedure TCastleJoysticks.TFakeJoystickEventsHandler.PressKey(const AKey: TKey);
var
  ButtonEvent: TInputPressRelease;
begin
  ButtonEvent := InputJoystick(AKey, FakeEventSenderJoystick);
  Joysticks.SendPressEventToAllContainers(ButtonEvent);
end;

procedure TCastleJoysticks.TFakeJoystickEventsHandler.ReleaseKey(const AKey: TKey);
var
  ButtonEvent: TInputPressRelease;
begin
  ButtonEvent := InputJoystick(AKey, FakeEventSenderJoystick);
  Joysticks.SendReleaseEventToAllContainers(ButtonEvent);
end;

procedure TCastleJoysticks.TFakeJoystickEventsHandler.ReleaseXAxes;
begin
  LastFakeEventTimerX := Never;
  ReleaseKey(joyFakeLeft);
  ReleaseKey(joyFakeRight);
end;

procedure TCastleJoysticks.TFakeJoystickEventsHandler.ReleaseYAxes;
begin
  LastFakeEventTimerY := Never;
  ReleaseKey(joyFakeUp);
  ReleaseKey(joyFakeDown);
end;

constructor TCastleJoysticks.TFakeJoystickEventsHandler.Create;
begin
  inherited; //parent is empty
  IsActive := true;
  Never := Timer;
  FakeEventsPause := DefaultFakeEventsPause;
end;

destructor TCastleJoysticks.TFakeJoystickEventsHandler.Destroy;
begin
  { Normally any problem shouldn't happen and we should free TFakeJoystickEventsHandler
    only in case finalize - and that means we're working with half-freed
    instances and therefore SIGSEGVs }
  //ReleaseAllFakeEvents;
  inherited;
end;

{ global --------------------------------------------------------------------- }

procedure EnableJoysticks;
begin
  Joysticks.Initialize;
end;

var
  FJoysticks: TCastleJoysticks;

function Joysticks: TCastleJoysticks;
begin
  if FJoysticks = nil then
    FJoysticks := TCastleJoysticks.Create;
  Result := FJoysticks;
end;

finalization
  FreeAndNil(FJoysticks);
end.
