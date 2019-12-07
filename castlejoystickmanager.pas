unit CastleJoystickManager;

interface

uses
  SysUtils, Classes, Generics.Collections,
  CastleWindow, CastleVectors, CastleJoysticks,
  CastleInternalJoystickRecord;

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
begin
  {case JE of
    else
      ;
  end;}
  SayJoystickEvent(Joy.Info.Name, JoystickEventToStr(JE), JE, Value);
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

