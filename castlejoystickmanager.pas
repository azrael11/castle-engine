unit CastleJoystickManager;

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

    WriteLnLog('--------------------');
  end;

  { we shouould free databases here, however, they own joystick records,
    i.e. first we should make copies }
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

