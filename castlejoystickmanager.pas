unit CastleJoystickManager;

{TODO:

 * TJoyState has max 8 axes [0..7], however, database has as much as 14 axes \
 * I have no idea what "~" means in the database for axes, simply removing the symbol
 * Data[0] is GUID and we can't use it at the moment, however, maybe we can get it from backend?
   it seems like this is a joystick driver-specific value
 * [CRITICAL] Note that in Windows we get a wrong reported joystick name as Microsoft PC-joystick driver
   while the joystick name reported by SDL2 is G-Shark GS-GP702 and the true name is Esperanza EG102
 * Defatul X-Box like gamepad layout
 * What is the difference between ButtonPress and ButtonDown events?
 * Button press/release based on Value for axis-driven buttons

}

interface

uses
  SysUtils, Classes, Generics.Collections,
  CastleJoysticks;

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
  protected
    { GUID of the joystick in the database, unused for now }
    Guid: String;
    Buttons, AxesPlus, AxesMinus, DPad: TJoystickDictionary;
    function StrToJoystickEvent(const AString: String): TJoystickEvent;
  public
    { Platform of the database report }
    Platform: String;
    JoystickName: String;
    function IsJoystickName(const AName: String): Boolean;
    function AxisEvent(const AxisID: Byte; const AxisValue: Single): TJoystickEvent;
    function ButtonEvent(const ButtonID: Byte): TJoystickEvent;
    function JoystickEventToStr(const Event: TJoystickEvent): String;
    procedure Parse(const AString: String);
    constructor Create; //override;
    destructor Destroy; override;
  end;

  TJoystickDatabase = specialize TObjectDictionary<TJoystick, TJoystickRecord>;

type
  TCastleJoystickManager = class
  strict private
    Database: TJoystickDatabase;
    FDefaultJoystickRecord: TJoystickRecord;
    function GetJoystickRecord(const Joy: TJoystick): TJoystickRecord; inline;
    procedure SayJoystickEvent(const Joy: TJoystick; const Prefix: String; const JE: TJoystickEvent; const Value: Single = 0);
    function DefaultJoystickRecord: TJoystickRecord;
  public
    procedure DoAxisMove(const Joy: TJoystick; const Axis: Byte; const Value: Single);
    procedure DoButtonDown(const Joy: TJoystick; const Button: Byte);
    procedure DoButtonUp(const Joy: TJoystick; const Button: Byte);
    procedure DoButtonPress(const Joy: TJoystick; const Button: Byte);
    procedure ParseJoysticksDatabase(const URL: String);
    destructor Destroy; override;
  end;

function JoystickManager: TCastleJoystickManager;
implementation
uses
  CastleLog, CastleDownload, CastleStringUtils;

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

function TJoystickRecord.StrToJoystickEvent(const AString: String): TJoystickEvent;
begin
  Result := unknownEvent; //this is an "error"
  case AString of
    'x': Result := padX; //note the mapping of ABYX is different for PC!
    'y': Result := padY;
    'a': Result := padA;
    'b': Result := padB;

    'back': Result := buttonBack;
    'start': Result := buttonStart;

    'rightshoulder': Result := buttonRightShoulder;
    'leftshoulder': Result := buttonLeftShoulder;
    'righttrigger': Result := buttonRightTrigger;
    'lefttrigger': Result := buttonLeftTrigger;

    'leftx': Result := axisLeftX;
    'lefty': Result := axisLeftY;
    'rightx': Result := axisRightX;
    'righty': Result := axisRightY;
    'leftstick': Result := buttonLeftStick;
    'rightstick': Result := buttonRightStick;

    'dpleft': Result := dpadLeft;
    'dpright': Result := dpadRight;
    'dpup': Result := dpadUp;
    'dpdown': Result := dpadDown;

    '+leftx': Result := axisLeftXPlus;
    '-leftx': Result := axisLeftXMinus;
    '+lefty': Result := axisLeftYPlus;
    '-lefty': Result := axisLeftYMinus;
    '+righty': Result := axisRightYPlus;
    '-righty': Result := axisRightYMinus;

    'guide': Result := buttonGuide;
    else
      WriteLnLog('ERROR: Unknown joystick JoystickEvent', AString);
  end;
end;

function TJoystickRecord.JoystickEventToStr(const Event: TJoystickEvent): String;
begin
  WriteStr(Result, Event);
end;

procedure TJoystickRecord.Parse(const AString: String);
  type
    TStringPair = record
      Caption: String;
      Value: String;
    end;
  function SplitStringPair(const AString: String): TStringPair;
  var
    SemicolonPosition: Integer;
  begin
    SemicolonPosition := Pos(':', AString);
    Result.Caption := Copy(AString, 1, SemicolonPosition - 1);
    Result.Value := Copy(AString, SemicolonPosition + 1, Length(AString));
  end;
  type
    TValueType = (
      vtButton, vtBothAxes, vtDPad,
      vtAxisPlus, vtAxisMinus);
    TParsedValue = record
      ValueType: TValueType;
      Value: Integer;
    end;
  function ParseValueType(const AString: String): TParsedValue;
  var
    Prefix: String;
    Value: String;
  begin
    if Copy(AString, 1, 1) = 'a' then
      Prefix := 'a'
    else
    if Copy(AString, 1, 1) = 'b' then
      Prefix := 'b'
    else
    if Copy(AString, 1, 3) = 'h0.' then
      Prefix := 'h0.'
    else
    if Copy(AString, 1, 2) = '+a' then
      Prefix := '+a'
    else
    if Copy(AString, 1, 2) = '-a' then
      Prefix := '-a'
    else
      ;

    case Prefix of
      'a': Result.ValueType := vtBothAxes;
      'b': Result.ValueType := vtButton;
      'h0.': Result.ValueType := vtDPad;
      '+a': Result.ValueType := vtAxisPlus;
      '-a': Result.ValueType := vtAxisMinus;
      else
        WriteLnLog('Unexpected prefix for joystick value', AString);
    end;
    Value := Copy(AString, Length(Prefix) + 1, Length(AString));
    if Pos('~', Value) > 0 then
      Value := Copy(Value, 1, Pos('~', Value) - 1);
    Result.Value := StrToInt(Value);
  end;
var
  Data: TStringList;
  J: Integer;
  Pair: TStringPair;
  VT: TParsedValue;
begin
  Data := CreateTokens(AString, [',']);
  begin
    Guid := Data[0];
    JoystickName := Data[1];
    for J := 2 to Pred(Data.Count) do
      if Data[J] <> '' then
      begin
        Pair := SplitStringPair(Data[J]);
        if Pair.Caption = 'platform' then
          Platform := Pair.Value
        else
        begin
          VT := ParseValueType(Pair.Value);
          case VT.ValueType of
            //warning! we have duplicates!
            vtButton: Buttons.AddOrSetValue(VT.Value, StrToJoystickEvent(Pair.Caption));
            vtDPad: DPad.AddOrSetValue(VT.Value, StrToJoystickEvent(Pair.Caption)); //note, we don't have access to D-pads.
            vtBothAxes: begin
                          AxesPlus.AddOrSetValue(VT.Value, StrToJoystickEvent(Pair.Caption));
                          AxesMinus.AddOrSetValue(VT.Value, StrToJoystickEvent(Pair.Caption));
                        end;
            vtAxisPlus: AxesPlus.AddOrSetValue(VT.Value, StrToJoystickEvent(Pair.Caption));
            vtAxisMinus: AxesMinus.AddOrSetValue(VT.Value, StrToJoystickEvent(Pair.Caption));
          end;
        end;
      end;
  end;
  FreeAndNil(Data);
end;

{ TJoystickManager ---------------------------------------------------------}

function TCastleJoystickManager.GetJoystickRecord(const Joy: TJoystick): TJoystickRecord; inline;
begin
  if not Database.TryGetValue(Joy, Result) then
    Result := DefaultJoystickRecord;
end;

procedure TCastleJoystickManager.SayJoystickEvent(const Joy: TJoystick; const Prefix: String; const JE: TJoystickEvent; const Value: Single = 0);
begin
  if JE in AxisEvents then
    WriteLnLog(Joy.Info.Name, Prefix + ':' + FloatToStr(Value))
  else
    WriteLnLog(Joy.Info.Name, Prefix);
end;

procedure TCastleJoystickManager.DoAxisMove(const Joy: TJoystick; const Axis: Byte; const Value: Single);
var
  R: TJoystickRecord;
  JE: TJoystickEvent;
begin
  R := GetJoystickRecord(Joy);
  JE := R.AxisEvent(Axis, Value);
  SayJoystickEvent(Joy, R.JoystickEventToStr(JE), JE, Value);
end;
procedure TCastleJoystickManager.DoButtonDown(const Joy: TJoystick; const Button: Byte);
var
  R: TJoystickRecord;
  JE: TJoystickEvent;
begin
  R := GetJoystickRecord(Joy);
  JE := R.ButtonEvent(Button);
  SayJoystickEvent(Joy, R.JoystickEventToStr(JE), JE);
end;
procedure TCastleJoystickManager.DoButtonUp(const Joy: TJoystick; const Button: Byte);
var
  R: TJoystickRecord;
  JE: TJoystickEvent;
begin
  R := GetJoystickRecord(Joy);
  JE := R.ButtonEvent(Button);
  SayJoystickEvent(Joy, R.JoystickEventToStr(JE), JE);
end;
procedure TCastleJoystickManager.DoButtonPress(const Joy: TJoystick; const Button: Byte);
var
  R: TJoystickRecord;
  JE: TJoystickEvent;
begin
  R := GetJoystickRecord(Joy);
  JE := R.ButtonEvent(Button);
  SayJoystickEvent(Joy, R.JoystickEventToStr(JE), JE);
end;

function TCastleJoystickManager.DefaultJoystickRecord: TJoystickRecord;
begin
  if FDefaultJoystickRecord = nil then
  begin
    FDefaultJoystickRecord := TJoystickRecord.Create;
    FDefaultJoystickRecord.Parse('03000000790000000600000010010000,Microntek USB Joystick,a:b2,b:b1,x:b3,y:b0,back:b8,start:b9,leftshoulder:b4,rightshoulder:b5,dpup:h0.1,dpdown:h0.4,dpleft:h0.8,dpright:h0.2,leftx:a0,lefty:a1,rightx:a2,righty:a3,lefttrigger:b6,righttrigger:b7,rightstick:b11,leftstick:b10,platform:Linux,');
  end;
  Result := FDefaultJoystickRecord;
end;


procedure TCastleJoystickManager.ParseJoysticksDatabase(const URL: String);
const
  CurrentPlatform =
    {$IFDEF Windows}'Windows'{$ENDIF}
    //{$IFDEF MacOS}'Mac OS X'{$ENDIF}
    {$IFDEF Linux}'Linux'{$ENDIF}
    //{$IFDEF Android}'Android'{$ENDIF}
    //{$IFDEF IOS}'iOS'{$ENDIF}
    ;
var
  Stream: TStream;
  Strings: TStringList;
  I, J: Integer;
  Rec: TJoystickRecord;
begin
  if not Joysticks.Initialized then
    Joysticks.Initialize;
  if Database = nil then
    Database := TJoystickDatabase.Create
  else
    Database.Clear;

  try
    Stream := Download(URL);
    Strings := TStringList.Create;
    Strings.LoadFromStream(Stream);
    for I := 0 to Pred(Strings.Count) do
      if (Strings[I] <> '') and (Copy(Strings[I], 0, 1) <> '#') then
      begin
        Rec := TJoystickRecord.Create;
        Rec.Parse(Strings[I]);
        if Rec.Platform = CurrentPlatform then
        begin
          for J := 0 to Pred(Joysticks.Count) do
            if Rec.IsJoystickName(JOysticks[J].Info.Name) then
            begin
              Database.AddOrSetValue(JOysticks[J], Rec);
              Break;
            end else
              FreeAndNil(Rec);
        end else
          FreeAndNil(Rec);
      end;
    FreeAndNil(Strings);
  finally
    FreeAndNil(Stream);
  end;
end;

destructor TCastleJoystickManager.Destroy;
begin
  FreeAndNil(FDefaultJoystickRecord);
  FreeAndNil(Database);
  inherited;
end;

{------------------------------------------------------------------------}

var
  FJoystickManager: TCastleJoystickManager;

function JoystickManager: TCastleJoystickManager;
begin
  if FJoystickManager = nil then
    FJoystickManager := TCastleJoystickManager.Create;
  Result := FJoystickManager;
end;

finalization
  FreeAndNil(FJoystickManager);
end.

