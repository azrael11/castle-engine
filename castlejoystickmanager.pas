unit CastleJoystickManager;

{TODO:

 * TJoyState has max 8 axes [0..7], however, database has as much as 14 axes \
 * I have no idea what "~" means in the database for axes, simply removing the symbol
 * Data[0] is GUID and we can't use it at the moment, however, maybe we can get it from backend?
   it seems like this is a joystick driver-specific value
 * Note that in Windows we get a wrong reported joystick name as Microsoft PC-joystick driver
   while the joystick name reported by SDL2 is G-Shark GS-GP702 and the true name is Esperanza EG102
 * Defatul X-Box like gamepad layout
 * What is the difference between ButtonPress and ButtonDown events?

}

interface

uses
  SysUtils, Classes, Generics.Collections,
  CastleJoysticks;

type
  TJoystickEvent = (
    { The event was not detected, in other words, it means an error }
    jeNone,

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
    hatLeft, hatRight, hatUp, hatDown,

    { X-Box button }
    buttonGuide

    );

type
  TJoystickDictionary = specialize TDictionary<Integer, TJoystickEvent>;

type
  TJoystickRecord = class
  protected
    { GUID of the joystick in the database, unused for now }
    Guid: String;
    Buttons, AxesPlus, AxesMinus, Hats: TJoystickDictionary;
    function StrToJoystickEvent(const AString: String): TJoystickEvent;
  public
    { Platform of the database report }
    Platform: String;
    JoystickName: String;
    function IsJoystickName(const AName: String): Boolean;
    procedure Parse(const AString: String);
    constructor Create; //override;
    destructor Destroy; override;
  end;

  TJoystickRecordsList = specialize TObjectList<TJoystickRecord>;

procedure ParseJoysticksDatabase(const URL: String);

implementation
uses
  CastleLog, CastleDownload, CastleStringUtils;

constructor TJoystickRecord.Create;
begin
  inherited; //parent is empty
  Buttons := TJoystickDictionary.Create;
  AxesPlus := TJoystickDictionary.Create;
  AxesMinus := TJoystickDictionary.Create;
  Hats := TJoystickDictionary.Create;
end;

destructor TJoystickRecord.Destroy;
begin
  FreeAndNil(Buttons);
  FreeAndNil(AxesPlus);
  FreeAndNil(AxesMinus);
  FreeAndNil(Hats);
  inherited;
end;

function TJoystickRecord.IsJoystickName(const AName: String): Boolean;
begin
  //we might have additional information in JoystickName
  Result := Pos(AName, JoystickName) > 0;
end;

function TJoystickRecord.StrToJoystickEvent(const AString: String): TJoystickEvent;
begin
  Result := jeNone; //this is an "error"
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

    'dpleft': Result := hatLeft;
    'dpright': Result := hatRight;
    'dpup': Result := hatUp;
    'dpdown': Result := hatDown;

    '+leftx': Result := axisLeftXPlus;
    '-leftx': Result := axisLeftXMinus;
    '+lefty': Result := axisLeftYPlus;
    '-lefty': Result := axisLeftYMinus;
    '+righty': Result := axisRightYPlus;
    '-righty': Result := axisRightYMinus;

    'guide': Result := buttonGuide;
    else
      WriteLnLog('Unknown joystick JoystickEvent', AString);
  end;
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
      vtButton, vtBothAxes, vtHat,
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
      'h0.': Result.ValueType := vtHat;
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
            vtHat: Hats.AddOrSetValue(VT.Value, StrToJoystickEvent(Pair.Caption)); //note, we don't have access to hats.
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

procedure ParseJoysticksDatabase(const URL: String);
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
  I: Integer;
  Rec: TJoystickRecord;
begin
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
          FreeAndNil(Rec) //add it to list
        else
          FreeAndNil(Rec);
      end;
    FreeAndNil(Strings);
  finally
    FreeAndNil(Stream);
  end;
end;

end.

