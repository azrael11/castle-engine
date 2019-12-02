program CastleInternalJoystickDatabaseConverter;

uses
  Classes, SysUtils, Generics.Collections,
  CastleDownload, CastleLog,
  CastleInternalJoystickRecord;

type
  TJoystickParser = class(TJoystickRecord)
  protected
    function StrToJoystickEvent(const AString: String): TJoystickEvent;
  public
    { Platform of the database report }
    Platform: String;
    procedure Parse(const AString: String);
  end;

function TJoystickParser.StrToJoystickEvent(const AString: String): TJoystickEvent;
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

procedure TJoystickParser.Parse(const AString: String);
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

var
  Database: TJoystickDatabase;

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
  if Database = nil then
    Database := TJoystickDatabase.Create([doOwnsValues])
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
          Database.AddOrSetValue(Rec.JoystickName, Rec)
        else
          FreeAndNil(Rec);
      end;
    FreeAndNil(Strings);
  finally
    FreeAndNil(Stream);
  end;
end;

begin
end.

