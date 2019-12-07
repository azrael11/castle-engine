program CastleInternalJoystickDatabaseConverter;

uses
  Classes, SysUtils, Generics.Collections,
  CastleDownload, CastleLog, CastleStringUtils, CastleUtils,
  CastleInternalJoystickRecord;

type
  TBuggyGuidDictionary = specialize TDictionary<String, TGuid>;

var
  BuggyGuids: TBuggyGuidDictionary;

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

  procedure AddDictionaryEntry(const ADictionary: TJoystickDictionary;
    const AValue: Byte; const AEvent: TJoystickEvent);
  begin
    if ADictionary.ContainsKey(AValue) then
      if ADictionary.Items[AValue] <> AEvent then
      begin
        WriteLnLog('Warning', 'Joystick "' + JoystickName + '"' +
          ' has duplicate entry.' +
          ' The old value ' + JoystickEventToStr(ADictionary.Items[AValue]) +
          ' was overwritten with ' + JoystickEventToStr(AEvent) + '.');
        BuggyDuplicateEvents := true;
      end;
    ADictionary.AddOrSetValue(AValue, AEvent);
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
            vtButton: AddDictionaryEntry(Buttons, VT.Value, StrToJoystickEvent(Pair.Caption));
            vtDPad: AddDictionaryEntry(DPad, VT.Value, StrToJoystickEvent(Pair.Caption));
            vtBothAxes: begin
                          AddDictionaryEntry(AxesPlus, VT.Value, StrToJoystickEvent(Pair.Caption));
                          AddDictionaryEntry(AxesMinus, VT.Value, StrToJoystickEvent(Pair.Caption));
                        end;
            vtAxisPlus: AddDictionaryEntry(AxesPlus, VT.Value, StrToJoystickEvent(Pair.Caption));
            vtAxisMinus: AddDictionaryEntry(AxesMinus, VT.Value, StrToJoystickEvent(Pair.Caption));
          end;
        end;
      end;
  end;
  FreeAndNil(Data);
end;

procedure GetBuggyGuids;
begin
  BuggyGuids := TBuggyGuidDictionary.Create;
  BuggyGuids.Add('Windows', '03000000790000000600000000000000');
  BuggyGuids.Add('Linux', '03000000780000000600000010010000');
end;

function IsBuggyGuid(const ARec: TJoystickParser): Boolean;
var
  OS: String;
begin
  Result := false;
  for OS in BuggyGuids.Keys do
    if (OS = ARec.Platform) and (BuggyGuids.Items[OS] = ARec.Guid) then
      Result := true;
end;

var
  Database: TJoystickDatabase;

procedure ParseJoysticksDatabase(const URL: String);
var
  Stream: TStream;
  Strings: TStringList;
  I: Integer;
  Rec: TJoystickParser;
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
        Rec := TJoystickParser.Create;
        Rec.Parse(Strings[I]);
        //[CRITICAL] we have a lot of duplicates here!
        //Database.AddOrSetValue(Rec.JoystickName, Rec);
        Database.Add(Rec.Guid + Rec.Platform, Rec);
      end;
    FreeAndNil(Strings);
  finally
    FreeAndNil(Stream);
  end;
end;

procedure WriteDatabase(const Platform: String);
var
  RecCount: Integer;

  function DatabaseToString: String;

    function DatabaseRecordToString(const Rec: TJoystickParser): String;

      function JoyDictionaryToString(const AName: String; const ADictionary: TJoystickDictionary): String;
      var
        B: Byte;
      begin
        Result := '';
        for B in ADictionary.Keys do
          Result +=
            '  JoyData.' + AName + '.Add(' + IntToStr(B) + ', ' +
            JoystickEventToStr(ADictionary.Items[B]) + ');' +
            NL;
      end;

    begin
      Inc(RecCount);
      Result := NL +
        '  JoyData := TJoystickRecord.Create;' + NL +
        '  JoyData.JoystickName := ''' + StringReplace(Rec.JoystickName, '''', '''''', [rfReplaceAll]) + ''';' + NL +
        '  JoyData.Guid := ''' + Rec.Guid + ''';' + NL;
      if IsBuggyGuid(Rec) then
        Result +=
        '  JoyData.BuggyGuid := true;' + NL;
      if Rec.BuggyDuplicateEvents then
      Result +=
        '  JoyData.BuggyDuplicateEvents := true;' + NL;
      Result += JoyDictionaryToString('Buttons', Rec.Buttons);
      Result += JoyDictionaryToString('AxesPlus', Rec.AxesPlus);
      Result += JoyDictionaryToString('AxesMinus', Rec.AxesMinus);
      Result += JoyDictionaryToString('DPad', Rec.DPad);
      Result +=
        '  JoyData.CacheJoystickEvents;' + NL;
      //duplicates allowed in names
      Result +=
        '  JoystickRecordsByName.AddOrSetValue(JoyData.JoystickName, JoyData);' + NL;
      //duplicates not allowed in GUID
      Result +=
        '  JoystickRecordsByGuid.Add(JoyData.Guid, JoyData);' + NL;
    end;
  var
    S: String;
  begin
    Result := NL +
      'procedure FreeJoysticksDatabase;' + NL +
      'begin' + NL +
      '  FreeAndNil(JoystickRecordsByName);' + NL +
      '  FreeAndNil(JoystickRecordsByGuid);' + NL +
      'end;' + NL + NL +

      'procedure InitJoysticksDatabase;' + NL +
      'var' + NL +
      '  JoyData: TJoystickRecord;' + NL +
      'begin' + NL +
      '  JoystickRecordsByName := TJoystickDatabase.Create;' + NL + //owns nothing as contains duplicates
      '  JoystickRecordsByGuid := TJoystickDatabase.Create([doOwnsValues]);' + NL;
    for S in Database.Keys do
      if (Database[S] as TJoystickParser).Platform = Platform then
        Result := Result + DatabaseRecordToString((Database[S] as TJoystickParser));
    Result += 'end;' + NL;
  end;

var
  OutputUnit: TTextWriter;
  UnitName: String;
begin
  RecCount := 0;
  UnitName := 'CastleInternalJoystickDatabase' + Platform;
  OutputUnit := TTextWriter.Create(UnitName + '.pas');
  OutputUnit.Write(
    '{ -*- buffer-read-only: t -*- }' + NL +
    NL +
    '{ Unit automatically generated by CastleInternalJoystickDatabaseConverter tool,' + NL +
    '  to store converted version of https://github.com/gabomdq/SDL_GameControllerDB.' + NL +
    '  @exclude (Exclude this unit from PasDoc documentation.) }' + NL +
    'unit ' + UnitName + ';' + NL +
    NL +
    'interface' + NL +
    NL +
    'uses CastleInternalJoystickRecord;' + NL +
    NL);

  OutputUnit.Write(
    'procedure InitJoysticksDatabase;' + NL +
    'procedure FreeJoysticksDatabase;' + NL +
    'implementation' + NL +
    NL +
    'uses SysUtils, Generics.Collections;' + NL + NL);

  OutputUnit.Write(DatabaseToString);

  OutputUnit.Write(NL +
    'finalization' + NL +
    '  FreeJoysticksDatabase;' + NL +
    'end.');
  FreeAndNil(OutputUnit);
  WriteLnLog('Written ' + IntToStr(RecCount) + ' records for platform', Platform);
end;

begin
  InitializeLog;
  GetBuggyGuids;
  ParseJoysticksDatabase('castle-data:/gamecontrollerdb.txt');
  WriteDatabase('Windows');
  WriteDatabase('Linux');
  FreeAndNil(BuggyGuids);
  FreeAndNil(Database);
end.

