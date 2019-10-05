{ Reading and using the database from https://github.com/gabomdq/SDL_GameControllerDB }
program JoystickDetector;

uses
  SysUtils,
  CastleJoystickManager, CastleJoysticks, CastleLog;

var
  I: Integer;
  J: TJoystick;
begin
  InitializeLog;
  Joysticks.Initialize;
  ParseJoysticksDatabase('castle-data:/gamecontrollerdb.txt');
  WriteLnLog('===================');
  for I := 0 to Pred(Joysticks.Count) do
  begin
    J := Joysticks[I];
    WriteLnLog('Joystick Name', J.Info.Name);
    WriteLnLog('Joystick Buttons', IntToStr(J.Info.Count.Buttons));
    WriteLnLog('Joystick Axes', IntToStr(J.Info.Count.Axes));
    WriteLnLog('Joystick Caps', IntToStr(J.Info.Caps));
    WriteLnLog('--------------------');
  end;
end.

