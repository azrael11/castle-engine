{ Reading and using the database from https://github.com/gabomdq/SDL_GameControllerDB }
program JoystickDetector;

uses
  SysUtils, CastleJoystickManager, CastleLog, CastleWindow,
  CastleApplicationProperties;

var
  Window: TCastleWindowBase;

procedure ApplicationInitialize;
begin
  JoysticksNew.Initialize;

  {WriteLnLog('===================');
  for I := 0 to Pred(Joysticks.Count) do
  begin
    //J := Joysticks[I];
    {WriteLnLog('Joystick Name', J.Info.Name);
    WriteLnLog('Joystick Buttons', IntToStr(J.Info.Count.Buttons));
    WriteLnLog('Joystick Axes', IntToStr(J.Info.Count.Axes));
    WriteLnLog('Joystick Caps', IntToStr(J.Info.Caps));
    WriteLnLog('--------------------'); }
  end;}
end;

begin
  InitializeLog;

  ApplicationProperties.ApplicationName := 'Joystick manager';
  Application.OnInitialize := @ApplicationInitialize;
  Window := TCastleWindowBase.Create(Application);
  Application.MainWindow := Window;

  Application.MainWindow.OpenAndRun;
end.

