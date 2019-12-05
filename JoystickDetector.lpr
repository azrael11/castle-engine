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
end;

begin
  InitializeLog;

  ApplicationProperties.ApplicationName := 'Joystick manager';
  Application.OnInitialize := @ApplicationInitialize;
  Window := TCastleWindowBase.Create(Application);
  Application.MainWindow := Window;

  Application.MainWindow.OpenAndRun;
end.

