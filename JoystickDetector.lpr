{ Reading and using the database from https://github.com/gabomdq/SDL_GameControllerDB }
program JoystickDetector;

uses
  Classes, SysUtils,
  CastleJoystickManager, CastleLog, CastleWindow, CastleJoysticks,
  CastleApplicationProperties;

var
  Window: TCastleWindowBase;

procedure ApplicationInitialize;

  procedure ListJoysticks;
  var
    SList: TStringList;
    S: String;
  begin
    SList := JoysticksNew.JoysticksLayoutsNames;
    if SList <> nil then
    begin
      for S in SList do
        WriteLnLog(S);
      FreeAndNil(SList);
    end;
  end;

begin
  JoysticksNew.Container := Window.Container;
  JoysticksNew.FreeJoysticksDatabaseAfterInitialization := false;
  JoysticksNew.Initialize;
  if Joysticks.Count > 0 then
  begin
    //ListJoysticks;

    {$ifdef Windows}
    JoysticksNew.AssignJoystickLayoutByName(Joysticks[0], 'G-Shark GS-GP702);
    {$endif}
    {$ifdef Linux}
    JoysticksNew.AssignJoystickLayoutByName(Joysticks[0], 'Microntek USB Joystick');
    {$endif}
  end;
  JoysticksNew.FreeDatabase;
end;

begin
  InitializeLog;

  ApplicationProperties.ApplicationName := 'Joystick manager';
  Application.OnInitialize := @ApplicationInitialize;
  Window := TCastleWindowBase.Create(Application);
  Application.MainWindow := Window;

  Application.MainWindow.OpenAndRun;
end.

