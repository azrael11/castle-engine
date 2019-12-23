{
  Copyright 2019-2019 Yevhen Loza, Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Temporary test app for new Joystick management pipeline.
  In future it will be merged with examples/joystick/joystick_demo_standalone.lpi. }
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
  JoysticksNew.UiContainers.Add(Window.Container);
  JoysticksNew.FreeJoysticksDatabaseAfterInitialization := false; //will prevent joystick database from freeing automatically after autodetecting joysticks
  JoysticksNew.Initialize;
  if Joysticks.Count > 0 then
  begin
    //ListJoysticks;

    {$ifdef Windows}
    JoysticksNew.AssignJoystickLayoutByName(Joysticks[0], 'Esperanza EG102');
    {$endif}
    {$ifdef Linux}
    JoysticksNew.AssignJoystickLayoutByName(Joysticks[0], 'Esperanza EG102');
    {$endif}
  end;
  JoysticksNew.FreeDatabase;
  JoysticksNew.GenerateFakeEvents := true;
end;

begin
  InitializeLog;

  ApplicationProperties.ApplicationName := 'Joystick manager';
  Application.OnInitialize := @ApplicationInitialize;
  Window := TCastleWindowBase.Create(Application);
  Application.MainWindow := Window;

  Application.MainWindow.OpenAndRun;
end.

