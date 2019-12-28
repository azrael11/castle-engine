{
  Copyright 2016-2019 Tomasz Wojtyś, Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ List all avalaible joysticks/gamepads in the system,
  and allows to test their inputs. }
unit GameInitialize;

interface

implementation

uses SysUtils, Classes,
  CastleVectors, CastleWindow, CastleControls, CastleOnScreenMenu,
  CastleControlsImages, CastleImages, CastleFilesUtils, CastleColors,
  CastleUIControls, CastleNotifications, CastleLog, CastleUiState,
  CastleJoysticks, CastleJoystickManager,
  CastleApplicationProperties,
  GameStateMain;

var
  Window: TCastleWindowBase;

  { One-time initialization of resources. }
procedure ApplicationInitialize;
begin
  { Adjust container settings for a scalable UI (adjusts to any window size in a smart way). }
  Window.Container.LoadSettings('castle-data:/CastleSettings.xml');

  //will prevent joystick database from freeing automatically after autodetecting joysticks
  JoysticksNew.FreeJoysticksDatabaseAfterInitialization := false;

  { Actually detect joysticks.
    This will automatically call TEventsHandler.JoysticksChanged on some platforms. }
  JoysticksNew.Initialize;

  StateMain := TStateMain.Create(Application);
  TUiState.Current := StateMain;
end;

initialization
  { Set ApplicationName early, as our log uses it.
    Optionally you could also set ApplicationProperties.Version here. }
  ApplicationProperties.ApplicationName := 'joystick_demo';

  { Start logging. Do this as early as possible,
    to log information and eventual warnings during initialization. }
  InitializeLog;

  { Initialize Application.OnInitialize. }
  Application.OnInitialize := @ApplicationInitialize;

  { Create and assign Application.MainWindow. }
  Window := TCastleWindowBase.Create(Application);
  Application.MainWindow := Window;
end.
