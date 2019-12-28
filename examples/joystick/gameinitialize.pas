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
  //Notifications: TCastleNotifications;
  //ButtonReinitialize: TCastleButton;
  //OnScreenMenu: TCastleOnScreenMenu;
  //LabelJoysticksCount: TCastleLabel;
  //LabelSelectedJoystick: TCastleLabel;
  //SelectedJoystick: Integer = -1;

{ One-time initialization of resources. }
procedure ApplicationInitialize;
{var
  MenuGroup: TCastleVerticalGroup;}
begin
  { Adjust container settings for a scalable UI (adjusts to any window size in a smart way). }
  Window.Container.LoadSettings('castle-data:/CastleSettings.xml');

  {Notifications := TCastleNotifications.Create(Application);
  Notifications.Anchor(vpBottom, 10);
  Notifications.Anchor(hpMiddle);
  Notifications.TextAlignment := hpMiddle;
  Notifications.Timeout := 2.0;
  Notifications.Fade := 0.5;
  Window.Controls.InsertBack(Notifications); }

  {ButtonReinitialize := TCastleButton.Create(Application);
  ButtonReinitialize.Caption := 'Detect connected joysticks again (Joysticks.Initialize)';
  ButtonReinitialize.Left := 10;
  ButtonReinitialize.Bottom := 10;
  ButtonReinitialize.OnClick := @TEventsHandler(nil).ClickReinitialize;
  Window.Controls.InsertFront(ButtonReinitialize);}

  {MenuGroup := TCastleVerticalGroup.Create(Application);
  MenuGroup.Left := 10;
  MenuGroup.Anchor(vpTop, -10);
  MenuGroup.Spacing := 10;
  Window.Controls.InsertFront(MenuGroup);}

  {LabelJoysticksCount := TCastleLabel.Create(Application);
  LabelJoysticksCount.Color := White;
  MenuGroup.InsertFront(LabelJoysticksCount);}

  {OnScreenMenu := TCastleOnScreenMenu.Create(Application);
  MenuGroup.InsertFront(OnScreenMenu);}

  {LabelSelectedJoystick := TCastleLabel.Create(Application);
  LabelSelectedJoystick.Color := White;
  LabelSelectedJoystick.Caption := 'Selected: none';
  MenuGroup.InsertFront(LabelSelectedJoystick);}

  JoysticksNew.FreeJoysticksDatabaseAfterInitialization := false; //will prevent joystick database from freeing automatically after autodetecting joysticks
  { Actually detect joysticks.
    This will automatically call TEventsHandler.JoysticksChanged on some platforms. }
  JoysticksNew.Initialize;

  {$ifdef Windows}
  JoysticksNew.AssignJoystickLayoutByName(Joysticks[0], 'Esperanza EG102');
  {$endif}
  {$ifdef Linux}
  JoysticksNew.AssignJoystickLayoutByName(Joysticks[0], 'Esperanza EG102');
  {$endif}

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
