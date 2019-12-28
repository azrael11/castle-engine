{
  Copyright 2019-2019 Michalis Kamburelis, Yevhen Loza.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Visualises joystick input. }
unit GameStateMain;

interface

uses
  Classes,
  CastleKeysMouse, CastleControls, CastleUiState, CastleUiControls;

type
  TStateMain = class(TUiState)
  strict private
    ImageSouth, ImageEast, ImageWest, ImageNorth,
    ImageStart, ImageBack, ImageGuide,
    ImageRightShoulder, ImageRightTrigger, ImageRightStick,
    ImageLeftShoulder, ImageLeftTrigger, ImageLeftStick,
    ImageDPadRight, ImageDPadLeft, ImageDPadUp, ImageDPadDown: TCastleImageControl;
    ImageRightAxis, ImageLeftAxis: TCastleImageControl;
    RightStickAxis, LeftStickAxis: TCastleUserInterface;
    procedure HideAllKeys;
    procedure ShowKey(const AKey: TKey; const AExists: Boolean);
  public
    procedure Start; override;
    procedure Stop; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
    function Release(const Event: TInputPressRelease): Boolean; override;
  end;

var
  StateMain: TStateMain;

implementation

uses
  SysUtils,
  CastleComponentSerialize,
  CastleJoystickManager;

procedure TStateMain.Start;
var
  UiOwner: TComponent;
begin
  inherited;
  InterceptInput := true;

  InsertUserInterface('castle-data:/main.castle-user-interface',
    FreeAtStop, UiOwner);

  ImageSouth := UIOwner.FindRequiredComponent('ImageSouth') as TCastleImageControl;
  ImageEast := UIOwner.FindRequiredComponent('ImageEast') as TCastleImageControl;
  ImageWest := UIOwner.FindRequiredComponent('ImageWest') as TCastleImageControl;
  ImageNorth := UIOwner.FindRequiredComponent('ImageNorth') as TCastleImageControl;
  ImageStart := UIOwner.FindRequiredComponent('ImageStart') as TCastleImageControl;
  ImageBack := UIOwner.FindRequiredComponent('ImageBack') as TCastleImageControl;
  ImageGuide := UIOwner.FindRequiredComponent('ImageGuide') as TCastleImageControl;
  ImageRightShoulder := UIOwner.FindRequiredComponent('ImageRightShoulder') as TCastleImageControl;
  ImageRightTrigger := UIOwner.FindRequiredComponent('ImageRightTrigger') as TCastleImageControl;
  ImageRightStick := UIOwner.FindRequiredComponent('ImageRightStick') as TCastleImageControl;
  ImageLeftShoulder := UIOwner.FindRequiredComponent('ImageLeftShoulder') as TCastleImageControl;
  ImageLeftTrigger := UIOwner.FindRequiredComponent('ImageLeftTrigger') as TCastleImageControl;
  ImageLeftStick := UIOwner.FindRequiredComponent('ImageLeftStick') as TCastleImageControl;
  ImageDPadRight := UIOwner.FindRequiredComponent('ImageDPadRight') as TCastleImageControl;
  ImageDPadLeft := UIOwner.FindRequiredComponent('ImageDPadLeft') as TCastleImageControl;
  ImageDPadUp := UIOwner.FindRequiredComponent('ImageDPadUp') as TCastleImageControl;
  ImageDPadDown := UIOwner.FindRequiredComponent('ImageDPadDown') as TCastleImageControl;

  ImageRightAxis := UIOwner.FindRequiredComponent('ImageRightAxis') as TCastleImageControl;
  ImageLeftAxis := UIOwner.FindRequiredComponent('ImageLeftAxis') as TCastleImageControl;

  RightStickAxis := UIOwner.FindRequiredComponent('RightStickAxis') as TCastleUserInterface;
  LeftStickAxis := UIOwner.FindRequiredComponent('LeftStickAxis') as TCastleUserInterface;

  HideAllKeys;
end;

procedure TStateMain.Stop;
begin

  inherited;
end;

procedure TStateMain.HideAllKeys;
begin
  ImageSouth.Exists := false;
  ImageEast.Exists := false;
  ImageWest.Exists := false;
  ImageNorth.Exists := false;
  ImageBack.Exists := false;
  ImageStart.Exists := false;
  ImageLeftShoulder.Exists := false;
  ImageRightShoulder.Exists := false;
  ImageLeftTrigger.Exists := false;
  ImageRightTrigger.Exists := false;
  ImageLeftStick.Exists := false;
  ImageRightStick.Exists := false;
  ImageGuide.Exists := false;
  ImageDPadLeft.Exists := false;
  ImageDPadRight.Exists := false;
  ImageDPadUp.Exists := false;
  ImageDPadDown.Exists := false;
end;

procedure TStateMain.ShowKey(const AKey: TKey; const AExists: Boolean);
begin
  case AKey of
    joySouth: ImageSouth.Exists := AExists;
    joyEast: ImageEast.Exists := AExists;
    joyWest: ImageWest.Exists := AExists;
    joyNorth: ImageNorth.Exists := AExists;
    joyBack: ImageBack.Exists := AExists;
    joyStart: ImageStart.Exists := AExists;
    joyLeftShoulder: ImageLeftShoulder.Exists := AExists;
    joyRightShoulder: ImageRightShoulder.Exists := AExists;
    joyLeftTrigger: ImageLeftTrigger.Exists := AExists;
    joyRightTrigger: ImageRightTrigger.Exists := AExists;
    joyLeftStick: ImageLeftStick.Exists := AExists;
    joyRightStick: ImageRightStick.Exists := AExists;
    joyGuide: ImageGuide.Exists := AExists;
    joyLeft: ImageDPadLeft.Exists := AExists;
    joyRight: ImageDPadRight.Exists := AExists;
    joyUp: ImageDPadUp.Exists := AExists;
    joyDown: ImageDPadDown.Exists := AExists;
  end;
end;

procedure TStateMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;

end;

function TStateMain.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  ShowKey(Event.Key, true);
end;

function TStateMain.Release(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  ShowKey(Event.Key, false);
end;

end.

