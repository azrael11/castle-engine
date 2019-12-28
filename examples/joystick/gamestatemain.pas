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
    ImageA, ImageB, ImageX, ImageY, ImageStart, ImageBack, ImageGuide,
    ImageRightShoulder, ImageRightTrigger, ImageRightStick,
    ImageLeftShoulder, ImageLeftTrigger, ImageLeftStick,
    ImageDPadRight, ImageDPadLeft, ImageDPadUp, ImageDPadDown: TCastleImageControl;
    ImageRightAxis, ImageLeftAxis: TCastleImageControl;
    RightStickAxis, LeftStickAxis: TCastleUserInterface;
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
  CastleComponentSerialize;

procedure TStateMain.Start;
var
  UiOwner: TComponent;
begin
  inherited;
  InterceptInput := true;

  InsertUserInterface('castle-data:/main.castle-user-interface',
    FreeAtStop, UiOwner);

  ImageA := UIOwner.FindRequiredComponent('ImageA') as TCastleImageControl;
  ImageB := UIOwner.FindRequiredComponent('ImageB') as TCastleImageControl;
  ImageX := UIOwner.FindRequiredComponent('ImageX') as TCastleImageControl;
  ImageY := UIOwner.FindRequiredComponent('ImageY') as TCastleImageControl;
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
end;

procedure TStateMain.Stop;
begin

  inherited;
end;

procedure TStateMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin

end;

function TStateMain.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;

end;

function TStateMain.Release(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;

end;

end.

