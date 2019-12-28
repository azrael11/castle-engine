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
  TJoystickLayoutButton = class(TCastleButton)
  public
    LayoutLabel: TCastleLabel;
  end;

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
    JoystickLayoutName, JoystickReportedName: TCastleLabel;
    //ScrollViewJoysticksNames: TCastleScrollView;
    VerticalGroupJoysticksNames: TCastleVerticalGroup;
    LastFocusedButton: TJoystickLayoutButton;
    procedure HideAllKeys;
    procedure ShowKey(const AKey: TKey; const AExists: Boolean);
    procedure FillJoystickNames;
    procedure ClickJoystickLayout(Sender: TObject);
    procedure AddFocusTo(const AButton: TJoystickLayoutButton);
    procedure RemoveFocusFrom(const AButton: TJoystickLayoutButton);
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
  CastleComponentSerialize, CastleVectors, CastleColors, CastleUtils,
  CastleJoysticks, CastleJoystickManager;

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

  JoystickLayoutName := UIOwner.FindRequiredComponent('JoystickLayoutName') as TCastleLabel;
  JoystickReportedName := UIOwner.FindRequiredComponent('JoystickReportedName') as TCastleLabel;

  //JoysticksNames := UIOwner.FindRequiredComponent('JoysticksNames') as TCastleScrollView;
  VerticalGroupJoysticksNames := UIOwner.FindRequiredComponent('VerticalGroupJoysticksNames') as TCastleVerticalGroup;

  HideAllKeys;
  FillJoystickNames;
end;

procedure TStateMain.Stop;
begin

  inherited;
end;

procedure TStateMain.FillJoystickNames;
var
  SList: TStringList;
  S: String;
  JoystickLayoutLabel: TCastleLabel;
  JoystickLayoutButton: TJoystickLayoutButton;
begin
  SList := JoysticksNew.JoysticksLayoutsNames;
  if SList <> nil then
  begin
    SList.Sort;
    for S in SList do
    begin
      JoystickLayoutButton := TJoystickLayoutButton.Create(FreeAtStop);
      JoystickLayoutButton.CustomBackground := true;
      JoystickLayoutButton.AutoSizeToChildren := true;
      JoystickLayoutButton.EnableParentDragging := true;
      JoystickLayoutButton.OnClick := @ClickJoystickLayout;
      JoystickLayoutLabel := TCastleLabel.Create(JoystickLayoutButton);
      JoystickLayoutLabel.Caption := S;
      JoystickLayoutLabel.Color := White;
      if JoysticksNew.JoysticksAdditionalData[Joysticks[0]].Layout.JoystickName = S then
        AddFocusTo(JoystickLayoutButton)
      else
        RemoveFocusFrom(JoystickLayoutButton);
      JoystickLayoutButton.LayoutLabel := JoystickLayoutLabel;
      JoystickLayoutButton.InsertFront(JoystickLayoutLabel);
      VerticalGroupJoysticksNames.InsertFront(JoystickLayoutButton);
    end;
    FreeAndNil(SList);
  end;
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

procedure TStateMain.AddFocusTo(const AButton: TJoystickLayoutButton);
begin
  LastFocusedButton := AButton;
  AButton.CustomColorNormal := Vector4(0.3, 0.3, 0.0, 1.0);
  AButton.CustomColorFocused := Vector4(0.4, 0.4, 0.0, 1.0);
  AButton.CustomColorPressed := Vector4(0.4, 0.4, 0.0, 1.0);
end;

procedure TStateMain.RemoveFocusFrom(const AButton: TJoystickLayoutButton);
begin
  AButton.CustomColorNormal := Vector4(0.0, 0.0, 0.0, 0.0);
  AButton.CustomColorFocused := Vector4(0.4, 0.4, 0.0, 0.2);
  AButton.CustomColorPressed := Vector4(0.4, 0.4, 0.0, 0.2);
end;

procedure TStateMain.ClickJoystickLayout(Sender: TObject);
begin
  RemoveFocusFrom(LastFocusedButton);
  AddFocusTo(Sender as TJoystickLayoutButton);
  JoysticksNew.AssignJoystickLayoutByName(Joysticks[0], LastFocusedButton.LayoutLabel.Caption);
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
    else
      raise EInternalError.CreateFmt('TStateMain.ShowKey received an impossible key "%s".', [KeyToStr(AKey)]);
  end;
end;

procedure TStateMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
var
  AxisNormalized: TVector2;
begin
  inherited;
  AxisNormalized := JoysticksNew.JoysticksAdditionalData[Joysticks[0]].RightAxis;
  AxisNormalized.NormalizeMe;
  ImageRightAxis.HorizontalAnchorDelta := RightStickAxis.Width / 2 *
    AxisNormalized[0];
  ImageRightAxis.VerticalAnchorDelta := RightStickAxis.Height / 2 *
    AxisNormalized[1];

  AxisNormalized := JoysticksNew.JoysticksAdditionalData[Joysticks[0]].LeftAxis;
  AxisNormalized.NormalizeMe;
  ImageLeftAxis.HorizontalAnchorDelta := LeftStickAxis.Width / 2 *
    AxisNormalized[0];
  ImageLeftAxis.VerticalAnchorDelta := LeftStickAxis.Height / 2 *
    AxisNormalized[1];

  JoystickReportedName.Caption := JoysticksNew.JoysticksAdditionalData[Joysticks[0]].TrimmedName;
  JoystickLayoutName.Caption := JoysticksNew.JoysticksAdditionalData[Joysticks[0]].Layout.JoystickName;
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

