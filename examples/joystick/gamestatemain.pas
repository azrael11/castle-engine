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
  CastleKeysMouse, CastleControls, CastleUiState, CastleUiControls,
  CastleJoysticks;

type
  TJoystickLayoutButton = class(TCastleButton)
  public
    LayoutLabel: TCastleLabel;
  end;

  TJoystickSelectButton = class(TCastleButton)
  public
    Joystick: TJoystick;
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
    HorizontalGroupDetectedJoysticks: TCastleHorizontalGroup;
    LastFocusedLayoutButton: TJoystickLayoutButton;
    LastFocusedJoystickButton: TJoystickSelectButton;
    CurrentJoystick: TJoystick;
    procedure HideAllKeys;
    procedure ShowKey(const AKey: TKey; const AExists: Boolean);
    procedure FillJoystickNames;
    procedure DetectJoysticks;
    procedure ClickJoystickLayout(Sender: TObject);
    procedure ClickSelectJoystick(Sender: TObject);
    procedure AddFocusToLayout(const AButton: TJoystickLayoutButton);
    procedure RemoveFocusFromLayout(const AButton: TJoystickLayoutButton);
    procedure AddFocusToJoystick(const AButton: TJoystickSelectButton);
    procedure RemoveFocusFromJoystick(const AButton: TJoystickSelectButton);
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
  CastleComponentSerialize, CastleVectors, CastleColors, CastleUtils, CastleLog,
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

  JoystickLayoutName := UIOwner.FindRequiredComponent('JoystickLayoutName') as TCastleLabel;
  JoystickReportedName := UIOwner.FindRequiredComponent('JoystickReportedName') as TCastleLabel;

  //JoysticksNames := UIOwner.FindRequiredComponent('JoysticksNames') as TCastleScrollView;
  VerticalGroupJoysticksNames := UIOwner.FindRequiredComponent('VerticalGroupJoysticksNames') as TCastleVerticalGroup;

  HorizontalGroupDetectedJoysticks := UIOwner.FindRequiredComponent('HorizontalGroupDetectedJoysticks') as TCastleHorizontalGroup;

  CurrentJoystick := Joysticks[0];

  HideAllKeys;
  DetectJoysticks;
  FillJoystickNames;
end;

procedure TStateMain.Stop;
begin

  inherited;
end;

procedure TStateMain.DetectJoysticks;
var
  I: Integer;
  JoystickSelectButton: TJoystickSelectButton;
begin
  HorizontalGroupDetectedJoysticks.ClearControls;
  for I := 0 to Pred(Joysticks.Count) do
  begin
    JoystickSelectButton := TJoystickSelectButton.Create(FreeAtStop);
    JoystickSelectButton.Joystick := Joysticks[I];
    JoystickSelectButton.OnClick := @ClickSelectJoystick;
    JoystickSelectButton.Caption := JoysticksNew.JoysticksAdditionalData[Joysticks[I]].TrimmedName;
    JoystickSelectButton.FontSize := 30;
    JoystickSelectButton.CustomTextColor := White;
    JoystickSelectButton.CustomTextColorUse := true;
    JoystickSelectButton.CustomBackground := true;
    if Joysticks[I] = CurrentJoystick then
      AddFocusToJoystick(JoystickSelectButton)
    else
      RemoveFocusFromJoystick(JoystickSelectButton);
    HorizontalGroupDetectedJoysticks.InsertFront(JoystickSelectButton);
  end;
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
    VerticalGroupJoysticksNames.ClearControls;
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
      if JoysticksNew.JoysticksAdditionalData[CurrentJoystick].Layout.JoystickName = S then
        AddFocusToLayout(JoystickLayoutButton)
      else
        RemoveFocusFromLayout(JoystickLayoutButton);
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

procedure TStateMain.AddFocusToLayout(const AButton: TJoystickLayoutButton);
begin
  LastFocusedLayoutButton := AButton;
  AButton.CustomColorNormal := Vector4(0.3, 0.3, 0.0, 1.0);
  AButton.CustomColorFocused := Vector4(0.4, 0.4, 0.0, 1.0);
  AButton.CustomColorPressed := Vector4(0.4, 0.4, 0.0, 1.0);
end;

procedure TStateMain.RemoveFocusFromLayout(const AButton: TJoystickLayoutButton);
begin
  AButton.CustomColorNormal := Vector4(0.3, 0.3, 0.0, 0.0);
  AButton.CustomColorFocused := Vector4(0.4, 0.4, 0.0, 0.2);
  AButton.CustomColorPressed := Vector4(0.4, 0.4, 0.0, 0.2);
end;

procedure TStateMain.AddFocusToJoystick(const AButton: TJoystickSelectButton);
begin
  LastFocusedJoystickButton := AButton;
  AButton.CustomColorNormal := Vector4(0.3, 0.3, 0.3, 1.0);
  AButton.CustomColorFocused := Vector4(0.4, 0.4, 0.4, 1.0);
  AButton.CustomColorPressed := Vector4(0.5, 0.5, 0.5, 1.0);
end;

procedure TStateMain.RemoveFocusFromJoystick(const AButton: TJoystickSelectButton);
begin
  AButton.CustomColorNormal := Vector4(0.3, 0.3, 0.3, 0.2);
  AButton.CustomColorFocused := Vector4(0.4, 0.4, 0.4, 0.2);
  AButton.CustomColorPressed := Vector4(0.5, 0.5, 0.5, 0.2);
end;


procedure TStateMain.ClickJoystickLayout(Sender: TObject);
begin
  RemoveFocusFromLayout(LastFocusedLayoutButton);
  AddFocusToLayout(Sender as TJoystickLayoutButton);
  JoysticksNew.AssignJoystickLayoutByName(CurrentJoystick, LastFocusedLayoutButton.LayoutLabel.Caption);
end;

procedure TStateMain.ClickSelectJoystick(Sender: TObject);
begin
  RemoveFocusFromJoystick(LastFocusedJoystickButton);
  LastFocusedJoystickButton := Sender as TJoystickSelectButton;
  AddFocusToJoystick(LastFocusedJoystickButton);
  CurrentJoystick := LastFocusedJoystickButton.Joystick;
  FillJoystickNames; //to highlight a correct layout
  HideAllKeys;
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
      //raise EInternalError.CreateFmt
      WriteLnLog('TStateMain.ShowKey received an impossible key "%s".', [KeyToStr(AKey)]);
  end;
end;

procedure TStateMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
var
  AxisNormalized: TVector2;
begin
  inherited;
  AxisNormalized := JoysticksNew.JoysticksAdditionalData[CurrentJoystick].RightAxis;
  AxisNormalized.NormalizeMe;
  ImageRightAxis.HorizontalAnchorDelta := RightStickAxis.Width / 2 * AxisNormalized[0];
  ImageRightAxis.VerticalAnchorDelta := RightStickAxis.Height / 2 * AxisNormalized[1];

  AxisNormalized := JoysticksNew.JoysticksAdditionalData[CurrentJoystick].LeftAxis;
  AxisNormalized.NormalizeMe;
  ImageLeftAxis.HorizontalAnchorDelta := LeftStickAxis.Width / 2 * AxisNormalized[0];
  ImageLeftAxis.VerticalAnchorDelta := LeftStickAxis.Height / 2 * AxisNormalized[1];

  JoystickReportedName.Caption := JoysticksNew.JoysticksAdditionalData[CurrentJoystick].TrimmedName;
  JoystickLayoutName.Caption := JoysticksNew.JoysticksAdditionalData[CurrentJoystick].Layout.JoystickName;
end;

function TStateMain.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Event.EventType = itJoystick then
    ShowKey(Event.Key, true);
end;

function TStateMain.Release(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Event.EventType = itJoystick then
    ShowKey(Event.Key, false);
end;

end.

