{
  Copyright 2015-2019 Tomasz Wojtyś, Michalis Kamburelis.
  Based on zgl_joystick.pas by Andrey Kemka.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.
  This file is based on ZenGL, with "zlib" license
  ( http://www.zengl.org/license.html ) which is fully compatible with
  Castle Game Engine "LGPL with static linking exception" / GPL licensing.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Windows Joystick support. }
unit CastleInternalJoysticksWindows;

// TODO: only partially works on Windows (no support of new features); need to be ported to xinput/DirectInput library

interface

uses CastleJoysticks;

type
  PJOYCAPSW = ^TJOYCAPSW;
  TJOYCAPSW = packed record
    wMid: Word;
    wPid: Word;
    szPname: array[ 0..31 ] of WideChar;
    wXmin: LongWord;
    wXmax: LongWord;
    wYmin: LongWord;
    wYmax: LongWord;
    wZmin: LongWord;
    wZmax: LongWord;
    wNumButtons: LongWord;
    wPeriodMin: LongWord;
    wPeriodMax: LongWord;
    wRmin: LongWord;
    wRmax: LongWord;
    wUmin: LongWord;
    wUmax: LongWord;
    wVmin: LongWord;
    wVmax: LongWord;
    wCaps: LongWord;
    wMaxAxes: LongWord;
    wNumAxes: LongWord;
    wMaxButtons: LongWord;
    szRegKey: array[ 0..31 ] of WideChar;
    szOEMVxD: array[ 0..259 ] of WideChar;
end;

type
  PJOYINFOEX = ^TJOYINFOEX;
  TJOYINFOEX = packed record
    dwSize: LongWord;
    dwFlags: LongWord;
    dwXpos: LongWord;
    dwYpos: LongWord;
    dwZpos: LongWord;
    dwRpos: LongWord;
    dwUpos: LongWord;
    dwVpos: LongWord;
    wButtons: LongWord;
    dwButtonNumber: LongWord;
    dwPOV: LongWord;
    dwReserved1: LongWord;
    dwReserved2: LongWord;
  end;

const
  JOY_POVCENTERED    = -1;
  JOY_POVFORWARD     = 0;
  JOY_POVRIGHT       = 9000;
  JOY_POVBACKWARD    = 18000;
  JOY_POVLEFT        = 27000;
  JOY_RETURNX        = 1;
  JOY_RETURNY        = 2;
  JOY_RETURNZ        = 4;
  JOY_RETURNR        = 8;
  JOY_RETURNU        = 16;
  JOY_RETURNV        = 32;
  JOY_RETURNPOV      = 64;
  JOY_RETURNBUTTONS  = 128;
  JOY_RETURNRAWDATA  = 256;
  JOY_RETURNPOVCTS   = 512;
  JOY_RETURNCENTERED = $400;
  JOY_USEDEADZONE    = $800;
  JOY_RETURNALL      = ( JOY_RETURNX or JOY_RETURNY or JOY_RETURNZ or JOY_RETURNR or JOY_RETURNU or JOY_RETURNV or JOY_RETURNPOV or JOY_RETURNBUTTONS );

  JOYCAPS_HASZ    = 1;
  JOYCAPS_HASR    = 2;
  JOYCAPS_HASU    = 4;
  JOYCAPS_HASV    = 8;
  JOYCAPS_HASPOV  = 16;
  JOYCAPS_POV4DIR = 32;
  JOYCAPS_POVCTS  = 64;

  JOYERR_NOERROR = 0;
  JOYERR_BASE = 160;
  JOYERR_PARMS = JOYERR_BASE + 5; // The specified joystick identifier is invalid.
  JOYERR_UNPLUGGED = JOYERR_BASE + 7; //The specified joystick is not connected to the system.

  WINMMLIB = 'winmm.dll';

function joyGetNumDevs: LongWord; stdcall; external WINMMLIB name 'joyGetNumDevs';
function joyGetDevCapsW(uJoyID: LongWord; lpCaps: PJOYCAPSW; uSize: LongWord ): LongWord; stdcall; external WINMMLIB name 'joyGetDevCapsW';
function joyGetPosEx(uJoyID: LongWord; lpInfo: PJOYINFOEX ): LongWord; stdcall; external WINMMLIB name 'joyGetPosEx';

type
  TWindowsJoystick = class(TJoystick)
    Capabilities: TJOYCAPSW;
  end;

  TWindowsJoysticksBackend = class(TJoysticksBackend)
    procedure Initialize(const List: TJoystickList); override;
    procedure Poll(const List: TJoystickList;
      const EventContainer: TJoysticks); override;
  end;

implementation

uses
  SysUtils, CastleLog, Math;

type
  TWindowsJoystickBackendInfo = class
    Caps    : TJOYCAPSW;
    AxesMap : array[ 0..5 ] of Byte;
  end;

procedure TWindowsJoysticksBackend.Initialize(const List: TJoystickList);
var
  JoyError: LongWord;
  State: TJOYINFOEX;
  NewBackendInfo: TWindowsJoystickBackendInfo;
  I: Integer;
  NewJoystick: TWindowsJoystick;
begin
  for I := 0 to joyGetNumDevs() - 1 do
  begin
    NewJoystick := TWindowsJoystick.Create;

    if joyGetDevCapsW(I, @NewJoystick.Capabilities, SizeOf(TJOYCAPSW)) = 0 then
    begin
      NewJoystick.Info.Name          := NewJoystick.Capabilities.szPname;
      NewJoystick.Info.Count.Axes    := NewJoystick.Capabilities.wNumAxes;
      NewJoystick.Info.Count.Buttons := NewJoystick.Capabilities.wNumButtons;

      //workaround Windows reporting recently disconnected joysticks as connected
      state.dwSize := SizeOf( TJOYINFOEX );
      state.dwFlags := JOY_RETURNALL or JOY_USEDEADZONE;
      NewBackendInfo := TWindowsJoystickBackendInfo.Create;
      if NewBackendInfo.Caps.wCaps and JOYCAPS_POVCTS > 0 then
        state.dwFlags := state.dwFlags or JOY_RETURNPOVCTS;
      JoyError := joyGetPosEx( i, @state );
      //if no errors, then add this joystick
      if JoyError = JOYERR_NOERROR then
      begin
        WritelnLog('CastleJoysticks Init', 'Find joy: %s (ID: %d); Axes: %d; Buttons: %d',
                   [NewJoystick.Info.Name, I, NewJoystick.Info.Count.Axes, NewJoystick.Info.Count.Buttons]);

        List.Add(NewJoystick);
      end else
      begin
        if JoyError = JOYERR_UNPLUGGED then
          WriteLnLog('CastleJoysticks Init', 'Found joy: %s, but it will not be added because it seems to have been disconnected from the system recently (JOYERR_UNPLUGGED).',
            [NewJoystick.Info.Name])
        else
          WriteLnWarning('CastleJoysticks Init', 'Found joy: %s, but it cannot be added due to an error %d.',
            [NewJoystick.Info.Name, JoyError]);
        FreeAndNil(NewJoystick);
      end;
    end else
      FreeAndNil(NewJoystick);
  end;
end;

procedure TWindowsJoysticksBackend.Poll(const List: TJoystickList;
  const EventContainer: TJoysticks);
var
  JoyError: LongWord;
  JoystickHasBeenDisconnected: Boolean;
  I: Integer;
  AxisValue: Single;
  Axis: Integer;
  Button: Integer;
  Btn: Integer;
  State: TJOYINFOEX;
  Joystick: TWindowsJoystick;
begin
  JoystickHasBeenDisconnected := false;
  state.dwSize := SizeOf(TJOYINFOEX);
  for I := 0 to List.Count - 1 do
  begin
    Joystick := List[I] as TWindowsJoystick;

    state.dwFlags := JOY_RETURNALL or JOY_USEDEADZONE;
    if Joystick.Capabilities.wCaps and JOYCAPS_POVCTS > 0 then
      State.dwFlags := State.dwFlags or JOY_RETURNPOVCTS;

    JoyError := joyGetPosEx( i, @state );
    case JoyError of
      JOYERR_NOERROR:
        begin
          for Axis := 0 to Joystick.Info.Count.Axes - 1 do
          begin
            case Axis of
              JOY_AXIS_X: AxisValue := State.dwXpos / (Joystick.Capabilities.wXmax - Joystick.Capabilities.wXmin) * 2 - 1;
              JOY_AXIS_Y: AxisValue := State.dwYpos / (Joystick.Capabilities.wYmax - Joystick.Capabilities.wYmin) * 2 - 1;
              JOY_AXIS_Z: AxisValue := State.dwZpos / (Joystick.Capabilities.wZmax - Joystick.Capabilities.wZmin) * 2 - 1;
              JOY_AXIS_R: AxisValue := State.dwRpos / (Joystick.Capabilities.wRmax - Joystick.Capabilities.wRmin) * 2 - 1;
              JOY_AXIS_U: AxisValue := State.dwUpos / (Joystick.Capabilities.wUmax - Joystick.Capabilities.wUmin) * 2 - 1;
              JOY_AXIS_V: AxisValue := State.dwVpos / (Joystick.Capabilities.wVmax - Joystick.Capabilities.wVmin) * 2 - 1;
              else
                AxisValue := 0;
            end;

            if Joystick.State.Axis[Axis] <> AxisValue then
              if Assigned(EventContainer.OnAxisMove) then
                EventContainer.OnAxisMove(Joystick, Axis, AxisValue);
            Joystick.State.Axis[Axis] := AxisValue;
          end;

          if Joystick.Capabilities.wCaps and JOYCAPS_HASPOV > 0 then
          begin
            if State.dwPOV and $FFFF <> $FFFF then
              AxisValue := Sin(DegToRad(State.dwPOV and $FFFF / 100.0))
            else
              AxisValue := 0;

            if Joystick.State.Axis[JOY_NEWPOVX] <> AxisValue then
              if Assigned(EventContainer.OnAxisMove) then
                EventContainer.OnAxisMove(Joystick, JOY_NEWPOVX, AxisValue);
            Joystick.State.Axis[JOY_NEWPOVX] := AxisValue;

            if State.dwPOV and $FFFF <> $FFFF then
              AxisValue := -Cos(DegToRad(State.dwPOV and $FFFF / 100.0) )
            else
              AxisValue := 0;
            if Joystick.State.Axis[JOY_NEWPOVY] <> AxisValue then
              if Assigned(EventContainer.OnAxisMove) then
                EventContainer.OnAxisMove(Joystick, JOY_NEWPOVY, AxisValue);
            Joystick.State.Axis[JOY_NEWPOVY] := AxisValue;
          end;

          for Button := 0 to Joystick.Info.Count.Buttons - 1 do
          begin
            Btn := State.wButtons and (1 shl Button);
            if (Joystick.State.BtnDown[Button]) and (Btn = 0) then
            begin
              Joystick.State.BtnPress[Button] := false;
              if Assigned(EventContainer.OnButtonUp) then
                EventContainer.OnButtonUp(Joystick, Button);
              Joystick.State.BtnCanPress[Button] := true;
            end;

            if (Joystick.State.BtnCanPress[Button]) and (not Joystick.State.BtnDown[Button]) and (Btn <> 0) then
            begin
              Joystick.State.BtnPress[Button] := true;
              if Assigned(EventContainer.OnButtonPress) then
                EventContainer.OnButtonPress(Joystick, Button);
              Joystick.State.BtnCanPress[Button] := false;
            end;
            Joystick.State.BtnDown[Button] := Btn <> 0;
            Joystick.State.BtnUp[Button] := Btn = 0;
            if Assigned(EventContainer.OnButtonDown) and (Btn <> 0) then
              EventContainer.OnButtonDown(Joystick, Button);
          end;
        end;
      JOYERR_UNPLUGGED:
        begin
          WritelnLog('Joystick %s was disconnected', [Joystick.Info.Name]);
          JoystickHasBeenDisconnected := true;
        end;
      JOYERR_PARMS:
        WriteLnWarning('Joystick %s parameters are no longer valid.', [Joystick.Info.Name]);
      else
        WriteLnWarning('Joystick %s error %d', [Joystick.Info.Name, JoyError]);
    end;
    if JoystickHasBeenDisconnected then
      Joysticks.InternalDisconnected;
  end;
end;

end.
