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

  WINMMLIB = 'winmm.dll';

function joyGetNumDevs : LongWord; stdcall; external WINMMLIB name 'joyGetNumDevs';
function joyGetDevCapsW( uJoyID : LongWord; lpCaps : PJOYCAPSW; uSize : LongWord ) : LongWord; stdcall; external WINMMLIB name 'joyGetDevCapsW';
function joyGetPosEx( uJoyID : LongWord; lpInfo : PJOYINFOEX ) : LongWord; stdcall; external WINMMLIB name 'joyGetPosEx';

type
  TWindowsJoystick = class(TJoystick)
    Caps    : TJOYCAPSW;
  end;

  TWindowsJoysticksBackend = class(TJoysticksBackend)
    procedure Initialize(const List: TJoystickList); override;
    procedure Poll(const List: TJoystickList;
      const EventContainer: TJoysticks); override;
  end;

implementation

uses
  SysUtils, CastleLog, Math;

procedure TWindowsJoysticksBackend.Initialize(const List: TJoystickList);
var
  i, j : Integer;
  axis : Integer;
  caps : PLongWord;
  NewJoystick: TWindowsJoystick;
begin
  for i := 0 to joyGetNumDevs() - 1 do
  begin
    NewJoystick := TWindowsJoystick.Create;

    if joyGetDevCapsW( i, @NewJoystick.Caps, SizeOf( TJOYCAPSW ) ) = 0 then
    begin
      NewJoystick.Info.Name          := NewJoystick.Caps.szPname;
      NewJoystick.Info.Count.Axes    := NewJoystick.Caps.wNumAxes;
      NewJoystick.Info.Count.Buttons := NewJoystick.Caps.wNumButtons;

      caps  := @NewJoystick.Info.Caps;
      if NewJoystick.Caps.wCaps and JOYCAPS_HASZ > 0 then
      begin
        caps^ := caps^ or JOY_HAS_Z;
      end;
      if NewJoystick.Caps.wCaps and JOYCAPS_HASR > 0 then
      begin
        caps^ := caps^ or JOY_HAS_R;
      end;
      if NewJoystick.Caps.wCaps and JOYCAPS_HASU > 0 then
      begin
        caps^ := caps^ or JOY_HAS_U;
      end;
      if NewJoystick.Caps.wCaps and JOYCAPS_HASV > 0 then
      begin
        caps^ := caps^ or JOY_HAS_V;
      end;
      if NewJoystick.Caps.wCaps and JOYCAPS_HASPOV > 0 then
      begin
        caps^ := caps^ or JOY_HAS_POV;
      end;

      WritelnLog('CastleJoysticks Init', 'Find joy: %s (ID: %d); Axes: %d; Buttons: %d',
                 [NewJoystick.Info.Name, i, NewJoystick.Info.Count.Axes, NewJoystick.Info.Count.Buttons]);

      List.Add(NewJoystick);
    end else
      FreeAndNil(NewJoystick);
  end;
end;

procedure TWindowsJoysticksBackend.Poll(const List: TJoystickList;
  const EventContainer: TJoysticks);
var
  i : Integer;
  AxisValue: Single;
  j, a  : Integer;
  btn   : Integer;
  state : TJOYINFOEX;
  pcaps : PLongWord;
  value : PLongWord;
  vMin  : LongWord;
  vMax  : LongWord;
  Joystick: TWindowsJoystick;
begin
  state.dwSize := SizeOf( TJOYINFOEX );
  for I := 0 to List.Count - 1 do
  begin
    Joystick := List[I] as TWindowsJoystick;

    state.dwFlags := JOY_RETURNALL or JOY_USEDEADZONE;
    if Joystick.Caps.wCaps and JOYCAPS_POVCTS > 0 then
      state.dwFlags := state.dwFlags or JOY_RETURNPOVCTS;

    if joyGetPosEx( i, @state ) = 0 then
    begin
      for j := 0 to Joystick.Info.Count.Axes - 1 do
      begin
        case j of
          JOY_AXIS_X: AxisValue := State.dwXpos / (Joystick.Caps.wXmax - Joystick.Caps.wXmin) * 2 - 1;
          JOY_AXIS_Y: AxisValue := State.dwYpos / (Joystick.Caps.wYmax - Joystick.Caps.wYmin) * 2 - 1;
          JOY_AXIS_Z: AxisValue := State.dwZpos / (Joystick.Caps.wZmax - Joystick.Caps.wZmin) * 2 - 1;
          JOY_AXIS_R: AxisValue := State.dwRpos / (Joystick.Caps.wRmax - Joystick.Caps.wRmin) * 2 - 1;
          JOY_AXIS_U: AxisValue := State.dwUpos / (Joystick.Caps.wUmax - Joystick.Caps.wUmin) * 2 - 1;
          JOY_AXIS_V: AxisValue := State.dwVpos / (Joystick.Caps.wVmax - Joystick.Caps.wVmin) * 2 - 1;
          else
            AxisValue := 0;
        end;

        if Joystick.State.Axis[ j ] <> AxisValue then
          if Assigned(EventContainer.OnAxisMove) then
            EventContainer.OnAxisMove(Joystick, j, AxisValue);
        Joystick.State.Axis[ j ] := AxisValue;
      end;

      //BUG: pressing dpad up then down may result in up not released
      if ( Joystick.Info.Caps and JOY_HAS_POV > 0 ) then
      begin
        if ( state.dwPOV and $FFFF <> $FFFF ) then
          AxisValue := Sin( DegToRad(state.dwPOV and $FFFF / 100.0) )
        else
          AxisValue := 0;

        if Joystick.State.Axis[ JOY_NEWPOVX ] <> AxisValue then
          if Assigned(EventContainer.OnAxisMove) then
            EventContainer.OnAxisMove(Joystick, JOY_NEWPOVX, AxisValue);
        Joystick.State.Axis[ JOY_NEWPOVX ] := AxisValue;

        if ( state.dwPOV and $FFFF <> $FFFF ) then
          AxisValue := -Cos( DegToRad(state.dwPOV and $FFFF / 100.0 ) )
        else
          AxisValue := 0;
        if Joystick.State.Axis[ JOY_NEWPOVY ] <> AxisValue then
          if Assigned(EventContainer.OnAxisMove) then
            EventContainer.OnAxisMove(Joystick, JOY_NEWPOVY, AxisValue);
        Joystick.State.Axis[ JOY_NEWPOVY ] := AxisValue;
      end;

      for j := 0 to Joystick.Info.Count.Buttons - 1 do
      begin
        btn := state.wButtons and ( 1 shl j );
        if ( Joystick.State.BtnDown[ j ] ) and ( btn = 0 ) then
        begin
          Joystick.State.BtnPress[ j ] := False;
          if Assigned(EventContainer.OnButtonUp) then EventContainer.OnButtonUp(Joystick, j);
          Joystick.State.BtnCanPress[ j ] := True;
        end;

        if ( Joystick.State.BtnCanPress[ j ] ) and ( not Joystick.State.BtnDown[ j ] ) and ( btn <> 0 ) then
        begin
          Joystick.State.BtnPress   [ j ] := True;
          if Assigned(EventContainer.OnButtonPress) then EventContainer.OnButtonPress(Joystick, j);
          Joystick.State.BtnCanPress[ j ] := False;
        end;
        Joystick.State.BtnDown[ j ] := btn <> 0;
        Joystick.State.BtnUp[ j ] := btn = 0;
        if Assigned(EventContainer.OnButtonDown) and (btn <> 0) then EventContainer.OnButtonDown(Joystick, j);
      end;
    end;
  end;
end;

end.
