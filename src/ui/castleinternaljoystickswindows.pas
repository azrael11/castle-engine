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

// TODO: only partially works on Windows (no support of new features); need to be ported to xinput library

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
    wXpos: LongWord;
    wYpos: LongWord;
    wZpos: LongWord;
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

  JS_AXIS : array[ 0..5 ] of LongWord = ( 17 {X}, 19 {Y}, 21 {Z}, 26 {R}, 28 {U}, 30 {V} );

function joyGetNumDevs : LongWord; stdcall; external WINMMLIB name 'joyGetNumDevs';
function joyGetDevCapsW( uJoyID : LongWord; lpCaps : PJOYCAPSW; uSize : LongWord ) : LongWord; stdcall; external WINMMLIB name 'joyGetDevCapsW';
function joyGetPosEx( uJoyID : LongWord; lpInfo : PJOYINFOEX ) : LongWord; stdcall; external WINMMLIB name 'joyGetPosEx';

type
  TWindowsJoystickBackendInfo = class
    Caps    : TJOYCAPSW;
    AxesMap : array[ 0..5 ] of Byte;
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
  NewJoystick: TJoystick;
  NewBackendInfo: TWindowsJoystickBackendInfo;
begin
  j := joyGetNumDevs();
  for i := 0 to j - 1 do
  begin
    NewJoystick := TJoystick.Create;
    NewBackendInfo := TWindowsJoystickBackendInfo.Create;
    NewJoystick.InternalBackendInfo := NewBackendInfo;

    if joyGetDevCapsW( i, @NewBackendInfo.Caps, SizeOf( TJOYCAPSW ) ) = 0 then
    begin
      NewJoystick.Info.Name          := NewBackendInfo.Caps.szPname;
      NewJoystick.Info.Count.Axes    := NewBackendInfo.Caps.wNumAxes;
      NewJoystick.Info.Count.Buttons := NewBackendInfo.Caps.wNumButtons;

      caps  := @NewJoystick.Info.Caps;
      NewBackendInfo.AxesMap[ 0 ] := JOY_AXIS_X;
      NewBackendInfo.AxesMap[ 1 ] := JOY_AXIS_Y;
      axis := 2;
      if NewBackendInfo.Caps.wCaps and JOYCAPS_HASZ > 0 then
      begin
        caps^ := caps^ or JOY_HAS_Z;
        NewBackendInfo.AxesMap[ axis ] := JOY_AXIS_Z;
        Inc( axis );
      end;
      if NewBackendInfo.Caps.wCaps and JOYCAPS_HASR > 0 then
      begin
        caps^ := caps^ or JOY_HAS_R;
        NewBackendInfo.AxesMap[ axis ] := JOY_AXIS_R;
        Inc( axis );
      end;
      if NewBackendInfo.Caps.wCaps and JOYCAPS_HASU > 0 then
      begin
        caps^ := caps^ or JOY_HAS_U;
        NewBackendInfo.AxesMap[ axis ] := JOY_AXIS_U;
        Inc( axis );
      end;
      if NewBackendInfo.Caps.wCaps and JOYCAPS_HASV > 0 then
      begin
        caps^ := caps^ or JOY_HAS_V;
        NewBackendInfo.AxesMap[ axis ] := JOY_AXIS_V;
        //Inc( axis );
      end;
      if NewBackendInfo.Caps.wCaps and JOYCAPS_HASPOV > 0 then
      begin
        caps^ := caps^ or JOY_HAS_POV;
        Inc( NewJoystick.Info.Count.Axes, 2 );
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
  _value: Single;
  j, a  : Integer;
  btn   : Integer;
  state : TJOYINFOEX;
  pcaps : PLongWord;
  value : PLongWord;
  vMin  : LongWord;
  vMax  : LongWord;
  Joystick: TJoystick;
  BackendInfo: TWindowsJoystickBackendInfo;
begin
  state.dwSize := SizeOf( TJOYINFOEX );
  for I := 0 to List.Count - 1 do
  begin
    Joystick := List[I];
    BackendInfo := Joystick.InternalBackendInfo as TWindowsJoystickBackendInfo;

    state.dwFlags := JOY_RETURNALL or JOY_USEDEADZONE;
    if BackendInfo.Caps.wCaps and JOYCAPS_POVCTS > 0 then
      state.dwFlags := state.dwFlags or JOY_RETURNPOVCTS;

    if joyGetPosEx( i, @state ) = 0 then
    begin
      for j := 0 to Joystick.Info.Count.Axes - 1 do
      begin
        //stop if joystick reported more axes than the backend can handle
        if j > High(BackendInfo.AxesMap) then
          Break;

        // Say "no" to if's, and do everything trciky :)
        a     := BackendInfo.AxesMap[ j ];
        pcaps := @BackendInfo.Caps;
        Inc( pcaps, JS_AXIS[ a ] );
        vMin  := pcaps^;
        Inc( pcaps );
        vMax  := pcaps^;
        value := @state;
        Inc( value, 2 + a );

        _value := value^ / ( vMax - vMin ) * 2 - 1;

        if Joystick.State.Axis[ a ] <> _value then
          if Assigned(EventContainer.OnAxisMove) then
            EventContainer.OnAxisMove(Joystick, j, _value);
        Joystick.State.Axis[ a ] := _value;
      end;

      //FillChar( Joystick.State.Axis[ JOY_NEWPOVX ], 8, 0 ); //fills NEWPOVY too? As Single is 4 bytes long.
      if ( Joystick.Info.Caps and JOY_HAS_POV > 0 ) then
      begin
        if ( state.dwPOV and $FFFF <> $FFFF ) then
          _value := Sin( DegToRad(state.dwPOV and $FFFF / 100.0) )
        else
          _value := 0;

        if Joystick.State.Axis[ JOY_NEWPOVX ] <> _value then
          if Assigned(EventContainer.OnAxisMove) then
            EventContainer.OnAxisMove(Joystick, JOY_NEWPOVX, _value);
        Joystick.State.Axis[ JOY_NEWPOVX ] := _value;

        if ( state.dwPOV and $FFFF <> $FFFF ) then
          _value := -Cos( DegToRad(state.dwPOV and $FFFF / 100.0 ) )
        else
          _value := 0;
        if Joystick.State.Axis[ JOY_NEWPOVY ] <> _value then
          if Assigned(EventContainer.OnAxisMove) then
            EventContainer.OnAxisMove(Joystick, JOY_NEWPOVY, _value);
        Joystick.State.Axis[ JOY_NEWPOVY ] := _value;
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
