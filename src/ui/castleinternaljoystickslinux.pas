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

{ Linux Joystick support. }
unit CastleInternalJoysticksLinux;

// TODO: drop legacy interface /dev/js#; enable /dev/event#

interface

uses BaseUnix,
  CastleJoysticks;

type
  TLinuxJsEvent = record
    time   : LongWord; // event timestamp in milliseconds
    value  : SmallInt; // value
    EventType  : Byte;     // event type
    number : Byte;     // axis/button number
  end;

const
  ABS_MAX = $3F;

  JS_EVENT_BUTTON = $01; // button pressed/released
  JS_EVENT_AXIS   = $02; // joystick moved
  JS_EVENT_INIT   = $80; // initial state of device

  JSIOCGNAME    = -2142213613;
  JSIOCGAXMAP   = -2143262158;
  JSIOCGAXES    = -2147390959;
  JSIOCGBUTTONS = -2147390958;

  JS_AXIS : array[ 0..17 ] of Byte = ( JOY_AXIS_X, JOY_AXIS_Y, JOY_AXIS_Z, JOY_AXIS_U, JOY_AXIS_V, JOY_AXIS_R, JOY_AXIS_Z, JOY_AXIS_R, 0, 0, 0, 0, 0, 0, 0, 0, JOY_POVX, JOY_POVY );

type
  TLinuxJoystickBackendInfo = class
    DeviceInitialized: Boolean;
    Device  : LongInt;
    AxesMap : array[ 0..ABS_MAX - 1 ] of Byte;
    destructor Destroy; override;
  end;

  TLinuxJoysticksBackend = class(TJoysticksBackend)
    procedure Initialize(const List: TJoystickList); override;
    procedure Poll(const List: TJoystickList;
      const EventContainer: TJoysticks); override;
  end;

implementation

uses
  SysUtils, CastleLog, Math;

{ TLinuxJoystickBackendInfo -------------------------------------------------- }

destructor TLinuxJoystickBackendInfo.Destroy;
begin
  { Check for DeviceInitialized, since any Device >= 0 is valid in theory. }
  if DeviceInitialized then
    FpClose(Device);
  inherited;
end;

{ TLinuxJoysticksBackend ----------------------------------------------------- }

procedure TLinuxJoysticksBackend.Initialize(const List: TJoystickList);
var
  I, J: Integer;
  NewJoystick: TJoystick;
  NewBackendInfo: TLinuxJoystickBackendInfo;
begin
  for I := 0 to 15 do
  begin
    NewJoystick := TJoystick.Create;
    NewBackendInfo := TLinuxJoystickBackendInfo.Create;
    NewJoystick.InternalBackendInfo := NewBackendInfo;

    NewBackendInfo.Device := FpOpen( '/dev/input/js' + IntToStr(I), O_RDONLY or O_NONBLOCK );
    if NewBackendInfo.Device < 0 then
      NewBackendInfo.Device := FpOpen( '/dev/js' + IntToStr(I), O_RDONLY or O_NONBLOCK );

    if NewBackendInfo.Device > -1 then
    begin
      NewBackendInfo.DeviceInitialized := true;
      SetLength(NewJoystick.Info.Name, 256);
      FpIOCtl(NewBackendInfo.Device, JSIOCGNAME,    @NewJoystick.Info.Name[1]);
      FpIOCtl(NewBackendInfo.Device, JSIOCGAXMAP,   @NewBackendInfo.AxesMap[0]);
      FpIOCtl(NewBackendInfo.Device, JSIOCGAXES,    @NewJoystick.Info.Count.Axes);
      FpIOCtl(NewBackendInfo.Device, JSIOCGBUTTONS, @NewJoystick.Info.Count.Buttons);

      for J := 1 to 255 do
        if NewJoystick.Info.Name[J] = #0 then
          begin
            SetLength(NewJoystick.Info.Name, J - 1);
            Break;
          end;

      // Checking if joystick is a real one, because laptops with accelerometer can be detected as a joystick :)
      // Note, that "030000000d0f00000d00000000010000,hori" doesn't have axes - only buttons and this condition will fail
      if (NewJoystick.Info.Count.Axes >= 2) and (NewJoystick.Info.Count.Buttons > 0) then
      begin
        WritelnLog('CastleJoysticks Init', 'Find joy: %s (ID: %d); Axes: %d; Buttons: %d', [NewJoystick.Info.Name, I, NewJoystick.Info.Count.Axes, NewJoystick.Info.Count.Buttons]);
        List.Add(NewJoystick);
      end else
        FreeAndNil(NewJoystick);
    end else
      FreeAndNil(NewJoystick);
  end;
end;

procedure TLinuxJoysticksBackend.Poll(const List: TJoystickList;
  const EventContainer: TJoysticks);
var
  I: Integer;
  Value: Single;
  Axis: Byte;
  Event : TLinuxJsEvent;
  Joystick: TJoystick;
  BackendInfo: TLinuxJoystickBackendInfo;
begin
  for I := 0 to List.Count - 1 do
  begin
    Joystick := List[I];
    BackendInfo := Joystick.InternalBackendInfo as TLinuxJoystickBackendInfo;
    while FpRead(BackendInfo.Device, Event, 8) = 8 do
      case Event.EventType of
        JS_EVENT_AXIS:
          begin
            Axis := Event.number;

            { Obsolete Linux backend reports POV as 16,17 axes
              however "physically" they are located at the end of "real axes"
              which forces us to use the provided BackendInfo.AxesMap }
            if BackendInfo.AxesMap[Event.number] = 16 then
              Axis := JOY_NEWPOVX;
            if BackendInfo.AxesMap[Event.number] = 17 then
              Axis := JOY_NEWPOVY;

            Value := Event.value / 32767;
            Joystick.State.Axis[Axis] := Value;
            if Assigned(EventContainer.OnAxisMove) then
              EventContainer.OnAxisMove(Joystick, Axis, Value);
          end;
        JS_EVENT_BUTTON:
          case Event.value of
            0:
              begin
                if Joystick.State.BtnDown[Event.number] then
                begin
                  Joystick.State.BtnUp[Event.number] := true;
                  Joystick.State.BtnPress[Event.number] := false;
                  if Assigned(EventContainer.OnButtonUp) then
                    EventContainer.OnButtonUp(Joystick, Event.number);
                  Joystick.State.BtnCanPress[Event.number] := true;
                end;

                Joystick.State.BtnDown[Event.number] := false;
              end;
            1:
              begin
                Joystick.State.BtnDown[Event.number] := true;
                if Assigned(EventContainer.OnButtonDown) then
                  EventContainer.OnButtonDown(Joystick, Event.number);
                Joystick.State.BtnUp[Event.number] := False;
                if Joystick.State.BtnCanPress[Event.number] then
                  begin
                    Joystick.State.BtnPress[Event.number] := true;
                    if Assigned(EventContainer.OnButtonPress) then
                      EventContainer.OnButtonPress(Joystick, Event.number);
                    Joystick.State.BtnCanPress[Event.number] := false;
                  end;
              end;
          end;
      end;
  end;
end;

end.
