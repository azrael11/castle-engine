{ Reading and using the database from https://github.com/gabomdq/SDL_GameControllerDB }
program JoystickDetector;

uses
  SysUtils,
  CastleJoystickManager, CastleJoysticks, CastleLog, CastleWindow, CastleApplicationProperties;

var
  Window: TCastleWindowBase;

type
  TJoyHandler = class
    procedure DoAxisMove(const Joy: TJoystick; const Axis: Byte; const Value: Single);
    procedure DoButtonDown(const Joy: TJoystick; const Button: Byte);
    procedure DoButtonUp(const Joy: TJoystick; const Button: Byte);
    procedure DoButtonPress(const Joy: TJoystick; const Button: Byte);
  end;

procedure TJoyHandler.DoAxisMove(const Joy: TJoystick; const Axis: Byte; const Value: Single);
begin
  WriteLnLog(Joy.Info.Name + 'AxisMove ' + IntToStr(Axis) + ':' + FloatToStr(Value));
end;
procedure TJoyHandler.DoButtonDown(const Joy: TJoystick; const Button: Byte);
begin
  WriteLnLog(Joy.Info.Name + 'ButtonDown ' + IntToStr(Button));
end;
procedure TJoyHandler.DoButtonUp(const Joy: TJoystick; const Button: Byte);
begin
  WriteLnLog(Joy.Info.Name + 'ButtonUp ' + IntToStr(Button));
end;
procedure TJoyHandler.DoButtonPress(const Joy: TJoystick; const Button: Byte);
begin
  WriteLnLog(Joy.Info.Name + 'ButtonPress ' + IntToStr(Button));
end;

procedure ApplicationInitialize;
var
  I: Integer;
  J: TJoystick;
begin
  JoystickManager.ParseJoysticksDatabase('castle-data:/gamecontrollerdb.txt');
  WriteLnLog('===================');
  for I := 0 to Pred(Joysticks.Count) do
  begin
    J := Joysticks[I];
    Joysticks.OnAxisMove := @TJoyHandler(nil).DoAxisMove;
    Joysticks.OnButtonDown := @TJoyHandler(nil).DoButtonDown;
    Joysticks.OnButtonUp := @TJoyHandler(nil).DoButtonUp;
    Joysticks.OnButtonPress := @TJoyHandler(nil).DoButtonPress;
    WriteLnLog('Joystick Name', J.Info.Name);
    WriteLnLog('Joystick Buttons', IntToStr(J.Info.Count.Buttons));
    WriteLnLog('Joystick Axes', IntToStr(J.Info.Count.Axes));
    WriteLnLog('Joystick Caps', IntToStr(J.Info.Caps));
    WriteLnLog('--------------------');
  end;
end;

begin
  InitializeLog;

  ApplicationProperties.ApplicationName := 'Joystick manager';
  Application.OnInitialize := @ApplicationInitialize;
  Window := TCastleWindowBase.Create(Application);
  Application.MainWindow := Window;

  Application.MainWindow.OpenAndRun;
end.

