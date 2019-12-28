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
  CastleKeysMouse, CastleControls, CastleUIState;

type
  TStateMain = class(TUiState)
  strict private

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

