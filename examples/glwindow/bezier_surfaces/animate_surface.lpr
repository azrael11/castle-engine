{
  Copyright 2006-2010 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Animate between two Bezier surfaces.
  Test on sample_data/*.animation files. }
program animate_surface;

uses Cameras, Surfaces, GLWindow, GL, GLU, VectorMath,
  KambiGLUtils, BezierCurve, Boxes3D, SysUtils, KambiUtils, KeysMouse,
  KambiStringUtils, GLWinMessages, KambiFilesUtils;

var
  Glw: TGLUIWindow;
  Camera: TWalkCamera;
  Surface1, Surface2: TSurface;
  SurfacePos, SurfaceDir, SurfaceUp: TVector3Single;
  SurfaceMoveSpeed: Single;
  F: TGLfloat = 0.0;
  FUp: boolean = true;

procedure CameraHome;
begin
  Camera.Init(Box3D(Vector3Single(0, 0, -1),
                    Vector3Single(1, 1,  1)), 0.0);
end;

procedure CameraScene;
begin
  Camera.Init(SurfacePos, SurfaceDir, SurfaceUp, SurfaceUp, SurfaceMoveSpeed, 0, 0);
end;

procedure SurfacesLoad(const FileName: string);
var
  N: Cardinal;
  F: TextFile;

  procedure Load(Surface: TSurface);
  var
    I, J: Integer;
    MyCurve: TRationalBezierCurve;
    V: TVector3Single;
  begin
    for I := 0 to N - 1 do
    begin
      MyCurve := TRationalBezierCurve.Create(Surface.XBegin, Surface.XEnd);
      for J := 0 to N - 1 do
      begin
        Read(F, V[0], V[1], V[2]);
        MyCurve.ControlPoints.Add(V);
        MyCurve.Weights.Add(1.0);
      end;
      Readln(F);
      MyCurve.UpdateControlPoints;
      Surface.Curves.Add(MyCurve);
    end;
  end;

begin
  Surface1 := TSurface.Create(0, 1, 0, 1);
  Surface2 := TSurface.Create(0, 1, 0, 1);

  SafeReset(F, FileName, true);
  try
    Readln(F, SurfacePos[0], SurfacePos[1], SurfacePos[2],
              SurfaceDir[0], SurfaceDir[1], SurfaceDir[2],
              SurfaceUp [0], SurfaceUp [1], SurfaceUp [2]);
    SurfaceMoveSpeed := VectorLen(SurfaceDir);
    NormalizeTo1st(SurfaceDir);
    Readln(F, N);
    Load(Surface1);
    Load(Surface2);
  finally CloseFile(F) end;

  CameraScene;
end;

procedure Draw(glwin: TGLWindow);
const
  SurfaceXSegments = 20;
  SurfaceYSegments = 20;
var
  Surface: TSurface;
  I, J: Integer;
  C1, C2, MyCurve: TRationalBezierCurve;
begin
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glLoadMatrix(Camera.Matrix);

  glColorv(White3Single);

  Surface := TSurface.Create(
    Surface1.XBegin, Surface1.XEnd,
    Surface1.YBegin, Surface1.YEnd);
  try
    for I := 0 to Surface1.Curves.High do
    begin
      C1 := Surface1.Curves.Items[I] as TRationalBezierCurve;
      C2 := Surface2.Curves.Items[I] as TRationalBezierCurve;
      MyCurve := TRationalBezierCurve.Create(C1.TBegin, C1.TEnd);
      for J := 0 to C1.ControlPoints.High do
      begin
        MyCurve.ControlPoints.Add(
          Lerp(F, C1.ControlPoints.Items[J],
                  C2.ControlPoints.Items[J]));
        MyCurve.Weights.Add(1.0);
      end;
      MyCurve.UpdateControlPoints;
      Surface.Curves.Add(MyCurve);
    end;

    Surface.Render(SurfaceXSegments, SurfaceYSegments);
  finally FreeAndNil(Surface) end;
end;

procedure Init(glwin: TGLWindow);
begin
  glEnable(GL_DEPTH_TEST);
  glEnable(GL_LIGHTING);
  glEnable(GL_LIGHT0);
  glEnable(GL_COLOR_MATERIAL);
  glShadeModel(GL_FLAT);
end;

procedure Resize(glwin: TGLWindow);
begin
  glViewport(0, 0, glwin.Width, glwin.Height);
  ProjectionGLPerspective(30, glwin.Width/glwin.Height, 0.1, 100);
end;

procedure Idle(Glwin: TGLWindow);
begin
  if FUp then
  begin
    F += 0.01 * Glwin.Fps.IdleSpeed * 50;
    if F >= 1.0 then
    begin
      F := 1.0;
      FUp := false;
    end;
  end else
  begin
    F -= 0.01 * Glwin.Fps.IdleSpeed * 50;
    if F <= 0.0 then
    begin
      F := 0.0;
      FUp := true;
    end;
  end;
end;

procedure KeyDown(Glwin: TGLWindow; Key: TKey; C: char);
begin
  case C of
    'c': begin
           Writeln(Format('%f %f %f   %f %f %f   %f %f %f',
             [ Camera.Position[0],
               Camera.Position[1],
               Camera.Position[2],
               Camera.Direction[0],
               Camera.Direction[1],
               Camera.Direction[2],
               Camera.Up[0],
               Camera.Up[1],
               Camera.Up[2] ]));
         end;
    'h': CameraHome;
    's': CameraScene;
  end;
end;

begin
  Glw := TGLUIWindow.Create(Application);

  Camera := TWalkCamera.Create(Glw);
  Camera.PreferGravityUpForRotations := false;
  Camera.PreferGravityUpForMoving := false;
  Glw.Controls.Add(Camera);

  Parameters.CheckHigh(1);
  SurfacesLoad(Parameters[1]);
  try
    Glw.OnInit := @Init;
    Glw.OnResize := @Resize;
    Glw.OnIdle := @Idle;
    Glw.OnDraw := @Draw;
    Glw.OnKeyDown := @KeyDown;

    Glw.AutoRedisplay := true;

    Glw.InitAndRun;
  finally
    FreeAndNil(Surface1);
    FreeAndNil(Surface2);
  end;
end.
