{ Demo of TVRMLGLAnimation class. In other words, this loads and displays
  animations of "Kambi VRML game engine".

  If you're looking for example animation models,
  see inside kanim/ subdirectory of
  [http://vrmlengine.sourceforge.net/kanim_format.php].
  If you're looking for a real, full-featured program to read
  kanim animations, see
  [http://vrmlengine.sourceforge.net/view3dscene.php].
  This program is only a demo for a programmer to see how easy
  it's to load animations in your own programs using our engine.

  1. Run this passing one command-line parameter:
     name of *.kanim file that describes the animation.

     Example commands:
      ./demo_animation sphere.kanim
      ./demo_animation raptor.kanim
      ./demo_animation gus.kanim
      ./demo_animation cube_opening.kanim
      ./demo_animation gus3.kanim

  2. Alternatively, more "manual" method:
     Run this passing an even number of command-line parameters.
     Each parameters pair specifies scene filename, and position in time
     of this scene. Scenes must be specified in increasing order of time.
     Time is in seconds. Animation goes from 1st scene to the 2nd,
     then to the 3rd etc. to the last scene.

     Example command with two scenes:
       ./demo_animation sphere_1.wrl 0 sphere_2.wrl 1
     Example command with more scenes:
       ./demo_animation gus_1_final.wrl 0 \
                        gus_2_final.wrl 1 \
                        gus_3_final.wrl 1.5 --backwards

   Additional command-line options:
     --loop
     --backwards
     --no-loop
     --no-backwards
   For precise meaning, see TVRMLGLAnimation documentation.
   In short, --loop causes animation to loop and --backwards causes
   animation to go backward after going forward.
   If you load from *.kanim file, then the default loop/backwards
   settings are loaded from this file. Otherwise, the default is
   --loop --no-backwards.

  This is all implemented in TVRMLGLAnimation class, see docs of this class
  for precise description how things work.

  You can navigate in the scene using the standard arrow keys, escape exits.
  (for full list of supported keys --- see view3dscene documentation,
  [http://vrmlengine.sourceforge.net/view3dscene.php],
  at Walk navigation method).
  Space key restarts the animation (definitely useful if you passed
  --no-loop option).

  At the beginning there is some preprocessing time
  ("Preparing animation") when we create display lists,
  to make future animation run smoothly.
  That's done by TVRMLGLAnimation.PrepareRender.
}

program demo_animation;

uses Math, VectorMath, Boxes3d, VRMLNodes, VRMLOpenGLRenderer, OpenGLh, GLWindow,
  GLW_Navigated, KambiClassUtils, KambiUtils, SysUtils, Classes, Object3dAsVRML,
  KambiGLUtils, VRMLFlatScene, VRMLFlatSceneGL, MatrixNavigation, VRMLGLAnimation,
  KambiFilesUtils, ParseParametersUnit, ProgressGL, ProgressUnit, VRMLErrors;

var
  Animation: TVRMLGLAnimation;
  AnimationTime: Float = 0.0;

procedure Draw(glwin: TGLWindow);
begin
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glLoadMatrix(glw.Navigator.Matrix);

  Animation.SceneFromTime(AnimationTime).Render(nil, tgAll);
end;

procedure Idle(glwin: TGLWindow);
begin
  AnimationTime += glwin.IdleCompSpeed / 50;
end;

procedure Init(glwin: TGLWindow);
begin
  glEnable(GL_LIGHTING);
  glEnable(GL_LIGHT0);
  AnimationTime := Animation.TimeBegin;
end;

procedure Close(glwin: TGLWindow);
begin
  Animation.CloseGL;
end;

procedure Resize(glwin: TGLWindow);
begin
  glViewport(0, 0, glwin.Width, glwin.Height);
  ProjectionGLPerspective(45.0, glwin.Width/glwin.Height,
    Box3dMaxSize(Animation.Scenes[0].BoundingBox) * 0.05,
    Box3dMaxSize(Animation.Scenes[0].BoundingBox) * 10.0);
end;

procedure KeyDown(Glwin: TGLWindow; Key: TKey; C: char);
begin
  if C = ' ' then
    AnimationTime := 0.0;
end;

procedure LoadAnimationFromCommandLine(Animation: TVRMLGLAnimation);
const
  { These are constants used only with "manual" method
    (even number of command-line params),
    in case of *.kanim file these informations are read from *.kanim file. }
  { This is the number of animation frames constructed per one unit of time.
    Increase this to get smoother animation. }
  ScenesPerTime = 50;
  { EqualityEpsilon used to marge nodes when creating animation.
    Larger values may speed up animation loading time and save memory use. }
  EqualityEpsilon = 0.001;
  RendererOptimization: TGLRendererOptimization = roSeparateShapeStatesNoTransform;
var
  AnimRootNodes: TVRMLNodesList;
  AnimTimes: TDynSingleArray;
  I: Integer;
begin
  { parse parameters to AnimRootNodes and AnimTimes }
  if (Parameters.High = 0) or Odd(Parameters.High) then
    raise EInvalidParams.Create('You must supply even number of paramaters: ' +
      '2 parameters "<scene> <time>" for each frame');

  AnimRootNodes := nil;
  AnimTimes := nil;
  try
    AnimRootNodes := TVRMLNodesList.Create;
    AnimTimes := TDynSingleArray.Create;

    AnimRootNodes.Count := Parameters.High div 2;
    AnimTimes    .Count := Parameters.High div 2;

    for I := 0 to Parameters.High div 2 - 1 do
    begin
      AnimRootNodes[I] := LoadAsVRML(Parameters[(I+1) * 2 - 1], false);
      AnimTimes[I] := StrToFloat(Parameters[(I+1) * 2]);
    end;

    Animation.Load(AnimRootNodes, true,
      AnimTimes, ScenesPerTime, RendererOptimization, EqualityEpsilon);

    Animation.TimeLoop := true;
    Animation.TimeBackwards := true;
  finally
    FreeAndNil(AnimRootNodes);
    FreeAndNil(AnimTimes);
  end;
end;

var
  WasParam_AnimTimeLoop: boolean = false;
  Param_AnimTimeLoop: boolean;
  WasParam_AnimTimeBackwards: boolean = false;
  Param_AnimTimeBackwards: boolean;

const
  Options: array[0..3] of TOption =
  (
    (Short:  #0; Long: 'loop'; Argument: oaNone),
    (Short:  #0; Long: 'backwards'; Argument: oaNone),
    (Short:  #0; Long: 'no-loop'; Argument: oaNone),
    (Short:  #0; Long: 'no-backwards'; Argument: oaNone)
  );

  procedure OptionProc(OptionNum: Integer; HasArgument: boolean;
    const Argument: string; const SeparateArgs: TSeparateArgs; Data: Pointer);
  begin
    case OptionNum of
      0: begin WasParam_AnimTimeLoop      := true; Param_AnimTimeLoop      := true; end;
      1: begin WasParam_AnimTimeBackwards := true; Param_AnimTimeBackwards := true; end;
      2: begin WasParam_AnimTimeLoop      := true; Param_AnimTimeLoop      := false; end;
      3: begin WasParam_AnimTimeBackwards := true; Param_AnimTimeBackwards := false; end;
      else raise EInternalError.Create('OptionProc');
    end;
  end;

var
  CamPos, CamDir, CamUp, GravityUp: TVector3Single;
begin
  Glw.ParseParameters(StandardParseOptions);
  ParseParameters(Options, @OptionProc, nil);

  try
    VRMLNonFatalError := @VRMLNonFatalError_WarningWrite;

    Animation := TVRMLGLAnimation.Create;
    if Parameters.High = 1 then
    begin
      { 1st method: *.kanim file }
      Animation.LoadFromFile(Parameters[1]);
    end else
    begin
      { 2nd method: even number of command-line params }
      LoadAnimationFromCommandLine(Animation);
    end;

    if WasParam_AnimTimeLoop then
      Animation.TimeLoop := Param_AnimTimeLoop;
    if WasParam_AnimTimeBackwards then
      Animation.TimeBackwards := Param_AnimTimeBackwards;

    { get camera from 1st scene in Animation }
    Animation.Scenes[0].GetPerspectiveViewpoint(CamPos, CamDir, CamUp,
      GravityUp);

    { init Glw.Navigator }
    Glw.Navigator := TMatrixWalker.Create(@Glw.PostRedisplayOnMatrixChanged);
    Glw.NavWalker.Init(CamPos, VectorAdjustToLength(CamDir,
      Box3dAvgSize(Animation.Scenes[0].BoundingBox) * 0.01*0.4), CamUp,
      GravityUp, 0.0, 0.0);

    ProgressGLInterface.Window := Glw;
    Progress.UserInterface := ProgressGLInterface;

    Glw.AutoRedisplay := true;
    Glw.OnInit := @Init;
    Glw.OnClose := @Close;
    Glw.OnResize := @Resize;
    Glw.OnIdle := @Idle;
    Glw.OnKeyDown := @KeyDown;
    Glw.Caption := ProgramName;
    Glw.OnDraw := @Draw;

    Glw.Init;

    Progress.Init(Animation.ScenesCount, 'Preparing animation');
    try
      Animation.PrepareRender([tgAll], [prBoundingBox], true, false);
    finally Progress.Fini end;

    Glwm.Loop;
  finally Animation.Free end;
end.
