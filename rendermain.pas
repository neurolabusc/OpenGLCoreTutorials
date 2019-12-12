unit rendermain;
{$mode objfpc}{$H+}
//{$DEFINE TWO_PASS} //decide if we use single or two-pass raycasting
interface

uses
  glcorearb, gl_core_matrix, gl_core_utils, OpenGLContext,
  Classes, SysUtils, FileUtil,  Forms, Controls, Graphics,
  Dialogs, ExtCtrls, Menus,  Types, loadNifti,  math;

type
   TGLForm1 = class(TForm)
    ColorDialog1: TColorDialog;
    GLBox: TOpenGLControl;
    MainMenu1: TMainMenu;
    FileMenu: TMenuItem;
    BackColorMenu: TMenuItem;
    HelpMenu: TMenuItem;
    AboutMenu: TMenuItem;
    AppleMenu: TMenuItem;
    AppleAbout: TMenuItem;
    PerspectiveMenu: TMenuItem;
    OpenDialog1: TOpenDialog;
    ViewMenu: TMenuItem;
    OpenMenu: TMenuItem;
    procedure BackColorMenuClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure GLMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormResize(Sender: TObject);
    procedure GLboxPaint(Sender: TObject);
    procedure GLMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure GLMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure OpenMenuClick(Sender: TObject);
    procedure PerspectiveMenuClick(Sender: TObject);
    procedure ShaderCheckClick(Sender: TObject);
    procedure About1Click(Sender: TObject);

  private

  public

  end;

var
  GLForm1: TGLForm1;

implementation

{$R *.lfm}
type
TRayCast = record
  ScaleDim: TScale;
  Distance: single;
  LightAzimuth,LightElevation,
  slices, Azimuth,Elevation,WINDOW_WIDTH,WINDOW_HEIGHT : integer;
  gradientTexture3D, intensityTexture3D, vao, programRaycast: GLuint;
  BackColor : TRGBA;
  RayCastQuality1to10: integer;
  intensityVolLoc, gradientVolLoc,
  mvpLoc, imvLoc, lightPositionLoc, sliceSizeLoc, stepSizeLoc, loopsLoc, clearColorLoc: GLint;
  {$IFDEF TWO_PASS}
  viewHeightLoc, viewWidthLoc, mvpLocBackface, backFaceLoc: GLint;
  programBackface, backFrameBuffer, backTexture, backDepthBuffer: GLuint;
  {$ELSE}
  imvpLoc: GLint;
  {$ENDIF}
  isPerspective: boolean;
end;


const
  kDefaultDistance = 2.25;
  kMaxDistance = 40;

var
  gPrefs: TRayCast;
  gMouse : TPoint;

procedure TGLForm1.About1Click(Sender: TObject);
const
  kSamp = 72;
var
  s: QWord;
  i: integer;
   fpsstr: string;
begin
 s := gettickcount64;
 fpsstr := '';
 for i := 1 to kSamp do begin
     gPrefs.Azimuth := (gPrefs.Azimuth + 5) mod 360;
     GLbox.Repaint;
  end;
  fpsstr := ' FPS '+floattostr((kSamp*1000)/(gettickcount64-s)) ;
  MessageDlg(fpsstr,mtInformation,[mbAbort, mbOK],0);
end;

{$IFDEF TWO_PASS}
procedure  FrameBufferGL (initialSetup: boolean);
begin
  if not initialSetup then begin
     glDeleteTextures(1,@gPrefs.backTexture);
     glDeleteFramebuffers(1, @gPrefs.backFrameBuffer );
  end;
  glGenTextures(1, @gPrefs.backTexture);
  glGenFramebuffers(1, @gPrefs.backFrameBuffer );
  glBindTexture(GL_TEXTURE_2D, gPrefs.backTexture);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR); //GL_NEAREST
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_BORDER);//GL_REPEAT
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_BORDER);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, gPrefs.WINDOW_WIDTH, gPrefs.WINDOW_HEIGHT, 0, GL_RGBA, GL_FLOAT, nil);
  glBindFramebuffer(GL_FRAMEBUFFER, gPrefs.backFrameBuffer );
  glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, gPrefs.backTexture, 0);
end;
{$ENDIF}

procedure LoadBufferData (var vao: gluint);
var
  vtx : packed array[0..23] of GLfloat = (
      0,0,0,
      0,1,0,
      1,1,0,
      1,0,0,
      0,0,1,
      0,1,1,
      1,1,1,
      1,0,1
      ); //vtx = 8 vertex positions (corners) of cube
  idx : packed array[0..35] of GLuint = (
      0,2,1,
      0,3,2,
      4,5,6,
      4,6,7,
      0,1,5,
      0,5,4,
      3,6,2,
      3,7,6,
      1,6,5,
      1,2,6,
      0,4,7,
      0,7,3
      ); //idx = each cube has 6 faces, each composed of two triangles = 12 tri indices
    vbo_point, vbo : gluint;
begin  //vboCube, vaoCube,
  vbo_point := 0;
  vbo := 0;
  vao := 0;
  glGenBuffers(1, @vbo_point);
  glBindBuffer(GL_ARRAY_BUFFER, vbo_point);
  glBufferData(GL_ARRAY_BUFFER, 8*3*sizeof(GLfloat), @vtx[0], GL_STATIC_DRAW); //cube has 8 vertices, each 3 coordinates X,Y,Z
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glGenVertexArrays(1, @vao);
  // vao like a closure binding 3 buffer object: verlocdat vercoldat and veridxdat
  glBindVertexArray(vao);
  glBindBuffer(GL_ARRAY_BUFFER, vbo_point);
  glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 3 * sizeof(GLfloat), nil);
  glEnableVertexAttribArray(0); // for vertexloc
  glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, 3 * sizeof(GLfloat), nil);
  glEnableVertexAttribArray(1); // for vertexcol
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glGenBuffers(1, @vbo);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, vbo);
  glBufferData(GL_ELEMENT_ARRAY_BUFFER, 36*sizeof(GLuint), @idx[0], GL_STATIC_DRAW); //cube is 6 faces, 2 triangles per face, 3 indices per triangle
  //do not delete the VBOs! http://stackoverflow.com/questions/25167562/how-to-dispose-vbos-stored-in-a-vao
  //glDeleteBuffers(1, @vbo);
  //glDeleteBuffers(1, @vbo_point);
end;

procedure resizeGL( w,h: integer);  //GLBox.ClientWidth
var
   scale, whratio: single;
begin
     gPrefs.WINDOW_HEIGHT:= h;
     gPrefs.WINDOW_Width:= w;
     glViewport(0, 0, gPrefs.WINDOW_Width, gPrefs.WINDOW_HEIGHT);
     nglMatrixMode(nGL_PROJECTION);
     nglLoadIdentity();
     if gPrefs.isPerspective then
        ngluPerspective(40.0, w/h, 0.01, kMaxDistance)
     else begin
       if gPrefs.Distance = 0 then
          scale := 1
       else
           scale := 1/abs(kDefaultDistance/(gPrefs.Distance+1.0));
       whratio := w/h;
       nglOrtho(whratio*-0.5*scale,whratio*0.5*scale,-0.5*scale,0.5*scale, 0.01, kMaxDistance);
     end;
     {$IFDEF TWO_PASS}FrameBufferGL(false);{$ENDIF}
end;

procedure rotateGL;
begin
  nglMatrixMode(nGL_MODELVIEW);
  nglLoadIdentity();
  nglTranslatef(0,0,-gPrefs.Distance);
  nglRotatef(90-gPrefs.Elevation,-1,0,0);
  nglRotatef(gPrefs.Azimuth,0,0,1);
  nglTranslatef(-gPrefs.ScaleDim[1]/2,-gPrefs.ScaleDim[2]/2,-gPrefs.ScaleDim[3]/2);
  nglScalef(gPrefs.ScaleDim[1],gPrefs.ScaleDim[2],gPrefs.ScaleDim[3]); //for volumes that are rectangular not square
end;

procedure drawBox(isFront: boolean);
begin
  glViewport(0, 0, gPrefs.WINDOW_Width, gPrefs.WINDOW_HEIGHT);
  glClearColor(gPrefs.BackColor.r/255,gPrefs.BackColor.g/255,gPrefs.BackColor.b/255, 0);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glEnable(GL_CULL_FACE);
  if isFront then
    glCullFace(GL_FRONT)
  else
     glCullFace(GL_BACK);
  glBindVertexArray(gPrefs.vao);
  glDrawElements(GL_TRIANGLES, 36, GL_UNSIGNED_INT, nil);
  glDisable(GL_CULL_FACE);
end;

function ComputeStepSize (Quality1to10: integer): single;
var
  f: single;
begin
  f := Quality1to10;
  if (f <= 1) or (f > 10) then
    f := 5;
  f := (gPrefs.slices*0.25)+ (gPrefs.slices*1.75)* (f/10);
  result := 1/f;
end;

procedure sph2cartDeg90(Azimuth,Elevation: single; out lX,lY,lZ: single);
//convert spherical AZIMUTH,ELEVATION,RANGE to Cartesion
//see Matlab's [x,y,z] = sph2cart(THETA,PHI,R)
// reverse with cart2sph
var
  E,Phi,Theta: single;
begin
  E := Azimuth;
  while E < 0 do
    E := E + 360;
  while E > 360 do
    E := E - 360;
  Theta := DegToRad(E);
  E := Elevation;
  while E > 90 do
    E := E - 90;
  while E < -90 do
    E := E + 90;
  Phi := DegToRad(E);
  lX := cos(Phi)*cos(Theta);
  lY := cos(Phi)*sin(Theta);
  lZ := sin(Phi);
end;

function  Defuzz(x:  single):  single;
const
 fuzz : double = 1.0E-6;
BEGIN
   if  abs(x) < fuzz then
       result := 0.0
   else
     result := x
end;

procedure LightUniforms;
var
  lA,lB,lC,
  lX,lY,lZ: single;
  lmv: TnMat44;
begin
  sph2cartDeg90(gPrefs.LightAzimuth,gPrefs.LightElevation,lX,lY,lZ);
  if true then begin //gPrefs.RayCastViewCenteredLight then begin
    lA := lY;
    lB := lZ;
    lC := lX;
    lmv := transposeMat(ngl_ModelViewMatrix);
    lX := defuzz(lA*lmv[0,0]+lB*lmv[1,0]+lC*lmv[2,0]);
    lY := defuzz(lA*lmv[0,1]+lB*lmv[1,1]+lC*lmv[2,1]);
    lZ := defuzz(lA*lmv[0,2]+lB*lmv[1,2]+lC*lmv[2,2]);
  end;
  lA := sqrt(sqr(lX)+sqr(lY)+Sqr(lZ));
  if lA > 0 then begin //normalize
    lX := lX/lA;
    lY := lY/lA;
    lZ := lZ/lA;
  end;
  glUniform3f(gPrefs.lightPositionLoc,lX,lY,lZ);
end;

procedure TGLForm1.GLboxPaint(Sender: TObject);
var
  mat44 : TnMat44;
begin
 rotateGL;
  mat44 := ngl_ModelViewProjectionMatrix;
  {$IFDEF TWO_PASS}
  //1st pass: create backface of cube
  glUseProgram(gPrefs.programBackface);
  glBindFramebuffer(GL_DRAW_FRAMEBUFFER, gPrefs.backFrameBuffer); //draw offscreen
  glUniformMatrix4fv(gPrefs.mvpLocBackface, 1, GL_FALSE, @mat44[0,0]);
  drawBox(false);
  {$ENDIF}
  glUseProgram(gPrefs.programRaycast);
  {$IFNDEF LCLgtk3} //https://stackoverflow.com/questions/47613181/opengl-strange-framebuffer-behavior-with-gtk-gl-area
  glBindFramebuffer(GL_FRAMEBUFFER, 0); //draw to screen
  {$ENDIF}
  glActiveTexture(GL_TEXTURE2);
  glBindTexture(GL_TEXTURE_3D, gPrefs.intensityTexture3D);
  glUniform1i(gPrefs.intensityVolLoc, 2);
  glActiveTexture(GL_TEXTURE3);
  glBindTexture(GL_TEXTURE_3D, gPrefs.gradientTexture3D);
  glUniform1i(gPrefs.gradientVolLoc, 3);
  glUniform1f(gPrefs.stepSizeLoc, ComputeStepSize(gPrefs.RayCastQuality1to10)) ;
  glUniform1f(gPrefs.sliceSizeLoc, 1/gPrefs.slices);
  glUniform1i( gPrefs.loopsLoc,round(gPrefs.slices*2.2));
  LightUniforms;
  glUniform3f(gPrefs.clearColorLoc,gPrefs.BackColor.r/255,gPrefs.BackColor.g/255,gPrefs.BackColor.b/255);
  glUniformMatrix4fv(gPrefs.mvpLoc, 1, GL_FALSE, @mat44[0,0]);
  mat44 := ngl_ModelViewMatrixInverse();
  glUniformMatrix4fv(gPrefs.imvLoc, 1, GL_FALSE, @mat44[0,0]);
  {$IFDEF TWO_PASS}
  glUniform1f(gPrefs.viewHeightLoc, gPrefs.WINDOW_HEIGHT);
  glUniform1f(gPrefs.viewWidthLoc, gPrefs.WINDOW_WIDTH);
  glActiveTexture(GL_TEXTURE1);
  glBindTexture(GL_TEXTURE_2D, gPrefs.backTexture);
  glUniform1i(gPrefs.backFaceLoc, 1);
  {$ELSE}
  mat44 := ngl_ModelViewProjectionMatrixInverse;
  glUniformMatrix4fv(gPrefs.imvpLoc, 1, GL_FALSE, @mat44[0,0]);
  {$ENDIF}
  drawBox(true);
  GLBox.SwapBuffers;
end;

 {$IFDEF TWO_PASS}
 const kVert = '#version 330 core'
 +#10'layout(location = 0) in vec3 vPos;'
 +#10'out vec3 TexCoord1;'
 +#10'uniform mat4 ModelViewProjectionMatrix;'
 +#10'void main() {'
 +#10'    TexCoord1 = vPos;'
 +#10'    gl_Position = ModelViewProjectionMatrix * vec4(vPos, 1.0);'
 +#10'}';
  kFrag ='#version 330 core'
+#10'in vec3 TexCoord1;'
+#10'out vec4 FragColor;'
+#10'uniform int loops;'
+#10'uniform float stepSize, sliceSize, viewWidth, viewHeight;'
+#10'uniform sampler3D intensityVol, gradientVol;'
+#10'uniform sampler2D backFace;'
+#10'uniform vec3 clearColor,lightPosition;'
+#10'uniform float ambient = 1.0;'
+#10'uniform float diffuse = 0.3;'
+#10'uniform float specular = 0.25;'
+#10'uniform float shininess = 10.0;'
+#10'void main() {'
+#10'	vec3 start = TexCoord1.xyz;'
+#10'	vec3 backPosition = texture(backFace,vec2(gl_FragCoord.x/viewWidth,gl_FragCoord.y/viewHeight)).xyz;'
+#10'	if (backPosition == clearColor) discard;'
+#10'	vec3 dir = backPosition - start;'
+#10'	float len = length(dir);'
+#10'	dir = normalize(dir);'
+#10'	vec3 deltaDir = dir * stepSize;'
+#10'	vec4 colorSample,gradientSample,colAcc = vec4(0.0,0.0,0.0,0.0);'
+#10'	float lengthAcc = 0.0;'
+#10'	vec3 samplePos = start.xyz + deltaDir* (fract(sin(gl_FragCoord.x * 12.9898 + gl_FragCoord.y * 78.233) * 43758.5453));'
+#10'	vec4 prevNorm = vec4(0.0,0.0,0.0,0.0);'
+#10'	for(int i = 0; i < loops; i++) {'
+#10'		float tex = texture(intensityVol,samplePos).r;'
+#10'		colorSample.rgba = vec4(tex,tex,tex,tex);'
+#10'		colorSample.a = 1.0-pow((1.0 - colorSample.a), stepSize/sliceSize);'
+#10'		if (colorSample.a > 0.01) {'
+#10'			gradientSample= texture(gradientVol,samplePos);'
+#10'			gradientSample.rgb = normalize(gradientSample.rgb*2.0 - 1.0);'
+#10'			if (gradientSample.a < prevNorm.a)'
+#10'				gradientSample.rgb = prevNorm.rgb;'
+#10'			prevNorm = gradientSample;'
+#10'			float lightNormDot = dot(gradientSample.rgb, lightPosition);'
+#10'			vec3 a = colorSample.rgb * ambient;'
+#10'			vec3 d = max(lightNormDot, 0.0) * colorSample.rgb * diffuse;'
+#10'			float s =   specular * pow(max(dot(reflect(lightPosition, gradientSample.rgb), dir), 0.0), shininess);'
+#10'			colorSample.rgb = a + d + s;'
+#10'		}'
+#10'		colorSample.rgb *= colorSample.a;'
+#10'		colAcc= (1.0 - colAcc.a) * colorSample + colAcc;'
+#10'		samplePos += deltaDir;'
+#10'		lengthAcc += stepSize;'
+#10'		if ( lengthAcc >= len || colAcc.a > 0.95 )'
+#10'			break;'
+#10'	}'
+#10'	colAcc.a = colAcc.a/0.95;'
+#10'	if ( colAcc.a < 1.0 )'
+#10'		colAcc.rgb = mix(clearColor,colAcc.rgb,colAcc.a);'
+#10'	FragColor = colAcc;'
+#10'}';

const kVertBackface = '#version 330 core'
+#10'layout(location = 0) in vec3 vPos;'
+#10'out vec3 fClr;'
+#10'uniform mat4 ModelViewProjectionMatrix;'
+#10+'mat4 mvp;'
+#10'void main() {'
+#10'    fClr = vPos;'
+#10'    gl_Position = ModelViewProjectionMatrix * vec4(vPos, 1.0);'
+#10'}';

const kFragBackface = '#version 330 core'
+#10'in vec3 fClr;'
+#10'out vec4 FragColor;'
+#10'void main() {'
+#10'    FragColor = vec4(fClr, 1.0);'
+#10'}';
 {$ELSE TWO_PASS}
 //single-pass rendering inspired by http://prideout.net/blog/?p=64
 const kVert = '#version 330 core'
+#10'layout(location = 0) in vec3 vPos;'
+#10'out vec3 TexCoord1;'
+#10'out vec4 vPosition;'
+#10'uniform mat4 ModelViewProjectionMatrix;'
+#10'void main() {'
+#10'  TexCoord1 = vPos;'
+#10'  gl_Position = ModelViewProjectionMatrix * vec4(vPos, 1.0);'
+#10'  vPosition = gl_Position;'
+#10'}';

 kFrag ='#version 330 core'
+#10'in vec3 TexCoord1;'
+#10'out vec4 FragColor;'
+#10'in vec4 vPosition;'
+#10'uniform mat4 ModelViewMatrixInverse, ModelViewProjectionMatrixInverse;'
+#10'uniform int loops;'
+#10'uniform float stepSize, sliceSize;'
+#10'uniform sampler3D intensityVol, gradientVol;'
+#10'uniform vec3 clearColor,lightPosition;'
+#10'uniform float ambient = 1.0;'
+#10'uniform float diffuse = 0.3;'
+#10'uniform float specular = 0.25;'
+#10'uniform float shininess = 10.0;'
+#10'vec3 GetBackPosition (vec3 startPosition) { //when does ray exit unit cube http://prideout.net/blog/?p=64'
+#10'	//next line only works for orthographic projections:'
+#10'	// vec3 rayDir =  normalize(ModelViewProjectionMatrixInverse * vec4(0.0,0.0,1.0,0.0)).xyz;'
+#10'	//https://github.com/almarklein/visvis/blob/master/core/shaders_3.py'
+#10'	// Calculate ray. In projective view the result is ok at the vertices'
+#10'	// but in between there can be all kind of non-linear bending of the'
+#10'	// rays. To solve this, one should use a denser grid of vertex-texture'
+#10'	// pairs. In textures.py, this is done by partitioning the quads.'
+#10'	// Get location of vertex in device coordinates'
+#10'	//float w = max(1.0, vPosition.w);'
+#10'	vec4 refPos1 = vPosition * vPosition.w;'
+#10'	// Calculate point right behind it. Distance depends on w-value'
+#10'	// to prevent wobly artifacts at low field of views.'
+#10'	float zdist = max(1.0, vPosition.w/10.0);'
+#10'	zdist *= vPosition.z/abs(vPosition.z); //required for orthographic'
+#10'	vec4 refPos2 = refPos1 + vec4(0.0, 0.0, zdist, 0.0);'
+#10'	// Project back to world coordinates to calculate ray direction'
+#10'	vec4 p1 = ModelViewProjectionMatrixInverse * refPos1;'
+#10'	vec4 p2 = ModelViewProjectionMatrixInverse * refPos2;'
+#10'	vec3 rayDir = normalize((p1.xyz/p1.w)-(p2.xyz/p2.w) );'
+#10'	vec3 invR = 1.0 / rayDir;'
+#10'    vec3 tbot = invR * (vec3(0.0)-startPosition);'
+#10'    vec3 ttop = invR * (vec3(1.0)-startPosition);'
+#10'    vec3 tmax = max(ttop, tbot);'
+#10'    vec2 t = min(tmax.xx, tmax.yz);'
+#10'	return startPosition + (rayDir * min(t.x, t.y));'
+#10'}'
+#10'void main() {'
+#10'	vec3 start = TexCoord1.xyz;'
+#10'	vec3 backPosition = GetBackPosition(start);'
+#10'	//FragColor = vec4(start, 1.0); return;'
+#10'	//FragColor = vec4(backPosition, 1.0); return;'
+#10'	vec3 dir = backPosition - start;'
+#10'	float len = length(dir);'
+#10'	dir = normalize(dir);'
+#10'	vec3 deltaDir = dir * stepSize;'
+#10'	vec4 colorSample,gradientSample,colAcc = vec4(0.0,0.0,0.0,0.0);'
+#10'	float lengthAcc = 0.0;'
+#10'	vec3 samplePos = start.xyz + deltaDir* (fract(sin(gl_FragCoord.x * 12.9898 + gl_FragCoord.y * 78.233) * 43758.5453));'
+#10'	vec4 prevNorm = vec4(0.0,0.0,0.0,0.0);'
+#10'	for(int i = 0; i < loops; i++) {'
+#10'		float tex = texture(intensityVol,samplePos).r;'
+#10'		colorSample.rgba = vec4(tex,tex,tex,tex);'
+#10'		colorSample.a = 1.0-pow((1.0 - colorSample.a), stepSize/sliceSize);'
+#10'		if (colorSample.a > 0.01) {'
+#10'			gradientSample= texture(gradientVol,samplePos);'
+#10'			gradientSample.rgb = normalize(gradientSample.rgb*2.0 - 1.0);'
+#10'			if (gradientSample.a < prevNorm.a)'
+#10'				gradientSample.rgb = prevNorm.rgb;'
+#10'			prevNorm = gradientSample;'
+#10'			float lightNormDot = dot(gradientSample.rgb, lightPosition);'
+#10'			vec3 a = colorSample.rgb * ambient;'
+#10'			vec3 d = max(lightNormDot, 0.0) * colorSample.rgb * diffuse;'
+#10'			float s =   specular * pow(max(dot(reflect(lightPosition, gradientSample.rgb), dir), 0.0), shininess);'
+#10'			colorSample.rgb = a + d + s;'
+#10'		}'
+#10'		colorSample.rgb *= colorSample.a;'
+#10'		colAcc= (1.0 - colAcc.a) * colorSample + colAcc;'
+#10'		samplePos += deltaDir;'
+#10'		lengthAcc += stepSize;'
+#10'		if ( lengthAcc >= len || colAcc.a > 0.95 )'
+#10'			break;'
+#10'	}'
+#10'	colAcc.a = colAcc.a/0.95;'
+#10'	if ( colAcc.a < 1.0 )'
+#10'		colAcc.rgb = mix(clearColor,colAcc.rgb,colAcc.a);'
+#10'	FragColor = colAcc;'
+#10'}';
 {$ENDIF TWO_PASS}

procedure  InitGL;
begin
  gPrefs.LightAzimuth := 0;
  gPrefs.LightElevation := 70;
  gPrefs.RayCastQuality1to10 := 5;
  gPrefs.gradientTexture3D := 0;
  gPrefs.intensityTexture3D := 0;
  gPrefs.BackColor.r := 222;
  gPrefs.BackColor.g := 222;
  gPrefs.BackColor.b := 255;
  gPrefs.Distance := kDefaultDistance;
  gPrefs.isPerspective := false; //not compatible with single pass!
  gPrefs.Azimuth := 220;
  gPrefs.Elevation := 30;
  gPrefs.vao:= 0;
  if (not  Load_GL_version_3_3_CORE) then begin
   showmessage('Unable to load OpenGL 3.3 Core');
   halt;
  end;
  GLForm1.caption := glGetString(GL_VENDOR)+'; OpenGL= '+glGetString(GL_VERSION)+'; Shader='+glGetString(GL_SHADING_LANGUAGE_VERSION);
  //setup backface
  {$IFDEF TWO_PASS}
  FrameBufferGL(true);
  gPrefs.programBackface :=  initVertFrag(kVertBackface,  kFragBackface);
  gPrefs.mvpLocBackface := glGetUniformLocation(gPrefs.programBackface, pAnsiChar('ModelViewProjectionMatrix'));
  {$ENDIF}
  //setup main rendering
  gPrefs.programRaycast :=  initVertFrag(kVert, kFrag);
  gPrefs.imvLoc := glGetUniformLocation(gPrefs.programRaycast, pAnsiChar('ModelViewMatrixInverse'));
  gPrefs.mvpLoc := glGetUniformLocation(gPrefs.programRaycast, pAnsiChar('ModelViewProjectionMatrix'));
  {$IFDEF TWO_PASS}
  gPrefs.backFaceLoc := glGetUniformLocation(gPrefs.programRaycast, pAnsiChar('backFace'));
  gPrefs.viewHeightLoc := glGetUniformLocation(gPrefs.programRaycast, pAnsiChar('viewHeight'));
  gPrefs.viewWidthLoc := glGetUniformLocation(gPrefs.programRaycast, pAnsiChar('viewWidth'));
  {$ELSE}
  //GLForm1.PerspectiveMenu.visible := false; //not supported
  gPrefs.imvpLoc := glGetUniformLocation(gPrefs.programRaycast, pAnsiChar('ModelViewProjectionMatrixInverse'));
  {$ENDIF}
  gPrefs.sliceSizeLoc := glGetUniformLocation(gPrefs.programRaycast, pAnsiChar('sliceSize'));
  gPrefs.stepSizeLoc := glGetUniformLocation(gPrefs.programRaycast, pAnsiChar('stepSize'));
  gPrefs.loopsLoc := glGetUniformLocation(gPrefs.programRaycast, pAnsiChar('loops'));
  gPrefs.clearColorLoc := glGetUniformLocation(gPrefs.programRaycast, pAnsiChar('clearColor'));
  gPrefs.lightPositionLoc := glGetUniformLocation(gPrefs.programRaycast, pAnsiChar('lightPosition'));
  gPrefs.intensityVolLoc := glGetUniformLocation(gPrefs.programRaycast, pAnsiChar('intensityVol'));
  gPrefs.gradientVolLoc := glGetUniformLocation(gPrefs.programRaycast, pAnsiChar('gradientVol'));
  LoadBufferData(gPrefs.vao);
  resizeGL(GLForm1.GLBox.ClientWidth, GLForm1.GLBox.ClientHeight);
  //Load3DTextures('/Users/rorden/Documents/osx/MRIcroGL-MRIcro/mni152_2009_256.nii.gz', gPrefs.gradientTexture3D, gPrefs.intensityTexture3D, gPrefs.slices, gPrefs.ScaleDim, true);
  Load3DTextures('', gPrefs.gradientTexture3D, gPrefs.intensityTexture3D, gPrefs.slices, gPrefs.ScaleDim, true);
  if GLErrorStr <>  '' then begin
   GLForm1.GLBox.ReleaseContext;
   showmessage(GLErrorStr);
   GLErrorStr := '';
  end;
end;

procedure TGLForm1.GLMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
 if gMouse.Y < 0 then exit; //mouse is not down
 gPrefs.Azimuth := gPrefs.Azimuth + (X - gMouse.X);
 gPrefs.Elevation := gPrefs.Elevation + (Y - gMouse.Y);
 while gPrefs.Azimuth > 360 do gPrefs.Azimuth := gPrefs.Azimuth - 360;
 while gPrefs.Azimuth < 0 do gPrefs.Azimuth := gPrefs.Azimuth + 360;
 if gPrefs.Elevation > 90 then gPrefs.Elevation := 90;
 if gPrefs.Azimuth < -90 then gPrefs.Elevation := -90;
 gMouse.X := X;
 gMouse.Y := Y;
 GLBox.Invalidate;
end;

procedure TGLForm1.GLMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
     gMouse.Y := -1; //released
end;

procedure TGLForm1.GLMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
     gMouse.Y := Y;
     gMouse.X := X;
end;

procedure TGLForm1.OpenMenuClick(Sender: TObject);
begin
  if not OpenDialog1.execute then
     OpenDialog1.filename := '';
  GLBox.MakeCurrent();
  Load3DTextures(OpenDialog1.filename, gPrefs.gradientTexture3D, gPrefs.intensityTexture3D, gPrefs.slices, gPrefs.ScaleDim, true);
  GLBox.invalidate;
end;

procedure TGLForm1.PerspectiveMenuClick(Sender: TObject);
begin
  PerspectiveMenu.checked := not PerspectiveMenu.checked;
  gPrefs.isPerspective:= PerspectiveMenu.checked;
  resizeGL(GLBox.ClientWidth, GLBox.ClientHeight);
  GLbox.Invalidate;
end;

procedure TGLForm1.ShaderCheckClick(Sender: TObject);
begin
 About1Click(Sender);
end;

procedure TGLForm1.FormCreate(Sender: TObject);
begin
  gMouse.Y := -1;
  //OSX has two modes:
  //    NSOpenGLProfileLegacy provides support for OpenGL 2.1/GLSL1.2 and earlier
  //    NSOpenGLProfileVersion3_2Core provides support for AT LEAST OpenGL 3.2/GLSL3.2 CORE
  //    NSOpenGLProfileVersion4_1Core provides support for AT LEAST OpenGL 4.1/GLSL4.1 CORE
  //NOTE: CORE support removes deprecated LEGACY features
  //  In other words, Core OpenGL3.2 is NOT a superset of OpenGL2.1
  //  Functions like gl_FragColor, glTranslate etc. do not exist in CORE OpenGL
  //  Therefore, CORE is similar to embedded OpenGL, and old tutorials will not work
  {$IFDEF LCLCarbon}
  Error: Carbon only supports Legacy OpenGL. Solution: compile to the Cocoa widgetset (Project/ProjectOptions/Additions&Overrides)
  {$ENDIF}
  {$IFDEF Darwin}
  AppleMenu.Visible := true;
  HelpMenu.Visible := false;
  {$ENDIF}
  GLBox.MakeCurrent();
  InitGL;
  GLBox.invalidate;
end;

procedure TGLForm1.GLMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if Wheeldelta < 0 then
     gPrefs.Distance := gPrefs.Distance - 0.1
  else
      gPrefs.Distance := gPrefs.Distance + 0.1;
  if gPrefs.Distance > kMaxDistance then
     gPrefs.Distance := kMaxDistance;
  if gPrefs.Distance < 1 then
     gPrefs.Distance := 1.0;
  resizeGL(GLBox.ClientWidth, GLBox.ClientHeight);
  GLbox.Invalidate;
end;

procedure TGLForm1.FormResize(Sender: TObject);
begin
  resizeGL(GLBox.ClientWidth, GLBox.ClientHeight);
end;

procedure TGLForm1.BackColorMenuClick(Sender: TObject);
begin
 ColorDialog1.Color:= RGBToColor(gPrefs.BackColor.r, gPrefs.BackColor.g, gPrefs.BackColor.b);
 if not ColorDialog1.Execute then exit;
 gPrefs.BackColor.r:= Red(ColorDialog1.Color);
 gPrefs.BackColor.g:= Green(ColorDialog1.Color);
 gPrefs.BackColor.b:= Blue(ColorDialog1.Color);
 GLbox.Invalidate;
end;

end.

