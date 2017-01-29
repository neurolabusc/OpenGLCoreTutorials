unit cubemain;

{$mode objfpc}{$H+}

interface

uses
  gl_core_matrix, gl_core_utils, Classes, SysUtils, FileUtil, OpenGLContext, Forms, Controls, Graphics,
  Dialogs, StdCtrls, ExtCtrls, glcorearb, Types;

type { TGLForm1 }
  TGLForm1 = class(TForm)
    GLBox: TOpenGLControl;
    procedure FormCreate(Sender: TObject);
    procedure MouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure GLboxPaint(Sender: TObject);
    procedure MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private

  public

  end;

var
  GLForm1: TGLForm1;

implementation

{$R *.lfm}

type
TPrefs = record
  vao, shaderProgram: GLuint;
  uniform_angle: GLint;
  distance: single;
  azimuth, elevation: integer;
end;

const
  kDefaultDistance = 2.25;
  kMaxDistance = 40;

var
  gPrefs: TPrefs;
  gMouse : TPoint;

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
  //glDeleteBuffers(1, @vbo);
  //glDeleteBuffers(1, @vbo_point);
end;

procedure TGLForm1.GLboxPaint(Sender: TObject);
var
  mvp : TnMat44;
  mvpLoc: GLint;
  scale, whratio: single;
  lTexScale : array [1..3] of single = (1,1,1);
begin
  glClearColor(0.1, 0.1, 0.4, 1.0); //Set blue background
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glUseProgram(gPrefs.shaderProgram);
  nglMatrixMode(nGL_PROJECTION);
  nglLoadIdentity();
  if gPrefs.Distance = 0 then
     scale := 1
  else
      scale := 1/abs(kDefaultDistance/(gPrefs.Distance+1.0));
  whratio := GLBox.ClientWidth/GLBox.ClientHeight;
  nglOrtho(whratio*-0.5*scale,whratio*0.5*scale,-0.5*scale,0.5*scale, 0.01, kMaxDistance);
  nglTranslatef(0,0,-gPrefs.Distance);
  nglRotatef(90-gPrefs.Elevation,-1,0,0);
  nglRotatef(gPrefs.Azimuth,0,0,1);
  nglTranslatef(-lTexScale[1]/2,-lTexScale[2]/2,-lTexScale[3]/2);
  nglScalef(lTexScale[1],lTexScale[2],lTexScale[3]);
  mvp := ngl_ModelViewProjectionMatrix;
  mvpLoc := glGetUniformLocation(gPrefs.shaderProgram, pAnsiChar('ModelViewProjectionMatrix'));
  glUniformMatrix4fv(mvpLoc, 1, GL_FALSE, @mvp[0,0]);
  glEnable(GL_DEPTH_TEST);
  glEnable(GL_CULL_FACE);
  glCullFace(GL_FRONT); //show front face
  //glCullFace(GL_BACK); //show back face
  glBindVertexArray(gPrefs.vao);
  glDrawElements(GL_TRIANGLES, 12*3, GL_UNSIGNED_INT, nil);
  glBindVertexArray(0);
  glDisable(GL_CULL_FACE);
  glUseProgram(0);
  GLbox.SwapBuffers;
end;

const kVert = '#version 330'
+#10'layout(location = 0) in vec3 vPos;'
+#10'//layout(location = 1) in vec3 vClr;'
+#10'out vec3 fClr;'
+#10'uniform mat4 ModelViewProjectionMatrix, ModelViewMatrix, ProjectionMatrix;'
+#10+'mat4 mvp;'
+#10'void main() {'
+#10'    fClr = vPos;'
+#10'    gl_Position = ModelViewProjectionMatrix * vec4(vPos, 1.0);'
+#10'    //mvp = ModelViewProjectionMatrix;'
+#10'    //gl_Position =  mvp * vec4(vPos, 1.0) ;'
+#10'    //gl_Position = vec4(vPos, 1.0);'
+#10'}';

const kFrag = '#version 330'
+#10'in vec3 fClr;'
+#10'out vec4 FragColor;'
+#10'void main() {'
+#10'    FragColor = vec4(fClr, 1.0);'
+#10'    //FragColor = vec4(1.0, 0.0, 0.0, 1.0);'
+#10'}';

procedure  InitGL;
begin
  gPrefs.vao:= 0;
  if (not  Load_GL_version_3_3_CORE) then begin
     showmessage('Unable to load OpenGL 3.3 Core');
     halt;
  end;
  GLForm1.caption := glGetString(GL_VENDOR)+'; OpenGL= '+glGetString(GL_VERSION)+'; Shader='+glGetString(GL_SHADING_LANGUAGE_VERSION);
  gPrefs.shaderProgram :=  initVertFrag(kVert,  kFrag);
  LoadBufferData(gPrefs.vao);//, gShader.vbo);
end;

procedure TGLForm1.MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
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

procedure TGLForm1.MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
     gMouse.Y := -1; //released
end;

procedure TGLForm1.MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
     gMouse.Y := Y;
     gMouse.X := X;
end;

procedure TGLForm1.FormCreate(Sender: TObject);
begin
 gMouse.Y := -1;
 gPrefs.Distance := kDefaultDistance;
 gPrefs.Azimuth := 220;
 gPrefs.Elevation := 30;
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
  GLBox.MakeCurrent();
  InitGL;
  GLBox.invalidate;
end;

procedure TGLForm1.MouseWheel(Sender: TObject; Shift: TShiftState;
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
  GLbox.Invalidate;
end;

end.

