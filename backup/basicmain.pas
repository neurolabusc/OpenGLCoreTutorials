unit basicmain;

{$mode objfpc}{$H+}

interface

uses
  gl_core_matrix, glcorearb, gl_core_utils, OpenGLContext, Classes, SysUtils, FileUtil, Forms,
  Controls, Graphics, Dialogs, ExtCtrls;

type
  TGLForm1 = class(TForm)
    GLbox: TOpenGLControl;
    procedure FormCreate(Sender: TObject);
    procedure GLboxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLboxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure GLboxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLboxPaint(Sender: TObject);
  private

  public

  end;

var
  GLForm1: TGLForm1;

implementation

{$R *.lfm}

{ TGLForm1 }
type
TShader = record
  vbo_point, vao_point, shaderProgram, vertexArrayObject: GLuint;
  uniform_angle: GLint;
end;

var
  gShader: TShader;
  gRotation : integer = 30;
  gMouseY : integer = -1;

const
 //Simple Vertex Shader
    kVert = '#version 330'
+#10'layout(location = 0) in vec2 point;'
+#10'uniform float angle;'
+#10'void main() {'
+#10'    mat2 rotate = mat2(cos(angle), -sin(angle),sin(angle), cos(angle));'
+#10'    gl_Position = vec4(0.75 * rotate * point, 0.0, 1.0);'
+#10'}';

//Simple Fragment Shader
    kFrag = '#version 330'
+#10'out vec4 color;'
+#10'void main() {'
+#10'    color = vec4(1, 0.15, 0.15, 0.5);'
+#10'}';

procedure LoadBufferData;
const
    kATTRIB_POINT = 0;
    SQUARE : array [0..7] of single = (
    -1.0,  1.0,
    -1.0, -1.0,
     1.0,  1.0,
     1.0, -1.0);
begin
  glGenBuffers(1, @gShader.vbo_point);
  glBindBuffer(GL_ARRAY_BUFFER, gShader.vbo_point);
  glBufferData(GL_ARRAY_BUFFER, sizeof(SQUARE), @SQUARE[0], GL_STATIC_DRAW);
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  // Prepare vertrex array object (VAO)
  glGenVertexArrays(1, @gShader.vao_point);
  glBindVertexArray(gShader.vao_point);
  glBindBuffer(GL_ARRAY_BUFFER, gShader.vbo_point);
  glVertexAttribPointer(kATTRIB_POINT, 2, GL_FLOAT, GL_FALSE, 0, nil);
  glEnableVertexAttribArray(kATTRIB_POINT);
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glBindVertexArray(0);
  gShader.uniform_angle := glGetUniformLocation(gShader.shaderProgram, pAnsiChar('angle'));
end;

procedure  InitGL(var GLcontrol: TOpenGLControl);
begin
  GLcontrol.MakeCurrent();
  if not Load_GL_version_3_3_CORE() then begin
     GLcontrol.ReleaseContext;
     {$IFNDEF Windows} writeln('Unable to load OpenGL');{$ENDIF}
     showmessage('Unable to load OpenGL 3.3');
     halt();
  end;
  GLForm1.caption := glGetString(GL_VENDOR)+'; OpenGL= '+glGetString(GL_VERSION)+'; Shader='+glGetString(GL_SHADING_LANGUAGE_VERSION);
  gShader.shaderProgram :=  initVertFrag(kVert,  kFrag);
  LoadBufferData;
  glUseProgram(0);
  GLcontrol.ReleaseContext;
  if GLErrorStr <> '' then begin
     showmessage(GLErrorStr);
     GLErrorStr := '';
  end;
end;

procedure TGLForm1.FormCreate(Sender: TObject);
begin
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
  InitGL (GLBox);
end;

procedure TGLForm1.GLboxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  gMouseY := Y;
end;

procedure TGLForm1.GLboxMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if gMouseY < 0 then exit; //mouse is not down
  gRotation := gRotation + (Y - gMouseY);
  gMouseY := Y;
  GLBox.Invalidate;
end;

procedure TGLForm1.GLboxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  gMouseY := -1; //released
end;

procedure TGLForm1.GLboxPaint(Sender: TObject);
begin
  glClearColor(0.1, 0.1, 0.2, 1.0); //Set blue background
  //glEnable(GL_DEPTH_TEST);
  glDisable(GL_DEPTH_TEST);
  glEnable (GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT); //GTK3 requires latter
  glUseProgram(gShader.shaderProgram);
  glBindVertexArray(gShader.vao_point);
  glUniform1f(gShader.uniform_angle, gRotation/90) ;
  glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);
  //next two lines optional: draw 2nd counter-rotating quad
  glUniform1f(gShader.uniform_angle, 35-(gRotation/90)) ;
  glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);
  glBindVertexArray(0);
  GLbox.SwapBuffers;
end;

end.

