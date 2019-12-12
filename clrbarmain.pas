unit clrbarmain;

{$mode objfpc}{$H+}
{$IFNDEF LCLCocoa}
{$ELSE}
  {$DEFINE RETINA} // <- requires Lazarus 1.9 svn 55355 or later
  // {$UNDEF RETINA} //disable retina support: works with older versions of lazarus
{$ENDIF}
interface

uses
  glcorearb, gl_core_utils, OpenGLContext, Classes, SysUtils, FileUtil, Forms,
  Controls, Graphics, Dialogs, ExtCtrls, Menus, gl_core_matrix, Types, glclrbar;
type
  { TGLForm1 }
  TGLForm1 = class(TForm)
    GLbox: TOpenGLControl;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    ClrbarOffMenu: TMenuItem;
    TranslucentWhiteMenu: TMenuItem;
    BlackMenu: TMenuItem;
    TranslucentBlackMenu: TMenuItem;
    WhiteMenu: TMenuItem;
    procedure ClrMenu(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GLboxMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure GLboxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLboxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure GLboxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLboxPaint(Sender: TObject);
    {$IFDEF RETINA}
    procedure SetRetina;
    {$ENDIF}
  private

  public

  end;

var
  GLForm1: TGLForm1;


implementation
{$R *.lfm}
{$IFDEF RETINA}
uses
  glcocoanscontext;
{$ENDIF}

var
  gRotation: single = 30;
  gZoom : single = 0.75;
  gMouseY : integer = -1;
  gMouseX : integer = -1;
  gClrbar: TGLClrbar;
  {$IFDEF RETINA}
  gIsRetina : boolean = true;

  procedure TGLForm1.SetRetina;
  begin
    //Requires Lazarus 1.9 svn 55355 or later
    //{$DEFINE FELIPE}
    {$IFDEF FELIPE}
    if gIsRetina then
       GLBox.Options := [ocoMacRetinaMode]
    else
      GLBox.Options := [];
    {$ELSE}
    LSetWantsBestResolutionOpenGLSurface(gIsRetina, GLBox.Handle);
    {$ENDIF}
  end;
{$ENDIF}//retina

{$DEFINE DRAWQUAD} //optional: show square on screen
{$IFDEF DRAWQUAD}
  type
  TShader = record
    vbo_point, vao_point, shaderProgram, vertexArrayObject: GLuint;
    uniform_angle, uniform_zoom: GLint;
  end;
  var
    gShader: TShader;

 const
 //Simple Vertex Shader
    kVert = '#version 330'
+#10'layout(location = 0) in vec2 point;'
+#10'uniform float angle;'
+#10'uniform float zoom;'
+#10'out vec2 uv;'
+#10'void main() {'
+#10'    uv = (point+1.0)*0.5;'
+#10'    mat2 rotate = mat2(cos(angle), -sin(angle),sin(angle), cos(angle));'
+#10'    gl_Position = vec4(zoom * rotate * point, -1.0, 1.0);'
+#10'}';

//Simple Fragment Shader
kFrag = '#version 330'
+#10'in vec2 uv;'
+#10'out vec4 color;'
+#10'void main() {'
+#10'    color = vec4(uv.r, uv.g, 0.15, 1);'
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
  gShader.uniform_zoom := glGetUniformLocation(gShader.shaderProgram, pAnsiChar('zoom'));
end;
{$ENDIF} // DRAWQUAD

procedure  InitGL(var GLcontrol: TOpenGLControl);
begin
  GLcontrol.MakeCurrent();
  if not Load_GL_version_3_3_CORE() then begin
     GLcontrol.ReleaseContext;
     {$IFNDEF Windows} writeln('Unable to load OpenGL');{$ENDIF}
     showmessage('Unable to load OpenGL 3.3');
     halt();
  end;
  {$IFDEF DRAWQUAD}
  gShader.shaderProgram :=  initVertFrag(kVert,  kFrag);
  LoadBufferData;
  {$ENDIF}
  GLcontrol.ReleaseContext;
  if GLErrorStr <> '' then begin
     showmessage(GLErrorStr);
     GLErrorStr := '';
  end;
end;

procedure TGLForm1.GLboxMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if WheelDelta = 0 then exit;
  if WheelDelta > 0 then
    gZoom := gZoom + 0.1
  else
     gZoom := gZoom - 0.1;
  if gZoom < 0.1 then gZoom := 0.1;
  if gZoom > 10 then gZoom := 10;
  GLBox.Invalidate;
end;

procedure TGLForm1.FormShow(Sender: TObject);
var
   i: integer;
   LUT: TLUT;
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
  {$IFDEF RETINA}
  SetRetina;
  {$ENDIF}
  //set up color bar
  gClrbar:= TGLClrbar.Create(GLBox);
  gClrbar.isTopOrRight:=true;
  for i := 0 to 255 do begin
      LUT[i].R := i;
      LUT[i].G := 0;
      LUT[i].B := 0;
  end;
  gClrbar.SetLUT(1, LUT, -1,1);
  for i := 0 to 255 do begin
      LUT[i].R := 0;
      LUT[i].G := i;
      LUT[i].B := 0;
  end;
  gClrbar.SetLUT(2, LUT, 1,7);
  if GLErrorStr <> '' then begin
     showmessage(GLErrorStr);
     GLErrorStr := '';
  end;
  GLBox.OnPaint:= @GLboxPaint;
end;

function setRGBA(r,g,b,a: byte): TRGBA;
begin
     result.r := r;
     result.g := g;
     result.b := b;
     result.a := a;
end;

procedure TGLForm1.ClrMenu(Sender: TObject);
begin
  Case (sender as TMenuItem).Tag of
       1: begin
         gClrbar.BackColor := (setRGBA(255,255,255,255));
         gClrbar.FontColor := (setRGBA(0,0,0,255));
       end;
       2: begin
         gClrbar.BackColor := (setRGBA(255,255,255,128));
         gClrbar.FontColor := (setRGBA(0,0,0,255));
       end;
       3: begin
         gClrbar.BackColor := (setRGBA(0,0,0,255));
         gClrbar.FontColor := (setRGBA(255,255,255,255));
       end;
       4: begin
         gClrbar.BackColor := (setRGBA(0,0,0,128));
         gClrbar.FontColor := (setRGBA(255,255,255,255));
       end;
  end;
  GLBox.Invalidate;
end;

procedure TGLForm1.GLboxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  gMouseY := Y;
  gMouseX := X;
  {$IFDEF RETINA}
  if (ssShift in Shift) then begin
     gIsRetina := not gIsRetina;
     SetRetina;
     exit;
  end;
 {$ENDIF}
   if (gClrbar.isVertical) then
     gClrbar.isTopOrRight := not gClrbar.isTopOrRight;
  gClrbar.isVertical := not gClrbar.isVertical;
  GLBox.Invalidate;
end;

procedure TGLForm1.GLboxMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if gMouseY < 0 then exit; //mouse is not down
  if (X <> gMouseX) or (Y <> gMouseY) then begin
     gRotation := gRotation + (Y - gMouseY);
     GLBox.Invalidate;
  end;
  gMouseY := Y;
  gMouseX := X;
end;

procedure TGLForm1.GLboxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  gMouseY := -1; //released
end;

procedure TGLForm1.GLboxPaint(Sender: TObject);
var
  w,h: integer;
begin
  nglMatrixMode(nGL_MODELVIEW);
  nglLoadIdentity;
  nglMatrixMode(nGL_PROJECTION);
  nglLoadIdentity();
  {$IFDEF RETINA} //requires Lazarus 1.9 svn 55355 or later
  w := round(GLBox.Width * LBackingScaleFactor(GLBox.Handle));
  h := round(GLBox.Height * LBackingScaleFactor(GLBox.Handle));
  {$ELSE}
  w := GLBox.ClientWidth;
  h := GLBox.ClientHeight;
  {$ENDIF}
  nglOrtho (0, w, 0, h, 0.1, 40);
  glClearColor(0.3, 0.5, 0.8, 1.0); //Set blue background
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT); //GTK3 requires latter
  glEnable (GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  //draw quad
  {$IFDEF DRAWQUAD}
  glUseProgram(gShader.shaderProgram);
  glUniform1f(gShader.uniform_angle, gRotation/90) ;
  glUniform1f(gShader.uniform_zoom, gZoom) ;
  glBindVertexArray(gShader.vao_point);
  glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);
  glBindVertexArray(0);
  glUseProgram(0);
  {$ENDIF}
  //draw colorbar
  if not ClrbarOffMenu.checked then
     gClrbar.Draw(2, w, h);
  //show result
  GLbox.SwapBuffers;
end;

end.

