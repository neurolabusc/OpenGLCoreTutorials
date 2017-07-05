unit texmain;

{$mode objfpc}{$H+}

interface

uses
  gltex, glcorearb, gl_core_utils, OpenGLContext, Classes, SysUtils, FileUtil, Forms,
  Controls, Graphics, Dialogs, ExtCtrls, gl_core_matrix, Types;
type
  { TGLForm1 }
  TGLForm1 = class(TForm)
    GLbox: TOpenGLControl;
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
  private

  public

  end;

var
  GLForm1: TGLForm1;

implementation
{$R *.lfm}

var
  gPositionX: single = 0;
  gPositionY: single = 0;
  gZoom : single = 1;
  gMouseY : integer = -1;
  gMouseX : integer = -1;
  gGLTexBG, gGLTex: TGLTex;

procedure  InitGL(var GLcontrol: TOpenGLControl);
begin
  GLcontrol.MakeCurrent();
  if not Load_GL_version_3_3_CORE() then begin
     GLcontrol.ReleaseContext;
     {$IFNDEF Windows} writeln('Unable to load OpenGL');{$ENDIF}
     showmessage('Unable to load OpenGL 3.3');
     halt();
  end;
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
  //GLBox.MakeCurrent(false);
  gGLTex.updateVbo(gPositionX,gPositionY,gZoom, GLBox);
  //GLBox.ReleaseContext;
  GLBox.Invalidate;
end;

procedure TGLForm1.FormShow(Sender: TObject);
var
  fnm, fishfnm, coralfnm: string;
begin
  fnm := extractfilepath(paramstr(0));
  {$IFDEF DARWIN}
  fnm := extractfilepath(ExcludeTrailingPathDelimiter(fnm))+'Resources/';
  {$ENDIF}
  fishfnm := fnm + 'fish.png'; //freeware icon from http://www.softicons.com/holidays-icons/scuba-diving-icons-by-diveandgo.com/fish-icon
  if not fileexists(fishfnm) then
    showmessage('Error: did not find image '+fishfnm);
  coralfnm := fnm + 'coral.png'; //freeware icon from http://www.softicons.com/holidays-icons/scuba-diving-icons-by-diveandgo.com/fish-icon
  if not fileexists(coralfnm) then
    showmessage('Error: did not find image '+coralfnm);
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
  gGLTexBG := TGLTex.Create(coralfnm, GLBox);
  gGLTex := TGLTex.Create(fishfnm, GLBox);
  GLBox.OnPaint:= @GLboxPaint;
end;

procedure TGLForm1.GLboxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  gMouseY := Y;
  gMouseX := X;
end;

procedure TGLForm1.GLboxMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if gMouseY < 0 then exit; //mouse is not down
  if (X <> gMouseX) or (Y <> gMouseY) then begin
        gPositionX := gPositionX + (X - gMouseX);
        gPositionY := gPositionY + (gMouseY - Y);
        gGLTex.updateVbo(gPositionX,gPositionY,gZoom, GLBox);
  end;
  gMouseY := Y;
  gMouseX := X;
  GLBox.Invalidate;
end;

procedure TGLForm1.GLboxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  gMouseY := -1; //released
end;

procedure TGLForm1.GLboxPaint(Sender: TObject);
begin
  nglMatrixMode(nGL_PROJECTION);
  nglLoadIdentity();
  nglOrtho (0, GLBox.ClientWidth, 0, GLBox.ClientHeight, 0.1, 40);
  glClearColor(0.3, 0.5, 0.8, 1.0); //Set blue background
  glClear(GL_COLOR_BUFFER_BIT);
    glEnable (GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  gGLTexBG.DrawTex;
  gGLTex.DrawTex;
  GLbox.SwapBuffers;
end;

end.

