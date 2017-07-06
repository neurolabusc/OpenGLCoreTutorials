unit basiclegacymain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, OpenGLContext, Forms, Controls, Graphics,
  Dialogs, gl, glext,  Types;

type

  { TGLForm1 }

  TGLForm1 = class(TForm)
    GLBox: TOpenGLControl;
    procedure FormShow(Sender: TObject);
    procedure GLBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
    procedure GLBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLBoxMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure GLboxPaint(Sender: TObject);
  private

  public

  end;

var
  GLForm1: TGLForm1;

implementation
{$R *.lfm}
var
  gZoom : single = 1;
  gRotation : integer = 30;
  gMouseY : integer = -1;


procedure  InitGL(var GLcontrol: TOpenGLControl);
begin
  GLcontrol.MakeCurrent();
  if not Load_GL_version_2_1() then begin
     GLcontrol.ReleaseContext;
     {$IFNDEF Windows} writeln('Unable to load OpenGL');{$ENDIF}
     showmessage('Unable to load OpenGL 2.1');
     halt();
  end;
  GLcontrol.ReleaseContext;
end;

procedure TGLForm1.FormShow(Sender: TObject);
begin
     InitGL (GLBox);
     GLBox.OnPaint:= @GLboxPaint;
end;

procedure TGLForm1.GLBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  gMouseY := Y;
end;

procedure TGLForm1.GLBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if gMouseY < 0 then exit; //mouse is not down
  if (Y <> gMouseY) then begin
        gRotation := gRotation + (Y - gMouseY);
        GLBox.Invalidate;
  end;
  gMouseY := Y;
end;

procedure TGLForm1.GLBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  gMouseY := -1; //released
end;

procedure TGLForm1.GLBoxMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if WheelDelta = 0 then exit;
  if WheelDelta > 0 then
    gZoom := gZoom + 0.1
  else
     gZoom := gZoom - 0.1;
  if gZoom < 0.1 then gZoom := 0.1;
  if gZoom > 2 then gZoom := 2;
  GLBox.Invalidate;
end;

procedure TGLForm1.GLboxPaint(Sender: TObject);
var
  aspect: single;
begin
  glClearColor(0.3, 0.5, 0.8, 1.0); //Set blue background
  glClear(GL_COLOR_BUFFER_BIT);
  glEnable (GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  //draw triangle - pixel coordinates
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  glOrtho (0, GLBox.ClientWidth, 0, GLBox.ClientHeight, 0.1, 40);
  glBegin(GL_TRIANGLES);
    glColor3f(1, 0, 0);
    glVertex3f( GLBox.ClientWidth/2, GLBox.ClientHeight, -1);
    glColor3f(0, 1, 0);
    glVertex3f(0,0, -1);
    glColor3f(0, 0, 1);
    glVertex3f( GLBox.ClientWidth,0, -1);
  glEnd;
  //draw quad - cartesian coordinates with screen center at 0,0
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  aspect := GLBox.ClientWidth / GLBox.ClientHeight;
  if (aspect > 1) then
     glOrtho(-2.0 * aspect, 2.0 * aspect, -2.0, 2.0, 0.1, 100)
   else
     glOrtho(-2.0, 2.0, -2.0 / aspect, 2.0 / aspect, 0.1, 100);
  glRotatef(gRotation,0,0,1);
  glScalef(gZoom,gZoom,1);
  glColor3f(1.0, 0.0, 0.0);
  glBegin(GL_QUADS);
    glVertex3f(-1, -1, -1);
    glVertex3f(-1, 1, -1);
    glVertex3f(1, 1, -1);
    glVertex3f(1, -1, -1);
  glEnd;
  //show result
  GLbox.SwapBuffers;
end;

end.

