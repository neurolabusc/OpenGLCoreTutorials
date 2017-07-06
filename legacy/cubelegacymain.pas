unit cubelegacymain;

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
  gZoom : single = 32;
  gElevation : integer = 30;
  gAzimuth : integer = 30;
  gMouseX : integer = -1;
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
  gMouseX := X;
end;

procedure TGLForm1.GLBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if gMouseY < 0 then exit; //mouse is not down
  if ((Y <> gMouseY) or (X <> gMouseX)) then begin
        gElevation := gElevation + (Y - gMouseY);
        gAzimuth := gAzimuth - (X - gMouseX);
        GLBox.Invalidate;
  end;
  gMouseY := Y;
  gMouseX := X;
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
    gZoom := gZoom + 1
  else
     gZoom := gZoom - 1;
  if gZoom < 1 then gZoom := 1;
  if gZoom > 128 then gZoom := 128;
  GLBox.Invalidate;
end;

procedure MakeCube(sz: single);
//draw a cube of size sz
var
  sz2: single;
begin
  sz2 := sz;
  glColor4f(0.1,0.1,0.1,1);
  glBegin(GL_TRIANGlE_STRIP); //* Bottom side
	glVertex3f(-sz, -sz, -sz2);
	glVertex3f(-sz, sz, -sz2);
	glVertex3f(sz, -sz, -sz2);
        glVertex3f(sz, sz, -sz2);
  glEnd;
  glColor4f(0.8,0.8,0.8,1);
  glBegin(GL_TRIANGlE_STRIP); //* Top side
	glVertex3f(-sz, -sz, sz2);
	glVertex3f(sz, -sz, sz2);
        glVertex3f(-sz, sz, sz2);
        glVertex3f(sz, sz, sz2);
  glEnd;
  glColor4f(0,0,0.5,1);
  glBegin(GL_TRIANGlE_STRIP); //* Front side
    glVertex3f(-sz, sz2, -sz);
    glVertex3f(-sz, sz2, sz);
    glVertex3f(sz, sz2, -sz);
    glVertex3f(sz, sz2, sz);
  glEnd;
  glColor4f(0.3,0,0.3,1);
  glBegin(GL_TRIANGlE_STRIP);//* Back side
	glVertex3f(-sz, -sz2, -sz);
	glVertex3f(sz, -sz2, -sz);
	glVertex3f(-sz, -sz2, sz);
	glVertex3f(sz, -sz2, sz);
  glEnd;
  glColor4f(0.6,0,0,1);
  glBegin(GL_TRIANGlE_STRIP); //* Left side
	glVertex3f(-sz2, -sz, -sz);
	glVertex3f(-sz2, -sz, sz);
	glVertex3f(-sz2, sz, -sz);
	glVertex3f(-sz2, sz, sz);
  glEnd;
  glColor4f(0,0.6,0,1);
  glBegin(GL_TRIANGlE_STRIP); //* Right side
	//glNormal3f(1.0, -sz, -sz);
	glVertex3f(sz2, -sz, -sz);
	glVertex3f(sz2, sz, -sz);
	glVertex3f(sz2, -sz, sz);
	glVertex3f(sz2, sz, sz);
  glEnd();
end; //MakeCube()

procedure TGLForm1.GLboxPaint(Sender: TObject);
begin
  glClearColor(0.3, 0.5, 0.8, 1.0); //Set blue background
  glClear(GL_COLOR_BUFFER_BIT);
  glEnable (GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  glOrtho(0, GLBox.ClientWidth, 0, GLBox.ClientHeight,-gZoom*4,0.01);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
  //glEnable(GL_DEPTH_TEST);
  glEnable(GL_CULL_FACE); //check on pyramid

  glTranslatef(0,0,gZoom*2);
  glTranslatef(GLBox.ClientWidth/2,GLBox.ClientHeight/2,0);
  glRotatef(90-gElevation,-1,0,0);
  glRotatef(-gAzimuth,0,0,1);
  MakeCube(gZoom);

  //draw triangle - pixel coordinates
  (*glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  glOrtho (0, GLBox.ClientWidth, 0, GLBox.ClientHeight, 0.1, 40);
  glBegin(GL_TRIANGLES);
    glColor3f(1, 0, 0);
    glVertex3f( GLBox.ClientWidth/2, GLBox.ClientHeight, -1);
    glColor3f(0, 1, 0);
    glVertex3f(0,0, -1);
    glColor3f(0, 0, 1);
    glVertex3f( GLBox.ClientWidth,0, -1);
  glEnd;  *)
  //draw quad - cartesian coordinates with screen center at 0,0
  (*glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  aspect := GLBox.ClientWidth / GLBox.ClientHeight;
  if (aspect > 1) then
     glOrtho(-2.0 * aspect, 2.0 * aspect, -2.0, 2.0, -0.1, -100)
   else
     glOrtho(-2.0, 2.0, -2.0 / aspect, 2.0 / aspect, -0.1, -100);

  //glEnable(GL_DEPTH_TEST);
  glTranslatef(0,0,gZoom*2);
  glRotatef(90-gElevation,-1,0,0);
  glRotatef(-gAzimuth,0,0,1);

  MakeCube(gZoom); *)
  //show result
  GLbox.SwapBuffers;
end;

end.

