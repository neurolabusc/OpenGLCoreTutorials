unit cubelegacymain;

{$mode objfpc}{$H+}
{$DEFINE cubeObject} //optional: demonstrate glcube class
interface

uses
    {$IFDEF cubeObject}glcube_legacy,{$ENDIF}
    Classes, SysUtils, FileUtil, OpenGLContext, Forms, Controls, Graphics,
  Dialogs, gl, glext,  Types, math;

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
  {$IFDEF cubeObject}gCube : TGLCube;{$ENDIF}


procedure  InitGL(var GLcontrol: TOpenGLControl);
begin
  GLcontrol.MakeCurrent();
  if not Load_GL_version_2_1() then begin
     GLcontrol.ReleaseContext;
     {$IFNDEF Windows} writeln('Unable to load OpenGL');{$ENDIF}
     showmessage('Unable to load OpenGL 2.1');
     halt();
  end;
  {$IFDEF cubeObject}gCube := TGLCube.Create(GLcontrol);{$ENDIF}
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
begin
  glBegin(GL_TRIANGlE_STRIP); //* Bottom side
        glColor4f(0,0,0,1);
	glVertex3f(-sz, -sz, -sz);
        glColor4f(0,1,0,1);
	glVertex3f(-sz, sz, -sz);
        glColor4f(1,0,0,1);
	glVertex3f(sz, -sz, -sz);
        glColor4f(1,1,0,1);
        glVertex3f(sz, sz, -sz);
  glEnd;
  glBegin(GL_TRIANGlE_STRIP); //* Top side
        glColor4f(0,0,1,1);
	glVertex3f(-sz, -sz, sz);
        glColor4f(1,0,1,1);
	glVertex3f(sz, -sz, sz);
        glColor4f(0,1,1,1);
        glVertex3f(-sz, sz, sz);
        glColor4f(1,1,1,1);
        glVertex3f(sz, sz, sz);
  glEnd;
  glBegin(GL_TRIANGlE_STRIP); //* Front side
    glColor4f(0,1,0,1);
    glVertex3f(-sz, sz, -sz);
    glColor4f(0,1,1,1);
    glVertex3f(-sz, sz, sz);
    glColor4f(1,1,0,1);
    glVertex3f(sz, sz, -sz);
    glColor4f(1,1,1,1);
    glVertex3f(sz, sz, sz);
  glEnd;
  glBegin(GL_TRIANGlE_STRIP);//* Back side
        glColor4f(0,0,0,1);
	glVertex3f(-sz, -sz, -sz);
        glColor4f(1,0,0,1);
	glVertex3f(sz, -sz, -sz);
        glColor4f(0,0,1,1);
	glVertex3f(-sz, -sz, sz);
        glColor4f(1,0,1,1);
	glVertex3f(sz, -sz, sz);
  glEnd;
  glBegin(GL_TRIANGlE_STRIP); //* Left side
	glColor4f(0,0,0,1);
        glVertex3f(-sz, -sz, -sz);
	glColor4f(0,0,1,1);
        glVertex3f(-sz, -sz, sz);
	glColor4f(0,1,0,1);
        glVertex3f(-sz, sz, -sz);
	glColor4f(0,1,1,1);
        glVertex3f(-sz, sz, sz);
  glEnd;
  glBegin(GL_TRIANGlE_STRIP); //* Right side
	glColor4f(1,0,0,1);
	glVertex3f(sz, -sz, -sz);
        glColor4f(1,1,0,1);
	glVertex3f(sz, sz, -sz);
        glColor4f(1,0,1,1);
	glVertex3f(sz, -sz, sz);
        glColor4f(1,1,1,1);
	glVertex3f(sz, sz, sz);
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
  glEnable(GL_CULL_FACE); //check on pyramid
  glTranslatef(0,0,gZoom*2);
  glTranslatef(GLBox.ClientWidth/2,GLBox.ClientHeight/2,0);
  glRotatef(90-gElevation,-1,0,0);
  glRotatef(-gAzimuth,0,0,1);
  MakeCube(gZoom);
  //show cube object - optional
  {$IFDEF cubeObject}
  gCube.Size := gZoom/(max(GLBox.ClientWidth,GLBox.ClientHeight));
  gCube.Azimuth:=gAzimuth;
  gCube.Elevation:=gElevation;
  gCube.Draw(GLBox.ClientWidth,GLBox.ClientHeight);
  {$ENDIF}
  //show result
  GLbox.SwapBuffers;
end;

end.

