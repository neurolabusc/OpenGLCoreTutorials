unit textmlegacymain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, OpenGLContext, Forms, Controls, Graphics,
  Dialogs, gl, glext, gltext_legacy, glmtext_legacy, Types;

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
    procedure UpdateText;
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
  gGLText: glmtext_legacy.TGLText;
  gGLText2: gltext_legacy.TGLText;
  gStr : string = 'The quick brown fox jumped over the lazy dog';


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
var
  success: boolean;
  basenm, fnm: string;
begin
     InitGL (GLBox);
     basenm := extractfilepath(paramstr(0));
     {$IFDEF DARWIN}
     basenm := extractfilepath(ExcludeTrailingPathDelimiter(basenm))+'Resources/';
     {$ENDIF}
     fnm := basenm + 'roboto.png';
     gGLText :=glmtext_legacy. TGLText.Create(fnm, success, GLBox);
     if not success then
       showmessage('Error: unable to load .png and .fnt '+fnm);
     fnm := basenm + 'hiero.png';//'serif.png';
     gGLText2 := gltext_legacy.TGLText.Create(fnm, true, success, GLBox);
     if not success then
       showmessage('Error: unable to load .png and .fnt '+fnm);
     if GLErrorStr <> '' then begin
        showmessage(GLErrorStr);
        GLErrorStr := '';
     end;
     GLBox.OnPaint:= @GLboxPaint;
     UpdateText;
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
  if (X <> gMouseX) or (Y <> gMouseY) then begin
        gPositionX := gPositionX + (X - gMouseX);
        gPositionY := gPositionY + (gMouseY - Y);
  end;
  gMouseY := Y;
  gMouseX := X;
  UpdateText;
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
  if gZoom > 10 then gZoom := 10;
  UpdateText;
end;

procedure TGLForm1.UpdateText;
begin
  gGLText.ClearText;
  gGLText.TextOut(gPositionX,gPositionY,gZoom, gStr);
  gGLText2.ClearText;
  gGLText2.TextOut(gPositionX,gPositionY+(64*gZoom),gZoom, gStr);
  GLBox.Invalidate;
end;

procedure TGLForm1.GLboxPaint(Sender: TObject);
begin
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  glOrtho (0, GLBox.ClientWidth, 0, GLBox.ClientHeight, 0.1, 40);
  glClearColor(0.3, 0.5, 0.8, 1.0); //Set blue background
  glClear(GL_COLOR_BUFFER_BIT);
  glEnable (GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  //draw triangle
  glBegin(GL_TRIANGLES);
    glColor3f(1, 0, 0);
    glVertex3f( GLBox.ClientWidth/2, GLBox.ClientHeight, -1);
    glColor3f(0, 1, 0);
    glVertex3f(0,0, -1);
    glColor3f(0, 0, 1);
    glVertex3f( GLBox.ClientWidth,0, -1);
  glEnd;
  //draw text
  gGLText.DrawText;
  gGLText2.DrawText;
  //show result
  GLbox.SwapBuffers;
end;

end.

