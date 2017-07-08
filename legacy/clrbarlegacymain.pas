unit clrbarlegacymain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, OpenGLContext, Forms, Controls, Graphics,
  Dialogs, Menus, gl, glext,  Types, glclrbar_legacy, gltext_legacy;

type

  { TGLForm1 }

  TGLForm1 = class(TForm)
    GLBox: TOpenGLControl;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    ClrbarOffMenu: TMenuItem;
    WhiteMenu: TMenuItem;
    TranslucentWhiteMenu: TMenuItem;
    BlackMenu: TMenuItem;
    TranslucentBlackMenu: TMenuItem;
    procedure ClrMenu(Sender: TObject);
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
  gClrbar: TGLClrbar;

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
  i: integer;
  LUT: TLUT;
begin
     InitGL (GLBox);
     gClrbar:= TGLClrbar.Create(GLBox);
     for i := 0 to 255 do begin
         LUT[i].R := i;
         LUT[i].G := 0;
         LUT[i].B := 0;
     end;
     gClrbar.SetLUT(1, LUT, 10,20);
     for i := 0 to 255 do begin
         LUT[i].R := 0;
         LUT[i].G := i;
     end;
     gClrbar.SetLUT(2, LUT, -5,5);
     if GLErrorStr <> '' then begin
        showmessage(GLErrorStr);
        GLErrorStr := '';
     end;
     gClrbar.SizeFraction:=0.04;
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

procedure TGLForm1.GLBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  gMouseY := Y;
  if (gClrbar.isVertical) then
     gClrbar.isTopOrRight := not gClrbar.isTopOrRight;
  gClrbar.isVertical := not gClrbar.isVertical;
  GLBox.Invalidate;
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
begin
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  glOrtho (0, GLBox.ClientWidth, 0, GLBox.ClientHeight, 0.1, 40);
  glClearColor(0.3, 0.5, 0.8, 1.0); //Set blue background
  glClear(GL_COLOR_BUFFER_BIT);
  glEnable (GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  //draw triangle -optional
  glBegin(GL_TRIANGLES);
    glColor3f(1, 0, 0);
    glVertex3f( GLBox.ClientWidth/2, GLBox.ClientHeight, -1);
    glColor3f(0, 1, 0);
    glVertex3f(0,0, -1);
    glColor3f(1, 1, 0);
    glVertex3f( GLBox.ClientWidth,0, -1);
  glEnd;
  //draw text
  if not ClrbarOffMenu.checked then
     gClrbar.Draw(2, GLBox.ClientWidth,GLBox.ClientHeight);
  //show result
  GLbox.SwapBuffers;
end;

end.

