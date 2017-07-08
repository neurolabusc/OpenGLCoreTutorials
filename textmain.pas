unit textmain;

{$mode objfpc}{$H+}
{$IFNDEF LCLCocoa}
{$ELSE}
  {$DEFINE RETINA} // <- requires Lazarus 1.9 svn 55355 or later
  // {$UNDEF RETINA} //disable retina support: works with older versions of lazarus
  {$UNDEF GLEXT} //GLEXT incompatible with MacOS OpenGL versions >2.1
{$ENDIF}
interface

uses
  gltext, glcorearb, gl_core_utils, OpenGLContext, Classes, SysUtils, FileUtil, Forms,
  Controls, Graphics, Dialogs, ExtCtrls, gl_core_matrix, Types;
type
  { TGLForm1 }
  TGLForm1 = class(TForm)
    GLbox: TOpenGLControl;
    {$IFDEF RETINA}
    procedure SetRetina;
    {$ENDIF}
    procedure FormShow(Sender: TObject);
    procedure UpdateText;
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
{$IFDEF RETINA}
uses
  glcocoanscontext;
{$ENDIF}

var
  gPositionX: single = 0;
  gPositionY: single = 0;
  gZoom : single = 1;
  gMouseY : integer = -1;
  gMouseX : integer = -1;
  gGLText, gGLText2: TGLText;
  gStr : string = 'The quick brown fox jumped over the lazy dog';
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
  UpdateText;
end;

procedure TGLForm1.FormShow(Sender: TObject);
var
  success: boolean;
  basenm, fnm: string;
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
  basenm := extractfilepath(paramstr(0));
  {$IFDEF DARWIN}
  basenm := extractfilepath(ExcludeTrailingPathDelimiter(basenm))+'Resources/';
  {$ENDIF}
  fnm := basenm + 'black.png'; //freeware icon from http://www.softicons.com/holidays-icons/scuba-diving-icons-by-diveandgo.com/fish-icon
  gGLText := TGLText.Create(fnm, true, success, GLBox);
  if not success then
    showmessage('Error: unable to load .png and .fnt '+fnm);
  fnm := basenm + 'arial.png';//'arial.png';//'serif.png';
  gGLText2 := TGLText.Create(fnm, true, success, GLBox);
  if not success then
    showmessage('Error: unable to load .png and .fnt '+fnm);
  //gGLText.TextOut(gPositionX,gPositionY,gZoom, gStr);
  GLBox.OnPaint:= @GLboxPaint;
  gGLText2.TextColor(0,0,0);//black
  gGLText2.TextOut(6,66,2, 45, 'The five boxing wizards jump quickly.');
  gGLText2.TextOut(6,36,1, 'Pack my box with five dozen liquor jugs.');
  gGLText2.TextOut(6,16,0.5, 'Sphinx of black quartz, judge my vow.');
  UpdateText;
  if GLErrorStr <> '' then begin
     showmessage(GLErrorStr);
     GLErrorStr := '';
  end;
end;

procedure TGLForm1.UpdateText;
begin
     gGLText.ClearText;
     gGLText.TextOut(gPositionX,gPositionY,gZoom, gStr);
     GLBox.Invalidate;
end;

procedure TGLForm1.GLboxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  {$IFDEF RETINA}
  if (ssShift in Shift) then begin
     gIsRetina := not gIsRetina;
     SetRetina;
     gPositionX := 0;
     gPositionY := 0;
     UpdateText;
  end;
 {$ENDIF}
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
  end;
  gMouseY := Y;
  gMouseX := X;
  UpdateText;
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
  {$IFDEF RETINA} //requires Lazarus 1.9 svn 55355 or later
  nglOrtho (0, Round(GLBox.Width * LBackingScaleFactor(GLBox.Handle)), 0, Round(GLBox.Height * LBackingScaleFactor(GLBox.Handle)), 0.1, 40);
  {$ELSE}
  nglOrtho (0, GLBox.ClientWidth, 0, GLBox.ClientHeight, 0.1, 40);
  {$ENDIF}
  glClearColor(0.3, 0.5, 0.8, 1.0); //Set blue background
  glClear(GL_COLOR_BUFFER_BIT);
    glEnable (GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  gGLText2.DrawText;
  gGLText.DrawText;
  GLbox.SwapBuffers;
  //caption := inttostr(gGLText.Crap);
end;

end.

