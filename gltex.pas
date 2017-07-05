unit gltex;
//openGL texture

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, glcorearb, gl_core_utils, Graphics, gl_core_matrix, OpenGLContext;

type
  TGLTex = class
  private
         tex, vbo, vao, shaderProgram: GLuint;
         uniform_tex, uniform_mtx: GLint;
         bmpHt, bmpWid: integer;
    procedure LoadBufferData;
    procedure LoadTex(fnm : string);
    procedure updateVboCore(x,y, z: single);
  public
    procedure DrawTex; //must be called while TOpenGLControl is current context
    procedure updateVbo(x,y, z: single; Ctx: TOpenGLControl);
    constructor Create(fnm : string; Ctx: TOpenGLControl); //overlod;
    Destructor  Destroy; override;
  end;

implementation

const
 //Simple Vertex Shader
    kVert = '#version 330'
+#10'layout(location = 0) in vec2 point;'
+#10'layout(location = 1) in vec2 uvX;'
+#10'uniform mat4 ModelViewProjectionMatrix;'
+#10'out vec2 uv;'
+#10'void main() {'
+#10'    uv = uvX;'
+#10'    vec2 ptx = point;'
+#10'    gl_Position = ModelViewProjectionMatrix * vec4(ptx, -0.5, 1.0);'
+#10'}';

//Simple Fragment Shader
    kFrag = '#version 330'
+#10'in vec2 uv;'
+#10'out vec4 color;'
+#10'uniform sampler2D tex;'
+#10'void main() {'
+#10'    color = texture(tex,uv);'
+#10'}';

type
Txyuv = Packed Record
  x,y   : single; //vertex coordinates
  u,v : single; //texture coordinates
end;

procedure TGLTex.updateVboCore(x,y, z: single);
var
    Sq : packed array [0..3] of Txyuv;
    i : integer;
begin
  Sq[0].u := 0;Sq[0].v := 1;
  Sq[1].u := 0;Sq[1].v := 0;
  Sq[2].u := 1;Sq[2].v := 1;
  Sq[3].u := 1;Sq[3].v := 0;
  for i := 0 to 3 do begin
      Sq[i].x := (z * bmpWid * Sq[i].u) + x;
      Sq[i].y := (z * bmpHt * Sq[i].v) + y;
  end;
  glBindBuffer(GL_ARRAY_BUFFER, vbo);
  glBufferSubData(GL_ARRAY_BUFFER,0,sizeof(Sq),@Sq[0]);
  glBindBuffer(GL_ARRAY_BUFFER, 0);
end;

procedure TGLTex.updateVbo(x,y, z: single; Ctx: TOpenGLControl);
begin
   Ctx.MakeCurrent(false);
   updateVboCore(x,y,z);
   Ctx.ReleaseContext;
end;

procedure TGLTex.LoadBufferData;
const
    kATTRIB_POINT = 0; //XY position on screen
    kATTRIB_UV = 1; //UV coordinates of texture
begin
  glGenBuffers(1, @vbo);
  glBindBuffer(GL_ARRAY_BUFFER, vbo);
  //glBufferData(GL_ARRAY_BUFFER, sizeof(sq), @sq[0], GL_DYNAMIC_DRAW); //GL_STATIC_DRAW
  glBufferData(GL_ARRAY_BUFFER, 4 * sizeof(Txyuv), nil, GL_DYNAMIC_DRAW); //GL_STATIC_DRAW
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glGenVertexArrays(1, @vao);
  glBindVertexArray(vao);
  glBindBuffer(GL_ARRAY_BUFFER, vbo);
  glVertexAttribPointer(kATTRIB_POINT, 2, GL_FLOAT, GL_FALSE, sizeof(Txyuv), PChar(0));
  glEnableVertexAttribArray(kATTRIB_POINT);
  glVertexAttribPointer(kATTRIB_UV, 2, GL_FLOAT, GL_FALSE, sizeof(Txyuv), PChar(sizeof(single)*2));
  glEnableVertexAttribArray(kATTRIB_UV);
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glBindVertexArray(0);
  uniform_tex := glGetUniformLocation(shaderProgram, pAnsiChar('tex'));
  uniform_mtx := glGetUniformLocation(shaderProgram, pAnsiChar('ModelViewProjectionMatrix'));
  updateVboCore(0.0,0.0, 1);
end;

procedure FlipVertical (var px: TPicture);
var
  p: array of byte;
  i, half, b: integer;
  LoPtr, HiPtr: PInteger;
begin
    if px.Height < 3 then exit;
    half := (px.Height div 2);
    b := px.Bitmap.RawImage.Description.BytesPerLine;
    LoPtr := PInteger(px.Bitmap.RawImage.Data);
    HiPtr := PInteger(px.Bitmap.RawImage.Data+ ((px.Height -1) * b));
    setlength(p, b);
    for i := 1 to half do begin
          System.Move(LoPtr^,p[0],b); //(src, dst,sz)
          System.Move(HiPtr^,LoPtr^,b);
          System.Move(p[0],HiPtr^,b);
          Inc(PByte(LoPtr), b );
          Dec(PByte(HiPtr), b);
    end;
end; //FlipVertical()

procedure TGLTex.LoadTex(fnm: string);
var
  px: TPicture;
  internalformat: GLint;
begin
  px := TPicture.Create;
  try
     px.LoadFromFile(fnm);
  except
    px.Bitmap.Width:=0;
  end;
  if (px.Bitmap.PixelFormat <> pf32bit ) or (px.Bitmap.Width < 1) or (px.Bitmap.Height < 1) then begin
     //showmessage('Error loading 32-bit power-of-two bitmap '+fnm);
     exit;
  end;
  FlipVertical (px); //flip vertical: pngs stored so first line is top of screen, OpenGL usually uses cartesian coordinates where increasing Y moves upwards
  bmpHt := px.Bitmap.Height;
  bmpWid := px.Bitmap.Width;
  glGenTextures(1, @tex);
  glBindTexture(GL_TEXTURE_2D,  tex);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_BORDER);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_BORDER);
   {$IFDEF Darwin}
  if px.Bitmap.PixelFormat = pf32bit then
     internalformat := GL_BGRA
  else
      internalformat := GL_BGR;
  //glTexImage2D(GL_TEXTURE_2D, 0,, px.Width, px.Height, 0, , GL_UNSIGNED_BYTE, PInteger(px.Bitmap.RawImage.Data));
  {$ELSE}
  if px.Bitmap.PixelFormat = pf32bit then
     internalformat := GL_RGBA
  else
      internalformat := GL_RGB;
  {$ENDIF}
   glTexImage2D(GL_TEXTURE_2D, 0,GL_RGBA8, px.Width, px.Height, 0, internalformat, GL_UNSIGNED_BYTE, PInteger(px.Bitmap.RawImage.Data));
  px.Free;
end;

constructor TGLTex.Create(fnm: string; Ctx: TOpenGLControl);
begin
  tex := 0;
  vbo := 0;
  vao := 0;
  shaderProgram := 0;
  uniform_tex := 0;
  if not fileexists(fnm) then exit;
  Ctx.MakeCurrent();
  shaderProgram :=  initVertFrag(kVert,  kFrag);
  LoadTex(fnm);
  LoadBufferData;
  glFinish;
  Ctx.ReleaseContext;
end;

procedure TGLTex.DrawTex;
var
  mvp : TnMat44;
begin
  glUseProgram(shaderProgram);
  mvp := ngl_ModelViewProjectionMatrix;
  glUniformMatrix4fv(uniform_mtx, 1, GL_FALSE, @mvp[0,0]);
  glBindVertexArray(vao);
  glActiveTexture(GL_TEXTURE1);
  glBindTexture(GL_TEXTURE_2D, tex);
  glUniform1i(uniform_tex, 1);
  glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);
  glBindVertexArray(0);
  glUseProgram(0);
end;

destructor TGLTex.Destroy;
begin
  //call the parent destructor:
  inherited;
end;

end.

