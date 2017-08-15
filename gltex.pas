unit gltex;
//openGL texture

{$mode objfpc}{$H+}

interface
{$include opts.inc} //<- defines CORE OpenGL >=3.3, else uses LEGACY OpenGL 2.1


uses
  {$IFDEF COREGL}glcorearb, gl_core_utils, gl_core_matrix, {$ELSE}gl, glext, {$ENDIF}
  Classes, SysUtils, Graphics, OpenGLContext;

type
Txyuv = Packed Record
  x,y   : single; //vertex coordinates
  u,v : single; //texture coordinates
end;
  TGLTex = class
  private
         {$IFDEF COREGL}vbo, vao,{$ENDIF}
         tex, shaderProgram: GLuint;
         {$IFDEF COREGL}uniform_mtx, {$ENDIF} uniform_tex: GLint;
         bmpHt, bmpWid: integer;
         {$IFNDEF COREGL}Sq : packed array [0..3] of Txyuv;{$ENDIF}

    {$IFDEF COREGL}procedure LoadBufferData;{$ENDIF}
    procedure LoadTex(fnm : string);
    procedure updateVboCore(x,y, z: single);
  public
    procedure DrawTex; //must be called while TOpenGLControl is current context
    procedure updateVbo(x,y, z: single; Ctx: TOpenGLControl);
    constructor Create(fnm : string; Ctx: TOpenGLControl); //overlod;
    Destructor  Destroy; override;
  end;
  {$IFNDEF COREGL}var GLErrorStr : string = '';{$ENDIF}

implementation

const
{$IFDEF COREGL}
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
{$ELSE} //if core opengl, else legacy shaders
kVert ='varying vec4 vClr;'
+#10'void main() {'
+#10'    gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;'
+#10'    vClr = gl_Color;'
+#10'}';

const kFrag = 'varying vec4 vClr;'
+#10'uniform sampler2D tex;'
+#10'uniform vec4 clr;'
+#10'void main() {'
+#10'  gl_FragColor = texture2D(tex,vClr.xy);'
+#10'}';
{$ENDIF}

procedure TGLTex.updateVboCore(x,y, z: single);
var
    {$IFDEF COREGL}Sq : packed array [0..3] of Txyuv;{$ENDIF}
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
  {$IFDEF COREGL}
  glBindBuffer(GL_ARRAY_BUFFER, vbo);
  glBufferSubData(GL_ARRAY_BUFFER,0,sizeof(Sq),@Sq[0]);
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  {$ENDIF}
end;

procedure TGLTex.updateVbo(x,y, z: single; Ctx: TOpenGLControl);
begin
   {$IFDEF COREGL}Ctx.MakeCurrent(false);{$ENDIF}
   updateVboCore(x,y,z);
   {$IFDEF COREGL}Ctx.ReleaseContext;{$ENDIF}
end;

{$IFDEF COREGL}
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
  uniform_mtx := glGetUniformLocation(shaderProgram, pAnsiChar('ModelViewProjectionMatrix'));
end;
{$ENDIF}

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
  {$ELSE}
  if px.Bitmap.PixelFormat = pf32bit then
     internalformat := GL_RGBA
  else
      internalformat := GL_RGB;
  {$ENDIF}
   glTexImage2D(GL_TEXTURE_2D, 0,GL_RGBA8, px.Width, px.Height, 0, internalformat, GL_UNSIGNED_BYTE, PInteger(px.Bitmap.RawImage.Data));
  px.Free;
end;

{$IFNDEF COREGL} //Shaders for Legacy OpenGL
procedure ReportCompileShaderError(glObjectID: GLuint);
var
  s : string;
  maxLength, status : GLint;
begin
  status := 0;
    glGetShaderiv(glObjectID, GL_COMPILE_STATUS, @status);
    if (status <> 0) then exit; //report compiling errors.
    glGetError;
    glGetShaderiv(glObjectID, GL_INFO_LOG_LENGTH, @maxLength);
     setlength(s, maxLength);
     glGetShaderInfoLog(glObjectID, maxLength, @maxLength, @s[1]);
     s:=trim(s);
     GLErrorStr := ('Compile Shader error '+s);
end;

procedure ReportCompileProgramError(glObjectID: GLuint);
var
  s : string;
  maxLength : GLint;
begin
  glGetProgramiv(glObjectID, GL_LINK_STATUS, @maxLength);
  if (maxLength = GL_TRUE) then exit;
  maxLength := 4096;
  setlength(s, maxLength);
  glGetProgramInfoLog(glObjectID, maxLength, @maxLength, @s[1]);
  s:=trim(s);
  if length(s) < 2 then exit;
  GLErrorStr := ('Program compile error '+s);
end;

function compileShaderOfType (shaderType: GLEnum;  shaderText: string): GLuint;
begin
   result := glCreateShader(shaderType);
   glShaderSource(result, 1, PChar(@shaderText), nil);
   glCompileShader(result);
   ReportCompileShaderError(result);
end;

procedure GetError(p: integer);  //report OpenGL Error
var
  Error: GLenum;
  s: string;
begin
 Error := glGetError();
 if Error = GL_NO_ERROR then exit;
 s := inttostr(p)+'->' + inttostr(Error);
 GLErrorStr := ('OpenGL error : '+s );
end;

function  initVertFrag(vert, frag: string): GLuint;
var
   fs, vs: GLuint;
begin
  result := 0;
  glGetError(); //clear errors
  result := glCreateProgram();
  if (length(vert) > 0) then begin
     vs := compileShaderOfType(GL_VERTEX_SHADER, vert);
     if (vs = 0) then exit;
     glAttachShader(result, vs);
  end;
  fs := compileShaderOfType(GL_FRAGMENT_SHADER, frag);
  if (fs = 0) then exit;
  glAttachShader(result, fs);
  glLinkProgram(result);
  ReportCompileProgramError(result);
  if (length(vert) > 0) then begin
     glDetachShader(result, vs);
     glDeleteShader(vs);
  end;
  glDetachShader(result, fs);
  glDeleteShader(fs);
  glUseProgram(0);
  GetError(1);
end;
{$ENDIF} //{$IFNDEF COREGL}

constructor TGLTex.Create(fnm: string; Ctx: TOpenGLControl);
begin
  {$IFDEF COREGL}
  vbo := 0;
  vao := 0;
  {$ENDIF}
  tex := 0;
  shaderProgram := 0;
  uniform_tex := 0;
  if not fileexists(fnm) then exit;
  Ctx.MakeCurrent();
  shaderProgram :=  initVertFrag(kVert,  kFrag);
  uniform_tex := glGetUniformLocation(shaderProgram, pAnsiChar('tex'));
  LoadTex(fnm);
  {$IFDEF COREGL}LoadBufferData;{$ENDIF}
  updateVboCore(0.0,0.0, 1);
  glFinish;
  Ctx.ReleaseContext;
end;

procedure TGLTex.DrawTex;
var
   z: single = -1;
{$IFDEF COREGL}
var
  mvp : TnMat44;
{$ENDIF}
begin
  glUseProgram(shaderProgram);
  glActiveTexture(GL_TEXTURE1);
  glBindTexture(GL_TEXTURE_2D, tex);
  glUniform1i(uniform_tex, 1);
  {$IFDEF COREGL}
  mvp := ngl_ModelViewProjectionMatrix;
  glUniformMatrix4fv(uniform_mtx, 1, GL_FALSE, @mvp[0,0]);
  glBindVertexArray(vao);
  glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);
  glBindVertexArray(0);
  {$ELSE}
    glBegin(GL_TRIANGLES);
        glColor3f(sQ[0].u, sQ[0].v, 1.0);
        glVertex3f(sQ[0].x, sQ[0].y, z);
        glColor3f(sQ[1].u, sQ[1].v, 1.0);
        glVertex3f(sQ[1].x, sQ[1].y, z);
        glColor3f(sQ[2].u, sQ[2].v, 1.0);
        glVertex3f(sQ[2].x, sQ[2].y, z);
        glColor3f(sQ[2].u, sQ[2].v, 1.0);
        glVertex3f(sQ[2].x, sQ[2].y, z);
        glColor3f(sQ[1].u, sQ[1].v, 1.0);
        glVertex3f(sQ[1].x, sQ[1].y, z);
        glColor3f(sQ[3].u, sQ[3].v, 1.0);
        glVertex3f(sQ[3].x, sQ[3].y, z);
    glEnd();
  //glCallList(displayLst);
  {$ENDIF}
  glUseProgram(0);
end;

destructor TGLTex.Destroy;
begin
  //call the parent destructor:
  inherited;
end;

end.

