unit gl_core_utils;
interface

uses
  glcorearb, SysUtils;

  function  initVertFrag(vert, frag: string): GLuint;
var
   GLErrorStr: string = '';

implementation

procedure ReportErrorsGL(glObjectID: GLuint);
var
  s : string;
  maxLength : GLint;
begin
  glGetShaderiv(glObjectID, GL_INFO_LOG_LENGTH, @maxLength);
  if (maxLength < 2) then exit;
  setlength(s, maxLength);
  glGetShaderInfoLog(glObjectID, maxLength, @maxLength, @s[1]);
  s:=trim(s);
  if GLErrorStr = '' then
     GLErrorStr := 'GLSL error '+s;
end;

procedure GetError(p: integer);  //report OpenGL Error
var
  Error: GLenum;
  s: string;
begin
 Error := glGetError();
 if Error = GL_NO_ERROR then exit;
  s := inttostr(p)+'->';
 if Error = GL_INVALID_ENUM then
    s := s+'GL_INVALID_ENUM'
 else if Error = GL_INVALID_VALUE then
    s := s+'GL_INVALID_VALUE'
 else
     s := s + inttostr(Error);
 if GLErrorStr = '' then
    GLErrorStr := 'GLSL error '+s;
end;

function compileShaderOfType (shaderType: GLEnum;  shaderText: string): GLuint;
var
   status: GLint;
begin
     result := glCreateShader(shaderType);
     glShaderSource(result, 1, PChar(@shaderText), nil);
     glCompileShader(result);
     ReportErrorsGL(result);
     status := 0;
     glGetShaderiv(result, GL_COMPILE_STATUS, @status);
     if (status =  0) and (GLErrorStr = '') then begin //report compiling errors.
        GLErrorStr := 'GLSL shader compile failure';
     end;
end;

function  initVertFrag(vert, frag: string): GLuint;
var
	fr, vt: GLuint;
begin
    result := 0;
    vt := compileShaderOfType(GL_VERTEX_SHADER, vert);
    fr := compileShaderOfType(GL_FRAGMENT_SHADER, frag);
    if (fr = 0) or (vt = 0) then exit;
    result := glCreateProgram();
    glAttachShader(result, vt);
    glAttachShader(result, fr);
    glBindFragDataLocation(result, 0, 'FragColor');
    glLinkProgram(result);
    glDeleteShader(vt);
    glDeleteShader(fr);
    GetError(1);
end;

end.

