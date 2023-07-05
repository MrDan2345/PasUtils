unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  CommonUtils, MediaUtils, gl, Windows;

type

  { TForm1 }

  TForm1 = class(TForm)
    Timer1: TTimer;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    var DeviceContext: HDC;
    var RenderingContext: HGLRC;
    var Scene: TUSceneData;
    procedure InitializeGL;
    procedure FinalizeGL;
    procedure InitializeData;
    procedure FinalizeData;
    procedure Render;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  InitializeGL;
  InitializeData;
  Timer1.Enabled := True;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Timer1.Enabled := False;
  FinalizeData;
  FinalizeGL;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
  var WinRect: TRect;
begin
  WinRect := ClientRect;
  glViewport(0, 0, WinRect.Right - WinRect.Left, WinRect.Bottom - WinRect.Top);
  glClearColor(0.5, 0.5, 0.5, 1);
  glClearDepth(1);
  glClear(GL_COLOR_BUFFER_BIT);
  Render;
  SwapBuffers(DeviceContext);
end;

procedure TForm1.InitializeGL;
  var pfd: TPixelFormatDescriptor;
  var pf: Int32;
begin
  DeviceContext := GetDC(Handle);
  UClear(pfd, SizeOf(pfd));
  pfd.nSize := SizeOf(pfd);
  pfd.nVersion := 1;
  pfd.dwFlags := PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER;
  pfd.iPixelType := PFD_TYPE_RGBA;
  pfd.cColorBits := 32;
  pfd.cAlphaBits := 8;
  pfd.cDepthBits := 16;
  pfd.iLayerType := PFD_MAIN_PLANE;
  pf := ChoosePixelFormat(DeviceContext, @pfd);
  SetPixelFormat(DeviceContext, pf, @pfd);
  RenderingContext := wglCreateContext(DeviceContext);
  wglMakeCurrent(DeviceContext, RenderingContext);
end;

procedure TForm1.FinalizeGL;
begin
  wglMakeCurrent(DeviceContext, RenderingContext);
  wglDeleteContext(RenderingContext);
  ReleaseDC(Handle, DeviceContext);
end;

procedure TForm1.InitializeData;
begin
  Scene := TUSceneDataDAE.Create();
  Scene.Load('Assets/Skin.dae');
  WriteLn(Length(Scene.MeshList));
end;

procedure TForm1.FinalizeData;
begin
  Scene.Free;
end;

procedure TForm1.Render;
  procedure SetupTransforms;
    var W, V, P, WV: TUMat;
  begin
    W := TUMat.RotationY(((GetTickCount mod 6000) / 6000) * 2 * Pi);
    V := TUMat.View(TUVec3.Make(0, 5, -5), TUVec3.Make(0, 0, 0), TUVec3.Make(0, 1, 0));
    P := TUMat.Proj(Pi / 4, 1, 1, 100);
    WV := W * V;
    glMatrixMode(GL_MODELVIEW);
    glLoadMatrixf(@WV);
    glMatrixMode(GL_PROJECTION);
    glLoadMatrixf(@P);
  end;
  procedure RenderMesh(Mesh: TUSceneData.TMeshInterface);
    procedure RenderSubset(Subset: TUSceneData.TMeshSubsetInterface);
      var i: Int32;
      var Vertex: PUVec3;
    begin
      for i := 0 to Subset.IndexCount - 1 do
      begin
        Vertex := PUVec3(Subset.VertexData + (Subset.Index[i] * Subset.VertexSize));
        glVertex3fv(PGLFloat(Vertex));
      end;
    end;
    var i: Int32;
  begin
    for i := 0 to High(Mesh.Subsets) do
    begin
      RenderSubset(Mesh.Subsets[i]);
    end;
  end;
  var i: Int32;
  const s = 0.9;
begin
  SetupTransforms;
  //glEnable(GL_TEXTURE_2D);
  glShadeModel(GL_FLAT);
  glShadeModel(GL_SMOOTH);
  glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
  glDisable(GL_CULL_FACE);
  //glEnable(GL_BLEND);
  glBegin(GL_TRIANGLES);
  glVertex2f(-s, -s); glVertex2f(-s, s); glVertex2f(s, s);
  glVertex2f(-s, -s); glVertex2f(s, s); glVertex2f(s, -s);
  glEnd();
  glBegin(GL_TRIANGLES);
  for i := 0 to High(Scene.MeshList) do
  begin
    RenderMesh(Scene.MeshList[i]);
  end;
  glEnd();
end;

end.

