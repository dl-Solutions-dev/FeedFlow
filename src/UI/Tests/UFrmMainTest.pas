(* C2PP
  ***************************************************************************

  Feed Flow

    Copyright 2026 - Dany Leblanc under AGPL 3.0 license.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
  OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
    THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
  OTHER
    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
    FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
    DEALINGS IN THE SOFTWARE.

  ***************************************************************************

  This program is a demo of the possibilities offered by the new WebStencils
  framework, combined with the HTMX JavaScript library.

  ***************************************************************************

  Author(s) :
  Dany Leblanc

  Project site :
  https://github.com/dl-Solutions-dev/FeedFlow

  ***************************************************************************
  File last update : 2026-01-04T14:37:08.907+01:00
  Signature : 6ed44551a44e62253062a777d29c03ec5f52c423
  ***************************************************************************
*)

/// <summary>
///   UI du serveur de tests
/// </summary>
unit UFrmMainTest;

interface

uses
  Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.AppEvnts, Vcl.StdCtrls, IdHTTPWebBrokerBridge, IdGlobal, Web.HTTPApp;

type
  /// <summary>
  ///   class du formulaire VCL
  /// </summary>
  TFrmMainTest = class(TForm)
    /// <summary>
    ///   Bouton permettant de démarrer le serveur web de tests
    /// </summary>
    ButtonStart: TButton;
    /// <summary>
    ///   Bouton permettant de stopper le serveur web de tests
    /// </summary>
    ButtonStop: TButton;
    /// <summary>
    ///   Zone édition pour modifier le port d'écoute du serveur de tests
    /// </summary>
    EditPort: TEdit;
    /// <summary>
    ///   Zone label
    /// </summary>
    Label1: TLabel;
    /// <summary>
    ///   Composant permettant d'intercépter le events windows
    /// </summary>
    ApplicationEvents1: TApplicationEvents;
    /// <summary>
    ///   Bouton permettant d'ouvrir le browser sur la page d'accueil
    /// </summary>
    ButtonOpenBrowser: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure ButtonOpenBrowserClick(Sender: TObject);
  private
    /// <summary>
    ///   Instance du serveur de tests
    /// </summary>
    FServer: TIdHTTPWebBrokerBridge;
    procedure StartServer;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  /// <summary>
  ///   Instance du formulaire
  /// </summary>
  FrmMainTest: TFrmMainTest;

implementation

{$R *.dfm}

uses
{$IFDEF MSWINDOWS}
  WinApi.Windows, Winapi.ShellApi,
{$ENDIF}
  System.Generics.Collections;

procedure TFrmMainTest.ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
begin
  ButtonStart.Enabled := not FServer.Active;
  ButtonStop.Enabled := FServer.Active;
  EditPort.Enabled := not FServer.Active;
end;

procedure TFrmMainTest.ButtonOpenBrowserClick(Sender: TObject);
{$IFDEF MSWINDOWS}
var
  LURL: string;
{$ENDIF}
begin
  StartServer;
{$IFDEF MSWINDOWS}
  LURL := Format('http://localhost:%s', [EditPort.Text]);
  ShellExecute(0,
        nil,
        PChar(LURL), nil, nil, SW_SHOWNOACTIVATE);
{$ENDIF}
end;

procedure TFrmMainTest.ButtonStartClick(Sender: TObject);
begin
  StartServer;
end;

procedure TFrmMainTest.ButtonStopClick(Sender: TObject);
begin
  FServer.Active := False;
  FServer.Bindings.Clear;
end;

procedure TFrmMainTest.FormCreate(Sender: TObject);
begin
  FServer := TIdHTTPWebBrokerBridge.Create(Self);
end;

procedure TFrmMainTest.StartServer;
begin
  if not FServer.Active then
  begin
    FServer.Bindings.Clear;
    FServer.DefaultPort := StrToInt(EditPort.Text);
    FServer.Active := True;
  end;
end;

end.
