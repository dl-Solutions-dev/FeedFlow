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
  File last update : 2026-01-04T14:37:08.615+01:00
  Signature : 0ed60deb2b83c27cd3644558f6bcaeff9420daed
  ***************************************************************************
*)

/// <summary>
///   Controller pour la page d'accueil
/// </summary>
unit UIndexController;

interface

uses
  System.Classes,
  System.SysUtils,
  System.JSON,
  Web.HTTPApp,
  Web.Stencils,
  uBaseController,
  uInterfaces,
  UDMSession;

type
  /// <summary>
  ///   Class du controller
  /// </summary>
  TIndexController = class( TBaseController )
  public
    /// <summary>
    ///   Endpoint initial (affiche le template d'authentification
    /// </summary>
    procedure Main( Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
    /// <summary>
    ///   Endpoint pour le controle des identifiants
    /// </summary>
    /// <remarks>
    ///   C'est dans cette méthode qu'il faut implémenter l'authentification
    ///   de l'utilisateur
    /// </remarks>
    procedure Login( Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
    /// <summary>
    ///   Initialisation des routes
    /// </summary>
    procedure InitializeActions( aWebModule: TWebModule; aWebStencil: TWebStencilsEngine ); override;
  end;

implementation

uses
  System.SyncObjs,
  System.IOUtils,
  System.Generics.Collections,
  System.StrUtils,
  Web.ReqMulti,
  IdHTTP,
  Web.ReqFiles,
  FireDAC.Stan.Param,
  utils.ClassHelpers,
  UConsts,
  UWMMain,
  Utils.Logger,
  UPagination,
  Helpers.Messages,
  Utils.Token,
  UControllersRegistry;

const
  /// <summary>
  ///   Nom du template HTML
  /// </summary>
  TMP_LOGIN: string = 'IndexAdmin.html';

  { TIndexController }

procedure TIndexController.InitializeActions( aWebModule: TWebModule;
  aWebStencil: TWebStencilsEngine );
begin
  inherited;

  aWebModule.AddRoutes( [
      TRoute.Create( mtGet, '/', Self.Main ),
      TRoute.Create( mtPost, '/Login', Self.Login )
      ] );
end;

procedure TIndexController.Login( Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean );
var
  LDM: TDMSession;
  LJwt: TToken;
  LToken: string;
  LCookie: TStrings;
begin
  LDM := GetDMSession( Request );

  if Assigned( LDM ) then
  begin
    if Request.ContentFields.Values[ 'username' ] = 'Admin' then
    begin
      LJwt := TToken.Create;
      LToken := LJwt.CreateToken(
        Request.ContentFields.Values[ 'username' ],
        'FR',
        'fr',
        '1',
        '1',
        'ADMIN' );

      LCookie := TStringList.Create;
      LCookie.Values[ 'jwt' ] := LToken;

      Response.SetCookieField( LCookie, '', '/', Now + 1, False, True );

      FreeAndNil( LCookie );

      Response.Content := Format( '{"token": "%s"}', [ LToken ] );

      lJwt.Free;
    end
    else if Request.ContentFields.Values[ 'username' ] = 'User' then
    begin
      LJwt := TToken.Create;
      LToken := LJwt.CreateToken(
        Request.ContentFields.Values[ 'username' ],
        'FR',
        'en',
        '1',
        '1',
        'USER' );

      LCookie := TStringList.Create;
      LCookie.Values[ 'jwt' ] := LToken;

      Response.SetCookieField( LCookie, '', '/', Now + 1, False, True );

      FreeAndNil( LCookie );

      Response.Content := Format( '{"token": "%s"}', [ LToken ] );

      lJwt.Free;
    end
    else
    begin
      Response.StatusCode := 401;
      Response.Content := '{"error":"Identifiants invalides"}';
    end;
  end;
end;

procedure TIndexController.Main( Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean );
begin
  Response.Content := RenderTemplate( TMP_LOGIN, Request );
end;

initialization

  TControllersRegistry.GetControllersList.AddClass( TIndexController );

end.

