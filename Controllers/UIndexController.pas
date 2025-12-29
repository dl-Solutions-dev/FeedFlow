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
  TIndexController = class( TBaseController )
  public
    procedure Main( Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
    procedure Login( Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
    // TODO: à supprimer
    procedure Redirect( Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );

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
  Utils.Token, UControllersList;

const
  TMP_LOGIN: string = 'IndexAdmin.html';

  { TIndexController }

procedure TIndexController.Redirect( Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean );
var
  LDM: TDMSession;
  LToken: TToken;
begin
  LDM := GetDMSession( Request );
  if Assigned( LDM ) then
  begin
    if ValidToken( Request, False, True, LToken ) then
    begin
      if ( LToken.Role = 'USER' ) then
      begin
        Response.SendRedirect( './Home' );
      end
      else if ( LToken.Role = 'ADMIN' ) then
      begin
        Response.SendRedirect( './FeedsList?scope=Page' );
      end;

      FreeAndNil(LToken);
    end
    else
    begin
      Response.StatusCode := 401;
      Response.Content := 'Accès non autorisérisé.';
    end;
  end;
end;

procedure TIndexController.InitializeActions( aWebModule: TWebModule;
  aWebStencil: TWebStencilsEngine );
begin
  inherited;

  aWebModule.AddRoutes( [
      TRoute.Create( mtGet, '/', Self.Main ),
      TRoute.Create( mtPost, '/Login', Self.Login ),
      TRoute.Create( mtGet, '/Redirect', Self.Redirect )
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

  TControllersList.GetControllersList.AddClass(TIndexController);

end.

