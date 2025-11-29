unit UUserFeedsController;

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
  TUserFeedsController = class( TBaseController )
  public
    procedure Home( Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
    procedure GetApps( Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );

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
  uInvokerActions,
  UWMMain,
  Utils.Logger,
  UPagination,
  Helpers.Messages,
  Utils.Token;

const
  TMP_HOME: string = 'Home.html';
  TMP_APPS: string = 'Apps.html';

  { TUserFeedsController }

procedure TUserFeedsController.GetApps( Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean );
var
  LDM: TDMSession;
  LToken: TToken;
begin
  LDM := GetDMSession( Request );
  if Assigned( LDM ) then
  begin
    if ValidToken( Request, True, True, LToken ) then
    begin
      if ( LToken.Role = 'ADMIN' ) then
      begin
        LDM.MtUrls.Open;
        LDM.MtUrls.Insert;
        LDM.MtUrlsURL.Value := './FeedsList?scope=Page';
        LDM.MtUrlsImageFileName.Value := 'parametre.png';
        LDM.MtUrlsAlt.Value := 'Paramètres';
        LDM.MtUrls.Post;
      end;

      LDM.MtUrls.Open;
      LDM.MtUrls.Insert;
      LDM.MtUrlsURL.Value := './FeedsList?scope=Page';
      LDM.MtUrlsImageFileName.Value := 'parametre.png';
      LDM.MtUrlsAlt.Value := 'Paramètres';
      LDM.MtUrls.Post;

      LDM.MtUrls.Open;
      LDM.MtUrls.Insert;
      LDM.MtUrlsURL.Value := './FeedsList?scope=Page';
      LDM.MtUrlsImageFileName.Value := 'parametre.png';
      LDM.MtUrlsAlt.Value := 'Paramètres';
      LDM.MtUrls.Post;

      FWebStencilsProcessor.AddVar( 'Urls', LDM.MtUrls, False );

      Response.Content := RenderTemplate( TMP_APPS, Request );

      LDM.MtUrls.Close;
    end
    else
    begin
      Response.StatusCode := 401;
      Response.Content := 'Accès non autorisé.';
    end;
  end;
end;

procedure TUserFeedsController.Home( Sender: TObject; Request: TWebRequest;
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
      Response.Content := RenderTemplate( TMP_HOME, Request );
    end
    else
    begin
      Response.StatusCode := 401;
      Response.Content := 'Accès non autorisé.';
    end;
  end;
end;

procedure TUserFeedsController.InitializeActions( aWebModule: TWebModule;
  aWebStencil: TWebStencilsEngine );
begin
  inherited;

  aWebModule.AddRoutes( [
      TRoute.Create( mtGet, '/Home', Self.Home ),
      TRoute.Create( mtPost, '/GetApps', Self.GetApps )
      ] );
end;

initialization

  TInvokerActions.GetInvokerActions.AddAction( TUserFeedsController.Create );

end.

