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
    procedure GetList( Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
    procedure GetDocuments( Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );

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
  TMP_HOME_DOCUMENTS: string = 'HomeDocuments.html';

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
        LDM.MtUrlsOrdre.Value := 1;
        LDM.MtUrlsURL.Value := './FeedsList?scope=Page';
        LDM.MtUrlsImageFileName.Value := 'parametre.png';
        if LToken.Lang = 'fr' then
        begin
          LDM.MtUrlsAlt.Value := 'Paramètres';
        end
        else
        begin
          LDM.MtUrlsAlt.Value := 'Setup';
        end;
        LDM.MtUrls.Post;
      end;

      LDM.MtUrls.Open;
      LDM.MtUrls.Insert;
      LDM.MtUrlsOrdre.Value := 2;
      LDM.MtUrlsURL.Value := './GetDocuments?idGroup=2';
      LDM.MtUrlsImageFileName.Value := 'parametre.png';
      LDM.MtUrlsAlt.Value := 'Documents';
      LDM.MtUrls.Post;

      LDM.MtUrls.Open;
      LDM.MtUrls.Insert;
      LDM.MtUrlsOrdre.Value := 3;
      LDM.MtUrlsURL.Value := './FeedsList?scope=Page';
      LDM.MtUrlsImageFileName.Value := 'parametre.png';
      LDM.MtUrlsAlt.Value := 'Paramètres';
      LDM.MtUrls.Post;

      LDM.MtUrls.IndexFieldNames := 'Order';

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

procedure TUserFeedsController.GetDocuments( Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
var
  LDM: TDMSession;
  LIdGroup: Integer;
  LToken: TToken;
begin
  //TODO: remettre True
  if ValidToken( Request, False, True, LToken ) then
  begin
    logger.Info( 'ShowNews' );
    LDM := GetDMSession( Request );

    if Assigned( LDM ) then
    begin
      LDM.Critical.Acquire;
      try
        if ( Request.QueryFields.Values[ 'IdGroup' ] <> '' ) then
        begin
          if not ( TryStrToInt( Request.QueryFields.Values[ 'IdGroup' ], LIdGroup ) ) then
          begin
            LIdGroup := 0;
          end;
        end;

        Logger.Info( 'ShowNews, LIdGroup : ' + LIdGroup.ToString );

        LDM.QryFeedsUser.ParamByName( 'FEED_GROUP' ).AsInteger := LIdGroup;
        LDM.QryFeedsUser.ParamByName( 'COUNTRY_CODE' ).AsString := LToken.Country;
        LDM.QryFeedsUser.ParamByName( 'LANGUAGE_CODE' ).AsString := LToken.Lang;
        LDM.QryFeedsUser.ParamByName( 'CATEGORY_ID' ).AsInteger := LToken.Category.ToInteger;
        LDM.QryFeedsUser.ParamByName( 'SUBCATEGORY_ID' ).AsInteger := LToken.SubCatgegory.ToInteger;
        LDM.QryFeedsUser.Open;

        FWebStencilsProcessor.AddVar( 'DocumentsList', LDM.QryFeedsUser, False );

        Response.ContentType := 'text/html; charset=UTF-8';
        Response.Content := RenderTemplate( TMP_HOME_DOCUMENTS, Request );

        LDM.QryFeedsUser.Close;
      finally
        LDM.Critical.Release;
      end;
    end;
  end;
end;

procedure TUserFeedsController.GetList( Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean );
var
  LDM: TDMSession;
  LIdFeed: Integer;
  LToken: TToken;
begin
  //TODO: remettre True
  if ValidToken( Request, False, True, LToken ) then
  begin
    logger.Info( 'ShowNews' );
    LDM := GetDMSession( Request );

    if Assigned( LDM ) then
    begin
      LDM.Critical.Acquire;
      try
        logger.Info( 'ContentField -> ' + Request.QueryFields.Text );
        logger.Info( 'QueryField -> ' + Request.QueryFields.Text );

        if ( Request.QueryFields.Values[ 'IdFeed' ] <> '' ) then
        begin
          if not ( TryStrToInt( Request.QueryFields.Values[ 'IdFeed' ], LIdFeed ) ) then
          begin
            LIdFeed := 0;
          end;
        end;

        LDM.qryFeeds.close;
        LDM.qryFeeds.ParamByName( 'FEED_ID' ).AsInteger := LIdFeed;
        LDM.qryFeeds.Open;

        logger.Info( 'LIdFeed -> ' + LIdFeed.ToString );
        logger.Info( 'qry eof : ' + if ( LDM.qryFeeds.Eof ) then
            'Oui'
          else
            'Non' );
        logger.Info( 'Template -> ' + TPath.Combine( FWebStencilsEngine.RootDirectory, LDM.qryFeedsDISPLAY_TEMPLATE.Value ) );

        if FileExists( TPath.Combine( FWebStencilsEngine.RootDirectory, LDM.qryFeedsDISPLAY_TEMPLATE.Value ) ) then
        begin
          Logger.Info( 'ShowNews, LIdFeed : ' + LIdFeed.ToString );

          LDM.QryShowNewsUser.ParamByName( 'FEED_ID' ).AsInteger := LIdFeed;
          LDM.QryShowNewsUser.ParamByName( 'COUNTRY_CODE' ).AsString := LToken.Country;
          LDM.QryShowNewsUser.ParamByName( 'LANGUAGE_CODE' ).AsString := LToken.Lang;
          LDM.QryShowNewsUser.ParamByName( 'CATEGORY_ID' ).AsInteger := LToken.Category.ToInteger;
          LDM.QryShowNewsUser.ParamByName( 'SUBCATEGORY_ID' ).AsInteger := LToken.SubCatgegory.ToInteger;
          LDM.QryShowNewsUser.Open;

          FWebStencilsProcessor.AddVar( 'News', LDM.QryShowNewsUser, False );

          Response.ContentType := 'text/html; charset=UTF-8';
          Response.Content := RenderTemplate( LDM.qryFeedsDISPLAY_TEMPLATE.Value, Request );

          LDM.QryShowNewsUser.Close;
        end
        else
        begin
          response.Content := 'Erreur : Template non trouvé ' + LDM.qryFeedsDISPLAY_TEMPLATE.Value;
        end;

        LDM.qryFeeds.close;
      finally
        LDM.Critical.Release;
      end;
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
      TRoute.Create( mtPost, '/GetApps', Self.GetApps ),
      TRoute.Create( mtPost, '/GetList', Self.GetList ),
      TRoute.Create( mtGet, '/GetDocuments', Self.GetDocuments )
      ] );
end;

initialization

  TInvokerActions.GetInvokerActions.AddAction( TUserFeedsController.Create );

end.

