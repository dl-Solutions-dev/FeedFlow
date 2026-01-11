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
  File last update : 2026-01-04T14:37:08.660+01:00
  Signature : 18685356f23307315152d86a25853a03e0d6b200
  ***************************************************************************
*)

/// <summary>
///   Controller pour la page destinée aux utilisateurs
/// </summary>
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
  /// <summary>
  ///   Class controller
  /// </summary>
  TUserFeedsController = class( TBaseController )
  public
    /// <summary>
    ///   Affiche la page principale
    /// </summary>
    procedure Home( Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
    /// <summary>
    ///   Retourne la liste des applications accessibles à l'utilisateur
    /// </summary>
    procedure GetApps( Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
    /// <summary>
    ///   Retourne le détail des newws en utilisant le template associé
    /// </summary>
    procedure GetList( Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
    /// <summary>
    ///   Retourne la liste des documents autorisés à l'utilisateur
    /// </summary>
    procedure GetDocuments( Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );

    /// <summary>
    ///   Initialise les routes exposées par le controller
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
  UFeeds,
  UNews,
  UControllersRegistry;

const
  /// <summary>
  ///   Nom du template HTML de la page principale
  /// </summary>
  TMP_HOME: string = 'Home.html';
  /// <summary>
  ///   Nom du tempalte HTML pour la liste des applications
  /// </summary>
  TMP_APPS: string = 'Apps.html';
  /// <summary>
  ///   Nom du tempalte HTML de la page des documents
  /// </summary>
  TMP_HOME_DOCUMENTS: string = 'HomeDocuments.html';

  { TUserFeedsController }

procedure TUserFeedsController.GetApps( Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean );
var
  LDM: TDMSession;
begin
  LDM := GetDMSession( Request );
  if Assigned( LDM ) then
  begin
    if ( FWebmodule.Token.Role = 'ADMIN' ) then
    begin
      LDM.MtUrls.Open;
      LDM.MtUrls.Insert;
      LDM.MtUrlsOrdre.Value := 1;
      LDM.MtUrlsURL.Value := './FeedsList?scope=Page';
      LDM.MtUrlsImageFileName.Value := 'parametre.png';
      if FWebmodule.Token.Lang = 'fr' then
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

    //      LDM.MtUrls.Open;
    //      LDM.MtUrls.Insert;
    //      LDM.MtUrlsOrdre.Value := 3;
    //      LDM.MtUrlsURL.Value := './FeedsList?scope=Page';
    //      LDM.MtUrlsImageFileName.Value := 'parametre.png';
    //      LDM.MtUrlsAlt.Value := 'Paramètres';
    //      LDM.MtUrls.Post;

    LDM.MtUrls.IndexFieldNames := 'Order';

    FWebStencilsProcessor.AddVar( 'Urls', LDM.MtUrls, False );

    Response.Content := RenderTemplate( TMP_APPS, Request );

    LDM.MtUrls.Close;
  end;
end;

procedure TUserFeedsController.GetDocuments( Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
var
  LDM: TDMSession;
  LIdGroup: Integer;
  LFeedsUser: TFeedsUser;
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

      LFeedsUser := TFeedsUser( GetSessionObject( Request, 'QryFeedsUser' ) );
      if not ( Assigned( LFeedsUser ) ) then
      begin
        LFeedsUser := TFeedsUser.Create;
        AddSessionObject( Request, 'QryFeedsUser', LFeedsUser );
      end;

      Logger.Info( 'ShowNews, LIdGroup : ' + LIdGroup.ToString );

      //        LDM.QryFeedsUser.ParamByName( 'FEED_GROUP' ).AsInteger := LIdGroup;
      //        LDM.QryFeedsUser.ParamByName( 'COUNTRY_CODE' ).AsString := FWebmodule.Token.Country;
      //        LDM.QryFeedsUser.ParamByName( 'LANGUAGE_CODE' ).AsString := FWebmodule.Token.Lang;
      //        LDM.QryFeedsUser.ParamByName( 'CATEGORY_ID' ).AsInteger := FWebmodule.Token.Category.ToInteger;
      //        LDM.QryFeedsUser.ParamByName( 'SUBCATEGORY_ID' ).AsInteger := FWebmodule.Token.SubCatgegory.ToInteger;
      //        LDM.QryFeedsUser.Open;

      FWebStencilsProcessor.AddVar( 'DocumentsList',
        LFeedsUser.GetFeedsUser( LDM.cnxFeedFlow, LIdGroup, FWebmodule.Token.Category.ToInteger,
          FWebmodule.Token.SubCatgegory.ToInteger,
        FWebmodule.Token.Country, FWebmodule.Token.Lang ),
        False );

      Response.ContentType := 'text/html; charset=UTF-8';
      Response.Content := RenderTemplate( TMP_HOME_DOCUMENTS, Request );

      //        LDM.QryFeedsUser.Close;
    finally
      LDM.Critical.Release;
    end;
  end;
end;

procedure TUserFeedsController.GetList( Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean );
var
  LDM: TDMSession;
  LIdFeed: Integer;
  LFeed: TFeed;
  LShowNews: TShowNewsUser;
  LTemplateName: string;
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

      LFeed := TFeed( GetSessionObject( Request, 'qryFeed' ) );
      if not ( Assigned( LFeed ) ) then
      begin
        LFeed := TFeed.Create;
        AddSessionObject( Request, 'qryFeed', LFeed );
      end;

      LShowNews := TShowNewsUser( GetSessionObject( Request, 'QryShowNewsUser' ) );
      if not ( Assigned( LShowNews ) ) then
      begin
        LShowNews := TShowNewsUser.Create;
        AddSessionObject( Request, 'QryShowNewsUser', LShowNews );
      end;

      LTemplateName := LFeed.GetTemplateName( LDM.cnxFeedFlow, LIdFeed );

      //        LDM.qryFeeds.close;
      //        LDM.qryFeeds.ParamByName( 'FEED_ID' ).AsInteger := LIdFeed;
      //        LDM.qryFeeds.Open;

      logger.Info( 'LIdFeed -> ' + LIdFeed.ToString );
      //        logger.Info( 'qry eof : ' + if ( LDM.qryFeeds.Eof ) then
      //            'Oui'
      //          else
      //            'Non' );
      logger.Info( 'Template -> ' + TPath.Combine( FWebStencilsEngine.RootDirectory, LTemplateName ) );

      if FileExists( TPath.Combine( FWebStencilsEngine.RootDirectory, LTemplateName ) ) then
      begin
        Logger.Info( 'ShowNews, LIdFeed : ' + LIdFeed.ToString );

        //          LDM.QryShowNewsUser.ParamByName( 'FEED_ID' ).AsInteger := LIdFeed;
        //          LDM.QryShowNewsUser.ParamByName( 'COUNTRY_CODE' ).AsString := FWebmodule.Token.Country;
        //          LDM.QryShowNewsUser.ParamByName( 'LANGUAGE_CODE' ).AsString := FWebmodule.Token.Lang;
        //          LDM.QryShowNewsUser.ParamByName( 'CATEGORY_ID' ).AsInteger := FWebmodule.Token.Category.ToInteger;
        //          LDM.QryShowNewsUser.ParamByName( 'SUBCATEGORY_ID' ).AsInteger := FWebmodule.Token.SubCatgegory.ToInteger;
        //          LDM.QryShowNewsUser.Open;

        FWebStencilsProcessor.AddVar(
          'News',
          LShowNews.GetNews( LDM.cnxFeedFlow, LIdFeed, FWebmodule.Token.Category.ToInteger,
            FWebmodule.Token.SubCatgegory.ToInteger,
          FWebmodule.Token.Country, FWebmodule.Token.Lang ),
          False );

        Response.ContentType := 'text/html; charset=UTF-8';
        Response.Content := RenderTemplate( LTemplateName, Request );

        //          LDM.QryShowNewsUser.Close;
      end
      else
      begin
        response.Content := 'Erreur : Template non trouvé ' + LTemplateName;
      end;

      //        LDM.qryFeeds.close;
    finally
      LDM.Critical.Release;
    end;
  end;
end;

procedure TUserFeedsController.Home( Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean );
begin
  Response.Content := RenderTemplate( TMP_HOME, Request );
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

  TControllersRegistry.GetControllersList.AddClass( TUserFeedsController );

end.

