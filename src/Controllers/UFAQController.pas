unit UFAQController;

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
  TFAQController = class( TBaseController )
  private
    FLanguage: string;
    FCountry: string;
    FSubcategory: string;
    FCategory: string;
    FIsThereFavorites: Boolean;
    FActualFeed: Integer;
    FSearchActive: Boolean;
    FSearchValue: string;

    procedure SetCategory( const Value: string );
    procedure SetCountry( const Value: string );
    procedure SetLanguage( const Value: string );
    procedure SetSubcategory( const Value: string );
    procedure SetIsThereFavorites( const Value: Boolean );
    procedure SetActualFeed( const Value: Integer );
    procedure SetSearchActive( const Value: Boolean );
    procedure SetSearchValue( const Value: string );
  public
    procedure FAQHome( Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
    procedure FAQCategory( Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
    procedure FAQDetails( Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
    procedure AddVue( Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
    /// <summary>
    ///   Initialise les routes exposées par le controller
    /// </summary>
    procedure InitializeActions( aWebModule: TWebModule; aWebStencil: TWebStencilsEngine ); override;

    property Category: string read FCategory write SetCategory;
    property Subcategory: string read FSubcategory write SetSubcategory;
    property Country: string read FCountry write SetCountry;
    property Language: string read FLanguage write SetLanguage;
    property IsThereFavorites: Boolean read FIsThereFavorites write SetIsThereFavorites;
    property ActualFeed: Integer read FActualFeed write SetActualFeed;
    property SearchActive: Boolean read FSearchActive write SetSearchActive;
    property SearchValue: string read FSearchValue write SetSearchValue;
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
  UControllersRegistry,
  UFaq;

const
  /// <summary>
  ///   Nom du template HTML de la pageFAQ
  /// </summary>
  TMP_FAQ: string = 'FAQ.html';
  /// <summary>
  ///   Nom du template HTML de la partie liste des questions
  /// </summary>
  TMP_FAQ_CATEGORY: string = 'FAQCategory.html';
  /// <summary>
  ///   Nom du template HTML de la partie liste des questions
  /// </summary>
  TMP_FAQ_DETAILS: string = 'FAQDetails.html';

  { TFAQController }

procedure TFAQController.AddVue(Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean);
begin
  logger.Info( 'AddVue' );
  var LDM := GetDMSession( Request );

  if Assigned( LDM ) then
  begin

  end;
end;

procedure TFAQController.FAQCategory( Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean );
var
  LFeed: Integer;
begin
  logger.Info( 'FAQCategory' );
  var LDM := GetDMSession( Request );

  if Assigned( LDM ) then
  begin
    logger.Info( 'ContentField -> ' + Request.QueryFields.Text );
    logger.Info( 'QueryField -> ' + Request.QueryFields.Text );

    var LCategory := FWebmodule.Token.Category.ToInteger;
    var LSubcategory := FWebmodule.Token.SubCatgegory.ToInteger;
    var LCountry := FWebmodule.Token.Country;
    var LLang := FWebmodule.Token.Lang;

    if ( FWebmodule.Token.Role = 'ADMIN' ) and ( Request.QueryFields.Values[ 'admin_category' ] <> '' ) then
    begin
      LCategory := Request.QueryFields.Values[ 'admin_category' ].ToInteger;
      LSubcategory := Request.QueryFields.Values[ 'admin_subcategory' ].ToInteger;
      LCountry := Request.QueryFields.Values[ 'admin_country' ];
      LLang := Request.QueryFields.Values[ 'admin_lang' ];
    end;

    if not ( TryStrToInt( Request.QueryFields.Values[ 'category' ], LFeed ) ) then
    begin
      LFeed := 0;
    end;

    var LFAQ := TFaq( GetSessionObject( Request, 'QryFAQ' ) );
    if not ( Assigned( LFAQ ) ) then
    begin
      LFAQ := TFaq.Create;
      AddSessionObject( Request, 'QryFAQ', LFAQ );
    end;

    FWebStencilsProcessor.AddVar(
      'Question',
      LFAQ.GetQuestionsList( LDM.cnxFeedFlow, LFeed, LCategory, LSubcategory, LCountry, LLang, '' ),
      False );

    Response.ContentType := 'text/html; charset=UTF-8';
    Response.Content := RenderTemplate( TMP_FAQ_CATEGORY, Request );
    Response.StatusCode := 200;
    Handled := True;
  end;
end;

procedure TFAQController.FAQDetails( Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean );
var
  LFaqId: Integer;
begin
  logger.Info( 'FAQDetails' );
  var LDM := GetDMSession( Request );

  if Assigned( LDM ) then
  begin
    if not ( TryStrToInt( Request.QueryFields.Values[ 'Faq' ], LFaqId ) ) then
    begin
      LFaqId := 0;
    end;

    var LFAQ := TFaq( GetSessionObject( Request, 'QryFAQ' ) );
    if not ( Assigned( LFAQ ) ) then
    begin
      LFAQ := TFaq.Create;
      AddSessionObject( Request, 'QryFAQ', LFAQ );
    end;

    FWebStencilsProcessor.AddVar(
      'Question',
      LFAQ.GetFAQDetails( LDM.cnxFeedFlow, LFaqId ),
      False );

    Response.ContentType := 'text/html; charset=UTF-8';
    Response.Content := RenderTemplate( TMP_FAQ_DETAILS, Request );
    Response.StatusCode := 200;
    Handled := True;
  end;
end;

procedure TFAQController.FAQHome( Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean );
begin
  logger.Info( 'FAQHome' );
  var LDM := GetDMSession( Request );

  if Assigned( LDM ) then
  begin
    logger.Info( 'ContentField -> ' + Request.QueryFields.Text );
    logger.Info( 'QueryField -> ' + Request.QueryFields.Text );

    var LCategory := FWebmodule.Token.Category.ToInteger;
    var LSubcategory := FWebmodule.Token.SubCatgegory.ToInteger;
    var LCountry := FWebmodule.Token.Country;
    var LLang := FWebmodule.Token.Lang;
    FSearchValue := Request.QueryFields.Values[ 'search' ];

    FSearchActive := ( FSearchValue <> '' );

    if ( FWebmodule.Token.Role = 'ADMIN' ) and ( Request.QueryFields.Values[ 'admin_category' ] <> '' ) then
    begin
      LCategory := Request.QueryFields.Values[ 'admin_category' ].ToInteger;
      LSubcategory := Request.QueryFields.Values[ 'admin_subcategory' ].ToInteger;
      LCountry := Request.QueryFields.Values[ 'admin_country' ];
      LLang := Request.QueryFields.Values[ 'admin_lang' ];
    end;

    var LFAQ := TFaq( GetSessionObject( Request, 'QryFAQ' ) );
    if not ( Assigned( LFAQ ) ) then
    begin
      LFAQ := TFaq.Create;
      AddSessionObject( Request, 'QryFAQ', LFAQ );
    end;

    FWebStencilsProcessor.AddVar(
      'Favorites',
      LFAQ.GetFavorites(
      LDM.cnxFeedFlow,
      LCategory,
      LSubcategory,
      LCountry,
      LLang,
      FIsThereFavorites ),
      False );

    FWebStencilsProcessor.AddVar(
      'Group',
      LFAQ.GetFAQCategories( LDM.cnxFeedFlow, LCategory, LSubcategory, LCountry, LLang, FActualFeed ),
      False );

    FWebStencilsProcessor.AddVar(
      'Question',
      LFAQ.GetQuestionsList( LDM.cnxFeedFlow, FActualFeed, LCategory, LSubcategory, LCountry, LLang, FSearchValue ),
      False );

    FWebStencilsProcessor.AddVar(
      'Form',
      Self,
      False );

    Response.ContentType := 'text/html; charset=UTF-8';
    Response.Content := RenderTemplate( TMP_FAQ, Request );
    Response.StatusCode := 200;
    Handled := True;
  end;
end;

procedure TFAQController.InitializeActions( aWebModule: TWebModule;
  aWebStencil: TWebStencilsEngine );
begin
  inherited;

  aWebModule.AddRoutes( [
      TRoute.Create( mtGet, '/FAQ', Self.FAQHome ),
      TRoute.Create( mtGet, '/FAQCategory', Self.FAQCategory ),
      TRoute.Create( mtGet, '/FAQDetails', Self.FAQDetails ),
      TRoute.Create( mtGet, '/AddVue', Self.AddVue )
      ] );
end;

procedure TFAQController.SetActualFeed( const Value: Integer );
begin
  FActualFeed := Value;
end;

procedure TFAQController.SetCategory( const Value: string );
begin
  FCategory := Value;
end;

procedure TFAQController.SetCountry( const Value: string );
begin
  FCountry := Value;
end;

procedure TFAQController.SetIsThereFavorites( const Value: Boolean );
begin
  FIsThereFavorites := Value;
end;

procedure TFAQController.SetLanguage( const Value: string );
begin
  FLanguage := Value;
end;

procedure TFAQController.SetSearchActive( const Value: Boolean );
begin
  FSearchActive := Value;
end;

procedure TFAQController.SetSearchValue( const Value: string );
begin
  FSearchValue := Value;
end;

procedure TFAQController.SetSubcategory( const Value: string );
begin
  FSubcategory := Value;
end;

initialization

  TControllersRegistry.GetControllersList.AddClass( TFAQController );

end.

