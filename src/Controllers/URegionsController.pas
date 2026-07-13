unit URegionsController;

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
  TRegionsController = class( TBaseController )
  private
    FTitre: string;

    function GetSelectedRegionCode( Request: TWebRequest ): string;

    procedure Regions( Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
    procedure SelectRegion( Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
    procedure DesaffecterCountry( Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
    procedure AffecterCountry( Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
    procedure AddRegion( Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
    procedure AddCountry( Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
    procedure DeleteRegion( Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
    procedure DeleteCountry( Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
    procedure SetTitre( const Value: string );
  public
    /// <summary>
    ///   Initialise les routes exposées par le controller
    /// </summary>
    procedure InitializeActions( aWebModule: TWebModule; aWebStencil: TWebStencilsEngine ); override;

    property Titre: string read FTitre write SetTitre;
  end;

implementation

uses
  System.SyncObjs,
  System.IOUtils,
  System.Generics.Collections,
  System.StrUtils,
  System.NetEncoding,
  Web.ReqMulti,
  IdHTTP,
  Web.ReqFiles,
  FireDAC.Stan.Param,
  utils.ClassHelpers,
  UConsts,
  UWMMain,
  Utils.Logger,
  UControllersRegistry,
  URegions;

const
  TMP_REGIONS: string = 'regions.html';
  TMP_AFFECTED_COUNTRIES: string = 'partials/col_regions_affectees_items.html';
  TMP_PORTAIL_OOB: string = 'partials/col_regions_portail_oob.html';
  TMP_NOT_AFFECTED_COUNTRY: string = 'partials/col_regions_non_affectees_items.html';
  TMP_COL_AFFECTE_OOB: string = 'partials/col_regions_affectees_oob.html';
  TMP_COL_NON_AFFECTE_OOB: string = 'partials/col_regions_non_affectees_oob.html';
  TMP_COL_PORTAIL_ITEM: string = 'partials/col_regions_portail_items.html';

  { TRegionsController }

procedure TRegionsController.AddRegion( Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean );
begin
  var LDM := GetDMSession( Request );

  var LCode := Request.ContentFields.Values[ 'code' ];
  if ( LCode.Trim <> '' ) then
  begin
    if Assigned( LDM ) then
    begin
      var LRegions := TRegions.Create;
      try
        var LRegion := LRegions.GetRegion( LDM.cnxFeedFlow, LCode );
        if not ( Assigned( LRegion ) ) then
        begin
          LRegions.AddRegion( LDM.cnxFeedFlow, LCode );

          FWebStencilsProcessor.AddVar( 'Regions', LRegions.GetRegions( LDM.cnxFeedFlow ), True );
          FWebStencilsProcessor.AddVar( 'SelectedRegion', LRegions.GetRegion( LDM.cnxFeedFlow, LCode ), True );
          FWebStencilsProcessor.AddVar( 'LangForm', Self, False );

          Response.StatusCode := 200;
          Response.Content := RenderTemplate( TMP_COL_PORTAIL_ITEM, Request );
        end
        else
        begin
          FreeAndNil( LRegion );

          ShowToastError( Response, 'Ce code région existe déjà' );
        end;
      finally
        FreeAndNil( LRegions );
      end;
    end;
  end
  else
  begin
    // Message code région non renseigné
    ShowToastError( Response, 'Il faut renseigner le code région' );
  end;

  Handled := True;
end;

procedure TRegionsController.AddCountry( Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
begin
  var LDM := GetDMSession( Request );

  var LCode := Request.ContentFields.Values[ 'code' ];
  if ( LCode.Trim <> '' ) then
  begin
    if Assigned( LDM ) then
    begin
      var LRegions := TRegions.Create;
      try
        var LRegion := LRegions.GetCountry( LDM.cnxFeedFlow, LCode );
        if not ( Assigned( LRegion ) ) then
        begin
          LRegions.AddCountry( LDM.cnxFeedFlow, LCode, Request.ContentFields.Values[ 'libelle' ] );

          FWebStencilsProcessor.AddVar( 'NonAffectees', LRegions.GetCountriesNotAffected( LDM.cnxFeedFlow ), True );
          FWebStencilsProcessor.AddVar( 'LangForm', Self, False );

          Response.StatusCode := 200;
          Response.Content := RenderTemplate( TMP_NOT_AFFECTED_COUNTRY, Request );
        end
        else
        begin
          FreeAndNil( LRegion );

          ShowToastError( Response, 'Ce code langue existe déjà' );
        end;
      finally
        FreeAndNil( LRegions );
      end;
    end;
  end
  else
  begin
    // Message code langue non renseigné
  end;

  Handled := True;
end;

procedure TRegionsController.AffecterCountry( Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
begin
  var LDM := GetDMSession( Request );

  if Assigned( LDM ) then
  begin
    var LCode := Request.Session.DataVars.Values[ 'SelectedRegionCode' ];
    var LUserCode := Request.ContentFields.Values[ 'country' ];
    var LRegions := TRegions.Create;
    try
      LRegions.AffecterCountry( LDM.cnxFeedFlow, LUserCode, LCode );

      FTitre := 'Regions du portail';
      FWebStencilsProcessor.AddVar( 'NonAffectees', LRegions.GetCountriesNotAffected( LDM.cnxFeedFlow ), True );
      FWebStencilsProcessor.AddVar( 'Affectees', LRegions.GetCountriesAffected( LDM.cnxFeedFlow, LCode ), True );
      FWebStencilsProcessor.AddVar( 'SelectedRegion', LRegions.GetRegion( LDM.cnxFeedFlow, LCode ), True );
      FWebStencilsProcessor.AddVar( 'LangForm', Self, False );

      Response.StatusCode := 200;
      Response.Content := RenderTemplate( TMP_AFFECTED_COUNTRIES, Request )
        + RenderTemplate( TMP_COL_NON_AFFECTE_OOB, Request );
    finally
      FreeAndNil( LRegions );
    end;

    Handled := True;
  end;
end;

procedure TRegionsController.DeleteRegion( Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
begin
  var LDM := GetDMSession( Request );

  if Assigned( LDM ) then
  begin
    var LCode := Request.ContentFields.Values[ 'code' ];
    var LRegions := TRegions.Create;
    try
      LRegions.DeleteRegion( LDM.cnxFeedFlow, LCode );

      FWebStencilsProcessor.AddVar( 'NonAffectees', LRegions.GetCountriesNotAffected( LDM.cnxFeedFlow ), True );
      FWebStencilsProcessor.AddVar( 'Affectees', LRegions.GetCountriesAffected( LDM.cnxFeedFlow, LCode ), True );
      FWebStencilsProcessor.AddVar( 'Regions', LRegions.GetRegions( LDM.cnxFeedFlow ), True );
      FWebStencilsProcessor.AddVar( 'SelectedRegion', LRegions.GetRegion( LDM.cnxFeedFlow, 'EU : FR' ), True );
      FWebStencilsProcessor.AddVar( 'LangForm', Self, False );

      Response.StatusCode := 200;
      Response.Content := RenderTemplate( TMP_COL_PORTAIL_ITEM, Request )
      + RenderTemplate( TMP_COL_NON_AFFECTE_OOB, Request )
      + RenderTemplate( TMP_COL_AFFECTE_OOB, Request );
    finally
      FreeAndNil( LRegions );
    end;
    Handled := True;
  end;
end;

procedure TRegionsController.DeleteCountry( Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
begin
  var LDM := GetDMSession( Request );

  if Assigned( LDM ) then
  begin
    var LCode := Request.ContentFields.Values[ 'code' ];
    var LRegions := TRegions.Create;
    try
      LRegions.DeleteCountry( LDM.cnxFeedFlow, LCode );

      FWebStencilsProcessor.AddVar( 'NonAffectees', LRegions.GetCountriesNotAffected( LDM.cnxFeedFlow ), True );
      FWebStencilsProcessor.AddVar( 'SelectedRegion', LRegions.GetRegion( LDM.cnxFeedFlow, 'EU : FR' ), True );
      FWebStencilsProcessor.AddVar( 'LangForm', Self, False );

      Response.StatusCode := 200;
      Response.Content := RenderTemplate( TMP_NOT_AFFECTED_COUNTRY, Request );
    finally
      FreeAndNil( LRegions );
    end;
    Handled := True;
  end;
end;

procedure TRegionsController.DesaffecterCountry( Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
begin
  var LDM := GetDMSession( Request );

  if Assigned( LDM ) then
  begin
    var LCode := Request.Session.DataVars.Values[ 'SelectedRegionCode' ];
    var LCountryCode := Request.ContentFields.Values[ 'country' ];
    var LRegions := TRegions.Create;
    try
      LRegions.DesaffecterCountry( LDM.cnxFeedFlow, LCountryCode );

      FTitre := 'Regions du portail';
      FWebStencilsProcessor.AddVar( 'NonAffectees', LRegions.GetCountriesNotAffected( LDM.cnxFeedFlow ), True );
      FWebStencilsProcessor.AddVar( 'Affectees', LRegions.GetCountriesAffected( LDM.cnxFeedFlow, LCode ), True );
      FWebStencilsProcessor.AddVar( 'SelectedRegion', LRegions.GetRegion( LDM.cnxFeedFlow, LCode ), True );
      FWebStencilsProcessor.AddVar( 'LangForm', Self, False );

      Response.StatusCode := 200;
      Response.Content := RenderTemplate( TMP_NOT_AFFECTED_COUNTRY, Request )
        + RenderTemplate( TMP_COL_AFFECTE_OOB, Request );
    finally
      FreeAndNil( LRegions );
    end;

    Handled := True;
  end;
end;

function TRegionsController.GetSelectedRegionCode( Request: TWebRequest ): string;
begin
  // La région sélectionnée est mémorisée en session pour ne pas avoir à la
  // faire transiter dans chaque hx-vals des boutons affecter/désaffecter.
  Result := Request.Session.DataVars.Values[ 'SelectedRegionCode' ];
  if Result = '' then
    Result := 'EU : FR'; // région par défaut si rien n'est encore sélectionné
end;

procedure TRegionsController.InitializeActions( aWebModule: TWebModule;
  aWebStencil: TWebStencilsEngine );
begin
  inherited;

  aWebModule.AddRoutes( [
      TRoute.Create( mtGet, '/ManageRegions', Self.Regions ),
      TRoute.Create( mtGet, '/selectRegion', Self.SelectRegion ),
      TRoute.Create( mtPost, '/desaffecterRegion', Self.DesaffecterCountry ),
      TRoute.Create( mtPost, '/affecterRegion', Self.AffecterCountry ),
      TRoute.Create( mtPost, '/addRegion', Self.AddRegion ),
      TRoute.Create( mtDelete, '/DeleteRegion', Self.DeleteRegion ),
      TRoute.Create( mtPost, '/AddCountry', Self.AddCountry ),
      TRoute.Create( mtDelete, '/DeleteCountry', Self.DeleteCountry )
      ] );
end;

procedure TRegionsController.Regions( Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean );
begin
  var LDM := GetDMSession( Request );

  if Assigned( LDM ) then
  begin
    var LCode := GetSelectedRegionCode( Request );
    var LRegion := TRegions.Create;
    try
      FTitre := 'Regions du portail';
      FWebStencilsProcessor.AddVar( 'LangForm', Self, False );
      FWebStencilsProcessor.AddVar( 'Regions', LRegion.GetRegions( LDM.cnxFeedFlow ), True );
      FWebStencilsProcessor.AddVar( 'SelectedRegion', LRegion.GetRegion( LDM.cnxFeedFlow, LCode ), True );
      FWebStencilsProcessor.AddVar( 'Affectees', LRegion.GetCountriesAffected( LDM.cnxFeedFlow, LCode ), True );
      FWebStencilsProcessor.AddVar( 'NonAffectees', LRegion.GetCountriesNotAffected( LDM.cnxFeedFlow ), True );

      Response.StatusCode := 200;
      Response.Content := RenderTemplate( TMP_REGIONS, Request );
    finally
      FreeAndNil( LRegion );
    end;

    //    ShowLanguages( LDM, Request, Response );

    Handled := True;
  end;
end;

procedure TRegionsController.SelectRegion( Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean );
begin
  var LDM := GetDMSession( Request );

  if Assigned( LDM ) then
  begin
    var LCode := Request.QueryFields.Values[ 'code' ];
    Request.Session.DataVars.Values[ 'SelectedRegionCode' ] := LCode;

    var LRegions := TRegions.Create;
    try
      FTitre := 'Regions du portail';
      FWebStencilsProcessor.AddVar( 'LangForm', Self, False );
      FWebStencilsProcessor.AddVar( 'Affectees', LRegions.GetCountriesAffected( LDM.cnxFeedFlow, LCode ), True );
      FWebStencilsProcessor.AddVar( 'SelectedRegion', LRegions.GetRegion( LDM.cnxFeedFlow, LCode ), True );
      FWebStencilsProcessor.AddVar( 'Regions', LRegions.GetRegions( LDM.cnxFeedFlow ), True );

      Response.StatusCode := 200;
      Response.Content := RenderTemplate( TMP_AFFECTED_COUNTRIES, Request )
        + RenderTemplate( TMP_PORTAIL_OOB, Request );
    finally
      FreeAndNil( LRegions );
    end;
  end;
end;

procedure TRegionsController.SetTitre( const Value: string );
begin
  FTitre := Value;
end;

initialization

  TControllersRegistry.GetControllersList.AddClass( TRegionsController );
end.

