unit ULanguesController;

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
  TLanguesController = class( TBaseController )
  private
    FTitre: string;

    function GetSelectedLangueCode( Request: TWebRequest ): string;

    procedure Langues( Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
    procedure SelectLangue( Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
    procedure DesaffecterLangue( Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
    procedure AffecterLangue( Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
    procedure AddLangue( Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
    procedure AddLangueUtilisateur( Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
    procedure DeleteLanguage( Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
    procedure DeleteUserLanguage( Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
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
  ULanguages;

const
  TMP_LANGUES: string = 'langues.html';
  TMP_AFFECTED_LANGUAGES: string = 'partials/col_affectees_items.html';
  TMP_PORTAIL_OOB: string = 'partials/col_portail_oob.html';
  TMP_NOT_AFFECTED_LANGUAGE: string = 'partials/col_non_affectees_items.html';
  TMP_COL_AFFECTE_OOB: string = 'partials/col_affectees_oob.html';
  TMP_COL_NON_AFFECTE_OOB: string = 'partials/col_non_affectees_oob.html';
  TMP_COL_PORTAIL_ITEM: string = 'partials/col_portail_items.html';

  { TLanguesController }

procedure TLanguesController.AddLangue( Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean );
begin
  var LDM := GetDMSession( Request );

  var LCode := Request.ContentFields.Values[ 'code' ];
  if ( LCode.Trim <> '' ) then
  begin
    if Assigned( LDM ) then
    begin
      var LLanguages := TLanguages.Create;
      try
        var LLangue := LLanguages.GetLanguage( LDM.cnxFeedFlow, LCode );
        if not ( Assigned( LLangue ) ) then
        begin
          LLanguages.AddLangue( LDM.cnxFeedFlow, LCode, Request.ContentFields.Values[ 'libelle' ] );

          FWebStencilsProcessor.AddVar( 'Langues', LLanguages.GetLanguages( LDM.cnxFeedFlow ), True );
          FWebStencilsProcessor.AddVar( 'SelectedLangue', LLanguages.GetLanguage( LDM.cnxFeedFlow, LCode ), True );
          FWebStencilsProcessor.AddVar( 'LangForm', Self, False );

          Response.StatusCode := 200;
          Response.Content := RenderTemplate( TMP_COL_PORTAIL_ITEM, Request );
        end
        else
        begin
          FreeAndNil( LLangue );

          ShowToastError( Response, 'Ce code langue existe déjà' );
        end;
      finally
        FreeAndNil( LLanguages );
      end;
    end;
  end
  else
  begin
    // Message code langue non renseigné
  end;

  Handled := True;
end;

procedure TLanguesController.AddLangueUtilisateur( Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
begin
  var LDM := GetDMSession( Request );

  var LCode := Request.ContentFields.Values[ 'code' ];
  if ( LCode.Trim <> '' ) then
  begin
    if Assigned( LDM ) then
    begin
      var LLanguages := TLanguages.Create;
      try
        var LLangue := LLanguages.GetUserLanguage( LDM.cnxFeedFlow, LCode );
        if not ( Assigned( LLangue ) ) then
        begin
          LLanguages.AddUserLanguage( LDM.cnxFeedFlow, LCode, Request.ContentFields.Values[ 'libelle' ] );

          FWebStencilsProcessor.AddVar( 'NonAffectees', LLanguages.GetUserLanguagesNotAffected( LDM.cnxFeedFlow ), True );
          FWebStencilsProcessor.AddVar( 'LangForm', Self, False );

          Response.StatusCode := 200;
          Response.Content := RenderTemplate( TMP_NOT_AFFECTED_LANGUAGE, Request );
        end
        else
        begin
          FreeAndNil( LLangue );

          ShowToastError( Response, 'Ce code langue existe déjà' );
        end;
      finally
        FreeAndNil( LLanguages );
      end;
    end;
  end
  else
  begin
    // Message code langue non renseigné
  end;

  Handled := True;
end;

procedure TLanguesController.AffecterLangue( Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
begin
  var LDM := GetDMSession( Request );

  if Assigned( LDM ) then
  begin
    var LCode := Request.Session.DataVars.Values[ 'SelectedLangueCode' ];
    var LUserCode := Request.ContentFields.Values[ 'langueUtilisateur' ];
    var LLanguage := TLanguages.Create;
    try
      LLanguage.AffecterLangue( LDM.cnxFeedFlow, LUserCode, LCode );

      FTitre := 'Langues du portail';
      FWebStencilsProcessor.AddVar( 'NonAffectees', LLanguage.GetUserLanguagesNotAffected( LDM.cnxFeedFlow ), True );
      FWebStencilsProcessor.AddVar( 'Affectees', LLanguage.GetUserLanguagesAffected( LDM.cnxFeedFlow, LCode ), True );
      FWebStencilsProcessor.AddVar( 'SelectedLangue', LLanguage.GetLanguage( LDM.cnxFeedFlow, LCode ), True );
      FWebStencilsProcessor.AddVar( 'LangForm', Self, False );

      Response.StatusCode := 200;
      Response.Content := RenderTemplate( TMP_AFFECTED_LANGUAGES, Request )
        + RenderTemplate( TMP_COL_NON_AFFECTE_OOB, Request );
    finally
      FreeAndNil( LLanguage );
    end;

    Handled := True;
  end;
end;

procedure TLanguesController.DeleteLanguage( Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
begin
  var LDM := GetDMSession( Request );

  if Assigned( LDM ) then
  begin
    var LCode := Request.ContentFields.Values[ 'code' ];
    var LLanguages := TLanguages.Create;
    try
      LLanguages.DeleteLanguage( LDM.cnxFeedFlow, LCode );

      FWebStencilsProcessor.AddVar( 'Langues', LLanguages.GetLanguages( LDM.cnxFeedFlow ), True );
      FWebStencilsProcessor.AddVar( 'NonAffectees', LLanguages.GetUserLanguagesNotAffected( LDM.cnxFeedFlow ), True );
      FWebStencilsProcessor.AddVar( 'Affectees', LLanguages.GetUserLanguagesAffected( LDM.cnxFeedFlow, LCode ), True );
      FWebStencilsProcessor.AddVar( 'SelectedLangue', LLanguages.GetLanguage( LDM.cnxFeedFlow, 'fr' ), True );
      FWebStencilsProcessor.AddVar( 'LangForm', Self, False );

      Response.StatusCode := 200;
      Response.Content := RenderTemplate( TMP_COL_PORTAIL_ITEM, Request )
      + RenderTemplate( TMP_COL_NON_AFFECTE_OOB, Request )
      + RenderTemplate( TMP_COL_AFFECTE_OOB, Request );
    finally
      FreeAndNil( LLanguages );
    end;
    Handled := True;
  end;
end;

procedure TLanguesController.DeleteUserLanguage( Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
begin
  var LDM := GetDMSession( Request );

  if Assigned( LDM ) then
  begin
    var LCode := Request.ContentFields.Values[ 'code' ];
    var LLanguages := TLanguages.Create;
    try
      LLanguages.DeleteUserLanguage( LDM.cnxFeedFlow, LCode );

      FWebStencilsProcessor.AddVar( 'NonAffectees', LLanguages.GetUserLanguagesNotAffected( LDM.cnxFeedFlow ), True );
      FWebStencilsProcessor.AddVar( 'SelectedLangue', LLanguages.GetLanguage( LDM.cnxFeedFlow, 'fr' ), True );
      FWebStencilsProcessor.AddVar( 'LangForm', Self, False );

      Response.StatusCode := 200;
      Response.Content := RenderTemplate( TMP_NOT_AFFECTED_LANGUAGE, Request );
    finally
      FreeAndNil( LLanguages );
    end;
    Handled := True;
  end;
end;

procedure TLanguesController.DesaffecterLangue( Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
begin
  var LDM := GetDMSession( Request );

  if Assigned( LDM ) then
  begin
    var LCode := Request.Session.DataVars.Values[ 'SelectedLangueCode' ];
    var LUserCode := Request.ContentFields.Values[ 'langueUtilisateur' ];
    var LLanguage := TLanguages.Create;
    try
      LLanguage.DesaffecterLangue( LDM.cnxFeedFlow, LUserCode );

      FTitre := 'Langues du portail';
      FWebStencilsProcessor.AddVar( 'NonAffectees', LLanguage.GetUserLanguagesNotAffected( LDM.cnxFeedFlow ), True );
      FWebStencilsProcessor.AddVar( 'Affectees', LLanguage.GetUserLanguagesAffected( LDM.cnxFeedFlow, LCode ), True );
      FWebStencilsProcessor.AddVar( 'SelectedLangue', LLanguage.GetLanguage( LDM.cnxFeedFlow, LCode ), True );
      FWebStencilsProcessor.AddVar( 'LangForm', Self, False );

      Response.StatusCode := 200;
      Response.Content := RenderTemplate( TMP_NOT_AFFECTED_LANGUAGE, Request )
        + RenderTemplate( TMP_COL_AFFECTE_OOB, Request );
    finally
      FreeAndNil( LLanguage );
    end;

    Handled := True;
  end;
end;

function TLanguesController.GetSelectedLangueCode( Request: TWebRequest ): string;
begin
  // La langue sélectionnée est mémorisée en session pour ne pas avoir à la
  // faire transiter dans chaque hx-vals des boutons affecter/désaffecter.
  Result := Request.Session.DataVars.Values[ 'SelectedLangueCode' ];
  if Result = '' then
    Result := 'fr'; // langue par défaut si rien n'est encore sélectionné
end;

procedure TLanguesController.InitializeActions( aWebModule: TWebModule;
  aWebStencil: TWebStencilsEngine );
begin
  inherited;

  aWebModule.AddRoutes( [
      TRoute.Create( mtGet, '/ManageLanguages', Self.Langues ),
      TRoute.Create( mtGet, '/selectLanguage', Self.SelectLangue ),
      TRoute.Create( mtPost, '/desaffecter', Self.DesaffecterLangue ),
      TRoute.Create( mtPost, '/affecter', Self.AffecterLangue ),
      TRoute.Create( mtPost, '/addLangues', Self.AddLangue ),
      TRoute.Create( mtDelete, '/deleteLanguage', Self.DeleteLanguage ),
      TRoute.Create( mtPost, '/Addlangues-utilisateur', Self.AddLangueUtilisateur ),
      TRoute.Create( mtDelete, '/deleteUserLanguage', Self.DeleteUserLanguage )
      ] );
end;

procedure TLanguesController.Langues( Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean );
begin
  var LDM := GetDMSession( Request );

  if Assigned( LDM ) then
  begin
    var LCode := GetSelectedLangueCode( Request );
    var LLanguage := TLanguages.Create;
    try
      FTitre := 'Langues du portail';
      FWebStencilsProcessor.AddVar( 'LangForm', Self, False );
      FWebStencilsProcessor.AddVar( 'Langues', LLanguage.GetLanguages( LDM.cnxFeedFlow ), True );
      FWebStencilsProcessor.AddVar( 'SelectedLangue', LLanguage.GetLanguage( LDM.cnxFeedFlow, LCode ), True );
      FWebStencilsProcessor.AddVar( 'Affectees', LLanguage.GetUserLanguagesAffected( LDM.cnxFeedFlow, LCode ), True );
      FWebStencilsProcessor.AddVar( 'NonAffectees', LLanguage.GetUserLanguagesNotAffected( LDM.cnxFeedFlow ), True );

      Response.StatusCode := 200;
      Response.Content := RenderTemplate( TMP_LANGUES, Request );
    finally
      FreeAndNil( LLanguage );
    end;

    //    ShowLanguages( LDM, Request, Response );

    Handled := True;
  end;
end;

procedure TLanguesController.SelectLangue( Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean );
begin
  var LDM := GetDMSession( Request );

  if Assigned( LDM ) then
  begin
    var LCode := Request.QueryFields.Values[ 'code' ];
    Request.Session.DataVars.Values[ 'SelectedLangueCode' ] := LCode;

    var LLanguage := TLanguages.Create;
    try
      FTitre := 'Langues du portail';
      FWebStencilsProcessor.AddVar( 'LangForm', Self, False );
      FWebStencilsProcessor.AddVar( 'Affectees', LLanguage.GetUserLanguagesAffected( LDM.cnxFeedFlow, LCode ), True );
      FWebStencilsProcessor.AddVar( 'SelectedLangue', LLanguage.GetLanguage( LDM.cnxFeedFlow, LCode ), True );
      FWebStencilsProcessor.AddVar( 'Langues', LLanguage.GetLanguages( LDM.cnxFeedFlow ), True );

      Response.StatusCode := 200;
      Response.Content := RenderTemplate( TMP_AFFECTED_LANGUAGES, Request )
        + RenderTemplate( TMP_PORTAIL_OOB, Request );
    finally
      FreeAndNil( LLanguage );
    end;
  end;
end;

procedure TLanguesController.SetTitre( const Value: string );
begin
  FTitre := Value;
end;

initialization

  TControllersRegistry.GetControllersList.AddClass( TLanguesController );

end.

