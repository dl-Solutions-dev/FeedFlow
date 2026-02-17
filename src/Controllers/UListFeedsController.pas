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
  File last update : 2026-01-25T10:14:24.000+01:00
  Signature : ace24e6b7f536420d50237dea89800a933761e6e
  ***************************************************************************
*)

/// <summary>
///   Controller pour les Feeds
/// </summary>
unit UListFeedsController;

interface

uses
  System.Classes,
  System.SysUtils,
  System.JSON,
  Web.HTTPApp,
  Web.Stencils,
  uBaseController,
  uInterfaces,
  UDMSession,
  UGroups;

type
  /// <summary>
  ///   Class controller
  /// </summary>
  TListFeedsController = class( TBaseController )
  private
    function SaisieOK( aTitre: string; aCategorie, aSousCategorie: Integer ): string;
  public
    procedure FeedsList( Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
    procedure DeleteFeeds( Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
    procedure GetNavigation( Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
    procedure FeedEditLineMode( Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
    procedure CancelFeedEditLine( Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
    procedure ApplyFeedEditLine( Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
    procedure AddFeed( Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
    procedure CancelAddFeed( Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
    procedure ApplyInsertFeed( Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
    procedure SaveContextFeed( Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
    procedure TriListeFeeds( Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );

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
  UCategories,
  USubcategories,
  UCountries,
  ULanguages,
  UControllersRegistry;

const
  /// <summary>
  ///   Nom de la variable session pour la pagination
  /// </summary>
  NAVIGATION_NAME: string = 'FeedList';
  /// <summary>
  ///   Nom de la variable de session conenant la recherche de l'utilisateur
  /// </summary>
  SEARCH_VARIABLE: string = 'FeedsList.Search';
  /// <summary>
  ///   Nombre de lignes par page
  /// </summary>
  LINEPERPAGE_VARIABLE: string = 'LinesPerPageFeed';
  /// <summary>
  ///   Nom de la variable de session pour le groupe
  /// </summary>
  GROUPID_VARIABLE: string = 'FeedsList.GroupId';
  /// <summary>
  ///   Nom du tempalte HTML pour ajouter un feed
  /// </summary>
  TMP_ADD: string = 'FeedAdd.html';
  /// <summary>
  ///   Nom du tempalte HTML de la liste des feeds
  /// </summary>
  TMP_LISTE: string = 'FeedsList.html';
  /// <summary>
  ///   Nom du tempalte HTML de la table
  /// </summary>
  TMP_TABLE: string = 'FeedsTable.html';
  /// <summary>
  ///   ActionList pour une ligne de tableau <br />
  /// </summary>
  TMP_LINE: string = 'FeedLine.html';
  /// <summary>
  ///   Nom du tempalte HTML du contenu du tableau
  /// </summary>
  TMP_LINES: string = 'FeedsLines.html';
  /// <summary>
  ///   Nom du tempalte HTML de la ligne de modification
  /// </summary>
  TMP_LINE_EDIT: string = 'FeedLineEdit.html';
  /// <summary>
  ///   Nom du tempalte HTML des listes de pages
  /// </summary>
  TMP_NAVIGATION: string = 'ListNavigation.html';

  { TListFeedsController }

procedure TListFeedsController.AddFeed( Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean );
begin
  var LDM := GetDMSession( Request );

  if Assigned( LDM ) then
  begin
    var LFeed := TFeed( GetSessionObject( Request, 'qryFeed' ) );
    if not ( Assigned( LFeed ) ) then
    begin
      LFeed := TFeed.Create;
      AddSessionObject( Request, 'qryFeed', LFeed );
    end;

    var LGroups := TGroups( GetSessionObject( Request, 'qryListGrpoups' ) );
    if not ( Assigned( LGroups ) ) then
    begin
      LGroups := TGroups.Create;
      AddSessionObject( Request, 'qryListGrpoups', LGroups );
    end;

    FWebStencilsProcessor.AddVar( 'Groups', LGroups.GetListOfGroups( LDM.cnxFeedFlow ), False );
    FWebStencilsProcessor.AddVar( 'Feed', LFeed.GetFeed( LDM.cnxFeedFlow, -1 ), False );

    Response.Content := RenderTemplate( TMP_ADD, Request );
  end
  else
  begin
    Response.Content := 'Invalid session';
  end;
end;

procedure TListFeedsController.ApplyFeedEditLine( Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
var
  LDM: TDMSession;
  LMsg: string;
  LCategorie,
    LSousCategorie,
    LGroupe: Integer;
  LFeed: TFeed;
begin
  LMsg := '';

  LDM := GetDMSession( Request );
  if Assigned( LDM ) then
  begin
    if ( Request.QueryFields.Values[ 'Id' ] <> '' ) then
    begin
      LDM.cnxFeedFlow.StartTransaction;

      LFeed := TFeed( GetSessionObject( Request, 'qryFeed' ) );
      if not ( Assigned( LFeed ) ) then
      begin
        LFeed := TFeed.Create;
        AddSessionObject( Request, 'qryFeed', LFeed );
      end;

      if not ( TryStrToInt( Request.ContentFields.Values[ 'groups' ], LGroupe ) ) then
      begin
        LGroupe := 0;
      end;
      if not ( TryStrToInt( Request.ContentFields.Values[ 'Categorie' ], LCategorie ) ) then
      begin
        LCategorie := 0;
      end;

      if not ( TryStrToInt( Request.ContentFields.Values[ 'SousCategorie' ], LSousCategorie ) ) then
      begin
        LSousCategorie := 0;
      end;

      LMsg := SaisieOK( Request.ContentFields.Values[ 'titre' ], LCategorie, LSousCategorie );

      if ( LMsg = 'OK' ) then
      begin
        LMsg := LFeed.UpdateFeed(
          LDM.cnxFeedFlow,
          StrToInt( Request.QueryFields.Values[ 'Id' ] ),
          Request.ContentFields.Values[ 'nom' ],
          Request.ContentFields.Values[ 'titre' ],
          Request.ContentFields.Values[ 'statut' ],
          LGroupe
          );

        if ( LMsg = 'OK' ) then
        begin
          FWebStencilsProcessor.AddVar(
            'Feed',
            LFeed.GetFeed( LDM.cnxFeedFlow, StrToInt( Request.QueryFields.Values[ 'Id' ] ) ),
            False
            );
          FWebStencilsProcessor.AddVar( 'Form', Self, False );

          Response.Content := RenderTemplate( TMP_LINE, Request );
        end
        else
        begin
          Response.Content := LMsg;
        end;
      end
      else
      begin
        Response.Content := LMsg;
      end;
    end;
  end;

  Handled := True;
end;

procedure TListFeedsController.ApplyInsertFeed( Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
var
  LDM: TDMSession;
  LLastId,
    LGroupe: Integer;
  LMsg: string;
  LCategorie, LSousCategorie: Integer;
  LFeed: TFeed;
begin
  LDM := GetDMSession( Request );

  if Assigned( LDM ) then
  begin
    if not ( TryStrToInt( Request.ContentFields.Values[ 'Categorie' ], LCategorie ) ) then
    begin
      LCategorie := 0;
    end;

    if not ( TryStrToInt( Request.ContentFields.Values[ 'SousCategorie' ], LSousCategorie ) ) then
    begin
      LSousCategorie := 0;
    end;

    LMsg := SaisieOK( Request.ContentFields.Values[ 'titre' ], LCategorie, LSousCategorie );

    if ( LMsg = 'OK' ) then
    begin
      LFeed := TFeed( GetSessionObject( Request, 'qryFeed' ) );
      if not ( Assigned( LFeed ) ) then
      begin
        LFeed := TFeed.Create;
        AddSessionObject( Request, 'qryFeed', LFeed );
      end;

      if not ( TryStrToInt( Request.ContentFields.Values[ 'groups' ], LGroupe ) ) then
      begin
        LGroupe := 0;
      end;

      LFeed.FeedName := Request.ContentFields.Values[ 'nom' ];
      LFeed.Title := Request.ContentFields.Values[ 'titre' ];
      LFeed.Status := Request.ContentFields.Values[ 'status' ];
      LFeed.DisplayTemplate := Request.ContentFields.Values[ 'template' ];
      LFeed.FeedGroup := LGroupe;

      LLastId := LFeed.CreateNewFeed( LDM.cnxFeedFlow, LMsg );

      if ( LLastId <> -1 ) then
      begin
        FWebStencilsProcessor.AddVar( 'Feed', LFeed.GetFeed( LDM.cnxFeedFlow, LLastId ), False );

        Response.Content := RenderTemplate( TMP_LINE, Request );
      end
      else
      begin
        Response.Content := LMsg;
      end;

      //        LDM.qryFeeds.Open;
      //        LDM.qryFeeds.Append;
      //        LDM.qryFeedsFEED_ID.Value := -1;
      //        LDM.qryFeedsFEED_NAME.Value := Request.ContentFields.Values[ 'nom' ];
      //        LDM.qryFeedsTITLE.Value := Request.ContentFields.Values[ 'titre' ];
      //        LDM.qryFeedsSTATUS.Value := Request.ContentFields.Values[ 'status' ];
      //        LDM.qryFeedsDISPLAY_TEMPLATE.Value := Request.ContentFields.Values[ 'template' ];
      //        if not ( TryStrToInt( Request.ContentFields.Values[ 'groupe' ], LGroupe ) ) then
      //        begin
      //          LGroupe := 0;
      //        end;
      //        LDM.qryFeedsFEED_GROUP.Value := LGroupe;
      //
      //        LDM.qryFeeds.Post;
      //
      //        LLAstId := LDM.cnxFeedFlow.GetLastAutoGenValue( 'GEN_FEED' );
      //
      //        LDM.qryFeeds.Close;
      //        LDM.qryFeeds.ParamByName( 'FEED_ID' ).AsInteger := LLAstId;
      //        LDM.qryFeeds.Open;
      //
      //        FWebStencilsProcessor.AddVar( 'Feed', LDM.qryFeeds, False );
      //
      //        Response.Content := RenderTemplate( TMP_LINE, Request );
    end
    else
    begin
      Response.Content := LMsg;
    end;
  end;

  Handled := True;
end;

procedure TListFeedsController.CancelAddFeed( Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
begin
  SendEmptyContent( Response );
end;

procedure TListFeedsController.CancelFeedEditLine( Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
var
  LDM: TDMSession;
  LFeed: TFeed;
begin
  LDM := GetDMSession( Request );
  if Assigned( LDM ) then
  begin
    LDM.Critical.Acquire;
    try
      LFeed := TFeed( GetSessionObject( Request, 'qryFeed' ) );
      if not ( Assigned( LFeed ) ) then
      begin
        LFeed := TFeed.Create;
        AddSessionObject( Request, 'qryFeed', LFeed );
      end;

      FWebStencilsProcessor.AddVar( 'Feed', LFeed.GetFeed( LDM.cnxFeedFlow, StrToInt( Request.QueryFields.Values[ 'Id' ] ) ),
        False );
      FWebStencilsProcessor.AddVar( 'Form', Self, False );

      Response.Content := RenderTemplate( TMP_LINE, Request );
      //
      //        LDM.qryFeeds.close;
      //        LDM.qryFeeds.ParamByName( 'FEED_ID' ).AsString := Request.QueryFields.Values[ 'Id' ];
      //        LDM.qryFeeds.Open;
      //
      //        if not ( LDM.qryFeeds.Eof ) then
      //        begin
      //          FWebStencilsProcessor.AddVar( 'Feed', LDM.qryFeeds, False );
      //          FWebStencilsProcessor.AddVar( 'Form', Self, False );
      //
      //          Response.Content := RenderTemplate( TMP_LINE, Request );
      //        end;
    finally
      LDM.Critical.Leave;
    end;
  end;
end;

procedure TListFeedsController.DeleteFeeds( Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
var
  LDM: TDMSession;
  LFeed: TFeed;
  LMsg: string;
begin
  LDM := GetDMSession( Request );
  if Assigned( LDM ) then
  begin
    LFeed := TFeed( GetSessionObject( Request, 'qryFeed' ) );
    if not ( Assigned( LFeed ) ) then
    begin
      LFeed := TFeed.Create;
      AddSessionObject( Request, 'qryFeed', LFeed );
    end;

    LMsg := LFeed.DeleteFeed( LDM.cnxFeedFlow, StrToInt( Request.QueryFields.Values[ 'Id' ] ) );

    if ( LMsg = 'OK' ) then
    begin
      SendEmptyContent( Response );
    end
    else
    begin
      Response.Content := 'ERR:' + LMsg;
    end;
  end;
end;

procedure TListFeedsController.FeedEditLineMode( Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
var
  LDM: TDMSession;
  //  LFeed: TFeed;
  //  LCategories: TCategories;
  //  LSubcategories: TSubcategories;
  //  LCountries: TCountries;
  //  LLanguages: TLanguages;
begin
  LDM := GetDMSession( Request );

  if Assigned( LDM ) then
  begin
    LDM.Critical.Acquire;
    try
      var LFeed := TFeed( GetSessionObject( Request, 'qryFeed' ) );
      if not ( Assigned( LFeed ) ) then
      begin
        LFeed := TFeed.Create;
        AddSessionObject( Request, 'qryFeed', LFeed );
      end;

      var LCategories := TCategories( GetSessionObject( Request, 'qryListCategories' ) );
      if not ( Assigned( LCategories ) ) then
      begin
        LCategories := TCategories.Create;
        AddSessionObject( Request, 'qryListCategories', LCategories );
      end;

      var LSubcategories := TSubcategories( GetSessionObject( Request, 'qryListSubcategories' ) );
      if not ( Assigned( LSubcategories ) ) then
      begin
        LSubcategories := TSubcategories.Create;
        AddSessionObject( Request, 'qryListSubcategories', LSubcategories );
      end;

      var LCountries := TCountries( GetSessionObject( Request, 'qryListCountries' ) );
      if not ( Assigned( LCountries ) ) then
      begin
        LCountries := TCountries.Create;
        AddSessionObject( Request, 'qryListCountries', LCountries );
      end;

      var LLanguages := TLanguages( GetSessionObject( Request, 'qryListLanguages' ) );
      if not ( Assigned( LLanguages ) ) then
      begin
        LLanguages := TLanguages.Create;
        AddSessionObject( Request, 'qryListLanguages', LLanguages );
      end;

      var LGroups := TGroups( GetSessionObject( Request, 'qryListGrpoups' ) );
      if not ( Assigned( LGroups ) ) then
      begin
        LGroups := TGroups.Create;
        AddSessionObject( Request, 'qryListGrpoups', LGroups );
      end;

      FWebStencilsProcessor.AddVar( 'Feed', LFeed.GetFeed( LDM.cnxFeedFlow, StrToInt( Request.QueryFields.Values[ 'Id' ] ) ),
        False );
      FWebStencilsProcessor.AddVar( 'Categories', LCategories.GetListOfCategories( LDM.cnxFeedFlow ), False );
      FWebStencilsProcessor.AddVar( 'SousCategories', LSubcategories.GetListOfSubcategories( LDM.cnxFeedFlow ), False );
      FWebStencilsProcessor.AddVar( 'Pays', LCountries.GetListOfCountries( LDM.cnxFeedFlow ), False );
      FWebStencilsProcessor.AddVar( 'Langues', LLanguages.GetListOfLanguages( LDM.cnxFeedFlow ), False );
      FWebStencilsProcessor.AddVar( 'Groups', LGroups.GetListOfGroups( LDM.cnxFeedFlow ), False );
      FWebStencilsProcessor.AddVar( 'Form', Self, False );

      Response.Content := RenderTemplate( TMP_LINE_EDIT, Request );
    finally
      LDM.Critical.Leave;
    end;
  end;
end;

procedure TListFeedsController.FeedsList( Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean );
var
  LLinesPerPage: Integer;
  LPage: Integer;
  LInt: Integer;
  LTemplate: string;
begin
  var LDM := GetDMSession( Request );

  if Assigned( LDM ) then
  begin
    LDM.cnxFeedFlow.Rollback;

    var LFeeds := TFeeds( GetSessionObject( Request, 'qryListeFeeds' ) );
    if not ( Assigned( LFeeds ) ) then
    begin
      LFeeds := TFeeds.Create;
      AddSessionObject( Request, 'qryListeFeeds', LFeeds );
    end;

    var LCategories := TCategories( GetSessionObject( Request, 'qryListCategories' ) );
    if not ( Assigned( LCategories ) ) then
    begin
      LCategories := TCategories.Create;
      AddSessionObject( Request, 'qryListCategories', LCategories );
    end;

    var LSubcategories := TSubcategories( GetSessionObject( Request, 'qryListSubcategories' ) );
    if not ( Assigned( LSubcategories ) ) then
    begin
      LSubcategories := TSubcategories.Create;
      AddSessionObject( Request, 'qryListSubcategories', LSubcategories );
    end;

    var LCountries := TCountries( GetSessionObject( Request, 'qryListCountries' ) );
    if not ( Assigned( LCountries ) ) then
    begin
      LCountries := TCountries.Create;
      AddSessionObject( Request, 'qryListCountries', LCountries );
    end;

    var LLanguages := TLanguages( GetSessionObject( Request, 'qryListLanguages' ) );
    if not ( Assigned( LLanguages ) ) then
    begin
      LLanguages := TLanguages.Create;
      AddSessionObject( Request, 'qryListLanguages', LLanguages );
    end;

    var LGroups := TGroups( GetSessionObject( Request, 'qryListGroups' ) );
    if not ( Assigned( LGroups ) ) then
    begin
      LGroups := TGroups.Create;
      AddSessionObject( Request, 'qryListGroups', LGroups );
    end;

    if not ( TryStrToInt( LDM.SessionVariables.Values[ LINEPERPAGE_VARIABLE ], LLinesPerPage ) ) then
    begin
      LLinesPerPage := 10;
    end;

    var LPagination := TPagination.Create; // LDM.Pagination( NAVIGATION_NAME );

    if not ( TryStrToInt( LDM.SessionVariables.Values[ NAVIGATION_NAME ], LPage ) ) then
    begin
      LPage := 0;
    end;

    if ( LPage > 0 ) then
    begin
      Dec( LPage );
    end;

    LDM.Critical.Acquire;
    try
      // Est-ce qu'on rafraichit également la barre de pagination
      if ( Request.QueryFields.Values[ 'Scope' ] = 'Page' ) then
      begin
        FTitre := 'Fils d''informations';

        if LDM.SessionVariables.Values[ 'SortFeedsField' ] = '' then
        begin
          LDM.SessionVariables.Values[ 'SortFeedsField' ] := 'CREATION_DATE';
          LDM.SessionVariables.Values[ 'SortFeedsOrd' ] := 'desc';
        end;

          LTemplate := TMP_LISTE;

        // Si on ne garde pas le contexte on initialise les variables de session
        if ( Request.QueryFields.Values[ 'KeepContext' ] <> 'O' ) then
        begin
          LDM.SessionVariables.Values[ SEARCH_VARIABLE ] := '';

          if not ( TryStrToInt( Request.QueryFields.Values[ 'Actual' ], LInt ) ) then
          begin
            LInt := 1;
          end;

          LDM.SessionVariables.Values[ NAVIGATION_NAME ] := LInt.ToString;

          if not ( TryStrToInt( Request.ContentFields.Values[ 'Groups' ], LInt ) ) then
          begin
            LInt := 0;
          end;

          LDM.SessionVariables.Values[ GROUPID_VARIABLE ] := Lint.ToString;
        end;

        LPagination.GroupId := LDM.SessionVariables.Values[ GROUPID_VARIABLE ].ToInteger;

        LPagination.GeneratePagesList(
          LFeeds.GetFeedsCount(
          LDM.cnxFeedFlow,
          LPagination.GroupId, //TODO: Voir pour contextualiser
          '%' + LDM.SessionVariables.Values[ SEARCH_VARIABLE ] + '%'
          ),
          LLinesPerPage,
          LInt,
          '',
          '',
          'FeedsList',
          'GetFeedNavigation',
          True
          );
        //          FWebStencilsProcessor.AddVar( 'pages', LDM.Pagination( NAVIGATION_NAME ), False );
        FWebStencilsProcessor.AddVar( 'pages', LPagination, False );
      end
      else // Sinon, on rafraichit juste la liste
      begin
        LTemplate := TMP_TABLE;
        LPagination.GroupId := LDM.SessionVariables.Values[ GROUPID_VARIABLE ].ToInteger;
      end;

      FMsg := FMsg + 'FeedsList';

      FWebStencilsProcessor.AddVar( 'Categories', LCategories.GetListOfCategories( LDM.cnxFeedFlow ), False );
      FWebStencilsProcessor.AddVar( 'SousCategories', LSubcategories.GetListOfSubcategories( LDM.cnxFeedFlow ), False );
      FWebStencilsProcessor.AddVar( 'Pays', LCountries.GetListOfCountries( LDM.cnxFeedFlow ), False );
      FWebStencilsProcessor.AddVar( 'Langues', LLanguages.GetListOfLanguages( LDM.cnxFeedFlow ), False );
      FWebStencilsProcessor.AddVar( 'Groups', LGroups.GetListOfGroups( LDM.cnxFeedFlow ), False );
      FWebStencilsProcessor.AddVar(
        'feedsList',
        LFeeds.GetListeFeeds(
        LDM.cnxFeedFlow,
        LLinesPerPage,
        LPage * LLinesPerPage,
        LPagination.GroupId, //TODO: Voir pour contextualiser
        '%' + LDM.SessionVariables.Values[ SEARCH_VARIABLE ] + '%',
        LDM.SessionVariables.Values[ 'SortFeedsField' ],
        LDM.SessionVariables.Values[ 'SortFeedsOrd' ]
        ),
        False
        );
      FWebStencilsProcessor.AddVar( 'Form', Self, False );

      Response.StatusCode := 200;
      Response.Content := RenderTemplate( LTemplate, Request );

      FreeAndNil( LPagination );
    finally
      LDM.Critical.Release;
    end;
  end;
end;

procedure TListFeedsController.GetNavigation( Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
var
  //  LSession: TUserSession;
  LPagination: TPagination;
  LInt, LGroupId: Integer;
  LLinesPerPage: Integer;
  LDM: TDMSession;
  LFeeds: TFeeds;
  LTitle: string;
begin
  LDM := GetDMSession( Request );

  if Assigned( LDM ) then
  begin
    if not ( TryStrToInt( Request.ContentFields.Values[ 'LinesPerPage' ], LLinesPerPage ) ) then
    begin
      LLinesPerPage := 10;
    end;

    LDM.SessionVariables.Values[ LINEPERPAGE_VARIABLE ] := LLinesPerPage.ToString;

    if not ( TryStrToInt( Request.ContentFields.Values[ 'Groups' ], LGroupId ) ) then
    begin
      LGroupId := 0;
    end;

    LDM.SessionVariables.Values[ GROUPID_VARIABLE ] := LGroupId.ToString;

    if ( Request.QueryFields.Values[ 'SearchChanged' ] <> '' ) then
    begin
      LDM.SessionVariables.Values[ SEARCH_VARIABLE ] := Request.ContentFields.Values[ 'Search' ].ToUpper;
      LInt := 1;
    end
    else
    begin
      if not ( TryStrToInt( Request.QueryFields.Values[ 'Page' ], LInt ) ) then
      begin
        LInt := 1;
      end;
    end;

    LDM.Critical.Acquire;
    try
      LFeeds := TFeeds( GetSessionObject( Request, 'qryListeFeeds' ) );
      if not ( Assigned( LFeeds ) ) then
      begin
        LFeeds := TFeeds.Create;
        AddSessionObject( Request, 'qryListeFeeds', LFeeds );
      end;

      LTitle := '%' + LDM.SessionVariables.Values[ SEARCH_VARIABLE ] + '%';

      //        LDM.qryCountFeeds.close;
      //        LDM.qryCountFeeds.ParamByName( 'TITLE' ).AsString := '%' + LDM.SessionVariables.Values[ SEARCH_VARIABLE ] + '%';
      //        LDM.qryCountFeeds.Open;

      FMsg := 'GetPagination';

      LDM.SessionVariables.Values[ NAVIGATION_NAME ] := LInt.ToString;

      LPagination := TPagination.Create; // LDM.Pagination( NAVIGATION_NAME );
      try
        //        if not ( TryStrToInt( Request.ContentFields.Values[ 'Groups' ], LInt ) ) then
        //        begin
        //          LInt := 0;
        //        end;

        LPagination.GroupId := LGroupId;

        LPagination.GeneratePagesList( LFeeds.GetFeedsCount( LDM.cnxFeedFlow, LPagination.GroupId, LTitle ),
          //TODO: Voir pour contextualiser
          LLinesPerPage, LInt, '', Request.ContentFields.Values[
          'Search' ], 'FeedsList', 'GetFeedNavigation', True );

        FWebStencilsProcessor.AddVar( 'pages', LPagination, False );
        FWebStencilsProcessor.AddVar( 'Form', Self, False );

        Response.Content := RenderTemplate( TMP_NAVIGATION, Request );
      finally
        FreeAndNil( LPagination );
      end;
    finally
      LDM.Critical.Release;
    end;
  end;

  Handled := True;
end;

procedure TListFeedsController.InitializeActions( aWebModule: TWebModule;
  aWebStencil: TWebStencilsEngine );
begin
  inherited;

  aWebModule.AddRoutes( [
      TRoute.Create( mtGet, '/FeedsList', Self.FeedsList ),
      TRoute.Create( mtDelete, '/DeleteFeed', Self.DeleteFeeds ),
      TRoute.Create( mtPost, '/GetFeedNavigation', Self.GetNavigation ),
      TRoute.Create( mtPost, '/FeedEditLineMode', Self.FeedEditLineMode ),
      TRoute.Create( mtAny, '/CancelFeedEditLine', Self.CancelFeedEditLine ),
      TRoute.Create( mtPost, '/ApplyFeedEditLine', Self.ApplyFeedEditLine ),
      TRoute.Create( mtPost, '/AddFeed', Self.AddFeed ),
      TRoute.Create( mtPost, '/CancelAddFeed', Self.CancelAddFeed ),
      TRoute.Create( mtPost, '/ApplyInsertFeed', Self.ApplyInsertFeed ),
      //      TRoute.Create( mtAny, '/GetFeed', Self.GetFeed ),
      TRoute.Create( mtAny, '/SaveContext', Self.SaveContextFeed ),
      TRoute.Create( mtGet, '/TriFeeds', Self.TriListeFeeds )
      ] );
end;

function TListFeedsController.SaisieOK( aTitre: string; aCategorie,
  aSousCategorie: Integer ): string;
begin
  Result := 'OK';

  if ( aTitre.Trim = '' ) then
  begin
    Result := 'ERR:Il faut renseigner un titre';
  end;
end;

procedure TListFeedsController.SaveContextFeed( Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
var
  LDM: TDMSession;
  LJsonObj: TJSONObject;
  LAllContext: string;
  LArrayCategorie,
    LArraySousCategorie,
    LArrayPays,
    LArrayLangue: TJSONArray;
  LFeed: TFeed;
  LCategories: TFeedCategory;
  LSubcategories: TFeedSubCategory;
  LCountries: TFeedCountry;
  LLanguages: TFeedLanguage;
begin
  LDM := GetDMSession( Request );

  if Assigned( LDM ) then
  begin
    LDM.Critical.Acquire;
    try
      LDM.cnxFeedFlow.StartTransaction;
      try
        LFeed := TFeed( GetSessionObject( Request, 'qryFeed' ) );
        if not ( Assigned( LFeed ) ) then
        begin
          LFeed := TFeed.Create;
          AddSessionObject( Request, 'qryFeed', LFeed );
        end;

        LCategories := TFeedCategory( GetSessionObject( Request, 'QryFeedCategories' ) );
        if not ( Assigned( LCategories ) ) then
        begin
          LCategories := TFeedCategory.Create;
          AddSessionObject( Request, 'QryFeedCategories', LCategories );
        end;

        LSubcategories := TFeedSubCategory( GetSessionObject( Request, 'QryFeedSubCategories' ) );
        if not ( Assigned( LSubcategories ) ) then
        begin
          LSubcategories := TFeedSubCategory.Create;
          AddSessionObject( Request, 'QryFeedSubCategories', LSubcategories );
        end;

        LCountries := TFeedCountry( GetSessionObject( Request, 'QryFeedCountry' ) );
        if not ( Assigned( LCountries ) ) then
        begin
          LCountries := TFeedCountry.Create;
          AddSessionObject( Request, 'QryFeedCountry', LCountries );
        end;

        LLanguages := TFeedLanguage( GetSessionObject( Request, 'QryFeedLanguage' ) );
        if not ( Assigned( LLanguages ) ) then
        begin
          LLanguages := TFeedLanguage.Create;
          AddSessionObject( Request, 'QryFeedLanguage', LLanguages );
        end;

        LJsonObj := TJSONObject.ParseJSONValue( Request.Content ) as TJSONObject;
        try
          LAllContext := LJsonObj.GetValue<string>( 'AllContext' );

          LArrayCategorie := LJsonObj.GetValue<TJSONArray>( 'Category' );
          LArraySousCategorie := LJsonObj.GetValue<TJSONArray>( 'Subcategory' );
          LArrayPays := LJsonObj.GetValue<TJSONArray>( 'Country' );
          LArrayLangue := LJsonObj.GetValue<TJSONArray>( 'Lang' );

          LFeed.ChangeContext( LDM.cnxFeedFlow, StrToInt( Request.QueryFields.Values[ 'IdFeed' ] ), LAllContext );

          // On supprime les anciens liens
          LCategories.DeleteCategories( LDM.cnxFeedFlow, StrToInt( Request.QueryFields.Values[ 'IdFeed' ] ) );
          LSubcategories.DeleteSubcategories( LDM.cnxFeedFlow, StrToInt( Request.QueryFields.Values[ 'IdFeed' ] ) );
          LCountries.DeleteCountries( LDM.cnxFeedFlow, StrToInt( Request.QueryFields.Values[ 'IdFeed' ] ) );
          LLanguages.DeleteLanguages( LDM.cnxFeedFlow, StrToInt( Request.QueryFields.Values[ 'IdFeed' ] ) );

          if ( LAllContext = 'N' ) then
          begin
            // on Sauvegarde le lien avec les catégories
            for var i := 0 to LArrayCategorie.Count - 1 do
            begin
              LCategories.AddCategory(
                LDM.cnxFeedFlow,
                StrToInt( LArrayCategorie.Items[ i ].Value ),
                StrToInt( Request.QueryFields.Values[ 'IdFeed' ] )
                );
            end;

            // on Sauvegarde le lien avec les sous-catégories
            for var i := 0 to LArraySousCategorie.Count - 1 do
            begin
              LSubcategories.AddSubcategory(
                LDM.cnxFeedFlow,
                StrToInt( LArraySousCategorie.Items[ i ].Value ),
                StrToInt( Request.QueryFields.Values[ 'IdFeed' ] )
                );
            end;

            // on Sauvegarde le lien avec les pays
            for var i := 0 to LArrayPays.Count - 1 do
            begin
              LCountries.AddCountry(
                LDM.cnxFeedFlow,
                LArrayPays.Items[ i ].Value,
                StrToInt( Request.QueryFields.Values[ 'IdFeed' ] )
                );
            end;

            // on Sauvegarde le lien avec les langues
            for var i := 0 to LArrayLangue.Count - 1 do
            begin
              LLanguages.AddLanguage(
                LDM.cnxFeedFlow,
                LArrayLangue.Items[ i ].Value,
                StrToInt( Request.QueryFields.Values[ 'IdFeed' ] )
                );
            end;
          end;
        finally
          FreeAndNil( LJsonObj );
        end;

        LDM.cnxFeedFlow.Commit;
      except
        on e: Exception do
        begin
          LDM.cnxFeedFlow.Rollback;
        end;
      end;
    finally
      LDM.Critical.Leave;
    end;
  end;

  Response.StatusCode := 200;

  Handled := True;
end;

procedure TListFeedsController.TriListeFeeds( Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
var
  LLinesPerPage: Integer;
  LDM: TDMSession;
  //  LPagination: TPagination;
  LPage: Integer;
  LFeeds: TFeeds;
  LTitle: string;
  LCategories: TCategories;
  LSubcategories: TSubcategories;
  LCountries: TCountries;
  LLanguages: TLanguages;
begin
  LDM := GetDMSession( Request );

  if Assigned( LDM ) then
  begin
    LDM.SessionVariables.Values[ 'SortFeedsField' ] := Request.QueryFields.Values[ 'col' ];
    LDM.SessionVariables.Values[ 'SortFeedsOrd' ] := Request.QueryFields.Values[ 'dir' ];

    if not ( TryStrToInt( LDM.SessionVariables.Values[ LINEPERPAGE_VARIABLE ], LLinesPerPage ) ) then
    begin
      LLinesPerPage := 10;
    end;

    //      LPagination := LDM.Pagination( NAVIGATION_NAME );

    if not ( TryStrToInt( LDM.SessionVariables.Values[ NAVIGATION_NAME ], LPage ) ) then
    begin
      LPage := 0;
    end;

    //      LPage := LPagination.actualPage;
    if ( LPage > 0 ) then
    begin
      Dec( LPage );
    end;

    LFeeds := TFeeds( GetSessionObject( Request, 'qryListeFeeds' ) );
    if not ( Assigned( LFeeds ) ) then
    begin
      LFeeds := TFeeds.Create;
      AddSessionObject( Request, 'qryListeFeeds', LFeeds );
    end;

    LCategories := TCategories( GetSessionObject( Request, 'qryListCategories' ) );
    if not ( Assigned( LCategories ) ) then
    begin
      LCategories := TCategories.Create;
      AddSessionObject( Request, 'qryListCategories', LCategories );
    end;

    LSubcategories := TSubcategories( GetSessionObject( Request, 'qryListSubcategories' ) );
    if not ( Assigned( LSubcategories ) ) then
    begin
      LSubcategories := TSubcategories.Create;
      AddSessionObject( Request, 'qryListSubcategories', LSubcategories );
    end;

    LCountries := TCountries( GetSessionObject( Request, 'qryListCountries' ) );
    if not ( Assigned( LCountries ) ) then
    begin
      LCountries := TCountries.Create;
      AddSessionObject( Request, 'qryListCountries', LCountries );
    end;

    LLanguages := TLanguages( GetSessionObject( Request, 'qryListLanguages' ) );
    if not ( Assigned( LLanguages ) ) then
    begin
      LLanguages := TLanguages.Create;
      AddSessionObject( Request, 'qryListLanguages', LLanguages );
    end;

    LTitle := '%' + LDM.SessionVariables.Values[ SEARCH_VARIABLE ] + '%';

    FWebStencilsProcessor.AddVar( 'Categories', LCategories.GetListOfCategories( LDM.cnxFeedFlow ), False );
    FWebStencilsProcessor.AddVar( 'SousCategories', LSubcategories.GetListOfSubcategories( LDM.cnxFeedFlow ), False );
    FWebStencilsProcessor.AddVar( 'Pays', LCountries.GetListOfCountries( LDM.cnxFeedFlow ), False );
    FWebStencilsProcessor.AddVar( 'Langues', LLanguages.GetListOfLanguages( LDM.cnxFeedFlow ), False );
    FWebStencilsProcessor.AddVar(
      'feedsList',
      LFeeds.GetListeFeeds(
      LDM.cnxFeedFlow, LLinesPerPage, LPage * LLinesPerPage,
      LDM.SessionVariables.Values[ GROUPID_VARIABLE ].ToInteger,
      LTitle, //TODO: Voir pour contextualiser
      LDM.SessionVariables.Values[ 'SortFeedsField' ], LDM.SessionVariables.Values[ 'SortFeedsOrd' ]
      ),
      False );
    FWebStencilsProcessor.AddVar( 'Form', Self, False );

    Response.StatusCode := 200;
    Response.Content := RenderTemplate( TMP_LINES, Request );
  end;
end;

initialization

  TControllersRegistry.GetControllersList.AddClass( TListFeedsController );

end.

