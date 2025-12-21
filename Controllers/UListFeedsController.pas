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
  UDMSession;

type
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
    procedure GetFeed( Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
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
  uInvokerActions,
  UWMMain,
  Utils.Logger,
  UPagination,
  Helpers.Messages,
  Utils.Token;

const
  NAVIGATION_NAME: string = 'FeedList';
  SEARCH_VARIABLE: string = 'FeedsList.Search';
  LINEPERPAGE_VARIABLE: string = 'LinesPerPageFeed';
  TMP_ADD: string = 'FeedAdd.html';
  TMP_LISTE: string = 'FeedsList.html';
  TMP_TABLE: string = 'FeedsTable.html';
  TMP_LINE: string = 'FeedLine.html';
  TMP_LINES: string = 'FeedsLines.html';
  TMP_LINE_EDIT: string = 'FeedLineEdit.html';
  TMP_NAVIGATION: string = 'ListNavigation.html';
  TMP_LOGIN: string = 'IndexAdmin.html';

  { TListFeedsController }

procedure TListFeedsController.AddFeed( Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean );
var
  LDM: TDMSession;
  LToken: TToken;
begin
  LDM := GetDMSession( Request );

  if Assigned( LDM ) then
  begin
    if ValidToken( Request, True, True, LToken ) and ( LToken.Role = 'ADMIN' ) then
    begin
      //    FWebStencilsProcessor.AddVar( 'Actions', FActionsParameters, False );
      FWebStencilsProcessor.AddVar( 'Feed', LDM.qryFeeds, False );

      Response.Content := RenderTemplate( TMP_ADD, Request );
    end;
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
//    LFileName: string;
//  FileData: TStream;
  LCategorie,
    LSousCategorie,
    LGroupe: Integer;
  LToken: TToken;
begin
  LMsg := '';

  LDM := GetDMSession( Request );
  if Assigned( LDM ) then
  begin
    if ValidToken( Request, True, True, LToken ) and ( LToken.Role = 'ADMIN' ) then
    begin
      if ( Request.QueryFields.Values[ 'Id' ] <> '' ) then
      begin
        LDM.cnxFeedFlow.StartTransaction;

        LDM.QryFeeds.close;
        LDM.QryFeeds.ParamByName( 'FEED_ID' ).AsString := Request.QueryFields.Values[ 'Id' ];
        LDM.QryFeeds.Open;

        if not ( LDM.QryFeeds.Eof ) then
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
            LDM.QryFeeds.Edit;

            LDM.qryFeedsFEED_NAME.Value := Request.ContentFields.Values[ 'nom' ];
            LDM.qryFeedsTITLE.Value := Request.ContentFields.Values[ 'titre' ];
            LDM.qryFeedsSTATUS.Value := Request.ContentFields.Values[ 'statut' ];
            if not ( TryStrToInt( Request.ContentFields.Values[ 'groupe' ], LGroupe ) ) then
            begin
              LGroupe := 0;
            end;
            LDM.qryFeedsFEED_GROUP.Value := LGroupe;
            //          LDM.qryFeedsTEMPLATE_AFFICHAGE.Value := LFileName;
            try
              LDM.QryFeeds.Post;
              LDM.cnxFeedFlow.Commit;
            except
              on e: Exception do
              begin
                LMsg := 'ERR:' + Request.QueryFields.Text;
                LDM.cnxFeedFlow.Rollback;
              end;
            end;

            LDM.QryFeeds.close;
            LDM.QryFeeds.ParamByName( 'FEED_ID' ).AsString := Request.QueryFields.Values[ 'Id' ];
            LDM.QryFeeds.Open;

            FWebStencilsProcessor.AddVar( 'Feed', LDM.QryFeeds, False );
            FWebStencilsProcessor.AddVar( 'Form', Self, False );
          end;

          if LMsg = 'OK' then
          begin
            Response.Content := RenderTemplate( TMP_LINE, Request );
          end
          else
          begin
            Response.Content := LMsg;
          end;
        end
        else
        begin
          Response.Content := Request.QueryFields.Values[ 'Id' ] + ' non trouvé.';
        end;
      end;
    end;
  end;

  Handled := True;
end;

procedure TListFeedsController.ApplyInsertFeed( Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
var
  LDM: TDMSession;
  LLAstId,
    LGroupe: Integer;
  LMsg: string;
  LCategorie, LSousCategorie: Integer;
  LToken: TToken;
begin
  LDM := GetDMSession( Request );

  if Assigned( LDM ) then
  begin
    if ValidToken( Request, True, True, LToken ) and ( LToken.Role = 'ADMIN' ) then
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
        LDM.qryFeeds.Open;
        LDM.qryFeeds.Append;
        LDM.qryFeedsFEED_ID.Value := -1;
        LDM.qryFeedsFEED_NAME.Value := Request.ContentFields.Values[ 'nom' ];
        LDM.qryFeedsTITLE.Value := Request.ContentFields.Values[ 'titre' ];
        LDM.qryFeedsSTATUS.Value := Request.ContentFields.Values[ 'status' ];
        LDM.qryFeedsDISPLAY_TEMPLATE.Value := Request.ContentFields.Values[ 'template' ];
        if not ( TryStrToInt( Request.ContentFields.Values[ 'groupe' ], LGroupe ) ) then
        begin
          LGroupe := 0;
        end;
        LDM.qryFeedsFEED_GROUP.Value := LGroupe;

        LDM.qryFeeds.Post;

        LLAstId := LDM.cnxFeedFlow.GetLastAutoGenValue( 'GEN_FEED' );

        LDM.qryFeeds.Close;
        LDM.qryFeeds.ParamByName( 'FEED_ID' ).AsInteger := LLAstId;
        LDM.qryFeeds.Open;

        FWebStencilsProcessor.AddVar( 'Feed', LDM.qryFeeds, False );

        Response.Content := RenderTemplate( TMP_LINE, Request );
      end
      else
      begin
        Response.Content := LMsg;
      end;
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
  LToken: TToken;
begin
  LDM := GetDMSession( Request );
  if Assigned( LDM ) then
  begin
    if ValidToken( Request, True, True, LToken ) and ( LToken.Role = 'ADMIN' ) then
    begin
      LDM.Critical.Acquire;
      try
        LDM.qryFeeds.close;
        LDM.qryFeeds.ParamByName( 'FEED_ID' ).AsString := Request.QueryFields.Values[ 'Id' ];
        LDM.qryFeeds.Open;

        if not ( LDM.qryFeeds.Eof ) then
        begin
          FWebStencilsProcessor.AddVar( 'Feed', LDM.qryFeeds, False );
          FWebStencilsProcessor.AddVar( 'Form', Self, False );

          Response.Content := RenderTemplate( TMP_LINE, Request );
        end;
      finally
        LDM.Critical.Leave;
      end;
    end;
  end;
end;

procedure TListFeedsController.DeleteFeeds( Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
var
  LDM: TDMSession;
  LToken: TToken;
begin
  LDM := GetDMSession( Request );
  if Assigned( LDM ) then
  begin
    if ValidToken( Request, True, True, LToken ) and ( LToken.Role = 'ADMIN' ) then
    begin
      LDM.qryFeeds.close;
      LDM.qryFeeds.ParamByName( 'FEED_ID' ).AsString := Request.QueryFields.Values[ 'Id' ];
      LDM.qryFeeds.Open;

      if not ( LDM.qryFeeds.Eof ) then
      begin
        LDM.qryFeeds.Delete;
        SendEmptyContent( Response );
      end
      else
      begin
        Response.Content := 'liste non trouvée.';
      end;
    end;
  end;
end;

procedure TListFeedsController.FeedEditLineMode( Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
var
  LDM: TDMSession;
  LToken: TToken;
begin
  LDM := GetDMSession( Request );

  if Assigned( LDM ) then
  begin
    if ValidToken( Request, True, True, LToken ) and ( LToken.Role = 'ADMIN' ) then
    begin
      LDM.Critical.Acquire;
      try
        LDM.qryFeeds.close;
        LDM.qryFeeds.ParamByName( 'FEED_ID' ).AsString := Request.QueryFields.Values[ 'Id' ];
        LDM.qryFeeds.Open;

        if not ( LDM.qryFeeds.Eof ) then
        begin
          LDM.qryFeeds.Open;
          LDM.qryFeeds.First;

          FWebStencilsProcessor.AddVar( 'Feed', LDM.qryFeeds, False );
          FWebStencilsProcessor.AddVar( 'Categories', LDM.QryListCategories, False );
          FWebStencilsProcessor.AddVar( 'SousCategories', LDM.QryListSubCategories, False );
          FWebStencilsProcessor.AddVar( 'Pays', LDM.QryListCountries, False );
          FWebStencilsProcessor.AddVar( 'Langues', LDM.QryListLanguages, False );
          FWebStencilsProcessor.AddVar( 'Form', Self, False );

          Response.Content := RenderTemplate( TMP_LINE_EDIT, Request );
        end;

        LDM.qryFeeds.close;
      finally
        LDM.Critical.Leave;
      end;
    end;
  end;
end;

procedure TListFeedsController.FeedsList( Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean );
var
  LDM: TDMSession;
  LLinesPerPage: Integer;
  LPagination: TPagination;
  LPage: Integer;
  LInt: Integer;
  LTemplate: string;
  LToken: TToken;
begin
  LDM := GetDMSession( Request );

  if Assigned( LDM ) then
  begin
    if ValidToken( Request, False, True, LToken ) and ( LToken.Role = 'ADMIN' ) then
    begin
      LDM.cnxFeedFlow.Rollback;

      if not ( TryStrToInt( LDM.SessionVariables.Values[ LINEPERPAGE_VARIABLE ], LLinesPerPage ) ) then
      begin
        LLinesPerPage := 10;
      end;

      LPagination := LDM.Pagination( NAVIGATION_NAME );

      LPage := LPagination.actualPage;

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

          LDM.SessionVariables.Values[ SEARCH_VARIABLE ] := '';

          LTemplate := TMP_LISTE;
          LDM.qryCountFeeds.close;
          LDM.qryCountFeeds.ParamByName( 'TITLE' ).AsString := '%' + LDM.SessionVariables.Values[ SEARCH_VARIABLE ] + '%';
          LDM.qryCountFeeds.Open;

          if not ( TryStrToInt( Request.QueryFields.Values[ 'Actual' ], LInt ) ) then
          begin
            LInt := 1;
          end;
          LPagination.GeneratePagesList( lDM.qryCountFeedsNB_ENR.Value, LLinesPerPage, LInt, '', '', 'FeedsList',
            'GetFeedNavigation' );

          FWebStencilsProcessor.AddVar( 'pages', LDM.Pagination( NAVIGATION_NAME ), False );
        end
        else // Sinon, on rafraichit juste la liste
        begin
          LTemplate := TMP_TABLE
        end;

        FMsg := FMsg + 'FeedsList';

        LDM.QryListeFeeds.close;
        LDM.QryListeFeeds.SQL.Text := QRY_LISTE_FEEDS +
          ' order by ' + LDM.SessionVariables.Values[ 'SortFeedsField' ] + ' ' + LDM.SessionVariables.Values[ 'SortFeedsOrd' ];
        LDM.QryListeFeeds.ParamByName( 'FIRST' ).AsInteger := LLinesPerPage;
        LDM.QryListeFeeds.ParamByName( 'SKIP' ).AsInteger := LPage * LLinesPerPage;
        LDM.QryListeFeeds.ParamByName( 'TITLE' ).AsString := '%' + LDM.SessionVariables.Values[ SEARCH_VARIABLE ] + '%';
        LDM.QryListeFeeds.Open;

        FWebStencilsProcessor.AddVar( 'Categories', LDM.QryListCategories, False );
        FWebStencilsProcessor.AddVar( 'SousCategories', LDM.QryListSubCategories, False );
        FWebStencilsProcessor.AddVar( 'Pays', LDM.QryListCountries, False );
        FWebStencilsProcessor.AddVar( 'Langues', LDM.QryListLanguages, False );
        FWebStencilsProcessor.AddVar( 'feedsList', LDM.QryListeFeeds, False );
        FWebStencilsProcessor.AddVar( 'Form', Self, False );

        Response.StatusCode := 200;
        Response.Content := RenderTemplate( LTemplate, Request );
      finally
        LDM.Critical.Release;
      end;
    end
    else
    begin
      Response.StatusCode := 403;
    end;
  end;
end;

procedure TListFeedsController.GetFeed( Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean );
var
  LDM: TDMSession;
  LToken: TToken;
  LJson: TJSONObject;
  LArray: TJSONArray;
begin
  if ValidToken( Request, True, True, LToken ) and ( LToken.Role = 'ADMIN' ) then
  begin
    LDM := GetDMSession( Request );

    if Assigned( LDM ) then
    begin
      LDM.Critical.Acquire;
      try
        LDM.qryFeeds.close;
        LDM.qryFeeds.ParamByName( 'FEED_ID' ).AsString := Request.QueryFields.Values[ 'Id' ];
        LDM.qryFeeds.Open;

        if not ( LDM.qryFeeds.Eof ) then
        begin
          LDM.qryFeeds.Open;
          LDM.qryFeeds.First;

          LJson := TJSONObject.Create;
          try
            LJson.AddPair( 'AllContext', LDM.qryFeedsALL_CONTEXTS.Value );

            // On envoi les catégories
            LArray := TJSONArray.Create;
            LDM.QryFeedCategories.ParamByName( 'FEED_ID' ).AsString := Request.QueryFields.Values[ 'Id' ];
            LDM.QryFeedCategories.Open;

            while not ( LDM.QryFeedCategories.Eof ) do
            begin
              LArray.Add( LDM.QryFeedCategoriesCATEGORY_ID.Value );

              LDM.QryFeedCategories.Next;
            end;

            LDM.QryFeedCategories.Close;

            LJson.AddPair( 'BU', LArray );

            // On envoi les sous-catégories
            LArray := TJSONArray.Create;
            LDM.QryFeedSubCategories.ParamByName( 'FEED_ID' ).AsString := Request.QueryFields.Values[ 'Id' ];
            LDM.QryFeedSubCategories.Open;

            while not ( LDM.QryFeedSubCategories.Eof ) do
            begin
              LArray.Add( LDM.QryFeedSubCategoriesSUBCATEGORY_ID.Value );

              LDM.QryFeedSubCategories.Next;
            end;

            LDM.QryFeedSubCategories.Close;

            LJson.AddPair( 'TypePartner', LArray );

            // On envoi les pays
            LArray := TJSONArray.Create;
            LDM.QryFeedCountry.ParamByName( 'FEED_ID' ).AsString := Request.QueryFields.Values[ 'Id' ];
            LDM.QryFeedCountry.Open;

            while not ( LDM.QryFeedCountry.Eof ) do
            begin
              LArray.Add( LDM.QryFeedCountryCOUNTRY_CODE.Value );

              LDM.QryFeedCountry.Next;
            end;

            LDM.QryFeedCountry.Close;

            LJson.AddPair( 'Country', LArray );

            // On envoi les langues
            LArray := TJSONArray.Create;
            LDM.QryFeedLanguage.ParamByName( 'FEED_ID' ).AsString := Request.QueryFields.Values[ 'Id' ];
            LDM.QryFeedLanguage.Open;

            while not ( LDM.QryFeedLanguage.Eof ) do
            begin
              LArray.Add( LDM.QryFeedLanguageLANGUAGE_CODE.Value );

              LDM.QryFeedLanguage.Next;
            end;

            LDM.QryFeedLanguage.Close;

            LJson.AddPair( 'Lang', LArray );

            Response.ContentType := 'application/json; charset=utf-8';
            Response.Content := LJson.ToJSON;
          finally
            LJson.Free;
          end;
        end;

        LDM.qryFeeds.Close;
      finally
        LDM.Critical.Leave;
      end;
    end;
  end;
end;

procedure TListFeedsController.GetNavigation( Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
var
  //  LSession: TUserSession;
  LPagination: TPagination;
  LInt: Integer;
  LLinesPerPage: Integer;
  LDM: TDMSession;
  LToken: TToken;
begin
  LDM := GetDMSession( Request );

  if Assigned( LDM ) then
  begin
    if ValidToken( Request, True, True, LToken ) and ( LToken.Role = 'ADMIN' ) then
    begin
      if not ( TryStrToInt( Request.ContentFields.Values[ 'LinesPerPage' ], LLinesPerPage ) ) then
      begin
        LLinesPerPage := 10;
      end;

      LDM.SessionVariables.Values[ LINEPERPAGE_VARIABLE ] := LLinesPerPage.ToString;

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
        LDM.qryCountFeeds.close;
        LDM.qryCountFeeds.ParamByName( 'TITLE' ).AsString := '%' + LDM.SessionVariables.Values[ SEARCH_VARIABLE ] + '%';
        LDM.qryCountFeeds.Open;

        FMsg := 'GetPagination';

        LPagination := LDM.Pagination( NAVIGATION_NAME );

        LPagination.GeneratePagesList( LDM.qryCountFeedsNB_ENR.Value, LLinesPerPage, LInt, '', Request.ContentFields.Values[
          'Search' ], 'FeedsList', 'GetFeedNavigation' );

        FWebStencilsProcessor.AddVar( 'pages', LPagination, False );
        FWebStencilsProcessor.AddVar( 'Form', Self, False );

        Response.Content := RenderTemplate( TMP_NAVIGATION, Request );
      finally
        LDM.Critical.Release;
      end;
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
      TRoute.Create( mtAny, '/GetFeed', Self.GetFeed ),
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
  LToken: TToken;
  LJsonObj: TJSONObject;
  LAllContext: string;
  LArrayCategorie,
    LArraySousCategorie,
    LArrayPays,
    LArrayLangue: TJSONArray;
begin
  if ValidToken( Request, True, True, LToken ) and ( LToken.Role = 'ADMIN' ) then
  begin
    LDM := GetDMSession( Request );

    if Assigned( LDM ) then
    begin
      LDM.Critical.Acquire;
      try
        LDM.cnxFeedFlow.StartTransaction;
        try
          LJsonObj := TJSONObject.ParseJSONValue( Request.Content ) as TJSONObject;
          try
            LAllContext := LJsonObj.GetValue<string>( 'AllContext' );

            LArrayCategorie := LJsonObj.GetValue<TJSONArray>( 'BU' );
            LArraySousCategorie := LJsonObj.GetValue<TJSONArray>( 'TypePartner' );
            LArrayPays := LJsonObj.GetValue<TJSONArray>( 'Country' );
            LArrayLangue := LJsonObj.GetValue<TJSONArray>( 'Lang' );

            LDM.qryFeeds.close;
            LDM.qryFeeds.ParamByName( 'FEED_ID' ).AsString := Request.QueryFields.Values[ 'IdFeed' ];
            LDM.qryFeeds.Open;

            if not ( LDM.qryFeeds.Eof ) then
            begin
              LDM.qryFeeds.Edit;
              LDM.qryFeedsALL_CONTEXTS.Value := LAllContext;

              LDM.qryFeeds.Post;
              LDM.qryFeeds.Close;
            end;

            // On supprime les anciens liens
            LDM.QryFeedCategories.ParamByName( 'FEED_ID' ).AsString := Request.QueryFields.Values[ 'IdFeed' ];
            LDM.QryFeedCategories.Open;
            while not ( LDM.QryFeedCategories.Eof ) do
            begin
              LDM.QryFeedCategories.Delete;
            end;

            LDM.QryFeedSubCategories.ParamByName( 'FEED_ID' ).AsString := Request.QueryFields.Values[ 'IdFeed' ];
            LDM.QryFeedSubCategories.Open;
            while not ( LDM.QryFeedSubCategories.Eof ) do
            begin
              LDM.QryFeedSubCategories.Delete;
            end;

            LDM.QryFeedCountry.ParamByName( 'FEED_ID' ).AsString := Request.QueryFields.Values[ 'IdFeed' ];
            LDM.QryFeedCountry.Open;
            while not ( LDM.QryFeedCountry.Eof ) do
            begin
              LDM.QryFeedCountry.Delete;
            end;

            LDM.QryFeedLanguage.ParamByName( 'FEED_ID' ).AsString := Request.QueryFields.Values[ 'IdFeed' ];
            LDM.QryFeedLanguage.Open;
            while not ( LDM.QryFeedLanguage.Eof ) do
            begin
              LDM.QryFeedLanguage.Delete;
            end;

            if ( LAllContext = 'N' ) then
            begin
              // on Sauvegarde le lien avec les catégories
              for var i := 0 to LArrayCategorie.Count - 1 do
              begin
                LDM.QryFeedCategories.Append;
                LDM.QryFeedCategoriesCATEGORY_ID.Value := StrToInt( LArrayCategorie.Items[ i ].Value );
                LDM.QryFeedCategoriesFEED_ID.Value := StrToInt( Request.QueryFields.Values[ 'IdFeed' ] );
                LDM.QryFeedCategories.Post;
              end;

              // on Sauvegarde le lien avec les sous-catégories
              for var i := 0 to LArraySousCategorie.Count - 1 do
              begin
                LDM.QryFeedSubCategories.Append;
                LDM.QryFeedSubCategoriesSUBCATEGORY_ID.Value := StrToInt( LArraySousCategorie.Items[ i ].Value );
                LDM.QryFeedSubCategoriesFEED_ID.Value := StrToInt( Request.QueryFields.Values[ 'IdFeed' ] );
                LDM.QryFeedSubCategories.Post;
              end;

              // on Sauvegarde le lien avec les pays
              for var i := 0 to LArrayPays.Count - 1 do
              begin
                LDM.QryFeedCountry.Append;
                LDM.QryFeedCountryCOUNTRY_CODE.Value := LArrayPays.Items[ i ].Value;
                LDM.QryFeedCountryFEED_ID.Value := StrToInt( Request.QueryFields.Values[ 'IdFeed' ] );
                LDM.QryFeedCountry.Post;
              end;

              // on Sauvegarde le lien avec les langues
              for var i := 0 to LArrayLangue.Count - 1 do
              begin
                LDM.QryFeedLanguage.Append;
                LDM.QryFeedLanguageLANGUAGE_CODE.Value := LArrayLangue.Items[ i ].Value;
                LDM.QryFeedLanguageFEED_ID.Value := StrToInt( Request.QueryFields.Values[ 'IdFeed' ] );
                LDM.QryFeedLanguage.Post;
              end;
            end;

            LDM.QryFeedCategories.Close;
            LDM.QryFeedSubCategories.Close;
            LDM.QryFeedCountry.Close;
            LDM.QryFeedLanguage.Close;
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
  end;

  Handled := True;
end;

procedure TListFeedsController.TriListeFeeds( Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
var
  LLinesPerPage: Integer;
  LToken: TToken;
  LDM: TDMSession;
  LPagination: TPagination;
  LPage: Integer;
begin
  if ValidToken( Request, False, True, LToken ) and ( LToken.Role = 'ADMIN' ) then
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

      LPagination := LDM.Pagination( NAVIGATION_NAME );

      LPage := LPagination.actualPage;
      if ( LPage > 0 ) then
      begin
        Dec( LPage );
      end;

      LDM.QryListeFeeds.close;
      LDM.QryListeFeeds.SQL.Text := QRY_LISTE_FEEDS +
        ' order by ' + LDM.SessionVariables.Values[ 'SortFeedsField' ] + ' ' + LDM.SessionVariables.Values[ 'SortFeedsOrd' ];
      LDM.QryListeFeeds.ParamByName( 'FIRST' ).AsInteger := LLinesPerPage;
      LDM.QryListeFeeds.ParamByName( 'SKIP' ).AsInteger := LPage * LLinesPerPage;
      LDM.QryListeFeeds.ParamByName( 'TITLE' ).AsString := '%' + LDM.SessionVariables.Values[ SEARCH_VARIABLE ] + '%';
      LDM.QryListeFeeds.Open;

      FWebStencilsProcessor.AddVar( 'Categories', LDM.QryListCategories, False );
      FWebStencilsProcessor.AddVar( 'SousCategories', LDM.QryListSubCategories, False );
      FWebStencilsProcessor.AddVar( 'Pays', LDM.QryListCountries, False );
      FWebStencilsProcessor.AddVar( 'Langues', LDM.QryListLanguages, False );
      FWebStencilsProcessor.AddVar( 'feedsList', LDM.QryListeFeeds, False );
      FWebStencilsProcessor.AddVar( 'Form', Self, False );

      Response.StatusCode := 200;
      Response.Content := RenderTemplate( TMP_LINES, Request );
    end;
  end
  else
  begin
    Response.StatusCode := 403;
  end;
end;

initialization

  TInvokerActions.GetInvokerActions.AddAction( TListFeedsController.Create );

end.

