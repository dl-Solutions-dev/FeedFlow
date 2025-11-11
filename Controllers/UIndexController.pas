unit UIndexController;

interface

uses
  System.SysUtils,
  Web.HTTPApp,
  Web.Stencils,
  uBaseController,
  uInterfaces,
  UDMSession;

type
  TIndexController = class( TBaseController )
  private
    function SaisieOK( aTitre: string ): string;
  public
    procedure Main( Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );

    procedure FeedsList( Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
    procedure DeleteFeeds( Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
    procedure GetNavigation( Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
    procedure FeedEditLineMode( Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
    procedure CancelFeedEditLine( Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
    procedure ApplyFeedEditLine( Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
    procedure AddFeed( Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
    procedure CancelAddFeed( Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
    procedure ApplyInsertFeed( Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );

    procedure InitializeActions( aWebModule: TWebModule; aWebStencil: TWebStencilsEngine ); override;
  end;

implementation

uses
  System.SyncObjs,
  System.Generics.Collections,
  System.StrUtils,
  IdHTTP,
  FireDAC.Stan.Param,
  utils.ClassHelpers,
  UConsts,
  uInvokerActions,
  UWMMain,
  Utils.Logger,
  UPagination;

const
  NAVIGATION_NAME: string = 'FeedList';
  SEARCH_VARIABLE: string = 'FeedsList.Search';
  LINEPERPAGE_VARIABLE: string = 'LinesPerPageFeed';
  TMP_ADD: string = 'FeedAdd.html';
  TMP_LISTE: string = 'FeedsList.html';
  TMP_TABLE: string = 'FeedsTable.html';
  TMP_LINE: string = 'FeedLine.html';
  TMP_LINE_EDIT: string = 'FeedLineEdit.html';
  TMP_NAVIGATION: string = 'ListNavigation.html';

  { TIndexController }

procedure TIndexController.AddFeed( Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean );
var
  LDM: TDMSession;
begin
  LDM := GetDMSession( Request );

  if Assigned( LDM ) then
  begin
    //    FWebStencilsProcessor.AddVar( 'Actions', FActionsParameters, False );
    FWebStencilsProcessor.AddVar( 'Feed', LDM.qryFeeds, False );

    Response.Content := RenderTemplate( TMP_ADD, Request );
  end
  else
  begin
    Response.Content := 'Invalid session';
  end;
end;

procedure TIndexController.ApplyFeedEditLine( Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
var
  LDM: TDMSession;
  LMsg: string;
begin
  LMsg := '';

  LDM := GetDMSession( Request );
  if Assigned( LDM ) then
  begin
    if ( Request.QueryFields.Values[ 'Id' ] <> '' ) then
    begin
      LDM.cnxFeedFlow.StartTransaction;

      LDM.QryFeeds.close;
      LDM.QryFeeds.ParamByName( 'ID_FEED' ).AsString := Request.QueryFields.Values[ 'Id' ];
      LDM.QryFeeds.Open;

      if not ( LDM.QryFeeds.Eof ) then
      begin
        LMsg := SaisieOK( Request.ContentFields.Values[ 'titre' ] );

        if ( LMsg = 'OK' ) then
        begin
          LDM.QryFeeds.Edit;

          //        LDM.QryFeedsID_FEED.Value := Request.ContentFields.Values[ 'idFeed' ].ToInteger;
          LDM.QryFeedsTITRE.Value := Request.ContentFields.Values[ 'titre' ];
          LDM.QryFeedsSTATUT.Value := Request.ContentFields.Values[ 'statut' ];
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
          LDM.QryFeeds.ParamByName( 'ID_FEED' ).AsString := Request.QueryFields.Values[ 'Id' ];
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

      Handled := True;
    end;
  end;
end;

procedure TIndexController.ApplyInsertFeed( Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
var
  LDM: TDMSession;
  LLAstId: Integer;
  LMsg: string;
begin
  LDM := GetDMSession( Request );

  if Assigned( LDM ) then
  begin
    LMsg := SaisieOK( Request.ContentFields.Values[ 'titre' ] );

    if ( LMsg = 'OK' ) then
    begin
      LDM.qryFeeds.Open;
      LDM.qryFeeds.Append;
      LDM.qryFeedsID_FEED.Value := -1;
      LDM.QryFeedsTITRE.Value := Request.ContentFields.Values[ 'titre' ];
      LDM.qryFeedsSTATUT.Value := Request.ContentFields.Values[ 'status' ];

      LDM.qryFeeds.Post;

      LLAstId := LDM.cnxFeedFlow.GetLastAutoGenValue( 'GEN_FEED' );

      LDM.qryFeeds.Close;
      LDM.qryFeeds.ParamByName( 'ID_FEED' ).AsInteger := LLAstId;
      LDM.qryFeeds.Open;

      FWebStencilsProcessor.AddVar( 'Feed', LDM.qryFeeds, False );

      Response.Content := RenderTemplate( TMP_LINE, Request );
    end
    else
    begin
      Response.Content := LMsg;
    end;

    Handled := True;
  end;
end;

procedure TIndexController.CancelAddFeed( Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean );
begin
  SendEmptyContent( Response );
end;

procedure TIndexController.CancelFeedEditLine( Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
var
  LDM: TDMSession;
begin
  LDM := GetDMSession( Request );
  if Assigned( LDM ) then
  begin
    LDM.Critical.Acquire;
    try
      LDM.qryFeeds.close;
      LDM.qryFeeds.ParamByName( 'ID_FEED' ).AsString := Request.QueryFields.Values[ 'Id' ];
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

procedure TIndexController.DeleteFeeds( Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean );
var
  LDM: TDMSession;
begin
  LDM := GetDMSession( Request );
  if Assigned( LDM ) then
  begin
    LDM.qryFeeds.close;
    LDM.qryFeeds.ParamByName( 'ID_FEED' ).AsString := Request.QueryFields.Values[ 'Id' ];
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

procedure TIndexController.FeedEditLineMode( Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
var
  LDM: TDMSession;
begin
  LDM := GetDMSession( Request );

  if Assigned( LDM ) then
  begin
    LDM.Critical.Acquire;
    try
      LDM.qryFeeds.close;
      LDM.qryFeeds.ParamByName( 'ID_FEED' ).AsString := Request.QueryFields.Values[ 'Id' ];
      LDM.qryFeeds.Open;

      if not ( LDM.qryFeeds.Eof ) then
      begin
        LDM.qryFeeds.Open;
        LDM.qryFeeds.First;

        FWebStencilsProcessor.AddVar( 'Feed', LDM.qryFeeds, False );
        FWebStencilsProcessor.AddVar( 'Form', Self, False );

        Response.Content := RenderTemplate( TMP_LINE_EDIT, Request );
      end;
    finally
      LDM.Critical.Leave;
    end;
  end;
end;

procedure TIndexController.FeedsList( Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean );
var
  LDM: TDMSession;
  LLinesPerPage: Integer;
  LPagination: TPagination;
  LPage: Integer;
  LInt: Integer;
  LTemplate: string;
begin
  LDM := GetDMSession( Request );

  if Assigned( LDM ) then
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

        LDM.SessionVariables.Values[ SEARCH_VARIABLE ] := '';

        LTemplate := TMP_LISTE;
        LDM.qryCountFeeds.close;
        LDM.qryCountFeeds.ParamByName( 'TITRE' ).AsString := '%' + LDM.SessionVariables.Values[ SEARCH_VARIABLE ] + '%';
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
      LDM.QryListeFeeds.ParamByName( 'FIRST' ).AsInteger := LLinesPerPage;
      LDM.QryListeFeeds.ParamByName( 'SKIP' ).AsInteger := LPage * LLinesPerPage;
      LDM.QryListeFeeds.ParamByName( 'TITRE' ).AsString := '%' + LDM.SessionVariables.Values[ SEARCH_VARIABLE ] + '%';
      LDM.QryListeFeeds.Open;

      FWebStencilsProcessor.AddVar( 'feedsList', LDM.QryListeFeeds, False );
      FWebStencilsProcessor.AddVar( 'Form', Self, False );

      Response.Content := RenderTemplate( LTemplate, Request );
    finally
      LDM.Critical.Release;
    end;
  end;
end;

procedure TIndexController.GetNavigation( Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean );
var
  //  LSession: TUserSession;
  LPagination: TPagination;
  LInt: Integer;
  LLinesPerPage: Integer;
  LDM: TDMSession;
begin
  LDM := GetDMSession( Request );

  if Assigned( LDM ) then
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
      LDM.qryCountFeeds.ParamByName( 'TITRE' ).AsString := '%' + LDM.SessionVariables.Values[ SEARCH_VARIABLE ] + '%';
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

    Handled := True;
  end;
end;

procedure TIndexController.InitializeActions( aWebModule: TWebModule;
  aWebStencil: TWebStencilsEngine );
begin
  inherited;

  aWebModule.AddRoutes( [
      TRoute.Create( mtGet, '/', Self.Main ),
      TRoute.Create( mtGet, '/FeedsList', Self.FeedsList ),
      TRoute.Create( mtDelete, '/DeleteFeed', Self.DeleteFeeds ),
      TRoute.Create( mtPost, '/GetFeedNavigation', Self.GetNavigation ),
      TRoute.Create( mtPost, '/FeedEditLineMode', Self.FeedEditLineMode ),
      TRoute.Create( mtAny, '/CancelFeedEditLine', Self.CancelFeedEditLine ),
      TRoute.Create( mtPost, '/ApplyFeedEditLine', Self.ApplyFeedEditLine ),
      TRoute.Create( mtPost, '/AddFeed', Self.AddFeed ),
      TRoute.Create( mtPost, '/CancelAddFeed', Self.CancelAddFeed ),
      TRoute.Create( mtPost, '/ApplyInsertFeed', Self.ApplyInsertFeed )
      ] );
end;

procedure TIndexController.Main( Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean );
begin
  Response.SendRedirect( './FeedsList?scope=Page' );
end;

function TIndexController.SaisieOK( aTitre: string ): string;
begin
  Result := 'OK';

  if ( aTitre.Trim = '' ) then
  begin
    Result := 'ERR:Il faut renseigner un titre';
  end;
end;

initialization

  TInvokerActions.GetInvokerActions.AddAction( TIndexController.Create );

end.

