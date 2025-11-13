unit UListNewsController;

interface

uses
  System.SysUtils,
  Web.HTTPApp,
  Web.Stencils,
  uBaseController,
  uInterfaces,
  UDMSession;

type
  TListENewsController = class( TBaseController )
  private
    FFeedId: string;

    function SaisieOK( aTitre, aOrdre: string; aDatePublication, aDatePeremption:
      TDateTime ): string;

    procedure SetFeedId( const Value: string );
  public
    procedure NewsList( Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
    procedure DeleteNews( Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
    procedure GetNavigation( Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
    procedure NewsEditLineMode( Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
    procedure CancelNewsEditLine( Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
    procedure ApplyNewsEditLine( Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
    procedure AddNews( Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
    procedure CancelAddNews( Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
    procedure ApplyInsertNews( Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
    procedure SaveContentNews( Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
    procedure ShowNews( Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
    procedure GetNews( Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );

    procedure InitializeActions( aWebModule: TWebModule; aWebStencil: TWebStencilsEngine ); override;

    property FeedId: string read FFeedId write SetFeedId;
  end;

implementation

uses
  System.SyncObjs,
  System.JSON,
  System.IOUtils,
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
  NAVIGATION_NAME: string = 'NewsList';
  SEARCH_VARIABLE: string = 'NewsList.Search';
  LINEPERPAGE_VARIABLE: string = 'LinesPerPageFeed';
  TMP_ADD: string = 'NewsAdd.html';
  TMP_LISTE: string = 'NewsList.html';
  TMP_TABLE: string = 'NewsTable.html';
  TMP_LINE: string = 'NewsLine.html';
  TMP_LINE_EDIT: string = 'NewsLineEdit.html';
  TMP_NAVIGATION: string = 'ListNavigation.html';

  { TListENewsController }

procedure TListENewsController.AddNews( Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean );
var
  LDM: TDMSession;
begin
  LDM := GetDMSession( Request );

  if Assigned( LDM ) then
  begin
    //    FWebStencilsProcessor.AddVar( 'Actions', FActionsParameters, False );
    FWebStencilsProcessor.AddVar( 'News', LDM.QryNews, False );

    Response.Content := RenderTemplate( TMP_ADD, Request );
  end
  else
  begin
    Response.Content := 'Invalid session';
  end;
end;

procedure TListENewsController.ApplyInsertNews( Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
var
  LDM: TDMSession;
  LLAstId: Integer;
  LDatePublication, lDatePeremption: TDateTime;
  LFs: TFormatSettings;
  LMsg: string;
  LOrdre: Integer;
begin
  LDM := GetDMSession( Request );

  if Assigned( LDM ) then
  begin
    LFs := TFormatSettings.Create;
    LFs.DateSeparator := '-';
    LFs.ShortDateFormat := 'yyyy-MM-dd';
    LFs.TimeSeparator := ':';
    LFs.ShortTimeFormat := 'hh:mm';
    LFs.LongTimeFormat := 'hh:mm:ss';

    if not ( TryStrToDate( Request.ContentFields.Values[ 'datepublication' ], LDatePublication, LFs ) ) then
    begin
      LDatePublication := 0;
    end;

    if not ( TryStrToDate( Request.ContentFields.Values[ 'dateperemption' ], lDatePeremption, LFs ) ) then
    begin
      lDatePeremption := 0;
    end;

    LMsg := SaisieOK( Request.ContentFields.Values[ 'titre' ], Request.ContentFields.Values[ 'ordreaffichage' ],
      LDatePublication, lDatePeremption );

    if ( LMsg = 'OK' ) then
    begin
      if not ( TryStrToInt( Request.ContentFields.Values[ 'ordreaffichage' ], LOrdre ) ) then
      begin
        LOrdre := 0;
      end;

      LDM.QryNews.Open;
      LDM.QryNews.Append;
      LDM.QryNewsIDNEWS.Value := -1;
      LDM.QryNewsTITRE_NEWS.Value := Request.ContentFields.Values[ 'titre' ];
      LDM.QryNewsORDRE_AFFICHAGE.Value := LOrdre;
      LDM.QryNewsHOLD.Value := Request.ContentFields.Values[ 'status' ];
      LDM.QryNewsDATE_PUBLICATION.Value := LDatePublication;
      LDM.QryNewsDATE_PEREMPTION.Value := lDatePeremption;
      LDM.QryNewsTEXTE.Value := '';
      LDM.QryNewsID_FEED.Value := FFeedId.ToInteger;
      LDM.QryNews.Post;

      LLAstId := LDM.cnxFeedFlow.GetLastAutoGenValue( 'GEN_NEWS' );

      LDM.QryNews.Close;
      LDM.QryNews.ParamByName( 'IDNEWS' ).AsInteger := LLAstId;
      LDM.QryNews.Open;

      FWebStencilsProcessor.AddVar( 'News', LDM.QryNews, False );
    end;

    if ( LMsg = 'OK' ) then
    begin
      Response.Content := RenderTemplate( TMP_LINE, Request );
    end
    else
    begin
      Response.Content := LMsg;
    end;

    Handled := True;
  end;
end;

procedure TListENewsController.ApplyNewsEditLine( Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
var
  LDM: TDMSession;
  LMsg: string;
  LDatePublication, lDatePeremption: TDateTime;
  LFs: TFormatSettings;
  LOrdre: Integer;
begin
  LMsg := '';

  LDM := GetDMSession( Request );
  if Assigned( LDM ) then
  begin
    if ( Request.QueryFields.Values[ 'Id' ] <> '' ) then
    begin
      LDM.cnxFeedFlow.StartTransaction;

      LDM.QryNews.close;
      LDM.QryNews.ParamByName( 'IDNEWS' ).AsString := Request.QueryFields.Values[ 'Id' ];
      LDM.QryNews.Open;

      if not ( LDM.QryNews.Eof ) then
      begin
        LFs := TFormatSettings.Create;
        LFs.DateSeparator := '-';
        LFs.ShortDateFormat := 'yyyy-MM-dd';
        LFs.TimeSeparator := ':';
        LFs.ShortTimeFormat := 'hh:mm';
        LFs.LongTimeFormat := 'hh:mm:ss';

        if not ( TryStrToDate( Request.ContentFields.Values[ 'datepublication' ], LDatePublication, LFs ) ) then
        begin
          LDatePublication := 0;
        end;

        if not ( TryStrToDate( Request.ContentFields.Values[ 'dateperemption' ], lDatePeremption, LFs ) ) then
        begin
          lDatePeremption := 0;
        end;

        LMsg := SaisieOK( Request.ContentFields.Values[ 'titre' ], Request.ContentFields.Values[ 'ordreaffichage' ],
          LDatePublication, lDatePeremption );
        if ( LMsg = 'OK' ) then
        begin
          if not ( TryStrToInt( Request.ContentFields.Values[ 'ordreaffichage' ], LOrdre ) ) then
          begin
            LOrdre := 0;
          end;

          LDM.QryNews.Edit;

          //        LDM.QryFeedsID_FEED.Value := Request.ContentFields.Values[ 'idFeed' ].ToInteger;
          LDM.QryNewsTITRE_NEWS.Value := Request.ContentFields.Values[ 'titre' ];
          LDM.QryNewsORDRE_AFFICHAGE.Value := LOrdre;
          LDM.QryNewsHOLD.Value := Request.ContentFields.Values[ 'status' ];
          LDM.QryNewsDATE_PUBLICATION.Value := LDatePublication;
          LDM.QryNewsDATE_PEREMPTION.Value := lDatePeremption;
          try
            LDM.QryNews.Post;
            LDM.cnxFeedFlow.Commit;
          except
            on e: Exception do
            begin
              LMsg := 'ERR:' + Request.QueryFields.Text;
              LDM.cnxFeedFlow.Rollback;
            end;
          end;

          LDM.QryNews.close;
          LDM.QryNews.ParamByName( 'IDNEWS' ).AsString := Request.QueryFields.Values[ 'Id' ];
          LDM.QryNews.Open;

          FWebStencilsProcessor.AddVar( 'News', LDM.QryNews, False );
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

procedure TListENewsController.CancelAddNews( Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
begin
  SendEmptyContent( Response );
end;

procedure TListENewsController.CancelNewsEditLine( Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
var
  LDM: TDMSession;
begin
  LDM := GetDMSession( Request );
  if Assigned( LDM ) then
  begin
    LDM.Critical.Acquire;
    try
      LDM.QryNews.close;
      LDM.QryNews.ParamByName( 'IDNEWS' ).AsString := Request.QueryFields.Values[ 'Id' ];
      LDM.QryNews.Open;

      if not ( LDM.QryNews.Eof ) then
      begin
        FWebStencilsProcessor.AddVar( 'News', LDM.QryNews, False );
        FWebStencilsProcessor.AddVar( 'Form', Self, False );

        Response.Content := RenderTemplate( TMP_LINE, Request );
      end;
    finally
      LDM.Critical.Leave;
    end;
  end;

end;

procedure TListENewsController.DeleteNews( Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean );
var
  LDM: TDMSession;
begin
  LDM := GetDMSession( Request );
  if Assigned( LDM ) then
  begin
    LDM.QryNews.close;
    LDM.QryNews.ParamByName( 'IDNEWS' ).AsString := Request.QueryFields.Values[ 'Id' ];
    LDM.QryNews.Open;

    if not ( LDM.QryNews.Eof ) then
    begin
      LDM.QryNews.Delete;
      SendEmptyContent( Response );
    end
    else
    begin
      Response.Content := 'liste non trouvée.';
    end;
  end;
end;

procedure TListENewsController.GetNavigation( Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
var
  //  LSession: TUserSession;
  LPagination: TPagination;
  LInt: Integer;
  LLinesPerPage: Integer;
  LDM: TDMSession;
  LDateSearch: TDateTime;
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

    if not ( TryStrToDate( LDM.SessionVariables.Values[ SEARCH_VARIABLE ], LDateSearch ) ) then
    begin
      LDateSearch := 0;
    end;

    LDM.Critical.Acquire;
    try
      LDM.QryCountNews.close;
      LDM.QryCountNews.ParamByName( 'ID_FEED' ).AsInteger := FFeedId.ToInteger;
      LDM.QryCountNews.ParamByName( 'TITRE_NEWS' ).AsString := '%' + LDM.SessionVariables.Values[ SEARCH_VARIABLE ] + '%';
      LDM.QryCountNews.ParamByName( 'DATE_CREATION' ).AsDateTime := LDateSearch;
      LDM.QryCountNews.ParamByName( 'DATE_PUBLICATION' ).AsDateTime := LDateSearch;
      LDM.QryCountNews.Open;

      FMsg := 'GetPagination';

      LPagination := LDM.Pagination( NAVIGATION_NAME );

      LPagination.GeneratePagesList( LDM.QryCountNewsNB_ENR.Value, LLinesPerPage, LInt, '', Request.ContentFields.Values[
        'Search' ], 'FeedsList', 'GetNewsNavigation' );

      FWebStencilsProcessor.AddVar( 'pages', LPagination, False );
      FWebStencilsProcessor.AddVar( 'Form', Self, False );

      Response.Content := RenderTemplate( TMP_NAVIGATION, Request );
    finally
      LDM.Critical.Release;
    end;

    Handled := True;
  end;
end;

procedure TListENewsController.GetNews( Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean );
var
  LDM: TDMSession;
begin
  LDM := GetDMSession( Request );

  if Assigned( LDM ) then
  begin
    LDM.Critical.Acquire;
    try
      LDM.QryNews.close;
      LDM.QryNews.ParamByName( 'IDNEWS' ).AsString := Request.QueryFields.Values[ 'Id' ];
      LDM.QryNews.Open;

      if not ( LDM.QryNews.Eof ) then
      begin
        LDM.QryNews.Open;
        LDM.QryNews.First;

        Response.ContentType := 'text/plain; charset=utf-8';
        Response.Content := LDM.QryNewsTEXTE.Value;
      end;
    finally
      LDM.Critical.Leave;
    end;
  end;
end;

procedure TListENewsController.InitializeActions( aWebModule: TWebModule;
  aWebStencil: TWebStencilsEngine );
begin
  inherited;

  aWebModule.AddRoutes( [
      TRoute.Create( mtGet, '/NewsList', Self.NewsList ),
      TRoute.Create( mtDelete, '/DeleteNews', Self.DeleteNews ),
      TRoute.Create( mtPost, '/GetNewsNavigation', Self.GetNavigation ),
      TRoute.Create( mtPost, '/NewsEditLineMode', Self.NewsEditLineMode ),
      TRoute.Create( mtAny, '/CancelNewsEditLine', Self.CancelNewsEditLine ),
      TRoute.Create( mtPost, '/ApplyNewsEditLine', Self.ApplyNewsEditLine ),
      TRoute.Create( mtPost, '/AddNews', Self.AddNews ),
      TRoute.Create( mtPost, '/CancelAddNews', Self.CancelAddNews ),
      TRoute.Create( mtPost, '/ApplyInsertNews', Self.ApplyInsertNews ),
      TRoute.Create( mtAny, '/SaveContent', Self.SaveContentNews ),
      TRoute.Create( mtAny, '/Show', Self.ShowNews ),
      TRoute.Create( mtAny, '/GetNews', Self.GetNews )
      ] );
end;

procedure TListENewsController.NewsEditLineMode( Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
var
  LDM: TDMSession;
begin
  LDM := GetDMSession( Request );

  if Assigned( LDM ) then
  begin
    LDM.Critical.Acquire;
    try
      LDM.QryNews.close;
      LDM.QryNews.ParamByName( 'IDNEWS' ).AsString := Request.QueryFields.Values[ 'Id' ];
      LDM.QryNews.Open;

      if not ( LDM.QryNews.Eof ) then
      begin
        LDM.QryNews.Open;
        LDM.QryNews.First;

        FWebStencilsProcessor.AddVar( 'News', LDM.QryNews, False );
        FWebStencilsProcessor.AddVar( 'Form', Self, False );

        Response.Content := RenderTemplate( TMP_LINE_EDIT, Request );
      end;
    finally
      LDM.Critical.Leave;
    end;
  end;
end;

procedure TListENewsController.NewsList( Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean );
var
  LDM: TDMSession;
  LLinesPerPage: Integer;
  LPagination: TPagination;
  LPage: Integer;
  LInt: Integer;
  LDateSearch: TDateTime;
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

    if not ( TryStrToDate( LDM.SessionVariables.Values[ SEARCH_VARIABLE ], LDateSearch ) ) then
    begin
      LDateSearch := 0;
    end;

    FFeedId := Request.ContentFields.Values[ 'FeedId' ];
    if FFeedId = '' then
    begin
      FFeedId := Request.QueryFields.Values[ 'FeedId' ];
    end;

    Logger.Info( 'Newslist, FeedId : ' + FFeedId );

    LDM.Critical.Acquire;
    try
      // Est-ce qu'on rafraichit également la barre de pagination
      if ( Request.QueryFields.Values[ 'Scope' ] = 'Page' ) then
      begin
        FTitre := Request.ContentFields.Values[ 'FeedName' ];

        LDM.SessionVariables.Values[ SEARCH_VARIABLE ] := '';

        LTemplate := TMP_LISTE;

        LDM.QryCountNews.close;
        LDM.QryCountNews.ParamByName( 'ID_FEED' ).AsInteger := FFeedId.ToInteger;
        LDM.QryCountNews.ParamByName( 'TITRE_NEWS' ).AsString := '%' + LDM.SessionVariables.Values[ SEARCH_VARIABLE ] + '%';
        LDM.QryCountNews.ParamByName( 'DATE_CREATION' ).AsDateTime := LDateSearch;
        LDM.QryCountNews.ParamByName( 'DATE_PUBLICATION' ).AsDateTime := LDateSearch;
        LDM.QryCountNews.Open;

        if not ( TryStrToInt( Request.QueryFields.Values[ 'Actual' ], LInt ) ) then
        begin
          LInt := 1;
        end;

        LPagination.GeneratePagesList( LDM.QryCountNewsNB_ENR.Value, LLinesPerPage, LInt, '', '', 'NewsList',
          'GetNewsNavigation' );

        FWebStencilsProcessor.AddVar( 'pages', LDM.Pagination( NAVIGATION_NAME ), False );
      end
      else // Sinon, on rafraichit juste la liste
      begin
        LTemplate := TMP_TABLE
      end;

      FMsg := FMsg + 'NewsList';

      LDM.QryListeNews.close;
      LDM.QryListeNews.ParamByName( 'FIRST' ).AsInteger := LLinesPerPage;
      LDM.QryListeNews.ParamByName( 'SKIP' ).AsInteger := LPage * LLinesPerPage;
      LDM.QryListeNews.ParamByName( 'ID_FEED' ).AsInteger := FFeedId.ToInteger;
      LDM.QryListeNews.ParamByName( 'TITRE_NEWS' ).AsString := '%' + LDM.SessionVariables.Values[ SEARCH_VARIABLE ] + '%';
      LDM.QryListeNews.ParamByName( 'DATE_CREATION' ).AsDateTime := LDateSearch;
      LDM.QryListeNews.ParamByName( 'DATE_PUBLICATION' ).AsDateTime := LDateSearch;
      LDM.QryListeNews.Open;

      FWebStencilsProcessor.AddVar( 'newsList', LDM.QryListeNews, False );
      FWebStencilsProcessor.AddVar( 'Form', Self, False );

      Response.Content := RenderTemplate( LTemplate, Request );
    finally
      LDM.Critical.Release;
    end;
  end;
end;

function TListENewsController.SaisieOK( aTitre, aOrdre: string; aDatePublication,
  aDatePeremption: TDateTime ): string;
begin
  Result := 'OK';

  if ( aTitre.Trim = '' ) then
  begin
    Result := 'ERR:Il faut renseigner un titre';
  end
  else if ( aDatePublication = 0 ) then
  begin
    Result := 'ERR:Il faut renseigner une date de publication';
  end
  else if ( aDatePeremption = 0 ) then
  begin
    Result := 'ERR:Il faut renseigner une date de péremption';
  end
  else if ( aDatePeremption < aDatePublication ) then
  begin
    Result := 'ERR:La date de péremption doit être postérieure à la date de publication';
  end
  else
  begin
    try
      StrToInt( aOrdre );
    except
      Result := 'ERR:l''ordre d''affichage de la news doit être un entier';
    end;
  end;
end;

procedure TListENewsController.SaveContentNews( Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
var
  LDM: TDMSession;
  JSONVal: TJSONValue;
  LObj: TJSONObject;
  ContentStr: string;
begin
  LDM := GetDMSession( Request );

  if Assigned( LDM ) then
  begin
    LDM.Critical.Acquire;
    try
      LDM.cnxFeedFlow.StartTransaction;
      try
        LDM.QryNews.close;
        LDM.QryNews.ParamByName( 'IDNEWS' ).AsString := Request.QueryFields.Values[ 'IdNews' ];
        LDM.QryNews.Open;

        if not ( LDM.QryNews.Eof ) then
        begin
          JSONVal := TJSONObject.ParseJSONValue( Request.Content );
          try
            if ( JSONVal <> nil ) and ( JSONVal is TJSONObject ) then
            begin
              LObj := TJSONObject( JSONVal );
              ContentStr := LObj.GetValue( 'content' ).Value;

              LDM.QryNews.Edit;
              LDM.QryNewsTEXTE.Value := ContentStr;
              LDM.QryNews.Post;
              LDM.QryNews.Close;
            end
            else
            begin
              Response.StatusCode := 400;
              Response.Content := '{"error":"invalid json"}';
            end;
          finally
            JSONVal.Free;
          end;
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

  Handled := True;

  Response.StatusCode := 200;
end;

procedure TListENewsController.SetFeedId( const Value: string );
begin
  FFeedId := Value;
end;

procedure TListENewsController.ShowNews( Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean );
var
  LDM: TDMSession;
  LIdFeed: Integer;
begin
  logger.Info( 'ShowNews' );
  if FileExists( TPath.Combine( FWebStencilsEngine.RootDirectory, Request.QueryFields.Values[ 'Template' ] ) ) then
  begin
    LDM := GetDMSession( Request );

    if ( Request.QueryFields.Values[ 'IdFeed' ] <> '' ) then
    begin
      if not ( TryStrToInt( Request.QueryFields.Values[ 'IdFeed' ], LIdFeed ) ) then
      begin
        LIdFeed := 0;
      end;
    end
    else
    begin
      if not ( TryStrToInt( Request.ContentFields.Values[ 'IdFeed' ], LIdFeed ) ) then
      begin
        LIdFeed := 0;
      end;
    end;

    Logger.Info( 'ShowNews, LIdFeed : ' + LIdFeed.ToString );

    if Assigned( LDM ) then
    begin
      LDM.Critical.Acquire;
      try
        LDM.QryShowNews.ParamByName( 'ID_FEED' ).AsInteger := LIdFeed;
        LDM.QryShowNews.Open;

        FWebStencilsProcessor.AddVar( 'News', LDM.QryShowNews, False );

        Response.ContentType := 'text/html; charset=UTF-8';
        Response.Content := RenderTemplate( Request.QueryFields.Values[ 'Template' ], Request );

        LDM.QryShowNews.Close;
      finally
        LDM.Critical.Release;
      end;
    end;
  end
  else
  begin
    response.Content := 'Erreur : Template non trouvé ' + Request.QueryFields.Values[ 'Template' ];
  end;
end;

initialization

  TInvokerActions.GetInvokerActions.AddAction( TListENewsController.Create );

end.

