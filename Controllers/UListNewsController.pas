unit UListNewsController;

interface

uses
  System.Classes,
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
    FTemplateName: string;

    function SaisieOK( aTitre, aOrdre: string; aDatePublication, aDatePeremption:
      TDateTime ): string;

    procedure SetFeedId( const Value: string );
    procedure SetTemplateName( const Value: string );
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
    procedure UploadTemplate( Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
    procedure GetGroup( Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
    procedure ShowGroup( Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
    procedure UploadDocument( Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
    procedure SortNews( Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );

    procedure InitializeActions( aWebModule: TWebModule; aWebStencil: TWebStencilsEngine ); override;

    property FeedId: string read FFeedId write SetFeedId;
    property TemplateName: string read FTemplateName write SetTemplateName;
  end;

implementation

uses
  System.SyncObjs,
  System.JSON,
  System.IOUtils,
  System.Generics.Collections,
  System.StrUtils,
  Web.ReqFiles,
  IdHTTP,
  FireDAC.Stan.Param,
  utils.ClassHelpers,
  UConsts,
  uInvokerActions,
  UWMMain,
  Utils.Logger,
  UPagination,
  Utils.Token;

const
  NAVIGATION_NAME: string = 'NewsList';
  SEARCH_VARIABLE: string = 'NewsList.Search';
  LINEPERPAGE_VARIABLE: string = 'LinesPerPageFeed';
  TMP_ADD: string = 'NewsAdd.html';
  TMP_LISTE: string = 'NewsList.html';
  TMP_TABLE: string = 'NewsTable.html';
  TMP_LINES: string = 'NewsLines.html';
  TMP_LINE: string = 'NewsLine.html';
  TMP_LINE_EDIT: string = 'NewsLineEdit.html';
  TMP_NAVIGATION: string = 'ListNavigation.html';
  TMP_GROUP: string = 'ShowGroupe.html';
  FILTER_CATEGORIES: string = 'FilterCategories';
  FILTER_SOUS_CATEGORIES: string = 'FilterSousCategories';
  FILTER_PAYS: string = 'FilterPays';
  FILTER_LANG: string = 'FilterLang';

  { TListENewsController }

procedure TListENewsController.AddNews( Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean );
var
  LDM: TDMSession;
  LToken: TToken;
begin
  if ValidToken( Request, True, True, LToken ) and ( LToken.Role = 'ADMIN' ) then
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
  LToken: TToken;
begin
  if ValidToken( Request, True, True, LToken ) and ( LToken.Role = 'ADMIN' ) then
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
        LDM.QryNewsNEWS_ID.Value := -1;
        LDM.QryNewsNEWS_TITLE.Value := Request.ContentFields.Values[ 'titre' ];
        LDM.QryNewsDISPLAY_ORDER.Value := LOrdre;
        LDM.QryNewsHOLD.Value := Request.ContentFields.Values[ 'status' ];
        LDM.QryNewsPUBLICATION_DATE.Value := LDatePublication;
        LDM.QryNewsEXPIRY_DATE.Value := lDatePeremption;
        LDM.QryNewsTEXT.Value := '';
        LDM.QryNewsFEED_ID.Value := FFeedId.ToInteger;
        LDM.QryNews.Post;

        LLAstId := LDM.cnxFeedFlow.GetLastAutoGenValue( 'GEN_NEWS' );

        LDM.QryNews.Close;
        LDM.QryNews.ParamByName( 'NEWS_ID' ).AsInteger := LLAstId;
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
    end;
  end;

  Handled := True;
end;

procedure TListENewsController.ApplyNewsEditLine( Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
var
  LDM: TDMSession;
  LMsg: string;
  LDatePublication, lDatePeremption: TDateTime;
  LFs: TFormatSettings;
  LOrdre: Integer;
  LToken: TToken;
begin
  if ValidToken( Request, True, True, LToken ) and ( LToken.Role = 'ADMIN' ) then
  begin
    LMsg := '';

    LDM := GetDMSession( Request );
    if Assigned( LDM ) then
    begin
      if ( Request.QueryFields.Values[ 'Id' ] <> '' ) then
      begin
        LDM.cnxFeedFlow.StartTransaction;

        LDM.QryNews.close;
        LDM.QryNews.ParamByName( 'NEWS_ID' ).AsString := Request.QueryFields.Values[ 'Id' ];
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
            LDM.QryNewsNEWS_TITLE.Value := Request.ContentFields.Values[ 'titre' ];
            LDM.QryNewsDISPLAY_ORDER.Value := LOrdre;
            LDM.QryNewsHOLD.Value := Request.ContentFields.Values[ 'status' ];
            LDM.QryNewsPUBLICATION_DATE.Value := LDatePublication;
            LDM.QryNewsEXPIRY_DATE.Value := lDatePeremption;
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
            LDM.QryNews.ParamByName( 'NEWS_ID' ).AsString := Request.QueryFields.Values[ 'Id' ];
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
      end;
    end;
  end;

  Handled := True;
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
  LToken: TToken;
begin
  if ValidToken( Request, True, True, LToken ) and ( LToken.Role = 'ADMIN' ) then
  begin
    LDM := GetDMSession( Request );
    if Assigned( LDM ) then
    begin
      LDM.Critical.Acquire;
      try
        LDM.QryNews.close;
        LDM.QryNews.ParamByName( 'NEWS_ID' ).AsString := Request.QueryFields.Values[ 'Id' ];
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
end;

procedure TListENewsController.DeleteNews( Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean );
var
  LDM: TDMSession;
  LToken: TToken;
begin
  if ValidToken( Request, True, True, LToken ) and ( LToken.Role = 'ADMIN' ) then
  begin
    LDM := GetDMSession( Request );
    if Assigned( LDM ) then
    begin
      LDM.QryNews.close;
      LDM.QryNews.ParamByName( 'NEWS_ID' ).AsString := Request.QueryFields.Values[ 'Id' ];
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
end;

procedure TListENewsController.GetGroup( Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean );
var
  LDM: TDMSession;
  LToken: TToken;
begin
  if ValidToken( Request, True, True, LToken ) then
  begin
    LDM := GetDMSession( Request );
    if Assigned( LDM ) then
    begin
      LDM.Critical.Acquire;
      try
//        LDM.QryListeGroup.close;
//        LDM.QryListeGroup.ParamByName( 'GROUPE' ).AsString := Request.QueryFields.Values[ 'IdGroupe' ];
//        LDM.QryListeGroup.Open;

        LDM.QryFeedsUser.ParamByName( 'FEED_GROUP' ).AsString := Request.QueryFields.Values[ 'IdGroupe' ];
        LDM.QryFeedsUser.ParamByName( 'COUNTRY_CODE' ).AsString := LToken.Country;
        LDM.QryFeedsUser.ParamByName( 'LANGUAGE_CODE' ).AsString := LToken.Lang;
        LDM.QryFeedsUser.ParamByName( 'CATEGORY_ID' ).AsInteger := LToken.Category.ToInteger;
        LDM.QryFeedsUser.ParamByName( 'SUBCATEGORY_ID' ).AsInteger := LToken.SubCatgegory.ToInteger;
        LDM.QryFeedsUser.Open;

        FWebStencilsProcessor.AddVar( 'Form', Self, False );
        FWebStencilsProcessor.AddVar( 'Group', LDM.QryFeedsUser, False );

        Response.Content := RenderTemplate( TMP_GROUP, Request );

        LDM.QryFeedsUser.Close;
      finally
        LDM.Critical.Release;
      end;
    end;
  end;

  Handled := True;
end;

procedure TListENewsController.GetNavigation( Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
var
  LPagination: TPagination;
  LInt: Integer;
  LLinesPerPage: Integer;
  LDM: TDMSession;
  LDateSearch: TDateTime;
  LToken: TToken;
  LWhereClause: string;
  LSelCategorie, LSelSousCategorie, LSelPays, LSelLangue: string;
begin
  if ValidToken( Request, True, True, LToken ) and ( LToken.Role = 'ADMIN' ) then
  begin
    LDM := GetDMSession( Request );

    if Assigned( LDM ) then
    begin
      LSelCategorie := Request.ContentFields.Values[ 'FilterCategorie' ];
      LSelSousCategorie := Request.ContentFields.Values[ 'FilterSousCategorie' ];
      LSelPays := Request.ContentFields.Values[ 'FilterPays' ];
      LSelLangue := Request.ContentFields.Values[ 'FilterLangue' ];

      LDM.SessionVariables.Values[ FILTER_CATEGORIES ] := LSelCategorie;
      LDM.SessionVariables.Values[ FILTER_SOUS_CATEGORIES ] := LSelSousCategorie;
      LDM.SessionVariables.Values[ FILTER_PAYS ] := LSelPays;
      LDM.SessionVariables.Values[ FILTER_LANG ] := LSelLangue;

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
        LWhereClause := '';

        if LSelCategorie <> '' then
        begin
          LWhereClause := LWhereClause + ' and NEWS_ID in (select NEWS_ID from NEWS_CONTEXT_CATEGORY where CATEGORY_ID = ' +
            LSelCategorie + ')';
        end;

        if LSelSousCategorie <> '' then
        begin
          LWhereClause := LWhereClause +
            ' and NEWS_ID in (select NEWS_ID from NEWS_CONTEXT_SUBCATEGORY where SUBCATEGORY_ID = ' +
            LSelSousCategorie + ')';
        end;

        if LSelPays <> '' then
        begin
          LWhereClause := LWhereClause + ' and NEWS_ID in (select NEWS_ID from NEWS_CONTEXT_COUNTRY where COUNTRY_CODE = ' +
            LSelPays.QuotedString + ')';
        end;

        if LSelLangue <> '' then
        begin
          LWhereClause := LWhereClause + ' and NEWS_ID in (select NEWS_ID from NEWS_CONTEXT_LANG where LANGUAGE_CODE = ' +
            LSelLangue.QuotedString + ')';
        end;

        LDM.QryCountNews.close;
        LDM.QryCountNews.SQL.Text := QRY_COUNT_NEWS +
          LWhereClause;
        LDM.QryCountNews.ParamByName( 'FEED_ID' ).AsInteger := FFeedId.ToInteger;
        LDM.QryCountNews.ParamByName( 'NEWS_TITLE' ).AsString := '%' + LDM.SessionVariables.Values[ SEARCH_VARIABLE ] + '%';
        LDM.QryCountNews.ParamByName( 'CREATION_DATE' ).AsDateTime := LDateSearch;
        LDM.QryCountNews.ParamByName( 'PUBLICATION_DATE' ).AsDateTime := LDateSearch;
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
    end;
  end;

  Handled := True;
end;

procedure TListENewsController.GetNews( Sender: TObject; Request: TWebRequest;
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
        LDM.QryNews.close;
        LDM.QryNews.ParamByName( 'NEWS_ID' ).AsString := Request.QueryFields.Values[ 'Id' ];
        LDM.QryNews.Open;

        if not ( LDM.QryNews.Eof ) then
        begin
          LDM.QryNews.Open;
          LDM.QryNews.First;

          LJson := TJSONObject.Create;
          try
            LJson.AddPair( 'content', LDM.QryNewsTEXT.Value );

            // On envoi les catégories
            LArray := TJSONArray.Create;
            LDM.QryNewsCategories.ParamByName( 'NEWS_ID' ).AsString := Request.QueryFields.Values[ 'Id' ];
            LDM.QryNewsCategories.Open;

            while not ( LDM.QryNewsCategories.Eof ) do
            begin
              LArray.Add( LDM.QryNewsCategoriesCATEGORY_ID.Value );

              LDM.QryNewsCategories.Next;
            end;

            LDM.QryNewsCategories.Close;

            LJson.AddPair( 'BU', LArray );

            // On envoi les sous-catégories
            LArray := TJSONArray.Create;
            LDM.QryNewsSubCategory.ParamByName( 'NEWS_ID' ).AsString := Request.QueryFields.Values[ 'Id' ];
            LDM.QryNewsSubCategory.Open;

            while not ( LDM.QryNewsSubCategory.Eof ) do
            begin
              LArray.Add( LDM.QryNewsSubCategorySUBCATEGORY_ID.Value );

              LDM.QryNewsSubCategory.Next;
            end;

            LDM.QryNewsSubCategory.Close;

            LJson.AddPair( 'TypePartner', LArray );

            // On envoi les pays
            LArray := TJSONArray.Create;
            LDM.QryNewsCountry.ParamByName( 'NEWS_ID' ).AsString := Request.QueryFields.Values[ 'Id' ];
            LDM.QryNewsCountry.Open;

            while not ( LDM.QryNewsCountry.Eof ) do
            begin
              LArray.Add( LDM.QryNewsCountryCOUNTRY_CODE.Value );

              LDM.QryNewsCountry.Next;
            end;

            LDM.QryNewsCountry.Close;

            LJson.AddPair( 'Country', LArray );

            // On envoi les langues
            LArray := TJSONArray.Create;
            LDM.QryNewsLanguage.ParamByName( 'NEWS_ID' ).AsString := Request.QueryFields.Values[ 'Id' ];
            LDM.QryNewsLanguage.Open;

            while not ( LDM.QryNewsLanguage.Eof ) do
            begin
              LArray.Add( LDM.QryNewsLanguageLANGUAGE_CODE.Value );

              LDM.QryNewsLanguage.Next;
            end;

            LDM.QryNewsLanguage.Close;

            LJson.AddPair( 'Lang', LArray );

            Response.ContentType := 'application/json; charset=utf-8';
            Response.Content := LJson.ToJSON;
          finally
            LJson.Free;
          end;
        end;
      finally
        LDM.Critical.Leave;
      end;
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
      TRoute.Create( mtAny, '/GetNews', Self.GetNews ),
      TRoute.Create( mtPost, '/UploadTemplate', Self.UploadTemplate ),
      TRoute.Create( mtPost, '/GetGroup', Self.GetGroup ),
      TRoute.Create( mtGet, '/ShowGroup', Self.ShowGroup ),
      TRoute.Create( mtPost, '/UploadDocument', Self.UploadDocument ),
      TRoute.Create( mtGet, '/SortNews', Self.SortNews )
      ] );
end;

procedure TListENewsController.NewsEditLineMode( Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
var
  LDM: TDMSession;
  LToken: TToken;
begin
  if ValidToken( Request, True, True, LToken ) and ( LToken.Role = 'ADMIN' ) then
  begin
    LDM := GetDMSession( Request );

    if Assigned( LDM ) then
    begin
      LDM.Critical.Acquire;
      try
        LDM.QryNews.close;
        LDM.QryNews.ParamByName( 'NEWS_ID' ).AsString := Request.QueryFields.Values[ 'Id' ];
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
  LToken: TToken;
  LSelCategorie, LSelSousCategorie, LSelPays, LSelLangue: string;
  LWhereClause: string;
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
          LDM.SessionVariables.Values[ FILTER_CATEGORIES ] := '';
          LDM.SessionVariables.Values[ FILTER_SOUS_CATEGORIES ] := '';
          LDM.SessionVariables.Values[ FILTER_PAYS ] := '';
          LDM.SessionVariables.Values[ FILTER_LANG ] := '';

          LTemplate := TMP_LISTE;

          if LDM.SessionVariables.Values[ 'SortNewsField' ] = '' then
          begin
            LDM.SessionVariables.Values[ 'SortNewsField' ] := 'CREATION_DATE';
            LDM.SessionVariables.Values[ 'SortNewsOrd' ] := 'desc';
          end;

          LDM.QryCountNews.close;
          LDM.QryCountNews.ParamByName( 'FEED_ID' ).AsInteger := FFeedId.ToInteger;
          LDM.QryCountNews.ParamByName( 'NEWS_TITLE' ).AsString := '%' + LDM.SessionVariables.Values[ SEARCH_VARIABLE ] + '%';
          LDM.QryCountNews.ParamByName( 'CREATION_DATE' ).AsDateTime := LDateSearch;
          LDM.QryCountNews.ParamByName( 'PUBLICATION_DATE' ).AsDateTime := LDateSearch;
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
          LTemplate := TMP_LINES; // TMP_TABLE
        end;

        LSelCategorie := LDM.SessionVariables.Values[ FILTER_CATEGORIES ];
        LSelSousCategorie := LDM.SessionVariables.Values[ FILTER_SOUS_CATEGORIES ];
        LSelPays := LDM.SessionVariables.Values[ FILTER_PAYS ];
        LSelLangue := LDM.SessionVariables.Values[ FILTER_LANG ];

        FMsg := FMsg + 'NewsList';

        LWhereClause := '';

        if LSelCategorie <> '' then
        begin
          LWhereClause := LWhereClause + ' and NEWS_ID in (select NEWS_ID from NEWS_CONTEXT_CATEGORY where CATEGORY_ID = ' +
            LSelCategorie + ')';
        end;

        if LSelSousCategorie <> '' then
        begin
          LWhereClause := LWhereClause +
            ' and NEWS_ID in (select NEWS_ID from NEWS_CONTEXT_SUBCATEGORY where SUBCATEGORY_ID = ' +
            LSelSousCategorie + ')';
        end;

        if LSelPays <> '' then
        begin
          LWhereClause := LWhereClause + ' and NEWS_ID in (select NEWS_ID from NEWS_CONTEXT_COUNTRY where COUNTRY_CODE = ' +
            LSelPays.QuotedString + ')';
        end;

        if LSelLangue <> '' then
        begin
          LWhereClause := LWhereClause + ' and NEWS_ID in (select NEWS_ID from NEWS_CONTEXT_LANG where LANGUAGE_CODE = ' +
            LSelLangue.QuotedString + ')';
        end;

        LDM.QryListNews.close;
        LDM.QryListNews.SQL.Text := QRY_LIST_NEWS +
          LWhereClause +
          ' order by ' + LDM.SessionVariables.Values[ 'SortNewsField' ] + ' ' + LDM.SessionVariables.Values[ 'SortNewsOrd' ];
        LDM.QryListNews.ParamByName( 'FIRST' ).AsInteger := LLinesPerPage;
        LDM.QryListNews.ParamByName( 'SKIP' ).AsInteger := LPage * LLinesPerPage;
        LDM.QryListNews.ParamByName( 'FEED_ID' ).AsInteger := FFeedId.ToInteger;
        LDM.QryListNews.ParamByName( 'NEWS_TITLE' ).AsString := '%' + LDM.SessionVariables.Values[ SEARCH_VARIABLE ] + '%';
        LDM.QryListNews.ParamByName( 'CREATION_DATE' ).AsDateTime := LDateSearch;
        LDM.QryListNews.ParamByName( 'PUBLICATION_DATE' ).AsDateTime := LDateSearch;
        LDM.QryListNews.Open;

        LDM.qryFeeds.close;
        LDM.qryFeeds.ParamByName( 'FEED_ID' ).AsInteger := FFeedId.ToInteger;
        LDM.qryFeeds.Open;

        FTemplateName := LDM.qryFeedsDISPLAY_TEMPLATE.Value;

        FWebStencilsProcessor.AddVar( 'newsList', LDM.QryListNews, False );
        FWebStencilsProcessor.AddVar( 'Categories', LDM.QryListCategories, False );
        FWebStencilsProcessor.AddVar( 'SubCategories', LDM.QryListSubCategories, False );
        FWebStencilsProcessor.AddVar( 'Country', LDM.QryListCountries, False );
        FWebStencilsProcessor.AddVar( 'Languages', LDM.QryListLanguages, False );
        FWebStencilsProcessor.AddVar( 'Form', Self, False );

        Response.Content := RenderTemplate( LTemplate, Request );
      finally
        LDM.Critical.Release;
      end;
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
  //  JSONVal: TJSONValue;
  //  LObj: TJSONObject;
  //  ContentStr: string;
  LToken: TToken;
  LJsonObj: TJSONObject;
  LContent: string;
  //  LValue: TJSONValue;
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
            LContent := LJsonObj.GetValue<string>( 'content' );

            LArrayCategorie := LJsonObj.GetValue<TJSONArray>( 'BU' );
            LArraySousCategorie := LJsonObj.GetValue<TJSONArray>( 'TypePartner' );
            LArrayPays := LJsonObj.GetValue<TJSONArray>( 'Country' );
            LArrayLangue := LJsonObj.GetValue<TJSONArray>( 'Lang' );

            LDM.QryNews.close;
            LDM.QryNews.ParamByName( 'NEWS_ID' ).AsString := Request.QueryFields.Values[ 'IdNews' ];
            LDM.QryNews.Open;

            if not ( LDM.QryNews.Eof ) then
            begin
              LDM.QryNews.Edit;
              LDM.QryNewsTEXT.Value := LContent;

              LDM.QryNews.Post;
              LDM.QryNews.Close;
            end;

            // on Sauvegarde le lien avec les catégories
            LDM.QryNewsCategories.ParamByName( 'NEWS_ID' ).AsString := Request.QueryFields.Values[ 'IdNews' ];
            LDM.QryNewsCategories.Open;
            while not ( LDM.QryNewsCategories.Eof ) do
            begin
              LDM.QryNewsCategories.Delete;
            end;

            for var i := 0 to LArrayCategorie.Count - 1 do
            begin
              LDM.QryNewsCategories.Append;
              LDM.QryNewsCategoriesCATEGORY_ID.Value := StrToInt( LArrayCategorie.Items[ i ].Value );
              LDM.QryNewsCategoriesNEWS_ID.Value := StrToInt( Request.QueryFields.Values[ 'IdNews' ] );
              LDM.QryNewsCategories.Post;
            end;

            LDM.QryNewsCategories.Close;

            // on Sauvegarde le lien avec les sous-catégories
            LDM.QryNewsSubCategory.ParamByName( 'NEWS_ID' ).AsString := Request.QueryFields.Values[ 'IdNews' ];
            LDM.QryNewsSubCategory.Open;
            while not ( LDM.QryNewsSubCategory.Eof ) do
            begin
              LDM.QryNewsSubCategory.Delete;
            end;

            for var i := 0 to LArraySousCategorie.Count - 1 do
            begin
              LDM.QryNewsSubCategory.Append;
              LDM.QryNewsSubCategorySUBCATEGORY_ID.Value := StrToInt( LArraySousCategorie.Items[ i ].Value );
              LDM.QryNewsSubCategoryNEWS_ID.Value := StrToInt( Request.QueryFields.Values[ 'IdNews' ] );
              LDM.QryNewsSubCategory.Post;
            end;

            LDM.QryNewsSubCategory.Close;

            // on Sauvegarde le lien avec les pays
            LDM.QryNewsCountry.ParamByName( 'NEWS_ID' ).AsString := Request.QueryFields.Values[ 'IdNews' ];
            LDM.QryNewsCountry.Open;
            while not ( LDM.QryNewsCountry.Eof ) do
            begin
              LDM.QryNewsCountry.Delete;
            end;

            for var i := 0 to LArrayPays.Count - 1 do
            begin
              LDM.QryNewsCountry.Append;
              LDM.QryNewsCountryCOUNTRY_CODE.Value := LArrayPays.Items[ i ].Value;
              LDM.QryNewsCountryNEWS_ID.Value := StrToInt( Request.QueryFields.Values[ 'IdNews' ] );
              LDM.QryNewsCountry.Post;
            end;

            LDM.QryNewsCountry.Close;

            // on Sauvegarde le lien avec les langues
            LDM.QryNewsLanguage.ParamByName( 'NEWS_ID' ).AsString := Request.QueryFields.Values[ 'IdNews' ];
            LDM.QryNewsLanguage.Open;
            while not ( LDM.QryNewsLanguage.Eof ) do
            begin
              LDM.QryNewsLanguage.Delete;
            end;

            for var i := 0 to LArrayLangue.Count - 1 do
            begin
              LDM.QryNewsLanguage.Append;
              LDM.QryNewsLanguageLANGUAGE_CODE.Value := LArrayLangue.Items[ i ].Value;
              LDM.QryNewsLanguageNEWS_ID.Value := StrToInt( Request.QueryFields.Values[ 'IdNews' ] );
              LDM.QryNewsLanguage.Post;
            end;

            LDM.QryNewsLanguage.Close;
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

procedure TListENewsController.SetFeedId( const Value: string );
begin
  FFeedId := Value;
end;

procedure TListENewsController.SetTemplateName( const Value: string );
begin
  FTemplateName := Value;
end;

procedure TListENewsController.ShowGroup( Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean );
var
  LDM: TDMSession;
  LToken: TToken;
begin
  if ValidToken( Request, True, True, LToken ) then
  begin
    LDM := GetDMSession( Request );
    if Assigned( LDM ) then
    begin
      LDM.QryShowGroup.close;
      LDM.QryShowGroup.ParamByName( 'FEED_ID' ).AsString := Request.QueryFields.Values[ 'contact-choice' ];
      LDM.QryShowGroup.Open;

      Response.ContentType := 'text/html';
      Response.Content := '<div class="slide-content"><p>' +
        LDM.QryShowGroupTEXT.Value +
        '</p></div>';
    end;
  end;

  Handled := True;
end;

procedure TListENewsController.ShowNews( Sender: TObject; Request: TWebRequest;
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

        LDM.qryFeeds.close;
        LDM.qryFeeds.ParamByName( 'FEED_ID' ).AsInteger := LIdFeed;
        LDM.qryFeeds.Open;

        if FileExists( TPath.Combine( FWebStencilsEngine.RootDirectory, LDM.qryFeedsDISPLAY_TEMPLATE.Value ) ) then
        begin
          Logger.Info( 'ShowNews, LIdFeed : ' + LIdFeed.ToString );

          LDM.QryShowNews.ParamByName( 'FEED_ID' ).AsInteger := LIdFeed;
          LDM.QryShowNews.Open;

          FWebStencilsProcessor.AddVar( 'News', LDM.QryShowNews, False );

          Response.ContentType := 'text/html; charset=UTF-8';
          Response.Content := RenderTemplate( LDM.qryFeedsDISPLAY_TEMPLATE.Value, Request );

          LDM.QryShowNews.Close;
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

procedure TListENewsController.SortNews( Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean );
var
  LLinesPerPage: Integer;
  LToken: TToken;
  LDM: TDMSession;
  LPagination: TPagination;
  LPage: Integer;
  LDateSearch: TDateTime;
begin
  if ValidToken( Request, False, True, LToken ) and ( LToken.Role = 'ADMIN' ) then
  begin
    LDM := GetDMSession( Request );

    if Assigned( LDM ) then
    begin
      LDM.SessionVariables.Values[ 'SortNewsField' ] := Request.QueryFields.Values[ 'col' ];
      LDM.SessionVariables.Values[ 'SortNewsOrd' ] := Request.QueryFields.Values[ 'dir' ];

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

      LDM.QryListNews.close;
      LDM.QryListNews.SQL.Text := QRY_LIST_NEWS +
        ' order by ' + LDM.SessionVariables.Values[ 'SortNewsField' ] + ' ' + LDM.SessionVariables.Values[ 'SortNewsOrd' ];
      LDM.QryListNews.ParamByName( 'FIRST' ).AsInteger := LLinesPerPage;
      LDM.QryListNews.ParamByName( 'SKIP' ).AsInteger := LPage * LLinesPerPage;
      LDM.QryListNews.ParamByName( 'FEED_ID' ).AsInteger := FFeedId.ToInteger;
      LDM.QryListNews.ParamByName( 'NEWS_TITLE' ).AsString := '%' + LDM.SessionVariables.Values[ SEARCH_VARIABLE ] + '%';
      LDM.QryListNews.ParamByName( 'CREATION_DATE' ).AsDateTime := LDateSearch;
      LDM.QryListNews.ParamByName( 'PUBLICATION_DATE' ).AsDateTime := LDateSearch;
      LDM.QryListNews.Open;

      FWebStencilsProcessor.AddVar( 'newsList', LDM.QryListNews, False );
      FWebStencilsProcessor.AddVar( 'Categories', LDM.QryListCategories, False );
      FWebStencilsProcessor.AddVar( 'SousCategories', LDM.QryListSubCategories, False );
      FWebStencilsProcessor.AddVar( 'Pays', LDM.QryListCountries, False );
      FWebStencilsProcessor.AddVar( 'Langues', LDM.QryListLanguages, False );
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

procedure TListENewsController.UploadDocument( Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
var
  LSavePath: string;
  LMemoryStream: TMemoryStream;
  LFile: TAbstractWebRequestFile;
  LDM: TDMSession;
  //  LIdFeed: Integer;
  LToken: TToken;
begin
  if ValidToken( Request, True, True, LToken ) and ( LToken.Role = 'ADMIN' ) then
  begin
    Logger.Info( 'UploadDocument' );

    LDM := GetDMSession( Request );

    if Assigned( LDM ) then
    begin
      if Request.Files.Count > 0 then
      begin
        //TODO: Paramétrer le chemin de sauvegarde
        LSavePath := TPath.Combine( ExtractFilePath( ParamStr( 0 ) ), 'Files', Request.Files[ 0 ].FileName );

        Logger.Info( 'LSavePAth : ' + LSavePath );

        LMemoryStream := TMemoryStream.Create;
        try
          LFile := Request.Files[ 0 ];
          LFile.Stream.Position := 0;
          Logger.Info( 'Copie du stream' );
          LMemoryStream.CopyFrom( LFile.Stream, LFile.Stream.Size );
          Logger.Info( 'Sauvegarde du fichier' );
          LMemoryStream.SaveToFile( LSavePath );
          Logger.Info( 'Sauvegardé' );
          FreeAndNil( LMemoryStream );
        except
          on e: Exception do
          begin
            Logger.Info( e.Message );
            FreeAndNil( LMemoryStream );
          end;
        end;

        Response.ContentType := 'application/json';
        //TODO: paramétrer l'url de base
        Response.Content := '{"status":"success","url":"http://localhost:8080/' + Request.Files[ 0 ].FileName + '"}';
      end
      else
      begin
        Response.StatusCode := 400;
        Response.Content := '{"status":"error","message":"No file uploaded"}';
      end;
    end;
  end;

  Handled := True;
end;

procedure TListENewsController.UploadTemplate( Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
var
  LSavePath: string;
  LMemoryStream: TMemoryStream;
  LFile: TAbstractWebRequestFile;
  LDM: TDMSession;
  LIdFeed: Integer;
  LToken: TToken;
begin
  if ValidToken( Request, True, True, LToken ) and ( LToken.Role = 'ADMIN' ) then
  begin
    Logger.Info( 'UploadTemplate' );

    LDM := GetDMSession( Request );

    if Assigned( LDM ) and TryStrToInt( Request.QueryFields.Values[ 'FeedId' ], LIdFeed ) then
    begin
      if Request.Files.Count > 0 then
      begin
        LSavePath := TPath.Combine( FWebStencilsEngine.RootDirectory, Request.Files[ 0 ].FileName );

        Logger.Info( 'LSavePAth : ' + LSavePath );

        LMemoryStream := TMemoryStream.Create;
        try
          LFile := Request.Files[ 0 ];
          LFile.Stream.Position := 0;
          Logger.Info( 'Copie du stream' );
          LMemoryStream.CopyFrom( LFile.Stream, LFile.Stream.Size );
          Logger.Info( 'Sauvegarde du fichier' );
          LMemoryStream.SaveToFile( LSavePath );
          Logger.Info( 'Sauvegardé' );
          FreeAndNil( LMemoryStream );
        except
          on e: Exception do
          begin
            Logger.Info( e.Message );
            FreeAndNil( LMemoryStream );
          end;
        end;

        LDM.qryFeeds.close;
        LDM.qryFeeds.ParamByName( 'FEED_ID' ).AsInteger := LIdFeed;
        LDM.qryFeeds.Open;

        LDM.qryFeeds.Edit;
        LDM.qryFeedsDISPLAY_TEMPLATE.Value := Request.Files[ 0 ].FileName;
        LDM.qryFeeds.Post;

        LDM.qryFeeds.Close;

        Response.ContentType := 'application/json';
        Response.Content := '{"status":"success","file":"' + Request.Files[ 0 ].FileName + '"}';
      end
      else
      begin
        Response.StatusCode := 400;
        Response.Content := '{"status":"error","message":"No file uploaded"}';
      end;
    end;
  end;

  Handled := True;
end;

initialization

  TInvokerActions.GetInvokerActions.AddAction( TListENewsController.Create );

end.

