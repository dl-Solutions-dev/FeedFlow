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
  File last update : 2026-01-04T14:37:08.648+01:00
  Signature : 615e0dc6c568b989f61c4281ff51ca603b0e9a33
  ***************************************************************************
*)

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
  UWMMain,
  Utils.Logger,
  UPagination,
  Utils.Token,
  Utils.Config,
  UFeeds,
  UNews,
  UCategories,
  USubcategories,
  UCountries,
  ULanguages,
  UControllersRegistry;

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
  LNews: TNews;
  LFeedId: Integer;
begin
  if ValidToken( Request, True, True, LToken ) and ( LToken.Role = 'ADMIN' ) then
  begin
    LDM := GetDMSession( Request );

    if Assigned( LDM ) then
    begin
      LNews := TNews( GetSessionObject( Request, 'qryNews' ) );
      if not ( Assigned( LNews ) ) then
      begin
        LNews := TNews.Create;
        AddSessionObject( Request, 'qryNews', LNews );
      end;

      if not ( TryStrToInt( Request.ContentFields.Values[ 'FeedId' ], LFeedId ) ) then
      begin
        LFeedId := -1;
      end;

      FFeedId := LFeedId.ToString;
      FWebStencilsProcessor.AddVar( 'Form', Self, False );
      FWebStencilsProcessor.AddVar( 'News', LNews.GetNews( LDM.cnxFeedFlow, -1 ), False );

      Response.Content := RenderTemplate( TMP_ADD, Request );
    end
    else
    begin
      Response.Content := 'Invalid session';
    end;

    FreeAndNil( LToken );
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
  LNews: TNews;
  LFeedId: Integer;
begin
  if ValidToken( Request, True, True, LToken ) and ( LToken.Role = 'ADMIN' ) then
  begin
    LDM := GetDMSession( Request );

    if Assigned( LDM ) then
    begin
      LNews := TNews( GetSessionObject( Request, 'qryNews' ) );
      if not ( Assigned( LNews ) ) then
      begin
        LNews := TNews.Create;
        AddSessionObject( Request, 'qryNews', LNews );
      end;

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

      if not ( TryStrToInt( Request.ContentFields.Values[ 'FeedId' ], LFeedId ) ) then
      begin
        LFeedId := -1;
      end;

      LMsg := SaisieOK( Request.ContentFields.Values[ 'titre' ], Request.ContentFields.Values[ 'ordreaffichage' ],
        LDatePublication, lDatePeremption );

      if ( LMsg = 'OK' ) then
      begin
        if not ( TryStrToInt( Request.ContentFields.Values[ 'ordreaffichage' ], LOrdre ) ) then
        begin
          LOrdre := 0;
        end;

        LNews.NewsId := -1;
        LNews.NewsTitle := Request.ContentFields.Values[ 'titre' ];
        LNews.DisplayOrder := LOrdre;
        LNews.Hold := Request.ContentFields.Values[ 'status' ];
        LNews.PublicationDate := LDatePublication;
        LNews.ExpiryDate := lDatePeremption;
        LNews.Text := '';
        LNews.FeedId := LFeedId;

        LLAstId := LNews.CreateNews( LDM.cnxFeedFlow, LMsg );

        //        LDM.QryNews.Open;
        //        LDM.QryNews.Append;
        //        LDM.QryNewsNEWS_ID.Value := -1;
        //        LDM.QryNewsNEWS_TITLE.Value := Request.ContentFields.Values[ 'titre' ];
        //        LDM.QryNewsDISPLAY_ORDER.Value := LOrdre;
        //        LDM.QryNewsHOLD.Value := Request.ContentFields.Values[ 'status' ];
        //        LDM.QryNewsPUBLICATION_DATE.Value := LDatePublication;
        //        LDM.QryNewsEXPIRY_DATE.Value := lDatePeremption;
        //        LDM.QryNewsTEXT.Value := '';
        //        LDM.QryNewsFEED_ID.Value := FFeedId.ToInteger;
        //        LDM.QryNews.Post;
        //
        //        LLAstId := LDM.cnxFeedFlow.GetLastAutoGenValue( 'GEN_NEWS' );
        //
        //        LDM.QryNews.Close;
        //        LDM.QryNews.ParamByName( 'NEWS_ID' ).AsInteger := LLAstId;
        //        LDM.QryNews.Open;

        if ( LLastId <> -1 ) then
        begin
          FFeedId := LFeedId.ToString;
          FWebStencilsProcessor.AddVar( 'Form', Self, False );
          FWebStencilsProcessor.AddVar( 'News', LNews.GetNews( LDM.cnxFeedFlow, LLAstId ), False );
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

    FreeAndNil( LToken );
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
  LOrdre, LIdNews: Integer;
  LToken: TToken;
  LNews: TNews;
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

        LNews := TNews( GetSessionObject( Request, 'qryNews' ) );
        if not ( Assigned( LNews ) ) then
        begin
          LNews := TNews.Create;
          AddSessionObject( Request, 'qryNews', LNews );
        end;

        //        LDM.QryNews.close;
        //        LDM.QryNews.ParamByName( 'NEWS_ID' ).AsString := Request.QueryFields.Values[ 'Id' ];
        //        LDM.QryNews.Open;
        //
        //        if not ( LDM.QryNews.Eof ) then
        //        begin
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

          LIdNEws := StrToInt( Request.QueryFields.Values[ 'Id' ] );

          LNews.NewsId := LIdNews;
          LNews.NewsTitle := Request.ContentFields.Values[ 'titre' ];
          LNews.DisplayOrder := LOrdre;
          LNews.Hold := Request.ContentFields.Values[ 'status' ];
          LNews.PublicationDate := LDatePublication;
          LNews.ExpiryDate := lDatePeremption;
          LMsg := LNews.UpdateNews( LDM.cnxFeedFlow );

          FFeedId := LNews.FeedId.ToString;

          //            LDM.QryNews.Edit;
          //
          //            //        LDM.QryFeedsID_FEED.Value := Request.ContentFields.Values[ 'idFeed' ].ToInteger;
          //            LDM.QryNewsNEWS_TITLE.Value := Request.ContentFields.Values[ 'titre' ];
          //            LDM.QryNewsDISPLAY_ORDER.Value := LOrdre;
          //            LDM.QryNewsHOLD.Value := Request.ContentFields.Values[ 'status' ];
          //            LDM.QryNewsPUBLICATION_DATE.Value := LDatePublication;
          //            LDM.QryNewsEXPIRY_DATE.Value := lDatePeremption;
          //            try
          //              LDM.QryNews.Post;
          //              LDM.cnxFeedFlow.Commit;
          //            except
          //              on e: Exception do
          //              begin
          //                LMsg := 'ERR:' + Request.QueryFields.Text;
          //                LDM.cnxFeedFlow.Rollback;
          //              end;
          //            end;
          //
          //            LDM.QryNews.close;
          //            LDM.QryNews.ParamByName( 'NEWS_ID' ).AsString := Request.QueryFields.Values[ 'Id' ];
          //            LDM.QryNews.Open;

          if LMsg = 'OK' then
          begin
            FWebStencilsProcessor.AddVar( 'News', LNews.GetNews( LDM.cnxFeedFlow, LIdNews ), False );
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

    FreeAndNil( LToken );
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
  LNews: TNews;
begin
  if ValidToken( Request, True, True, LToken ) and ( LToken.Role = 'ADMIN' ) then
  begin
    LDM := GetDMSession( Request );
    if Assigned( LDM ) then
    begin
      LDM.Critical.Acquire;
      try
        LNews := TNews( GetSessionObject( Request, 'qryNews' ) );
        if not ( Assigned( LNews ) ) then
        begin
          LNews := TNews.Create;
          AddSessionObject( Request, 'qryNews', LNews );
        end;

        //        LDM.QryNews.close;
        //        LDM.QryNews.ParamByName( 'NEWS_ID' ).AsString := Request.QueryFields.Values[ 'Id' ];
        //        LDM.QryNews.Open;
        //
        //        if not ( LDM.QryNews.Eof ) then
        //        begin
        FWebStencilsProcessor.AddVar( 'News', LNews.GetNews( LDM.cnxFeedFlow, Request.QueryFields.Values[ 'Id' ].ToInteger ),
          False );
        FFeedId := LNews.FeedId.ToString;
        FWebStencilsProcessor.AddVar( 'Form', Self, False );

        Response.Content := RenderTemplate( TMP_LINE, Request );
        //        end;
      finally
        LDM.Critical.Leave;
      end;
    end;

    FreeAndNil( LToken );
  end;
end;

procedure TListENewsController.DeleteNews( Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean );
var
  LDM: TDMSession;
  LToken: TToken;
  LNews: TNews;
  LMsg: string;
begin
  if ValidToken( Request, True, True, LToken ) and ( LToken.Role = 'ADMIN' ) then
  begin
    LDM := GetDMSession( Request );
    if Assigned( LDM ) then
    begin
      LNews := TNews( GetSessionObject( Request, 'qryNews' ) );
      if not ( Assigned( LNews ) ) then
      begin
        LNews := TNews.Create;
        AddSessionObject( Request, 'qryNews', LNews );
      end;

      LMsg := LNews.DeleteNews( LDM.cnxFeedFlow, Request.QueryFields.Values[ 'Id' ].ToInteger );

      if ( LMsg = 'OK' ) then
      begin
        SendEmptyContent( Response );
      end
      else
      begin
        Response.Content := 'ERR:' + LMsg;
      end;

      //      LDM.QryNews.close;
      //      LDM.QryNews.ParamByName( 'NEWS_ID' ).AsString := Request.QueryFields.Values[ 'Id' ];
      //      LDM.QryNews.Open;
      //
      //      if not ( LDM.QryNews.Eof ) then
      //      begin
      //        LDM.QryNews.Delete;
      //        SendEmptyContent( Response );
      //      end
      //      else
      //      begin
      //        Response.Content := 'liste non trouvée.';
      //      end;
    end;

    FreeAndNil( LToken );
  end;
end;

procedure TListENewsController.GetGroup( Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean );
var
  LDM: TDMSession;
  LToken: TToken;
  LFeedsUser: TFeedsUser;
  LFeedGroup: Integer;
begin
  if ValidToken( Request, True, True, LToken ) then
  begin
    LDM := GetDMSession( Request );
    if Assigned( LDM ) then
    begin
      LDM.Critical.Acquire;
      try
        LFeedsUser := TFeedsUser( GetSessionObject( Request, 'QryFeedsUser' ) );
        if not ( Assigned( LFeedsUser ) ) then
        begin
          LFeedsUser := TFeedsUser.Create;
          AddSessionObject( Request, 'QryFeedsUser', LFeedsUser );
        end;

        if not ( TryStrToInt( Request.QueryFields.Values[ 'IdGroupe' ], LFeedGroup ) ) then
        begin
          LFeedGroup := -1;
        end;

        FWebStencilsProcessor.AddVar(
          'Group',
          LFeedsUser.GetFeedsUser( LDM.cnxFeedFlow, LFeedGroup, LToken.Category.ToInteger, LToken.SubCatgegory.ToInteger,
          LToken.Country, LToken.Lang ),
          False );

        Response.Content := RenderTemplate( TMP_GROUP, Request );
      finally
        LDM.Critical.Release;
      end;
    end;

    FreeAndNil( LToken );
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
  LListNews: TListNews;
  LNbEnr: Integer;
  LFeedId: Integer;
begin
  if ValidToken( Request, True, True, LToken ) and ( LToken.Role = 'ADMIN' ) then
  begin
    LDM := GetDMSession( Request );

    if Assigned( LDM ) then
    begin
      LListNews := TListNews( GetSessionObject( Request, 'QryListNews' ) );
      if not ( Assigned( LListNews ) ) then
      begin
        LListNews := TListNews.Create;
        AddSessionObject( Request, 'QryListNews', LListNews );
      end;

      if ( Request.ContentFields.Values[ 'SELECTION' ] = 'O' ) then
      begin
        LSelCategorie := Request.ContentFields.Values[ 'FilterCategorie' ];
        LSelSousCategorie := Request.ContentFields.Values[ 'FilterSousCategorie' ];
        LSelPays := Request.ContentFields.Values[ 'FilterPays' ];
        LSelLangue := Request.ContentFields.Values[ 'FilterLangue' ];

        LDM.SessionVariables.Values[ FILTER_CATEGORIES ] := LSelCategorie;
        LDM.SessionVariables.Values[ FILTER_SOUS_CATEGORIES ] := LSelSousCategorie;
        LDM.SessionVariables.Values[ FILTER_PAYS ] := LSelPays;
        LDM.SessionVariables.Values[ FILTER_LANG ] := LSelLangue;
      end
      else
      begin
        LSelCategorie := LDM.SessionVariables.Values[ FILTER_CATEGORIES ];
        LSelSousCategorie := LDM.SessionVariables.Values[ FILTER_SOUS_CATEGORIES ];
        LSelPays := LDM.SessionVariables.Values[ FILTER_PAYS ];
        LSelLangue := LDM.SessionVariables.Values[ FILTER_LANG ];
      end;

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

      if not ( TryStrToInt( Request.QueryFields.Values[ 'FeedId' ], LFeedId ) ) then
      begin
        LFeedId := -1;
      end;
      FFeedId := LFeedId.ToString;

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

        LNbEnr := LListNews.GetNewsCount(
          LDM.cnxFeedFlow,
          LFeedId,
          '%' + LDM.SessionVariables.Values[ SEARCH_VARIABLE ] + '%',
          LWhereClause,
          LDateSearch,
          LDateSearch );

        //        LDM.QryCountNews.close;
        //        LDM.QryCountNews.SQL.Text := QRY_COUNT_NEWS +
        //          LWhereClause;
        //        LDM.QryCountNews.ParamByName( 'FEED_ID' ).AsInteger := FFeedId.ToInteger;
        //        LDM.QryCountNews.ParamByName( 'NEWS_TITLE' ).AsString := '%' + LDM.SessionVariables.Values[ SEARCH_VARIABLE ] + '%';
        //        LDM.QryCountNews.ParamByName( 'CREATION_DATE' ).AsDateTime := LDateSearch;
        //        LDM.QryCountNews.ParamByName( 'PUBLICATION_DATE' ).AsDateTime := LDateSearch;
        //        LDM.QryCountNews.Open;

        FMsg := 'GetPagination';

        LDM.SessionVariables.Values[ NAVIGATION_NAME ] := LInt.ToString;

        LPagination := TPagination.Create; // LDM.Pagination( NAVIGATION_NAME );
        try
          LPagination.GeneratePagesList( LNbEnr, LLinesPerPage, LInt, 'FeedId=' + FFeedId, Request.ContentFields.Values[
            'Search' ], 'FeedsList', 'GetNewsNavigation' );

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

    FreeAndNil( LToken );
  end;

  Handled := True;
end;

procedure TListENewsController.GetNews( Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean );
var
  LDM: TDMSession;
  LToken: TToken;
  LJson: TJSONObject;
  LNews: TNews;
begin
  if ValidToken( Request, True, True, LToken ) and ( LToken.Role = 'ADMIN' ) then
  begin
    LDM := GetDMSession( Request );

    if Assigned( LDM ) then
    begin
      LDM.Critical.Acquire;
      try
        LNews := TNews( GetSessionObject( Request, 'qryNews' ) );
        if not ( Assigned( LNews ) ) then
        begin
          LNews := TNews.Create;
          AddSessionObject( Request, 'qryNews', LNews );
        end;

        LJSon := LNews.GetNewsDetails( LDM.cnxFeedFlow, Request.QueryFields.Values[ 'Id' ].ToInteger );

        Response.ContentType := 'application/json; charset=utf-8';
        Response.Content := LJson.ToJSON;

        FreeAndNil( LJson );
      finally
        LDM.Critical.Leave;
      end;
    end;

    FreeAndNil( LToken );
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
  LNews: TNews;
begin
  if ValidToken( Request, True, True, LToken ) and ( LToken.Role = 'ADMIN' ) then
  begin
    LDM := GetDMSession( Request );

    if Assigned( LDM ) then
    begin
      LDM.Critical.Acquire;
      try
        LNews := TNews( GetSessionObject( Request, 'qryNews' ) );
        if not ( Assigned( LNews ) ) then
        begin
          LNews := TNews.Create;
          AddSessionObject( Request, 'qryNews', LNews );
        end;

        FWebStencilsProcessor.AddVar( 'News', LNews.GetNews( LDM.cnxFeedFlow, Request.QueryFields.Values[ 'Id' ].ToInteger ),
          False );
        FFeedId := LNews.FeedId.ToString;
        FWebStencilsProcessor.AddVar( 'Form', Self, False );

        Response.Content := RenderTemplate( TMP_LINE_EDIT, Request );

        //        LDM.QryNews.close;
        //        LDM.QryNews.ParamByName( 'NEWS_ID' ).AsString := Request.QueryFields.Values[ 'Id' ];
        //        LDM.QryNews.Open;
        //
        //        if not ( LDM.QryNews.Eof ) then
        //        begin
        //          LDM.QryNews.Open;
        //          LDM.QryNews.First;
        //
        //          FWebStencilsProcessor.AddVar( 'News', LDM.QryNews, False );
        //          FWebStencilsProcessor.AddVar( 'Form', Self, False );
        //
        //          Response.Content := RenderTemplate( TMP_LINE_EDIT, Request );
        //        end;
      finally
        LDM.Critical.Leave;
      end;
    end;

    FreeAndNil( LToken );
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
  LFeed: TFeed;
  LCategories: TCategories;
  LSubcategories: TSubcategories;
  LCountries: TCountries;
  LLanguages: TLanguages;
  LListNews: TListNews;
  LNbEnr: Integer;
  LTitle: string;
begin
  LDM := GetDMSession( Request );

  if Assigned( LDM ) then
  begin
    if ValidToken( Request, False, True, LToken ) and ( LToken.Role = 'ADMIN' ) then
    begin
      LDM.cnxFeedFlow.Rollback;

      LFeed := TFeed( GetSessionObject( Request, 'qryFeed' ) );
      if not ( Assigned( LFeed ) ) then
      begin
        LFeed := TFeed.Create;
        AddSessionObject( Request, 'qryFeed', LFeed );
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

      if not ( TryStrToInt( LDM.SessionVariables.Values[ LINEPERPAGE_VARIABLE ], LLinesPerPage ) ) then
      begin
        LLinesPerPage := 10;
      end;

      LPagination := TPagination.Create; // LDM.Pagination( NAVIGATION_NAME );

      //      LPage := LPagination.actualPage;

      //      if ( LPage > 0 ) then
      //      begin
      //        Dec( LPage );
      //      end;

      if not ( TryStrToInt( LDM.SessionVariables.Values[ NAVIGATION_NAME ], LPage ) ) then
      begin
        LPage := 0;
      end;

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

      LTitle := '%' + LDM.SessionVariables.Values[ SEARCH_VARIABLE ] + '%';

      Logger.Info( 'Newslist, FeedId : ' + FFeedId );

      LDM.Critical.Acquire;
      try
        LListNews := TListNews( GetSessionObject( Request, 'QryListNews' ) );
        if not ( Assigned( LListNews ) ) then
        begin
          LListNews := TListNews.Create;
          AddSessionObject( Request, 'QryListNews', LListNews );
        end;

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

          LNbEnr := LListNews.GetNewsCount(
            LDM.cnxFeedFlow,
            FFeedId.ToInteger,
            LTitle,
            '',
            LDateSearch,
            LDateSearch );

          //          LDM.QryCountNews.close;
          //          LDM.QryCountNews.ParamByName( 'FEED_ID' ).AsInteger := FFeedId.ToInteger;
          //          LDM.QryCountNews.ParamByName( 'NEWS_TITLE' ).AsString := '%' + LDM.SessionVariables.Values[ SEARCH_VARIABLE ] + '%';
          //          LDM.QryCountNews.ParamByName( 'CREATION_DATE' ).AsDateTime := LDateSearch;
          //          LDM.QryCountNews.ParamByName( 'PUBLICATION_DATE' ).AsDateTime := LDateSearch;
          //          LDM.QryCountNews.Open;

          if not ( TryStrToInt( Request.QueryFields.Values[ 'Actual' ], LInt ) ) then
          begin
            LInt := 1;
          end;

          LDM.SessionVariables.Values[ NAVIGATION_NAME ] := LInt.ToString;

          LPagination.GeneratePagesList( LNbEnr, LLinesPerPage, LInt, 'FeedId=' + FFeedId, '', 'NewsList',
            'GetNewsNavigation' );

          //          FWebStencilsProcessor.AddVar( 'pages', LDM.Pagination( NAVIGATION_NAME ), False );
          FWebStencilsProcessor.AddVar( 'pages', LPagination, False );
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

        //        LDM.QryListNews.close;
        //        LDM.QryListNews.SQL.Text := QRY_LIST_NEWS +
        //          LWhereClause +
        //          ' order by ' + LDM.SessionVariables.Values[ 'SortNewsField' ] + ' ' + LDM.SessionVariables.Values[ 'SortNewsOrd' ];
        //        LDM.QryListNews.ParamByName( 'FIRST' ).AsInteger := LLinesPerPage;
        //        LDM.QryListNews.ParamByName( 'SKIP' ).AsInteger := LPage * LLinesPerPage;
        //        LDM.QryListNews.ParamByName( 'FEED_ID' ).AsInteger := FFeedId.ToInteger;
        //        LDM.QryListNews.ParamByName( 'NEWS_TITLE' ).AsString := '%' + LDM.SessionVariables.Values[ SEARCH_VARIABLE ] + '%';
        //        LDM.QryListNews.ParamByName( 'CREATION_DATE' ).AsDateTime := LDateSearch;
        //        LDM.QryListNews.ParamByName( 'PUBLICATION_DATE' ).AsDateTime := LDateSearch;
        //        LDM.QryListNews.Open;

                //        LDM.qryFeeds.close;
                //        LDM.qryFeeds.ParamByName( 'FEED_ID' ).AsInteger := FFeedId.ToInteger;
                //        LDM.qryFeeds.Open;

        FTemplateName := LFeed.GetTemplateName( LDM.cnxFeedFlow, FFeedId.ToInteger ); // LDM.qryFeedsDISPLAY_TEMPLATE.Value;

        FWebStencilsProcessor.AddVar( 'newsList',
          LListNews.GetNewslist( LDM.cnxFeedFlow, LLinesPerPage, LPage * LLinesPerPage, FFeedId.ToInteger, LTitle, LWhereClause,
          LDM.SessionVariables.Values[ 'SortNewsField' ], LDM.SessionVariables.Values[ 'SortNewsOrd' ], LDateSearch, LDateSearch
          ),
          False );
        FWebStencilsProcessor.AddVar( 'Categories', LCategories.GetListOfCategories( LDM.cnxFeedFlow ), False );
        FWebStencilsProcessor.AddVar( 'SubCategories', LSubcategories.GetListOfSubcategories( LDM.cnxFeedFlow ), False );
        FWebStencilsProcessor.AddVar( 'Country', LCountries.GetListOfCountries( LDM.cnxFeedFlow ), False );
        FWebStencilsProcessor.AddVar( 'Languages', LLanguages.GetListOfLanguages( LDM.cnxFeedFlow ), False );
        FWebStencilsProcessor.AddVar( 'Form', Self, False );

        Response.Content := RenderTemplate( LTemplate, Request );

        FreeAndNil( LPagination );
      finally
        LDM.Critical.Release;
      end;

      FreeAndNil( LToken );
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
  LToken: TToken;
  LJsonObj: TJSONObject;
  LContent: string;
  LArrayCategorie,
    LArraySousCategorie,
    LArrayPays,
    LArrayLangue: TJSONArray;
  LNews: TNews;
  LCategories: TNewsCategory;
  LSubcategories: TNewsSubCategory;
  LCountries: TNewsCountry;
  LLanguages: TNewsLanguage;
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

            LArrayCategorie := LJsonObj.GetValue<TJSONArray>( 'Category' );
            LArraySousCategorie := LJsonObj.GetValue<TJSONArray>( 'Subcategory' );
            LArrayPays := LJsonObj.GetValue<TJSONArray>( 'Country' );
            LArrayLangue := LJsonObj.GetValue<TJSONArray>( 'Lang' );

            LNews := TNews( GetSessionObject( Request, 'qryNews' ) );
            if not ( Assigned( LNews ) ) then
            begin
              LNews := TNews.Create;
              AddSessionObject( Request, 'qryNews', LNews );
            end;

            LCategories := TNewsCategory( GetSessionObject( Request, 'QryNewsCategories' ) );
            if not ( Assigned( LCategories ) ) then
            begin
              LCategories := TNewsCategory.Create;
              AddSessionObject( Request, 'QryNewsCategories', LCategories );
            end;

            LSubcategories := TNewsSubCategory( GetSessionObject( Request, 'QryNewsSubCategories' ) );
            if not ( Assigned( LSubcategories ) ) then
            begin
              LSubcategories := TNewsSubCategory.Create;
              AddSessionObject( Request, 'QryNewsSubCategories', LSubcategories );
            end;

            LCountries := TNewsCountry( GetSessionObject( Request, 'QryNewsCountry' ) );
            if not ( Assigned( LCountries ) ) then
            begin
              LCountries := TNewsCountry.Create;
              AddSessionObject( Request, 'QryNewsCountry', LCountries );
            end;

            LLanguages := TNewsLanguage( GetSessionObject( Request, 'QryNewsLanguage' ) );
            if not ( Assigned( LLanguages ) ) then
            begin
              LLanguages := TNewsLanguage.Create;
              AddSessionObject( Request, 'QryNewsLanguage', LLanguages );
            end;

            LNews.SetContentNews( LDM.cnxFeedFlow, Request.QueryFields.Values[ 'IdNews' ].ToInteger, LContent );

            // on Sauvegarde le lien avec les catégories
            LCategories.DeleteCategories( LDM.cnxFeedFlow, StrToInt( Request.QueryFields.Values[ 'IdNews' ] ) );

            for var i := 0 to LArrayCategorie.Count - 1 do
            begin
              LCategories.AddCategory(
                LDM.cnxFeedFlow,
                StrToInt( Request.QueryFields.Values[ 'IdNews' ] ),
                StrToInt( LArrayCategorie.Items[ i ].Value )
                );
            end;

            // on Sauvegarde le lien avec les sous-catégories
            LSubcategories.DeleteSubcategories( LDM.cnxFeedFlow, StrToInt( Request.QueryFields.Values[ 'IdNews' ] ) );

            for var i := 0 to LArraySousCategorie.Count - 1 do
            begin
              LSubcategories.AddSubcategory(
                LDM.cnxFeedFlow,
                StrToInt( Request.QueryFields.Values[ 'IdNews' ] ),
                StrToInt( LArraySousCategorie.Items[ i ].Value )
                );
            end;

            // on Sauvegarde le lien avec les pays
            LCountries.DeleteCountries( LDM.cnxFeedFlow, StrToInt( Request.QueryFields.Values[ 'IdNews' ] ) );

            for var i := 0 to LArrayPays.Count - 1 do
            begin
              LCountries.AddCountry(
                LDM.cnxFeedFlow,
                LArrayPays.Items[ i ].Value,
                StrToInt( Request.QueryFields.Values[ 'IdNews' ] )
                );
            end;

            // on Sauvegarde le lien avec les langues
            LLanguages.DeleteLanguages( LDM.cnxFeedFlow, StrToInt( Request.QueryFields.Values[ 'IdNews' ] ) );

            for var i := 0 to LArrayLangue.Count - 1 do
            begin
              LLanguages.AddLanguage(
                LDM.cnxFeedFlow,
                LArrayLangue.Items[ i ].Value,
                StrToInt( Request.QueryFields.Values[ 'IdNews' ] )
                );
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

    FreeAndNil( LToken );

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
  LShowGroup: TShowGroup;
  LIdFeed: Integer;
begin
  if ValidToken( Request, True, True, LToken ) then
  begin
    LDM := GetDMSession( Request );
    if Assigned( LDM ) then
    begin
      LShowGroup := TShowGroup( GetSessionObject( Request, 'QryShowGroup' ) );
      if not ( Assigned( LShowGroup ) ) then
      begin
        LShowGroup := TShowGroup.Create;
        AddSessionObject( Request, 'QryShowGroup', LShowGroup );
      end;

      if not ( TryStrToInt( Request.QueryFields.Values[ 'contact-choice' ], LIdFeed ) ) then
      begin
        LIdFeed := -1;
      end;

      //      LDM.QryShowGroup.close;
      //      LDM.QryShowGroup.ParamByName( 'FEED_ID' ).AsString := Request.QueryFields.Values[ 'contact-choice' ];
      //      LDM.QryShowGroup.Open;

      Response.ContentType := 'text/html';
      Response.Content := '<div class="slide-content"><p>' +
        LShowGroup.GetGroup( LDM.cnxFeedFlow, LIdFeed ).FieldByName( 'TEXT' ).AsString +
        '</p></div>';
    end;

    FreeAndNil( LToken );
  end;

  Handled := True;
end;

procedure TListENewsController.ShowNews( Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean );
var
  LDM: TDMSession;
  LIdFeed: Integer;
  LToken: TToken;
  LFeed: TFeed;
  LShowNews: TShowNews;
  LTemplateName: string;
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

        LFeed := TFeed( GetSessionObject( Request, 'qryFeed' ) );
        if not ( Assigned( LFeed ) ) then
        begin
          LFeed := TFeed.Create;
          AddSessionObject( Request, 'qryFeed', LFeed );
        end;

        LShowNews := TShowNews( GetSessionObject( Request, 'QryShowNews' ) );
        if not ( Assigned( LShowNews ) ) then
        begin
          LShowNews := TShowNews.Create;
          AddSessionObject( Request, 'QryShowNews', LShowNews );
        end;

        LTemplateName := LFeed.GetTemplateName( LDM.cnxFeedFlow, LIdFeed );

        //        LDM.qryFeeds.close;
        //        LDM.qryFeeds.ParamByName( 'FEED_ID' ).AsInteger := LIdFeed;
        //        LDM.qryFeeds.Open;

        if FileExists( TPath.Combine( FWebStencilsEngine.RootDirectory, LTemplateName ) ) then
        begin
          Logger.Info( 'ShowNews, LIdFeed : ' + LIdFeed.ToString );

          //          LDM.QryShowNews.ParamByName( 'FEED_ID' ).AsInteger := LIdFeed;
          //          LDM.QryShowNews.Open;

          FWebStencilsProcessor.AddVar( 'News', LShowNews.GetNews( LDM.cnxFeedFlow, LIdFeed ), False );

          Response.ContentType := 'text/html; charset=UTF-8';
          Response.Content := RenderTemplate( LTemplateName, Request );

          //          LDM.QryShowNews.Close;
        end
        else
        begin
          response.Content := 'Erreur : Template non trouvé ' + LTemplateName;
        end;
      finally
        LDM.Critical.Release;
      end;
    end;

    FreeAndNil( LToken );
  end;
end;

procedure TListENewsController.SortNews( Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean );
var
  LLinesPerPage: Integer;
  LToken: TToken;
  LDM: TDMSession;
  //  LPagination: TPagination;
  LPage: Integer;
  LDateSearch: TDateTime;
  LListNews: TListNews;
  LTitle: string;
  LFeedId: Integer;
  LCategories: TCategories;
  LSubcategories: TSubcategories;
  LCountries: TCountries;
  LLanguages: TLanguages;
begin
  if ValidToken( Request, False, True, LToken ) and ( LToken.Role = 'ADMIN' ) then
  begin
    LDM := GetDMSession( Request );

    if Assigned( LDM ) then
    begin
      LDM.SessionVariables.Values[ 'SortNewsField' ] := Request.QueryFields.Values[ 'col' ];
      LDM.SessionVariables.Values[ 'SortNewsOrd' ] := Request.QueryFields.Values[ 'dir' ];
      if not ( TryStrToInt( Request.QueryFields.Values[ 'FeedId' ], LFeedId ) ) then
      begin
        LFeedId := -1;
      end;
      FFeedId := LFeedId.ToString;

      if not ( TryStrToInt( LDM.SessionVariables.Values[ LINEPERPAGE_VARIABLE ], LLinesPerPage ) ) then
      begin
        LLinesPerPage := 10;
      end;

      //      LPagination := TPagination.Create; // LDM.Pagination( NAVIGATION_NAME );

      if not ( TryStrToInt( LDM.SessionVariables.Values[ NAVIGATION_NAME ], LPage ) ) then
      begin
        LPage := 0;
      end;

      //      LPage := LPagination.actualPage;
      if ( LPage > 0 ) then
      begin
        Dec( LPage );
      end;

      if not ( TryStrToDate( LDM.SessionVariables.Values[ SEARCH_VARIABLE ], LDateSearch ) ) then
      begin
        LDateSearch := 0;
      end;

      LListNews := TListNews( GetSessionObject( Request, 'QryListNews' ) );
      if not ( Assigned( LListNews ) ) then
      begin
        LListNews := TListNews.Create;
        AddSessionObject( Request, 'QryListNews', LListNews );
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

      //      LDM.QryListNews.close;
      //      LDM.QryListNews.SQL.Text := QRY_LIST_NEWS +
      //        ' order by ' + LDM.SessionVariables.Values[ 'SortNewsField' ] + ' ' + LDM.SessionVariables.Values[ 'SortNewsOrd' ];
      //      LDM.QryListNews.ParamByName( 'FIRST' ).AsInteger := LLinesPerPage;
      //      LDM.QryListNews.ParamByName( 'SKIP' ).AsInteger := LPage * LLinesPerPage;
      //      LDM.QryListNews.ParamByName( 'FEED_ID' ).AsInteger := FFeedId.ToInteger;
      //      LDM.QryListNews.ParamByName( 'NEWS_TITLE' ).AsString := '%' + LDM.SessionVariables.Values[ SEARCH_VARIABLE ] + '%';
      //      LDM.QryListNews.ParamByName( 'CREATION_DATE' ).AsDateTime := LDateSearch;
      //      LDM.QryListNews.ParamByName( 'PUBLICATION_DATE' ).AsDateTime := LDateSearch;
      //      LDM.QryListNews.Open;

      FWebStencilsProcessor.AddVar( 'newsList',
        LListNews.GetNewslist( LDM.cnxFeedFlow, LLinesPerPage, LPage * LLinesPerPage, LFeedId, LTitle, '',
        LDM.SessionVariables.Values[ 'SortNewsField' ], LDM.SessionVariables.Values[ 'SortNewsOrd' ], LDateSearch, LDateSearch
        ),
        False );
      FWebStencilsProcessor.AddVar( 'Categories', LCategories.GetListOfCategories( LDM.cnxFeedFlow ), False );
      FWebStencilsProcessor.AddVar( 'SousCategories', LSubcategories.GetListOfSubcategories( LDM.cnxFeedFlow ), False );
      FWebStencilsProcessor.AddVar( 'Pays', LCountries.GetListOfCountries( LDM.cnxFeedFlow ), False );
      FWebStencilsProcessor.AddVar( 'Langues', LLanguages.GetListOfLanguages( LDM.cnxFeedFlow ), False );
      FWebStencilsProcessor.AddVar( 'Form', Self, False );

      Response.StatusCode := 200;
      Response.Content := RenderTemplate( TMP_LINES, Request );
    end;

    FreeAndNil( LToken );
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
        LSavePath := TPath.Combine( TConfig.GetInstance.FilesFolder, Request.Files[ 0 ].FileName );

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

    FreeAndNil( LToken );
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
  LFeed: TFeed;
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

        LFeed := TFeed( GetSessionObject( Request, 'qryFeed' ) );
        if not ( Assigned( LFeed ) ) then
        begin
          LFeed := TFeed.Create;
          AddSessionObject( Request, 'qryFeed', LFeed );
        end;

        LFeed.SetTemplateName( LDM.cnxFeedFlow, LIdFeed, Request.Files[ 0 ].FileName );

        //        LDM.qryFeeds.close;
        //        LDM.qryFeeds.ParamByName( 'FEED_ID' ).AsInteger := LIdFeed;
        //        LDM.qryFeeds.Open;
        //
        //        LDM.qryFeeds.Edit;
        //        LDM.qryFeedsDISPLAY_TEMPLATE.Value := Request.Files[ 0 ].FileName;
        //        LDM.qryFeeds.Post;
        //
        //        LDM.qryFeeds.Close;

        Response.ContentType := 'application/json';
        Response.Content := '{"status":"success","file":"' + Request.Files[ 0 ].FileName + '"}';
      end
      else
      begin
        Response.StatusCode := 400;
        Response.Content := '{"status":"error","message":"No file uploaded"}';
      end;
    end;

    FreeAndNil( LToken );
  end;

  Handled := True;
end;

initialization

  TControllersRegistry.GetControllersList.AddClass( TListENewsController );

end.

