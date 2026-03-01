unit UFaq;

interface

uses
  System.Classes,
  System.SysUtils,
  FireDAC.Comp.Client,
  Firedac.Stan.Param,
  Data.DB,
  System.JSON,
  System.Generics.Collections;

type
  TSlide = class( TPersistent )
  private
    FSlideNo: Integer;
    FTitle2: string;
    FTitle3: string;
    FTitle1: string;
    FSummary2: string;
    FSummary3: string;
    FSummary1: string;
    FFAQId1: Integer;
    FFAQId2: Integer;
    FFAQId3: Integer;

    procedure SetSlideNo( const Value: Integer );
    procedure SetFAQId1( const Value: Integer );
    procedure SetSummary1( const Value: string );
    procedure SetSummary2( const Value: string );
    procedure SetSummary3( const Value: string );
    procedure SetTitle1( const Value: string );
    procedure SetTitle2( const Value: string );
    procedure SetTitle3( const Value: string );
    procedure SetFAQId2( const Value: Integer );
    procedure SetFAQId3( const Value: Integer );
  published
    property SlideNo: Integer read FSlideNo write SetSlideNo;
    property FAQId1: Integer read FFAQId1 write SetFAQId1;
    property Title1: string read FTitle1 write SetTitle1;
    property Summary1: string read FSummary1 write SetSummary1;
    property FAQId2: Integer read FFAQId2 write SetFAQId2;
    property Title2: string read FTitle2 write SetTitle2;
    property Summary2: string read FSummary2 write SetSummary2;
    property FAQId3: Integer read FFAQId3 write SetFAQId3;
    property Title3: string read FTitle3 write SetTitle3;
    property Summary3: string read FSummary3 write SetSummary3;
  end;

  TFaq = class
  private
    FQryFavorites: TFDQuery;
    FQryQuestionsList: TFDQuery;
    FQryFAQ: TFDQuery;
    FQrySearch: TFDQuery;
    FQryFAQDetails: TFDQuery;
    FQryAddVue: TFDQuery;
    FQryAddReaction: TFDQuery;
    FQryCountFAQ: TFDQuery;
    FMtSlide: TFDMemTable;
  public
    constructor Create;
    destructor Destroy; override;

    function ExistsFAQ( aConnection: TFDConnection; aCategoryId, aSubcategoryId: Integer; aCountryCode,
      aLanguageCode: string ): Boolean;
    function GetFavorites( aConnection: TFDConnection; aCategoryId, aSubcategoryId:
      Integer; aCountryCode, aLanguageCode: string; out aIsThereFavorites:
      Boolean; out aRecordCount: Integer ): TObjectList<TSlide>;
    /// <summary>
    ///   Retourne la liste des FAQ de la catégorie en fonction des droits de
    ///   l'utilisateur
    /// </summary>
    function GetQuestionsList( aConnection: TFDConnection; aFeedId, aCategoryId, aSubcategoryId: Integer; aCountryCode,
      aLanguageCode, aSearch: string ): TFDQuery;
    /// <summary>
    ///   Retourne la liste des feeds autorisés à l'utilisateur
    /// </summary>
    /// <param name="aConnection">
    ///   Connexion base de données
    /// </param>
    /// <param name="aCategoryId">
    ///   Id catégorie recherchée
    /// </param>
    /// <param name="aSubcategoryId">
    ///   Id sous-catégorie recherchée
    /// </param>
    /// <param name="aCountryCode">
    ///   Code pays recherché
    /// </param>
    /// <param name="aLanguageCode">
    ///   Code langue recherché
    /// </param>
    function GetFAQCategories( aConnection: TFDConnection; aCategoryId,
      aSubcategoryId: Integer; aCountryCode, aLanguageCode: string; out
      aFirstFeed: Integer ): TFDQuery;
    function GetFAQDetails( aConnection: TFDConnection; aFAQId, aUserID: Integer ): TFDQuery;

    procedure AddVue( aConnection: TFDConnection; aFAQId: Integer );
    procedure AddReaction( aConnection: TFDConnection; aFAQId, aUserId: Integer;
      aReaction: string );
  end;

implementation

{ TFaq }

procedure TFaq.AddReaction( aConnection: TFDConnection; aFAQId,
  aUserId: Integer; aReaction: string );
begin
  FQryAddReaction.Connection := aConnection;
  FQryAddReaction.ParamByName( 'NEWS_ID' ).AsInteger := aFAQId;
  FQryAddReaction.ParamByName( 'USER_ID' ).AsInteger := aUserId;
  if ( aReaction = 'Like' ) then
  begin
    FQryAddReaction.ParamByName( 'REACTION' ).AsInteger := 1;
  end
  else
  begin
    FQryAddReaction.ParamByName( 'REACTION' ).AsInteger := 0;
  end;
  FQryAddReaction.ParamByName( 'REACTION_DATE' ).AsDateTime := Now;
  FQryAddReaction.ExecSQL;
end;

procedure TFaq.AddVue( aConnection: TFDConnection; aFAQId: Integer );
begin
  FQryAddVue.Connection := aConnection;
  FQryAddVue.ParamByName( 'NEWS_ID' ).AsInteger := aFAQId;
  FQryAddVue.ExecSQL;
end;

constructor TFaq.Create;
begin
  FQryFavorites := TFDQuery.Create( nil );
  FQryFavorites.Name := 'qryFavorites';
  FQryFavorites.SQL.Clear;
  FQryFavorites.SQL.Add( '''
    select FIRST 6
      n.NEWS_ID,
      n.FEED_ID,
      n.NEWS_TITLE,
      n.TEXT,
      n.SUMMARY
    from NEWS n
    join FEED_NEWS f on (f.FEED_ID = n.FEED_ID)
    join GROUPS g on (g.GROUP_ID = f.FEED_GROUP)
    where g.GROUP_TYPE = 'F'
      and HOLD = 'O'
      and PUBLICATION_DATE <= localtimestamp
      and EXPIRY_DATE > localtimestamp
      and n.NB_VIEWS is not NULL
      and n.NB_VIEWS <> 0
      and EXISTS (
              SELECT 1
              FROM NEWS_CONTEXT_CATEGORY cc
              WHERE cc.NEWS_ID = n.NEWS_ID
                AND cc.CATEGORY_ID = :CATEGORY_ID
        )
      and EXISTS (
              SELECT 1
              FROM NEWS_CONTEXT_SUBCATEGORY sc
              WHERE sc.NEWS_ID = n.NEWS_ID
                AND sc.SUBCATEGORY_ID = :SUBCATEGORY_ID
        )
      and EXISTS (
              SELECT 1
              FROM NEWS_CONTEXT_COUNTRY cp
              WHERE cp.NEWS_ID = n.NEWS_ID
                AND cp.COUNTRY_CODE = :COUNTRY_CODE
        )
      and EXISTS (
              SELECT 1
              FROM NEWS_CONTEXT_LANG cl
              WHERE cl.NEWS_ID = n.NEWS_ID
                AND cl.LANGUAGE_CODE = :LANGUAGE_CODE
        )
    order by n.NB_VIEWS DESC
  ''');

  FQryQuestionsList := TFDQuery.Create( nil );
  FQryQuestionsList.Name := 'QryQuestionslist';
  FQryQuestionsList.SQL.Clear;
  FQryQuestionsList.SQL.Add( '''
    select
      n.DISPLAY_ORDER,
      n.NEWS_ID,
      n.NEWS_TITLE,
      n.TEXT,
      n.FEED_ID,
      n.SUMMARY
    from NEWS n
    join FEED_NEWS f on (f.FEED_ID = n.FEED_ID)
    where n.FEED_ID = :FEED_ID
      and HOLD = 'O'
      and PUBLICATION_DATE <= localtimestamp
      and EXPIRY_DATE > localtimestamp
      and EXISTS (
              SELECT 1
              FROM NEWS_CONTEXT_CATEGORY cc
              WHERE cc.NEWS_ID = n.NEWS_ID
                AND cc.CATEGORY_ID = :CATEGORY_ID
        )
      and EXISTS (
              SELECT 1
              FROM NEWS_CONTEXT_SUBCATEGORY sc
              WHERE sc.NEWS_ID = n.NEWS_ID
                AND sc.SUBCATEGORY_ID = :SUBCATEGORY_ID
        )
      and EXISTS (
              SELECT 1
              FROM NEWS_CONTEXT_COUNTRY cp
              WHERE cp.NEWS_ID = n.NEWS_ID
                AND cp.COUNTRY_CODE = :COUNTRY_CODE
        )
      and EXISTS (
              SELECT 1
              FROM NEWS_CONTEXT_LANG cl
              WHERE cl.NEWS_ID = n.NEWS_ID
                AND cl.LANGUAGE_CODE = :LANGUAGE_CODE
        )
    order by DISPLAY_ORDER, PUBLICATION_DATE desc, NEWS_TITLE;
  ''');

  FQrySearch := TFDQuery.Create( nil );
  FQrySearch.Name := 'QryQuestionslist';
  FQrySearch.SQL.Clear;
  FQrySearch.SQL.Add( '''
    select
      n.DISPLAY_ORDER,
      n.NEWS_ID,
      n.NEWS_TITLE,
      n.TEXT,
      n.FEED_ID,
      n.SUMMARY
    from NEWS n
    join FEED_NEWS f on (f.FEED_ID = n.FEED_ID)
    join GROUPS g on (g.GROUP_ID = f.FEED_GROUP)
    where g.GROUP_TYPE = 'F'
      and HOLD = 'O'
      and PUBLICATION_DATE <= localtimestamp
      and EXPIRY_DATE > localtimestamp
      and EXISTS (
              SELECT 1
              FROM NEWS_CONTEXT_CATEGORY cc
              WHERE cc.NEWS_ID = n.NEWS_ID
                AND cc.CATEGORY_ID = :CATEGORY_ID
        )
      and EXISTS (
              SELECT 1
              FROM NEWS_CONTEXT_SUBCATEGORY sc
              WHERE sc.NEWS_ID = n.NEWS_ID
                AND sc.SUBCATEGORY_ID = :SUBCATEGORY_ID
        )
      and EXISTS (
              SELECT 1
              FROM NEWS_CONTEXT_COUNTRY cp
              WHERE cp.NEWS_ID = n.NEWS_ID
                AND cp.COUNTRY_CODE = :COUNTRY_CODE
        )
      and EXISTS (
              SELECT 1
              FROM NEWS_CONTEXT_LANG cl
              WHERE cl.NEWS_ID = n.NEWS_ID
                AND cl.LANGUAGE_CODE = :LANGUAGE_CODE
        )
      and ( n.TEXT containing :SEARCH
       or n.SUMMARY containing :SEARCH)
    order by DISPLAY_ORDER, PUBLICATION_DATE desc, NEWS_TITLE;
  ''');

  FQryFAQ := TFDQuery.Create( nil );
  FQryFAQ.Name := 'QryFAQ';
  FQryFAQ.SQL.Clear;
  FQryFAQ.SQL.Add( '''
    SELECT
        r.FEED_ID,
        r.FEED_GROUP,
        r.FEED_NAME,
        r.TITLE,
        r.DISPLAY_TEMPLATE
    FROM FEED_NEWS r
    JOIN GROUPS g
        ON g.GROUP_ID = r.FEED_GROUP
    WHERE r.STATUS = 'O'
      AND g.GROUP_TYPE = 'F'

      AND EXISTS (
            SELECT 1
            FROM NEWS n
            WHERE n.FEED_ID = r.FEED_ID
              AND EXISTS (
                    SELECT 1
                    FROM NEWS_CONTEXT_CATEGORY cc
                    WHERE cc.NEWS_ID = n.NEWS_ID
                      AND cc.CATEGORY_ID = :CATEGORY_ID
              )
              AND EXISTS (
                    SELECT 1
                    FROM NEWS_CONTEXT_SUBCATEGORY sc
                    WHERE sc.NEWS_ID = n.NEWS_ID
                      AND sc.SUBCATEGORY_ID = :SUBCATEGORY_ID
              )
              AND EXISTS (
                    SELECT 1
                    FROM NEWS_CONTEXT_COUNTRY cp
                    WHERE cp.NEWS_ID = n.NEWS_ID
                      AND cp.COUNTRY_CODE = :COUNTRY_CODE
              )
              AND EXISTS (
                    SELECT 1
                    FROM NEWS_CONTEXT_LANG cl
                    WHERE cl.NEWS_ID = n.NEWS_ID
                      AND cl.LANGUAGE_CODE = :LANGUAGE_CODE
              )
      )
    ORDER BY r.FEED_ID;
  ''');

  FQryCountFAQ := TFDQuery.Create( nil );
  FQryCountFAQ.Name := 'QryCountFAQ';
  FQryCountFAQ.SQL.Clear;
  FQryCountFAQ.SQL.Add( '''
    SELECT
       count( r.FEED_ID) as "NB_FAQ"
    FROM FEED_NEWS r
    JOIN GROUPS g
        ON g.GROUP_ID = r.FEED_GROUP
    WHERE r.STATUS = 'O'
      AND g.GROUP_TYPE = 'F'

      AND EXISTS (
            SELECT 1
            FROM NEWS n
            WHERE n.FEED_ID = r.FEED_ID
              AND EXISTS (
                    SELECT 1
                    FROM NEWS_CONTEXT_CATEGORY cc
                    WHERE cc.NEWS_ID = n.NEWS_ID
                      AND cc.CATEGORY_ID = :CATEGORY_ID
              )
              AND EXISTS (
                    SELECT 1
                    FROM NEWS_CONTEXT_SUBCATEGORY sc
                    WHERE sc.NEWS_ID = n.NEWS_ID
                      AND sc.SUBCATEGORY_ID = :SUBCATEGORY_ID
              )
              AND EXISTS (
                    SELECT 1
                    FROM NEWS_CONTEXT_COUNTRY cp
                    WHERE cp.NEWS_ID = n.NEWS_ID
                      AND cp.COUNTRY_CODE = :COUNTRY_CODE
              )
              AND EXISTS (
                    SELECT 1
                    FROM NEWS_CONTEXT_LANG cl
                    WHERE cl.NEWS_ID = n.NEWS_ID
                      AND cl.LANGUAGE_CODE = :LANGUAGE_CODE
              )
      )
  ''');

  FQryFAQ.UpdateOptions.RequestLive := True;

  FQryFAQDetails := TFDQuery.Create( nil );
  FQryFAQDetails.Name := 'QryFAQDetails';
  FQryFAQDetails.SQL.Clear;
  FQryFAQDetails.SQL.Add( '''
    SELECT
        n.NEWS_ID,
        n.NEWS_TITLE,
        n.TEXT,
        COALESCE(r.REACTION, -1) as "REACTION"
    FROM NEWS n
    left join NEWS_REACTIONS r on (r.NEWS_ID = n.NEWS_ID and r.USER_ID = :USER_ID)
    where n.NEWS_ID = :NEWS_ID
  ''');

  FQryFAQDetails.UpdateOptions.RequestLive := True;

  FQryAddVue := TFDQuery.Create( nil );
  FQryAddVue.Name := 'QryAddVue';
  FQryAddVue.SQL.Clear;
  FQryAddVue.SQL.Text := '''
    update NEWS set
      NB_VIEWS = NB_VIEWS + 1
    where NEWS_ID = :NEWS_ID
  ''';

  FQryAddReaction := TFDQuery.Create( nil );
  FQryAddReaction.Name := 'QryAddReaction';
  FQryAddReaction.SQL.Clear;
  FQryAddReaction.SQL.Text := '''
    update or insert into NEWS_REACTIONS (NEWS_ID, USER_ID, REACTION, REACTION_DATE)
    values (:NEWS_ID, :USER_ID, :REACTION, :REACTION_DATE)
    matching (NEWS_ID, USER_ID)
  ''';
end;

destructor TFaq.Destroy;
begin
  FreeAndNil( FQryFavorites );
  FreeAndNil( FQryQuestionsList );
  FreeAndNil( FQryFAQ );
  FreeAndNil( FQrySearch );
  FreeAndNil( FQryFAQDetails );
  FreeAndNil( FQryAddVue );
  FreeAndNil( FQryAddReaction );
  FreeAndNil( FQryCountFAQ );

  inherited;
end;

function TFaq.ExistsFAQ( aConnection: TFDConnection; aCategoryId,
  aSubcategoryId: Integer; aCountryCode, aLanguageCode: string ): Boolean;
begin
  FQryCountFAQ.Connection := aConnection;
  FQryCountFAQ.Close;
  FQryCountFAQ.ParamByName( 'CATEGORY_ID' ).AsInteger := aCategoryId;
  FQryCountFAQ.ParamByName( 'SUBCATEGORY_ID' ).AsInteger := aSubcategoryId;
  FQryCountFAQ.ParamByName( 'COUNTRY_CODE' ).AsString := aCountryCode;
  FQryCountFAQ.ParamByName( 'LANGUAGE_CODE' ).AsString := aLanguageCode;
  FQryCountFAQ.Open;

  Result := ( FQryCountFAQ.FieldByName( 'NB_FAQ' ).AsInteger > 0 );

  FQryCountFAQ.Close;
end;

function TFaq.GetFAQCategories( aConnection: TFDConnection; aCategoryId,
  aSubcategoryId: Integer; aCountryCode, aLanguageCode: string;
  out aFirstFeed: Integer ): TFDQuery;
begin
  FQryFAQ.Connection := aConnection;
  FQryFAQ.Close;
  FQryFAQ.ParamByName( 'CATEGORY_ID' ).AsInteger := aCategoryId;
  FQryFAQ.ParamByName( 'SUBCATEGORY_ID' ).AsInteger := aSubcategoryId;
  FQryFAQ.ParamByName( 'COUNTRY_CODE' ).AsString := aCountryCode;
  FQryFAQ.ParamByName( 'LANGUAGE_CODE' ).AsString := aLanguageCode;
  FQryFAQ.Open;

  if not ( FQryFAQ.Eof ) then
  begin
    FQryFAQ.First;
    aFirstFeed := FQryFAQ.FieldByName( 'FEED_ID' ).AsInteger;
  end;

  Result := FQryFAQ;
end;

function TFaq.GetFAQDetails( aConnection: TFDConnection; aFAQId, aUserID:
  Integer ): TFDQuery;
begin
  FQryFAQDetails.Connection := aConnection;
  FQryFAQDetails.Close;
  FQryFAQDetails.ParamByName( 'NEWS_ID' ).AsInteger := aFAQId;
  FQryFAQDetails.ParamByName( 'USER_ID' ).AsInteger := aUserID;
  FQryFAQDetails.Open;

  Result := FQryFAQDetails;
end;

function TFaq.GetFavorites( aConnection: TFDConnection; aCategoryId,
  aSubcategoryId: Integer; aCountryCode, aLanguageCode: string; out
  aIsThereFavorites: Boolean; out aRecordCount: Integer ): TObjectList<TSlide>;
var
  LSlide: TSlide;
begin
  FQryFavorites.Connection := aConnection;
  FQryFavorites.Close;
  FQryFavorites.ParamByName( 'COUNTRY_CODE' ).AsString := aCountryCode;
  FQryFavorites.ParamByName( 'LANGUAGE_CODE' ).AsString := aLanguageCode;
  FQryFavorites.ParamByName( 'CATEGORY_ID' ).AsInteger := aCategoryId;
  FQryFavorites.ParamByName( 'SUBCATEGORY_ID' ).AsInteger := aSubcategoryId;
  FQryFavorites.Open;

  aIsThereFavorites := not ( FQryFavorites.Eof );
  aRecordCount := FQryFavorites.RecordCount;

  Result := TObjectList<TSlide>.Create( True );

  var LSlideNo := 0;

  FQryFavorites.First;

  for var i := 1 to aRecordCount div 3 do
  begin
    Inc( LSlideNo );

    LSlide := TSlide.Create;
    LSlide.SlideNo := LSlideNo;
    Result.Add( LSlide );

    if not ( FQryFavorites.Eof ) then
    begin
      LSlide.FAQId1 := FQryFavorites.FieldByName( 'NEWS_ID' ).AsInteger;
      LSlide.Title1 := FQryFavorites.FieldByName( 'NEWS_TITLE' ).AsString;
      LSlide.Summary1 := FQryFavorites.FieldByName( 'SUMMARY' ).AsString;

      FQryFavorites.Next;
    end;

    if not ( FQryFavorites.Eof ) then
    begin
      LSlide.FAQId2 := FQryFavorites.FieldByName( 'NEWS_ID' ).AsInteger;
      LSlide.Title2 := FQryFavorites.FieldByName( 'NEWS_TITLE' ).AsString;
      LSlide.Summary2 := FQryFavorites.FieldByName( 'SUMMARY' ).AsString;

      FQryFavorites.Next;
    end;

    if not ( FQryFavorites.Eof ) then
    begin
      LSlide.FAQId3 := FQryFavorites.FieldByName( 'NEWS_ID' ).AsInteger;
      LSlide.Title3 := FQryFavorites.FieldByName( 'NEWS_TITLE' ).AsString;
      LSlide.Summary3 := FQryFavorites.FieldByName( 'SUMMARY' ).AsString;

      FQryFavorites.Next;
    end;
  end;

  if ( aRecordCount mod 3 > 0 ) then
  begin
    Inc( LSlideNo );

    LSlide := TSlide.Create;
    LSlide.SlideNo := LSlideNo;
    Result.Add( LSlide );

    if not ( FQryFavorites.Eof ) then
    begin
      LSlide.FAQId1 := FQryFavorites.FieldByName( 'NEWS_ID' ).AsInteger;
      LSlide.Title1 := FQryFavorites.FieldByName( 'NEWS_TITLE' ).AsString;
      LSlide.Summary1 := FQryFavorites.FieldByName( 'SUMMARY' ).AsString;

      FQryFavorites.Next;
    end;

    if not ( FQryFavorites.Eof ) then
    begin
      LSlide.FAQId2 := FQryFavorites.FieldByName( 'NEWS_ID' ).AsInteger;
      LSlide.Title2 := FQryFavorites.FieldByName( 'NEWS_TITLE' ).AsString;
      LSlide.Summary2 := FQryFavorites.FieldByName( 'SUMMARY' ).AsString;

      FQryFavorites.Next;
    end;

    if not ( FQryFavorites.Eof ) then
    begin
      LSlide.FAQId3 := FQryFavorites.FieldByName( 'NEWS_ID' ).AsInteger;
      LSlide.Title3 := FQryFavorites.FieldByName( 'NEWS_TITLE' ).AsString;
      LSlide.Summary3 := FQryFavorites.FieldByName( 'SUMMARY' ).AsString;

      FQryFavorites.Next;
    end;
  end;
end;

function TFaq.GetQuestionsList( aConnection: TFDConnection; aFeedId,
  aCategoryId, aSubcategoryId: Integer; aCountryCode, aLanguageCode, aSearch:
  string ): TFDQuery;
var
  LFeedId: Integer;
begin
  if ( aFeedId <> -1 ) then
  begin
    LFeedId := aFeedId;
  end
  else
  begin
    FQryFavorites.First;
    LFeedId := FQryFavorites.FieldByName( 'FEED_ID' ).AsInteger;
  end;

  if ( aSearch = '' ) then
  begin
    FQryQuestionsList.Connection := aConnection;
    FQryQuestionsList.Close;
    FQryQuestionsList.ParamByName( 'FEED_ID' ).AsInteger := LFeedId;
    FQryQuestionsList.ParamByName( 'COUNTRY_CODE' ).AsString := aCountryCode;
    FQryQuestionsList.ParamByName( 'LANGUAGE_CODE' ).AsString := aLanguageCode;
    FQryQuestionsList.ParamByName( 'CATEGORY_ID' ).AsInteger := aCategoryId;
    FQryQuestionsList.ParamByName( 'SUBCATEGORY_ID' ).AsInteger := aSubcategoryId;
    FQryQuestionsList.Open;

    Result := FQryQuestionsList;
  end
  else
  begin
    FQrySearch.Connection := aConnection;
    FQrySearch.Close;
    FQrySearch.ParamByName( 'COUNTRY_CODE' ).AsString := aCountryCode;
    FQrySearch.ParamByName( 'LANGUAGE_CODE' ).AsString := aLanguageCode;
    FQrySearch.ParamByName( 'CATEGORY_ID' ).AsInteger := aCategoryId;
    FQrySearch.ParamByName( 'SUBCATEGORY_ID' ).AsInteger := aSubcategoryId;
    FQrySearch.ParamByName( 'SEARCH' ).AsString := aSearch;
    FQrySearch.Open;

    Result := FQrySearch;
  end;
end;

{ TSlide }

procedure TSlide.SetFAQId1( const Value: Integer );
begin
  FFAQId1 := Value;
end;

procedure TSlide.SetFAQId2( const Value: Integer );
begin
  FFAQId2 := Value;
end;

procedure TSlide.SetFAQId3( const Value: Integer );
begin
  FFAQId3 := Value;
end;

procedure TSlide.SetSlideNo( const Value: Integer );
begin
  FSlideNo := Value;
end;

procedure TSlide.SetSummary1( const Value: string );
begin
  FSummary1 := Value;
end;

procedure TSlide.SetSummary2( const Value: string );
begin
  FSummary2 := Value;
end;

procedure TSlide.SetSummary3( const Value: string );
begin
  FSummary3 := Value;
end;

procedure TSlide.SetTitle1( const Value: string );
begin
  FTitle1 := Value;
end;

procedure TSlide.SetTitle2( const Value: string );
begin
  FTitle2 := Value;
end;

procedure TSlide.SetTitle3( const Value: string );
begin
  FTitle3 := Value;
end;

end.

