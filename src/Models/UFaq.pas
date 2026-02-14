unit UFaq;

interface

uses
  System.Classes,
  System.SysUtils,
  FireDAC.Comp.Client,
  Firedac.Stan.Param,
  Data.DB,
  System.JSON;

type
  TFaq = class
  private
    FQryFavorites: TFDQuery;
    FQryQuestionsList: TFDQuery;
    FQryFAQ: TFDQuery;
    FQrySearch: TFDQuery;
    FQryFAQDetails: TFDQuery;
  public
    constructor Create;
    destructor Destroy; override;

    function GetFavorites( aConnection: TFDConnection; aCategoryId, aSubcategoryId:
      Integer; aCountryCode, aLanguageCode: string; out aIsThereFavorites:
      Boolean ): TFDQuery;
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
    function GetFAQDetails( aConnection: TFDConnection; aFAQId: Integer ): TFDQuery;
  end;

implementation

{ TFaq }

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

  FQryFAQ.UpdateOptions.RequestLive := True;

  FQryFAQDetails := TFDQuery.Create( nil );
  FQryFAQDetails.Name := 'QryFAQDetails';
  FQryFAQDetails.SQL.Clear;
  FQryFAQDetails.SQL.Add( '''
    SELECT
        r.NEWS_ID,
        r.NEWS_TITLE,
        r.TEXT
    FROM NEWS r
    where NEWS_ID = :NEWS_ID
  ''');

  FQryFAQDetails.UpdateOptions.RequestLive := True;
end;

destructor TFaq.Destroy;
begin
  FreeAndNil( FQryFavorites );
  FreeAndNil( FQryQuestionsList );
  FreeAndNil( FQryFAQ );
  FreeAndNil( FQrySearch );
  FreeAndNil( FQryFAQDetails );

  inherited;
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

function TFaq.GetFAQDetails( aConnection: TFDConnection;
  aFAQId: Integer ): TFDQuery;
begin
  FQryFAQDetails.Connection := aConnection;
  FQryFAQDetails.Close;
  FQryFAQDetails.ParamByName( 'NEWS_ID' ).AsInteger := aFAQId;
  FQryFAQDetails.Open;

  Result := FQryFAQDetails;
end;

function TFaq.GetFavorites( aConnection: TFDConnection; aCategoryId,
  aSubcategoryId: Integer; aCountryCode, aLanguageCode: string; out
  aIsThereFavorites: Boolean ): TFDQuery;
begin
  FQryFavorites.Connection := aConnection;
  FQryFavorites.Close;
  FQryFavorites.ParamByName( 'COUNTRY_CODE' ).AsString := aCountryCode;
  FQryFavorites.ParamByName( 'LANGUAGE_CODE' ).AsString := aLanguageCode;
  FQryFavorites.ParamByName( 'CATEGORY_ID' ).AsInteger := aCategoryId;
  FQryFavorites.ParamByName( 'SUBCATEGORY_ID' ).AsInteger := aSubcategoryId;
  FQryFavorites.Open;

  aIsThereFavorites := not ( FQryFavorites.Eof );

  Result := FQryFavorites;
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

end.

