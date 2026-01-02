unit UFeeds;

interface

uses
  System.Classes,
  System.SysUtils,
  FireDAC.Comp.Client,
  FireDAC.Stan.Param,
  Data.DB;

type
  TFeeds = class
  strict private
    FQryListeFeeds: TFDQuery;
    FQryCountFeeds: TFDQuery;

    FSelectListeFeeds: string;
  public
    constructor Create;
    destructor Destroy; override;

    function GetListeFeeds( aConnection: TFDConnection; aFirst, aSkip: Integer; aTitle, aOrderField, aOrder: string ): TFDQuery;
    function GetFeedsCount( aConnection: TFDConnection; aTitle: string ): Integer;
  end;

  TFeed = class
  strict private
    FAllContexts: string;
    FDisplayTemplate: string;
    FFeedGroup: Integer;
    FFeedId: Integer;
    FFeedName: string;
    FQryFeed: TFDQuery;
    FStatus: string;
    FTitle: string;

    procedure SetAllContexts( const Value: string );
    procedure SetDisplayTemplate( const Value: string );
    procedure SetFeedGroup( const Value: Integer );
    procedure SetFeedId( const Value: Integer );
    procedure SetFeedName( const Value: string );
    procedure SetStatus( const Value: string );
    procedure SetTitle( const Value: string );
  public
    constructor Create;
    destructor Destroy; override;

    function GetFeed( aConnection: TFDConnection; aFeedId: Integer ): TFDQuery;
    function UpdateFeed( aConnection: TFDConnection; const aFeedId: integer; const
      aName, aTitle, aStatus: string; const aGroup: Integer ): string;
    function CreateNewFeed( aConnection: TFDConnection; out aMsg: string ): Integer;
    function DeleteFeed( aConnection: TFDConnection; aFeedId: Integer ): string;
    function ChangeContext( aConnection: TFDConnection; aFeedId: Integer; aContext: string ): string;
    function GetTemplateName( aConnection: TFDConnection; aFeedId: Integer ): string;
    function SetTemplateName( aConnection: TFDConnection; aFeedId: Integer; aTemplateName: string ): string;

    property AllContexts: string read FAllContexts write SetAllContexts;
    property DisplayTemplate: string read FDisplayTemplate write SetDisplayTemplate;
    property FeedGroup: Integer read FFeedGroup write SetFeedGroup;
    property FeedId: Integer read FFeedId write SetFeedId;
    property FeedName: string read FFeedName write SetFeedName;
    /// <summary>
    ///   Statut du Feed (O:Actif; N: Inactif)
    /// </summary>
    property Status: string read FStatus write SetStatus;
    property Title: string read FTitle write SetTitle;
  end;

  TFeedCategory = class
  strict private
    FCategoryId: Integer;
    FFeedId: Integer;
    FQryCategories: TFDQuery;

    procedure SetCategoryId( const Value: Integer );
    procedure SetFeedId( const Value: Integer );
  public
    constructor Create;
    destructor Destroy; override;

    function GetCategories( aConnection: TFDConnection; aFeedId: Integer ): TFDQuery;
    function DeleteCategories( aConnection: TFDConnection; aFeedId: Integer ): string;
    function AddCategory( aConnection: TFDConnection; aFeedId, aCategoryId: Integer ): string;

    property CategoryId: Integer read FCategoryId write SetCategoryId;
    property FeedId: Integer read FFeedId write SetFeedId;
  end;

  TFeedSubCategory = class
  strict private
    FQrySubcategories: TFDQuery;
    FFeedId: Integer;
    FSubcategoryId: Integer;

    procedure SetFeedId( const Value: Integer );
    procedure SetSubcategoryId( const Value: Integer );
  public
    constructor Create;
    destructor Destroy; override;

    function GetSubcategories( aConnection: TFDConnection; aFeedId: Integer ): TFDQuery;
    function DeleteSubcategories( aConnection: TFDConnection; aFeedId: Integer ): string;
    function AddSubcategory( aConnection: TFDConnection; aFeedId, aSubcategoryId:
      Integer ): string;

    property FeedId: Integer read FFeedId write SetFeedId;
    property SubcategoryId: Integer read FSubcategoryId write SetSubcategoryId;
  end;

  TFeedCountry = class
  strict private
    FQryCountries: TFDQuery;
    FCountryCode: string;
    FFeedId: Integer;

    procedure SetCountryCode( const Value: string );
    procedure SetFeedId( const Value: Integer );
  public
    constructor Create;
    destructor Destroy; override;

    function GetCountries( aConnection: TFDConnection; aFeedId: Integer ): TFDQuery;
    function DeleteCountries( aConnection: TFDConnection; aFeedId: Integer ): string;
    function AddCountry( aConnection: TFDConnection; aCountryCode: string; aFeedId:
      Integer ): string;

    property CountryCode: string read FCountryCode write SetCountryCode;
    property FeedId: Integer read FFeedId write SetFeedId;
  end;

  TFeedLanguage = class
  strict private
    FQryLanguage: TFDQuery;
    FFeedId: Integer;
    FLanguageCode: string;

    procedure SetFeedId( const Value: Integer );
    procedure SetLanguageCode( const Value: string );
  public
    constructor Create;
    destructor Destroy; override;

    function GetLanguages( aConnection: TFDConnection; aFeedId: Integer ): TFDQuery;
    function DeleteLanguages( aConnection: TFDConnection; aFeedId: Integer ): string;
    function AddLanguage( aConnection: TFDConnection; aLanguageCode: string; aFeedId:
      Integer ): string;

    property FeedId: Integer read FFeedId write SetFeedId;
    property LanguageCode: string read FLanguageCode write SetLanguageCode;
  end;

  TFeedsUser = class
  strict private
    FQryFeedsUser: TFDQuery;
  public
    constructor Create;
    destructor Destroy; override;

    function GetFeedsUser( aConnection: TFDConnection; aFeedGroup, aCategoryId, aSubcategoryId: Integer; aCountryCode,
      aLanguageCode: string ): TFDQuery;
  end;

implementation

uses
  System.Variants;

{ TFeeds }

constructor TFeeds.Create;
begin
  FQryListeFeeds := TFDQuery.Create( nil );

  FSelectListeFeeds := '''
    SELECT first :FIRST skip :SKIP f.* FROM FEED_NEWS f
    where upper(f.TITLE) like :TITLE
  ''';

  FQryListeFeeds.Name := 'QryListeFeeds';
  FQryListeFeeds.SQL.Clear;
  FQryListeFeeds.SQL.Add( FSelectListeFeeds );
  FQryListeFeeds.SQL.Add( 'order by f.CREATION_DATE desc' );

  FQryCountFeeds := TFDQuery.Create( nil );
  FQryCountFeeds.Name := 'qryCountFeeds';
  FQryCountFeeds.SQL.Clear;
  FQryCountFeeds.SQL.Add( '''
   SELECT count(FEED_ID) as "NB_ENR" FROM FEED_NEWS
   where TITLE like :TITLE
  ''');
end;

destructor TFeeds.Destroy;
begin
  FreeAndNil( FQryListeFeeds );
  FreeAndNil( FQryCountFeeds );

  inherited;
end;

function TFeeds.GetFeedsCount( aConnection: TFDConnection;
  aTitle: string ): Integer;
begin
  FQryCountFeeds.Connection := aConnection;
  FQryCountFeeds.ParamByName( 'TITLE' ).AsString := aTitle;
  FQryCountFeeds.Open;

  Result := FQryCountFeeds.FieldByName( 'NB_ENR' ).AsInteger;

  FQryCountFeeds.Close;
end;

function TFeeds.GetListeFeeds( aConnection: TFDConnection; aFirst, aSkip:
  Integer; aTitle, aOrderField, aOrder: string ): TFDQuery;
begin
  FQryListeFeeds.SQL.Text := FSelectListeFeeds +
    if ( aOrder <> '' ) then
    ' order by ' + aOrderField + ' ' + aOrder
  else
    '';
  FQryListeFeeds.Connection := aConnection;
  FQryListeFeeds.ParamByName( 'FIRST' ).AsInteger := aFirst;
  FQryListeFeeds.ParamByName( 'SKIP' ).AsInteger := aSkip;
  FQryListeFeeds.ParamByName( 'TITLE' ).AsString := aTitle;
  FQryListeFeeds.Open;

  Result := FQryListeFeeds;
end;

{ TFeed }

function TFeed.ChangeContext( aConnection: TFDConnection; aFeedId: Integer;
  aContext: string ): string;
var
  LQry: TFDQuery;
begin
  LQry := GetFeed( aConnection, aFeedId );

  if not ( LQry.Eof ) then
  begin
    LQry.Edit;
    LQry.FieldByName( 'ALL_CONTEXTS' ).AsString := aContext;
    LQry.Post;
    LQry.Close;
  end
  else
  begin
    Result := 'Feed non trouvé.';
  end;
end;

constructor TFeed.Create;
begin
  FQryFeed := TFDQuery.Create( nil );
  FQryFeed.Name := 'qryFeed';
  FQryFeed.SQL.Clear;
  FQryFeed.SQL.Add( '''
    SELECT f.* FROM FEED_NEWS f
    where FEED_ID = :FEED_ID
  ''');

  FQryFeed.UpdateOptions.UpdateTableName := 'FEED_NEWS';
  FQryFeed.UpdateOptions.KeyFields := 'FEED_ID';
  FQryFeed.UpdateOptions.RequestLive := True;
end;

function TFeed.CreateNewFeed( aConnection: TFDConnection;
  out aMsg: string ): Integer;
begin
  FQryFeed.Open;
  try
    FQryFeed.Append;
    FQryFeed.FieldByName( 'FEED_ID' ).AsInteger := -1;
    FQryFeed.FieldByName( 'FEED_NAME' ).AsString := FFeedName;
    FQryFeed.FieldByName( 'TITLE' ).AsString := FTitle;
    FQryFeed.FieldByName( 'STATUS' ).AsString := FStatus;
    FQryFeed.FieldByName( 'DISPLAY_TEMPLATE' ).AsString := FDisplayTemplate;
    FQryFeed.FieldByName( 'FEED_GROUP' ).AsInteger := FFeedGroup;

    FQryFeed.Post;

    Result := aConnection.GetLastAutoGenValue( 'GEN_FEED' );

    aMsg := 'OK';
  except
    on e: exception do
    begin
      Result := -1;
      aMsg := e.Message;
    end;
  end;
end;

function TFeed.DeleteFeed( aConnection: TFDConnection; aFeedId: Integer ): string;
var
  LQry: TFDQuery;
begin
  LQry := GetFeed( aConnection, aFeedId );
  if not ( LQry.Eof ) then
  begin
    try
      LQry.Delete;

      Result := 'OK'
    except
      on e: Exception do
      begin
        Result := e.Message;
      end;
    end;
  end
  else
  begin
    Result := 'Feed non trouvé.';
  end;
end;

destructor TFeed.Destroy;
begin
  FreeAndNil( FQryFeed );

  inherited;
end;

function TFeed.GetFeed( aConnection: TFDConnection; aFeedId: Integer ): TFDQuery;
begin
  FQryFeed.Connection := aConnection;
  FQryFeed.Close;
  FQryFeed.ParamByName( 'FEED_ID' ).AsInteger := aFeedId;
  FQryFeed.Open;

  Result := FQryFeed;
end;

function TFeed.GetTemplateName( aConnection: TFDConnection;
  aFeedId: Integer ): string;
var
  LQry: TFDQuery;
begin
  LQry := GetFeed( aConnection, aFeedId );
  if not ( LQry.Eof ) then
  begin
    Result := LQry.FieldByName( 'DISPLAY_TEMPLATE' ).AsString;
  end
  else
  begin
    Result := '';
  end;
  LQry.Close;
end;

procedure TFeed.SetAllContexts( const Value: string );
begin
  FAllContexts := Value;
end;

procedure TFeed.SetDisplayTemplate( const Value: string );
begin
  FDisplayTemplate := Value;
end;

procedure TFeed.SetFeedGroup( const Value: Integer );
begin
  FFeedGroup := Value;
end;

procedure TFeed.SetFeedId( const Value: Integer );
begin
  FFeedId := Value;
end;

procedure TFeed.SetFeedName( const Value: string );
begin
  FFeedName := Value;
end;

procedure TFeed.SetStatus( const Value: string );
begin
  FStatus := Value;
end;

function TFeed.SetTemplateName( aConnection: TFDConnection; aFeedId: Integer;
  aTemplateName: string ): string;
var
  LQry: TFDQuery;
begin
  LQry := GetFeed( aConnection, aFeedId );

  if not ( LQry.Eof ) then
  begin
    LQry.Edit;
    LQry.FieldByName( 'DISPLAY_TEMPLATE' ).AsString := aTemplateName;
    LQry.Post;
    LQry.Close;
  end
  else
  begin
    Result := 'Feed non trouvé.';
  end;

end;

procedure TFeed.SetTitle( const Value: string );
begin
  FTitle := Value;
end;

function TFeed.UpdateFeed( aConnection: TFDConnection; const aFeedId: integer;
  const aName, aTitle, aStatus: string; const aGroup: Integer ): string;
var
  wQry: TFDQuery;
begin
  wQry := GetFeed( aConnection, aFeedId );

  if not ( wQry.Eof ) then
  begin

    wQry.Edit;

    wQry.FieldByName( 'FEED_NAME' ).AsString := aName;
    wQry.FieldByName( 'TITLE' ).AsString := aTitle;
    wQry.FieldByName( 'STATUS' ).AsString := aStatus;
    wQry.FieldByName( 'FEED_GROUP' ).AsInteger := aGroup;
    //          LDM.qryFeedsTEMPLATE_AFFICHAGE.Value := LFileName;
    try
      wQry.Post;
      aConnection.Commit;

      Result := 'OK';
    except
      on e: Exception do
      begin
        Result := 'ERR:' + aFeedId.ToString;
        aConnection.Rollback;
      end;
    end;
  end;

  wQry.Close;
end;

function TFeedCategory.AddCategory( aConnection: TFDConnection; aFeedId,
  aCategoryId: Integer ): string;
var
  LQry: TFDQuery;
begin
  LQry := GetCategories( aConnection, -1 );
  try
    LQry.Append;
    LQry.FieldByName( 'FEED_ID' ).AsInteger := aFeedId;
    LQry.FieldByName( 'CATEGORY_ID' ).AsInteger := aCategoryId;
    LQry.Post;

    Result := 'OK';
  except
    on e: Exception do
    begin
      Result := e.Message;
    end;
  end;
end;

constructor TFeedCategory.Create;
begin
  FQryCategories := TFDQuery.Create( nil );
  FQryCategories.Name := 'QryFeedCategories';
  FQryCategories.SQL.Clear;
  FQryCategories.SQL.Add( '''
    select * from FEED_CONTEXT_CATEGORY
    where FEED_ID = :FEED_ID
  ''');

  FQryCategories.UpdateOptions.UpdateTableName := 'FEED_CONTEXT_CATEGORY';
  FQryCategories.UpdateOptions.KeyFields := 'FEED_ID';
  FQryCategories.UpdateOptions.RequestLive := True;
end;

function TFeedCategory.DeleteCategories( aConnection: TFDConnection;
  aFeedId: Integer ): string;
var
  LQry: TFDQuery;
begin
  LQry := GetCategories( aConnection, aFeedId );
  if not ( LQry.Eof ) then
  begin
    try
      while not ( LQry.Eof ) do
      begin
        LQry.Delete;
      end;

      Result := 'OK'
    except
      on e: Exception do
      begin
        Result := e.Message;
      end;
    end;
  end
  else
  begin
    Result := 'Feed non trouvé.';
  end;
end;

destructor TFeedCategory.Destroy;
begin
  FreeAndNil( FQryCategories );

  inherited;
end;

function TFeedCategory.GetCategories( aConnection: TFDConnection;
  aFeedId: Integer ): TFDQuery;
begin
  FQryCategories.Connection := aConnection;
  FQryCategories.Close;
  FQryCategories.ParamByName( 'FEED_ID' ).AsInteger := aFeedId;
  FQryCategories.Open;

  Result := FQryCategories;
end;

procedure TFeedCategory.SetCategoryId( const Value: Integer );
begin
  FCategoryId := Value;
end;

procedure TFeedCategory.SetFeedId( const Value: Integer );
begin
  FFeedId := Value;
end;

function TFeedSubCategory.AddSubcategory( aConnection: TFDConnection; aFeedId,
  aSubcategoryId: Integer ): string;
var
  LQry: TFDQuery;
begin
  LQry := GetSubcategories( aConnection, -1 );
  try
    LQry.Append;
    LQry.FieldByName( 'FEED_ID' ).AsInteger := aFeedId;
    LQry.FieldByName( 'SUBCATEGORY_ID' ).AsInteger := aSubcategoryId;
    LQry.Post;

    Result := 'OK';
  except
    on e: Exception do
    begin
      Result := e.Message;
    end;
  end;
end;

constructor TFeedSubCategory.Create;
begin
  FQrySubcategories := TFDQuery.Create( nil );
  FQrySubcategories.Name := 'QryFeedCategories';
  FQrySubcategories.SQL.Clear;
  FQrySubcategories.SQL.Add( '''
    select * from FEED_CONTEXT_SUBCATEGORY
    where FEED_ID = :FEED_ID
  ''');

  FQrySubcategories.UpdateOptions.UpdateTableName := 'FEED_CONTEXT_SUBCATEGORY';
  FQrySubcategories.UpdateOptions.KeyFields := 'FEED_ID';
  FQrySubcategories.UpdateOptions.RequestLive := True;
end;

function TFeedSubCategory.DeleteSubcategories( aConnection: TFDConnection;
  aFeedId: Integer ): string;
var
  LQry: TFDQuery;
begin
  LQry := GetSubcategories( aConnection, aFeedId );
  if not ( LQry.Eof ) then
  begin
    try
      while not ( LQry.Eof ) do
      begin
        LQry.Delete;
      end;

      Result := 'OK'
    except
      on e: Exception do
      begin
        Result := e.Message;
      end;
    end;
  end
  else
  begin
    Result := 'Feed non trouvé.';
  end;
end;

destructor TFeedSubCategory.Destroy;
begin
  FreeAndNil( FQrySubcategories );

  inherited;
end;

function TFeedSubCategory.GetSubcategories( aConnection: TFDConnection;
  aFeedId: Integer ): TFDQuery;
begin
  FQrySubcategories.Connection := aConnection;
  FQrySubcategories.Close;
  FQrySubcategories.ParamByName( 'FEED_ID' ).AsInteger := aFeedId;
  FQrySubcategories.Open;

  Result := FQrySubcategories;
end;

procedure TFeedSubCategory.SetFeedId( const Value: Integer );
begin
  FFeedId := Value;
end;

procedure TFeedSubCategory.SetSubcategoryId( const Value: Integer );
begin
  FSubcategoryId := Value;
end;

function TFeedCountry.AddCountry( aConnection: TFDConnection; aCountryCode: string; aFeedId: Integer ): string;
var
  LQry: TFDQuery;
begin
  LQry := GetCountries( aConnection, -1 );
  try
    LQry.Append;
    LQry.FieldByName( 'COUNTRY_CODE' ).AsString := aCountryCode;
    LQry.FieldByName( 'FEED_ID' ).AsInteger := aFeedId;
    LQry.Post;

    Result := 'OK';
  except
    on e: Exception do
    begin
      Result := e.Message;
    end;
  end;
end;

constructor TFeedCountry.Create;
begin
  FQryCountries := TFDQuery.Create( nil );
  FQryCountries.Name := 'QryFeedCountry';
  FQryCountries.SQL.Clear;
  FQryCountries.SQL.Add( '''
    select * from FEED_CONTEXT_COUNTRY
    where FEED_ID = :FEED_ID
  ''');

  FQryCountries.UpdateOptions.UpdateTableName := 'FEED_CONTEXT_COUNTRY';
  FQryCountries.UpdateOptions.KeyFields := 'FEED_ID';
  FQryCountries.UpdateOptions.RequestLive := True;
end;

function TFeedCountry.DeleteCountries( aConnection: TFDConnection;
  aFeedId: Integer ): string;
var
  LQry: TFDQuery;
begin
  LQry := GetCountries( aConnection, aFeedId );
  if not ( LQry.Eof ) then
  begin
    try
      while not ( LQry.Eof ) do
      begin
        LQry.Delete;
      end;

      Result := 'OK'
    except
      on e: Exception do
      begin
        Result := e.Message;
      end;
    end;
  end
  else
  begin
    Result := 'Feed non trouvé.';
  end;
end;

destructor TFeedCountry.Destroy;
begin
  FreeAndNil( FQryCountries );

  inherited;
end;

function TFeedCountry.GetCountries( aConnection: TFDConnection;
  aFeedId: Integer ): TFDQuery;
begin
  FQryCountries.Connection := aConnection;
  FQryCountries.Close;
  FQryCountries.ParamByName( 'FEED_ID' ).AsInteger := aFeedId;
  FQryCountries.Open;

  Result := FQryCountries;
end;

procedure TFeedCountry.SetCountryCode( const Value: string );
begin
  FCountryCode := Value;
end;

procedure TFeedCountry.SetFeedId( const Value: Integer );
begin
  FFeedId := Value;
end;

function TFeedLanguage.AddLanguage( aConnection: TFDConnection;
  aLanguageCode: string; aFeedId: Integer ): string;
var
  LQry: TFDQuery;
begin
  LQry := GetLanguages( aConnection, -1 );
  try
    LQry.Append;
    LQry.FieldByName( 'LANGUAGE_CODE' ).AsString := aLanguageCode;
    LQry.FieldByName( 'FEED_ID' ).AsInteger := aFeedId;
    LQry.Post;

    Result := 'OK';
  except
    on e: Exception do
    begin
      Result := e.Message;
    end;
  end;

end;

constructor TFeedLanguage.Create;
begin
  FQryLanguage := TFDQuery.Create( nil );
  FQryLanguage.Name := 'QryFeedLanguage';
  FQryLanguage.SQL.Clear;
  FQryLanguage.SQL.Add( '''
    select * from FEED_CONTEXT_LANG
    where FEED_ID = :FEED_ID
  ''');

  FQryLanguage.UpdateOptions.UpdateTableName := 'FEED_CONTEXT_LANG';
  FQryLanguage.UpdateOptions.KeyFields := 'FEED_ID';
  FQryLanguage.UpdateOptions.RequestLive := True;
end;

function TFeedLanguage.DeleteLanguages( aConnection: TFDConnection;
  aFeedId: Integer ): string;
var
  LQry: TFDQuery;
begin
  LQry := GetLanguages( aConnection, aFeedId );
  if not ( LQry.Eof ) then
  begin
    try
      while not ( LQry.Eof ) do
      begin
        LQry.Delete;
      end;

      Result := 'OK'
    except
      on e: Exception do
      begin
        Result := e.Message;
      end;
    end;
  end
  else
  begin
    Result := 'Feed non trouvé.';
  end;
end;

destructor TFeedLanguage.Destroy;
begin
  FreeAndNil( FQryLanguage );

  inherited;
end;

function TFeedLanguage.GetLanguages( aConnection: TFDConnection;
  aFeedId: Integer ): TFDQuery;
begin
  FQryLanguage.Connection := aConnection;
  FQryLanguage.Close;
  FQryLanguage.ParamByName( 'FEED_ID' ).AsInteger := aFeedId;
  FQryLanguage.Open;

  Result := FQryLanguage;
end;

procedure TFeedLanguage.SetFeedId( const Value: Integer );
begin
  FFeedId := Value;
end;

procedure TFeedLanguage.SetLanguageCode( const Value: string );
begin
  FLanguageCode := Value;
end;

{ TFeedsUser }

constructor TFeedsUser.Create;
begin
  FQryFeedsUser := TFDQuery.Create( nil );
  FQryFeedsUser.Name := 'QryFeedsUser';
  FQryFeedsUser.SQL.Clear;
  FQryFeedsUser.SQL.Add( '''
    SELECT distinct r.FEED_ID, r.FEED_GROUP, r.FEED_NAME, r.TITLE, r.DISPLAY_TEMPLATE
    FROM FEED_NEWS r
    join NEWS n on (n.FEED_ID = r.FEED_ID)
    join NEWS_CONTEXT_CATEGORY cc on (cc.NEWS_ID = n.NEWS_ID)
    join NEWS_CONTEXT_SUBCATEGORY sc on (sc.NEWS_ID = n.NEWS_ID)
    join NEWS_CONTEXT_COUNTRY cp on (cp.NEWS_ID = n.NEWS_ID)
    join NEWS_CONTEXT_LANG cl on (cl.NEWS_ID = n.NEWS_ID)
    where r.STATUS = 'O'
    and r.FEED_GROUP = :FEED_GROUP
    and cc.CATEGORY_ID = :CATEGORY_ID
    and sc.SUBCATEGORY_ID = :SUBCATEGORY_ID
    and cp.COUNTRY_CODE = :COUNTRY_CODE
    and cl.LANGUAGE_CODE = :LANGUAGE_CODE
  ''');

  FQryFeedsUser.UpdateOptions.RequestLive := True;
end;

destructor TFeedsUser.Destroy;
begin
  FreeAndNil( FQryFeedsUser );

  inherited;
end;

function TFeedsUser.GetFeedsUser( aConnection: TFDConnection; aFeedGroup,
  aCategoryId, aSubcategoryId: Integer; aCountryCode,
  aLanguageCode: string ): TFDQuery;
begin
  FQryFeedsUser.Connection := aConnection;
  FQryFeedsUser.Close;
  FQryFeedsUser.ParamByName( 'FEED_GROUP' ).AsSmallInt := aFeedGroup;
  FQryFeedsUser.ParamByName( 'CATEGORY_ID' ).AsInteger := aCategoryId;
  FQryFeedsUser.ParamByName( 'SUBCATEGORY_ID' ).AsInteger := aSubcategoryId;
  FQryFeedsUser.ParamByName( 'COUNTRY_CODE' ).AsString := aCountryCode;
  FQryFeedsUser.ParamByName( 'LANGUAGE_CODE' ).AsString := aLanguageCode;
  FQryFeedsUser.Open;

  Result := FQryFeedsUser;
end;

end.

