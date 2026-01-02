/// <summary>
///   Unié modèle pour les news
/// </summary>
unit UNews;

interface

uses
  System.Classes,
  System.SysUtils,
  FireDAC.Comp.Client,
  Firedac.Stan.Param,
  Data.DB,
  System.JSON;

type
  /// <summary>
  ///   Liste des news
  /// </summary>
  TListNews = class
  strict private
    /// <summary>
    ///   Qry retournant la liste des news
    /// </summary>
    FQryNewsList: TFDQuery;
    /// <summary>
    ///   Qry retrounant le nombre d'rengistrements dans la sélection
    /// </summary>
    FQryCountNews: TFDQuery;

    /// <summary>
    ///   Requête SQL de base (avant filtre et tri)
    /// </summary>
    FSelectNewsList: string;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    ///   Méthode retournant la liste des news
    /// </summary>
    function GetNewslist( aConnection: TFDConnection; aFirst, aSkip, aFeedId:
      Integer; aTitle, aFilter, aOrderField, aOrder: string; aCreationDate,
      aPublicationDate: TDateTime ): TFDQuery;
    /// <summary>
    ///   Méthode retournant le nombre de news dans la liste
    /// </summary>
    function GetNewsCount( aConnection: TFDConnection; aFeedId: Integer; aTitle,
      aFilter: string; aCreationDate, aPublicationDate: TDateTime ): Integer;
  end;

  /// <summary>
  ///   News
  /// </summary>
  /// <remarks>
  ///   Une news sera affichée entre la date de publication et la date
  ///   d'expiration incluses.
  /// </remarks>
  TNews = class
  strict private
    /// <summary>
    ///   Query retournant les inforomations de la news
    /// </summary>
    FQryNews: TFDQuery;
  private
    FCreationDate: TDateTime;
    FDisplayOrder: Integer;
    FExpiryDate: TDateTime;
    FFeedId: Integer;
    FHold: string;
    FModificationDate: TDateTime;
    FNewsId: Integer;
    FNewsTitle: string;
    FPublicationDate: TDateTime;
    FText: string;

    procedure QryNewsCalcFields( DataSet: TDataSet );
    procedure SetCreationDate( const Value: TDateTime );
    procedure SetDisplayOrder( const Value: Integer );
    procedure SetExpiryDate( const Value: TDateTime );
    procedure SetFeedId( const Value: Integer );
    procedure SetHold( const Value: string );
    procedure SetModificationDate( const Value: TDateTime );
    procedure SetNewsId( const Value: Integer );
    procedure SetNewsTitle( const Value: string );
    procedure SetPublicationDate( const Value: TDateTime );
    procedure SetText( const Value: string );
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    ///   Lecture d'une news
    /// </summary>
    function GetNews( aConnection: TFDConnection; aNewsId: Integer ): TFDQuery;
    /// <summary>
    ///   Ajout d'une news
    /// </summary>
    function CreateNews( aConnection: TFDConnection; out aMsg: string ): Integer;
    /// <summary>
    ///   Mise à jour d'une news
    /// </summary>
    function UpdateNews( aConnection: TFDConnection ): string;
    /// <summary>
    ///   Suppresion d'une news
    /// </summary>
    function DeleteNews( aConnection: TFDConnection; aNewsId: Integer ): string;
    function SetContentNews( aConnection: TFDConnection; aNewsId: Integer; aContentText: string ): string;
    /// <summary>
    ///   Lecture du détail d'une news (catégorie, sous-catégorie, ...)
    /// </summary>
    function GetNewsDetails( aConnection: TFDConnection; aNewsId: Integer ): TJSONObject;

    /// <summary>
    ///   Date de création de la news
    /// </summary>
    property CreationDate: TDateTime read FCreationDate write SetCreationDate;
    /// <summary>
    ///   Ordre d'affichage si imposé
    /// </summary>
    property DisplayOrder: Integer read FDisplayOrder write SetDisplayOrder;
    /// <summary>
    ///   date d'expiration
    /// </summary>
    property ExpiryDate: TDateTime read FExpiryDate write SetExpiryDate;
    /// <summary>
    ///   Id du feed auquel appartient la news
    /// </summary>
    property FeedId: Integer read FFeedId write SetFeedId;
    /// <summary>
    ///   Affichée ou pas (O/N)
    /// </summary>
    property Hold: string read FHold write SetHold;
    /// <summary>
    ///   Date de modification
    /// </summary>
    property ModificationDate: TDateTime read FModificationDate write
      SetModificationDate;
    /// <summary>
    ///   Id de la news
    /// </summary>
    property NewsId: Integer read FNewsId write SetNewsId;
    /// <summary>
    ///   Titre
    /// </summary>
    property NewsTitle: string read FNewsTitle write SetNewsTitle;
    /// <summary>
    ///   Date de publication
    /// </summary>
    property PublicationDate: TDateTime read FPublicationDate write
      SetPublicationDate;
    /// <summary>
    ///   Contenu textuel
    /// </summary>
    property Text: string read FText write SetText;
  end;

  /// <summary>
  ///   Prévisualisation de la news
  /// </summary>
  TShowNews = class
  strict private
    FQryShowNews: TFDQuery;

    procedure QryShowNewsCalcFields( DataSet: TDataSet );
  public
    constructor Create;
    destructor Destroy; override;

    function GetNews( aConnection: TFDConnection; aFeedId: Integer ): TFDQuery;
  end;

  /// <summary>
  ///   Affichage utilisateur de la news <br />
  /// </summary>
  TShowNewsUser = class
  strict private
    FQryShowNewsUser: TFDQuery;

    procedure QryShowNewsUserCalcFields( DataSet: TDataSet );
  public
    constructor Create;
    destructor Destroy; override;

    function GetNews( aConnection: TFDConnection; aFeedId, aCategoryId, aSubcategoryId: Integer; aCountryCode, aLanguageCode:
      string ): TFDQuery;
  end;

  /// <summary>
  ///   Categories auxquelles la news est attachée
  /// </summary>
  TNewsCategory = class
  strict private
    /// <summary>
    ///   Id catégorie
    /// </summary>
    FCategoryId: Integer;
    /// <summary>
    ///   Id news
    /// </summary>
    FNewsId: Integer;
    /// <summary>
    ///   Query retournant la liste des catégoeirs attachées à la news
    /// </summary>
    FQryCategories: TFDQuery;

    procedure SetCategoryId( const Value: Integer );
    procedure SetNewsId( const Value: Integer );
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    ///   liste les catégories attachées à la news
    /// </summary>
    function GetCategories( aConnection: TFDConnection; aNewsId: Integer ): TFDQuery;
    /// <summary>
    ///   Supprime les catégories liées à la news
    /// </summary>
    function DeleteCategories( aConnection: TFDConnection; aNewsId: Integer ): string;
    /// <summary>
    ///   Ajoute une catégorie
    /// </summary>
    function AddCategory( aConnection: TFDConnection; aNewsId, aCategoryId: Integer ): string;

    /// <summary>
    ///   Id de la catégorie
    /// </summary>
    property CategoryId: Integer read FCategoryId write SetCategoryId;
    /// <summary>
    ///   Id de la news
    /// </summary>
    property NewsId: Integer read FNewsId write SetNewsId;
  end;

  /// <summary>
  ///   Sous-catégories auxquelles la news est attachée
  /// </summary>
  TNewsSubCategory = class
  strict private
    /// <summary>
    ///   Query retournant la liste des sous-catégories liées à la news
    /// </summary>
    FQrySubcategories: TFDQuery;
    FNewsId: Integer;
    FSubcategoryId: Integer;

    procedure SetNewsId( const Value: Integer );
    procedure SetSubcategoryId( const Value: Integer );
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    ///   Liste les sou-catégories liées à la news
    /// </summary>
    function GetSubcategories( aConnection: TFDConnection; aNewsId: Integer ): TFDQuery;
    /// <summary>
    ///   Supprimer les sous-catégorie liées à la news
    /// </summary>
    function DeleteSubcategories( aConnection: TFDConnection; aNewsId: Integer ): string;
    /// <summary>
    ///   Ajouter une sous-catégorie
    /// </summary>
    function AddSubcategory( aConnection: TFDConnection; aNewsId, aSubcategoryId:
      Integer ): string;

    /// <summary>
    ///   id de la news
    /// </summary>
    property NewsId: Integer read FNewsId write SetNewsId;
    /// <summary>
    ///   id de la sous-catégorie
    /// </summary>
    property SubcategoryId: Integer read FSubcategoryId write SetSubcategoryId;
  end;

  /// <summary>
  ///   Pays auxquels la news est attachée
  /// </summary>
  TNewsCountry = class
  strict private
    /// <summary>
    ///   Query retournant la liste des pays liés à la news
    /// </summary>
    FQryCountries: TFDQuery;
    FCountryCode: string;
    FNewsId: Integer;

    procedure SetCountryCode( const Value: string );
    procedure SetNewsId( const Value: Integer );
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    ///   Liste les pays liés à la news
    /// </summary>
    function GetCountries( aConnection: TFDConnection; aNewsId: Integer ): TFDQuery;
    /// <summary>
    ///   Supprimer les pays liés à la news pays
    /// </summary>
    function DeleteCountries( aConnection: TFDConnection; aNewsId: Integer ): string;
    /// <summary>
    ///   Ajouter un pays
    /// </summary>
    function AddCountry( aConnection: TFDConnection; aCountryCode: string; aNewsId:
      Integer ): string;

    /// <summary>
    ///   Code pays
    /// </summary>
    property CountryCode: string read FCountryCode write SetCountryCode;
    /// <summary>
    ///   News ID
    /// </summary>
    property NewsId: Integer read FNewsId write SetNewsId;
  end;

  /// <summary>
  ///   langue auxquelles la news est attachée
  /// </summary>
  TNewsLanguage = class
  strict private
    /// <summary>
    ///   Query retournant la liste des langues associées à la news
    /// </summary>
    FQryLanguage: TFDQuery;
    FNewsId: Integer;
    FLanguageCode: string;

    procedure SetNewsId( const Value: Integer );
    procedure SetLanguageCode( const Value: string );
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    ///   Liste les langues associées à la news
    /// </summary>
    function GetLanguages( aConnection: TFDConnection; aNewsId: Integer ): TFDQuery;
    /// <summary>
    ///   Supprimer les langues asscoiées à la news
    /// </summary>
    function DeleteLanguages( aConnection: TFDConnection; aNewsId: Integer ): string;
    /// <summary>
    ///   Ajouter une langue
    /// </summary>
    function AddLanguage( aConnection: TFDConnection; aLanguageCode: string; aNewsId:
      Integer ): string;

    /// <summary>
    ///   Id de la news
    /// </summary>
    property NewsId: Integer read FNewsId write SetNewsId;
    /// <summary>
    ///   Code langue
    /// </summary>
    property LanguageCode: string read FLanguageCode write SetLanguageCode;
  end;

  /// <summary>
  ///   Groupe d'affichage de la news
  /// </summary>
  TShowGroup = class
  strict private
    FQryShowGroup: TFDQuery;

  public
    constructor Create;
    destructor Destroy; override;

    function GetGroup( aConnection: TFDConnection; aFeedId: Integer ): TFDQuery;
  end;

implementation

{ TListNews }

constructor TListNews.Create;
begin
  FQryNewsList := TFDQuery.Create( nil );

  FSelectNewsList := '''
    SELECT first :FIRST skip :SKIP n.* FROM NEWS n
    where FEED_ID = :FEED_ID
    and (upper(NEWS_TITLE) like :NEWS_TITLE
    or CREATION_DATE = :CREATION_DATE
    or PUBLICATION_DATE = :PUBLICATION_DATE)
  ''';

  FQryNewsList.Name := 'QryListNews';
  FQryNewsList.SQL.Clear;
  FQryNewsList.SQL.Add( FSelectNewsList );

  FQryCountNews := TFDQuery.Create( nil );
  FQryCountNews.Name := 'QryCountNews';
  FQryCountNews.SQL.Clear;
  FQryCountNews.SQL.Add( '''
    SELECT count(FEED_ID) as "NB_ENR" FROM NEWS
    where FEED_ID = :FEED_ID
    and (upper(NEWS_TITLE) like :NEWS_TITLE
    or CREATION_DATE = :CREATION_DATE
    or PUBLICATION_DATE = :PUBLICATION_DATE)
  ''');
end;

destructor TListNews.Destroy;
begin
  FreeAndNil( FQryNewsList );
  FreeAndNil( FQryCountNews );

  inherited;
end;

function TListNews.GetNewsCount( aConnection: TFDConnection; aFeedId: Integer;
  aTitle, aFilter: string; aCreationDate, aPublicationDate: TDateTime ): Integer;
begin
  FQryCountNews.Connection := aConnection;
  FQryCountNews.ParamByName( 'FEED_ID' ).AsInteger := aFeedId;
  FQryCountNews.ParamByName( 'NEWS_TITLE' ).AsString := aTitle;
  FQryCountNews.ParamByName( 'CREATION_DATE' ).AsDateTime := aCreationDate;
  FQryCountNews.ParamByName( 'PUBLICATION_DATE' ).AsDateTime := aPublicationDate;
  FQryCountNews.Open;

  Result := FQryCountNews.FieldByName( 'NB_ENR' ).AsInteger;

  FQryCountNews.Close;
end;

function TListNews.GetNewslist( aConnection: TFDConnection; aFirst, aSkip,
  aFeedId: Integer; aTitle, aFilter, aOrderField, aOrder: string; aCreationDate,
  aPublicationDate: TDateTime ): TFDQuery;
begin
  FQryNewsList.SQL.Text := FSelectNewsList + aFilter +
    if ( aOrder <> '' ) then
    ' order by ' + aOrderField + ' ' + aOrder
  else
    '';
  FQryNewsList.Connection := aConnection;
  FQryNewsList.ParamByName( 'FEED_ID' ).AsInteger := aFeedId;
  FQryNewsList.ParamByName( 'FIRST' ).AsInteger := aFirst;
  FQryNewsList.ParamByName( 'SKIP' ).AsInteger := aSkip;
  FQryNewsList.ParamByName( 'NEWS_TITLE' ).AsString := aTitle;
  FQryNewsList.ParamByName( 'CREATION_DATE' ).AsDateTime := aCreationDate;
  FQryNewsList.ParamByName( 'PUBLICATION_DATE' ).AsDateTime := aPublicationDate;
  FQryNewsList.Open;

  Result := FQryNewsList;
end;

{ TNews }

constructor TNews.Create;
begin
  FQryNews := TFDQuery.Create( nil );
  FQryNews.Name := 'qryNews1';
  FQryNews.SQL.Clear;
  FQryNews.SQL.Add( '''
    SELECT NEWS_ID,
    PUBLICATION_DATE,
    EXPIRY_DATE,
    DISPLAY_ORDER,
    HOLD,
    NEWS_TITLE,
    TEXT,
    FEED_ID,
    CREATION_DATE,
    MODIFICATION_DATE
    FROM NEWS n
    where NEWS_ID = :NEWS_ID
  ''');

  with TIntegerField.Create( FQryNews ) do
  begin
    FieldName := 'NEWS_ID';
    ProviderFlags := [ pfInKey ];
    Required := True;
    DataSet := FQryNews;
  end;

  with TDateField.Create( FQryNews ) do
  begin
    FieldName := 'PUBLICATION_DATE';
    DataSet := FQryNews;
  end;

  with TDateField.Create( FQryNews ) do
  begin
    FieldName := 'EXPIRY_DATE';
    DataSet := FQryNews;
  end;

  with TIntegerField.Create( FQryNews ) do
  begin
    FieldName := 'DISPLAY_ORDER';
    DataSet := FQryNews;
  end;

  with TWideStringField.Create( FQryNews ) do
  begin
    FieldName := 'HOLD';
    FixedChar := True;
    Size := 1;
    DataSet := FQryNews;
  end;

  with TWideStringField.Create( FQryNews ) do
  begin
    FieldName := 'NEWS_TITLE';
    Size := 500;
    DataSet := FQryNews;
  end;

  with TWideMemoField.Create( FQryNews ) do
  begin
    FieldName := 'TEXT';
    BlobType := ftMemo;
    DataSet := FQryNews;
  end;

  with TIntegerField.Create( FQryNews ) do
  begin
    FieldName := 'FEED_ID';
    DataSet := FQryNews;
  end;

  with TDateField.Create( FQryNews ) do
  begin
    FieldName := 'CREATION_DATE';
    DataSet := FQryNews;
  end;

  with TSQLTimeStampField.Create( FQryNews ) do
  begin
    FieldName := 'MODIFICATION_DATE';
    DataSet := FQryNews;
  end;

  //  Champ calculé
  with TStringField.Create( FQryNews ) do
  begin
    FieldKind := fkCalculated;
    FieldName := 'PUBLICATION_DATE_FMT';
    Size := 10;
    DataSet := FQryNews;
  end;

  with TStringField.Create( FQryNews ) do
  begin
    FieldKind := fkCalculated;
    FieldName := 'EXPIRY_DATE_FMT';
    Size := 10;
    DataSet := FQryNews;
  end;

  FQryNews.OnCalcFields := QryNewsCalcFields;

  // Options d’update
  FQryNews.UpdateOptions.UpdateTableName := 'NEWS';
  FQryNews.UpdateOptions.KeyFields := 'NEWS_ID';
end;

function TNews.CreateNews( aConnection: TFDConnection;
  out aMsg: string ): Integer;
begin
  FQryNews.Open;
  try
    FQryNews.Append;
    FQryNews.FieldByName( 'NEWS_ID' ).AsInteger := FNewsId;
    FQryNews.FieldByName( 'NEWS_TITLE' ).AsString := FNewsTitle;
    FQryNews.FieldByName( 'DISPLAY_ORDER' ).AsInteger := FDisplayOrder;
    FQryNews.FieldByName( 'HOLD' ).AsString := FHold;
    FQryNews.FieldByName( 'PUBLICATION_DATE' ).AsDateTime := FPublicationDate;
    FQryNews.FieldByName( 'EXPIRY_DATE' ).AsDateTime := FExpiryDate;
    FQryNews.FieldByName( 'TEXT' ).AsString := FText;
    FQryNews.FieldByName( 'FEED_ID' ).AsInteger := FFeedId;
    FQryNews.Post;

    Result := aConnection.GetLastAutoGenValue( 'GEN_NEWS' );

    aMsg := 'OK';
  except
    on e: exception do
    begin
      Result := -1;
      aMsg := e.Message;
    end;
  end;
end;

function TNews.DeleteNews( aConnection: TFDConnection; aNewsId: Integer ): string;
var
  LQry: TFDQuery;
begin
  LQry := GetNews( aConnection, aNewsId );

  if not ( LQry.Eof ) then
  begin
    LQry.Delete;

    Result := 'OK';
  end
  else
  begin
    Result := 'News non trouvées.';
  end;

  LQry.Close;
end;

destructor TNews.Destroy;
begin
  FreeAndNil( FQryNews );

  inherited;
end;

function TNews.GetNews( aConnection: TFDConnection; aNewsId: Integer ): TFDQuery;
begin
  FQryNews.Connection := aConnection;
  FQryNews.Close;
  FQryNews.ParamByName( 'NEWS_ID' ).AsInteger := aNewsId;
  FQryNews.Open;

  FFeedId := FQryNews.FieldByName( 'FEED_ID' ).AsInteger;

  Result := FQryNews;
end;

function TNews.GetNewsDetails( aConnection: TFDConnection;
  aNewsId: Integer ): TJSONObject;
var
  LQryNews: TFDQuery;
  LNewsCategory: TNewsCategory;
  LNewsSubcategory: TNewsSubCategory;
  LNewsCountry: TNewsCountry;
  LNewsLanguage: TNewsLanguage;
  LQry: TFDQuery;
  LArray: TJSONArray;
begin
  Result := TJSONObject.Create;

  LQryNews := GetNews( aConnection, aNewsId );

  if not ( LQryNews.Eof ) then
  begin
    LQryNews.First;

    Result.AddPair( 'content', LQryNews.FieldByName( 'TEXT' ).AsString );

    // On envoi les catégories
    LArray := TJSONArray.Create;
    LNewsCategory := TNewsCategory.Create;
    LQry := LNewsCategory.GetCategories( aConnection, aNewsId );

    while not ( LQry.Eof ) do
    begin
      LArray.Add( LQry.FieldByName( 'CATEGORY_ID' ).AsInteger );

      LQry.Next;
    end;

    FreeAndNil( LNewsCategory );

    Result.AddPair( 'Category', LArray );

    // On envoi les sous-catégories
    LArray := TJSONArray.Create;
    LNewsSubcategory := TNewsSubCategory.Create;
    LQry := LNewsSubcategory.GetSubcategories( aConnection, aNewsId );

    while not ( LQry.Eof ) do
    begin
      LArray.Add( LQry.FieldByName( 'SUBCATEGORY_ID' ).AsInteger );

      LQry.Next;
    end;

    FreeAndNil( LNewsSubcategory );

    Result.AddPair( 'Subcategory', LArray );

    // On envoi les pays
    LArray := TJSONArray.Create;

    LNewsCountry := TNewsCountry.Create;
    LQry := LNewsCountry.GetCountries( aConnection, aNewsId );

    while not ( LQry.Eof ) do
    begin
      LArray.Add( LQry.FieldByName( 'COUNTRY_CODE' ).AsString );

      LQry.Next;
    end;

    FreeAndNil( LNewsCountry );

    Result.AddPair( 'Country', LArray );

    // On envoi les langues
    LArray := TJSONArray.Create;

    LNewsLanguage := TNewsLanguage.Create;
    LQry := LNewsLanguage.GetLanguages( aConnection, aNewsId );

    while not ( LQry.Eof ) do
    begin
      LArray.Add( LQry.FieldByName( 'LANGUAGE_CODE' ).AsString );

      LQry.Next;
    end;

    FreeAndNil( LNewsLanguage );

    Result.AddPair( 'Lang', LArray );
  end;
end;

procedure TNews.QryNewsCalcFields( DataSet: TDataSet );
begin
  DataSet.FieldByName( 'PUBLICATION_DATE_FMT' ).AsString := FormatDateTime( 'YYYY-MM-DD', DataSet.FieldByName( 'PUBLICATION_DATE'
    ).AsDateTime );
  DataSet.FieldByName( 'EXPIRY_DATE_FMT' ).AsString := FormatDateTime( 'YYYY-MM-DD', DataSet.FieldByName( 'EXPIRY_DATE'
    ).AsDateTime );
end;

function TNews.SetContentNews( aConnection: TFDConnection; aNewsId: Integer;
  aContentText: string ): string;
var
  wQry: TFDQuery;
begin
  wQry := GetNews( aConnection, aNewsId );

  if not ( wQry.Eof ) then
  begin
    wQry.Edit;
    wQry.FieldByName( 'TEXT' ).AsString := aContentText;
    wQry.Post;
    wQry.Close;
    Result := 'OK';
  end
  else
  begin
    Result := 'News non trouvée.';
  end;
end;

procedure TNews.SetCreationDate( const Value: TDateTime );
begin
  FCreationDate := Value;
end;

procedure TNews.SetDisplayOrder( const Value: Integer );
begin
  FDisplayOrder := Value;
end;

procedure TNews.SetExpiryDate( const Value: TDateTime );
begin
  FExpiryDate := Value;
end;

procedure TNews.SetFeedId( const Value: Integer );
begin
  FFeedId := Value;
end;

procedure TNews.SetHold( const Value: string );
begin
  FHold := Value;
end;

procedure TNews.SetModificationDate( const Value: TDateTime );
begin
  FModificationDate := Value;
end;

procedure TNews.SetNewsId( const Value: Integer );
begin
  FNewsId := Value;
end;

procedure TNews.SetNewsTitle( const Value: string );
begin
  FNewsTitle := Value;
end;

procedure TNews.SetPublicationDate( const Value: TDateTime );
begin
  FPublicationDate := Value;
end;

procedure TNews.SetText( const Value: string );
begin
  FText := Value;
end;

function TNews.UpdateNews( aConnection: TFDConnection ): string;
var
  wQry: TFDQuery;
begin
  wQry := GetNews( aConnection, FNewsId );

  if not ( wQry.Eof ) then
  begin

    wQry.Edit;

    wQry.FieldByName( 'NEWS_TITLE' ).AsString := FNewsTitle;
    wQry.FieldByName( 'DISPLAY_ORDER' ).AsInteger := FDisplayOrder;
    wQry.FieldByName( 'HOLD' ).AsString := FHold;
    wQry.FieldByName( 'PUBLICATION_DATE' ).AsDateTime := FPublicationDate;
    wQry.FieldByName( 'EXPIRY_DATE' ).AsDateTime := FExpiryDate;

    try
      wQry.Post;
      aConnection.Commit;

      Result := 'OK';
    except
      on e: Exception do
      begin
        Result := 'ERR:' + FNewsId.ToString;
        aConnection.Rollback;
      end;
    end;
  end;

  wQry.Close;
end;

{ TNewsCategory }

function TNewsCategory.AddCategory( aConnection: TFDConnection; aNewsId,
  aCategoryId: Integer ): string;
var
  LQry: TFDQuery;
begin
  LQry := GetCategories( aConnection, -1 );
  try
    LQry.Append;
    LQry.FieldByName( 'NEWS_ID' ).AsInteger := aNewsId;
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

constructor TNewsCategory.Create;
begin
  FQryCategories := TFDQuery.Create( nil );
  FQryCategories.Name := 'QryNewsCategories';
  FQryCategories.SQL.Clear;
  FQryCategories.SQL.Add( '''
    select * from NEWS_CONTEXT_CATEGORY
    where NEWS_ID = :NEWS_ID
  ''');

  FQryCategories.UpdateOptions.UpdateTableName := 'NEWS_CONTEXT_CATEGORY';
  FQryCategories.UpdateOptions.KeyFields := 'CATEGORY_ID;NEWS_ID';
  FQryCategories.UpdateOptions.RequestLive := True;
end;

function TNewsCategory.DeleteCategories( aConnection: TFDConnection;
  aNewsId: Integer ): string;
var
  LQry: TFDQuery;
begin
  LQry := GetCategories( aConnection, aNewsId );
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

destructor TNewsCategory.Destroy;
begin
  FreeAndNil( FQryCategories );

  inherited;
end;

function TNewsCategory.GetCategories( aConnection: TFDConnection;
  aNewsId: Integer ): TFDQuery;
begin
  FQryCategories.Connection := aConnection;
  FQryCategories.Close;
  FQryCategories.ParamByName( 'NEWS_ID' ).AsInteger := aNewsId;
  FQryCategories.Open;

  Result := FQryCategories;
end;

procedure TNewsCategory.SetCategoryId( const Value: Integer );
begin
  FCategoryId := Value
end;

procedure TNewsCategory.SetNewsId( const Value: Integer );
begin
  FNewsId := Value;
end;

{ TNewsSubCategory }

function TNewsSubCategory.AddSubcategory( aConnection: TFDConnection; aNewsId,
  aSubcategoryId: Integer ): string;
var
  LQry: TFDQuery;
begin
  LQry := GetSubcategories( aConnection, -1 );
  try
    LQry.Append;
    LQry.FieldByName( 'NEWS_ID' ).AsInteger := aNewsId;
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

constructor TNewsSubCategory.Create;
begin
  FQrySubcategories := TFDQuery.Create( nil );
  FQrySubcategories.Name := 'QryNewsCategories';
  FQrySubcategories.SQL.Clear;
  FQrySubcategories.SQL.Add( '''
    select * from NEWS_CONTEXT_SUBCATEGORY
    where NEWS_ID = :NEWS_ID
  ''');

  FQrySubcategories.UpdateOptions.UpdateTableName := 'NEWS_CONTEXT_SUBCATEGORY';
  FQrySubcategories.UpdateOptions.KeyFields := 'SUBCATEGORY_ID;NEWS_ID';
  FQrySubcategories.UpdateOptions.RequestLive := True;
end;

function TNewsSubCategory.DeleteSubcategories( aConnection: TFDConnection;
  aNewsId: Integer ): string;
var
  LQry: TFDQuery;
begin
  LQry := GetSubcategories( aConnection, aNewsId );
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

destructor TNewsSubCategory.Destroy;
begin
  FreeAndNil( FQrySubcategories );

  inherited;
end;

function TNewsSubCategory.GetSubcategories( aConnection: TFDConnection;
  aNewsId: Integer ): TFDQuery;
begin
  FQrySubcategories.Connection := aConnection;
  FQrySubcategories.Close;
  FQrySubcategories.ParamByName( 'NEWS_ID' ).AsInteger := aNewsId;
  FQrySubcategories.Open;

  Result := FQrySubcategories;
end;

procedure TNewsSubCategory.SetNewsId( const Value: Integer );
begin
  FNewsId := Value;
end;

procedure TNewsSubCategory.SetSubcategoryId( const Value: Integer );
begin
  FSubcategoryId := Value;
end;

{ TNewsCountry }

function TNewsCountry.AddCountry( aConnection: TFDConnection;
  aCountryCode: string; aNewsId: Integer ): string;
var
  LQry: TFDQuery;
begin
  LQry := GetCountries( aConnection, -1 );
  try
    LQry.Append;
    LQry.FieldByName( 'COUNTRY_CODE' ).AsString := aCountryCode;
    LQry.FieldByName( 'NEWS_ID' ).AsInteger := aNewsId;
    LQry.Post;

    Result := 'OK';
  except
    on e: Exception do
    begin
      Result := e.Message;
    end;
  end;

end;

constructor TNewsCountry.Create;
begin
  FQryCountries := TFDQuery.Create( nil );
  FQryCountries.Name := 'QryNewsCountry';
  FQryCountries.SQL.Clear;
  FQryCountries.SQL.Add( '''
    select * from NEWS_CONTEXT_COUNTRY
    where NEWS_ID = :NEWS_ID
  ''');

  FQryCountries.UpdateOptions.UpdateTableName := 'NEWS_CONTEXT_COUNTRY';
  FQryCountries.UpdateOptions.KeyFields := 'COUNTRY_CODE;NEWS_ID';
  FQryCountries.UpdateOptions.RequestLive := True;
end;

function TNewsCountry.DeleteCountries( aConnection: TFDConnection;
  aNewsId: Integer ): string;
var
  LQry: TFDQuery;
begin
  LQry := GetCountries( aConnection, aNewsId );
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
    Result := 'News non trouvé.';
  end;

end;

destructor TNewsCountry.Destroy;
begin
  FreeAndNil( FQryCountries );

  inherited;
end;

function TNewsCountry.GetCountries( aConnection: TFDConnection;
  aNewsId: Integer ): TFDQuery;
begin
  FQryCountries.Connection := aConnection;
  FQryCountries.Close;
  FQryCountries.ParamByName( 'NEWS_ID' ).AsInteger := aNewsId;
  FQryCountries.Open;

  Result := FQryCountries;
end;

procedure TNewsCountry.SetCountryCode( const Value: string );
begin
  FCountryCode := Value;
end;

procedure TNewsCountry.SetNewsId( const Value: Integer );
begin
  FNewsId := Value;
end;

{ TNewsLanguage }

function TNewsLanguage.AddLanguage( aConnection: TFDConnection;
  aLanguageCode: string; aNewsId: Integer ): string;
var
  LQry: TFDQuery;
begin
  LQry := GetLanguages( aConnection, -1 );
  try
    LQry.Append;
    LQry.FieldByName( 'LANGUAGE_CODE' ).AsString := aLanguageCode;
    LQry.FieldByName( 'NEWS_ID' ).AsInteger := aNewsId;
    LQry.Post;

    Result := 'OK';
  except
    on e: Exception do
    begin
      Result := e.Message;
    end;
  end;
end;

constructor TNewsLanguage.Create;
begin
  FQryLanguage := TFDQuery.Create( nil );
  FQryLanguage.Name := 'QryNewsLanguage';
  FQryLanguage.SQL.Clear;
  FQryLanguage.SQL.Add( '''
    select * from NEWS_CONTEXT_LANG
    where NEWS_ID = :NEWS_ID
  ''');

  FQryLanguage.UpdateOptions.UpdateTableName := 'NEWS_CONTEXT_LANG';
  FQryLanguage.UpdateOptions.KeyFields := 'LANGUAGE_CODE;NEWS_ID';
  FQryLanguage.UpdateOptions.RequestLive := True;
end;

function TNewsLanguage.DeleteLanguages( aConnection: TFDConnection;
  aNewsId: Integer ): string;
var
  LQry: TFDQuery;
begin
  LQry := GetLanguages( aConnection, aNewsId );
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
    Result := 'News non trouvé.';
  end;
end;

destructor TNewsLanguage.Destroy;
begin
  FreeAndNil( FQryLanguage );

  inherited;
end;

function TNewsLanguage.GetLanguages( aConnection: TFDConnection;
  aNewsId: Integer ): TFDQuery;
begin
  FQryLanguage.Connection := aConnection;
  FQryLanguage.Close;
  FQryLanguage.ParamByName( 'NEWS_ID' ).AsInteger := aNewsId;
  FQryLanguage.Open;

  Result := FQryLanguage;
end;

procedure TNewsLanguage.SetLanguageCode( const Value: string );
begin
  FLanguageCode := Value;
end;

procedure TNewsLanguage.SetNewsId( const Value: Integer );
begin
  FNewsId := Value;
end;

{ TShowNews }

constructor TShowNews.Create;
begin
  FQryShowNews := TFDQuery.Create( nil );
  FQryShowNews.Name := 'QryShowNews';
  FQryShowNews.SQL.Clear;
  FQryShowNews.SQL.Add( '''
    select
      case n.DISPLAY_ORDER
        when null then 100000
        when 0 then 10000
        else n.DISPLAY_ORDER
      end as REVERSE_ORDER,
      n.DISPLAY_ORDER,
      n.NEWS_ID,
      n.PUBLICATION_DATE,
      n.EXPIRY_DATE,
      n.HOLD,
      n.NEWS_TITLE,
      n.TEXT,
      n.FEED_ID,
      n.CREATION_DATE,
      n.MODIFICATION_DATE,
      f.TITLE
    from NEWS n
    join FEED_NEWS f on (f.FEED_ID = n.FEED_ID)
    where n.FEED_ID = :FEED_ID
      and HOLD = 'O'
      and PUBLICATION_DATE <= localtimestamp
      and EXPIRY_DATE > localtimestamp
    order by REVERSE_ORDER, PUBLICATION_DATE desc, NEWS_TITLE;
  ''');

  with TIntegerField.Create( FQryShowNews ) do
  begin
    FieldName := 'REVERSE_ORDER';
    ProviderFlags := [ ];
    Required := True;
    DataSet := FQryShowNews;
  end;

  with TIntegerField.Create( FQryShowNews ) do
  begin
    FieldName := 'DISPLAY_ORDER';
    ProviderFlags := [ ];
    Required := True;
    DataSet := FQryShowNews;
  end;

  with TIntegerField.Create( FQryShowNews ) do
  begin
    FieldName := 'NEWS_ID';
    ProviderFlags := [ ];
    Required := True;
    DataSet := FQryShowNews;
  end;

  with TDateField.Create( FQryShowNews ) do
  begin
    FieldName := 'PUBLICATION_DATE';
    ProviderFlags := [ ];
    Required := True;
    DataSet := FQryShowNews;
  end;

  with TDateField.Create( FQryShowNews ) do
  begin
    FieldName := 'EXPIRY_DATE';
    ProviderFlags := [ ];
    Required := True;
    DataSet := FQryShowNews;
  end;

  with TWideStringField.Create( FQryShowNews ) do
  begin
    FieldName := 'HOLD';
    ProviderFlags := [ ];
    Required := True;
    DataSet := FQryShowNews;
  end;

  with TWideStringField.Create( FQryShowNews ) do
  begin
    FieldName := 'NEWS_TITLE';
    Size := 500;
    ProviderFlags := [ ];
    Required := True;
    DataSet := FQryShowNews;
  end;

  with TWideMemoField.Create( FQryShowNews ) do
  begin
    FieldName := 'TEXT';
    ProviderFlags := [ ];
    Required := True;
    DataSet := FQryShowNews;
  end;

  with TIntegerField.Create( FQryShowNews ) do
  begin
    FieldName := 'FEED_ID';
    ProviderFlags := [ ];
    Required := True;
    DataSet := FQryShowNews;
  end;

  with TDateField.Create( FQryShowNews ) do
  begin
    FieldName := 'CREATION_DATE';
    ProviderFlags := [ ];
    Required := True;
    DataSet := FQryShowNews;
  end;

  with TSQLTimeStampField.Create( FQryShowNews ) do
  begin
    FieldName := 'MODIFICATION_DATE';
    ProviderFlags := [ ];
    Required := True;
    DataSet := FQryShowNews;
  end;

  with TWideStringField.Create( FQryShowNews ) do
  begin
    FieldName := 'TiTLE';
    ProviderFlags := [ ];
    Required := True;
    DataSet := FQryShowNews;
  end;

  with TWideStringField.Create( FQryShowNews ) do
  begin
    FieldName := 'DISPLAY_DATE';
    FieldKind := fkCalculated;
    ProviderFlags := [ ];
    Required := True;
    DataSet := FQryShowNews;
  end;

  FQryShowNews.UpdateOptions.RequestLive := True;
  FQryShowNews.OnCalcFields := QryShowNewsCalcFields;
end;

destructor TShowNews.Destroy;
begin
  FreeAndNil( FQryShowNews );

  inherited;
end;

function TShowNews.GetNews( aConnection: TFDConnection;
  aFeedId: Integer ): TFDQuery;
begin
  FQryShowNews.Connection := aConnection;
  FQryShowNews.Close;
  FQryShowNews.ParamByName( 'FEED_ID' ).AsInteger := aFeedId;
  FQryShowNews.Open;

  Result := FQryShowNews;
end;

procedure TShowNews.QryShowNewsCalcFields( DataSet: TDataSet );
begin
  Dataset.FieldByName( 'DISPLAY_DATE' ).AsString := FormatDateTime( 'dd mmmm yyyy', DataSet.FieldByName( 'PUBLICATION_DATE'
    ).AsDateTime );
end;

{ TShowNewsUser }

constructor TShowNewsUser.Create;
begin
  FQryShowNewsUser := TFDQuery.Create( nil );
  FQryShowNewsUser.Name := 'QryShowNews';
  FQryShowNewsUser.SQL.Clear;
  FQryShowNewsUser.SQL.Add( '''
    select
      case n.DISPLAY_ORDER
        when null then 100000
        when 0 then 10000
        else n.DISPLAY_ORDER
      end as REVERSE_ORDER,
      n.DISPLAY_ORDER,
      n.NEWS_ID,
      n.PUBLICATION_DATE,
      n.EXPIRY_DATE,
      n.HOLD,
      n.NEWS_TITLE,
      n.TEXT,
      n.FEED_ID,
      n.CREATION_DATE,
      n.MODIFICATION_DATE,
      f.TITLE
    from NEWS n
    join FEED_NEWS f on (f.FEED_ID = n.FEED_ID)
    join NEWS_CONTEXT_COUNTRY p on (p.NEWS_ID = n.NEWS_ID)
    join NEWS_CONTEXT_LANG l on (l.NEWS_ID = n.NEWS_ID)
    join NEWS_CONTEXT_CATEGORY c on (c.NEWS_ID = n.NEWS_ID)
    join NEWS_CONTEXT_SUBCATEGORY s on (s.NEWS_ID = n.NEWS_ID)
    where n.FEED_ID = :FEED_ID
      and p.COUNTRY_CODE = :COUNTRY_CODE
      and l.LANGUAGE_CODE = :LANGUAGE_CODE
      and c.CATEGORY_ID = :CATEGORY_ID
      and s.SUBCATEGORY_ID = :SUBCATEGORY_ID
      and HOLD = 'O'
      and PUBLICATION_DATE <= localtimestamp
      and EXPIRY_DATE > localtimestamp
    order by REVERSE_ORDER, PUBLICATION_DATE desc, NEWS_TITLE;
  ''');

  with TIntegerField.Create( FQryShowNewsUser ) do
  begin
    FieldName := 'REVERSE_ORDER';
    ProviderFlags := [ ];
    Required := True;
    DataSet := FQryShowNewsUser;
  end;

  with TIntegerField.Create( FQryShowNewsUser ) do
  begin
    FieldName := 'DISPLAY_ORDER';
    ProviderFlags := [ ];
    Required := True;
    DataSet := FQryShowNewsUser;
  end;

  with TIntegerField.Create( FQryShowNewsUser ) do
  begin
    FieldName := 'NEWS_ID';
    ProviderFlags := [ ];
    Required := True;
    DataSet := FQryShowNewsUser;
  end;

  with TDateField.Create( FQryShowNewsUser ) do
  begin
    FieldName := 'PUBLICATION_DATE';
    ProviderFlags := [ ];
    Required := True;
    DataSet := FQryShowNewsUser;
  end;

  with TDateField.Create( FQryShowNewsUser ) do
  begin
    FieldName := 'EXPIRY_DATE';
    ProviderFlags := [ ];
    Required := True;
    DataSet := FQryShowNewsUser;
  end;

  with TWideStringField.Create( FQryShowNewsUser ) do
  begin
    FieldName := 'HOLD';
    ProviderFlags := [ ];
    Required := True;
    DataSet := FQryShowNewsUser;
  end;

  with TWideStringField.Create( FQryShowNewsUser ) do
  begin
    FieldName := 'NEWS_TITLE';
    Size := 500;
    ProviderFlags := [ ];
    Required := True;
    DataSet := FQryShowNewsUser;
  end;

  with TWideMemoField.Create( FQryShowNewsUser ) do
  begin
    FieldName := 'TEXT';
    ProviderFlags := [ ];
    Required := True;
    DataSet := FQryShowNewsUser;
  end;

  with TIntegerField.Create( FQryShowNewsUser ) do
  begin
    FieldName := 'FEED_ID';
    ProviderFlags := [ ];
    Required := True;
    DataSet := FQryShowNewsUser;
  end;

  with TDateField.Create( FQryShowNewsUser ) do
  begin
    FieldName := 'CREATION_DATE';
    ProviderFlags := [ ];
    Required := True;
    DataSet := FQryShowNewsUser;
  end;

  with TSQLTimeStampField.Create( FQryShowNewsUser ) do
  begin
    FieldName := 'MODIFICATION_DATE';
    ProviderFlags := [ ];
    Required := True;
    DataSet := FQryShowNewsUser;
  end;

  with TWideStringField.Create( FQryShowNewsUser ) do
  begin
    FieldName := 'TITLE';
    ProviderFlags := [ ];
    Required := True;
    DataSet := FQryShowNewsUser;
  end;

  with TWideStringField.Create( FQryShowNewsUser ) do
  begin
    FieldName := 'DISPLAY_DATE';
    FieldKind := fkCalculated;
    ProviderFlags := [ ];
    Required := True;
    DataSet := FQryShowNewsUser;
  end;

  FQryShowNewsUser.UpdateOptions.RequestLive := True;
  FQryShowNewsUser.OnCalcFields := QryShowNewsUserCalcFields;
end;

destructor TShowNewsUser.Destroy;
begin
  FreeAndNil( FQryShowNewsUser );

  inherited;
end;

function TShowNewsUser.GetNews( aConnection: TFDConnection; aFeedId, aCategoryId,
  aSubcategoryId: Integer; aCountryCode, aLanguageCode: string ): TFDQuery;
begin
  FQryShowNewsUser.Connection := aConnection;
  FQryShowNewsUser.Close;
  FQryShowNewsUser.ParamByName( 'FEED_ID' ).AsInteger := aFeedId;
  FQryShowNewsUser.ParamByName( 'COUNTRY_CODE' ).AsString := aCountryCode;
  FQryShowNewsUser.ParamByName( 'LANGUAGE_CODE' ).AsString := aLanguageCode;
  FQryShowNewsUser.ParamByName( 'CATEGORY_ID' ).AsInteger := aCategoryId;
  FQryShowNewsUser.ParamByName( 'SUBCATEGORY_ID' ).AsInteger := aSubcategoryId;
  FQryShowNewsUser.Open;

  Result := FQryShowNewsUser;
end;

procedure TShowNewsUser.QryShowNewsUserCalcFields( DataSet: TDataSet );
begin
  Dataset.FieldByName( 'DISPLAY_DATE' ).AsString := FormatDateTime( 'dd mmmm yyyy', DataSet.FieldByName( 'PUBLICATION_DATE'
    ).AsDateTime );
end;

{ TShowGroup }

constructor TShowGroup.Create;
begin
  FQryShowGroup := TFDQuery.Create( nil );
  FQryShowGroup.Name := 'QryShowGroup';
  FQryShowGroup.SQL.Clear;
  FQryShowGroup.SQL.Add( '''
    select first 1 TEXT from NEWS
    where FEED_ID = :FEED_ID
  ''');

  FQryShowGroup.UpdateOptions.RequestLive := True;
end;

destructor TShowGroup.Destroy;
begin
  FreeAndNil( FQryShowGroup );

  inherited;
end;

function TShowGroup.GetGroup( aConnection: TFDConnection;
  aFeedId: Integer ): TFDQuery;
begin
  FQryShowGroup.Connection := aConnection;
  FQryShowGroup.Close;
  FQryShowGroup.ParamByName( 'FEED_ID' ).AsInteger := aFeedId;
  FQryShowGroup.Open;

  Result := FQryShowGroup;
end;

end.

