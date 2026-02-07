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
  File last update : 2026-01-25T09:48:18.000+01:00
  Signature : b4f6a982441c6b9b6fde13face75e284f6fb9073
  ***************************************************************************
*)

/// <summary>
///   Modèle Feed
/// </summary>
unit UFeeds;

interface

uses
  System.Classes,
  System.SysUtils,
  FireDAC.Comp.Client,
  FireDAC.Stan.Param,
  Data.DB;

type
  /// <summary>
  ///   Class représentant la liste des Feeds
  /// </summary>
  TFeeds = class
  strict private
    /// <summary>
    ///   Champ FDQuery
    /// </summary>
    FQryListeFeeds: TFDQuery;
    /// <summary>
    ///   Champ FDQuery
    /// </summary>
    FQryCountFeeds: TFDQuery;

    /// <summary>
    ///   Champ contenant la requete de sélection
    /// </summary>
    FSelectListeFeeds: string;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    ///   Retourne un FDQuery contenant la loiste des feeds
    /// </summary>
    function GetListeFeeds(aConnection: TFDConnection; aFirst, aSkip, aGroupId:
        Integer; aTitle, aOrderField, aOrder: string): TFDQuery;
    /// <summary>
    ///   retourne le nombre de feeds resultatnt
    /// </summary>
    function GetFeedsCount(aConnection: TFDConnection; aGroupId:Integer; aTitle:
        string): Integer;
  end;

  /// <summary>
  ///   Class représentant un Feed
  /// </summary>
  TFeed = class
  strict private
    FAllContexts: string;
    /// <summary>
    ///   Champ nom template
    /// </summary>
    FDisplayTemplate: string;
    /// <summary>
    ///   Champ Groupe d'appartenance du Feed
    /// </summary>
    FFeedGroup: Integer;
    /// <summary>
    ///   Champ Id Unique
    /// </summary>
    FFeedId: Integer;
    /// <summary>
    ///   Champ Nom
    /// </summary>
    FFeedName: string;
    /// <summary>
    ///   Champ FDQuery
    /// </summary>
    FQryFeed: TFDQuery;
    /// <summary>
    ///   Champs status du Feed
    /// </summary>
    FStatus: string;
    /// <summary>
    ///   Champ titre
    /// </summary>
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

    /// <summary>
    ///   Retourne un FDQuery avec les information du Feed
    /// </summary>
    function GetFeed( aConnection: TFDConnection; aFeedId: Integer ): TFDQuery;
    function UpdateFeed( aConnection: TFDConnection; const aFeedId: integer; const
      aName, aTitle, aStatus: string; const aGroup: Integer ): string;
    /// <summary>
    ///   Création d'un nouveau Feed
    /// </summary>
    function CreateNewFeed( aConnection: TFDConnection; out aMsg: string ): Integer;
    /// <summary>
    ///   Suppression d'un Feed
    /// </summary>
    function DeleteFeed( aConnection: TFDConnection; aFeedId: Integer ): string;
    function ChangeContext( aConnection: TFDConnection; aFeedId: Integer; aContext: string ): string;
    /// <summary>
    ///   Retourne le nom du template
    /// </summary>
    function GetTemplateName( aConnection: TFDConnection; aFeedId: Integer ): string;
    function SetTemplateName( aConnection: TFDConnection; aFeedId: Integer; aTemplateName: string ): string;

    property AllContexts: string read FAllContexts write SetAllContexts;
    /// <summary>
    ///   Template d'affichage
    /// </summary>
    property DisplayTemplate: string read FDisplayTemplate write SetDisplayTemplate;
    /// <summary>
    ///   Groupe d'appartenance du Feed
    /// </summary>
    property FeedGroup: Integer read FFeedGroup write SetFeedGroup;
    /// <summary>
    ///   Id unique
    /// </summary>
    property FeedId: Integer read FFeedId write SetFeedId;
    /// <summary>
    ///   Nom
    /// </summary>
    property FeedName: string read FFeedName write SetFeedName;
    /// <summary>
    ///   Statut du Feed (O:Actif; N: Inactif)
    /// </summary>
    property Status: string read FStatus write SetStatus;
    /// <summary>
    ///   Titre
    /// </summary>
    property Title: string read FTitle write SetTitle;
  end;

  /// <summary>
  ///   Class représentant les catégories d'appartenance du Feed
  /// </summary>
  TFeedCategory = class
  strict private
    /// <summary>
    ///   Champ Id catégorie
    /// </summary>
    FCategoryId: Integer;
    /// <summary>
    ///   Champ Id Feed
    /// </summary>
    FFeedId: Integer;
    /// <summary>
    ///   Champ FDQuery
    /// </summary>
    FQryCategories: TFDQuery;

    procedure SetCategoryId( const Value: Integer );
    procedure SetFeedId( const Value: Integer );
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    ///   Liste des catégories du Feed
    /// </summary>
    function GetCategories( aConnection: TFDConnection; aFeedId: Integer ): TFDQuery;
    /// <summary>
    ///   Supprimer les catégories du Feed
    /// </summary>
    function DeleteCategories( aConnection: TFDConnection; aFeedId: Integer ): string;
    /// <summary>
    ///   Ajouter une catégoie pour le Feed
    /// </summary>
    function AddCategory( aConnection: TFDConnection; aFeedId, aCategoryId: Integer ): string;

    /// <summary>
    ///   Id catégorie
    /// </summary>
    property CategoryId: Integer read FCategoryId write SetCategoryId;
    /// <summary>
    ///   Id du Feed
    /// </summary>
    property FeedId: Integer read FFeedId write SetFeedId;
  end;

  /// <summary>
  ///   Class représentant les Sous-catégories d'appartenance du Feed <br />
  /// </summary>
  TFeedSubCategory = class
  strict private
    /// <summary>
    ///   Champ FDQuery
    /// </summary>
    FQrySubcategories: TFDQuery;
    /// <summary>
    ///   Champ id feed
    /// </summary>
    FFeedId: Integer;
    /// <summary>
    ///   Champ id sous-catégorie
    /// </summary>
    FSubcategoryId: Integer;

    procedure SetFeedId( const Value: Integer );
    procedure SetSubcategoryId( const Value: Integer );
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    ///   Retourne la lister des sous-catégories
    /// </summary>
    function GetSubcategories( aConnection: TFDConnection; aFeedId: Integer ): TFDQuery;
    /// <summary>
    ///   Supprimer les sous-catégories
    /// </summary>
    function DeleteSubcategories( aConnection: TFDConnection; aFeedId: Integer ): string;
    /// <summary>
    ///   Ajouter une sous-catégorie au feed
    /// </summary>
    function AddSubcategory( aConnection: TFDConnection; aFeedId, aSubcategoryId:
      Integer ): string;

    /// <summary>
    ///   Id du feed
    /// </summary>
    property FeedId: Integer read FFeedId write SetFeedId;
    /// <summary>
    ///   Id de la sous-catégorie
    /// </summary>
    property SubcategoryId: Integer read FSubcategoryId write SetSubcategoryId;
  end;

  /// <summary>
  ///   Class représentant les pays d'appartenance du Feed <br />
  /// </summary>
  TFeedCountry = class
  strict private
    /// <summary>
    ///   Champ FDQuery
    /// </summary>
    FQryCountries: TFDQuery;
    /// <summary>
    ///   Champ code pays
    /// </summary>
    FCountryCode: string;
    /// <summary>
    ///   Champ Id feed
    /// </summary>
    FFeedId: Integer;

    procedure SetCountryCode( const Value: string );
    procedure SetFeedId( const Value: Integer );
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    ///   Liste des pays du feed
    /// </summary>
    function GetCountries( aConnection: TFDConnection; aFeedId: Integer ): TFDQuery;
    /// <summary>
    ///   Supprimer les pays du feed
    /// </summary>
    function DeleteCountries( aConnection: TFDConnection; aFeedId: Integer ): string;
    /// <summary>
    ///   Ajouter un pays pour le feed
    /// </summary>
    function AddCountry( aConnection: TFDConnection; aCountryCode: string; aFeedId:
      Integer ): string;

    /// <summary>
    ///   Code Pays
    /// </summary>
    property CountryCode: string read FCountryCode write SetCountryCode;
    /// <summary>
    ///   Id Feed
    /// </summary>
    property FeedId: Integer read FFeedId write SetFeedId;
  end;

  /// <summary>
  ///   Class représentant les langues d'appartenance du Feed <br />
  /// </summary>
  TFeedLanguage = class
  strict private
    /// <summary>
    ///   Champ FDQuery
    /// </summary>
    FQryLanguage: TFDQuery;
    /// <summary>
    ///   Champ id feed
    /// </summary>
    FFeedId: Integer;
    /// <summary>
    ///   Champ code langue
    /// </summary>
    FLanguageCode: string;

    procedure SetFeedId( const Value: Integer );
    procedure SetLanguageCode( const Value: string );
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    ///   Liste des langues du feed
    /// </summary>
    function GetLanguages( aConnection: TFDConnection; aFeedId: Integer ): TFDQuery;
    /// <summary>
    ///   Supprimer les langues du feed
    /// </summary>
    function DeleteLanguages( aConnection: TFDConnection; aFeedId: Integer ): string;
    /// <summary>
    ///   Ajouter une langue pour le feed
    /// </summary>
    function AddLanguage( aConnection: TFDConnection; aLanguageCode: string; aFeedId:
      Integer ): string;

    /// <summary>
    ///   Id du feed
    /// </summary>
    property FeedId: Integer read FFeedId write SetFeedId;
    /// <summary>
    ///   Code langue
    /// </summary>
    property LanguageCode: string read FLanguageCode write SetLanguageCode;
  end;

  /// <summary>
  ///   Class représentant la liste des Feeds accessibels d'un utilisateur
  /// </summary>
  /// <remarks>
  ///   La sé"lection se fait à partir de la catégorie / sous-cvatégorie /
  ///   pays / langue associé aux news
  /// </remarks>
  TFeedsUser = class
  strict private
    /// <summary>
    ///   Champ FDQuery
    /// </summary>
    FQryFeedsUser: TFDQuery;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    ///   Retourne la liste des feeds autorisés à l'utilisateur
    /// </summary>
    /// <param name="aConnection">
    ///   Connexion base de données
    /// </param>
    /// <param name="aFeedGroup">
    ///   Groupe d'appartenance des feeds recherchés
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
    SELECT first :FIRST skip :SKIP f.*, g.GROUP_NAME FROM FEED_NEWS f
    join GROUPS g on (g.GROUP_ID = f.FEED_GROUP)
    where f.FEED_GROUP = :FEED_GROUP
     and upper(f.TITLE) like :TITLE
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
   where FEED_GROUP = :FEED_GROUP
     and upper(TITLE) like :TITLE
  ''');
end;

destructor TFeeds.Destroy;
begin
  FreeAndNil( FQryListeFeeds );
  FreeAndNil( FQryCountFeeds );

  inherited;
end;

function TFeeds.GetFeedsCount( aConnection: TFDConnection; aGroupId:Integer;
  aTitle: string ): Integer;
begin
  FQryCountFeeds.Connection := aConnection;
  FQryCountFeeds.ParamByName( 'FEED_GROUP' ).AsInteger:=aGroupId;
  FQryCountFeeds.ParamByName( 'TITLE' ).AsString := aTitle;
  FQryCountFeeds.Open;

  Result := FQryCountFeeds.FieldByName( 'NB_ENR' ).AsInteger;

  FQryCountFeeds.Close;
end;

function TFeeds.GetListeFeeds( aConnection: TFDConnection; aFirst, aSkip, aGroupId:
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
  FQryListeFeeds.ParamByName( 'FEED_GROUP' ).AsInteger:=aGroupId;
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
    SELECT f.*, g.GROUP_NAME FROM FEED_NEWS f
    join GROUPS g on (g.GROUP_ID = f.FEED_GROUP)
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

