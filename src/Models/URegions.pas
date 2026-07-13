unit URegions;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
  FireDAC.Comp.Client,
  Firedac.Stan.Param,
  Data.DB;

type
  TRegion = class
  private
    FCode: string;
  public
    constructor Create( const ACode: string );
    property Code: string read FCode;
  end;

  TAssociationRow = class
  private
    FCountry: string;
    FLibelle: string;
    procedure SetLibelle( const Value: string );
  public
    constructor Create( const ACountry, ALibelle: string );
    property Country: string read FCountry;
    property Libelle: string read FLibelle write SetLibelle;
  end;

  /// <summary>
  ///   Class langues
  /// </summary>
  TRegions = class
  strict private
    /// <summary>
    ///   Instance du FDQuery
    /// </summary>
    FQryRegions: TFDQuery;
    FQryRegion: TFDQuery;
    FQryAffectedCountries: TFDQuery;
    FQryNotAffectedCountries: TFDQuery;
    FQryDesaffecterCountry: TFDQuery;
    FQryAffecterCountry: TFDQuery;
    FQryDeleteRegion: TFDQuery;
    FQryCountry: TFDQuery;
    FQryDeleteCountry: TFDQuery;

    function GetQryListeRegions( AConnection: TFDConnection ): TFDQuery;
    function GetQryRegion( aConnection: TFDConnection ): TFDQuery;
    function GetQryAffectedCountries( aConnection: TFDConnection ): TFDQuery;
    function GetQryNotAffectedCountries( aConnection: TFDConnection ): TFDQuery;
    function GetQryDesaffecterCountry( aConnection: TFDConnection ): TFDQuery;
    function GetQryAffecterCountry( aConnection: TFDConnection ): TFDQuery;
    function GetQryDeleteRegion( aConnection: TFDConnection ): TFDQuery;
    function GetQryCountry( aConnection: TFDConnection ): TFDQuery;
    function GetQryDeleteCountry( aConnection: TFDConnection ): TFDQuery;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    ///   Retourne un FDQuery avec la liste des langues existantes
    /// </summary>
    function GetRegion( aConnection: TFDConnection; ACode: string ): TRegion;
    function GetCountry( aConnection: TFDConnection; ACode: string ): TAssociationRow;
    function GetRegions( aConnection: TFDConnection ): TObjectList<TRegion>;
    function GetCountriesAffected( aConnection: TFDConnection; aCode: string ): TObjectList<TAssociationRow>;
    function GetCountriesNotAffected( aConnection: TFDConnection ): TObjectList<TAssociationRow>;
    function DesaffecterCountry( aConnection: TFDConnection; aCode: string ): string;
    function AffecterCountry( aConnection: TFDConnection; aCountry, aRegion: string ): string;

    procedure DeleteRegion( aConnection: TFDConnection; ACode: string );
    procedure DeleteCountry( aConnection: TFDConnection; ACode: string );
    procedure AddRegion( aConnection: TFDConnection; ACode: string );
    procedure AddCountry( aConnection: TFDConnection; ACode, ANom: string );
  end;

implementation

const
  QRY_LISTE_REGIONS: string = '''
    select * from ZONE_GEOGRAPHIQUE
    where CODE_ZONE <> 'Toutes_Zones'
    order by CODE_ZONE
  ''';

  QRY_REGION: string = '''
    select * from ZONE_GEOGRAPHIQUE
    where CODE_ZONE = :CODE_ZONE
  ''';

  QRY_AFFECTED_COUNTRIES: string = '''
    select * from COUNTRY
    where ZONE_PORTAIL = :ZONE_PORTAIL
  ''';

  QRY_NOT_AFFECTED_COUNTRIES: string = '''
    select * from COUNTRY
    where ZONE_PORTAIL is null
  ''';

  QRY_DESAFFECTER_COUNTRY: string = '''
    update COUNTRY set
    ZONE_PORTAIL = null
    where COUNTRY_CODE = :COUNTRY_CODE
  ''';

  QRY_AFFECTER_COUNTRY: string = '''
    update COUNTRY set
    ZONE_PORTAIL = :ZONE_PORTAIL
    where  COUNTRY_CODE = :COUNTRY_CODE
  ''';

  QRY_DELETE_REGION: string = '''
    delete from ZONE_GEOGRAPHIQUE
    where CODE_ZONE = :CODE_ZONE
  ''';

  QRY_COUNTRY: string = '''
    select * from COUNTRY
    where COUNTRY_CODE = :COUNTRY_CODE
  ''';

  QRY_DELETE_COUNTRY: string = '''
    delete from COUNTRY
    where COUNTRY_CODE = :COUNTRY_CODE
  ''';

  { TRegions }

procedure TRegions.AddRegion( aConnection: TFDConnection; ACode: string );
begin
  var LQryRegions := GetQryListeRegions( aConnection );
  LQryRegions.Open;
  LQryRegions.Append;
  LQryRegions.FieldByName( 'CODE_ZONE' ).AsString := ACode;
  LQryRegions.Post;
  LQryRegions.Close;
end;

procedure TRegions.AddCountry( aConnection: TFDConnection; ACode,
  ANom: string );
begin
  var LQryCountry := GetQryCountry( aConnection );
  LQryCountry.Open;
  LQryCountry.Append;
  LQryCountry.FieldByName( 'COUNTRY_CODE' ).AsString := ACode;
  LQryCountry.FieldByName( 'COUNTRY_NAME' ).AsString := ANom;
  LQryCountry.Post;
  LQryCountry.Close;
end;

function TRegions.AffecterCountry( aConnection: TFDConnection; aCountry,
  aRegion: string ): string;
begin
  var LQryAffecterCountry := GetQryAffecterCountry( aConnection );
  LQryAffecterCountry.ParamByName( 'COUNTRY_CODE' ).AsString := aCountry;
  LQryAffecterCountry.ParamByName( 'ZONE_PORTAIL' ).AsString := aRegion;
  LQryAffecterCountry.ExecSQL;
end;

constructor TRegions.Create;
begin
  FQryRegions := nil;
  //    FQryRegions := TFDQuery.Create( nil );
  //
  //    FQryRegions.Name := 'QryListeRegions';
  //    FQryRegions.SQL.Clear;
  //    FQryRegions.SQL.Add( '''
  //      select * from ZONE_GEOGRAPHIQUE
  //      where CODE_ZONE <> 'Toutes_Zones'
  //      order by CODE_ZONE
  //    ''');

  FQryRegion := nil;
  //  FQryRegion := TFDQuery.Create( nil );
  //
  //  FQryRegion.Name := 'QryRegion';
  //  FQryRegion.SQL.Clear;
  //  FQryRegion.SQL.Add( '''
  //    select * from ZONE_GEOGRAPHIQUE
  //    where CODE_ZONE = :CODE_ZONE
  //  ''');

  FQryAffectedCountries := nil;
  //  FQryAffectedCountries := TFDQuery.Create( nil );
  //
  //  FQryAffectedCountries.Name := 'QryAffectedCountries';
  //  FQryAffectedCountries.SQL.Clear;
  //  FQryAffectedCountries.SQL.Add( '''
  //    select * from COUNTRY
  //    where ZONE_PORTAIL = :ZONE_PORTAIL
  //  ''');

  FQryNotAffectedCountries := nil;
  //  FQryNotAffectedCountries := TFDQuery.Create( nil );
  //
  //  FQryNotAffectedCountries.Name := 'QryNotAffectedCountries';
  //  FQryNotAffectedCountries.SQL.Clear;
  //  FQryNotAffectedCountries.SQL.Add( '''
  //    select * from COUNTRY
  //    where ZONE_PORTAIL is null
  //  ''');

  FQryDesaffecterCountry := nil;
  //  FQryDesaffecterCountry := TFDQuery.Create( nil );
  //
  //  FQryDesaffecterCountry.Name := 'QryDesaffecterCountry';
  //  FQryDesaffecterCountry.SQL.Clear;
  //  FQryDesaffecterCountry.SQL.Add( '''
  //    update COUNTRY set
  //    ZONE_PORTAIL = null
  //    where COUNTRY_CODE = :COUNTRY_CODE
  //  ''');

  FQryAffecterCountry := nil;
  //  FQryAffecterCountry := TFDQuery.Create( nil );
  //
  //  FQryAffecterCountry.Name := 'QryAffecterCountry';
  //  FQryAffecterCountry.SQL.Clear;
  //  FQryAffecterCountry.SQL.Add( '''
  //    update COUNTRY set
  //    ZONE_PORTAIL = :ZONE_PORTAIL
  //    where  COUNTRY_CODE = :COUNTRY_CODE
  //  ''');

  FQryDeleteRegion := nil;
  //  FQryDeleteRegion := TFDQuery.Create( nil );
  //
  //  FQryDeleteRegion.Name := 'QryDeleteRegion';
  //  FQryDeleteRegion.SQL.Clear;
  //  FQryDeleteRegion.SQL.Add( '''
  //    delete from ZONE_GEOGRAPHIQUE
  //    where CODE_ZONE = :CODE_ZONE
  //  ''');

  FQryCountry := nil;
  //  FQryCountry := TFDQuery.Create( nil );
  //
  //  FQryCountry.Name := 'QryCountry';
  //  FQryCountry.SQL.Clear;
  //  FQryCountry.SQL.Add( '''
  //    select * from COUNTRY
  //    where COUNTRY_CODE = :COUNTRY_CODE
  //  ''');

  FQryDeleteCountry := nil;
  //  FQryDeleteCountry := TFDQuery.Create( nil );
  //
  //  FQryDeleteCountry.Name := 'QryDeleteUserCountry';
  //  FQryDeleteCountry.SQL.Clear;
  //  FQryDeleteCountry.SQL.Add( '''
  //    delete from COUNTRY
  //    where COUNTRY_CODE = :COUNTRY_CODE
  //  ''');
end;

procedure TRegions.DeleteRegion( aConnection: TFDConnection; ACode: string );
begin
  var LQryDeleteRegion := GetQryDeleteRegion( aConnection );
  LQryDeleteRegion.ParamByName( 'CODE_ZONE' ).AsString := ACode;
  LQryDeleteRegion.ExecSQL;
end;

procedure TRegions.DeleteCountry( aConnection: TFDConnection; ACode: string );
begin
  var LQryDeleteCountry := GetQryDeleteCountry( aConnection );
  LQryDeleteCountry.ParamByName( 'COUNTRY_CODE' ).AsString := ACode;
  LQryDeleteCountry.ExecSQL;
end;

function TRegions.DesaffecterCountry( aConnection: TFDConnection;
  aCode: string ): string;
begin
  var LQryDesaffecterCountry := GetQryDesaffecterCountry( aConnection );
  LQryDesaffecterCountry.ParamByName( 'COUNTRY_CODE' ).AsString := aCode;
  LQryDesaffecterCountry.ExecSQL;
end;

destructor TRegions.Destroy;
begin
  FreeAndNil( FQryRegions );
  FreeAndNil( FQryRegion );
  FreeAndNil( FQryAffectedCountries );
  FreeAndNil( FQryNotAffectedCountries );
  FreeAndNil( FQryDesaffecterCountry );
  FreeAndNil( FQryAffecterCountry );
  FreeAndNil( FQryDeleteRegion );
  FreeAndNil( FQryCountry );
  FreeAndNil( FQryDeleteCountry );

  inherited;
end;

function TRegions.GetRegion( aConnection: TFDConnection; ACode: string ): TRegion;
begin
  var LQryRegion := GetQryRegion( aConnection );
  LQryRegion.ParamByName( 'CODE_ZONE' ).AsString := ACode;
  LQryRegion.Open;

  if not ( LQryRegion.Eof ) then
  begin
    Result := TRegion.Create(
      LQryRegion.FieldByName( 'CODE_ZONE' ).AsString
      );
  end
  else
  begin
    Result := nil;
  end;
end;

function TRegions.GetRegions( aConnection: TFDConnection ): TObjectList<TRegion>;
begin
  Result := TObjectList<TRegion>.Create;

  var LQry := GetQryListeRegions( aConnection ); // GetListOfRegions( aConnection );

  LQry.Open;

  while not ( LQry.Eof ) do
  begin
    Result.Add(
      TRegion.Create(
        LQry.FieldByName( 'CODE_ZONE' ).AsString
        )
      );

    LQry.Next;
  end;
end;

function TRegions.GetQryAffectedCountries(
  aConnection: TFDConnection ): TFDQuery;
begin
  if not ( Assigned( FQryAffectedCountries ) ) then
  begin
    FQryAffectedCountries := TFDQuery.Create( nil );

    FQryAffectedCountries.Name := 'QryAffectedCountries';
    FQryAffectedCountries.SQL.Clear;
    FQryAffectedCountries.SQL.Add( QRY_AFFECTED_COUNTRIES );
  end;
  FQryAffectedCountries.connection := AConnection;
  FQryAffectedCountries.Close;

  Result := FQryAffectedCountries;
end;

function TRegions.GetQryAffecterCountry( aConnection: TFDConnection ): TFDQuery;
begin
  if not ( Assigned( FQryAffecterCountry ) ) then
  begin
    FQryAffecterCountry := TFDQuery.Create( nil );

    FQryAffecterCountry.Name := 'QryAffecterCountry';
    FQryAffecterCountry.SQL.Clear;
    FQryAffecterCountry.SQL.Add( QRY_AFFECTER_COUNTRY );
  end;
  FQryAffecterCountry.Connection := AConnection;
  FQryAffecterCountry.Close;

  Result := FQryAffecterCountry;
end;

function TRegions.GetQryCountry( aConnection: TFDConnection ): TFDQuery;
begin
  if not ( Assigned( FQryCountry ) ) then
  begin
    FQryCountry := TFDQuery.Create( nil );

    FQryCountry.Name := 'QryCountry';
    FQryCountry.SQL.Clear;
    FQryCountry.SQL.Add( QRY_COUNTRY );
  end;
  FQryCountry.Connection := AConnection;
  FQryCountry.Close;

  Result := FQryCountry;
end;

function TRegions.GetQryDeleteCountry( aConnection: TFDConnection ): TFDQuery;
begin
  if not ( Assigned( FQryDeleteCountry ) ) then
  begin
    FQryDeleteCountry := TFDQuery.Create( nil );

    FQryDeleteCountry.Name := 'QryDeleteCountry';
    FQryDeleteCountry.SQL.Clear;
    FQryDeleteCountry.SQL.Add( QRY_DELETE_COUNTRY );
  end;
  FQryDeleteCountry.Connection := AConnection;
  FQryDeleteCountry.Close;

  Result := FQryDeleteCountry;
end;

function TRegions.GetQryDeleteRegion( aConnection: TFDConnection ): TFDQuery;
begin
  if not ( Assigned( FQryDeleteRegion ) ) then
  begin
    FQryDeleteRegion := TFDQuery.Create( nil );

    FQryDeleteRegion.Name := 'QryDeleteRegion';
    FQryDeleteRegion.SQL.Clear;
    FQryDeleteRegion.SQL.Add( QRY_DELETE_REGION );
  end;
  FQryDeleteRegion.Connection := AConnection;
  FQryDeleteRegion.Close;

  Result := FQryDeleteRegion;
end;

function TRegions.GetQryDesaffecterCountry(
  aConnection: TFDConnection ): TFDQuery;
begin
  if not ( Assigned( FQryDesaffecterCountry ) ) then
  begin
    FQryDesaffecterCountry := TFDQuery.Create( nil );

    FQryDesaffecterCountry.Name := 'QryDesaffecterCountry';
    FQryDesaffecterCountry.SQL.Clear;
    FQryDesaffecterCountry.SQL.Add( QRY_DESAFFECTER_COUNTRY );
  end;
  FQryDesaffecterCountry.connection := AConnection;
  FQryDesaffecterCountry.Close;

  Result := FQryDesaffecterCountry;
end;

//function TRegions.GetListOfRegions( aConnection: TFDConnection ): TFDQuery;
//begin
//  FQryRegions.Close;
//  FQryRegions.Connection := aConnection;
//  FQryRegions.Open;
//
//  Result := FQryRegions;
//end;

function TRegions.GetQryListeRegions( AConnection: TFDConnection ): TFDQuery;
begin
  if not ( Assigned( FQryRegions ) ) then
  begin
    FQryRegions := TFDQuery.Create( nil );

    FQryRegions.Name := 'QryListeRegions';
    FQryRegions.SQL.Clear;
    FQryRegions.SQL.Add( QRY_LISTE_REGIONS );
  end;
  FQryRegions.connection := AConnection;
  FQryRegions.Close;

  Result := FQryRegions;
end;

function TRegions.GetQryNotAffectedCountries(
  aConnection: TFDConnection ): TFDQuery;
begin
  if not ( Assigned( FQryNotAffectedCountries ) ) then
  begin
    FQryNotAffectedCountries := TFDQuery.Create( nil );

    FQryNotAffectedCountries.Name := 'QryNotAffectedCountries';
    FQryNotAffectedCountries.SQL.Clear;
    FQryNotAffectedCountries.SQL.Add( QRY_NOT_AFFECTED_COUNTRIES );
  end;
  FQryNotAffectedCountries.connection := AConnection;
  FQryNotAffectedCountries.Close;

  Result := FQryNotAffectedCountries;
end;

function TRegions.GetQryRegion( aConnection: TFDConnection ): TFDQuery;
begin
  if not ( Assigned( FQryRegion ) ) then
  begin
    FQryRegion := TFDQuery.Create( nil );

    FQryRegion.Name := 'QryRegion';
    FQryRegion.SQL.Clear;
    FQryRegion.SQL.Add( QRY_REGION );
  end;
  FQryRegion.connection := AConnection;
  FQryRegion.Close;

  Result := FQryRegion;
end;

function TRegions.GetCountry( aConnection: TFDConnection;
  ACode: string ): TAssociationRow;
begin
  var LQryCountry := GetQryCountry(aConnection);
  LQryCountry.ParamByName( 'COUNTRY_CODE' ).AsString := ACode;
  LQryCountry.Open;

  if not ( LQryCountry.Eof ) then
  begin
    Result := TAssociationRow.Create(
      LQryCountry.FieldByName( 'COUNTRY_CODE' ).AsString,
      LQryCountry.FieldByName( 'COUNTRY_NAME' ).AsString
      );
  end
  else
  begin
    Result := nil;
  end;
end;

function TRegions.GetCountriesAffected( aConnection: TFDConnection;
  aCode: string ): TObjectList<TAssociationRow>;
begin
  Result := TObjectList<TAssociationRow>.Create;

  var LQryAffectedCountries := GetQryAffectedCountries( aConnection );
  LQryAffectedCountries.ParamByName( 'ZONE_PORTAIL' ).AsString := ACode;
  LQryAffectedCountries.Open;

  while not ( LQryAffectedCountries.Eof ) do
  begin
    Result.Add(
      TAssociationRow.Create(
        LQryAffectedCountries.FieldByName( 'COUNTRY_CODE' ).AsString,
        LQryAffectedCountries.FieldByName( 'COUNTRY_NAME' ).AsString
        )
      );

    LQryAffectedCountries.Next;
  end;

  LQryAffectedCountries.Close;
end;

function TRegions.GetCountriesNotAffected( aConnection: TFDConnection ): TObjectList<TAssociationRow>;
begin
  Result := TObjectList<TAssociationRow>.Create;

  var LQryNotAffectedCountries := GetQryNotAffectedCountries( aConnection );
  LQryNotAffectedCountries.Open;

  while not ( LQryNotAffectedCountries.Eof ) do
  begin
    Result.Add(
      TAssociationRow.Create(
        LQryNotAffectedCountries.FieldByName( 'COUNTRY_CODE' ).AsString,
        LQryNotAffectedCountries.FieldByName( 'COUNTRY_NAME' ).AsString
        )
      );

    LQryNotAffectedCountries.Next;
  end;
end;

{ TRegion }

constructor TRegion.Create( const ACode: string );
begin
  FCode := ACode;
end;

{ TAssociationRow }

constructor TAssociationRow.Create( const ACountry, ALibelle: string );
begin
  FCountry := ACountry;
  FLibelle := ALibelle;
end;

procedure TAssociationRow.SetLibelle( const Value: string );
begin
  FLibelle := Value;
end;

end.

