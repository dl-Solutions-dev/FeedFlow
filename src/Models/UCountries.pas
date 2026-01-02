/// <summary>
///   Modèle pour les pays
/// </summary>
unit UCountries;

interface

uses
  System.Classes,
  System.SysUtils,
  FireDAC.Comp.Client,
  Data.DB;

type
  TCountries = class
  strict private
    /// <summary>
    ///   Query retournant la liste des pays
    /// </summary>
    FQryCountries: TFDQuery;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    ///   Retourne la liste des pays
    /// </summary>
    function GetListOfCountries( aConnection: TFDConnection ): TFDQuery;
  end;

implementation

{ TCountries }

constructor TCountries.Create;
begin
  FQryCountries := TFDQuery.Create( nil );

  FQryCountries.Name := 'QryListeCountries';
  FQryCountries.SQL.Clear;
  FQryCountries.SQL.Add( '''
    select * from COUNTRY
    order by COUNTRY_NAME
  ''');
end;

destructor TCountries.Destroy;
begin
  FreeAndNil(FQryCountries);

  inherited;
end;

function TCountries.GetListOfCountries(aConnection: TFDConnection): TFDQuery;
begin
  FQryCountries.Close;
  FQryCountries.Connection := aConnection;
  FQryCountries.Open;

  Result := FQryCountries;
end;

end.
