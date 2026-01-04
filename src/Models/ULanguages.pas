/// <summary>
///   Modèle pou rles langues
/// </summary>
unit ULanguages;

interface

uses
  System.Classes,
  System.SysUtils,
  FireDAC.Comp.Client,
  Data.DB;

type
  /// <summary>
  ///   Class langues
  /// </summary>
  TLanguages = class
  strict private
    /// <summary>
    ///   Instance du FDQuery
    /// </summary>
    FQryLanguages: TFDQuery;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    ///   Retourne un FDQuery avec la liste des langues existantes
    /// </summary>
    function GetListOfLanguages( aConnection: TFDConnection ): TFDQuery;
  end;

implementation

{ TLanguages }

constructor TLanguages.Create;
begin
  FQryLanguages := TFDQuery.Create( nil );

  FQryLanguages.Name := 'QryListeLanguages';
  FQryLanguages.SQL.Clear;
  FQryLanguages.SQL.Add( '''
    select * from LANGUAGE
    order by LANGUAGE_NAME
  ''');
end;

destructor TLanguages.Destroy;
begin
FreeAndNil(FQryLanguages);

  inherited;
end;

function TLanguages.GetListOfLanguages(aConnection: TFDConnection): TFDQuery;
begin
  FQryLanguages.Close;
  FQryLanguages.Connection := aConnection;
  FQryLanguages.Open;

  Result := FQryLanguages;
end;

end.
