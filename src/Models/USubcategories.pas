/// <summary>
///   Modèle sous-catégories
/// </summary>
unit USubcategories;

interface

uses
  System.Classes,
  System.SysUtils,
  FireDAC.Comp.Client,
  Data.DB;

type
  /// <summary>
  ///   Sous-catégories
  /// </summary>
  TSubcategories = class
  strict private
    /// <summary>
    ///   Intance du FDQuery
    /// </summary>
    FQrySubcategories: TFDQuery;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    ///   Retournel al iste des sous-catégories
    /// </summary>
    function GetListOfSubcategories( aConnection: TFDConnection ): TFDQuery;
  end;

implementation

{ TSubcategories }

constructor TSubcategories.Create;
begin
  FQrySubcategories := TFDQuery.Create( nil );

  FQrySubcategories.Name := 'QryListeSubcategories';
  FQrySubcategories.SQL.Clear;
  FQrySubcategories.SQL.Add( '''
    select * from SUBCATEGORY
    order by SUBCATEGORY_NAME
  ''');
end;

destructor TSubcategories.Destroy;
begin
  FreeAndNil(FQrySubcategories);

  inherited;
end;

function TSubcategories.GetListOfSubcategories(
  aConnection: TFDConnection): TFDQuery;
begin
  FQrySubcategories.Close;
  FQrySubcategories.Connection := aConnection;
  FQrySubcategories.Open;

  Result := FQrySubcategories;
end;

end.
