unit USubcategories;

interface

uses
  System.Classes,
  System.SysUtils,
  FireDAC.Comp.Client,
  Data.DB;

type
  TSubcategories = class
  strict private
    FQrySubcategories: TFDQuery;
  public
    constructor Create;
    destructor Destroy; override;

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
