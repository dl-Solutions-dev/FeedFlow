unit UCategories;

interface

uses
  System.Classes,
  System.SysUtils,
  FireDAC.Comp.Client,
  Data.DB;

type
  TCategories = class
  strict private
    FQryCategories: TFDQuery;
  public
    constructor Create;
    destructor Destroy; override;
    function GetListOfCategories( aConnection: TFDConnection ): TFDQuery;
  end;

implementation

{ TCategories }

constructor TCategories.Create;
begin
  FQryCategories := TFDQuery.Create( nil );

  FQryCategories.Name := 'QryListeCategories';
  FQryCategories.SQL.Clear;
  FQryCategories.SQL.Add( '''
    select * from CATEGORY
    order by CATEGORY_NAME
  ''');
end;

destructor TCategories.Destroy;
begin
  FreeAndNil( FQryCategories );

  inherited;
end;

function TCategories.GetListOfCategories( aConnection: TFDConnection ): TFDQuery;
begin
  FQryCategories.Close;
  FQryCategories.Connection := aConnection;
  FQryCategories.Open;

  Result := FQryCategories;
end;

end.

