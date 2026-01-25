unit UGroups;

interface

uses
  System.Classes,
  System.SysUtils,
  FireDAC.Comp.Client,
  FireDAC.Stan.Param,
  Data.DB;

type
  TGroups = class
  strict private
    FQryGroups: TFDQuery;
  public
    constructor Create;
    destructor Destroy; override;

    function GetListOfGroups( aConnection: TFDConnection ): TFDQuery;
  end;

implementation

{ TGroups }

constructor TGroups.Create;
begin
  FQryGroups := TFDQuery.Create( nil );
  FQryGroups.SQL.Text := '''
    select * from GROUPS
    order by GROUP_ID
  ''';
end;

destructor TGroups.Destroy;
begin
  FreeAndNil( FQryGroups );

  inherited;
end;

function TGroups.GetListOfGroups( aConnection: TFDConnection ): TFDQuery;
begin
  FQryGroups.Connection := aConnection;
  FQryGroups.Open;

  Result := FQryGroups;
end;

end.

