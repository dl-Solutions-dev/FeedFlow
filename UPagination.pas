unit UPagination;

interface

uses
  system.Classes,
  system.SysUtils,
  system.Generics.Collections,
  Communs.SerializableClass;

type
  TPagination = class;

  TPage = class( TSerializableClass )
  private
    FNoPage: Integer;
    FText: string;
    FLast: Boolean;
    FFirst: Boolean;
    FIsActualPge: Boolean;
//    FPagination: TPagination;
    FIsClickable: Boolean;
    FActualPage: Integer;

    function GetIsActualPage: Boolean;
    function GetIsClickable: Boolean;

    procedure SetNoPage( const Value: Integer );
    procedure SetText( const Value: string );
    procedure SetFirst( const Value: Boolean );
    procedure SetLast( const Value: Boolean );
//    procedure SetPagination( const Value: TPagination );
    procedure SetIsClickable( const Value: Boolean );
    procedure SetActualPage(const Value: Integer);
  public
    property NoPage: Integer read FNoPage write SetNoPage;
    property ActualPage: Integer read FActualPage write SetActualPage;
    property Text: string read FText write SetText;
    property First: Boolean read FFirst write SetFirst;
    property Last: Boolean read FLast write SetLast;
    property IsActualPage: Boolean read GetIsActualPage write FIsActualPge;
    property IsClickable: Boolean read GetIsClickable write SetIsClickable;
//    property Pagination: TPagination read FPagination write SetPagination;
  end;

  TPagination = class( TSerializableClass )
  private
    FPagesList: TObjectList< TPage >;
    FActualPage: Integer;
    FLinesPerPage: Integer;
    FActionList: string;
    FActionPagination: string;
    FSearch: string;
    FNbTabs: SmallInt;
    FUrlParameters: string;

    procedure EntireList( aNbPages: SmallInt );
    procedure PartialList( aNbPages: SmallInt );

    procedure SetActualPage( const Value: Integer );
    procedure SetPagesList( const Value: TObjectList< TPage > );
    procedure SetLinesPerPage( const Value: Integer );
    procedure SetActionList( const Value: string );
    procedure SetActionPagination( const Value: string );
    procedure SetSearch( const Value: string );
    procedure SetNbTabs( const Value: SmallInt );
    procedure SetUrlParameters( const Value: string );
  public
    constructor Create;
    destructor Destroy; override;

    procedure GeneratePagesList( NbRecords, NbPerPage, aActualPage: SmallInt; aUrlParameters, aSearch, aActionList, aActionPagination: string );

    property ActionList: string read FActionList write SetActionList;
    property ActionPagination: string read FActionPagination write SetActionPagination;
    property actualPage: Integer read FActualPage write SetActualPage;
    property LinesPerPage: Integer read FLinesPerPage write SetLinesPerPage;
    property PagesList: TObjectList< TPage > read FPagesList write SetPagesList;
    property Search: string read FSearch write SetSearch;
    property NbTabs: SmallInt read FNbTabs write SetNbTabs;
    property UrlParameters: string read FUrlParameters write SetUrlParameters;
  end;

implementation

{ TPage }

function TPage.GetIsActualPage: Boolean;
begin
  Result := ( NoPage = FActualPage );
end;

function TPage.GetIsClickable: Boolean;
begin
  Result := FText <> '...';
end;

procedure TPage.SetActualPage(const Value: Integer);
begin
  FActualPage := Value;
end;

procedure TPage.SetFirst( const Value: Boolean );
begin
  FFirst := Value;
end;

procedure TPage.SetIsClickable( const Value: Boolean );
begin
  FIsClickable := Value;
end;

procedure TPage.SetLast( const Value: Boolean );
begin
  FLast := Value;
end;

procedure TPage.SetNoPage( const Value: Integer );
begin
  FNoPage := Value;
end;

//procedure TPage.SetPagination( const Value: TPagination );
//begin
//  FPagination := Value;
//end;

procedure TPage.SetText( const Value: string );
begin
  FText := Value;
end;

{ TPagination }

constructor TPagination.Create;
begin
  FPagesList := TObjectList< TPage >.Create;
  FNbTabs := 10;
end;

destructor TPagination.Destroy;
begin
  FreeAndNil( FPagesList );

  inherited;
end;

procedure TPagination.EntireList( aNbPages: SmallInt );
begin
  for var i := 1 to aNbPages do
  begin
    var
    LPage := TPage.Create;
    LPage.NoPage := i;
    LPage.Text := i.ToString;
    LPage.ActualPage := Self.actualPage;
    // --> nécessaire pour connaitre la page actuelle dans les webstencils parce que la variable d'un @foreach ne peut pas être utilisée dans une expression

    FPagesList.Add( LPage );
  end;
end;

procedure TPagination.GeneratePagesList( NbRecords, NbPerPage,
  aActualPage: SmallInt; aUrlParameters, aSearch, aActionList, aActionPagination: string );
var
  LNbPages: SmallInt;
begin
  FLinesPerPage := NbPerPage;
  FActionList := aActionList;
  FActionPagination := aActionPagination;
  FActualPage := aActualPage;
  FSearch := aSearch;
  FUrlParameters := aUrlParameters;

  LNbPages := ( NbRecords div NbPerPage );
  if ( NbRecords mod NbPerPage > 0 ) then
  begin
    Inc( LNbPages );
  end;

  FPagesList.Clear;

  if ( LNbPages <= FNbTabs ) then
  begin
    EntireList( LNbPages );
  end
  else
  begin
    PartialList( LNbPages )
  end;
end;

procedure TPagination.PartialList( aNbPages: SmallInt );
var
  LPage: TPage;
begin
  if ( FActualPage < 4 ) then
  begin
    for var i := 1 to FNbTabs - 1 do
    begin
      LPage := TPage.Create;
      LPage.NoPage := i;
      LPage.Text := i.ToString;
      LPage.ActualPage := Self.actualPage;

      FPagesList.Add( LPage );
    end;

    LPage := TPage.Create;
    LPage.NoPage := 0;
    LPage.Text := '...';
    LPage.ActualPage := Self.actualPage;

    FPagesList.Add( LPage );

    LPage := TPage.Create;
    LPage.NoPage := aNbPages;
    LPage.Text := aNbPages.ToString;
    LPage.ActualPage := Self.actualPage;

    FPagesList.Add( LPage );
  end
  else if ( FActualPage > 3 ) and ( FActualPage < aNbPages - ( FNbTabs - 3 ) ) then
  begin
    LPage := TPage.Create;
    LPage.NoPage := 0;
    LPage.Text := '1';
    LPage.ActualPage := Self.actualPage;

    FPagesList.Add( LPage );

    LPage := TPage.Create;
    LPage.NoPage := 0;
    LPage.Text := '...';
    LPage.ActualPage := Self.actualPage;

    FPagesList.Add( LPage );

    for var i := FActualPage - 1 to FActualPage + FNbTabs - 3 - 1 do
    begin
      LPage := TPage.Create;
      LPage.NoPage := i;
      LPage.Text := i.ToString;
      LPage.ActualPage := Self.actualPage;

      FPagesList.Add( LPage );
    end;

    LPage := TPage.Create;
    LPage.NoPage := 0;
    LPage.Text := '...';
    LPage.ActualPage := Self.actualPage;

    FPagesList.Add( LPage );

    LPage := TPage.Create;
    LPage.NoPage := aNbPages;
    LPage.Text := aNbPages.ToString;
    LPage.ActualPage := Self.actualPage;

    FPagesList.Add( LPage );
  end
  else
  begin
    LPage := TPage.Create;
    LPage.NoPage := 0;
    LPage.Text := '1';
    LPage.ActualPage := Self.actualPage;

    FPagesList.Add( LPage );

    LPage := TPage.Create;
    LPage.NoPage := 0;
    LPage.Text := '...';
    LPage.ActualPage := Self.actualPage;

    FPagesList.Add( LPage );

    for var i := aNbPages - ( FNbTabs - 2 ) to aNbPages do
    begin
      LPage := TPage.Create;
      LPage.NoPage := i;
      LPage.Text := i.ToString;
      LPage.ActualPage := Self.actualPage;

      FPagesList.Add( LPage );
    end;
  end;
end;

procedure TPagination.SetActionList( const Value: string );
begin
  FActionList := Value;
end;

procedure TPagination.SetActionPagination( const Value: string );
begin
  FActionPagination := Value;
end;

procedure TPagination.SetActualPage( const Value: Integer );
begin
  FActualPage := Value;
end;

procedure TPagination.SetLinesPerPage( const Value: Integer );
begin
  FLinesPerPage := Value;
end;

procedure TPagination.SetNbTabs( const Value: SmallInt );
begin
  FNbTabs := Value;
end;

procedure TPagination.SetPagesList( const Value: TObjectList< TPage > );
begin
  FPagesList := Value;
end;

procedure TPagination.SetSearch( const Value: string );
begin
  FSearch := Value;
end;

procedure TPagination.SetUrlParameters( const Value: string );
begin
  FUrlParameters := Value;
end;

end.
