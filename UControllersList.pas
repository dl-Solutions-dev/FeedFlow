unit UControllersList;

interface

uses
  Classes,
  System.SysUtils,
  Web.HTTPApp,
  Web.Stencils,
  uInterfaces,
  System.Generics.Collections,
  uBaseController;

type
  TControllersList = class( TInterfacedObject )
  private
    FList: TList<TBaseControllerRef>;

    class var FControllersList: TControllersList;
  public
    constructor Create;
    destructor Destroy; override;

    class destructor DestroyClass;

    class function GetControllersList: TControllersList;

    procedure AddClass( aClass: TBaseControllerRef );

    property List: TList<TBaseControllerRef> read FList;
  end;

implementation

{ TControllersList }

procedure TControllersList.AddClass( aClass: TBaseControllerRef );
begin
  FList.Add( aClass );
end;

constructor TControllersList.Create;
begin
  FList := TList<TBaseControllerRef>.Create;
end;

destructor TControllersList.Destroy;
begin
  FList.Free;

  inherited;
end;

class destructor TControllersList.DestroyClass;
begin
  if Assigned( FControllersList ) then
    FControllersList.Free;
end;

class function TControllersList.GetControllersList: TControllersList;
begin
  if not Assigned( FControllersList ) then
  begin
    FControllersList := TControllersList.Create;
  end;

  Result := FControllersList;
end;

end.

