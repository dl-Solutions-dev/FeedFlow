/// <summary>
///   Liste des class de controller
/// </summary>
unit UControllersRegistry;

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
  /// <summary>
  ///   Singleton contenant la liste des class controllers
  /// </summary>
  TControllersRegistry = class( TInterfacedObject )
  private
    FList: TList<TBaseControllerRef>;

    class var FControllersList: TControllersRegistry;
  public
    constructor Create;
    destructor Destroy; override;

    class destructor DestroyClass;

    /// <summary>
    ///   Fournit l'instance du singleton
    /// </summary>
    class function GetControllersList: TControllersRegistry;

    /// <summary>
    ///   Méthode permettant à chaque class de controller de s'enregistrer
    /// </summary>
    procedure AddClass( aClass: TBaseControllerRef );

    /// <summary>
    ///   Contient la liste
    /// </summary>
    property List: TList<TBaseControllerRef> read FList;
  end;

implementation

{ TControllersRegistry }

procedure TControllersRegistry.AddClass( aClass: TBaseControllerRef );
begin
  FList.Add( aClass );
end;

constructor TControllersRegistry.Create;
begin
  FList := TList<TBaseControllerRef>.Create;
end;

destructor TControllersRegistry.Destroy;
begin
  FList.Free;

  inherited;
end;

class destructor TControllersRegistry.DestroyClass;
begin
  if Assigned( FControllersList ) then
    FControllersList.Free;
end;

class function TControllersRegistry.GetControllersList: TControllersRegistry;
begin
  if not Assigned( FControllersList ) then
  begin
    FControllersList := TControllersRegistry.Create;
  end;

  Result := FControllersList;
end;

end.

