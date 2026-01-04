(* C2PP
  ***************************************************************************

  Feed Flow

    Copyright 2026 - Dany Leblanc under AGPL 3.0 license.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
  OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
    THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
  OTHER
    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
    FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
    DEALINGS IN THE SOFTWARE.

  ***************************************************************************

  This program is a demo of the possibilities offered by the new WebStencils
  framework, combined with the HTMX JavaScript library.

  ***************************************************************************

  Author(s) :
  Dany Leblanc

  Project site :
  https://github.com/dl-Solutions-dev/FeedFlow

  ***************************************************************************
  File last update : 2026-01-04T14:37:08.737+01:00
  Signature : 30c195c45d502fe47231af0051af5a2688be18d1
  ***************************************************************************
*)

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

