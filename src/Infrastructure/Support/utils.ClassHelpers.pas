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
  File last update : 2026-01-11T10:34:56.000+01:00
  Signature : 8680d08cc0a5022f593a1037ca368b90ec672ca6
  ***************************************************************************
*)

(*
  this code is from https://github.com/Embarcadero/WebStencilsDemos/blob/main/WebBrokerProject/Delphi/Helpers.WebModule.pas
*)

/// <summary>
///   Helper ajoutant la possibilité de créer des route facilement par code
/// </summary>
unit utils.ClassHelpers;

interface

uses
  Web.HTTPApp,
  System.SysUtils;

type
  /// <summary>
  ///   Route
  /// </summary>
  TRoute = record
  public
    /// <summary>
    ///   Type de route (Get - Post - Delete - ...)
    /// </summary>
    MethodType: TMethodType;
    /// <summary>
    ///   Path de l'url
    /// </summary>
    PathInfo: string;
    /// <summary>
    ///   Action associée
    /// </summary>
    OnAction: THTTPMethodEvent;
    /// <summary>
    ///   Indique la route par défaut
    /// </summary>
    Default: Boolean;
    constructor Create(const AMethodType: TMethodType; const APathInfo: String; const AOnAction: THTTPMethodEvent;
      const ADefault: Boolean = false);
  end;

  // Allows creating Actions/Routes in a more declarative way
  /// <summary>
  ///   Permet d'ajouter des actions au datamodule
  /// </summary>
  TWebModuleHelper = class Helper for TwebModule
  public
    /// <summary>
    ///   Ajoute une route
    /// </summary>
    /// <param name="AMethodType">
    ///   Type de méthode (Get - Post - ...)
    /// </param>
    /// <param name="APathInfo">
    ///   Path de l'url
    /// </param>
    /// <param name="AOnAction">
    ///   Méthode à appeler
    /// </param>
    /// <param name="ADefault">
    ///   Indique l'action par défaut
    /// </param>
    function AddAction(const AMethodType: TMethodType; const APathInfo: String; const AOnAction: THTTPMethodEvent;
      const ADefault: Boolean = false): TwebModule;
    /// <summary>
    ///   Ajpoute les routes
    /// </summary>
    /// <param name="ARoutes">
    ///   Tableau contenant l'ensemble des routes
    /// </param>
    procedure AddRoutes(ARoutes: array of TRoute);
  end;

implementation

uses
  RTTI;

{ TRoute }

constructor TRoute.Create(const AMethodType: TMethodType; const APathInfo: string; const AOnAction: THTTPMethodEvent;
  const ADefault: Boolean = false);
begin
  MethodType := AMethodType;
  PathInfo := APathInfo;
  OnAction := AOnAction;
  default := ADefault;
end;

{ TWebModuleHelper }

function TWebModuleHelper.AddAction(const AMethodType: TMethodType; const APathInfo: String;
  const AOnAction: THTTPMethodEvent; const ADefault: Boolean = false): TwebModule;
begin
  var act := Actions.Add;
  act.MethodType := AMethodType;
  // Convert the enum AMethodType to string to add it to the Action name and avoid collisions between endpoints
  var MethodTypeString := TRttiEnumerationType.GetName(AMethodType);
  act.Name := APathInfo + MethodTypeString;
  act.PathInfo := APathInfo;
  act.OnAction := AOnAction;
  act.Default := ADefault;
  Result := self;
end;

procedure TWebModuleHelper.AddRoutes(ARoutes: array of TRoute);
begin
  for var Route in ARoutes do
    AddAction(Route.MethodType, Route.PathInfo, Route.OnAction, Route.Default);
end;

end.
