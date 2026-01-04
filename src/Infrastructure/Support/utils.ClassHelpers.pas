(* C2PP
  ***************************************************************************

  Copyright D. LEBLANC 2025
  Ce programme peut être copié et utilisé librement.

  ***************************************************************************

  Ce projet est une démo des possibilités combinés des webstencils et de
  HTMX.

  ***************************************************************************
  File last update : 2025-07-02T23:53:18.494+02:00
  Signature : d6bfc0e581c774400da55a334cf56a120cdefebc
  ***************************************************************************
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
