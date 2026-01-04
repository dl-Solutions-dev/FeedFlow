(* C2PP
  ***************************************************************************

  Copyright D. LEBLANC 2025
  Ce programme peut être copié et utilisé librement.

  ***************************************************************************

  Ce projet est une démo des possibilités combinés des webstencils et de
  HTMX.

  ***************************************************************************
  File last update : 2025-07-02T23:53:18.485+02:00
  Signature : 69d33535ea89c9048777ea3648404e228913f5eb
  ***************************************************************************
*)

/// <summary>
///   Interfaces
/// </summary>
unit uInterfaces;

interface

uses
  Web.HTTPApp,
  Web.Stencils;

type
  /// <summary>
  ///   Interface à utiliser pour les controller quidoivent exposer une
  ///   méthode Initializations dans laquelle ils doivent enregistrer les
  ///   routes qu'ils exposent
  /// </summary>
  IAction = interface
    ['{EDD3F333-F82D-4618-B49D-450E02D3C16C}']
    procedure InitializeActions( aWebModule: TWebModule; aWebStencil:TWebStencilsEngine );
  end;

implementation

end.
