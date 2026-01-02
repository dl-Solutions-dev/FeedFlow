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

unit uInterfaces;

interface

uses
  Web.HTTPApp,
  Web.Stencils;

type
  IInvokeAction = interface;

  IAction = interface
    ['{EDD3F333-F82D-4618-B49D-450E02D3C16C}']
    procedure InitializeActions( aWebModule: TWebModule; aWebStencil:TWebStencilsEngine );
  end;

  IInvokeAction = interface
    ['{D75EDAD9-1C9E-43AF-8A9A-1F164703335E}']
    procedure AddAction( aAction : IAction);
    procedure RemoveAction( aAction: IAction);
    procedure InitializeActions( aWebModule:TWebModule; aWebStencil:TWebStencilsEngine);
  end;

implementation

end.
