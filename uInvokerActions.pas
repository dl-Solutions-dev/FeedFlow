(* C2PP
  ***************************************************************************

  Copyright D. LEBLANC 2025
  Ce programme peut être copié et utilisé librement.

  ***************************************************************************

  Ce projet est une démo des possibilités combinés des webstencils et de
  HTMX.

  ***************************************************************************
  File last update : 2025-07-02T23:53:18.486+02:00
  Signature : 54fbb0cb54d0c1387e1a12b0c0048fbeaa8ff802
  ***************************************************************************
*)

unit uInvokerActions;

interface

uses
  Classes, System.SysUtils, Web.HTTPApp,
  Web.Stencils, uInterfaces;

type
  TInvokerActions = class( TInterfacedObject, IInvokeAction )
  private
    FActionList: TInterfaceList;

    class var FInvoker: TInvokerActions;
  public
    constructor Create;
    destructor Destroy; override;

    class destructor DestroyClass;

    class function GetInvokerActions: TInvokerActions;

    // IInvokeAction
    procedure AddAction( aAction: IAction );
    procedure RemoveAction( aAction: IAction );
    procedure InitializeActions( aWebModule:TWebModule; aWebStencil:TWebStencilsEngine );
//    procedure LogParametres( ASession: TIWApplication );
  end;

implementation

{ TInvokerActions }

procedure TInvokerActions.AddAction( aAction: IAction );
var
  t: textfile;
begin
//  AssignFile( t, ExtractFilePath( ParamStr( 0 ) ) + 'tractinit.txt' );
//  if not( FileExists( ExtractFilePath( ParamStr( 0 ) ) + 'tractinit.txt' ) ) then
//    rewrite( t )
//  else
//    append( t );

//  Writeln( t, 'ajout action' );
//  CloseFile( t );
  if FActionList.IndexOf( aAction ) = -1 then
  begin
    FActionList.Add( aAction );
  end;
end;

constructor TInvokerActions.Create;
begin
  inherited;

  FActionList := TInterfaceList.Create;
end;

destructor TInvokerActions.Destroy;
begin
  FActionList.Free;

  inherited;
end;

class destructor TInvokerActions.DestroyClass;
begin
  if Assigned( FInvoker ) then
    FInvoker.Free;
end;

class function TInvokerActions.GetInvokerActions: TInvokerActions;
begin
  if not Assigned( FInvoker ) then
  begin
    FInvoker := TInvokerActions.Create;
  end;

  Result := FInvoker;
end;

//procedure TInvokerActions.LogParametres( ASession: TIWApplication );
//var
//  I: Integer;
//begin
//  for I := 0 to ASession.RunParams.Count - 1 do
//  begin
//    AddTrace( trInformation, ASession.RunParams.Names[ I ] + ' -> ' + ASession.RunParams.ValueFromIndex[ I ] );
//  end;
//end;

procedure TInvokerActions.InitializeActions( aWebModule:TWebModule; aWebStencil:TWebStencilsEngine );
var
  I: Integer;
  wHandledAction: Boolean;
begin
  wHandledAction := False;

  for I := 0 to FActionList.Count - 1 do
  begin
    IAction( FActionList[ I ] ).InitializeActions( aWebModule, aWebStencil );

    if wHandledAction then
      Break;
  end;

  if not( wHandledAction ) then
  begin

  end;
end;

procedure TInvokerActions.RemoveAction( aAction: IAction );
begin
  if FActionList.IndexOf( aAction ) <> -1 then
    FActionList.Remove( aAction );
end;

end.
