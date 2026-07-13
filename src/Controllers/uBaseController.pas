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
  File last update : 2026-02-22T00:09:47.384+01:00
  Signature : f3480217af381a47355a38bf26e18e67d05fc6c3
  ***************************************************************************
*)

/// <summary>
///   Unité comprenant la classe de base pour toute les class controller
/// </summary>
unit uBaseController;

interface

uses
  System.Classes,
  System.SysUtils,
  system.Generics.Collections,
  FireDAC.Comp.Client,
  Web.HTTPApp,
  Web.Stencils,
  uInterfaces,
  UDMSession,
  Utils.Token,
  UWMMain;

type
  /// <summary>
  ///   Class ancètre de toute les classes controller
  /// </summary>
  TBaseController = class( TInterfacedObject, IAction )
  private
    //    FLocation: string;

    procedure SetTitre( const Value: string );
  protected
    FWebmodule: TwmMain;
    FWebStencilsEngine: TWebStencilsEngine;
    FWebStencilsProcessor: TWebStencilsProcessor;
    FControllerName: string;
    FMsg: string;
    FTitre: string;

    function PickList( aListe: TFDQuery; aPickListName, aCSSClass, aKey, aValue,
      aSelectedValue: string ): string;
    function LoginUser: string;
    function WebModule( aWebActionitem: TObject ): TWebModule;
    function HtmlTemplate( aTemplateName: string ): string; inline;
    function RenderTemplate( const ATemplatePath: string; ARequest: TWebRequest ): string;
    function GetDMSession( Request: TWebRequest ): TDMSession;
    function GetSessionObject( Request: TWebRequest; aObjectName: string ): TObject;

    procedure AddSessionObject( Request: TWebRequest; aSessionObjectName: string;
      aSessionObject: TObject );
    procedure SendEmptyContent( aResponse: TWebResponse );
    procedure SendJson( Response: TWebResponse; var Handled: Boolean; StatusCode: Integer; Json: string );
    procedure SendJsonOk( Response: TWebResponse; var Handled: Boolean; Json: string );
    procedure SendJsonError( Response: TWebResponse; var Handled: Boolean; StatusCode: Integer; Msg: string );
    procedure ShowToastError( Response: TWebResponse; aMsg: string );
  public
    destructor Destroy; override;

    procedure InitializeActions( aWebModule: TWebModule; aWebStencil: TWebStencilsEngine ); virtual;
    procedure CheckSession( Request: TWebRequest );

    //    property LayoutTemplate: string read FLayoutTemplate;
    //    property Location: string read FLocation;
    property Titre: string read FTitre write SetTitre;
  end;

  /// <summary>
  ///   Référence de class permettant d'instancier les classes de type
  ///   TBAseController
  /// </summary>
  TBaseControllerRef = class of TBaseController;

implementation

uses
  System.StrUtils,
  System.IOUtils,
  UConsts,
  Utils.Logger,
  Utils.Config;

function EscapeJsonUnicode(const AText: string): string;
var
  I: Integer;
  C: Char;
begin
  Result := '';
  for I := 1 to Length(AText) do
  begin
    C := AText[I];
    case C of
      '"': Result := Result + '\"';
      '\': Result := Result + '\\';
    else
      if Ord(C) > 127 then
        Result := Result + Format('\u%.4x', [Ord(C)])
      else
        Result := Result + C;
    end;
  end;
end;

{ TBaseController }

procedure TBaseController.AddSessionObject( Request: TWebRequest;
  aSessionObjectName: string; aSessionObject: TObject );
begin
  Request.Session.DataVars.AddObject( aSessionObjectName, aSessionObject );
end;

procedure TBaseController.CheckSession( Request: TWebRequest );
begin
  if ( Request.QueryFields.Values[ 'sesid' ] <> '' ) then
  begin

  end;
end;

destructor TBaseController.Destroy;
begin
  FreeAndNil( FWebStencilsProcessor );

  inherited;
end;

function TBaseController.GetDMSession( Request: TWebRequest ): TDMSession;
begin
  Result := TDMSession( Request.Session.DataVars.Objects[ Request.Session.DataVars.IndexOf( 'DM' ) ] );
end;

function TBaseController.GetSessionObject( Request: TWebRequest;
  aObjectName: string ): TObject;
begin
  if ( Request.Session.DataVars.IndexOf( aObjectName ) <> -1 ) then
  begin
    Result := Request.Session.DataVars.Objects[ Request.Session.DataVars.IndexOf( aObjectName ) ];
  end
  else
  begin
    Result := nil;
  end;
end;

function TBaseController.HtmlTemplate( aTemplateName: string ): string;
begin
  Result := TConfig.GetInstance.TemplateFolder + aTemplateName; // TEMPLATE_FOLDER + aTemplateName;
end;

procedure TBaseController.InitializeActions( aWebModule: TWebModule;
  aWebStencil: TWebStencilsEngine );
begin
  //  FLayoutTemplate := TConfig.GetInstance.TemplateFolder + LAYOUT_TEMPLATE; // TEMPLATE_FOLDER + LAYOUT_TEMPLATE;

  try
    FWebmodule := TwmMain( aWebModule );

    FWebStencilsEngine := aWebStencil;
    FWebStencilsProcessor := TWebStencilsProcessor.Create( nil );
    FWebStencilsProcessor.Engine := FWebStencilsEngine;
    FWebStencilsProcessor.DataVars.Duplicates := TWebStencilsDataVarDuplicates.ddReplace;

    //    FControllerName := AControllerName;

    //    Logger.Debug(Format('Created base controller: %s', [FControllerName]));
  except
    on E: Exception do
      Logger.Error( Format( 'TBaseController constructor error: %s', [ E.Message ] ) );
  end;
end;

function TBaseController.LoginUser: string;
var
  LProcessorEngine: TWebStencilsProcessor;
begin
  LProcessorEngine := TWebStencilsProcessor.Create( nil );
  try
    LProcessorEngine.Engine := FWebStencilsEngine;
    LProcessorEngine.InputFileName := './templates/Login.html';
    LProcessorEngine.PathTemplate := './Templates';

    Result := LProcessorEngine.Content;
  finally
    FreeAndNil( LProcessorEngine )
  end;
end;

function TBaseController.PickList( aListe: TFDQuery; aPickListName, aCSSClass,
  aKey, aValue,
  aSelectedValue: string ): string;
begin
  Result := '<select id="' + aPickListName + '" name="' + aPickListName + '"' +
    IfThen( aCSSClass <> '', 'class="' + aCSSClass, '' ) + '>';

  aListe.Open;
  aListe.First;

  while not ( aListe.Eof ) do
  begin
    Result := Result + '<option value="' + aListe.FieldByName( aKey ).AsString +
      '"' + IfThen( aListe.FieldByName( aKey ).AsString = aSelectedValue,
      ' selected', '' ) + '>' + aListe.FieldByName( aValue ).AsString +
      '</option>';

    aListe.Next;
  end;

  Result := Result + '</select>';
end;

function TBaseController.RenderTemplate( const ATemplatePath: string;
  ARequest: TWebRequest ): string;
begin
  try
    FWebStencilsProcessor.InputFileName := TPath.Combine( FWebStencilsEngine.RootDirectory, ATemplatePath );
    if Assigned( ARequest ) then
    begin
      FWebStencilsProcessor.WebRequest := ARequest;
    end;
    Result := FWebStencilsProcessor.Content;
  except
    on E: Exception do
    begin
      Logger.Error( Format( 'Error rendering template %s: %s', [ ATemplatePath,
            E.Message ] ) );
      Result := '';
    end;
  end;
end;

procedure TBaseController.SendEmptyContent( aResponse: TWebResponse );
begin
  aResponse.Content := ' ';
  aResponse.ContentLength := 1;
  aResponse.StatusCode := 200;
end;

procedure TBaseController.SendJson( Response: TWebResponse; var Handled: Boolean;
  StatusCode: Integer; Json: string );
begin
  Response.StatusCode := StatusCode;
  Response.ContentType := 'application/json';
  Response.Content := Json;
  Handled := True;
end;

procedure TBaseController.SendJsonError( Response: TWebResponse;
  var Handled: Boolean; StatusCode: Integer; Msg: string );
begin
  SendJson( Response, Handled, StatusCode, Format( '{"error":"%s"}', [ Msg ] ) );
end;

procedure TBaseController.SendJsonOk( Response: TWebResponse;
  var Handled: Boolean; Json: string );
begin
  SendJson( Response, Handled, 200, Json );
end;

procedure TBaseController.SetTitre( const Value: string );
begin
  FTitre := Value;
end;

procedure TBaseController.ShowToastError( Response: TWebResponse; aMsg: string );
begin
  var LPayload := Format( '{"showErrorToast":{"message":"%s"}}',
    [ EscapeJsonUnicode( aMsg.Replace('"', '\"', [rfReplaceAll])) ] );
  Response.CustomHeaders.Values[ 'HX-Trigger' ] := LPayload;
  Response.StatusCode := 400; // ou le code approprié
  Response.Content := ''; // ou un fragment HTML si besoin
end;

function TBaseController.WebModule( aWebActionitem: TObject ): TWebModule;
begin
  Result := TWebModule( TWebActionItem( aWebActionitem ).Collection.Owner )
end;

end.

