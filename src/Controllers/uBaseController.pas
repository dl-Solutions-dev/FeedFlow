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
  File last update : 2026-01-10T18:59:10.000+01:00
  Signature : 56ecaa49942e7b6447d05da32d4aeec1e8feda9a
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
  public
    constructor Create;
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

constructor TBaseController.Create;
begin
  //  FLocation := TConfig.GetInstance.Location; // LOCAT;
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

procedure TBaseController.SetTitre( const Value: string );
begin
  FTitre := Value;
end;

function TBaseController.WebModule( aWebActionitem: TObject ): TWebModule;
begin
  Result := TWebModule( TWebActionItem( aWebActionitem ).Collection.Owner )
end;

end.

