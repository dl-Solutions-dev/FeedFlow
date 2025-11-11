(* C2PP
  ***************************************************************************

  Copyright D. LEBLANC 2025
  Ce programme peut être copié et utilisé librement.

  ***************************************************************************

  Ce projet est une démo des possibilités combinés des webstencils et de
  HTMX.

  ***************************************************************************
  File last update : 2025-07-05T18:00:34.000+02:00
  Signature : a3da069154ec278374c13e9e4d0cb248d952edd9
  ***************************************************************************
*)

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
  UDMSession;

type
  TActions = class
  private
    FCancelEditLine: string;
    FApplyEditLine: string;
    FNavigation: string;
    FListe: string;
    FCancelAddLine: string;
    FApplyAddLine: string;
    FDeleteLine: string;
    FEditLine: string;
    FAddLine: string;
  public
    constructor Create( aListe, aNavigation, aEditLine, aCancelEditLine,
      aApplyEditLine, aDeleteLine, aAddLine, aCancelAddLine,
      aApplyAddLine: string );

    property Liste: string read FListe;
    property Navigation: string read FNavigation;
    property EditLine: string read FEditLine;
    property CancelEditLine: string read FCancelEditLine;
    property ApplyEditLine: string read FApplyEditLine;
    property DeleteLine: string read FDeleteLine;
    property AddLine: string read FAddLine;
    property CancelAddLine: string read FCancelAddLine;
    property ApplyAddLine: string read FApplyAddLine;
  end;

  // Field error manager for WebStencils compatibility
  {$M+}
  TFieldErrorManager = class
  private
    FErrors: TDictionary<string, string>;
    function GetErrorCount: integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddError( const AFieldName, AMessage: string );
    procedure Clear;
  published
    function GetError( const AFieldName: string ): string;
    function HasError( const AFieldName: string ): Boolean;
    property ErrorCount: integer read GetErrorCount;
  end;
  {$M-}

  TBaseController = class( TInterfacedObject, IAction )
  private
    FLayoutTemplate: string;
    FLocation: string;

    procedure SetTitre(const Value: string);
  protected
    // FWebStencilsProcessor: TWebStencilsProcessor;
    FWebStencilsEngine: TWebStencilsEngine;
    FWebStencilsProcessor: TWebStencilsProcessor;
    FFieldErrorManager: TFieldErrorManager;
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

    procedure SendEmptyContent( aResponse: TWebResponse );
    procedure ClearValidationErrors;

    // Validation error management
    procedure InitializeValidationErrors;
    procedure AddValidationError( const AFieldName, AMessage: string );

    function GetFieldError( const AFieldName: string ): string;
    function HasFieldError( const AFieldName: string ): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure InitializeActions( aWebModule: TWebModule; aWebStencil: TWebStencilsEngine ); virtual;
    procedure CheckSession( Request: TWebRequest );

    property LayoutTemplate: string read FLayoutTemplate;
    property Location: string read FLocation;
    property Titre:string read FTitre write SetTitre;
  end;

implementation

uses
  System.StrUtils,
  System.IOUtils,
  UConsts,
  Utils.Logger,
  Utils.Config;

{ TBaseController }

procedure TBaseController.AddValidationError( const AFieldName,
  AMessage: string );
begin
  FFieldErrorManager.AddError( AFieldName, AMessage );
end;

procedure TBaseController.CheckSession( Request: TWebRequest );
begin
  if ( Request.QueryFields.Values[ 'sesid' ] <> '' ) then
  begin

  end;
end;

procedure TBaseController.ClearValidationErrors;
begin
  //  FFieldErrorManager.Clear;
end;

constructor TBaseController.Create;
begin
  FLocation := TConfig.GetInstance.Location; // LOCAT;
end;

destructor TBaseController.Destroy;
begin
  FreeAndNil( FWebStencilsProcessor );
  FreeAndNil( FFieldErrorManager );

  inherited;
end;

function TBaseController.GetDMSession( Request: TWebRequest ): TDMSession;
begin
  Result := TDMSession( Request.Session.DataVars.Objects[ Request.Session.DataVars.IndexOf( 'DM' ) ] );

//  FWebStencilsProcessor.AddVar( 'Menus', Result.Menus, False );
end;

function TBaseController.GetFieldError( const AFieldName: string ): string;
begin
  Result := FFieldErrorManager.GetError( AFieldName );
end;

function TBaseController.HasFieldError( const AFieldName: string ): Boolean;
begin
  Result := FFieldErrorManager.HasError( AFieldName );
end;

function TBaseController.HtmlTemplate( aTemplateName: string ): string;
begin
  Result := TConfig.GetInstance.TemplateFolder+ aTemplateName; // TEMPLATE_FOLDER + aTemplateName;
end;

procedure TBaseController.InitializeActions( aWebModule: TWebModule;
  aWebStencil: TWebStencilsEngine );
begin
  FLayoutTemplate := TConfig.GetInstance.TemplateFolder + LAYOUT_TEMPLATE; // TEMPLATE_FOLDER + LAYOUT_TEMPLATE;

  try
    FWebStencilsEngine := aWebStencil;
    FWebStencilsProcessor := TWebStencilsProcessor.Create( nil );
    FWebStencilsProcessor.Engine := FWebStencilsEngine;
    FWebStencilsProcessor.DataVars.Duplicates:=TWebStencilsDataVarDuplicates.ddReplace;

    //    FControllerName := AControllerName;

        // Initialize field error manager
    FFieldErrorManager := TFieldErrorManager.Create;

    // Initialize validation errors in template processor
    InitializeValidationErrors;

    //    Logger.Debug(Format('Created base controller: %s', [FControllerName]));
  except
    on E: Exception do
      Logger.Error( Format( 'TBaseController constructor error: %s', [ E.Message ] ) );
  end;
end;

procedure TBaseController.InitializeValidationErrors;
begin
  // Always add field error manager to template processor (empty by default)
  FWebStencilsProcessor.AddVar( 'fieldErrors', FFieldErrorManager, False );
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

    // Clear validation errors after template processing
    ClearValidationErrors;
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

procedure TBaseController.SetTitre(const Value: string);
begin
  FTitre := Value;
end;

function TBaseController.WebModule( aWebActionitem: TObject ): TWebModule;
begin
  Result := TWebModule( TWebActionItem( aWebActionitem ).Collection.Owner )
end;

{ TActions }

constructor TActions.Create( aListe, aNavigation, aEditLine, aCancelEditLine,
  aApplyEditLine, aDeleteLine, aAddLine, aCancelAddLine, aApplyAddLine: string
  );
begin
  FListe := aListe;
  FNavigation := aNavigation;
  FEditLine := aEditLine;
  FCancelEditLine := aCancelEditLine;
  FApplyEditLine := aApplyEditLine;
  FDeleteLine := aDeleteLine;
  FAddLine := aAddLine;
  FCancelAddLine := aCancelAddLine;
  FApplyAddLine := aApplyAddLine;
end;

{ TFieldErrorManager }

procedure TFieldErrorManager.AddError( const AFieldName, AMessage: string );
begin
  FErrors.AddOrSetValue( AFieldName.ToUpper, AMessage );
end;

procedure TFieldErrorManager.Clear;
begin
  FErrors.Clear;
end;

constructor TFieldErrorManager.Create;
begin
  inherited Create;
  FErrors := TDictionary<string, string>.Create;
end;

destructor TFieldErrorManager.Destroy;
begin
  FErrors.Free;
  inherited;
end;

function TFieldErrorManager.GetError( const AFieldName: string ): string;
begin
  if FErrors.ContainsKey( AFieldName.ToUpper ) then
    Result := FErrors[ AFieldName.ToUpper ]
  else
    Result := '';
end;

function TFieldErrorManager.GetErrorCount: integer;
begin
  Result := FErrors.Count;
end;

function TFieldErrorManager.HasError( const AFieldName: string ): Boolean;
begin
  Result := FErrors.ContainsKey( AFieldName.ToUpper );
end;

end.

