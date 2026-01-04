/// <summary>
///   UI du serveur de tests
/// </summary>
unit UWMMain;

interface

uses
  System.SysUtils,
  System.Classes,
  Web.HTTPApp,
  Web.Stencils,
  Helpers.Messages,
  System.JSON,
  UDmSession;

type
  TwmMain = class( TWebModule )
    wsEngineApplication: TWebStencilsEngine;
    WebFileDispatcher: TWebFileDispatcher;
    WebSessionManager: TWebSessionManager;
    WebStencilsProcessor1: TWebStencilsProcessor;
    procedure WebModuleDestroy( Sender: TObject );
    procedure WebModuleCreate( Sender: TObject );
    procedure WebModule1DefaultHandlerAction( Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
    procedure WebModuleAfterDispatch( Sender: TObject; Request: TWebRequest;
      Response: TWebResponse; var Handled: Boolean );
    procedure WebModuleBeforeDispatch( Sender: TObject; Request: TWebRequest;
      Response: TWebResponse; var Handled: Boolean );
    procedure WebSessionManagerCreated( Sender: TCustomWebSessionManager; Request:
      TWebRequest; Session: TWebSession );
  private
    { Private declarations }
    FActionsList: TInterfaceList;
    FResourcesPath: string;
    FDM: TDMSession;

    procedure InitRequiredData;
  public
    { Public declarations }
    function TokenValid( aRequest: TWebRequest; out Payload: TJSONObject ): Boolean;
  end;

var
  WebModuleClass: TComponentClass = TwmMain;

implementation

uses
  System.IOUtils,
  Utils.Logger,
  Utils.Config,
  uBaseController,
  uInterfaces, UControllersRegistry;

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

procedure TwmMain.WebModuleDestroy( Sender: TObject );
begin
  FActionsList.Free;
end;

procedure TwmMain.WebModuleCreate( Sender: TObject );
var
  LController: TBaseControllerRef;
  LInstance: IAction;
begin
  InitRequiredData;

  FActionsList := TInterfaceList.Create;

  for LController in TControllersRegistry.GetControllersList.List do
  begin
    LInstance := LController.Create;
    LInstance.InitializeActions( Self, wsEngineApplication );

    FActionsList.Add( LInstance );
  end;

  FormatSettings.ShortDateFormat := 'dd/mm/YYYY';
end;

procedure TwmMain.InitRequiredData;
begin
  FResourcesPath := TConfig.GetInstance.ResourcePath;

  wsEngineApplication.RootDirectory := TPath.Combine( FResourcesPath, 'Templates' );
  WebFileDispatcher.RootDirectory := wsEngineApplication.RootDirectory;

  Logger.Info( 'Resource ' + TConfig.GetInstance.Resource );

  wsEngineApplication.AddVar( 'env', nil, false,
    function( AVar: TWebStencilsDataVar; const APropName: string; var AValue:
      string ): Boolean
    begin
      if APropName = 'app_name' then
        AValue := TConfig.GetInstance.App_Name // 'Feeds Management'
      else if APropName = 'version' then
        AValue := TConfig.GetInstance.Version // '1.0.0'
      else if APropName = 'edition' then
        AValue := TConfig.GetInstance.Edition // 'WebBroker Delphi'{$IFDEF CONTAINER} + ' in Docker'{$ENDIF}
      else if APropName = 'company' then
        Avalue := TConfig.GetInstance.Company // 'Solution dev'
      else if APropName = 'resource' then
        AValue := TConfig.GetInstance.Resource // {$IFDEF DEBUG}AValue := '.'{$ELSE}AValue := '.'{$ENDIF}
      else if APropName = 'is_rad_server' then
        AValue := 'False'
      else if APropName = 'debug' then
        Avalue := {$IFDEF DEBUG} 'True'{$ELSE} 'False'{$ENDIF}
      else
      begin
        Result := False;
        Exit;
      end;
      Result := True;
    end );
end;

function TwmMain.TokenValid( aRequest: TWebRequest;
  out Payload: TJSONObject ): Boolean;
begin
  Result := True;
end;

procedure TwmMain.WebModule1DefaultHandlerAction( Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
begin
  //  Response.Content :=
  //    '<html>' +
  //    '<head><title>Web Server Application</title></head>' +
  //    '<body>Web Server Application</body>' +
  //    '</html>';
end;

procedure TwmMain.WebModuleAfterDispatch( Sender: TObject; Request:
  TWebRequest; Response: TWebResponse; var Handled: Boolean );
begin
  var IsRedirect := ( Response.StatusCode >= 300 ) and ( Response.StatusCode <
    400 );
  if not ( IsRedirect ) and Assigned( Request.Session ) then
    TMessageManager.ClearMessages( Request.Session );
end;

procedure TwmMain.WebModuleBeforeDispatch( Sender: TObject; Request:
  TWebRequest; Response: TWebResponse; var Handled: Boolean );
begin
  Logger.Info( Request.PathInfo );
end;

procedure TwmMain.WebSessionManagerCreated( Sender:
  TCustomWebSessionManager; Request: TWebRequest; Session: TWebSession );
begin
  Logger.Info( Format( 'New session created: %s', [ Session.Id ] ) );
  Logger.Info( Format( 'Request Path: %s', [ Request.PathInfo ] ) );
  Logger.Info( Format( 'Request Method: %s', [ Request.Method ] ) );

  // Add session creation timestamp for demo purposes
  Session.DataVars.Values[ 'created' ] := FormatDateTime( 'yyyy-mm-dd hh:nn:ss',
    Now );
  TMessageManager.EnsureMessageProvider( Session );
  if Assigned( Session.User ) then
    Logger.Info( Format( 'Session created for authenticated user: %s', [
        Session.User.UserName ] ) )
  else
    Logger.Info( 'Session created for anonymous user' );

  FDM := TDMSession.Create( nil );

  Logger.Info( 'datamodule chargés' );

  Session.DataVars.AddObject( 'DM', FDM );
end;

end.

