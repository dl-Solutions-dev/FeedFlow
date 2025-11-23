unit UWMMain;

interface

uses
  System.SysUtils,
  System.Classes,
  Web.HTTPApp,
  Web.Stencils,
  uInvokerActions,
  Helpers.Messages, System.JSON,
  UDmSession;

type
  TWebModule1 = class( TWebModule )
    wsEngineApplication: TWebStencilsEngine;
    WebFileDispatcher: TWebFileDispatcher;
    WebSessionManager: TWebSessionManager;
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
    FResourcesPath: string;
    FDM: TDMSession;

    procedure InitRequiredData;
  public
    { Public declarations }
    function TokenValid(aRequest:TWebRequest; out Payload:TJSONObject):Boolean;
  end;

var
  WebModuleClass: TComponentClass = TWebModule1;

implementation

uses
  System.IOUtils,
  Utils.Logger,
  Utils.Config;

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

procedure TWebModule1.WebModuleCreate( Sender: TObject );
begin
  InitRequiredData;

  TInvokerActions.GetInvokerActions.InitializeActions( Self, wsEngineApplication );

  FormatSettings.ShortDateFormat := 'dd/mm/YYYY';
end;

procedure TWebModule1.InitRequiredData;
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

function TWebModule1.TokenValid(aRequest: TWebRequest;
  out Payload: TJSONObject): Boolean;
begin
  Result:=True;
end;

procedure TWebModule1.WebModule1DefaultHandlerAction( Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
begin
  //  Response.Content :=
  //    '<html>' +
  //    '<head><title>Web Server Application</title></head>' +
  //    '<body>Web Server Application</body>' +
  //    '</html>';
end;

procedure TWebModule1.WebModuleAfterDispatch( Sender: TObject; Request:
  TWebRequest; Response: TWebResponse; var Handled: Boolean );
begin
  var IsRedirect := ( Response.StatusCode >= 300 ) and ( Response.StatusCode <
    400 );
  if not ( IsRedirect ) and Assigned( Request.Session ) then
    TMessageManager.ClearMessages( Request.Session );
end;

procedure TWebModule1.WebModuleBeforeDispatch( Sender: TObject; Request:
  TWebRequest; Response: TWebResponse; var Handled: Boolean );
begin
  Logger.Info( Request.PathInfo );
end;

procedure TWebModule1.WebSessionManagerCreated( Sender:
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

