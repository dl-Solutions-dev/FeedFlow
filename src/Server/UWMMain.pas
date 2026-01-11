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
  File last update : 2026-01-04T14:37:08.884+01:00
  Signature : b9fc5147249a0b46e464dbd9c48dfc11c33ff78e
  ***************************************************************************
*)

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
  UDmSession,
  Utils.Token;

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
    function ValidToken( Request: TWebRequest; aHeader: Boolean; aLoadData: Boolean; out Token: TToken ): Boolean;
  public
    { Public declarations }
    Token: TToken;

    //    function TokenValid( aRequest: TWebRequest; out Payload: TJSONObject ): Boolean;
  end;

var
  WebModuleClass: TComponentClass = TwmMain;

implementation

uses
  System.IOUtils,
  Utils.Logger,
  Utils.Config,
  uBaseController,
  uInterfaces,
  UControllersRegistry;

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

procedure TwmMain.WebModuleDestroy( Sender: TObject );
begin
  FActionsList.Free;
  FreeAndNil( Token );
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

//function TwmMain.TokenValid( aRequest: TWebRequest;
//  out Payload: TJSONObject ): Boolean;
//begin
//  Result := True;
//end;

function TwmMain.ValidToken( Request: TWebRequest; aHeader, aLoadData: Boolean;
  out Token: TToken ): Boolean;
var
  LReqToken: string;
  LToken: TToken;
begin
  if aHeader then
  begin
    // Récupérer le LReqToken depuis l'en-tête "Authorization"
    Request.AllHeaders.NameValueSeparator := ':';
    LReqToken := Request.AllHeaders.Values[ 'jwt' ].Trim;
  end
  else
  begin
    LReqToken := Request.CookieFields.Values[ 'jwt' ];
  end;

  if ( LReqToken <> '' ) then
  begin
    if LReqToken.StartsWith( 'Bearer ' ) then
      LReqToken := LReqToken.Substring( 7 ); // Enlever "Bearer "

    LToken := TToken.Create;
    Result := LToken.Valid( LReqToken, aLoadData );

    if aLoadData then
    begin
      Token := LToken;
    end
    else
    begin
      FreeAndNil( LToken );
    end;
  end
  else
  begin
    Result := False;
  end;
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

  FreeAndNil( Token );
end;

procedure TwmMain.WebModuleBeforeDispatch( Sender: TObject; Request:
  TWebRequest; Response: TWebResponse; var Handled: Boolean );

begin
  Logger.Info( Request.PathInfo );

  if ( Request.PathInfo <> '/' )
    and ( Request.PathInfo.ToLower <> '/login' )
    and ( Request.PathInfo.ToLower <> '/favicon.ico' )
    and not ( Request.PathInfo.Contains( '/static', True ) )
    and not ( ValidToken( Request, False, True, Token )
    ) then
  begin
    FreeAndNil( Token );
    Response.StatusCode := 401;
    Response.SendRedirect( './' );
  end;
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

