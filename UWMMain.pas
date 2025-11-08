unit UWMMain;

interface

uses
  System.SysUtils,
  System.Classes,
  Web.HTTPApp,
  Web.Stencils,
  uInvokerActions,
  Helpers.Messages,
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
  private
    { Private declarations }
    FResourcesPath: string;
    FDM: TDMSession;

    procedure InitRequiredData;
  public
    { Public declarations }
    property DM: TDMSession read FDM;
  end;

var
  WebModuleClass: TComponentClass = TWebModule1;

implementation

uses
  System.IOUtils,
  Utils.Logger;

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

procedure TWebModule1.WebModuleCreate( Sender: TObject );
begin
  InitRequiredData;

  TInvokerActions.GetInvokerActions.InitializeActions( Self, wsEngineApplication );
end;

procedure TWebModule1.InitRequiredData;
begin
  FResourcesPath := './';

  wsEngineApplication.RootDirectory := TPath.Combine( FResourcesPath, 'Templates' );
  WebFileDispatcher.RootDirectory := wsEngineApplication.RootDirectory;

  // Mettre ça dans le onauthenticate lorsqu'il y aura une authentification
  FDM := TDMSession.Create( nil );

  Logger.Info( 'datamodule chargés' );

  Request.Session.DataVars.AddObject( 'DM', FDM );
  //--
end;

procedure TWebModule1.WebModule1DefaultHandlerAction( Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean );
begin
  Response.Content :=
    '<html>' +
    '<head><title>Web Server Application</title></head>' +
    '<body>Web Server Application</body>' +
    '</html>';
end;

procedure TWebModule1.WebModuleAfterDispatch( Sender: TObject; Request:
  TWebRequest; Response: TWebResponse; var Handled: Boolean );
begin
  var IsRedirect := ( Response.StatusCode >= 300 ) and ( Response.StatusCode <
    400 );
  if not ( IsRedirect ) and Assigned( Request.Session ) then
    TMessageManager.ClearMessages( Request.Session );
end;

end.

