program FeedFlow_Tests;
{$APPTYPE GUI}

uses
  Vcl.Forms,
  Web.WebReq,
  IdHTTPWebBrokerBridge,
  UFrmMainTest in 'UFrmMainTest.pas' {FrmMainTest},
  UWMMain in 'UWMMain.pas' {WebModule1: TWebModule},
  uInterfaces in 'uInterfaces.pas',
  uInvokerActions in 'uInvokerActions.pas',
  utils.ClassHelpers in 'utils.ClassHelpers.pas',
  Utils.Logger in 'Utils.Logger.pas',
  UIndexController in 'Controllers\UIndexController.pas',
  uBaseController in 'Controllers\uBaseController.pas',
  UDmSession in 'UDmSession.pas' {DmSession: TDataModule},
  UConsts in 'UConsts.pas',
  Helpers.Messages in 'Helpers.Messages.pas',
  UPagination in 'UPagination.pas',
  UListNewsController in 'Controllers\UListNewsController.pas',
  Utils.Token in 'Utils.Token.pas';

{$R *.res}

begin
  if WebRequestHandler <> nil then
    WebRequestHandler.WebModuleClass := WebModuleClass;
  Application.Initialize;
  Application.CreateForm(TFrmMainTest, FrmMainTest);
  Application.CreateForm(TDmSession, DmSession);
  Application.Run;
end.
