library FeedFlow;

uses
  Winapi.ActiveX,
  System.Win.ComObj,
  Web.WebBroker,
  Web.Win.ISAPIApp,
  Web.Win.ISAPIThreadPool,
  UWMMain in 'UWMMain.pas' {wmMain: TWebModule},
  Utils.Logger in 'Utils.Logger.pas',
  utils.ClassHelpers in 'utils.ClassHelpers.pas',
  uInvokerActions in 'uInvokerActions.pas',
  uInterfaces in 'uInterfaces.pas',
  UIndexController in 'Controllers\UIndexController.pas',
  uBaseController in 'Controllers\uBaseController.pas',
  UDmSession in 'UDmSession.pas' {DmSession: TDataModule},
  UConsts in 'UConsts.pas',
  Helpers.Messages in 'Helpers.Messages.pas',
  UPagination in 'UPagination.pas',
  Utils.Config in 'Utils.Config.pas',
  UListFeedsController in 'Controllers\UListFeedsController.pas',
  UListNewsController in 'Controllers\UListNewsController.pas',
  UUserFeedsController in 'Controllers\UUserFeedsController.pas',
  Utils.Token in 'Utils.Token.pas';

{$R *.res}

exports
  GetExtensionVersion,
  HttpExtensionProc,
  TerminateExtension;

begin
  CoInitFlags := COINIT_MULTITHREADED;
  Application.Initialize;
  Application.WebModuleClass := WebModuleClass;
  Application.CreateForm(TDmSession, DmSession);
  Application.Run;
end.
