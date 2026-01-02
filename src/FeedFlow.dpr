library FeedFlow;

uses
  Winapi.ActiveX,
  System.Win.ComObj,
  Web.WebBroker,
  Web.Win.ISAPIApp,
  Web.Win.ISAPIThreadPool,
  UWMMain in 'Server\UWMMain.pas' {wmMain: TWebModule},
  Utils.Logger in 'Infrastructure\Loggin\Utils.Logger.pas',
  utils.ClassHelpers in 'Infrastructure\Support\utils.ClassHelpers.pas',
  uInterfaces in 'Core\uInterfaces.pas',
  UIndexController in 'Controllers\UIndexController.pas',
  uBaseController in 'Controllers\uBaseController.pas',
  UDmSession in 'Services\UDmSession.pas' {DmSession: TDataModule},
  UConsts in 'Core\UConsts.pas',
  Helpers.Messages in 'Infrastructure\Support\Helpers.Messages.pas',
  UPagination in 'Infrastructure\Support\UPagination.pas',
  Utils.Config in 'Infrastructure\Config\Utils.Config.pas',
  UListFeedsController in 'Controllers\UListFeedsController.pas',
  UListNewsController in 'Controllers\UListNewsController.pas',
  UUserFeedsController in 'Controllers\UUserFeedsController.pas',
  Utils.Token in 'Infrastructure\Security\Utils.Token.pas',
  ULanguages in 'Models\ULanguages.pas',
  UCountries in 'Models\UCountries.pas',
  USubcategories in 'Models\USubcategories.pas',
  UCategories in 'Models\UCategories.pas',
  UFeeds in 'Models\UFeeds.pas',
  UNews in 'Models\UNews.pas',
  UControllersRegistry in 'Infrastructure\Routing\UControllersRegistry.pas';

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
