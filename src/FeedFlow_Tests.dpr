/// <summary>
///   Application de tests pour FeedFlow
/// </summary>
program FeedFlow_Tests;
{$APPTYPE GUI}

uses
  FastMM4,
  Vcl.Forms,
  System.Rtti,
  Web.WebReq,
  IdHTTPWebBrokerBridge,
  UFrmMainTest in 'UI\Tests\UFrmMainTest.pas' {FrmMainTest},
  UWMMain in 'Server\UWMMain.pas' {wmMain: TWebModule},
  uInterfaces in 'Core\uInterfaces.pas',
  utils.ClassHelpers in 'Infrastructure\Support\utils.ClassHelpers.pas',
  Utils.Logger in 'Infrastructure\Loggin\Utils.Logger.pas',
  UIndexController in 'Controllers\UIndexController.pas',
  uBaseController in 'Controllers\uBaseController.pas',
  UDmSession in 'Services\UDmSession.pas' {DmSession: TDataModule},
  UConsts in 'Core\UConsts.pas',
  Helpers.Messages in 'Infrastructure\Support\Helpers.Messages.pas',
  UPagination in 'Infrastructure\Support\UPagination.pas',
  UListNewsController in 'Controllers\UListNewsController.pas',
  Utils.Token in 'Infrastructure\Security\Utils.Token.pas',
  UListFeedsController in 'Controllers\UListFeedsController.pas',
  UUserFeedsController in 'Controllers\UUserFeedsController.pas',
  UFeeds in 'Models\UFeeds.pas',
  UNews in 'Models\UNews.pas',
  UCategories in 'Models\UCategories.pas',
  USubcategories in 'Models\USubcategories.pas',
  UCountries in 'Models\UCountries.pas',
  ULanguages in 'Models\ULanguages.pas',
  UControllersRegistry in 'Infrastructure\Routing\UControllersRegistry.pas',
  Utils.Config in 'Infrastructure\Config\Utils.Config.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;

  if WebRequestHandler <> nil then
    WebRequestHandler.WebModuleClass := WebModuleClass;
  Application.Initialize;
  Application.CreateForm(TFrmMainTest, FrmMainTest);
//  Application.CreateForm(TDmSession, DmSession);
  Application.Run;
  TRttiContext.Create.Free;
end.

