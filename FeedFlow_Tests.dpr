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
  UFrmMainTest in 'UFrmMainTest.pas' {FrmMainTest},
  UWMMain in 'UWMMain.pas' {wmMain: TWebModule},
  uInterfaces in 'uInterfaces.pas',
  utils.ClassHelpers in 'utils.ClassHelpers.pas',
  Utils.Logger in 'Utils.Logger.pas',
  UIndexController in 'Controllers\UIndexController.pas',
  uBaseController in 'Controllers\uBaseController.pas',
  UDmSession in 'UDmSession.pas' {DmSession: TDataModule},
  UConsts in 'UConsts.pas',
  Helpers.Messages in 'Helpers.Messages.pas',
  UPagination in 'UPagination.pas',
  UListNewsController in 'Controllers\UListNewsController.pas',
  Utils.Token in 'Utils.Token.pas',
  UListFeedsController in 'Controllers\UListFeedsController.pas',
  UUserFeedsController in 'Controllers\UUserFeedsController.pas',
  UFeeds in 'Models\UFeeds.pas',
  UNews in 'Models\UNews.pas',
  UCategories in 'Models\UCategories.pas',
  USubcategories in 'Models\USubcategories.pas',
  UCountries in 'Models\UCountries.pas',
  ULanguages in 'Models\ULanguages.pas',
  UControllersList in 'UControllersList.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;

  if WebRequestHandler <> nil then
    WebRequestHandler.WebModuleClass := WebModuleClass;
  Application.Initialize;
  Application.CreateForm(TFrmMainTest, FrmMainTest);
  Application.CreateForm(TDmSession, DmSession);
  Application.Run;
  TRttiContext.Create.Free;
end.

