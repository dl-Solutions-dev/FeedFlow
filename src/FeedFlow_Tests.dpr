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
  File last update : 2026-01-25T09:51:02.000+01:00
  Signature : b28edf989d7a94e0f6936599d70aa93606135801
  ***************************************************************************
*)

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
  Utils.Config in 'Infrastructure\Config\Utils.Config.pas',
  UGroups in 'Models\UGroups.pas';

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

