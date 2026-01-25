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
  File last update : 2026-01-04T14:37:08.234+01:00
  Signature : f8cf89c7c2f60af69555aa9d7df3df50d52fb32c
  ***************************************************************************
*)

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
  UControllersRegistry in 'Infrastructure\Routing\UControllersRegistry.pas',
  UGroups in 'Models\UGroups.pas';

{$R *.res}

exports
  GetExtensionVersion,
  HttpExtensionProc,
  TerminateExtension;

begin
  CoInitFlags := COINIT_MULTITHREADED;
  Application.Initialize;
  Application.WebModuleClass := WebModuleClass;
//  Application.CreateForm(TDmSession, DmSession);
  Application.Run;
end.
