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
  File last update : 2026-01-04T14:37:08.689+01:00
  Signature : c074c44b8473ae0e2e8165e577521ff7326ab2af
  ***************************************************************************
*)

/// <summary>
///   Interfaces
/// </summary>
unit uInterfaces;

interface

uses
  Web.HTTPApp,
  Web.Stencils;

type
  /// <summary>
  ///   Interface à utiliser pour les controller quidoivent exposer une
  ///   méthode Initializations dans laquelle ils doivent enregistrer les
  ///   routes qu'ils exposent
  /// </summary>
  IAction = interface
    ['{EDD3F333-F82D-4618-B49D-450E02D3C16C}']
    procedure InitializeActions( aWebModule: TWebModule; aWebStencil:TWebStencilsEngine );
  end;

implementation

end.
