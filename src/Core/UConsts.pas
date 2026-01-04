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
  File last update : 2026-01-04T14:37:08.674+01:00
  Signature : 2fda30d08e76c3eca2e2532d5656c07c875c0c15
  ***************************************************************************
*)

/// <summary>
///   Unité contenant des constantes générales à l'application
/// </summary>
unit UConsts;

interface

const
  /// <summary>
  ///   Id de l'application
  /// </summary>
  APP_ID: string = 'FEED_FLOW';

  /// <summary>
  ///   Prefix pour les messages d'erreur (définit la couleur du toast)
  /// </summary>
  PREFIX_MSGERR: string = 'ERR:';

  /// <summary>
  ///   Prefix pour les messages d'information (définit la couleur du toast)
  ///   <br />
  /// </summary>
  PREFIX_MSG_INFO: string = 'INFO:';

//  LAYOUT_TEMPLATE: string = 'IndexTemplate.html';

{$IFDEF VPS}
  TEMPLATE_FOLDER: string = '/var/www/html/templates/';
  LOCAT: string = '/usermanagement';
{$ELSE}
{$IFDEF DEBUG}
//  FILES_FOLDER: string = 'C:\Users\danyleblanc\Documents\DL-Projets\FeedFlow\Win64\Debug\Files';
//  TEMPLATE_FOLDER: string = 'C:\Users\danyleblanc\Documents\DL-Projets\UserGuard\UserManagement\Win64\Templates\';
//  LOCAT: string = '';
{$ELSE}
//  FILES_FOLDER: string = 'C:\inetpub\wwwroot\Files';
//  TEMPLATE_FOLDER: string = '.\Templates\';
//  LOCAT: string = '/';
{$ENDIF}
{$ENDIF}

implementation

end.

