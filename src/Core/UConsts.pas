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

