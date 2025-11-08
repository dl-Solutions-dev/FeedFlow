unit UConsts;

interface

const
  APP_ID: string = 'USER_GUARD';

  PREFIX_MSGERR: string = 'ERR:';
  PREFIX_MSG_INFO: string = 'INFO:';

  LAYOUT_TEMPLATE:string = 'IndexTemplate.html';

{$IFDEF VPS}
  TEMPLATE_FOLDER: string = '/var/www/html/templates/';
  LOCAT: string = '/usermanagement';
{$ELSE}
  TEMPLATE_FOLDER: string = 'C:\Users\danyleblanc\Documents\DL-Projets\UserGuard\UserManagement\Win64\Templates\';
  LOCAT: string = '';
{$ENDIF}

implementation

end.
