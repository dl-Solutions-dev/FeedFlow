unit UConsts;

interface

const
  APP_ID: string = 'USER_GUARD';

  PREFIX_MSGERR: string = 'ERR:';
  PREFIX_MSG_INFO: string = 'INFO:';

  LAYOUT_TEMPLATE: string = 'IndexTemplate.html';

{$IFDEF VPS}
  TEMPLATE_FOLDER: string = '/var/www/html/templates/';
  LOCAT: string = '/usermanagement';
{$ELSE}
{$IFDEF DEBUG}
  TEMPLATE_FOLDER: string = 'C:\Users\danyleblanc\Documents\DL-Projets\UserGuard\UserManagement\Win64\Templates\';
  LOCAT: string = '';
{$ELSE}
  TEMPLATE_FOLDER: string = '.\Templates\';
  LOCAT: string = '/';
{$ENDIF}
{$ENDIF}

  // SQL
  QRY_LISTE_FEEDS: string = '''
    SELECT first :FIRST skip :SKIP f.* FROM FEED_NEWS f
    where upper(f.TITRE) like :TITRE
  ''';

  QRY_LIST_NEWS: string = '''
    SELECT first :FIRST skip :SKIP n.* FROM NEWS n
    where ID_FEED = :ID_FEED
    and (upper(TITRE_NEWS) like :TITRE_NEWS
    or DATE_CREATION = :DATE_CREATION
    or DATE_PUBLICATION = :DATE_PUBLICATION)
  ''';

implementation

end.

