unit Utils;

interface

uses
  System.Classes,
  System.SysUtils;

function GetContentType( aFileName: string ): string;

implementation

uses
  System.IOUtils;

function GetContentType( aFileName: string ): string;
begin
  var LExt := TPath.GetExtension( aFileName ).TrimLeft(['.']);

  if ( CompareText( LExt, 'pdf' ) = 0 ) then
  begin
    Exit( 'application/pdf' );
  end;

  if ( CompareText( LExt, 'doc' ) = 0 ) then
  begin
    Exit( 'application/msword' );
  end;

  if ( CompareText( LExt, 'docx' ) = 0 ) then
  begin
    Exit( 'application/vnd.openxmlformats-officedocument.wordprocessingml.document' );
  end;

  if ( CompareText( LExt, 'xls' ) = 0 ) then
  begin
    Exit( 'application/vnd.ms-excel' );
  end;

  if ( CompareText( LExt, 'xlsx' ) = 0 ) then
  begin
    Exit( 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet' );
  end;

  if ( CompareText( LExt, 'jpg' ) = 0 ) or ( CompareText( LExt, 'jpeg' ) = 0 ) then
  begin
    Exit( 'image/jpeg' );
  end;

  if ( CompareText( LExt, 'png' ) = 0 ) then
  begin
    Exit( 'image/png' );
  end;

  if ( CompareText( LExt, 'bmp' ) = 0 ) then
  begin
    Exit( 'image/bmp' );
  end;
end;

end.

