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
  File last update : 2026-01-04T14:37:08.841+01:00
  Signature : 9da83ce1275cd2032577ecccba449c7d892f216e
  ***************************************************************************
*)

/// <summary>
///   Modèle pou rles langues
/// </summary>
unit ULanguages;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
  FireDAC.Comp.Client,
  Firedac.Stan.Param,
  Data.DB;

type
  TLanguage = class
  private
    FCode: string;
    FLibelle: string;
  public
    constructor Create( const ACode, ALibelle: string );
    property Code: string read FCode;
    property Libelle: string read FLibelle;
  end;

  TAssociationRow = class
  private
    FLangueUtilisateur: string;
    FLibelle: string;
    procedure SetLibelle( const Value: string );
  public
    constructor Create( const ALangueUtilisateur, ALibelle: string );
    property LangueUtilisateur: string read FLangueUtilisateur;
    property Libelle: string read FLibelle write SetLibelle;
  end;

  /// <summary>
  ///   Class langues
  /// </summary>
  TLanguages = class
  strict private
    /// <summary>
    ///   Instance du FDQuery
    /// </summary>
    FQryLanguages: TFDQuery;
    FQryLanguage: TFDQuery;
    FQryAffectedLanguages: TFDQuery;
    FQryNotAffectedLanguages: TFDQuery;
    FQryDesaffecterLangue: TFDQuery;
    FQryAffecterLangue: TFDQuery;
    FQryDeleteLangue: TFDQuery;
    FQryUserLanguage: TFDQuery;
    FQryDeleteUSerLanguage: TFDQuery;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    ///   Retourne un FDQuery avec la liste des langues existantes
    /// </summary>
    function GetListOfLanguages( aConnection: TFDConnection ): TFDQuery;
    function GetLanguage( aConnection: TFDConnection; ACode: string ): TLanguage;
    function GetUserLanguage( aConnection: TFDConnection; ACode: string ): TLanguage;
    function GetLanguages( aConnection: TFDConnection ): TObjectList<TLanguage>;
    function GetUserLanguagesAffected( aConnection: TFDConnection; aCode: string ): TObjectList<TAssociationRow>;
    function GetUserLanguagesNotAffected( aConnection: TFDConnection ): TObjectList<TAssociationRow>;
    function DesaffecterLangue( aConnection: TFDConnection; aCodeLangueUtilisateur: string ): string;
    function AffecterLangue( aConnection: TFDConnection; aCodeLangueUtilisateur, aCodeLanguePortail: string ): string;

    procedure DeleteLanguage( aConnection: TFDConnection; ACode: string );
    procedure DeleteUserLanguage( aConnection: TFDConnection; ACode: string );
    procedure AddLangue( aConnection: TFDConnection; ACode, ANom: string );
    procedure AddUserLanguage( aConnection: TFDConnection; ACode, ANom: string );
  end;

implementation

  { TLanguages }

procedure TLanguages.AddLangue( aConnection: TFDConnection; ACode, ANom: string );
begin
  FQryLanguages.Connection := aConnection;
  FQryLanguages.Open;
  FQryLanguages.Append;
  FQryLanguages.FieldByName( 'LANGUAGE_CODE' ).AsString := ACode;
  FQryLanguages.FieldByName( 'LANGUAGE_NAME' ).AsString := ANom;
  FQryLanguages.Post;
  FQryLanguages.Close;
end;

procedure TLanguages.AddUserLanguage( aConnection: TFDConnection; ACode,
  ANom: string );
begin
  FQryUserLanguage.Connection := aConnection;
  FQryUserLanguage.Open;
  FQryUserLanguage.Append;
  FQryUserLanguage.FieldByName( 'USER_LANGUAGE_CODE' ).AsString := ACode;
  FQryUserLanguage.FieldByName( 'LANGUAGE_NAME' ).AsString := ANom;
  FQryUserLanguage.Post;
  FQryUserLanguage.Close;
end;

function TLanguages.AffecterLangue( aConnection: TFDConnection;
  aCodeLangueUtilisateur, aCodeLanguePortail: string ): string;
begin
  FQryAffecterLangue.Close;
  FQryAffecterLangue.Connection := aConnection;
  FQryAffecterLangue.ParamByName( 'USER_LANGUAGE_CODE' ).AsString := aCodeLangueUtilisateur;
  FQryAffecterLangue.ParamByName( 'NEWS_LANGUAGE_CODE' ).AsString := aCodeLanguePortail;
  FQryAffecterLangue.ExecSQL;
end;

constructor TLanguages.Create;
begin
  FQryLanguages := TFDQuery.Create( nil );

  FQryLanguages.Name := 'QryListeLanguages';
  FQryLanguages.SQL.Clear;
  FQryLanguages.SQL.Add( '''
    select * from LANGUAGE
    where LANGUAGE_CODE <> '*'
    order by LANGUAGE_NAME
  ''');

  FQryLanguage := TFDQuery.Create( nil );

  FQryLanguage.Name := 'QryListeLanguage';
  FQryLanguage.SQL.Clear;
  FQryLanguage.SQL.Add( '''
    select * from LANGUAGE
    where LANGUAGE_CODE = :LANGUAGE_CODE
  ''');

  FQryAffectedLanguages := TFDQuery.Create( nil );

  FQryAffectedLanguages.Name := 'QryAffectedLanguage';
  FQryAffectedLanguages.SQL.Clear;
  FQryAffectedLanguages.SQL.Add( '''
    select * from USER_LANGUAGE
    where NEWS_LANGUAGE_CODE = :LANGUAGE_CODE
  ''');

  FQryNotAffectedLanguages := TFDQuery.Create( nil );

  FQryNotAffectedLanguages.Name := 'QryNotAffectedLanguage';
  FQryNotAffectedLanguages.SQL.Clear;
  FQryNotAffectedLanguages.SQL.Add( '''
    select * from USER_LANGUAGE
    where NEWS_LANGUAGE_CODE is null
  ''');

  FQryDesaffecterLangue := TFDQuery.Create( nil );

  FQryDesaffecterLangue.Name := 'QryDesaffecterLangue';
  FQryDesaffecterLangue.SQL.Clear;
  FQryDesaffecterLangue.SQL.Add( '''
    update USER_LANGUAGE set
    NEWS_LANGUAGE_CODE = null
    where USER_LANGUAGE_CODE = :USER_LANGUAGE_CODE
  ''');

  FQryAffecterLangue := TFDQuery.Create( nil );

  FQryAffecterLangue.Name := 'QryAffecterLangue';
  FQryAffecterLangue.SQL.Clear;
  FQryAffecterLangue.SQL.Add( '''
    update USER_LANGUAGE set
    NEWS_LANGUAGE_CODE = :NEWS_LANGUAGE_CODE
    where USER_LANGUAGE_CODE = :USER_LANGUAGE_CODE
  ''');

  FQryDeleteLangue := TFDQuery.Create( nil );

  FQryDeleteLangue.Name := 'QryDeleteLanguage';
  FQryDeleteLangue.SQL.Clear;
  FQryDeleteLangue.SQL.Add( '''
    delete from LANGUAGE
    where LANGUAGE_CODE = :LANGUAGE_CODE
  ''');

  FQryUserLanguage := TFDQuery.Create( nil );

  FQryUserLanguage.Name := 'QryUserLanguage';
  FQryUserLanguage.SQL.Clear;
  FQryUserLanguage.SQL.Add( '''
    select * from USER_LANGUAGE
    where USER_LANGUAGE_CODE = :USER_LANGUAGE_CODE
  ''');

  FQryDeleteUSerLanguage := TFDQuery.Create( nil );

  FQryDeleteUSerLanguage.Name := 'QryDeleteUserLanguage';
  FQryDeleteUSerLanguage.SQL.Clear;
  FQryDeleteUSerLanguage.SQL.Add( '''
    delete from USER_LANGUAGE
    where USER_LANGUAGE_CODE = :USER_LANGUAGE_CODE
  ''');
end;

procedure TLanguages.DeleteLanguage( aConnection: TFDConnection; ACode: string );
begin
  FQryDeleteLangue.Connection := aConnection;
  FQryDeleteLangue.ParamByName( 'LANGUAGE_CODE' ).AsString := ACode;
  FQryDeleteLangue.ExecSQL;
end;

procedure TLanguages.DeleteUserLanguage( aConnection: TFDConnection; ACode:
  string );
begin
  FQryDeleteUSerLanguage.Connection := aConnection;
  FQryDeleteUSerLanguage.ParamByName( 'USER_LANGUAGE_CODE' ).AsString := ACode;
  FQryDeleteUSerLanguage.ExecSQL;
end;

function TLanguages.DesaffecterLangue( aConnection: TFDConnection;
  aCodeLangueUtilisateur: string ): string;
begin
  FQryDesaffecterLangue.Close;
  FQryDesaffecterLangue.Connection := aConnection;
  FQryDesaffecterLangue.ParamByName( 'USER_LANGUAGE_CODE' ).AsString := aCodeLangueUtilisateur;
  FQryDesaffecterLangue.ExecSQL;
end;

destructor TLanguages.Destroy;
begin
  FreeAndNil( FQryLanguages );
  FreeAndNil( FQryLanguage );
  FreeAndNil( FQryAffectedLanguages );
  FreeAndNil( FQryNotAffectedLanguages );
  FreeAndNil( FQryDesaffecterLangue );
  FreeAndNil( FQryAffecterLangue );
  FreeAndNil( FQryDeleteLangue );
  FreeAndNil( FQryUserLanguage );
  FreeAndNil( FQryDeleteUSerLanguage );

  inherited;
end;

function TLanguages.GetLanguage( aConnection: TFDConnection; ACode: string ): TLanguage;
begin
  FQryLanguage.Close;
  FQryLanguage.Connection := aConnection;
  FQryLanguage.ParamByName( 'LANGUAGE_CODE' ).AsString := ACode;
  FQryLanguage.Open;

  if not ( FQryLanguage.Eof ) then
  begin
    Result := TLanguage.Create(
      FQryLanguage.FieldByName( 'LANGUAGE_CODE' ).AsString,
      FQryLanguage.FieldByName( 'LANGUAGE_NAME' ).AsString
      );
  end
  else
  begin
    Result := nil;
  end;
end;

function TLanguages.GetLanguages( aConnection: TFDConnection ): TObjectList<TLanguage>;
begin
  Result := TObjectList<TLanguage>.Create;

  var LQry := GetListOfLanguages( aConnection );

  while not ( LQry.Eof ) do
  begin
    Result.Add(
      TLanguage.Create(
        LQry.FieldByName( 'LANGUAGE_CODE' ).AsString,
        LQry.FieldByName( 'LANGUAGE_NAME' ).AsString
        )
      );

    LQry.Next;
  end;
end;

function TLanguages.GetListOfLanguages( aConnection: TFDConnection ): TFDQuery;
begin
  FQryLanguages.Close;
  FQryLanguages.Connection := aConnection;
  FQryLanguages.Open;

  Result := FQryLanguages;
end;

function TLanguages.GetUserLanguage( aConnection: TFDConnection;
  ACode: string ): TLanguage;
begin
  FQryUserLanguage.Close;
  FQryUserLanguage.Connection := aConnection;
  FQryUserLanguage.ParamByName( 'USER_LANGUAGE_CODE' ).AsString := ACode;
  FQryUserLanguage.Open;

  if not ( FQryUserLanguage.Eof ) then
  begin
    Result := TLanguage.Create(
      FQryUserLanguage.FieldByName( 'USER_LANGUAGE_CODE' ).AsString,
      FQryUserLanguage.FieldByName( 'LANGUAGE_NAME' ).AsString
      );
  end
  else
  begin
    Result := nil;
  end;
end;

function TLanguages.GetUserLanguagesAffected( aConnection: TFDConnection;
  aCode: string ): TObjectList<TAssociationRow>;
begin
  Result := TObjectList<TAssociationRow>.Create;

  FQryAffectedLanguages.Close;
  FQryAffectedLanguages.Connection := aConnection;
  FQryAffectedLanguages.ParamByName( 'LANGUAGE_CODE' ).AsString := ACode;
  FQryAffectedLanguages.Open;

  while not ( FQryAffectedLanguages.Eof ) do
  begin
    Result.Add(
      TAssociationRow.Create(
        FQryAffectedLanguages.FieldByName( 'USER_LANGUAGE_CODE' ).AsString,
        FQryAffectedLanguages.FieldByName( 'LANGUAGE_NAME' ).AsString
        )
      );

    FQryAffectedLanguages.Next;
  end;
end;

function TLanguages.GetUserLanguagesNotAffected( aConnection: TFDConnection ): TObjectList<TAssociationRow>;
begin
  Result := TObjectList<TAssociationRow>.Create;

  FQryNotAffectedLanguages.Close;
  FQryNotAffectedLanguages.Connection := aConnection;
  FQryNotAffectedLanguages.Open;

  while not ( FQryNotAffectedLanguages.Eof ) do
  begin
    Result.Add(
      TAssociationRow.Create(
        FQryNotAffectedLanguages.FieldByName( 'USER_LANGUAGE_CODE' ).AsString,
        FQryNotAffectedLanguages.FieldByName( 'LANGUAGE_NAME' ).AsString
        )
      );

    FQryNotAffectedLanguages.Next;
  end;
end;

{ TLanguage }

constructor TLanguage.Create( const ACode, ALibelle: string );
begin
  FCode := ACode;
  FLibelle := ALibelle;
end;

{ TAssociationRow }

constructor TAssociationRow.Create( const ALangueUtilisateur, ALibelle: string );
begin
  FLangueUtilisateur := ALangueUtilisateur;
  FLibelle := ALibelle;
end;

procedure TAssociationRow.SetLibelle( const Value: string );
begin
  FLibelle := Value;
end;

end.

