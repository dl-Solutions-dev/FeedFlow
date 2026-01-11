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
  File last update : 2026-01-10T19:50:48.000+01:00
  Signature : df252191016af7e00b8c191e48b4897be78baed8
  ***************************************************************************
*)

/// <summary>
///   Data module pour la session
/// </summary>
unit UDmSession;

interface

uses
  System.SysUtils,
  System.Classes,
  FireDAC.Stan.Intf,
  FireDAC.Stan.Option,
  FireDAC.Stan.Error,
  FireDAC.UI.Intf,
  FireDAC.Phys.Intf,
  FireDAC.Stan.Def,
  FireDAC.Stan.Pool,
  FireDAC.Stan.Async,
  FireDAC.Phys,
  FireDAC.Phys.FB,
  FireDAC.Phys.FBDef,
  FireDAC.VCLUI.Wait,
  FireDAC.Stan.Param,
  FireDAC.DatS,
  FireDAC.DApt.Intf,
  FireDAC.DApt,
  Data.DB,
  FireDAC.Comp.DataSet,
  FireDAC.Comp.Client,
  System.SyncObjs,
  System.Generics.Collections,
  UPagination;

type
  /// <summary>
  ///   class du datamodule
  /// </summary>
  TDmSession = class( TDataModule )
    /// <summary>
    ///   Connexion à la database
    /// </summary>
    cnxFeedFlow: TFDConnection;
    /// <summary>
    ///   Table mémpoire contenant les Urls des applications
    /// </summary>
    MtUrls: TFDMemTable;
    /// <summary>
    ///   URL de l'application
    /// </summary>
    MtUrlsURL: TStringField;
    /// <summary>
    ///   Url de l'icône
    /// </summary>
    MtUrlsImageFileName: TStringField;
    /// <summary>
    ///   Texte alternatif
    /// </summary>
    MtUrlsAlt: TStringField;
    /// <summary>
    ///   Ordre d'affichage de l'application
    /// </summary>
    MtUrlsOrdre: TIntegerField;

    /// <summary>
    ///   Destructor du datamodule
    /// </summary>
    procedure DataModuleDestroy( Sender: TObject );
    /// <summary>
    ///   Creator du datamodule
    /// </summary>
    procedure DataModuleCreate( Sender: TObject );
//    procedure QryNews_CalcFields( DataSet: TDataSet );
  private
    /// <summary>
    ///   Champ section critique
    /// </summary>
    FCritical: TCriticalSection;
    /// <summary>
    ///   Champ Variables de session
    /// </summary>
    FSessionVariables: TStrings;
//    FPaginations: TObjectDictionary<string, TPagination>;

    /// <summary>
    ///   Accesseur de la propriété varaibels de session
    /// </summary>
    procedure SetSessionVariables( const Value: TStrings );
  public
    { Déclarations publiques }
//    function Pagination( aPagination: string ): TPagination;

    /// <summary>
    ///   Instance de criticalsection pour protéger les accès concurrents
    /// </summary>
    property Critical: TCriticalSection read FCritical;
    /// <summary>
    ///   Contient les variables de session
    /// </summary>
    property SessionVariables: TStrings read FSessionVariables write SetSessionVariables;
  end;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

uses
  Utils.Config;

{$R *.dfm}

procedure TDmSession.DataModuleDestroy( Sender: TObject );
//var
//  LPagination: TPair<string, TPagination>;
begin
  cnxFeedFlow.Connected := False;
  FreeAndNil( FCritical );
  FreeAndNil( FSessionVariables );

//  for LPagination in FPaginations do
//  begin
//    LPagination.Value.Free;
//  end;
//  FreeAndNil( FPaginations );
end;

//function TDmSession.Pagination( aPagination: string ): TPagination;
//begin
//  if not ( FPaginations.TryGetValue( aPagination, Result ) ) then
//  begin
//    Result := TPagination.Create;
//    FPaginations.Add( aPagination, Result );
//  end;
//end;

procedure TDmSession.SetSessionVariables( const Value: TStrings );
begin
  FSessionVariables := Value;
end;

procedure TDmSession.DataModuleCreate( Sender: TObject );
begin
  FCritical := TCriticalSection.Create;
  FSessionVariables := TStringList.Create;
//  FPaginations := TObjectDictionary<string, TPagination>.Create;

  cnxFeedFlow.Params.Database := TConfig.GetInstance.DatabaseName;
  cnxFeedFlow.Connected := True;
end;

//procedure TDmSession.QryNews_CalcFields( DataSet: TDataSet );
//begin
//  DataSet.FieldByName( 'PUBLICATION_DATE_FMT' ).AsString := FormatDateTime( 'YYYY-MM-DD', DataSet.FieldByName( 'PUBLICATION_DATE'
//    ).AsDateTime );
//  DataSet.FieldByName( 'EXPIRY_DATE_FMT' ).AsString := FormatDateTime( 'YYYY-MM-DD', DataSet.FieldByName( 'EXPIRY_DATE'
//    ).AsDateTime );
//end;

end.

