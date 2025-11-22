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
  TDmSession = class( TDataModule )
    cnxFeedFlow: TFDConnection;
    QryListeFeeds: TFDQuery;
    QryListeFeedsID_FEED: TIntegerField;
    QryListeFeedsTITRE: TWideStringField;
    QryListeFeedsSTATUT: TWideStringField;
    QryListeFeedsDATE_CREATION: TSQLTimeStampField;
    QryListeFeedsDATE_MODIFICATION: TSQLTimeStampField;
    qryCountFeeds: TFDQuery;
    qryCountFeedsNB_ENR: TIntegerField;
    qryFeeds: TFDQuery;
    qryFeedsID_FEED: TIntegerField;
    qryFeedsTITRE: TWideStringField;
    qryFeedsSTATUT: TWideStringField;
    qryFeedsDATE_CREATION: TSQLTimeStampField;
    qryFeedsDATE_MODIFICATION: TSQLTimeStampField;
    QryListeNews: TFDQuery;
    QryCountNews: TFDQuery;
    QryNews: TFDQuery;
    QryListeNewsIDNEWS: TIntegerField;
    QryListeNewsDATE_PUBLICATION: TDateField;
    QryListeNewsDATE_PEREMPTION: TDateField;
    QryListeNewsHOLD: TWideStringField;
    QryListeNewsTITRE_NEWS: TWideStringField;
    QryListeNewsTEXTE: TWideMemoField;
    QryListeNewsID_FEED: TIntegerField;
    QryListeNewsDATE_CREATION: TDateField;
    QryListeNewsDATE_MODIFICATION: TSQLTimeStampField;
    QryNewsIDNEWS: TIntegerField;
    QryNewsDATE_PUBLICATION: TDateField;
    QryNewsDATE_PEREMPTION: TDateField;
    QryNewsHOLD: TWideStringField;
    QryNewsTITRE_NEWS: TWideStringField;
    QryNewsTEXTE: TWideMemoField;
    QryNewsID_FEED: TIntegerField;
    QryNewsDATE_CREATION: TDateField;
    QryNewsDATE_MODIFICATION: TSQLTimeStampField;
    QryNewsDATE_PUBLICATION_FMT: TWideStringField;
    QryNewsDATE_PEREMPTION_FMT: TWideStringField;
    QryShowNews: TFDQuery;
    QryShowNewsIDNEWS: TIntegerField;
    QryShowNewsDATE_PUBLICATION: TDateField;
    QryShowNewsDATE_PEREMPTION: TDateField;
    QryShowNewsHOLD: TWideStringField;
    QryShowNewsTITRE_NEWS: TWideStringField;
    QryShowNewsTEXTE: TWideMemoField;
    QryShowNewsID_FEED: TIntegerField;
    QryShowNewsDATE_CREATION: TDateField;
    QryShowNewsDATE_MODIFICATION: TSQLTimeStampField;
    QryShowNewsDATE_AFFICHAGE: TStringField;
    QryNewsORDRE_AFFICHAGE: TIntegerField;
    QryListeNewsORDRE_AFFICHAGE: TIntegerField;
    QryCountNewsNB_ENR: TIntegerField;
    qryFeedsTEMPLATE_AFFICHAGE: TWideStringField;
    QryListeFeedsTEMPLATE_AFFICHAGE: TWideStringField;
    qryFeedsCODE_PAYS: TWideStringField;
    qryFeedsCODE_LANGUE: TWideStringField;
    qryFeedsID_CATEGORIE: TIntegerField;
    qryFeedsID_SOUS_CATEGORIE: TIntegerField;
    QryListeFeedsCODE_PAYS: TWideStringField;
    QryListeFeedsCODE_LANGUE: TWideStringField;
    QryListeFeedsID_CATEGORIE: TIntegerField;
    QryListeFeedsID_SOUS_CATEGORIE: TIntegerField;
    QryListeFeedsCATEGORIE: TWideStringField;
    QryListeFeedsSOUS_CATEGORIE: TWideStringField;
    QryListeFeedsPAYS: TWideStringField;
    QryListeFeedsLANGUE: TWideStringField;
    QryListeCategorie: TFDQuery;
    QryListeSousCategorie: TFDQuery;
    QryListeCategorieID_CATEGORIE: TIntegerField;
    QryListeCategorieLIBELLE: TWideStringField;
    QryListeSousCategorieID_SOUS_CATEGORIE: TIntegerField;
    QryListeSousCategorieLIBELLE: TWideStringField;
    QryListePays: TFDQuery;
    QryListePaysCODE_PAYS: TWideStringField;
    QryListePaysLIBELLE: TWideStringField;
    QryListeLangue: TFDQuery;
    QryListeLangueCODE_LANGUE: TWideStringField;
    QryListeLangueLIBELLE: TWideStringField;
    QryListeLangueLIBELLE_TRADUIT: TWideStringField;
    qryFeedsCATEGORIE: TWideStringField;
    qryFeedsSOUS_CATEGORIE: TWideStringField;
    qryFeedsPAYS: TWideStringField;
    qryFeedsLANGUE: TWideStringField;
    qryFeedsNOM: TWideStringField;
    QryListeFeedsNOM: TWideStringField;

    procedure DataModuleDestroy( Sender: TObject );
    procedure DataModuleCreate( Sender: TObject );
    procedure QryNewsCalcFields( DataSet: TDataSet );
    procedure QryShowNewsCalcFields(DataSet: TDataSet);
  private
    FCritical: TCriticalSection;
    FSessionVariables: TStrings;
    FPaginations: TObjectDictionary<string, TPagination>;

    procedure SetSessionVariables( const Value: TStrings );
  public
    { Déclarations publiques }
    function Pagination( aPagination: string ): TPagination;

    property Critical: TCriticalSection read FCritical;
    property SessionVariables: TStrings read FSessionVariables write SetSessionVariables;
  end;

var
  DmSession: TDmSession;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

procedure TDmSession.DataModuleDestroy( Sender: TObject );
var
  LPagination: TPair<string, TPagination>;
begin
  cnxFeedFlow.Connected := False;
  FreeAndNil( FCritical );
  FreeAndNil( FSessionVariables );

  for LPagination in FPaginations do
  begin
    LPagination.Value.Free;
  end;
  FreeAndNil( FPaginations );
end;

function TDmSession.Pagination( aPagination: string ): TPagination;
begin
  if not ( FPaginations.TryGetValue( aPagination, Result ) ) then
  begin
    Result := TPagination.Create;
    FPaginations.Add( aPagination, Result );
  end;
end;

procedure TDmSession.SetSessionVariables( const Value: TStrings );
begin
  FSessionVariables := Value;
end;

procedure TDmSession.DataModuleCreate( Sender: TObject );
begin
  FCritical := TCriticalSection.Create;
  FSessionVariables := TStringList.Create;
  FPaginations := TObjectDictionary<string, TPagination>.Create;

  cnxFeedFlow.Connected := True;
end;

procedure TDmSession.QryNewsCalcFields( DataSet: TDataSet );
begin
  DataSet.FieldByName( 'DATE_PUBLICATION_FMT' ).AsString := FormatDateTime( 'YYYY-MM-DD', DataSet.FieldByName( 'DATE_PUBLICATION'
    ).AsDateTime );
  DataSet.FieldByName( 'DATE_PEREMPTION_FMT' ).AsString := FormatDateTime( 'YYYY-MM-DD', DataSet.FieldByName( 'DATE_PEREMPTION'
    ).AsDateTime );
end;

procedure TDmSession.QryShowNewsCalcFields(DataSet: TDataSet);
begin
  Dataset.FieldByName('DATE_AFFICHAGE').AsString:=FormatDateTime('dd mmmm yyyy', DataSet.FieldByName('DATE_PUBLICATION').AsDateTime);
end;

end.

