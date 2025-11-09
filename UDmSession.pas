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
    QryCountNewsNB_ENR: TIntegerField;
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

    procedure DataModuleDestroy( Sender: TObject );
    procedure DataModuleCreate( Sender: TObject );
    procedure QryNewsCalcFields( DataSet: TDataSet );
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

end.

