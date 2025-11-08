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
    qryFeedsCancel: TFDQuery;
    qryFeedsCancelID_FEED: TIntegerField;
    qryFeedsCancelTITRE: TWideStringField;
    qryFeedsCancelSTATUT: TWideStringField;
    qryFeedsCancelDATE_CREATION: TSQLTimeStampField;
    qryFeedsCancelDATE_MODIFICATION: TSQLTimeStampField;

    procedure DataModuleDestroy( Sender: TObject );
    procedure DataModuleCreate( Sender: TObject );
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
end;

end.

