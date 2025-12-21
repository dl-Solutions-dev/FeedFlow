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
    qryCountFeeds: TFDQuery;
    qryCountFeedsNB_ENR: TIntegerField;
    qryFeeds: TFDQuery;
    QryListNews: TFDQuery;
    QryCountNews: TFDQuery;
    QryNews: TFDQuery;
    QryShowNews: TFDQuery;
    QryListCategories: TFDQuery;
    QryListSubCategories: TFDQuery;
    QryListCountries: TFDQuery;
    QryListLanguages: TFDQuery;
    MtUrls: TFDMemTable;
    MtUrlsURL: TStringField;
    MtUrlsImageFileName: TStringField;
    MtUrlsAlt: TStringField;
    QryShowNewsUser: TFDQuery;
    QryNewsCategories: TFDQuery;
    QryNewsSubCategory: TFDQuery;
    QryNewsCountry: TFDQuery;
    QryNewsLanguage: TFDQuery;
    QryListeGroup: TFDQuery;
    QryShowGroup: TFDQuery;
    QryFeedCategories: TFDQuery;
    QryFeedSubCategories: TFDQuery;
    QryFeedCountry: TFDQuery;
    QryFeedLanguage: TFDQuery;
    QryFeedsUser: TFDQuery;
    MtUrlsOrdre: TIntegerField;
    QryListeFeedsFEED_ID: TIntegerField;
    QryListeFeedsFEED_GROUP: TSmallintField;
    QryListeFeedsFEED_NAME: TWideStringField;
    QryListeFeedsTITLE: TWideStringField;
    QryListeFeedsSTATUS: TWideStringField;
    QryListeFeedsDISPLAY_TEMPLATE: TWideStringField;
    QryListeFeedsALL_CONTEXTS: TWideStringField;
    qryFeedsFEED_ID: TIntegerField;
    qryFeedsFEED_GROUP: TSmallintField;
    qryFeedsFEED_NAME: TWideStringField;
    qryFeedsTITLE: TWideStringField;
    qryFeedsSTATUS: TWideStringField;
    qryFeedsDISPLAY_TEMPLATE: TWideStringField;
    qryFeedsALL_CONTEXTS: TWideStringField;
    qryFeedsCREATION_DATE: TSQLTimeStampField;
    qryFeedsMODIFICATION_DATE: TSQLTimeStampField;
    QryListeFeedsCREATION_DATE: TSQLTimeStampField;
    QryListeFeedsMODIFICATION_DATE: TSQLTimeStampField;
    QryListeGroupFEED_ID: TIntegerField;
    QryListeGroupTITLE: TWideStringField;
    QryFeedCategoriesFEED_ID: TIntegerField;
    QryFeedCategoriesCATEGORY_ID: TIntegerField;
    QryFeedSubCategoriesSUBCATEGORY_ID: TIntegerField;
    QryFeedSubCategoriesFEED_ID: TIntegerField;
    QryFeedCountryCOUNTRY_CODE: TWideStringField;
    QryFeedCountryFEED_ID: TIntegerField;
    QryFeedLanguageLANGUAGE_CODE: TWideStringField;
    QryFeedLanguageFEED_ID: TIntegerField;
    QryListNewsNEWS_ID: TIntegerField;
    QryListNewsPUBLICATION_DATE: TDateField;
    QryListNewsEXPIRY_DATE: TDateField;
    QryListNewsDISPLAY_ORDER: TIntegerField;
    QryListNewsHOLD: TWideStringField;
    QryListNewsNEWS_TITLE: TWideStringField;
    QryListNewsTEXT: TWideMemoField;
    QryListNewsFEED_ID: TIntegerField;
    QryListNewsCREATION_DATE: TDateField;
    QryListNewsMODIFICATION_DATE: TSQLTimeStampField;
    QryCountNewsNB_ENR: TIntegerField;
    QryNewsCategoriesCATEGORY_ID: TIntegerField;
    QryNewsCategoriesNEWS_ID: TIntegerField;
    QryNewsSubCategorySUBCATEGORY_ID: TIntegerField;
    QryNewsSubCategoryNEWS_ID: TIntegerField;
    QryNewsCountryCOUNTRY_CODE: TWideStringField;
    QryNewsCountryNEWS_ID: TIntegerField;
    QryNewsLanguageLANGUAGE_CODE: TWideStringField;
    QryNewsLanguageNEWS_ID: TIntegerField;
    QryShowNewsUserREVERSE_ORDER: TIntegerField;
    QryShowNewsUserDISPLAY_ORDER: TIntegerField;
    QryShowNewsUserNEWS_ID: TIntegerField;
    QryShowNewsUserPUBLICATION_DATE: TDateField;
    QryShowNewsUserEXPIRY_DATE: TDateField;
    QryShowNewsUserHOLD: TWideStringField;
    QryShowNewsUserNEWS_TITLE: TWideStringField;
    QryShowNewsUserTEXT: TWideMemoField;
    QryShowNewsUserFEED_ID: TIntegerField;
    QryShowNewsUserCREATION_DATE: TDateField;
    QryShowNewsUserMODIFICATION_DATE: TSQLTimeStampField;
    QryShowNewsUserTITLE: TWideStringField;
    QryShowNewsREVERSE_ORDER: TIntegerField;
    QryShowNewsDISPLAY_ORDER: TIntegerField;
    QryShowNewsNEWS_ID: TIntegerField;
    QryShowNewsPUBLICATION_DATE: TDateField;
    QryShowNewsEXPIRY_DATE: TDateField;
    QryShowNewsHOLD: TWideStringField;
    QryShowNewsNEWS_TITLE: TWideStringField;
    QryShowNewsTEXT: TWideMemoField;
    QryShowNewsFEED_ID: TIntegerField;
    QryShowNewsCREATION_DATE: TDateField;
    QryShowNewsMODIFICATION_DATE: TSQLTimeStampField;
    QryShowNewsTITLE: TWideStringField;
    QryListCategoriesCATEGORY_ID: TIntegerField;
    QryListCategoriesCATEGORY_NAME: TWideStringField;
    QryShowGroupTEXT: TWideMemoField;
    QryFeedsUserFEED_ID: TIntegerField;
    QryFeedsUserFEED_GROUP: TSmallintField;
    QryFeedsUserFEED_NAME: TWideStringField;
    QryFeedsUserTITLE: TWideStringField;
    QryFeedsUserDISPLAY_TEMPLATE: TWideStringField;
    QryListSubCategoriesSUBCATEGORY_ID: TIntegerField;
    QryListSubCategoriesSUBCATEGORY_NAME: TWideStringField;
    QryListCountriesCOUNTRY_CODE: TWideStringField;
    QryListCountriesCOUNTRY_NAME: TWideStringField;
    QryListLanguagesLANGUAGE_CODE: TWideStringField;
    QryListLanguagesLANGUAGE_NAME: TWideStringField;
    QryListLanguagesTRANSLATED_LANGUAGE_NAME: TWideStringField;
    QryNewsNEWS_ID: TIntegerField;
    QryNewsPUBLICATION_DATE: TDateField;
    QryNewsEXPIRY_DATE: TDateField;
    QryNewsDISPLAY_ORDER: TIntegerField;
    QryNewsHOLD: TWideStringField;
    QryNewsNEWS_TITLE: TWideStringField;
    QryNewsTEXT: TWideMemoField;
    QryNewsFEED_ID: TIntegerField;
    QryNewsCREATION_DATE: TDateField;
    QryNewsMODIFICATION_DATE: TSQLTimeStampField;
    QryNewsPUBLICATION_DATE_FMT: TWideStringField;
    QryNewsEXPIRY_DATE_FMT: TWideStringField;
    QryShowNewsDISPLAY_DATE: TWideStringField;

    procedure DataModuleDestroy( Sender: TObject );
    procedure DataModuleCreate( Sender: TObject );
    procedure QryNewsCalcFields( DataSet: TDataSet );
    procedure QryShowNewsCalcFields( DataSet: TDataSet );
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
  DataSet.FieldByName( 'PUBLICATION_DATE_FMT' ).AsString := FormatDateTime( 'YYYY-MM-DD', DataSet.FieldByName( 'PUBLICATION_DATE'
    ).AsDateTime );
  DataSet.FieldByName( 'EXPIRY_DATE_FMT' ).AsString := FormatDateTime( 'YYYY-MM-DD', DataSet.FieldByName( 'EXPIRY_DATE'
    ).AsDateTime );
end;

procedure TDmSession.QryShowNewsCalcFields( DataSet: TDataSet );
begin
  Dataset.FieldByName( 'DISPLAY_DATE' ).AsString := FormatDateTime( 'dd mmmm yyyy', DataSet.FieldByName( 'PUBLICATION_DATE'
    ).AsDateTime );
end;

end.

