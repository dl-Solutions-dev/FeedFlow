object DmSession: TDmSession
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 1920
  Width = 2560
  PixelsPerInch = 192
  object cnxFeedFlow: TFDConnection
    Params.Strings = (
      'Database=FeedFlow'
      'DriverID=FB'
      'Password=masterkey'
      'User_Name=sysdba'
      'Protocol=TCPIP'
      'Server=localhost'
      'CharacterSet=UNICODE_FSS')
    Connected = True
    LoginPrompt = False
    Left = 291
    Top = 100
  end
  object QryListeFeeds: TFDQuery
    Connection = cnxFeedFlow
    SQL.Strings = (
      
        'SELECT first :FIRST skip :SKIP f.*, c.LIBELLE as "CATEGORIE", sc' +
        '.LIBELLE as "SOUS_CATEGORIE",'
      'p.LIBELLE as "PAYS", l.LIBELLE as "LANGUE" FROM FEED_NEWS f'
      'join CATEGORIE c on (c.ID_CATEGORIE = f.ID_CATEGORIE)'
      
        'join SOUS_CATEGORIE sc on (sc.ID_SOUS_CATEGORIE = f.ID_SOUS_CATE' +
        'GORIE)'
      'join PAYS p on (p.CODE_PAYS = f.CODE_PAYS)'
      'join LANGUE l on (l.CODE_LANGUE = f.CODE_LANGUE)'
      'where upper(f.TITRE) like :TITRE'
      'order by f.DATE_CREATION desc'
      '')
    Left = 294
    Top = 245
    ParamData = <
      item
        Name = 'FIRST'
        DataType = ftLargeint
        ParamType = ptInput
        Value = 10
      end
      item
        Name = 'SKIP'
        DataType = ftLargeint
        ParamType = ptInput
        Value = 0
      end
      item
        Name = 'TITRE'
        DataType = ftWideString
        ParamType = ptInput
        Size = 500
        Value = '%%'
      end>
    object QryListeFeedsID_FEED: TIntegerField
      FieldName = 'ID_FEED'
      Origin = 'ID_FEED'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
    object QryListeFeedsTITRE: TWideStringField
      FieldName = 'TITRE'
      Origin = 'TITRE'
      Size = 500
    end
    object QryListeFeedsSTATUT: TWideStringField
      FieldName = 'STATUT'
      Origin = 'STATUT'
      FixedChar = True
      Size = 1
    end
    object QryListeFeedsDATE_CREATION: TSQLTimeStampField
      FieldName = 'DATE_CREATION'
      Origin = 'DATE_CREATION'
    end
    object QryListeFeedsDATE_MODIFICATION: TSQLTimeStampField
      FieldName = 'DATE_MODIFICATION'
      Origin = 'DATE_MODIFICATION'
    end
    object QryListeFeedsTEMPLATE_AFFICHAGE: TWideStringField
      FieldName = 'TEMPLATE_AFFICHAGE'
      Origin = 'TEMPLATE_AFFICHAGE'
      Size = 512
    end
    object QryListeFeedsCODE_PAYS: TWideStringField
      FieldName = 'CODE_PAYS'
      Origin = 'CODE_PAYS'
      Size = 3
    end
    object QryListeFeedsCODE_LANGUE: TWideStringField
      FieldName = 'CODE_LANGUE'
      Origin = 'CODE_LANGUE'
      Size = 3
    end
    object QryListeFeedsID_CATEGORIE: TIntegerField
      FieldName = 'ID_CATEGORIE'
      Origin = 'ID_CATEGORIE'
    end
    object QryListeFeedsID_SOUS_CATEGORIE: TIntegerField
      FieldName = 'ID_SOUS_CATEGORIE'
      Origin = 'ID_SOUS_CATEGORIE'
    end
    object QryListeFeedsCATEGORIE: TWideStringField
      AutoGenerateValue = arDefault
      FieldName = 'CATEGORIE'
      Origin = 'LIBELLE'
      ProviderFlags = []
      ReadOnly = True
      Size = 500
    end
    object QryListeFeedsSOUS_CATEGORIE: TWideStringField
      AutoGenerateValue = arDefault
      FieldName = 'SOUS_CATEGORIE'
      Origin = 'LIBELLE'
      ProviderFlags = []
      ReadOnly = True
      Size = 500
    end
    object QryListeFeedsPAYS: TWideStringField
      AutoGenerateValue = arDefault
      FieldName = 'PAYS'
      Origin = 'LIBELLE'
      ProviderFlags = []
      ReadOnly = True
      Size = 500
    end
    object QryListeFeedsLANGUE: TWideStringField
      AutoGenerateValue = arDefault
      FieldName = 'LANGUE'
      Origin = 'LIBELLE'
      ProviderFlags = []
      ReadOnly = True
      Size = 100
    end
    object QryListeFeedsNOM: TWideStringField
      FieldName = 'NOM'
      Origin = 'NOM'
      Size = 500
    end
  end
  object qryCountFeeds: TFDQuery
    Connection = cnxFeedFlow
    SQL.Strings = (
      'SELECT count(ID_FEED) as "NB_ENR" FROM FEED_NEWS'
      'where TITRE like :TITRE')
    Left = 514
    Top = 240
    ParamData = <
      item
        Name = 'TITRE'
        DataType = ftString
        ParamType = ptInput
        Size = 500
        Value = ''
      end>
    object qryCountFeedsNB_ENR: TIntegerField
      AutoGenerateValue = arDefault
      FieldName = 'NB_ENR'
      Origin = 'NB_ENR'
      ProviderFlags = []
      ReadOnly = True
    end
  end
  object qryFeeds: TFDQuery
    Connection = cnxFeedFlow
    FormatOptions.AssignedValues = [fvFmtDisplayDate]
    FormatOptions.FmtDisplayDate = 'yyyy-mm-dd'
    SQL.Strings = (
      
        'SELECT f.*, c.LIBELLE as "CATEGORIE", sc.LIBELLE as "SOUS_CATEGO' +
        'RIE",'
      'p.LIBELLE as "PAYS", l.LIBELLE as "LANGUE" FROM FEED_NEWS f'
      'join CATEGORIE c on (c.ID_CATEGORIE = f.ID_CATEGORIE)'
      
        'join SOUS_CATEGORIE sc on (sc.ID_SOUS_CATEGORIE = f.ID_SOUS_CATE' +
        'GORIE)'
      'join PAYS p on (p.CODE_PAYS = f.CODE_PAYS)'
      'join LANGUE l on (l.CODE_LANGUE = f.CODE_LANGUE)'
      'where ID_FEED = :ID_FEED')
    Left = 294
    Top = 396
    ParamData = <
      item
        Name = 'ID_FEED'
        DataType = ftInteger
        ParamType = ptInput
        Value = Null
      end>
    object qryFeedsID_FEED: TIntegerField
      FieldName = 'ID_FEED'
      Origin = 'ID_FEED'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
    object qryFeedsTITRE: TWideStringField
      FieldName = 'TITRE'
      Origin = 'TITRE'
      Size = 500
    end
    object qryFeedsSTATUT: TWideStringField
      FieldName = 'STATUT'
      Origin = 'STATUT'
      FixedChar = True
      Size = 1
    end
    object qryFeedsDATE_CREATION: TSQLTimeStampField
      FieldName = 'DATE_CREATION'
      Origin = 'DATE_CREATION'
    end
    object qryFeedsDATE_MODIFICATION: TSQLTimeStampField
      FieldName = 'DATE_MODIFICATION'
      Origin = 'DATE_MODIFICATION'
    end
    object qryFeedsTEMPLATE_AFFICHAGE: TWideStringField
      FieldName = 'TEMPLATE_AFFICHAGE'
      Origin = 'TEMPLATE_AFFICHAGE'
      Size = 512
    end
    object qryFeedsCODE_PAYS: TWideStringField
      FieldName = 'CODE_PAYS'
      Origin = 'CODE_PAYS'
      Size = 3
    end
    object qryFeedsCODE_LANGUE: TWideStringField
      FieldName = 'CODE_LANGUE'
      Origin = 'CODE_LANGUE'
      Size = 3
    end
    object qryFeedsID_CATEGORIE: TIntegerField
      FieldName = 'ID_CATEGORIE'
      Origin = 'ID_CATEGORIE'
    end
    object qryFeedsID_SOUS_CATEGORIE: TIntegerField
      FieldName = 'ID_SOUS_CATEGORIE'
      Origin = 'ID_SOUS_CATEGORIE'
    end
    object qryFeedsCATEGORIE: TWideStringField
      AutoGenerateValue = arDefault
      FieldName = 'CATEGORIE'
      Origin = 'LIBELLE'
      ProviderFlags = []
      ReadOnly = True
      Size = 500
    end
    object qryFeedsSOUS_CATEGORIE: TWideStringField
      AutoGenerateValue = arDefault
      FieldName = 'SOUS_CATEGORIE'
      Origin = 'LIBELLE'
      ProviderFlags = []
      ReadOnly = True
      Size = 500
    end
    object qryFeedsPAYS: TWideStringField
      AutoGenerateValue = arDefault
      FieldName = 'PAYS'
      Origin = 'LIBELLE'
      ProviderFlags = []
      ReadOnly = True
      Size = 500
    end
    object qryFeedsLANGUE: TWideStringField
      AutoGenerateValue = arDefault
      FieldName = 'LANGUE'
      Origin = 'LIBELLE'
      ProviderFlags = []
      ReadOnly = True
      Size = 100
    end
    object qryFeedsNOM: TWideStringField
      FieldName = 'NOM'
      Origin = 'NOM'
      Size = 500
    end
  end
  object QryListeNews: TFDQuery
    Connection = cnxFeedFlow
    SQL.Strings = (
      
        'SELECT first :FIRST skip :SKIP n.*, c.LIBELLE as "CATEGORIE", sc' +
        '.LIBELLE as "SOUS_CATEGORIE",'
      'p.LIBELLE as "PAYS", l.LIBELLE as "LANGUE" FROM NEWS n'
      'join CATEGORIE c on (c.ID_CATEGORIE = n.ID_CATEGORIE)'
      
        'join SOUS_CATEGORIE sc on (sc.ID_SOUS_CATEGORIE = n.ID_SOUS_CATE' +
        'GORIE)'
      'join PAYS p on (p.CODE_PAYS = n.CODE_PAYS)'
      'join LANGUE l on (l.CODE_LANGUE = n.CODE_LANGUE)'
      'where ID_FEED = :ID_FEED'
      'and (upper(TITRE_NEWS) like :TITRE_NEWS'
      'or DATE_CREATION = :DATE_CREATION'
      'or DATE_PUBLICATION = :DATE_PUBLICATION)'
      'order by DATE_CREATION desc, TITRE_NEWS')
    Left = 294
    Top = 541
    ParamData = <
      item
        Name = 'FIRST'
        DataType = ftLargeint
        ParamType = ptInput
        Value = 1
      end
      item
        Name = 'SKIP'
        DataType = ftLargeint
        ParamType = ptInput
        Value = 10
      end
      item
        Name = 'ID_FEED'
        DataType = ftInteger
        ParamType = ptInput
        Value = Null
      end
      item
        Name = 'TITRE_NEWS'
        DataType = ftString
        ParamType = ptInput
        Size = 500
        Value = Null
      end
      item
        Name = 'DATE_CREATION'
        DataType = ftDateTime
        ParamType = ptInput
        Value = Null
      end
      item
        Name = 'DATE_PUBLICATION'
        DataType = ftDateTime
        ParamType = ptInput
        Value = Null
      end>
    object QryListeNewsIDNEWS: TIntegerField
      FieldName = 'IDNEWS'
      Origin = 'IDNEWS'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
    object QryListeNewsDATE_PUBLICATION: TDateField
      FieldName = 'DATE_PUBLICATION'
      Origin = 'DATE_PUBLICATION'
    end
    object QryListeNewsDATE_PEREMPTION: TDateField
      FieldName = 'DATE_PEREMPTION'
      Origin = 'DATE_PEREMPTION'
    end
    object QryListeNewsHOLD: TWideStringField
      FieldName = 'HOLD'
      Origin = 'HOLD'
      FixedChar = True
      Size = 1
    end
    object QryListeNewsTITRE_NEWS: TWideStringField
      FieldName = 'TITRE_NEWS'
      Origin = 'TITRE_NEWS'
      Size = 500
    end
    object QryListeNewsTEXTE: TWideMemoField
      FieldName = 'TEXTE'
      Origin = 'TEXTE'
      BlobType = ftWideMemo
    end
    object QryListeNewsID_FEED: TIntegerField
      FieldName = 'ID_FEED'
      Origin = 'ID_FEED'
      Required = True
    end
    object QryListeNewsDATE_CREATION: TDateField
      FieldName = 'DATE_CREATION'
      Origin = 'DATE_CREATION'
    end
    object QryListeNewsDATE_MODIFICATION: TSQLTimeStampField
      FieldName = 'DATE_MODIFICATION'
      Origin = 'DATE_MODIFICATION'
    end
    object QryListeNewsORDRE_AFFICHAGE: TIntegerField
      FieldName = 'ORDRE_AFFICHAGE'
      Origin = 'ORDRE_AFFICHAGE'
      Required = True
    end
    object QryListeNewsCODE_PAYS: TWideStringField
      FieldName = 'CODE_PAYS'
      Origin = 'CODE_PAYS'
      Size = 3
    end
    object QryListeNewsCODE_LANGUE: TWideStringField
      FieldName = 'CODE_LANGUE'
      Origin = 'CODE_LANGUE'
      Size = 3
    end
    object QryListeNewsID_CATEGORIE: TIntegerField
      FieldName = 'ID_CATEGORIE'
      Origin = 'ID_CATEGORIE'
    end
    object QryListeNewsID_SOUS_CATEGORIE: TIntegerField
      FieldName = 'ID_SOUS_CATEGORIE'
      Origin = 'ID_SOUS_CATEGORIE'
    end
    object QryListeNewsCATEGORIE: TWideStringField
      AutoGenerateValue = arDefault
      FieldName = 'CATEGORIE'
      Origin = 'LIBELLE'
      ProviderFlags = []
      ReadOnly = True
      Size = 500
    end
    object QryListeNewsSOUS_CATEGORIE: TWideStringField
      AutoGenerateValue = arDefault
      FieldName = 'SOUS_CATEGORIE'
      Origin = 'LIBELLE'
      ProviderFlags = []
      ReadOnly = True
      Size = 500
    end
    object QryListeNewsPAYS: TWideStringField
      AutoGenerateValue = arDefault
      FieldName = 'PAYS'
      Origin = 'LIBELLE'
      ProviderFlags = []
      ReadOnly = True
      Size = 500
    end
    object QryListeNewsLANGUE: TWideStringField
      AutoGenerateValue = arDefault
      FieldName = 'LANGUE'
      Origin = 'LIBELLE'
      ProviderFlags = []
      ReadOnly = True
      Size = 100
    end
  end
  object QryCountNews: TFDQuery
    Connection = cnxFeedFlow
    SQL.Strings = (
      'SELECT count(ID_FEED) as "NB_ENR" FROM NEWS'
      'where ID_FEED = :ID_FEED'
      'and (upper(TITRE_NEWS) like :TITRE_NEWS'
      'or DATE_CREATION = :DATE_CREATION'
      'or DATE_PUBLICATION = :DATE_PUBLICATION)')
    Left = 514
    Top = 536
    ParamData = <
      item
        Name = 'ID_FEED'
        DataType = ftInteger
        ParamType = ptInput
        Value = Null
      end
      item
        Name = 'TITRE_NEWS'
        DataType = ftString
        ParamType = ptInput
        Size = 500
        Value = Null
      end
      item
        Name = 'DATE_CREATION'
        DataType = ftDateTime
        ParamType = ptInput
        Value = Null
      end
      item
        Name = 'DATE_PUBLICATION'
        DataType = ftDateTime
        ParamType = ptInput
        Value = Null
      end>
    object QryCountNewsNB_ENR: TIntegerField
      AutoGenerateValue = arDefault
      FieldName = 'NB_ENR'
      Origin = 'NB_ENR'
      ProviderFlags = []
      ReadOnly = True
    end
  end
  object QryNews: TFDQuery
    OnCalcFields = QryNewsCalcFields
    Connection = cnxFeedFlow
    SQL.Strings = (
      
        'SELECT n.*, c.LIBELLE as "CATEGORIE", sc.LIBELLE as "SOUS_CATEGO' +
        'RIE",'
      'p.LIBELLE as "PAYS", l.LIBELLE as "LANGUE"  FROM NEWS n'
      'join CATEGORIE c on (c.ID_CATEGORIE = n.ID_CATEGORIE)'
      
        'join SOUS_CATEGORIE sc on (sc.ID_SOUS_CATEGORIE = n.ID_SOUS_CATE' +
        'GORIE)'
      'join PAYS p on (p.CODE_PAYS = n.CODE_PAYS)'
      'join LANGUE l on (l.CODE_LANGUE = n.CODE_LANGUE)'
      'where IDNEWS = :IDNEWS')
    Left = 294
    Top = 692
    ParamData = <
      item
        Name = 'IDNEWS'
        DataType = ftInteger
        ParamType = ptInput
        Value = Null
      end>
    object QryNewsIDNEWS: TIntegerField
      FieldName = 'IDNEWS'
      Origin = 'IDNEWS'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
    object QryNewsDATE_PUBLICATION: TDateField
      FieldName = 'DATE_PUBLICATION'
      Origin = 'DATE_PUBLICATION'
      DisplayFormat = 'YYYY-MM-DD'
    end
    object QryNewsDATE_PEREMPTION: TDateField
      FieldName = 'DATE_PEREMPTION'
      Origin = 'DATE_PEREMPTION'
      DisplayFormat = 'YYYY-MM-DD'
    end
    object QryNewsHOLD: TWideStringField
      FieldName = 'HOLD'
      Origin = 'HOLD'
      FixedChar = True
      Size = 1
    end
    object QryNewsTITRE_NEWS: TWideStringField
      FieldName = 'TITRE_NEWS'
      Origin = 'TITRE_NEWS'
      Size = 500
    end
    object QryNewsTEXTE: TWideMemoField
      FieldName = 'TEXTE'
      Origin = 'TEXTE'
      BlobType = ftWideMemo
    end
    object QryNewsID_FEED: TIntegerField
      FieldName = 'ID_FEED'
      Origin = 'ID_FEED'
      Required = True
    end
    object QryNewsDATE_CREATION: TDateField
      FieldName = 'DATE_CREATION'
      Origin = 'DATE_CREATION'
    end
    object QryNewsDATE_MODIFICATION: TSQLTimeStampField
      FieldName = 'DATE_MODIFICATION'
      Origin = 'DATE_MODIFICATION'
    end
    object QryNewsDATE_PUBLICATION_FMT: TWideStringField
      FieldKind = fkCalculated
      FieldName = 'DATE_PUBLICATION_FMT'
      Calculated = True
    end
    object QryNewsDATE_PEREMPTION_FMT: TWideStringField
      FieldKind = fkCalculated
      FieldName = 'DATE_PEREMPTION_FMT'
      Calculated = True
    end
    object QryNewsORDRE_AFFICHAGE: TIntegerField
      FieldName = 'ORDRE_AFFICHAGE'
      Origin = 'ORDRE_AFFICHAGE'
      Required = True
    end
    object QryNewsCODE_PAYS: TWideStringField
      FieldName = 'CODE_PAYS'
      Origin = 'CODE_PAYS'
      Size = 3
    end
    object QryNewsCODE_LANGUE: TWideStringField
      FieldName = 'CODE_LANGUE'
      Origin = 'CODE_LANGUE'
      Size = 3
    end
    object QryNewsID_CATEGORIE: TIntegerField
      FieldName = 'ID_CATEGORIE'
      Origin = 'ID_CATEGORIE'
    end
    object QryNewsID_SOUS_CATEGORIE: TIntegerField
      FieldName = 'ID_SOUS_CATEGORIE'
      Origin = 'ID_SOUS_CATEGORIE'
    end
    object QryNewsCATEGORIE: TWideStringField
      AutoGenerateValue = arDefault
      FieldName = 'CATEGORIE'
      Origin = 'LIBELLE'
      ProviderFlags = []
      ReadOnly = True
      Size = 500
    end
    object QryNewsSOUS_CATEGORIE: TWideStringField
      AutoGenerateValue = arDefault
      FieldName = 'SOUS_CATEGORIE'
      Origin = 'LIBELLE'
      ProviderFlags = []
      ReadOnly = True
      Size = 500
    end
    object QryNewsPAYS: TWideStringField
      AutoGenerateValue = arDefault
      FieldName = 'PAYS'
      Origin = 'LIBELLE'
      ProviderFlags = []
      ReadOnly = True
      Size = 500
    end
    object QryNewsLANGUE: TWideStringField
      AutoGenerateValue = arDefault
      FieldName = 'LANGUE'
      Origin = 'LIBELLE'
      ProviderFlags = []
      ReadOnly = True
      Size = 100
    end
  end
  object QryShowNews: TFDQuery
    OnCalcFields = QryShowNewsCalcFields
    Connection = cnxFeedFlow
    SQL.Strings = (
      'select '
      '  (100000 - COALESCE(n.ORDRE_AFFICHAGE, 0) ) as ORDRE_INV,'
      '  n.ORDRE_AFFICHAGE,'
      '  n.IDNEWS,'
      '  n.DATE_PUBLICATION,'
      '  n.DATE_PEREMPTION,'
      '  n.HOLD,'
      '  n.TITRE_NEWS,'
      '  n.TEXTE,'
      '  n.ID_FEED,'
      '  n.DATE_CREATION,'
      '  n.DATE_MODIFICATION'
      'from NEWS n'
      'where ID_FEED = :ID_FEED'
      '  and HOLD = '#39'O'#39
      '  and DATE_PUBLICATION <= localtimestamp'
      '  and DATE_PEREMPTION > localtimestamp'
      'order by ORDRE_INV, DATE_PUBLICATION desc, TITRE_NEWS;')
    Left = 296
    Top = 824
    ParamData = <
      item
        Name = 'ID_FEED'
        DataType = ftInteger
        ParamType = ptInput
        Value = 3
      end>
    object QryShowNewsIDNEWS: TIntegerField
      FieldName = 'IDNEWS'
      Origin = 'IDNEWS'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
    object QryShowNewsDATE_PUBLICATION: TDateField
      FieldName = 'DATE_PUBLICATION'
      Origin = 'DATE_PUBLICATION'
    end
    object QryShowNewsDATE_PEREMPTION: TDateField
      FieldName = 'DATE_PEREMPTION'
      Origin = 'DATE_PEREMPTION'
    end
    object QryShowNewsHOLD: TWideStringField
      FieldName = 'HOLD'
      Origin = 'HOLD'
      FixedChar = True
      Size = 1
    end
    object QryShowNewsTITRE_NEWS: TWideStringField
      FieldName = 'TITRE_NEWS'
      Origin = 'TITRE_NEWS'
      Size = 500
    end
    object QryShowNewsTEXTE: TWideMemoField
      FieldName = 'TEXTE'
      Origin = 'TEXTE'
      BlobType = ftWideMemo
    end
    object QryShowNewsID_FEED: TIntegerField
      FieldName = 'ID_FEED'
      Origin = 'ID_FEED'
      Required = True
    end
    object QryShowNewsDATE_CREATION: TDateField
      FieldName = 'DATE_CREATION'
      Origin = 'DATE_CREATION'
    end
    object QryShowNewsDATE_MODIFICATION: TSQLTimeStampField
      FieldName = 'DATE_MODIFICATION'
      Origin = 'DATE_MODIFICATION'
    end
    object QryShowNewsDATE_AFFICHAGE: TStringField
      FieldKind = fkCalculated
      FieldName = 'DATE_AFFICHAGE'
      Size = 50
      Calculated = True
    end
  end
  object QryListeCategorie: TFDQuery
    Connection = cnxFeedFlow
    SQL.Strings = (
      'select * from CATEGORIE')
    Left = 296
    Top = 960
    object QryListeCategorieID_CATEGORIE: TIntegerField
      FieldName = 'ID_CATEGORIE'
      Origin = 'ID_CATEGORIE'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
    object QryListeCategorieLIBELLE: TWideStringField
      FieldName = 'LIBELLE'
      Origin = 'LIBELLE'
      Size = 500
    end
  end
  object QryListeSousCategorie: TFDQuery
    Connection = cnxFeedFlow
    SQL.Strings = (
      'select * from SOUS_CATEGORIE')
    Left = 296
    Top = 1088
    object QryListeSousCategorieID_SOUS_CATEGORIE: TIntegerField
      FieldName = 'ID_SOUS_CATEGORIE'
      Origin = 'ID_SOUS_CATEGORIE'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
    object QryListeSousCategorieLIBELLE: TWideStringField
      FieldName = 'LIBELLE'
      Origin = 'LIBELLE'
      Size = 500
    end
  end
  object QryListePays: TFDQuery
    Connection = cnxFeedFlow
    SQL.Strings = (
      'select * from PAYS')
    Left = 296
    Top = 1216
    object QryListePaysCODE_PAYS: TWideStringField
      FieldName = 'CODE_PAYS'
      Origin = 'CODE_PAYS'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
      Size = 3
    end
    object QryListePaysLIBELLE: TWideStringField
      FieldName = 'LIBELLE'
      Origin = 'LIBELLE'
      Size = 500
    end
  end
  object QryListeLangue: TFDQuery
    Connection = cnxFeedFlow
    SQL.Strings = (
      'select * from LANGUE')
    Left = 296
    Top = 1344
    object QryListeLangueCODE_LANGUE: TWideStringField
      FieldName = 'CODE_LANGUE'
      Origin = 'CODE_LANGUE'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
      Size = 3
    end
    object QryListeLangueLIBELLE: TWideStringField
      FieldName = 'LIBELLE'
      Origin = 'LIBELLE'
      Size = 100
    end
    object QryListeLangueLIBELLE_TRADUIT: TWideStringField
      FieldName = 'LIBELLE_TRADUIT'
      Origin = 'LIBELLE_TRADUIT'
      Size = 100
    end
  end
end
