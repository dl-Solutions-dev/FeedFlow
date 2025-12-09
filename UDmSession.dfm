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
    LoginPrompt = False
    Left = 291
    Top = 100
  end
  object QryListeFeeds: TFDQuery
    Connection = cnxFeedFlow
    SQL.Strings = (
      'SELECT first :FIRST skip :SKIP f.* FROM FEED_NEWS f'
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
    object QryListeFeedsNOM: TWideStringField
      FieldName = 'NOM'
      Origin = 'NOM'
      Size = 500
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
    object QryListeFeedsTEMPLATE_AFFICHAGE: TWideStringField
      FieldName = 'TEMPLATE_AFFICHAGE'
      Origin = 'TEMPLATE_AFFICHAGE'
      Size = 512
    end
    object QryListeFeedsDATE_CREATION: TSQLTimeStampField
      FieldName = 'DATE_CREATION'
      Origin = 'DATE_CREATION'
    end
    object QryListeFeedsDATE_MODIFICATION: TSQLTimeStampField
      FieldName = 'DATE_MODIFICATION'
      Origin = 'DATE_MODIFICATION'
    end
    object QryListeFeedsGROUPE: TSmallintField
      FieldName = 'GROUPE'
      Origin = 'GROUPE'
      Required = True
    end
    object QryListeFeedsALL_CONTEXTS: TWideStringField
      FieldName = 'ALL_CONTEXTS'
      Origin = 'ALL_CONTEXTS'
      FixedChar = True
      Size = 1
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
      'SELECT f.* FROM FEED_NEWS f'
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
    object qryFeedsNOM: TWideStringField
      FieldName = 'NOM'
      Origin = 'NOM'
      Size = 500
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
    object qryFeedsTEMPLATE_AFFICHAGE: TWideStringField
      FieldName = 'TEMPLATE_AFFICHAGE'
      Origin = 'TEMPLATE_AFFICHAGE'
      Size = 512
    end
    object qryFeedsDATE_CREATION: TSQLTimeStampField
      FieldName = 'DATE_CREATION'
      Origin = 'DATE_CREATION'
    end
    object qryFeedsDATE_MODIFICATION: TSQLTimeStampField
      FieldName = 'DATE_MODIFICATION'
      Origin = 'DATE_MODIFICATION'
    end
    object qryFeedsGROUPE: TSmallintField
      FieldName = 'GROUPE'
      Origin = 'GROUPE'
      Required = True
    end
    object qryFeedsALL_CONTEXTS: TWideStringField
      FieldName = 'ALL_CONTEXTS'
      Origin = 'ALL_CONTEXTS'
      FixedChar = True
      Size = 1
    end
  end
  object QryListeNews: TFDQuery
    Connection = cnxFeedFlow
    SQL.Strings = (
      'SELECT first :FIRST skip :SKIP n.* FROM NEWS n'
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
      'SELECT n.*  FROM NEWS n'
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
  end
  object QryShowNews: TFDQuery
    OnCalcFields = QryShowNewsCalcFields
    Connection = cnxFeedFlow
    SQL.Strings = (
      'select '
      '  case n.ORDRE_AFFICHAGE'
      '    when null then 100000'
      '    when 0 then 10000'
      '    else n.ORDRE_AFFICHAGE'
      '  end as ORDRE_INV,'
      '  n.ORDRE_AFFICHAGE,'
      '  n.IDNEWS,'
      '  n.DATE_PUBLICATION,'
      '  n.DATE_PEREMPTION,'
      '  n.HOLD,'
      '  n.TITRE_NEWS,'
      '  n.TEXTE,'
      '  n.ID_FEED,'
      '  n.DATE_CREATION,'
      '  n.DATE_MODIFICATION,'
      '  f.TITRE'
      'from NEWS n'
      'join FEED_NEWS f on (f.ID_FEED = n.ID_FEED)'
      'where n.ID_FEED = :ID_FEED'
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
        Value = 20
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
      'select * from CATEGORIE'
      'order by LIBELLE')
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
      'select * from SOUS_CATEGORIE'
      'order by LIBELLE')
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
      'select * from PAYS'
      'order by LIBELLE')
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
      'select * from LANGUE'
      'order by LIBELLE')
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
  object MtUrls: TFDMemTable
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    Left = 296
    Top = 1480
    object MtUrlsURL: TStringField
      FieldName = 'URL'
      Size = 500
    end
    object MtUrlsImageFileName: TStringField
      FieldName = 'ImageFileName'
      Size = 500
    end
    object MtUrlsAlt: TStringField
      FieldName = 'Alt'
      Size = 500
    end
  end
  object QryShowNewsUser: TFDQuery
    OnCalcFields = QryShowNewsCalcFields
    Connection = cnxFeedFlow
    SQL.Strings = (
      'select '
      '  case n.ORDRE_AFFICHAGE'
      '    when null then 100000'
      '    when 0 then 10000'
      '    else n.ORDRE_AFFICHAGE'
      '  end as ORDRE_INV,'
      '  n.ORDRE_AFFICHAGE,'
      '  n.IDNEWS,'
      '  n.DATE_PUBLICATION,'
      '  n.DATE_PEREMPTION,'
      '  n.HOLD,'
      '  n.TITRE_NEWS,'
      '  n.TEXTE,'
      '  n.ID_FEED,'
      '  n.DATE_CREATION,'
      '  n.DATE_MODIFICATION,'
      '  f.TITRE'
      'from NEWS n'
      'join FEED_NEWS f on (f.ID_FEED = n.ID_FEED)'
      'join NEWS_CONTEXT_COUNTRY p on (p.ID_NEWS = n.IDNEWS)'
      'join NEWS_CONTEXT_LANG l on (l.ID_NEWS = n.IDNEWS)'
      'join NEWS_CONTEXT_CATEGORY c on (c.ID_NEWS = n.IDNEWS)'
      'join NEWS_CONTEXT_SUB_CATEGORY s on (s.ID_NEWS = n.IDNEWS)'
      'where n.ID_FEED = :ID_FEED'
      '  and p.CODE_PAYS = :CODE_PAYS'
      '  and l.CODE_LANGUE = :CODE_LANGUE'
      '  and c.ID_CATEGORIE = :ID_CATEGORIE'
      '  and s.ID_SOUS_CATEGORIE = :ID_SOUS_CATEGORIE'
      '  and HOLD = '#39'O'#39
      '  and DATE_PUBLICATION <= localtimestamp'
      '  and DATE_PEREMPTION > localtimestamp'
      'order by ORDRE_INV, DATE_PUBLICATION desc, TITRE_NEWS;')
    Left = 504
    Top = 824
    ParamData = <
      item
        Name = 'ID_FEED'
        DataType = ftInteger
        ParamType = ptInput
        Value = 10
      end
      item
        Name = 'CODE_PAYS'
        DataType = ftString
        ParamType = ptInput
        Size = 3
        Value = 'FR'
      end
      item
        Name = 'CODE_LANGUE'
        DataType = ftString
        ParamType = ptInput
        Size = 3
        Value = 'fr'
      end
      item
        Name = 'ID_CATEGORIE'
        DataType = ftInteger
        ParamType = ptInput
        Value = 1
      end
      item
        Name = 'ID_SOUS_CATEGORIE'
        DataType = ftInteger
        ParamType = ptInput
        Value = 1
      end>
    object QryShowNewsUserIDNEWS: TIntegerField
      FieldName = 'IDNEWS'
      Origin = 'IDNEWS'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
    object QryShowNewsUserDATE_PUBLICATION: TDateField
      FieldName = 'DATE_PUBLICATION'
      Origin = 'DATE_PUBLICATION'
    end
    object QryShowNewsUserDATE_PEREMPTION: TDateField
      FieldName = 'DATE_PEREMPTION'
      Origin = 'DATE_PEREMPTION'
    end
    object QryShowNewsUserHOLD: TWideStringField
      FieldName = 'HOLD'
      Origin = 'HOLD'
      FixedChar = True
      Size = 1
    end
    object QryShowNewsUserTITRE_NEWS: TWideStringField
      FieldName = 'TITRE_NEWS'
      Origin = 'TITRE_NEWS'
      Size = 500
    end
    object QryShowNewsUserTEXTE: TWideMemoField
      FieldName = 'TEXTE'
      Origin = 'TEXTE'
      BlobType = ftWideMemo
    end
    object QryShowNewsUserID_FEED: TIntegerField
      FieldName = 'ID_FEED'
      Origin = 'ID_FEED'
      Required = True
    end
    object QryShowNewsUserDATE_CREATION: TDateField
      FieldName = 'DATE_CREATION'
      Origin = 'DATE_CREATION'
    end
    object QryShowNewsUserDATE_MODIFICATION: TSQLTimeStampField
      FieldName = 'DATE_MODIFICATION'
      Origin = 'DATE_MODIFICATION'
    end
    object QryShowNewsUserDATE_AFFICHAGE: TStringField
      FieldKind = fkCalculated
      FieldName = 'DATE_AFFICHAGE'
      Size = 50
      Calculated = True
    end
  end
  object QryNewsCategories: TFDQuery
    Connection = cnxFeedFlow
    SQL.Strings = (
      'select * from NEWS_CONTEXT_CATEGORY'
      'where ID_NEWS = :ID_NEWS')
    Left = 512
    Top = 688
    ParamData = <
      item
        Name = 'ID_NEWS'
        DataType = ftInteger
        ParamType = ptInput
        Value = 10
      end>
    object QryNewsCategoriesID_CATEGORIE: TIntegerField
      FieldName = 'ID_CATEGORIE'
      Origin = 'ID_CATEGORIE'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
    object QryNewsCategoriesID_NEWS: TIntegerField
      FieldName = 'ID_NEWS'
      Origin = 'ID_NEWS'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
  end
  object QryNewsSousCategories: TFDQuery
    Connection = cnxFeedFlow
    SQL.Strings = (
      'select * from NEWS_CONTEXT_SUB_CATEGORY'
      'where ID_NEWS = :ID_NEWS')
    Left = 712
    Top = 688
    ParamData = <
      item
        Name = 'ID_NEWS'
        DataType = ftInteger
        ParamType = ptInput
        Value = 10
      end>
    object QryNewsSousCategoriesID_SOUS_CATEGORIE: TIntegerField
      FieldName = 'ID_SOUS_CATEGORIE'
      Origin = 'ID_SOUS_CATEGORIE'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
    object QryNewsSousCategoriesID_NEWS: TIntegerField
      FieldName = 'ID_NEWS'
      Origin = 'ID_NEWS'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
  end
  object QryNewsPays: TFDQuery
    Connection = cnxFeedFlow
    SQL.Strings = (
      'select * from NEWS_CONTEXT_COUNTRY'
      'where ID_NEWS = :ID_NEWS')
    Left = 840
    Top = 688
    ParamData = <
      item
        Name = 'ID_NEWS'
        DataType = ftInteger
        ParamType = ptInput
        Value = 10
      end>
    object QryNewsPaysCODE_PAYS: TWideStringField
      FieldName = 'CODE_PAYS'
      Origin = 'CODE_PAYS'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
      Size = 3
    end
    object QryNewsPaysID_NEWS: TIntegerField
      FieldName = 'ID_NEWS'
      Origin = 'ID_NEWS'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
  end
  object QryNewsLangue: TFDQuery
    Connection = cnxFeedFlow
    SQL.Strings = (
      'select * from NEWS_CONTEXT_LANG'
      'where ID_NEWS = :ID_NEWS')
    Left = 968
    Top = 688
    ParamData = <
      item
        Name = 'ID_NEWS'
        DataType = ftInteger
        ParamType = ptInput
        Value = 10
      end>
    object QryNewsLangueCODE_LANGUE: TWideStringField
      FieldName = 'CODE_LANGUE'
      Origin = 'CODE_LANGUE'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
      Size = 3
    end
    object QryNewsLangueID_NEWS: TIntegerField
      FieldName = 'ID_NEWS'
      Origin = 'ID_NEWS'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
  end
  object QryListeGroup: TFDQuery
    Connection = cnxFeedFlow
    SQL.Strings = (
      'select ID_FEED, TITRE from FEED_NEWS'
      'where GROUPE = :GROUPE'
      'order by TITRE')
    Left = 512
    Top = 392
    ParamData = <
      item
        Name = 'GROUPE'
        DataType = ftInteger
        ParamType = ptInput
        Value = 0
      end>
    object QryListeGroupID_FEED: TIntegerField
      FieldName = 'ID_FEED'
      Origin = 'ID_FEED'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
    object QryListeGroupTITRE: TWideStringField
      FieldName = 'TITRE'
      Origin = 'TITRE'
      Size = 500
    end
  end
  object QryShowGroup: TFDQuery
    Connection = cnxFeedFlow
    SQL.Strings = (
      'select first 1 TEXTE from NEWS'
      'where ID_FEED = :ID_FEED')
    Left = 504
    Top = 960
    ParamData = <
      item
        Name = 'ID_FEED'
        DataType = ftInteger
        ParamType = ptInput
        Value = Null
      end>
    object QryShowGroupTEXTE: TWideMemoField
      FieldName = 'TEXTE'
      Origin = 'TEXTE'
      BlobType = ftWideMemo
    end
  end
  object QryFeedCategories: TFDQuery
    Connection = cnxFeedFlow
    SQL.Strings = (
      'select * from FEED_CONTEXT_CATEGORY'
      'where ID_FEED = :ID_FEED')
    Left = 688
    Top = 392
    ParamData = <
      item
        Name = 'ID_FEED'
        DataType = ftInteger
        ParamType = ptInput
        Value = Null
      end>
    object QryFeedCategoriesID_CATEGORIE: TIntegerField
      FieldName = 'ID_CATEGORIE'
      Origin = 'ID_CATEGORIE'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
    object QryFeedCategoriesID_FEED: TIntegerField
      FieldName = 'ID_FEED'
      Origin = 'ID_FEED'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
  end
  object QryFeedSousCategories: TFDQuery
    Connection = cnxFeedFlow
    SQL.Strings = (
      'select * from FEED_CONTEXT_SUB_CATEGORY'
      'where ID_FEED = :ID_FEED')
    Left = 888
    Top = 392
    ParamData = <
      item
        Name = 'ID_FEED'
        DataType = ftInteger
        ParamType = ptInput
        Value = Null
      end>
    object QryFeedSousCategoriesID_SOUS_CATEGORIE: TIntegerField
      FieldName = 'ID_SOUS_CATEGORIE'
      Origin = 'ID_SOUS_CATEGORIE'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
    object QryFeedSousCategoriesID_FEED: TIntegerField
      FieldName = 'ID_FEED'
      Origin = 'ID_FEED'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
  end
  object QryFeedPays: TFDQuery
    Connection = cnxFeedFlow
    SQL.Strings = (
      'select * from FEED_CONTEXT_COUNTRY'
      'where ID_FEED = :ID_FEED')
    Left = 1016
    Top = 392
    ParamData = <
      item
        Name = 'ID_FEED'
        DataType = ftInteger
        ParamType = ptInput
        Value = Null
      end>
    object QryFeedPaysCODE_PAYS: TWideStringField
      FieldName = 'CODE_PAYS'
      Origin = 'CODE_PAYS'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
      Size = 3
    end
    object QryFeedPaysID_FEED: TIntegerField
      FieldName = 'ID_FEED'
      Origin = 'ID_FEED'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
  end
  object QryFeedLangue: TFDQuery
    Connection = cnxFeedFlow
    SQL.Strings = (
      'select * from FEED_CONTEXT_LANG'
      'where ID_FEED = :ID_FEED')
    Left = 1144
    Top = 392
    ParamData = <
      item
        Name = 'ID_FEED'
        DataType = ftInteger
        ParamType = ptInput
        Value = Null
      end>
    object QryFeedLangueCODE_LANGUE: TWideStringField
      FieldName = 'CODE_LANGUE'
      Origin = 'CODE_LANGUE'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
      Size = 3
    end
    object QryFeedLangueID_FEED: TIntegerField
      FieldName = 'ID_FEED'
      Origin = 'ID_FEED'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
  end
  object QryFeedsUser: TFDQuery
    Connection = cnxFeedFlow
    SQL.Strings = (
      
        'SELECT distinct r.ID_FEED, r.GROUPE, r.NOM, r.TITRE, r.TEMPLATE_' +
        'AFFICHAGE'
      'FROM FEED_NEWS r'
      'join NEWS n on (n.ID_FEED = r.ID_FEED)'
      'join NEWS_CONTEXT_CATEGORY cc on (cc.ID_NEWS = n.IDNEWS)'
      'join NEWS_CONTEXT_SUB_CATEGORY sc on (sc.ID_NEWS = n.IDNEWS)'
      'join NEWS_CONTEXT_COUNTRY cp on (cp.ID_NEWS = n.IDNEWS)'
      'join NEWS_CONTEXT_LANG cl on (cl.ID_NEWS = n.IDNEWS)'
      'where r.STATUT = '#39'O'#39
      'and r.GROUPE = :ID_GROUPE'
      'and cc.ID_CATEGORIE = :ID_CATEGORIE'
      'and sc.ID_SOUS_CATEGORIE = :ID_SOUS_CATEGORIE'
      'and cp.CODE_PAYS = :CODE_PAYS'
      'and cl.CODE_LANGUE = :CODE_LANGUE')
    Left = 712
    Top = 960
    ParamData = <
      item
        Name = 'ID_GROUPE'
        DataType = ftInteger
        ParamType = ptInput
        Value = 2
      end
      item
        Name = 'ID_CATEGORIE'
        DataType = ftInteger
        ParamType = ptInput
        Value = 1
      end
      item
        Name = 'ID_SOUS_CATEGORIE'
        DataType = ftInteger
        ParamType = ptInput
        Value = 1
      end
      item
        Name = 'CODE_PAYS'
        DataType = ftString
        ParamType = ptInput
        Value = 'FR'
      end
      item
        Name = 'CODE_LANGUE'
        DataType = ftString
        ParamType = ptInput
        Value = 'fr'
      end>
    object QryFeedsUserID_FEED: TIntegerField
      FieldName = 'ID_FEED'
      Origin = 'ID_FEED'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
    object QryFeedsUserGROUPE: TSmallintField
      FieldName = 'GROUPE'
      Origin = 'GROUPE'
      Required = True
    end
    object QryFeedsUserNOM: TWideStringField
      FieldName = 'NOM'
      Origin = 'NOM'
      Size = 500
    end
    object QryFeedsUserTITRE: TWideStringField
      FieldName = 'TITRE'
      Origin = 'TITRE'
      Size = 500
    end
    object QryFeedsUserTEMPLATE_AFFICHAGE: TWideStringField
      FieldName = 'TEMPLATE_AFFICHAGE'
      Origin = 'TEMPLATE_AFFICHAGE'
      Size = 512
    end
  end
end
