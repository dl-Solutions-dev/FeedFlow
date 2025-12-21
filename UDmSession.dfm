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
      'SELECT first :FIRST skip :SKIP f.* FROM FEED_NEWS f'
      'where upper(f.TITLE) like :TITLE'
      'order by f.CREATION_DATE desc'
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
        Name = 'TITLE'
        DataType = ftWideString
        ParamType = ptInput
        Size = 500
      end>
    object QryListeFeedsFEED_ID: TIntegerField
      FieldName = 'FEED_ID'
      Origin = 'FEED_ID'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
    object QryListeFeedsFEED_GROUP: TSmallintField
      FieldName = 'FEED_GROUP'
      Origin = 'FEED_GROUP'
      Required = True
    end
    object QryListeFeedsFEED_NAME: TWideStringField
      FieldName = 'FEED_NAME'
      Origin = 'FEED_NAME'
      Size = 500
    end
    object QryListeFeedsTITLE: TWideStringField
      FieldName = 'TITLE'
      Origin = 'TITLE'
      Size = 500
    end
    object QryListeFeedsSTATUS: TWideStringField
      FieldName = 'STATUS'
      Origin = 'STATUS'
      FixedChar = True
      Size = 1
    end
    object QryListeFeedsDISPLAY_TEMPLATE: TWideStringField
      FieldName = 'DISPLAY_TEMPLATE'
      Origin = 'DISPLAY_TEMPLATE'
      Size = 512
    end
    object QryListeFeedsALL_CONTEXTS: TWideStringField
      FieldName = 'ALL_CONTEXTS'
      Origin = 'ALL_CONTEXTS'
      FixedChar = True
      Size = 1
    end
    object QryListeFeedsCREATION_DATE: TSQLTimeStampField
      FieldName = 'CREATION_DATE'
      Origin = 'CREATION_DATE'
    end
    object QryListeFeedsMODIFICATION_DATE: TSQLTimeStampField
      FieldName = 'MODIFICATION_DATE'
      Origin = 'MODIFICATION_DATE'
    end
  end
  object qryCountFeeds: TFDQuery
    Connection = cnxFeedFlow
    SQL.Strings = (
      'SELECT count(FEED_ID) as "NB_ENR" FROM FEED_NEWS'
      'where TITLE like :TITLE')
    Left = 514
    Top = 240
    ParamData = <
      item
        Name = 'TITLE'
        DataType = ftString
        ParamType = ptInput
        Value = Null
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
      'where FEED_ID = :FEED_ID')
    Left = 294
    Top = 396
    ParamData = <
      item
        Name = 'FEED_ID'
        DataType = ftInteger
        ParamType = ptInput
        Value = Null
      end>
    object qryFeedsFEED_ID: TIntegerField
      FieldName = 'FEED_ID'
      Origin = 'FEED_ID'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
    object qryFeedsFEED_GROUP: TSmallintField
      FieldName = 'FEED_GROUP'
      Origin = 'FEED_GROUP'
      Required = True
    end
    object qryFeedsFEED_NAME: TWideStringField
      FieldName = 'FEED_NAME'
      Origin = 'FEED_NAME'
      Size = 500
    end
    object qryFeedsTITLE: TWideStringField
      FieldName = 'TITLE'
      Origin = 'TITLE'
      Size = 500
    end
    object qryFeedsSTATUS: TWideStringField
      FieldName = 'STATUS'
      Origin = 'STATUS'
      FixedChar = True
      Size = 1
    end
    object qryFeedsDISPLAY_TEMPLATE: TWideStringField
      FieldName = 'DISPLAY_TEMPLATE'
      Origin = 'DISPLAY_TEMPLATE'
      Size = 512
    end
    object qryFeedsALL_CONTEXTS: TWideStringField
      FieldName = 'ALL_CONTEXTS'
      Origin = 'ALL_CONTEXTS'
      FixedChar = True
      Size = 1
    end
    object qryFeedsCREATION_DATE: TSQLTimeStampField
      FieldName = 'CREATION_DATE'
      Origin = 'CREATION_DATE'
    end
    object qryFeedsMODIFICATION_DATE: TSQLTimeStampField
      FieldName = 'MODIFICATION_DATE'
      Origin = 'MODIFICATION_DATE'
    end
  end
  object QryListNews: TFDQuery
    Connection = cnxFeedFlow
    SQL.Strings = (
      'SELECT first :FIRST skip :SKIP n.* FROM NEWS n'
      'where FEED_ID = :FEED_ID'
      'and (upper(NEWS_TITLE) like :NEWS_TITLE'
      'or CREATION_DATE = :CREATION_DATE'
      'or PUBLICATION_DATE = :PUBLICATION_DATE)'
      'order by CREATION_DATE desc, NEWS_TITLE')
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
        Name = 'FEED_ID'
        DataType = ftInteger
        ParamType = ptInput
      end
      item
        Name = 'NEWS_TITLE'
        DataType = ftWideString
        ParamType = ptInput
        Size = 500
      end
      item
        Name = 'CREATION_DATE'
        DataType = ftDate
        ParamType = ptInput
      end
      item
        Name = 'PUBLICATION_DATE'
        DataType = ftDate
        ParamType = ptInput
      end>
    object QryListNewsNEWS_ID: TIntegerField
      FieldName = 'NEWS_ID'
      Origin = 'NEWS_ID'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
    object QryListNewsPUBLICATION_DATE: TDateField
      FieldName = 'PUBLICATION_DATE'
      Origin = 'PUBLICATION_DATE'
    end
    object QryListNewsEXPIRY_DATE: TDateField
      FieldName = 'EXPIRY_DATE'
      Origin = 'EXPIRY_DATE'
    end
    object QryListNewsDISPLAY_ORDER: TIntegerField
      FieldName = 'DISPLAY_ORDER'
      Origin = 'DISPLAY_ORDER'
      Required = True
    end
    object QryListNewsHOLD: TWideStringField
      FieldName = 'HOLD'
      Origin = 'HOLD'
      FixedChar = True
      Size = 1
    end
    object QryListNewsNEWS_TITLE: TWideStringField
      FieldName = 'NEWS_TITLE'
      Origin = 'NEWS_TITLE'
      Size = 500
    end
    object QryListNewsTEXT: TWideMemoField
      FieldName = 'TEXT'
      Origin = 'TEXT'
      BlobType = ftWideMemo
    end
    object QryListNewsFEED_ID: TIntegerField
      FieldName = 'FEED_ID'
      Origin = 'FEED_ID'
      Required = True
    end
    object QryListNewsCREATION_DATE: TDateField
      FieldName = 'CREATION_DATE'
      Origin = 'CREATION_DATE'
    end
    object QryListNewsMODIFICATION_DATE: TSQLTimeStampField
      FieldName = 'MODIFICATION_DATE'
      Origin = 'MODIFICATION_DATE'
    end
  end
  object QryCountNews: TFDQuery
    Connection = cnxFeedFlow
    SQL.Strings = (
      'SELECT count(FEED_ID) as "NB_ENR" FROM NEWS'
      'where FEED_ID = :FEED_ID'
      'and (upper(NEWS_TITLE) like :NEWS_TITLE'
      'or CREATION_DATE = :CREATION_DATE'
      'or PUBLICATION_DATE = :PUBLICATION_DATE)')
    Left = 514
    Top = 536
    ParamData = <
      item
        Name = 'FEED_ID'
        DataType = ftInteger
        ParamType = ptInput
        Value = Null
      end
      item
        Name = 'NEWS_TITLE'
        DataType = ftWideString
        ParamType = ptInput
        Size = 500
      end
      item
        Name = 'CREATION_DATE'
        DataType = ftDate
        ParamType = ptInput
      end
      item
        Name = 'PUBLICATION_DATE'
        DataType = ftDate
        ParamType = ptInput
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
      'where NEWS_ID = :NEWS_ID')
    Left = 294
    Top = 692
    ParamData = <
      item
        Name = 'NEWS_ID'
        DataType = ftInteger
        ParamType = ptInput
        Value = Null
      end>
    object QryNewsNEWS_ID: TIntegerField
      FieldName = 'NEWS_ID'
      Origin = 'NEWS_ID'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
    object QryNewsPUBLICATION_DATE: TDateField
      FieldName = 'PUBLICATION_DATE'
      Origin = 'PUBLICATION_DATE'
    end
    object QryNewsEXPIRY_DATE: TDateField
      FieldName = 'EXPIRY_DATE'
      Origin = 'EXPIRY_DATE'
    end
    object QryNewsDISPLAY_ORDER: TIntegerField
      FieldName = 'DISPLAY_ORDER'
      Origin = 'DISPLAY_ORDER'
      Required = True
    end
    object QryNewsHOLD: TWideStringField
      FieldName = 'HOLD'
      Origin = 'HOLD'
      FixedChar = True
      Size = 1
    end
    object QryNewsNEWS_TITLE: TWideStringField
      FieldName = 'NEWS_TITLE'
      Origin = 'NEWS_TITLE'
      Size = 500
    end
    object QryNewsTEXT: TWideMemoField
      FieldName = 'TEXT'
      Origin = 'TEXT'
      BlobType = ftWideMemo
    end
    object QryNewsFEED_ID: TIntegerField
      FieldName = 'FEED_ID'
      Origin = 'FEED_ID'
      Required = True
    end
    object QryNewsCREATION_DATE: TDateField
      FieldName = 'CREATION_DATE'
      Origin = 'CREATION_DATE'
    end
    object QryNewsMODIFICATION_DATE: TSQLTimeStampField
      FieldName = 'MODIFICATION_DATE'
      Origin = 'MODIFICATION_DATE'
    end
    object QryNewsPUBLICATION_DATE_FMT: TWideStringField
      DisplayWidth = 20
      FieldKind = fkCalculated
      FieldName = 'PUBLICATION_DATE_FMT'
      Calculated = True
    end
    object QryNewsEXPIRY_DATE_FMT: TWideStringField
      FieldKind = fkCalculated
      FieldName = 'EXPIRY_DATE_FMT'
      Calculated = True
    end
  end
  object QryShowNews: TFDQuery
    OnCalcFields = QryShowNewsCalcFields
    Connection = cnxFeedFlow
    SQL.Strings = (
      'select '
      '  case n.DISPLAY_ORDER'
      '    when null then 100000'
      '    when 0 then 10000'
      '    else n.DISPLAY_ORDER'
      '  end as REVERSE_ORDER,'
      '  n.DISPLAY_ORDER,'
      '  n.NEWS_ID,'
      '  n.PUBLICATION_DATE,'
      '  n.EXPIRY_DATE,'
      '  n.HOLD,'
      '  n.NEWS_TITLE,'
      '  n.TEXT,'
      '  n.FEED_ID,'
      '  n.CREATION_DATE,'
      '  n.MODIFICATION_DATE,'
      '  f.TITLE'
      'from NEWS n'
      'join FEED_NEWS f on (f.FEED_ID = n.FEED_ID)'
      'where n.FEED_ID = :FEED_ID'
      '  and HOLD = '#39'O'#39
      '  and PUBLICATION_DATE <= localtimestamp'
      '  and EXPIRY_DATE > localtimestamp'
      'order by REVERSE_ORDER, PUBLICATION_DATE desc, NEWS_TITLE;')
    Left = 296
    Top = 824
    ParamData = <
      item
        Name = 'FEED_ID'
        DataType = ftInteger
        ParamType = ptInput
        Value = Null
      end>
    object QryShowNewsREVERSE_ORDER: TIntegerField
      AutoGenerateValue = arDefault
      FieldName = 'REVERSE_ORDER'
      Origin = 'REVERSE_ORDER'
      ProviderFlags = []
      ReadOnly = True
    end
    object QryShowNewsDISPLAY_ORDER: TIntegerField
      FieldName = 'DISPLAY_ORDER'
      Origin = 'DISPLAY_ORDER'
      Required = True
    end
    object QryShowNewsNEWS_ID: TIntegerField
      FieldName = 'NEWS_ID'
      Origin = 'NEWS_ID'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
    object QryShowNewsPUBLICATION_DATE: TDateField
      FieldName = 'PUBLICATION_DATE'
      Origin = 'PUBLICATION_DATE'
    end
    object QryShowNewsEXPIRY_DATE: TDateField
      FieldName = 'EXPIRY_DATE'
      Origin = 'EXPIRY_DATE'
    end
    object QryShowNewsHOLD: TWideStringField
      FieldName = 'HOLD'
      Origin = 'HOLD'
      FixedChar = True
      Size = 1
    end
    object QryShowNewsNEWS_TITLE: TWideStringField
      FieldName = 'NEWS_TITLE'
      Origin = 'NEWS_TITLE'
      Size = 500
    end
    object QryShowNewsTEXT: TWideMemoField
      FieldName = 'TEXT'
      Origin = 'TEXT'
      BlobType = ftWideMemo
    end
    object QryShowNewsFEED_ID: TIntegerField
      FieldName = 'FEED_ID'
      Origin = 'FEED_ID'
      Required = True
    end
    object QryShowNewsCREATION_DATE: TDateField
      FieldName = 'CREATION_DATE'
      Origin = 'CREATION_DATE'
    end
    object QryShowNewsMODIFICATION_DATE: TSQLTimeStampField
      FieldName = 'MODIFICATION_DATE'
      Origin = 'MODIFICATION_DATE'
    end
    object QryShowNewsTITLE: TWideStringField
      AutoGenerateValue = arDefault
      FieldName = 'TITLE'
      Origin = 'TITLE'
      ProviderFlags = []
      ReadOnly = True
      Size = 500
    end
    object QryShowNewsDISPLAY_DATE: TWideStringField
      FieldKind = fkCalculated
      FieldName = 'DISPLAY_DATE'
      Calculated = True
    end
  end
  object QryListCategories: TFDQuery
    Connection = cnxFeedFlow
    SQL.Strings = (
      'select * from CATEGORY'
      'order by CATEGORY_NAME')
    Left = 296
    Top = 960
    object QryListCategoriesCATEGORY_ID: TIntegerField
      FieldName = 'CATEGORY_ID'
      Origin = 'CATEGORY_ID'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
    object QryListCategoriesCATEGORY_NAME: TWideStringField
      FieldName = 'CATEGORY_NAME'
      Origin = 'CATEGORY_NAME'
      Size = 500
    end
  end
  object QryListSubCategories: TFDQuery
    Connection = cnxFeedFlow
    SQL.Strings = (
      'select * from SUBCATEGORY'
      'order by SUBCATEGORY_NAME')
    Left = 296
    Top = 1088
    object QryListSubCategoriesSUBCATEGORY_ID: TIntegerField
      FieldName = 'SUBCATEGORY_ID'
      Origin = 'SUBCATEGORY_ID'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
    object QryListSubCategoriesSUBCATEGORY_NAME: TWideStringField
      FieldName = 'SUBCATEGORY_NAME'
      Origin = 'SUBCATEGORY_NAME'
      Size = 500
    end
  end
  object QryListCountries: TFDQuery
    Connection = cnxFeedFlow
    SQL.Strings = (
      'select * from COUNTRY'
      'order by COUNTRY_NAME')
    Left = 296
    Top = 1216
    object QryListCountriesCOUNTRY_CODE: TWideStringField
      FieldName = 'COUNTRY_CODE'
      Origin = 'COUNTRY_CODE'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
      Size = 3
    end
    object QryListCountriesCOUNTRY_NAME: TWideStringField
      FieldName = 'COUNTRY_NAME'
      Origin = 'COUNTRY_NAME'
      Size = 500
    end
  end
  object QryListLanguages: TFDQuery
    Connection = cnxFeedFlow
    SQL.Strings = (
      'select * from LANGUAGE'
      'order by LANGUAGE_NAME')
    Left = 296
    Top = 1344
    object QryListLanguagesLANGUAGE_CODE: TWideStringField
      FieldName = 'LANGUAGE_CODE'
      Origin = 'LANGUAGE_CODE'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
      Size = 3
    end
    object QryListLanguagesLANGUAGE_NAME: TWideStringField
      FieldName = 'LANGUAGE_NAME'
      Origin = 'LANGUAGE_NAME'
      Size = 100
    end
    object QryListLanguagesTRANSLATED_LANGUAGE_NAME: TWideStringField
      FieldName = 'TRANSLATED_LANGUAGE_NAME'
      Origin = 'TRANSLATED_LANGUAGE_NAME'
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
    object MtUrlsOrdre: TIntegerField
      FieldName = 'Order'
    end
  end
  object QryShowNewsUser: TFDQuery
    OnCalcFields = QryShowNewsCalcFields
    Connection = cnxFeedFlow
    SQL.Strings = (
      'select '
      '  case n.DISPLAY_ORDER'
      '    when null then 100000'
      '    when 0 then 10000'
      '    else n.DISPLAY_ORDER'
      '  end as REVERSE_ORDER,'
      '  n.DISPLAY_ORDER,'
      '  n.NEWS_ID,'
      '  n.PUBLICATION_DATE,'
      '  n.EXPIRY_DATE,'
      '  n.HOLD,'
      '  n.NEWS_TITLE,'
      '  n.TEXT,'
      '  n.FEED_ID,'
      '  n.CREATION_DATE,'
      '  n.MODIFICATION_DATE,'
      '  f.TITLE'
      'from NEWS n'
      'join FEED_NEWS f on (f.FEED_ID = n.FEED_ID)'
      'join NEWS_CONTEXT_COUNTRY p on (p.NEWS_ID = n.NEWS_ID)'
      'join NEWS_CONTEXT_LANG l on (l.NEWS_ID = n.NEWS_ID)'
      'join NEWS_CONTEXT_CATEGORY c on (c.NEWS_ID = n.NEWS_ID)'
      'join NEWS_CONTEXT_SUBCATEGORY s on (s.NEWS_ID = n.NEWS_ID)'
      'where n.FEED_ID = :FEED_ID'
      '  and p.COUNTRY_CODE = :COUNTRY_CODE'
      '  and l.LANGUAGE_CODE = :LANGUAGE_CODE'
      '  and c.CATEGORY_ID = :CATEGORY_ID'
      '  and s.SUBCATEGORY_ID = :SUBCATEGORY_ID'
      '  and HOLD = '#39'O'#39
      '  and PUBLICATION_DATE <= localtimestamp'
      '  and EXPIRY_DATE > localtimestamp'
      'order by REVERSE_ORDER, PUBLICATION_DATE desc, NEWS_TITLE;')
    Left = 504
    Top = 824
    ParamData = <
      item
        Name = 'FEED_ID'
        DataType = ftInteger
        ParamType = ptInput
        Value = Null
      end
      item
        Name = 'COUNTRY_CODE'
        DataType = ftWideString
        ParamType = ptInput
        Size = 3
      end
      item
        Name = 'LANGUAGE_CODE'
        DataType = ftWideString
        ParamType = ptInput
        Size = 3
      end
      item
        Name = 'CATEGORY_ID'
        DataType = ftInteger
        ParamType = ptInput
      end
      item
        Name = 'SUBCATEGORY_ID'
        DataType = ftInteger
        ParamType = ptInput
      end>
    object QryShowNewsUserREVERSE_ORDER: TIntegerField
      AutoGenerateValue = arDefault
      FieldName = 'REVERSE_ORDER'
      Origin = 'REVERSE_ORDER'
      ProviderFlags = []
      ReadOnly = True
    end
    object QryShowNewsUserDISPLAY_ORDER: TIntegerField
      FieldName = 'DISPLAY_ORDER'
      Origin = 'DISPLAY_ORDER'
      Required = True
    end
    object QryShowNewsUserNEWS_ID: TIntegerField
      FieldName = 'NEWS_ID'
      Origin = 'NEWS_ID'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
    object QryShowNewsUserPUBLICATION_DATE: TDateField
      FieldName = 'PUBLICATION_DATE'
      Origin = 'PUBLICATION_DATE'
    end
    object QryShowNewsUserEXPIRY_DATE: TDateField
      FieldName = 'EXPIRY_DATE'
      Origin = 'EXPIRY_DATE'
    end
    object QryShowNewsUserHOLD: TWideStringField
      FieldName = 'HOLD'
      Origin = 'HOLD'
      FixedChar = True
      Size = 1
    end
    object QryShowNewsUserNEWS_TITLE: TWideStringField
      FieldName = 'NEWS_TITLE'
      Origin = 'NEWS_TITLE'
      Size = 500
    end
    object QryShowNewsUserTEXT: TWideMemoField
      FieldName = 'TEXT'
      Origin = 'TEXT'
      BlobType = ftWideMemo
    end
    object QryShowNewsUserFEED_ID: TIntegerField
      FieldName = 'FEED_ID'
      Origin = 'FEED_ID'
      Required = True
    end
    object QryShowNewsUserCREATION_DATE: TDateField
      FieldName = 'CREATION_DATE'
      Origin = 'CREATION_DATE'
    end
    object QryShowNewsUserMODIFICATION_DATE: TSQLTimeStampField
      FieldName = 'MODIFICATION_DATE'
      Origin = 'MODIFICATION_DATE'
    end
    object QryShowNewsUserTITLE: TWideStringField
      AutoGenerateValue = arDefault
      FieldName = 'TITLE'
      Origin = 'TITLE'
      ProviderFlags = []
      ReadOnly = True
      Size = 500
    end
  end
  object QryNewsCategories: TFDQuery
    Connection = cnxFeedFlow
    SQL.Strings = (
      'select * from NEWS_CONTEXT_CATEGORY'
      'where NEWS_ID = :NEWS_ID')
    Left = 512
    Top = 688
    ParamData = <
      item
        Name = 'NEWS_ID'
        DataType = ftInteger
        ParamType = ptInput
        Value = Null
      end>
    object QryNewsCategoriesCATEGORY_ID: TIntegerField
      FieldName = 'CATEGORY_ID'
      Origin = 'CATEGORY_ID'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
    object QryNewsCategoriesNEWS_ID: TIntegerField
      FieldName = 'NEWS_ID'
      Origin = 'NEWS_ID'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
  end
  object QryNewsSubCategory: TFDQuery
    Connection = cnxFeedFlow
    SQL.Strings = (
      'select * from NEWS_CONTEXT_SUBCATEGORY'
      'where NEWS_ID = :NEWS_ID')
    Left = 712
    Top = 688
    ParamData = <
      item
        Name = 'NEWS_ID'
        DataType = ftInteger
        ParamType = ptInput
        Value = Null
      end>
    object QryNewsSubCategorySUBCATEGORY_ID: TIntegerField
      FieldName = 'SUBCATEGORY_ID'
      Origin = 'SUBCATEGORY_ID'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
    object QryNewsSubCategoryNEWS_ID: TIntegerField
      FieldName = 'NEWS_ID'
      Origin = 'NEWS_ID'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
  end
  object QryNewsCountry: TFDQuery
    Connection = cnxFeedFlow
    SQL.Strings = (
      'select * from NEWS_CONTEXT_COUNTRY'
      'where NEWS_ID = :NEWS_ID')
    Left = 840
    Top = 688
    ParamData = <
      item
        Name = 'NEWS_ID'
        DataType = ftInteger
        ParamType = ptInput
        Value = Null
      end>
    object QryNewsCountryCOUNTRY_CODE: TWideStringField
      FieldName = 'COUNTRY_CODE'
      Origin = 'COUNTRY_CODE'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
      Size = 3
    end
    object QryNewsCountryNEWS_ID: TIntegerField
      FieldName = 'NEWS_ID'
      Origin = 'NEWS_ID'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
  end
  object QryNewsLanguage: TFDQuery
    Connection = cnxFeedFlow
    SQL.Strings = (
      'select * from NEWS_CONTEXT_LANG'
      'where NEWS_ID = :NEWS_ID')
    Left = 968
    Top = 688
    ParamData = <
      item
        Name = 'NEWS_ID'
        DataType = ftInteger
        ParamType = ptInput
        Value = Null
      end>
    object QryNewsLanguageLANGUAGE_CODE: TWideStringField
      FieldName = 'LANGUAGE_CODE'
      Origin = 'LANGUAGE_CODE'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
      Size = 3
    end
    object QryNewsLanguageNEWS_ID: TIntegerField
      FieldName = 'NEWS_ID'
      Origin = 'NEWS_ID'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
  end
  object QryListeGroup: TFDQuery
    Connection = cnxFeedFlow
    SQL.Strings = (
      'select FEED_ID, TITLE from FEED_NEWS'
      'where FEED_GROUP = :FEED_GROUP'
      'order by TITLE')
    Left = 512
    Top = 392
    ParamData = <
      item
        Name = 'FEED_GROUP'
        DataType = ftInteger
        ParamType = ptInput
        Value = Null
      end>
    object QryListeGroupFEED_ID: TIntegerField
      FieldName = 'FEED_ID'
      Origin = 'FEED_ID'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
    object QryListeGroupTITLE: TWideStringField
      FieldName = 'TITLE'
      Origin = 'TITLE'
      Size = 500
    end
  end
  object QryShowGroup: TFDQuery
    Connection = cnxFeedFlow
    SQL.Strings = (
      'select first 1 TEXT from NEWS'
      'where FEED_ID = :FEED_ID')
    Left = 504
    Top = 960
    ParamData = <
      item
        Name = 'FEED_ID'
        DataType = ftInteger
        ParamType = ptInput
        Value = Null
      end>
    object QryShowGroupTEXT: TWideMemoField
      FieldName = 'TEXT'
      Origin = 'TEXT'
      BlobType = ftWideMemo
    end
  end
  object QryFeedCategories: TFDQuery
    Connection = cnxFeedFlow
    SQL.Strings = (
      'select * from FEED_CONTEXT_CATEGORY'
      'where FEED_ID = :FEED_ID')
    Left = 688
    Top = 392
    ParamData = <
      item
        Name = 'FEED_ID'
        DataType = ftInteger
        ParamType = ptInput
        Value = Null
      end>
    object QryFeedCategoriesFEED_ID: TIntegerField
      FieldName = 'FEED_ID'
      Origin = 'FEED_ID'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
    object QryFeedCategoriesCATEGORY_ID: TIntegerField
      FieldName = 'CATEGORY_ID'
      Origin = 'CATEGORY_ID'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
  end
  object QryFeedSubCategories: TFDQuery
    Connection = cnxFeedFlow
    SQL.Strings = (
      'select * from FEED_CONTEXT_SUBCATEGORY'
      'where FEED_ID = :FEED_ID')
    Left = 888
    Top = 392
    ParamData = <
      item
        Name = 'FEED_ID'
        DataType = ftInteger
        ParamType = ptInput
        Value = Null
      end>
    object QryFeedSubCategoriesSUBCATEGORY_ID: TIntegerField
      FieldName = 'SUBCATEGORY_ID'
      Origin = 'SUBCATEGORY_ID'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
    object QryFeedSubCategoriesFEED_ID: TIntegerField
      FieldName = 'FEED_ID'
      Origin = 'FEED_ID'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
  end
  object QryFeedCountry: TFDQuery
    Connection = cnxFeedFlow
    SQL.Strings = (
      'select * from FEED_CONTEXT_COUNTRY'
      'where FEED_ID = :FEED_ID')
    Left = 1016
    Top = 392
    ParamData = <
      item
        Name = 'FEED_ID'
        DataType = ftInteger
        ParamType = ptInput
        Value = Null
      end>
    object QryFeedCountryCOUNTRY_CODE: TWideStringField
      FieldName = 'COUNTRY_CODE'
      Origin = 'COUNTRY_CODE'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
      Size = 3
    end
    object QryFeedCountryFEED_ID: TIntegerField
      FieldName = 'FEED_ID'
      Origin = 'FEED_ID'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
  end
  object QryFeedLanguage: TFDQuery
    Connection = cnxFeedFlow
    SQL.Strings = (
      'select * from FEED_CONTEXT_LANG'
      'where FEED_ID = :FEED_ID')
    Left = 1144
    Top = 392
    ParamData = <
      item
        Name = 'FEED_ID'
        DataType = ftInteger
        ParamType = ptInput
        Value = Null
      end>
    object QryFeedLanguageLANGUAGE_CODE: TWideStringField
      FieldName = 'LANGUAGE_CODE'
      Origin = 'LANGUAGE_CODE'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
      Size = 3
    end
    object QryFeedLanguageFEED_ID: TIntegerField
      FieldName = 'FEED_ID'
      Origin = 'FEED_ID'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
  end
  object QryFeedsUser: TFDQuery
    Connection = cnxFeedFlow
    SQL.Strings = (
      
        'SELECT distinct r.FEED_ID, r.FEED_GROUP, r.FEED_NAME, r.TITLE, r' +
        '.DISPLAY_TEMPLATE'
      'FROM FEED_NEWS r'
      'join NEWS n on (n.FEED_ID = r.FEED_ID)'
      'join NEWS_CONTEXT_CATEGORY cc on (cc.NEWS_ID = n.NEWS_ID)'
      'join NEWS_CONTEXT_SUBCATEGORY sc on (sc.NEWS_ID = n.NEWS_ID)'
      'join NEWS_CONTEXT_COUNTRY cp on (cp.NEWS_ID = n.NEWS_ID)'
      'join NEWS_CONTEXT_LANG cl on (cl.NEWS_ID = n.NEWS_ID)'
      'where r.STATUS = '#39'O'#39
      'and r.FEED_GROUP = :FEED_GROUP'
      'and cc.CATEGORY_ID = :CATEGORY_ID'
      'and sc.SUBCATEGORY_ID = :SUBCATEGORY_ID'
      'and cp.COUNTRY_CODE = :COUNTRY_CODE'
      'and cl.LANGUAGE_CODE = :LANGUAGE_CODE')
    Left = 712
    Top = 960
    ParamData = <
      item
        Name = 'FEED_GROUP'
        DataType = ftSmallint
        ParamType = ptInput
        Value = Null
      end
      item
        Name = 'CATEGORY_ID'
        DataType = ftInteger
        ParamType = ptInput
      end
      item
        Name = 'SUBCATEGORY_ID'
        DataType = ftInteger
        ParamType = ptInput
      end
      item
        Name = 'COUNTRY_CODE'
        DataType = ftWideString
        ParamType = ptInput
        Size = 3
      end
      item
        Name = 'LANGUAGE_CODE'
        DataType = ftWideString
        ParamType = ptInput
        Size = 3
      end>
    object QryFeedsUserFEED_ID: TIntegerField
      FieldName = 'FEED_ID'
      Origin = 'FEED_ID'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
    object QryFeedsUserFEED_GROUP: TSmallintField
      FieldName = 'FEED_GROUP'
      Origin = 'FEED_GROUP'
      Required = True
    end
    object QryFeedsUserFEED_NAME: TWideStringField
      FieldName = 'FEED_NAME'
      Origin = 'FEED_NAME'
      Size = 500
    end
    object QryFeedsUserTITLE: TWideStringField
      FieldName = 'TITLE'
      Origin = 'TITLE'
      Size = 500
    end
    object QryFeedsUserDISPLAY_TEMPLATE: TWideStringField
      FieldName = 'DISPLAY_TEMPLATE'
      Origin = 'DISPLAY_TEMPLATE'
      Size = 512
    end
  end
end
