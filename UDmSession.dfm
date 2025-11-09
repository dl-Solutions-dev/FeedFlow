object DmSession: TDmSession
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 1920
  Width = 2560
  PixelsPerInch = 192
  object cnxFeedFlow: TFDConnection
    Params.Strings = (
      'ConnectionDef=FeedFlow')
    LoginPrompt = False
    Left = 291
    Top = 100
  end
  object QryListeFeeds: TFDQuery
    Connection = cnxFeedFlow
    SQL.Strings = (
      'SELECT first :FIRST skip :SKIP * FROM FEED_NEWS'
      'where upper(TITRE) like :TITRE'
      'order by DATE_CREATION desc'
      '')
    Left = 294
    Top = 245
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
        Name = 'TITRE'
        DataType = ftWideString
        ParamType = ptInput
        Size = 500
        Value = '%'
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
      'SELECT * FROM FEED_NEWS'
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
  end
  object QryListeNews: TFDQuery
    Connection = cnxFeedFlow
    SQL.Strings = (
      'SELECT first :FIRST skip :SKIP * FROM NEWS'
      'where ID_FEED = :ID_FEED'
      'and (upper(TITRE_NEWS) like :TITRE_NEWS'
      'or DATE_CREATION = :DATE_CREATION'
      'or DATE_PUBLICATION = :DATE_PUBLICATION)'
      'order by DATE_CREATION desc, TITRE_NEWS'
      '')
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
      'SELECT * FROM NEWS'
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
  end
end
