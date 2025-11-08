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
      'order by TITRE desc'
      '')
    Left = 294
    Top = 245
    ParamData = <
      item
        Name = 'FIRST'
        DataType = ftLargeint
        ParamType = ptInput
        Value = Null
      end
      item
        Name = 'SKIP'
        DataType = ftLargeint
        ParamType = ptInput
      end
      item
        Name = 'TITRE'
        DataType = ftWideString
        ParamType = ptInput
        Size = 500
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
    Top = 248
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
  object qryFeedsCancel: TFDQuery
    Connection = cnxFeedFlow
    SQL.Strings = (
      'SELECT * FROM FEED_NEWS'
      'where ID_FEED = :ID_FEED')
    Left = 518
    Top = 396
    ParamData = <
      item
        Name = 'ID_FEED'
        DataType = ftInteger
        ParamType = ptInput
        Value = Null
      end>
    object qryFeedsCancelID_FEED: TIntegerField
      FieldName = 'ID_FEED'
      Origin = 'ID_FEED'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
    object qryFeedsCancelTITRE: TWideStringField
      FieldName = 'TITRE'
      Origin = 'TITRE'
      Size = 500
    end
    object qryFeedsCancelSTATUT: TWideStringField
      FieldName = 'STATUT'
      Origin = 'STATUT'
      FixedChar = True
      Size = 1
    end
    object qryFeedsCancelDATE_CREATION: TSQLTimeStampField
      FieldName = 'DATE_CREATION'
      Origin = 'DATE_CREATION'
    end
    object qryFeedsCancelDATE_MODIFICATION: TSQLTimeStampField
      FieldName = 'DATE_MODIFICATION'
      Origin = 'DATE_MODIFICATION'
    end
  end
end
