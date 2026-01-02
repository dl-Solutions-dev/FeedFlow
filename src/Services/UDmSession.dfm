object DmSession: TDmSession
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 754
  Width = 875
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
  object MtUrls: TFDMemTable
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    Left = 296
    Top = 224
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
end
