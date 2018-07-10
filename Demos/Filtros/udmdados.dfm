object dmdados: Tdmdados
  OldCreateOrder = False
  Height = 303
  Width = 393
  object FDConnection1: TFDConnection
    Params.Strings = (
      
        'Database=C:\Users\juliomar\Desktop\Demos\3 Com Dados\FireDAC\chi' +
        'nook.db'
      'OpenMode=ReadOnly'
      'DriverID=SQLite')
    Connected = True
    LoginPrompt = False
    BeforeConnect = FDConnection1BeforeConnect
    Left = 56
    Top = 24
  end
  object FDQuery1: TFDQuery
    Connection = FDConnection1
    SQL.Strings = (
      'select * from employees')
    Left = 56
    Top = 88
    object FDQuery1EmployeeId: TFDAutoIncField
      FieldName = 'EmployeeId'
      Origin = 'EmployeeId'
      ProviderFlags = [pfInWhere, pfInKey]
    end
    object FDQuery1LastName: TWideStringField
      FieldName = 'LastName'
      Origin = 'LastName'
      Required = True
    end
    object FDQuery1FirstName: TWideStringField
      FieldName = 'FirstName'
      Origin = 'FirstName'
      Required = True
    end
    object FDQuery1Title: TWideStringField
      FieldName = 'Title'
      Origin = 'Title'
      Size = 30
    end
    object FDQuery1ReportsTo: TIntegerField
      FieldName = 'ReportsTo'
      Origin = 'ReportsTo'
    end
    object FDQuery1BirthDate: TDateTimeField
      FieldName = 'BirthDate'
      Origin = 'BirthDate'
    end
    object FDQuery1HireDate: TDateTimeField
      FieldName = 'HireDate'
      Origin = 'HireDate'
    end
    object FDQuery1Address: TWideStringField
      FieldName = 'Address'
      Origin = 'Address'
      Size = 70
    end
    object FDQuery1City: TWideStringField
      FieldName = 'City'
      Origin = 'City'
      Size = 40
    end
    object FDQuery1State: TWideStringField
      FieldName = 'State'
      Origin = 'State'
      Size = 40
    end
    object FDQuery1Country: TWideStringField
      FieldName = 'Country'
      Origin = 'Country'
      Size = 40
    end
    object FDQuery1PostalCode: TWideStringField
      FieldName = 'PostalCode'
      Origin = 'PostalCode'
      Size = 10
    end
    object FDQuery1Phone: TWideStringField
      FieldName = 'Phone'
      Origin = 'Phone'
      Size = 24
    end
    object FDQuery1Fax: TWideStringField
      FieldName = 'Fax'
      Origin = 'Fax'
      Size = 24
    end
    object FDQuery1Email: TWideStringField
      FieldName = 'Email'
      Origin = 'Email'
      Size = 60
    end
  end
end
