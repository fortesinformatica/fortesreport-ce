object dmdados: Tdmdados
  OldCreateOrder = False
  Height = 303
  Width = 393
  object SQLConnection1: TSQLConnection
    ConnectionName = 'SQLiteEmployees'
    DriverName = 'Sqlite'
    LoginPrompt = False
    Params.Strings = (
      'DriverName=Sqlite'
      'DriverUnit=Data.DbxSqlite'
      
        'DriverPackageLoader=TDBXSqliteDriverLoader,DBXSqliteDriver250.bp' +
        'l'
      
        'MetaDataPackageLoader=TDBXSqliteMetaDataCommandFactory,DbxSqlite' +
        'Driver250.bpl'
      'FailIfMissing=False'
      
        'Database=C:\Users\juliomar\Desktop\Demos\3 Com Dados\DBX\chinook' +
        '.db')
    Left = 40
    Top = 24
  end
  object SQLQuery1: TSQLQuery
    MaxBlobSize = -1
    Params = <>
    SQL.Strings = (
      'select * from employees')
    SQLConnection = SQLConnection1
    Left = 40
    Top = 80
  end
end
