object KgsdumData: TKgsdumData
  OldCreateOrder = False
  Height = 172
  Width = 294
  object Conn: TFDConnection
    Params.Strings = (
      'Database=kgsdum'
      'User_Name=postgres'
      'Password=falena190312'
      'Server=localhost'
      'DriverID=PG')
    LoginPrompt = False
    Left = 32
    Top = 48
  end
  object FDPhysPgDriverLink1: TFDPhysPgDriverLink
    Left = 168
    Top = 64
  end
end
