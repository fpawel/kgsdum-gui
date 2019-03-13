object KgsdumData: TKgsdumData
  OldCreateOrder = False
  Height = 172
  Width = 294
  object Conn: TFDConnection
    Params.Strings = (
      'Database=$(APPDATA)\kgsdum\kgsdum.sqlite'
      'JournalMode=WAL'
      'LockingMode=Normal'
      'DriverID=SQLite')
    LoginPrompt = False
    Left = 32
    Top = 48
  end
  object FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink
    Left = 160
    Top = 48
  end
  object ConnJournal: TFDConnection
    Params.Strings = (
      'Database=$(APPDATA)\kgsdum\kgsdum_journal.sqlite'
      'JournalMode=WAL'
      'LockingMode=Normal'
      'DriverID=SQLite')
    LoginPrompt = False
    Left = 64
    Top = 104
  end
end
