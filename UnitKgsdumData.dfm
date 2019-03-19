object KgsdumData: TKgsdumData
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 297
  Width = 508
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
  object FDQuery1: TFDQuery
    Connection = Conn
    SQL.Strings = (
      'CREATE TABLE IF NOT EXISTS party'
      '('
      '  party_id     INTEGER PRIMARY KEY      NOT NULL,'
      
        '  created_at   TIMESTAMP WITH TIME ZONE NOT NULL                ' +
        '     DEFAULT (datetime('#39'now'#39')) UNIQUE,'
      
        '  product_type TEXT                     NOT NULL                ' +
        '     DEFAULT '#39'00.01'#39','
      
        '  pgs1         REAL                     NOT NULL CHECK ( pgs1 >=' +
        ' 0 ) DEFAULT 0,'
      
        '  pgs2         REAL                     NOT NULL CHECK ( pgs2 >=' +
        ' 0 ) DEFAULT 4,'
      
        '  pgs3         REAL                     NOT NULL CHECK ( pgs3 >=' +
        ' 0 ) DEFAULT 7.5,'
      
        '  pgs4         REAL                     NOT NULL CHECK ( pgs4 >=' +
        ' 0 ) DEFAULT 12'
      ');'
      ''
      'CREATE TABLE IF NOT EXISTS product'
      '('
      '  product_id    INTEGER PRIMARY KEY      NOT NULL,'
      '  party_id      INTEGER                  NOT NULL,'
      
        '  created_at    TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT (datet' +
        'ime('#39'now'#39')) UNIQUE,'
      
        '  serial_number TEXT                     NOT NULL CHECK (serial_' +
        'number <> '#39#39' ),'
      
        '  addr          SMALLINT                 NOT NULL CHECK (addr > ' +
        '0),'
      '  production    BOOLEAN                  NOT NULL DEFAULT FALSE,'
      ''
      '  c_norm1       REAL,'
      '  c_norm2       REAL,'
      '  c_norm3       REAL,'
      '  c_norm4       REAL,'
      ''
      '  c_plus1       REAL,'
      '  c_plus2       REAL,'
      '  c_plus3       REAL,'
      '  c_plus4       REAL,'
      ''
      '  c_minus1      REAL,'
      '  c_minus2      REAL,'
      '  c_minus3      REAL,'
      '  c_minus4      REAL,'
      ''
      '  UNIQUE (party_id, addr),'
      '  UNIQUE (party_id, serial_number),'
      
        '  FOREIGN KEY (party_id) REFERENCES party (party_id) ON DELETE C' +
        'ASCADE'
      ');'
      ''
      ''
      'CREATE VIEW IF NOT EXISTS last_party AS'
      'SELECT *'
      'FROM party'
      'ORDER BY created_at DESC'
      'LIMIT 1;'
      ''
      'CREATE VIEW IF NOT EXISTS last_party_id AS'
      'SELECT party_id'
      'FROM party'
      'ORDER BY created_at DESC'
      'LIMIT 1;'
      ''
      'CREATE VIEW IF NOT EXISTS last_party_product AS'
      'SELECT *'
      'FROM product'
      'WHERE party_id IN (SELECT * FROM last_party_id)'
      'ORDER BY created_at;'
      ''
      ''
      'CREATE TABLE IF NOT EXISTS app_config'
      '('
      '  property TEXT PRIMARY KEY NOT NULL,'
      '  value                     NOT NULL'
      ');')
    Left = 200
    Top = 120
  end
  object FDQuery2: TFDQuery
    Connection = ConnJournal
    SQL.Strings = (
      'PRAGMA foreign_keys = ON;'
      'PRAGMA encoding = '#39'UTF-8'#39';'
      ''
      'CREATE TABLE IF NOT EXISTS work'
      '('
      '  work_id    INTEGER   NOT NULL PRIMARY KEY,'
      
        '  created_at TIMESTAMP NOT NULL UNIQUE DEFAULT (DATETIME('#39'now'#39'))' +
        ','
      '  name       TEXT      NOT NULL'
      ');'
      ''
      'CREATE TABLE IF NOT EXISTS entry'
      '('
      '  entry_id   INTEGER   NOT NULL PRIMARY KEY,'
      '  work_id    INTEGER   NOT NULL,'
      
        '  created_at TIMESTAMP NOT NULL UNIQUE DEFAULT (DATETIME('#39'now'#39'))' +
        ','
      '  level      INTEGER   NOT NULL,'
      '  message    TEXT      NOT NULL,'
      
        '  FOREIGN KEY (work_id) REFERENCES work (work_id) ON DELETE CASC' +
        'ADE'
      ');'
      ''
      'CREATE VIEW IF NOT EXISTS last_work AS'
      'SELECT * FROM work'
      'ORDER BY created_at DESC'
      'LIMIT 1;')
    Left = 288
    Top = 120
  end
end
