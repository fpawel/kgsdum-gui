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
      
        'INSERT INTO party (created_at) SELECT CURRENT_TIMESTAMP WHERE NO' +
        'T EXISTS(SELECT 1 FROM party);')
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
  object ConnCharts: TFDConnection
    Params.Strings = (
      'Database=$(APPDATA)\kgsdum\kgsdum_charts.sqlite'
      'JournalMode=WAL'
      'LockingMode=Normal'
      'DriverID=SQLite')
    LoginPrompt = False
    Left = 88
    Top = 184
  end
  object FDQuery3: TFDQuery
    Connection = ConnCharts
    SQL.Strings = (
      'PRAGMA foreign_keys = ON;'
      'PRAGMA encoding = '#39'UTF-8'#39';'
      ''
      'CREATE TABLE IF NOT EXISTS bucket'
      '('
      '  bucket_id  INTEGER NOT NULL PRIMARY KEY,'
      
        '  created_at TIMESTAMP    NOT NULL UNIQUE DEFAULT (DATETIME('#39'now' +
        #39','#39'+3 hours'#39')),'
      
        '  updated_at TIMESTAMP    NOT NULL DEFAULT (DATETIME('#39'now'#39','#39'+3 h' +
        'ours'#39')),'
      '  name       TEXT    NOT NULL'
      ');'
      ''
      'CREATE TABLE IF NOT EXISTS series'
      '('
      '  bucket_id INTEGER NOT NULL,'
      '  address   INTEGER NOT NULL CHECK (address > 0),'
      '  variable  INTEGER NOT NULL CHECK (variable >= 0),'
      '  stored_at REAL    NOT NULL UNIQUE,'
      '  value     REAL    NOT NULL,'
      '  FOREIGN KEY (bucket_id) REFERENCES bucket (bucket_id)'
      '    ON DELETE CASCADE'
      ');'
      ''
      'CREATE TRIGGER IF NOT EXISTS trigger_bucket_updated_at'
      '  AFTER INSERT'
      '  ON series'
      '  FOR EACH ROW'
      '  BEGIN'
      '    UPDATE bucket'
      '    SET updated_at = DATETIME('#39'now'#39','#39'+3 hours'#39')'
      '    WHERE bucket.bucket_id = new.bucket_id;'
      '  END;')
    Left = 312
    Top = 200
  end
end
