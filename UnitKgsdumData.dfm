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
      '    party_id     INTEGER PRIMARY KEY      NOT NULL,'
      
        '    created_at   TIMESTAMP WITH TIME ZONE NOT NULL              ' +
        '       DEFAULT (datetime('#39'now'#39')) UNIQUE,'
      
        '    product_type TEXT                     NOT NULL              ' +
        '       DEFAULT '#39'00.01'#39','
      
        '    pgs1         REAL                     NOT NULL CHECK ( pgs1 ' +
        '>= 0 ) DEFAULT 0,'
      
        '    pgs2         REAL                     NOT NULL CHECK ( pgs2 ' +
        '>= 0 ) DEFAULT 4,'
      
        '    pgs3         REAL                     NOT NULL CHECK ( pgs3 ' +
        '>= 0 ) DEFAULT 7.5,'
      
        '    pgs4         REAL                     NOT NULL CHECK ( pgs4 ' +
        '>= 0 ) DEFAULT 12'
      ');'
      ''
      'CREATE TABLE IF NOT EXISTS product'
      '('
      '    product_id                INTEGER PRIMARY KEY      NOT NULL,'
      '    party_id                  INTEGER                  NOT NULL,'
      
        '    created_at                TIMESTAMP WITH TIME ZONE NOT NULL ' +
        'DEFAULT (datetime('#39'now'#39')) UNIQUE,'
      
        '    serial_number             TEXT                     NOT NULL ' +
        'CHECK (serial_number <> '#39#39' ),'
      
        '    addr                      SMALLINT                 NOT NULL ' +
        'CHECK (addr > 0),'
      
        '    production                BOOLEAN                  NOT NULL ' +
        'DEFAULT FALSE,'
      ''
      '    work_plus20        REAL,'
      '    ref_plus20         REAL,'
      ''
      '    work_gas3          REAL,'
      ''
      '    work_minus5        REAL,'
      '    ref_minus5         REAL,'
      ''
      '    work_plus50        REAL,'
      '    ref_plus50         REAL,'
      ''
      '    c1_plus20 REAL,'
      '    c4_plus20 REAL,'
      '    c1_zero   REAL,'
      '    c4_zero   REAL,'
      '    c1_plus50 REAL,'
      '    c4_plus50 REAL,'
      '    c1_plus20ret REAL,'
      '    c4_plus20ret REAL,'
      ''
      '    UNIQUE (party_id, addr),'
      '    UNIQUE (party_id, serial_number),'
      
        '    FOREIGN KEY (party_id) REFERENCES party (party_id) ON DELETE' +
        ' CASCADE'
      ');'
      ''
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
      '')
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
      
        '  created_at TIMESTAMP NOT NULL UNIQUE DEFAULT (DATETIME('#39'now'#39', ' +
        #39'+3 hours'#39')),'
      '  name       TEXT      NOT NULL'
      ');'
      ''
      'CREATE TABLE IF NOT EXISTS entry'
      '('
      '  entry_id   INTEGER NOT NULL PRIMARY KEY,'
      '  work_id    INTEGER NOT NULL,'
      
        '  created_at REAL    NOT NULL UNIQUE DEFAULT (julianday('#39'now'#39', '#39 +
        '+3 hours'#39')),'
      '  level      INTEGER NOT NULL,'
      '  message    TEXT    NOT NULL,'
      
        '  FOREIGN KEY (work_id) REFERENCES work (work_id) ON DELETE CASC' +
        'ADE'
      ');'
      ''
      'CREATE VIEW IF NOT EXISTS last_work AS'
      'SELECT *'
      'FROM work'
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
  object Timer1: TTimer
    Interval = 120000
    Left = 384
    Top = 64
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Console'
    Left = 408
    Top = 160
  end
  object FDQueryProductsValues: TFDQuery
    Connection = Conn
    SQL.Strings = (
      'SELECT CAST(product_id AS TEXT) AS product_id,'
      '       CAST(product.party_id AS TEXT) AS party_id,'
      '       CAST(addr AS TEXT) AS addr,'
      '       CAST(serial_number AS TEXT) AS serial_number,'
      '       cast(strftime('#39'%d'#39', party.created_at) AS INTEGER) AS day,'
      '       work_plus20,'
      '       ref_plus20,'
      '       work_gas3,'
      '       work_minus5,'
      '       ref_minus5,'
      '       work_plus50,'
      '       ref_plus50,'
      ''
      
        '       100 * (c1_plus20 - pgs1) / (0.1 + pgs1 * 0.12) AS err1_pl' +
        'us20,'
      
        '       100 * (c4_plus20 - pgs1) / (0.1 + pgs4 * 0.12) AS err4_pl' +
        'us20,'
      
        '       100 * (c1_zero - pgs1) / (0.1 + pgs1 * 0.12) AS err1_zero' +
        ','
      
        '       100 * (c4_zero - pgs1) / (0.1 + pgs4 * 0.12) AS err4_zero' +
        ','
      
        '       100 * (c1_plus50 - pgs1) / (0.1 + pgs1 * 0.12) AS err1_pl' +
        'us50,'
      
        '       100 * (c4_plus50 - pgs1) / (0.1 + pgs4 * 0.12) AS err4_pl' +
        'us50,'
      
        '       100 * (c1_plus20ret - pgs1) / (0.1 + pgs1 * 0.12) AS err1' +
        '_plus20ret,'
      
        '       100 * (c4_plus20ret - pgs1) / (0.1 + pgs4 * 0.12) AS err4' +
        '_plus20ret,'
      ''
      '       c1_plus20,'
      '       c4_plus20,'
      '       c1_zero,'
      '       c4_zero,'
      '       c1_plus50,'
      '       c4_plus50,'
      '       c1_plus20ret,'
      '       c4_plus20ret,'
      '       pgs1,'
      '       pgs2,'
      '       pgs3,'
      '       pgs4'
      ''
      'FROM product'
      '         INNER JOIN party on product.party_id = party.party_id'
      'WHERE cast(strftime('#39'%Y'#39', party.created_at) AS INTEGER) = :year'
      '  AND cast(strftime('#39'%m'#39', party.created_at) AS INTEGER) = :month'
      'ORDER BY product.created_at')
    Left = 176
    Top = 216
    ParamData = <
      item
        Name = 'YEAR'
        ParamType = ptInput
      end
      item
        Name = 'MONTH'
        ParamType = ptInput
      end>
  end
end
