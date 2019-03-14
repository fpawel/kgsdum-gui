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
      '  party_id          INTEGER PRIMARY KEY      NOT NULL,'
      
        '  created_at        TIMESTAMP WITH TIME ZONE NOT NULL           ' +
        '             DEFAULT (datetime('#39'now'#39')) UNIQUE,'
      
        '  product_type      TEXT                     NOT NULL           ' +
        '             DEFAULT '#39'00.01'#39','
      
        '  pgs_beg           REAL                     NOT NULL CHECK ( pg' +
        's_beg >= 0 ) DEFAULT 0,'
      
        '  pgs_mid           REAL                     NOT NULL CHECK ( pg' +
        's_mid >= 0 ) DEFAULT 50,'
      
        '  pgs_end           REAL                     NOT NULL CHECK ( pg' +
        's_end >= 0 ) DEFAULT 100,'
      
        '  temperature_norm  REAL                     NOT NULL           ' +
        '             DEFAULT 20,'
      
        '  temperature_plus  REAL                     NOT NULL           ' +
        '             DEFAULT 60,'
      
        '  temperature_minus REAL                     NOT NULL           ' +
        '             DEFAULT -30'
      ');'
      ''
      'CREATE TABLE IF NOT EXISTS product'
      '('
      '  product_id              INTEGER PRIMARY KEY      NOT NULL,'
      '  party_id                INTEGER                  NOT NULL,'
      
        '  created_at              TIMESTAMP WITH TIME ZONE NOT NULL DEFA' +
        'ULT (datetime('#39'now'#39')) UNIQUE,'
      
        '  serial_number           TEXT                     NOT NULL CHEC' +
        'K (serial_number <> '#39#39' ),'
      
        '  addr                    SMALLINT                 NOT NULL CHEC' +
        'K (addr > 0),'
      
        '  production              BOOLEAN                  NOT NULL DEFA' +
        'ULT FALSE,'
      '  concentration_beg_norm  REAL,'
      '  concentration_mid_norm  REAL,'
      '  concentration_end_norm  REAL,'
      '  concentration_beg_minus REAL,'
      '  concentration_mid_minus REAL,'
      '  concentration_end_minus REAL,'
      '  concentration_beg_plus  REAL,'
      '  concentration_mid_plus  REAL,'
      '  concentration_end_plus  REAL,'
      '  UNIQUE (party_id, addr),'
      '  UNIQUE (party_id, serial_number),'
      
        '  FOREIGN KEY (party_id) REFERENCES party (party_id) ON DELETE C' +
        'ASCADE'
      ');'
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
      ''
      'CREATE VIEW IF NOT EXISTS product_info AS'
      '  WITH q1 AS ('
      '    SELECT product.*,'
      ''
      '           concentration_beg_norm - pgs_beg  AS d_beg_norm,'
      '           concentration_mid_norm - pgs_mid  AS d_mid_norm,'
      '           concentration_end_norm - pgs_end  AS d_end_norm,'
      ''
      '           concentration_beg_minus - pgs_beg AS d_beg_minus,'
      '           concentration_mid_minus - pgs_mid AS d_mid_minus,'
      '           concentration_end_minus - pgs_end AS d_end_minus,'
      ''
      '           concentration_beg_plus - pgs_beg  AS d_beg_plus,'
      '           concentration_mid_plus - pgs_mid  AS d_mid_plus,'
      '           concentration_end_plus - pgs_end  AS d_end_plus,'
      ''
      '           (0.1 + 0.12 * party.pgs_beg)      AS err_beg_limit,'
      '           (0.1 + 0.12 * party.pgs_mid)      AS err_mid_limit,'
      '           (0.1 + 0.12 * party.pgs_end)      AS err_end_limit'
      '    FROM product'
      '           INNER JOIN party on product.party_id = party.party_id'
      '    )'
      '    SELECT q1.*,'
      ''
      
        '           round(d_beg_norm / err_beg_limit, 3)  AS err_beg_norm' +
        '_percent,'
      
        '           round(d_mid_norm / err_mid_limit, 3)  AS err_mid_norm' +
        '_percent,'
      
        '           round(d_end_norm / err_end_limit, 3)  AS err_end_norm' +
        '_percent,'
      ''
      
        '           round(d_beg_minus / err_beg_limit, 3) AS err_beg_minu' +
        's_percent,'
      
        '           round(d_mid_minus / err_mid_limit, 3) AS err_mid_minu' +
        's_percent,'
      
        '           round(d_end_minus / err_end_limit, 3) AS err_end_minu' +
        's_percent,'
      ''
      
        '           round(d_beg_plus / err_beg_limit, 3)  AS err_beg_plus' +
        '_percent,'
      
        '           round(d_mid_plus / err_mid_limit, 3)  AS err_mid_plus' +
        '_percent,'
      
        '           round(d_end_plus / err_end_limit, 3)  AS err_end_plus' +
        '_percent,'
      ''
      '           abs(d_beg_norm) < err_beg_limit       AS ok_beg_norm,'
      '           abs(d_mid_norm) < err_mid_limit       AS ok_mid_norm,'
      '           abs(d_end_norm) < err_end_limit       AS ok_end_norm,'
      ''
      
        '           abs(d_beg_minus) < err_beg_limit      AS ok_beg_minus' +
        ','
      
        '           abs(d_mid_minus) < err_mid_limit      AS ok_mid_minus' +
        ','
      
        '           abs(d_end_minus) < err_end_limit      AS ok_end_minus' +
        ','
      ''
      '           abs(d_beg_plus) < err_beg_limit       AS ok_beg_plus,'
      '           abs(d_mid_plus) < err_mid_limit       AS ok_mid_plus,'
      '           abs(d_end_plus) < err_end_limit       AS ok_end_plus'
      '    FROM q1;')
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
