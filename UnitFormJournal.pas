unit UnitFormJournal;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, Vcl.StdCtrls, Vcl.ExtCtrls;

type
    TFormJournal = class(TForm)
        Panel1: TPanel;
        StringGrid1: TStringGrid;
        Splitter1: TSplitter;
        Panel2: TPanel;
        ComboBox1: TComboBox;
        procedure Panel1Resize(Sender: TObject);
    private
        { Private declarations }
    public
        { Public declarations }
        procedure fetch_days;
    end;

var
    FormJournal: TFormJournal;

implementation

uses FireDAC.Comp.Client, UnitKgsdumData;

{$R *.dfm}


procedure TFormJournal.Panel1Resize(Sender: TObject);
begin
    with StringGrid1 do
    begin
        ColWidths[0] := 70;
        ColWidths[1] := Panel1.Width - ColWidths[0] - 5;
    end;
end;

procedure TFormJournal.fetch_days;
begin

    with TFDQuery.Create(nil) do
    begin
        Connection := KgsdumData.Conn;
        try
            SQL.Text := 'INSERT INTO party DEFAULT VALUES;';
            ExecSQL;
            SQL.Text :=
              'INSERT INTO product (party_id, serial_number, addr) VALUES ((SELECT * FROM last_party_id), ''1'', 1);';
            ExecSQL;
        finally
            Free;
        end;
    end;


end;

end.
