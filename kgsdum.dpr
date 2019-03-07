program kgsdum;

uses
  Vcl.Forms,
  UnitKgsdumMainForm in 'UnitKgsdumMainForm.pas' {KgsdumMainForm},
  UnitKgsdumData in 'UnitKgsdumData.pas' {KgsdumData: TDataModule},
  vclutils in 'utils\vclutils.pas',
  UnitFormLastParty in 'UnitFormLastParty.pas' {FormLastParty},
  data_model in 'data_model.pas',
  crud in 'crud.pas',
  stringutils in 'utils\stringutils.pas',
  stringgridutils in 'utils\stringgridutils.pas',
  ComponentBaloonHintU in 'utils\ComponentBaloonHintU.pas',
  comport in 'utils\comport.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TKgsdumMainForm, KgsdumMainForm);
  Application.CreateForm(TKgsdumData, KgsdumData);
  Application.CreateForm(TFormLastParty, FormLastParty);
  Application.Run;
end.
