program kgsdum;

uses
  Vcl.Forms,
  UnitKgsdumMainForm in 'UnitKgsdumMainForm.pas' {KgsdumMainForm},
  UnitKgsdumData in 'UnitKgsdumData.pas' {KgsdumData: TDataModule},
  vclutils in 'utils\vclutils.pas',
  UnitFormLastParty in 'UnitFormLastParty.pas' {FormLastParty},
  data_model in 'data_model.pas',
  crud in 'crud.pas',
  stringgridutils in 'utils\stringgridutils.pas',
  ComponentBaloonHintU in 'utils\ComponentBaloonHintU.pas',
  comport in 'hardware\comport.pas',
  kgs in 'hardware\kgs.pas',
  UnitFormSelectWorksDialog in 'UnitFormSelectWorksDialog.pas' {FormSelectWorksDialog},
  run_work in 'work\run_work.pas',
  errors in 'errors.pas',
  works in 'work\works.pas',
  PropertyValueEditors in 'settings\PropertyValueEditors.pas',
  UnitFormProperties in 'settings\UnitFormProperties.pas' {FormProperties},
  config_value in 'settings\config_value.pas',
  stringutils in 'utils\stringutils.pas',
  modbus in 'hardware\modbus.pas',
  UnitFormConsole in 'UnitFormConsole.pas' {FormConsole},
  richeditutils in 'utils\richeditutils.pas',
  UnitFormJournal in 'UnitFormJournal.pas' {FormJournal};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TKgsdumMainForm, KgsdumMainForm);
  Application.CreateForm(TKgsdumData, KgsdumData);
  Application.CreateForm(TFormLastParty, FormLastParty);
  Application.CreateForm(TFormSelectWorksDialog, FormSelectWorksDialog);
  Application.CreateForm(TFormProperties, FormProperties);
  Application.CreateForm(TFormConsole, FormConsole);
  Application.CreateForm(TFormJournal, FormJournal);
  Application.Run;
end.
