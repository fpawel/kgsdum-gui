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
  comport in 'hardware\comport.pas',
  kgs in 'work\kgs.pas',
  UnitFormSelectWorksDialog in 'UnitFormSelectWorksDialog.pas' {FormSelectWorksDialog},
  run_work in 'work\run_work.pas',
  hardware_errors in 'hardware\hardware_errors.pas',
  works in 'work\works.pas',
  stringutils in 'utils\stringutils.pas',
  modbus in 'hardware\modbus.pas',
  UnitFormConsole in 'UnitFormConsole.pas' {FormConsole},
  richeditutils in 'utils\richeditutils.pas',
  UnitFormJournal in 'UnitFormJournal.pas' {FormJournal},
  bcd in 'hardware\bcd.pas',
  termo in 'hardware\termo.pas',
  UnitFormPopup in 'UnitFormPopup.pas' {FormPopup},
  UnitFormAppConfig in 'UnitFormAppConfig.pas' {FormAppConfig},
  ComponentBaloonHintU in 'utils\ComponentBaloonHintU.pas',
  do_each_product in 'work\do_each_product.pas',
  UnitAppIni in 'UnitAppIni.pas' {AppIni: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;

  Application.CreateForm(TAppIni, AppIni);
  Application.CreateForm(TKgsdumData, KgsdumData);
  Application.CreateForm(TKgsdumMainForm, KgsdumMainForm);
  Application.CreateForm(TFormLastParty, FormLastParty);
  Application.CreateForm(TFormSelectWorksDialog, FormSelectWorksDialog);
  Application.CreateForm(TFormConsole, FormConsole);
  Application.CreateForm(TFormJournal, FormJournal);
  Application.CreateForm(TFormPopup, FormPopup);
  Application.CreateForm(TFormAppConfig, FormAppConfig);
  Application.Run;
end.
