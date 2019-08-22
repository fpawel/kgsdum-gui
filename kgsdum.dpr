program kgsdum;

uses
  Vcl.Forms,
  UnitKgsdumMainForm in 'UnitKgsdumMainForm.pas' {KgsdumMainForm},
  UnitKgsdumData in 'UnitKgsdumData.pas' {KgsdumData: TDataModule},
  UnitFormLastParty in 'UnitFormLastParty.pas' {FormLastParty},
  data_model in 'data_model.pas',
  crud in 'crud.pas',
  stringgridutils in 'utils\stringgridutils.pas',
  comport in 'hardware\comport.pas',
  UnitFormSelectWorksDialog in 'UnitFormSelectWorksDialog.pas' {FormSelectWorksDialog},
  hardware_errors in 'hardware\hardware_errors.pas',
  works in 'work\works.pas',
  stringutils in 'utils\stringutils.pas',
  modbus in 'hardware\modbus.pas',
  UnitFormConsole in 'UnitFormConsole.pas' {FormConsole},
  richeditutils in 'utils\richeditutils.pas',
  UnitFormJournal in 'UnitFormJournal.pas' {FormJournal},
  bcd in 'hardware\bcd.pas',
  UnitFormPopup in 'UnitFormPopup.pas' {FormPopup},
  UnitFormAppConfig in 'UnitFormAppConfig.pas' {FormAppConfig},
  ComponentBaloonHintU in 'utils\ComponentBaloonHintU.pas',
  UnitAppIni in 'UnitAppIni.pas' {AppIni: TDataModule},
  termochamber in 'hardware\termochamber.pas',
  wask in 'hardware\wask.pas',
  UnitWorker in 'work\UnitWorker.pas' {Worker: TDataModule},
  UnitFormChartSeries in 'UnitFormChartSeries.pas' {FormChartSeries},
  UnitFormCharts in 'UnitFormCharts.pas' {FormCharts},
  vclutils in 'utils\vclutils.pas',
  UnitFormData in 'UnitFormData.pas' {FormData};

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
  Application.CreateForm(TWorker, Worker);
  Application.CreateForm(TFormChartSeries, FormChartSeries);
  Application.CreateForm(TFormCharts, FormCharts);
  Application.CreateForm(TFormData, FormData);
  Application.Run;
end.
