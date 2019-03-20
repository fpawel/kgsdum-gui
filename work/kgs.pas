unit kgs;

interface

uses comport;

function KgsReadCoefficient(DeviceAddr: byte; coefficient: byte): double;

procedure KgsWriteCoefficient(DeviceAddr: byte; coefficient: byte;
  Value: double);

procedure KgsWriteVar(DeviceAddr: byte; ValueAddr: byte; Value: double);

function KgsReadVar(DeviceAddr: byte; ValueAddr: byte): double;

implementation

uses math, wask, sysutils, stringutils, hardware_errors, run_work,
    UnitFormLastParty, data_model;

function KgsGetResponse(r: TKgsRequest): double;
var
    _result: double;
begin
    try
        result := r.GetResponse(ComportProductsworker);
        _result := result;
        Synchronize(
            procedure
            begin
                if r.Direction = KgsRead then
                    FormLastParty.SetAddrValue(r.DeviceAddr,
                      r.ValueAddr, _result)
                else
                    FormLastParty.SetAddrConnection(r.DeviceAddr,
                      'ok: ' + r.ToString, false);

            end);
    except
        on e: EConnectionError do
        begin
            Synchronize(
                procedure
                begin
                    FormLastParty.SetAddrConnection(r.DeviceAddr,
                      Format('%s: %s, %s', [r.ToString, e.ClassName,
                      e.Message]), true);
                end);
            e.Message := 'สรั: ' + r.ToString + ': ' + e.Message;
            raise;
        end;

    end;
end;

function KgsReadVar(DeviceAddr: byte; ValueAddr: byte): double;
var
    r: TKgsRequest;
begin
    r.DeviceAddr := DeviceAddr;
    r.ValueAddr := ValueAddr;
    r.Direction := KgsRead;
    r.Value := 0;
    result := KgsGetResponse(r);
end;

procedure KgsWriteVar(DeviceAddr: byte; ValueAddr: byte; Value: double);
var
    r: TKgsRequest;
begin
    r.DeviceAddr := DeviceAddr;
    r.ValueAddr := 97;
    r.Direction := KgsWrite;
    r.Value := Value;
    KgsGetResponse(r);
end;

function KgsReadCoefficient(DeviceAddr: byte; coefficient: byte): double;
var
    r: TKgsRequest;
begin
    r.DeviceAddr := DeviceAddr;
    r.ValueAddr := 97;
    r.Direction := KgsRead;
    r.Value := (coefficient div 60) * 60.;
    KgsGetResponse(r);
    result := KgsReadVar(DeviceAddr, ceil(1. * coefficient - r.Value));
end;

procedure KgsWriteCoefficient(DeviceAddr: byte;
coefficient: byte; Value: double);
var
    r: TKgsRequest;
begin
    r.DeviceAddr := DeviceAddr;
    r.ValueAddr := 97;
    r.Direction := KgsRead;
    r.Value := (coefficient div 60) * 60.;
    KgsGetResponse(r);

    r.ValueAddr := ceil(1. * coefficient - r.Value);
    r.Value := Value;
    r.Direction := KgsWrite;
    KgsGetResponse(r);

end;


end.
