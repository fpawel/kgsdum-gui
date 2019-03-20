unit termo;

interface

uses comport, data_model;

procedure TermochamberStart;
procedure TermochamberStop;
procedure TermochamberSetSetpoint(setpoint: double);
function TermochamberReadTemperature: double;

implementation

uses math, types, StrUtils, RegularExpressions, hardware_errors, sysutils,
    run_work, termochamber;


function getResponse(strRequest: string): double;
begin
    result := TermochamberGetResponse(ComportTermoWorker, strRequest);
end;


procedure TermochamberStart;
begin
    getResponse(TermochamberStartRequest);
end;

procedure TermochamberStop;
begin
    getResponse(TermochamberStopRequest);
end;

procedure TermochamberSetSetpoint(setpoint: double);
var
    s: string;
    v:integer;
begin
    setpoint := setpoint * 10.0;
    v := Ceil(setpoint);

    s := IntToHex(Ceil(v), 4);
    if Length(s) > 4 then
        s := Copy(s, Length(s) - 4, 4);
    TermochamberStop;
    getResponse(TermochamberSetpointRequest1 + s + #13#10);
end;

function TermochamberReadTemperature: double;
begin
    result := getResponse(TermochamberTemperatureRequest);
end;

end.
