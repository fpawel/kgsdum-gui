unit termo;

interface

uses comport, data_model;

procedure TermochamberStart(w: TComportWorker);
procedure TermochamberStop(w: TComportWorker);
procedure TermochamberSetSetpoint(w: TComportWorker; setpoint: double);
function TermochamberReadTemperature(w: TComportWorker): double;

implementation

uses math, types, StrUtils, RegularExpressions, hardware_errors, sysutils,
    run_work;

function parseTemperature(strResponse: string): double;
var
    match: TMatch;
    group: TGroup;
    temperature10, i: integer;
const
    pattern = '^'#2'01RRD,OK,([0-9a-fA-F]{4,4}),([0-9a-fA-F]{4,4})'#13#10'$';
begin
    match := TRegEx.match(strResponse, pattern, []);

    if (not match.Success) or (match.Groups.Count <> 3) then
        raise EBadResponse.Create
          ('ответ на запрос температуры не соответствует образцу');

    if not TryStrToInt('$' + match.Groups[1].Value, temperature10) then
        raise EBadResponse.Create
          (format('ответ на запрос температуры : match.Groups[1]: %s - ?',
          [match.Groups[1].Value]));
    result := (temperature10 * 1.0) / 10.0;
end;

function getResponse(w: TComportWorker; strRequest: string): double;
var
    response: TBytes;
    strResponse: string;
const
    validResponseOnWrite = #2'01WRD,OK'#13#10;
begin

    try
        response := comport.getResponse
          (TEncoding.ANSI.GetBytes(#2 + strRequest + #13#10), w,
            procedure(_: TBytes)
            begin
            end);

        strResponse := TEncoding.ASCII.GetString(response);

        if AnsiStartsStr('01WRD,', strRequest) then
        begin
            if strResponse <> validResponseOnWrite then
                raise EBadResponse.Create
                  (format('ожидался ответ "%s" на команду записи',
                  [validResponseOnWrite]));
            exit(0);
        end;

        result := parseTemperature(strResponse)
    except
        on e: EHardwareError do
        begin
            if strResponse <> '' then

                e.Message := format('термокамера: %s -> %s: %s',
                  [strRequest, strResponse, e.Message])
            else
                e.Message := format('термокамера: %s: %s',
                  [strRequest, e.Message]);

            raise;
        end;
    end;

end;

procedure TermochamberStart(w: TComportWorker);
begin
    getResponse(w, '01WRD,01,0101,0001');
end;

procedure TermochamberStop(w: TComportWorker);
begin
    getResponse(w, '01WRD,01,0101,0004');
end;

procedure TermochamberSetSetpoint(w: TComportWorker; setpoint: double);
var s:string;
begin
    s := IntToHex(Ceil(setpoint * 10. ), 4);
    if Length(s) > 4 then
        s := Copy(s, length(s)-4, 4);
    TermochamberStop(w);
    getResponse(w, '01WRD,01,0102,' + s);
end;

function TermochamberReadTemperature(w: TComportWorker): double;
begin
    result := getResponse(w, '01RRD,02,0001,0002');
end;

end.
