unit termochamber;

interface

uses comport;

const
    TermochamberStartRequest = #2'01WRD,01,0101,0001'#13#10;
    TermochamberStopRequest = #2'01WRD,01,0101,0004'#13#10;
    TermochamberSetpointRequest1 = #2'01WRD,01,0102,';
    TermochamberTemperatureRequest = #2'01RRD,02,0001,0002'#13#10;
    TermochamberValidResponseOnWrite = #2'01WRD,OK'#13#10;

function TermochamberParseTemperature(strResponse: string): double;
function TermochamberFormatRequest(request: string): string;
function TermochamberFormatResponse(response: string): string;
function TermochamberGetResponse(ComportWorker: TComportWorker;
  strRequest: string): double;

implementation

uses strutils, hardware_errors, sysutils, RegularExpressions;

function TermochamberGetResponse(ComportWorker: TComportWorker;
  strRequest: string): double;
var
    response: TBytes;
    strResponse: string;

begin
    try

        response := comport.getResponse(TEncoding.ANSI.GetBytes(strRequest),
          ComportWorker,
            procedure(_: TBytes)
            begin
            end);
        strResponse := TEncoding.ASCII.GetString(response);

        if AnsiStartsStr(#2'01WRD,', strRequest) then
        begin
            if strResponse <> TermochamberValidResponseOnWrite then
                raise EBadResponse.Create
                  (format('�������� ����� "%s" �� ������� ������',
                  [TermochamberValidResponseOnWrite]));
            exit(0);
        end;

        result := TermochamberParseTemperature(strResponse);
    except
        on e: EConnectionError do
        begin
            if strResponse <> '' then
                e.Message := format('�����������: "%s" -> "%s", %s --> %s: %s',
                  [strRequest, strResponse,
                  TermochamberFormatRequest(strRequest),
                  TermochamberFormatResponse(strResponse),

                  e.Message])
            else
                e.Message := format('�����������: "%s", %s: %s',
                  [strRequest, TermochamberFormatRequest(strRequest),
                  e.Message]);

            raise;
        end;
    end;

end;

function TermochamberFormatRequest(request: string): string;
var
    setpoint: integer;
    s: string;
begin
    if request = TermochamberStartRequest then
        exit('�����');
    if request = TermochamberStopRequest then
        exit('����');

    if request = TermochamberTemperatureRequest then
        exit('�����������');

    if AnsiStartsStr(TermochamberSetpointRequest1, request) and
      (length(request) > 20) then
    begin
        s := '$' + Copy(request, 16, 4);
        if TryStrToInt(s, setpoint) then
            exit('������� ' + FloatToStr((setpoint * 1.0) / 10.0));
    end;

    exit('?');
end;

function TermochamberParseTemperature(strResponse: string): double;
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
          ('����� �� ������ ����������� �� ������������� �������');

    if not TryStrToInt('$' + match.Groups[1].Value, temperature10) then
        raise EBadResponse.Create
          (format('����� �� ������ ����������� : match.Groups[1]: %s - ?',
          [match.Groups[1].Value]));
    result := (temperature10 * 1.0) / 10.0;
end;

function TermochamberFormatResponse(response: string): string;
var
    s: string;
begin
    if response = TermochamberValidResponseOnWrite then
        exit('��');
    try
        exit(FloatToStr(TermochamberParseTemperature(response)) + '"C');
    except

    end;
    exit('?');
end;

end.
