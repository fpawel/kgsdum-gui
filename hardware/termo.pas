unit termo;

interface

uses comport, data_model;

implementation

uses hardware_errors, sysutils, StrUtils, run_work;

const
    reqStart = '01WRD,01,0101,0001';
    reqStop = '01WRD,01,0101,0004';
    reqRead = '01RRD,02,0001,0002';
    reqSetFmt = '01WRD,01,0102,%s';

function _getResponse(strRequest: string; w: TComportWorker): string;
var
    response: TBytes;
    s: string;
const
    validWriteResponse = #2'01WRD,OK'#$D#$A;
begin

    try
        response := comport.GetResponse
          (TEncoding.ANSI.GetBytes(#2 + strRequest + #13#10), w,
            procedure(_: TBytes)
            begin
            end);
    except
        on e: EHardwareError do
        begin
            e.Message := 'термокамера: ' + strRequest + ': ' + e.Message;
            raise;
        end;
    end;

    s := TEncoding.ASCII.GetString(response);
//    if AnsiStartsStr('01WRD,', strRequest) and (s <> validWriteResponse) then
    // raise EBadResponse.Create
    // (format('термокамера: %s: ответ на команду %s'));

end;

end.
