unit works;

interface

implementation

uses sysutils, comport;

var _flag_canceled : longint;

procedure _on_comport_background;
begin
    if AtomicIncrement(_flag_canceled, 0) = 1 then
        raise EAbort.Create('выполнение перрвано');


end;

end.
