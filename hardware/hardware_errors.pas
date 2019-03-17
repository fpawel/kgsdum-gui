unit hardware_errors;

interface

uses sysutils;

type
    EHardwareError = class(Exception);
    EConnectionError = class(EHardwareError);
    EBadResponse = class(EConnectionError);
    EDeadlineExceeded = class(EConnectionError);

implementation

end.
