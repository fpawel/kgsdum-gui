unit hardware_errors;

interface

uses sysutils;

type
    EHardwareError = class(Exception);
    EBadResponse = class(EHardwareError);
    EDeadlineExceeded = class(EHardwareError);

implementation

end.
