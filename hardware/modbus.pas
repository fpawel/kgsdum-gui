unit modbus;

interface

    type
        TModbusRequest = record
            Addr : byte;
            Cmd : byte  ;
            Data : TArray<byte>;
        end;

implementation

end.
