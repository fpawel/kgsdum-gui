unit comport;

interface

uses classes;

type
    TComport = record
        Handle: THandle;
    end;

    TComPortHwHandshaking = (hhNONE, hhNONERTSON, hhRTSCTS);
    TComPortSwHandshaking = (shNONE, shXONXOFF);

    TPacketMode = (pmDiscard, pmPass);
    { What to do with incomplete (incoming) packets }

    TComPortLineStatus = (lsCTS, lsDSR, lsRING, lsRLSD);
    TComPortLineStatusSet = set of TComPortLineStatus;

const
    dcb_Binary = $00000001;
    dcb_ParityCheck = $00000002;
    dcb_OutxCtsFlow = $00000004;
    dcb_OutxDsrFlow = $00000008;
    dcb_DtrControlMask = $00000030;
    dcb_DtrControlDisable = $00000000;
    dcb_DtrControlEnable = $00000010;
    dcb_DtrControlHandshake = $00000020;
    dcb_DsrSensivity = $00000040;
    dcb_TXContinueOnXoff = $00000080;
    dcb_OutX = $00000100;
    dcb_InX = $00000200;
    dcb_ErrorChar = $00000400;
    dcb_NullStrip = $00000800;
    dcb_RtsControlMask = $00003000;
    dcb_RtsControlDisable = $00000000;
    dcb_RtsControlEnable = $00001000;
    dcb_RtsControlHandshake = $00002000;
    dcb_RtsControlToggle = $00003000;
    dcb_AbortOnError = $00004000;
    dcb_Reserveds = $FFFF8000;

procedure EnumComPorts(const Ports: TStrings);

implementation

uses registry, windows, sysutils;

function DefaulTCommConfig: TCommConfig;
begin
    with result do
    begin
        ZeroMemory(@dcb, sizeof(dcb));
        dcb.DCBlength := sizeof(dcb);
        dcb.wReserved := 0;
        dcb.BaudRate := CBR_9600;

        dcb.ByteSize := 8;
        dcb.Parity := NoParity;
        dcb.StopBits := OneStopBit;
        dcb.Flags := 1;
        dcb.wReserved = 0;

        dcb.XonLim = 1024 / 4;
        dcb.XoffLim = dcb.XonLim;
        dcb.ByteSize = 8;
        dcb.Parity = NoParity;
        dcb.StopBits = OneStopBit;
        dcb.XonChar = 17;
        dcb.XoffChar = 19;
        dcb.ErrorChar = $C0;
        dcb.EofChar = 0;
        dcb.EvtChar = 0;

        dcb.Flags := dcb_Binary;
    end;

end;

function portNameAvail(portName: string): bool;
var
    Ports: TStringList;
begin
    Ports := TStringList.Create;
    try
        EnumComPorts(Ports);
        result := Ports.IndexOf(portName) > -1;
    finally
        Ports.Free;
    end;

end;

function OpencomPort(portName: string; baud: integer;
  var comport: TComport): string;
var
    commConfig: TCommConfig;
begin
    try
        if not portNameAvail(portName) then
            exit(format('COM порт "%s" не представлен в системе', [portName]));

        if portName < 'COM10' then
            portName := '\\.\' + portName;
        comport.Handle := CreateFile(PChar(portName), GENERIC_READ or
          GENERIC_WRITE, 0, 0, OPEN_EXISTING, 0, 0);

        if comport.Handle = INVALID_HANDLE_VALUE then
            exit(SysErrorMessage(GetLastError));

        commConfig := DefaulTCommConfig;
        SetCommState(comport.Handle, commConfig);

        // Устанавливаем таймауты
        with TimeOuts do
        Begin
            ReadIntervalTimeout := MAXDWORD;
            ReadTotalTimeoutMultiplier := 0;
            ReadTotalTimeoutConstant := 0;
        End;
        SetCommTimeOuts(hComm, TimeOuts);

    except
        on e: Exception do
            exit(e.Message);
    end;
end;

procedure EnumComPorts(const Ports: TStrings);
var
    nInd: integer;
begin { EnumComPorts }
    with TRegistry.Create(KEY_READ) do
        try
            RootKey := HKEY_LOCAL_MACHINE;
            if OpenKey('hardware\devicemap\serialcomm', FALSE) then
                try
                    Ports.BeginUpdate();
                    try
                        GetValueNames(Ports);
                        for nInd := Ports.Count - 1 downto 0 do
                            Ports.Strings[nInd] :=
                              ReadString(Ports.Strings[nInd]);
                    finally
                        Ports.EndUpdate()
                    end { try-finally }
                finally
                    CloseKey()
                end { try-finally }
            else
                Ports.Clear()
        finally
            Free()
        end { try-finally }
end { EnumComPorts };

end.
