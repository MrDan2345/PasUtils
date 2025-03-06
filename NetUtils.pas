unit NetUtils;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$modeswitch nestedprocvars}
{$modeswitch typehelpers}
{$optimization autoinline}
{$macro on}

interface

uses
{$if defined(windows)}
  Windows,
  WinSock2,
{$endif}
  SysUtils,
  Classes,
  CommonUtils,
  DateUtils;

{$if defined(windows)}
const SockLib = 'ws2_32.dll';
{$define call_decl := stdcall}
{$else}
const SockLib = 'c';
{$define call_decl := cdecl}
{$endif}

//address families
const AF_UNSPEC = 0;
const AF_UNIX = 1;
const AF_INET = 2;
const AF_PACKET = 17;
{$if defined(windows)}
const AF_IPX = 6;
const AF_INET6 = 23;
{$else}
const AF_IPX = 4;
const AF_INET6 = 10;
{$endif}

//socket types
{$if defined(sunos)}
const SOCK_STREAM = 2;
const SOCK_DGRAM = 1;
const SOCK_RAW = 4;
const SOCK_RDM = 5;
const SOCK_SEQPACKET = 6;
{$else}
{$if defined(cpumips) and defined(linux)}
const SOCK_STREAM = 2;
const SOCK_DGRAM = 1;
{$else cpumips and linux}
const SOCK_STREAM = 1;               { stream (connection) socket   }
const SOCK_DGRAM = 2;               { datagram (conn.less) socket  }
{$endif cpumips and linux}
const SOCK_RAW = 3;               { raw socket                   }
const SOCK_RDM = 4;               { reliably-delivered message   }
const SOCK_SEQPACKET = 5;               { sequential packet socket     }
{$endif}

const INADDR_ANY = UInt32(0);
const INADDR_NONE = UInt32(-1);
const SOCKET_ERROR = -1;

//shutdown options
const SHUT_RD = 0;
const SHUT_WR = 1;
const SHUT_RDWR = 2;

const UNET_IPPROTO_IP = 0;
const UNET_IPPROTO_ICMP = 1;

const ICMP_ECHO = 8;

const UOK = 0;
{$if defined(windows)}
const SOL_SOCKET = $ffff;
const SO_DEBUG = $0001;
const SO_ACCEPTCONN = $0002;
const SO_REUSEADDR = $0004;
const SO_KEEPALIVE = $0008;
const SO_DONTROUTE = $0010;
const SO_BROADCAST = $0020;
const SO_USELOOPBACK = $0040;
const SO_LINGER = $0080;
const SO_OOBINLINE = $0100;
const SO_DONTLINGER = Int32(not SO_LINGER);
const SO_EXCLUSIVEADDRUSE = Int32(not SO_REUSEADDR);
const SO_SNDBUF = $1001;
const SO_RCVBUF = $1002;
const SO_SNDLOWAT = $1003;
const SO_RCVLOWAT = $1004;
const SO_SNDTIMEO = $1005;
const SO_RCVTIMEO = $1006;
const SO_ERROR = $1007;
const SO_TYPE = $1008;
const SO_CONNDATA = $7000;
const SO_CONNOPT = $7001;
const SO_DISCDATA = $7002;
const SO_DISCOPT = $7003;
const SO_CONNDATALEN = $7004;
const SO_CONNOPTLEN = $7005;
const SO_DISCDATALEN = $7006;
const SO_DISCOPTLEN = $7007;
const SO_OPENTYPE = $7008;
const SO_SYNCHRONOUS_ALERT = $10;
const SO_SYNCHRONOUS_NONALERT = $20;
const SO_MAXDG = $7009;
const SO_MAXPATHDG = $700A;
const SO_UPDATE_ACCEPT_CONTEXT = $700B;
const SO_CONNECT_TIME = $700C;
const TCP_NODELAY = $0001;
const TCP_BSDURGENT = $7000;

const IOCPARM_MASK = $7f;
const IOC_VOID = $20000000;
const IOC_OUT = $40000000;
const IOC_IN = $80000000;
const IOC_INOUT = (IOC_IN or IOC_OUT);
const FIONREAD = UInt32(IOC_OUT or
    ((UInt32(SizeOf(UInt32)) and IOCPARM_MASK) shl 16) or
    (UInt32(UInt8('f')) shl 8) or 127
);
const FIONBIO = UInt32(IOC_IN or
    ((UInt32(SizeOf(UInt32)) and IOCPARM_MASK) shl 16) or
    (UInt32(UInt8('f')) shl 8) or 126
);
const FIOASYNC = UInt32(IOC_IN or
    ((UInt32(SizeOf(UInt32)) and IOCPARM_MASK) shl 16) or
    (UInt32(UInt8('f')) shl 8) or 125
);

const UBASEERR = 10000;
const UEINTR = UBASEERR + 4;
const UEBADF = UBASEERR + 9;
const UEACCES = UBASEERR + 13;
const UEFAULT = UBASEERR + 14;
const UEINVAL = UBASEERR + 22;
const UEMFILE = UBASEERR + 24;
const UEWOULDBLOCK = UBASEERR + 35;
const UEINPROGRESS = UBASEERR + 36;
const UEALREADY = UBASEERR + 37;
const UENOTSOCK = UBASEERR + 38;
const UEDESTADDRREQ = UBASEERR + 39;
const UEMSGSIZE = UBASEERR + 40;
const UEPROTOTYPE = UBASEERR + 41;
const UENOPROTOOPT = UBASEERR + 42;
const UEPROTONOSUPPORT = UBASEERR + 43;
const UESOCKTNOSUPPORT = UBASEERR + 44;
const UEOPNOTSUPP = UBASEERR + 45;
const UEPFNOSUPPORT = UBASEERR + 46;
const UEAFNOSUPPORT = UBASEERR + 47;
const UEADDRINUSE = UBASEERR + 48;
const UEADDRNOTAVAIL = UBASEERR + 49;
const UENETDOWN = UBASEERR + 50;
const UENETUNREACH = UBASEERR + 51;
const UENETRESET = UBASEERR + 52;
const UECONNABORTED = UBASEERR + 53;
const UECONNRESET = UBASEERR + 54;
const UENOBUFS = UBASEERR + 55;
const UEISCONN = UBASEERR + 56;
const UENOTCONN = UBASEERR + 57;
const UESHUTDOWN = UBASEERR + 58;
const UETOOMANYREFS = UBASEERR + 59;
const UETIMEDOUT = UBASEERR + 60;
const UECONNREFUSED = UBASEERR + 61;
const UELOOP = UBASEERR + 62;
const UENAMETOOLONG = UBASEERR + 63;
const UEHOSTDOWN = UBASEERR + 64;
const UEHOSTUNREACH = UBASEERR + 65;
const UENOTEMPTY = UBASEERR + 66;
const UEPROCLIM = UBASEERR + 67;
const UEUSERS = UBASEERR + 68;
const UEDQUOT = UBASEERR + 69;
const UESTALE = UBASEERR + 70;
const UEREMOTE = UBASEERR + 71;
const USYSNOTREADY = UBASEERR + 91;
const UVERNOTSUPPORTED = UBASEERR + 92;
const UNOTINITIALISED = UBASEERR + 93;
const UEDISCON = UBASEERR + 101;
const UENOMORE = UBASEERR + 102;
const UECANCELLED = UBASEERR + 103;
const UEINVALIDPROCTABLE = UBASEERR + 104;
const UEINVALIDPROVIDER = UBASEERR + 105;
const UEPROVIDERFAILEDINIT = UBASEERR + 106;
const USYSCALLFAILURE = UBASEERR + 107;
const USERVICE_NOT_FOUND = UBASEERR + 108;
const UTYPE_NOT_FOUND = UBASEERR + 109;
const U_E_NO_MORE = UBASEERR + 110;
const U_E_CANCELLED = UBASEERR + 111;
const UEREFUSED = UBASEERR + 112;
{$else}
const SOL_SOCKET = 1;
const SO_DEBUG = 1;
const SO_REUSEADDR = 2;
const SO_TYPE = 3;
const SO_ERROR = 4;
const SO_DONTROUTE = 5;
const SO_BROADCAST = 6;
const SO_SNDBUF = 7;
const SO_RCVBUF = 8;
const SO_KEEPALIVE = 9;
const SO_OOBINLINE = 10;
const SO_NO_CHECK = 11;
const SO_PRIORITY = 12;
const SO_LINGER = 13;
const SO_BSDCOMPAT = 14;
const SO_REUSEPORT = 15;
const SO_PASSCRED = 16;
const SO_PEERCRED = 17;
const SO_RCVLOWAT = 18;
const SO_SNDLOWAT = 19;
const SO_RCVTIMEO = 20;
const SO_SNDTIMEO = 21;
const SO_SECURITY_AUTHENTICATION = 22;
const SO_SECURITY_ENCRYPTION_TRANSPORT = 23;
const SO_SECURITY_ENCRYPTION_NETWORK = 24;
const SO_BINDTODEVICE = 25;
const SO_ATTACH_FILTER = 26;
const SO_DETACH_FILTER = 27;
const SO_PEERNAME = 28;
const SO_TIMESTAMP = 29;
const SCM_TIMESTAMP = SO_TIMESTAMP;
const SO_ACCEPTCONN = 30;
{$endif}

MSG_OOB = $1;
MSG_PEEK = $2;
MSG_DONTROUTE = $4;
MSG_INTERRUPT = $10;
MSG_MAXIOVLEN = 16;
MSG_PARTIAL = $8000;

type TUSockLen = UInt32;
type PUSockLen = ^TUSockLen;

type TUMacAddr = array[0..5] of UInt8;
type PUMacAddr = ^TUMacAddr;
type TUMacAddrArray = array of TUMacAddr;

type PUInAddr = ^TUInAddr;
TUInAddr = packed record
  case UInt8 of
  0: (Addr8: array[0..3] of UInt8);
  1: (Addr32: UInt32);
end;
type TUInAddrArray = array of TUInAddr;

type TUSockAddr = packed record
  sin_family: UInt16;
  sin_port: UInt16;
  sin_addr: TUInAddr;
  sin_zero: packed array[0..7] of UInt8;
end;
type PUSockAddr = ^TUSockAddr;
type TUInetSockAddr = TUSockAddr;

type PUInAddr6 = ^TUInAddr6;
TUInAddr6 = packed record
  case UInt8 of
  0: (Addr8: array[0..15] of UInt8);
  1: (Addr16: array[0..7] of UInt16);
  2: (Addr32: array[0..3] of UInt32);
end;

type PUSockAddr6 = ^TUSockAddr6;
TUSockAddr6 = packed record
  sin6_len: UInt8;
  sin6_family: UInt8;
  sin6_port: UInt16;
  sin6_flowinfo: UInt32;
  sin6_addr: TUInAddr6;
  sin6_scope_id: UInt32;
end;

type PPUAddrInfo = ^PUAddrInfo;
PUAddrInfo = ^TUAddrInfo;
TUAddrInfo = record
  ai_flags: Int32;
  ai_family: Int32;
  ai_socktype: Int32;
  ai_protocol: Int32;
  ai_addrlen: Int32;
  ai_addr: PUSockAddr;
  ai_canonname: PAnsiChar;
  ai_next: PUAddrInfo;
end;

type PUHostEnt = ^TUHostEnt;
TUHostEnt = record
  h_name: PAnsiChar;
  h_aliases: ^PAnsiChar;
  h_addrtype: LongInt;
  h_length: LongInt;
  h_addr_list: ^PAnsiChar;
end;

type PPUIfAddrs = ^PUIfAddrs;
PUIfAddrs = ^TUIfAddrs;
TUIfAddrs = record
  ifa_next: PUIfAddrs;
  ifa_name: PAnsiChar;
  ifa_flags: UInt32;
  ifa_addr: PUSockAddr;
  ifa_netmask: PUSockAddr;
  ifa_ifu: PUSockAddr;
  ifa_data: Pointer;
end;

type TUIfMap = record
  mem_start: UInt64;
  mem_end: UInt64;
  base_addr: UInt16;
  irq: UInt8;
  dma: UInt8;
  port: UInt8;
end;

const UNET_IF_NAMESIZE = 16;
type TUIfReq = record
  const IFHWADDRLEN = 6;
  const IFNAMSIZ = UNET_IF_NAMESIZE;
  var ifrn_name: array[0..IFNAMSIZ - 1] of AnsiChar;
  case Int32 of
  0: (ifru_addr: TUSockAddr);
  1: (ifru_dstaddr: TUSockAddr);
  2: (ifru_broadaddr: TUSockAddr);
  3: (ifru_netmask: TUSockAddr);
  4: (ifru_hwaddr: TUSockAddr);
  5: (ifru_flags: Int16);
  6: (ifru_ivalue: Int32);
  7: (ifru_mtu: Int32);
  8: (ifru_map: TUIfMap);
  9: (ifru_slave: array[0..IFNAMSIZ - 1] of AnsiChar);
  10: (ifru_newname: array[0..IFNAMSIZ - 1] of AnsiChar);
  11: (ifru_data: Pointer);
end;
type PUIfReq = ^TUIfReq;

type TUIfConf = record
  ifc_len: Int32;
  case UInt8 of
  0: (ifcu_buf: Pointer);
  1: (ifcu_req: PUIfReq);
end;
type PUIfConf = ^TUIfConf;

type TUSocket = Int32;
type PUSocket = ^TUSocket;
const INVALID_SOCKET = TUSocket(not(0));

type TTimeVal = record
  tv_sec: Int32;
  tv_usec: Int32;
end;
type PTimeVal = ^TTimeVal;

{$if defined(windows)}
type TUFDSet = record
  const SETSIZE = 64;
  var fd_count: UInt32;
  var fd_array: array[0..SETSIZE - 1] of TUSocket;
end;
{$else}
const UFD_MAXFDSET = 1024;
{$ifdef cpu64}
type TUFDSetUnit = UInt64;
{$else}
type TUFDSetUnit = UInt32;
{$endif}
type TUFDSet = array[0..(UFD_MAXFDSET div (8 * SizeOf(TUFDSetUnit))) - 1] of TUFDSetUnit;
{$endif}
type PUFDSet = ^TUFDSet;

type TUMacAddrImpl = type helper for TUMacAddr
  const Zero: TUMacAddr = (0, 0, 0, 0, 0, 0);
  const Broadcast: TUMacAddr = ($ff, $ff, $ff, $ff, $ff, $ff);
  function IsValid: Boolean;
end;

type TUInAddrImpl = type helper for TUInAddr
  const Zero: TUInAddr = (Addr32: 0);
  const Any: TUInAddr = (Addr32: 0);
  const LocalhostH: TUInAddr = (Addr8: (1, 0, 0, 127));
  const LocalhostN: TUInAddr = (Addr8: (127, 0, 0, 1));
  const Broadcast: TUInAddr = (Addr32: $ffffffff);
end;

type TUInAddr6Impl = type helper for TUInAddr6
  const Zero: TUInAddr6 = (Addr32: (0, 0, 0, 0));
end;

type TUSockAddrImpl = type helper for TUSockAddr
  const Default: TUSockAddr = (
    sin_family: AF_INET;
    sin_port: 0; sin_addr: (Addr32: 0);
    sin_zero: (0, 0, 0, 0, 0, 0, 0, 0);
  );
end;

type TUSockAddr6Impl = type helper for TUSockAddr6

end;

type TUSocketImpl = type helper for TUSocket
  const Invalid = -1;
  function Make(
    const SockDomain: Int32 = AF_INET;
    const SockType: Int32 = SOCK_STREAM;
    const SockProtocol: Int32 = 0
  ): TUSocket;
  function MakeTCP(
    const SockDomain: Int32 = AF_INET;
    const SockProtocol: Int32 = 0
  ): TUSocket;
  function MakeUDP(
    const SockDomain: Int32 = AF_INET;
    const SockProtocol: Int32 = 0
  ): TUSocket;
  function Bind(const Addr: PUSockAddr; const AddrLen: TUSockLen): Int32;
  function Listen(const Backlog: Int32): Int32;
  function Accept(const Addr: PUSockAddr; const AddrLen: PUSockLen): Int32;
  function Connect(const Addr: PUSockAddr; const AddrLen: TUSockLen): Int32;
  function Send(
    const Buffer: Pointer;
    const BufferLen: UInt32;
    const Flags: Int32
  ): Int32;
  function Send(const Msg: AnsiString): Int32;
  function Recv(
    const Buffer: Pointer;
    const BufferLen: UInt32;
    const Flags: Int32
  ): Int32;
  function Recv: String;
  function SendTo(
    const Buffer: Pointer;
    const BufferLen: UInt32;
    const Flags: Int32;
    const ToAddr: PUSockAddr;
    const ToLen: TUSockLen
  ): Int32;
  function RecvFrom(
    const Buffer: Pointer;
    const BufferLen: UInt32;
    const Flags: Int32;
    const AddrFrom: PUSockAddr;
    const AddrLen: PUSockLen
  ): Int32;
  function GetSockOpt(
    const Level: Int32; const OptName: Int32;
    const OptVal: Pointer; const OptLen: PUSockLen
  ): Int32;
  function SetSockOpt(
    const Level: Int32; const OptName: Int32;
    const OptVal: Pointer; const OptLen: TUSockLen
  ): Int32;
  function SetSockOpt(
    const OptName: Int32;
    const OptVal: Int32
  ): Int32;
  function SelectRead(const TimeoutMs: UInt32 = UInt32(-1)): Int32;
  function SelectWrite(const TimeoutMs: UInt32 = UInt32(-1)): Int32;
  function SetBlocking(const Blocking: Boolean): Int32;
  function Shutdown(const How: Int32 = SHUT_RDWR): Int32;
  function Close: Int32;
  function IsValid: Boolean;
  class function Create(
    const SockDomain: Int32 = AF_INET;
    const SockType: Int32 = SOCK_STREAM;
    const SockProtocol: Int32 = 0
  ): TUSocket; static;
  class function CreateTCP(
    const SockDomain: Int32 = AF_INET;
    const SockProtocol: Int32 = 0
  ): TUSocket; static;
  class function CreateUDP(
    const SockDomain: Int32 = AF_INET;
    const SockProtocol: Int32 = 0
  ): TUSocket; static;
end;

type TUNetInAddrProc = procedure (const Addr: TUInAddr) of Object;
type TUNetSockAddrProc = procedure (const Addr: TUSockAddr) of Object;

type TUNet = class
public
  type TBeacon = class (TURefClass)
  public
    type TPeer = record
      Addr: TUInAddr;
      TimeStamp: UInt64;
      Message: String;
    end;
    type TPeerArray = array of TPeer;
  private
    type TBeaconThread = class (TThread)
    public
      var Beacon: TBeacon;
      var Message: String;
    end;
    type TListener = class (TBeaconThread)
    private
      var _Sock: TUSocket;
    public
      procedure Execute; override;
      procedure TerminatedSet; override;
    end;
    type TBroadcaster = class (TBeaconThread)
    private
      var _Sock: TUSocket;
      var _Event: TUEvent;
    public
      procedure Execute; override;
      procedure TerminatedSet; override;
    end;
    var _Enabled: Boolean;
    var _Active: Boolean;
    var _Port: UInt16;
    var _BroadcastInterval: UInt32;
    var _LocalAddr: TUInAddr;
    var _HostName: String;
    var _MessageJson: TUJsonRef;
    var _Message: String;
    var _Listener: TListener;
    var _Broadcaster: TBroadcaster;
    var _Peers: TPeerArray;
    var _PeersLock: TUCriticalSection;
    property LocalAddr: TUInAddr read _LocalAddr;
    property HostName: String read _HostName;
    procedure SetEnabled(const Value: Boolean);
    procedure SetActive(const Value: Boolean);
    procedure SetPort(const Value: UInt16);
    procedure SetMessage(const Value: String);
    procedure AddPeer(const Addr: TUInAddr; const Message: String);
    function GetPeers: TPeerArray;
    procedure ClearOldPeers;
  public
    property Enabled: Boolean read _Enabled write SetEnabled;
    property Active: Boolean read _Active write SetActive;
    property Port: UInt16 read _Port write SetPort;
    property BroadcastInterval: Uint32 read _BroadcastInterval write _BroadcastInterval;
    property Peers: TPeerArray read GetPeers;
    property Message: String read _Message write SetMessage;
    procedure Reset;
    constructor Create;
    destructor Destroy; override;
  end;
  type TBeaconRef = specialize TUSharedRef<TBeacon>;
  type TSessionMarker = array[0..3] of AnsiChar;
  const DefaultMarker: TSessionMarker = 'UDSM';
  type TSessionMessage = packed record
    Marker: TSessionMarker;
    Id: TGuid;
    Domain: TGuid;
    Mask: UInt32;
    LocalAddr: TUInAddr;
    Visible: Boolean;
    SeekId: TGuid;
  end;
  type TPeerMessage = packed record
    PeerId: TGuid;
    Addr: TUInAddr;
    Port: UInt16;
  end;
  type TBroker = class (TURefClass)
  public
  private
    type TBrokerThread = class (TThread)
    private
      var _Broker: TBroker;
    public
      property Broker: TBroker read _Broker write _Broker;
    end;
    type TListener = class (TBrokerThread)
    public
      procedure Execute; override;
      procedure TerminatedSet; override;
    end;
    type TUpdater = class (TBrokerThread)
    private
      var _Event: TUEvent;
    public
      procedure Resume;
      procedure Execute; override;
      procedure TerminatedSet; override;
    end;
    type TClient = class
    public
      var Id: TGuid;
      var AddrLocal: TUInAddr;
      var Addr: TUInAddr;
      var Port: UInt16;
      var TimeStamp: TDateTime;
      var Visible: Boolean;
      var SeekId: TGuid;
      function MakePeerMessage: TPeerMessage;
      function MakeSockAddr: TUSockAddr;
    end;
    type TClientArray = array of TClient;
    type TDomain = class
    public
      var Id: TGuid;
      var Clients: array of TClient;
      function FindClient(const ClientId: TGuid): TClient;
      destructor Destroy; override;
    end;
    type TDomainArray = array of TDomain;
    var Sock: TUSocket;
    var _Enabled: Boolean;
    var _Listener: TListener;
    var _Updater: TUpdater;
    var _Domains: TDomainArray;
    var _Marker: TSessionMarker;
    var _Port: UInt16;
    var _CS: TUCriticalSection;
    function QueryClient(const DomainId: TGuid; const ClientId: TGuid): TClient;
    procedure Connection(
      const ClientAddr: TUInAddr;
      const ClientPort: UInt16;
      const Message: TSessionMessage
    );
    procedure Update;
    procedure PurgeStaleConnections(const ThresholdSec: Int32 = 60);
    procedure SetEnabled(const Value: Boolean);
  public
    property Port: UInt16 read _Port write _Port;
    property Enabled: Boolean read _Enabled write SetEnabled;
    constructor Create;
    destructor Destroy; override;
  end;
end;

{$if defined(windows)}
const UNET_WSADESCRIPTION_LEN = 256;
const UNET_WSASYS_STATUS_LEN = 128;
type PUNetWSAData = ^TUNetWSAData;
TUNetWSAData = record
   wVersion: UInt16;
   wHighVersion: UInt16;
{$ifdef win64}
   iMaxSockets: UInt16;
   iMaxUdpDg: UInt16;
   lpVendorInfo: PAnsiChar;
   szDescription: array[0..UNET_WSADESCRIPTION_LEN] of AnsiChar;
   szSystemStatus: array[0..UNET_WSASYS_STATUS_LEN] of AnsiChar;
{$else}
   szDescription: array[0..UNET_WSADESCRIPTION_LEN] of AnsiChar;
   szSystemStatus: array[0..UNET_WSASYS_STATUS_LEN] of AnsiChar;
   iMaxSockets: UInt16;
   iMaxUdpDg: UInt16;
   lpVendorInfo: PAnsiChar;
{$endif}
end;
const MAX_ADAPTER_NAME_LENGTH = 256;
const MAX_ADAPTER_DESCRIPTION_LENGTH = 128;
const MAX_ADAPTER_ADDRESS_LENGTH = 8;
type PIP_ADDRESS_STRING = ^TIP_ADDRESS_STRING;
TIP_ADDRESS_STRING = record
  Str: array[0..15] of AnsiChar;
end;
type PIP_MASK_STRING = ^TIP_MASK_STRING;
TIP_MASK_STRING = TIP_ADDRESS_STRING;
type PIP_ADDR_STRING = ^TIP_ADDR_STRING;
TIP_ADDR_STRING = record
  Next: PIP_ADDR_STRING;
  IpAddress: TIP_ADDRESS_STRING;
  IpMask: TIP_MASK_STRING;
  Context: DWORD;
end;
type PIP_ADAPTER_INFO = ^TIP_ADAPTER_INFO;
TIP_ADAPTER_INFO = record
  Next: PIP_ADAPTER_INFO;
  ComboIndex: DWORD;
  AdapterName: array[0..MAX_ADAPTER_NAME_LENGTH + 3] of AnsiChar;
  Description: array[0..MAX_ADAPTER_DESCRIPTION_LENGTH + 3] of AnsiChar;
  AddressLength: UINT;
  Address: array[0..MAX_ADAPTER_ADDRESS_LENGTH - 1] of BYTE;
  Index: DWORD;
  _Type: UINT;
  DhcpEnabled: UINT;
  CurrentIpAddress: PIP_ADDR_STRING;
  IpAddressList: TIP_ADDR_STRING;
  GatewayList: TIP_ADDR_STRING;
  DhcpServer: TIP_ADDR_STRING;
  HaveWins: BOOL;
  PrimaryWinsServer: TIP_ADDR_STRING;
  SecondaryWinsServer: TIP_ADDR_STRING;
  LeaseObtained: Int32;
  LeaseExpires: Int32;
end;
function GetAdaptersInfo(
  AdapterInfo: PIP_ADAPTER_INFO;
  SizePointer: PULONG
): ULONG; call_decl; external 'iphlpapi' name 'GetAdaptersInfo';
function UNetWSAStartup(
  VersionRequired: word;
  var WSData: TUNetWSAData
): Longint; call_decl; external SockLib name 'WSAStartup';
function UNetWSACleanup: Longint; call_decl; external SockLib name 'WSACleanup';
{$else}
const UNET_SIOCGIFNAME = $8910;
const UNET_SIOCSIFLINK = $8911;
const UNET_SIOCGIFCONF = $8912;
const UNET_SIOCGIFFLAGS = $8913;
const UNET_SIOCSIFFLAGS = $8914;
const UNET_SIOCGIFADDR = $8915;
const UNET_SIOCSIFADDR = $8916;
const UNET_SIOCGIFDSTADDR = $8917;
const UNET_SIOCSIFDSTADDR = $8918;
const UNET_SIOCGIFBRDADDR = $8919;
const UNET_SIOCSIFBRDADDR = $891a;
const UNET_SIOCGIFNETMASK = $891b;
const UNET_SIOCSIFNETMASK = $891c;
const UNET_SIOCGIFMETRIC = $891d;
const UNET_SIOCSIFMETRIC = $891e;
const UNET_SIOCGIFMEM = $891f;
const UNET_SIOCSIFMEM = $8920;
const UNET_SIOCGIFMTU = $8921;
const UNET_SIOCSIFMTU = $8922;
const UNET_SIOCSIFNAME = $8923;
const UNET_SIOCSIFHWADDR = $8924;
const UNET_SIOCGIFENCAP = $8925;
const UNET_SIOCSIFENCAP = $8926;
const UNET_SIOCGIFHWADDR = $8927;
const UNET_SIOCGIFSLAVE = $8929;
const UNET_SIOCSIFSLAVE = $8930;
const UNET_SIOCADDMULTI = $8931;
const UNET_SIOCDELMULTI = $8932;
const UNET_SIOCGIFINDEX = $8933;
const UNET_SIOGIFINDEX = UNET_SIOCGIFINDEX;
const UNET_SIOCSIFPFLAGS = $8934;
const UNET_SIOCGIFPFLAGS = $8935;
const UNET_SIOCDIFADDR = $8936;
const UNET_SIOCSIFHWBROADCAST = $8937;
const UNET_SIOCGIFCOUNT = $8938;
const UNET_SIOCGIFBR = $8940;
const UNET_SIOCSIFBR = $8941;
const UNET_SIOCGIFTXQLEN = $8942;
const UNET_SIOCSIFTXQLEN = $8943;
const UNET_SIOCETHTOOL = $8946;
const UNET_SIOCGMIIPHY = $8947;
const UNET_SIOCGMIIREG = $8948;
const UNET_SIOCSMIIREG = $8949;
const UNET_SIOCWANDEV = $894A;

const UNET_IFF_UP = $1;
const UNET_IFF_BROADCAST = $2;
const UNET_IFF_DEBUG = $4;
const UNET_IFF_LOOPBACK = $8;
const UNET_IFF_POINTOPOINT = $10;
const UNET_IFF_NOTRAILERS = $20;
const UNET_IFF_RUNNING = $40;
const UNET_IFF_NOARP = $80;
const UNET_IFF_PROMISC = $100;
const UNET_IFF_ALLMULTI = $200;
const UNET_IFF_MASTER = $400;
const UNET_IFF_SLAVE = $800;
const UNET_IFF_MULTICAST = $1000;
const UNET_IFF_PORTSEL = $2000;
const UNET_IFF_AUTOMEDIA = $4000;
const UNET_IFF_DYNAMIC = $8000;

function UNetGetIfAddrs(
  const IfAddrs: PPUIfAddrs
): Int32; call_decl; external SockLib name 'getifaddrs';
procedure FreeIfAddrs(
  const IfAddrs: PUIfAddrs
); call_decl; external SockLib name 'freeifaddrs';
function UNetIOCtl(
  Handle: Int32;
  Ndx: UInt32;
  Data: Pointer
): Int32; call_decl; external name 'ioctl';
{$endif}

function UNetGetAddrInfo(
  const Node: PAnsiChar;
  const Service: PAnsiChar;
  const Hints: PUAddrInfo;
  const Results: PPUAddrInfo
): Int32; call_decl; external SockLib name 'getaddrinfo';
procedure UNetFreeAddrInfo(
  const AddrInfo: PUAddrInfo
); call_decl; external SockLib name 'freeaddrinfo';
function UNetGetHostName(
  const Name: PAnsiChar;
  const Len: Int32
): Int32; call_decl; external SockLib name 'gethostname';
function UNetGetHostByName(
  const Name: PAnsiChar
): PUHostEnt; call_decl; external SockLib name 'gethostbyname';

function UNetSocket(
  Domain: Int32; SockType: Int32; Protocol: Int32
): Int32; call_decl; external SockLib name 'socket';
function UNetRecv(
  Sock: Int32; Buf: Pointer; Len: UInt32; Flags: Int32
): Int32; call_decl; external SockLib name 'recv';
function UNetRecvFrom(
  Sock: Int32; Buf: Pointer; Len: UInt32; Flags: Int32;
  FromAddr: PUSockAddr; FromLen: PUSockLen
): Int32; call_decl; external SockLib name 'recvfrom';
function UNetSend(
  Sock: Int32; Msg: Pointer; Len: UInt32; Flags: Int32
): Int32; call_decl; external SockLib name 'send';
function UNetSendTo(
  Sock: Int32; Msg: Pointer; Len: UInt32; Flags: Int32;
  ToAddr: PUSockAddr; ToLen: TUSockLen
): Int32; call_decl; external SockLib name 'sendto';
function UNetBind(
  Sock: Int32; Addr: PUSockAddr; AddrLen: TUSockLen
): Int32; call_decl; external SockLib name 'bind';
function UNetListen(
    Sock: Int32; Backlog: Int32
): Int32; call_decl; external SockLib name 'listen';
function UNetAccept(
    Sock: Int32; Addr: PUSockAddr; AddrLen: PUSockLen
): Int32; call_decl; external SockLib name 'accept';
function UNetConnect(
    Sock: Int32; Addr: PUSockAddr; AddrLen: TUSockLen
): Int32; call_decl; external SockLib name 'connect';
function UNetShutDown(
    Sock: Int32; How: Int32
): Int32; call_decl; external SockLib name 'shutdown';
function UNetGetSockName(
    Sock: Int32; Name: PUSockAddr; NameLen: PUSockLen
): Int32; call_decl; external SockLib name 'sockname';
function UNetGetPeerName(
    Sock: Int32; Name: PUSockAddr; NameLen: PUSockLen
): Int32; call_decl; external SockLib name 'peername';
function UNetGetSockOpt(
    Sock: Int32; Level: Int32; OptName: Int32;
    OptVal: Pointer; OptLen : PUSockLen
): Int32; call_decl; external SockLib name 'getsockopt';
function UNetSetSockOpt(
    Sock: Int32; Level: Int32; OptName: Int32;
    OptVal: Pointer; OptLen: TUSockLen
): Int32; call_decl; external SockLib name 'setsockopt';
function UNetSocketPair(
    Domain: Int32; SockType: Int32; Protocol: Int32; OutSock: PInt32
): Int32; call_decl; external SockLib name 'socketpair';
function USelect(
    nfds: Int32;
    ReadFDs, WriteFDs, ExceptFDs: PUFDSet;
    Timeout: PTimeVal
): Int32; call_decl; external SockLib name 'select';
function USelect(
    const ReadFDs, WriteFDs, ExceptFDs: array of TUSocket;
    const Timeout: PTimeVal
): Int32;
{$if defined(windows)}
function UNetIOCtl(
    Sock: Int32;
    Cmd: UInt32;
    Arg: PUInt32
): Int32; call_decl; external SockLib name 'ioctlsocket';
function UNetClose(
    Sock: Int32
): Int32; call_decl; external SockLib name 'closesocket';
{$else}
function UNetClose(
    Sock: Int32
): Int32; call_decl; external SockLib name 'close';
{$endif}

function UNetHostName: String;
function UNetLocalAddr: TUInAddr;
function UNetLocalMacAddr: TUMacAddr;
function UNetWakeOnLan(const MacAddr: TUMacAddr): Boolean;
function UNetPing(const InAddrN: TUInAddr): Boolean;

function HToNl(const Host: UInt32): UInt32; inline;
function NToHl(const Net: UInt32): UInt32; inline;
function HToNs(const Host: UInt16): UInt16; inline;
function NToHs(const Net: UInt16): UInt16; inline;

function UNetHostToNetLong(const Host: UInt32): UInt32; overload;
function UNetHostToNetShort(const Host: UInt16): UInt16; overload;
function UNetHostToNet(const Host: TUInAddr): TUInAddr; overload;
function UNetHostToNet(const Host: TUSockAddr): TUSockAddr; overload;
function UNetNetToHostLong(const Net: UInt32): UInt32; overload;
function UNetNetToHostShort(const Net: UInt16): UInt16; overload;
function UNetNetToHost(const Net: TUInAddr): TUInAddr; overload;
function UNetNetToHost(const Net: TUSockAddr): TUSockAddr; overload;

function UNetMacAddrToStr(const Addr: TUMacAddr): AnsiString;
function UNetStrToMacAddr(const AddrStr: AnsiString): TUMacAddr;
function UNetNetAddrToStr(const Addr: TUInAddr): AnsiString;
function UNetHostAddrToStr(const Addr: TUInAddr): AnsiString;
function UNetStrToHostAddr(const AddrStr: AnsiString): TUInAddr;
function UNetStrToNetAddr(const AddrStr: AnsiString): TUInAddr;
function UNetTryStrToNetAddr(const AddrStr: AnsiString; out OutAddr: TUInAddr): Boolean;

function UNetHostAddrToStr6(const Addr: TUInAddr6): AnsiString;
function UNetStrToHostAddr6(const AddrStr: AnsiString): TUInAddr6;
function UNetNetAddrToStr6(const Addr: TUInAddr6): AnsiString;
function UNetStrToNetAddr6(const AddrStr: AnsiString): TUInAddr6;
function UNetTryStrToNetAddr6(const AddrStr: AnsiString; out OutAddr: TUInAddr6): Boolean;

procedure UNetFDZero(out FDSet: TUFDSet);
procedure UNetFDClr(var FDSet: TUFDSet; const FileDesc: TUSocket);
function UNetFDIsSet(const FDSet: TUFDSet; const FileDesc: TUSocket): Boolean;
procedure UNetFDSet(var FDSet: TUFDSet; const FileDesc: TUSocket);

implementation

function TUMacAddrImpl.IsValid: Boolean;
  var i: Int32;
begin
  for i := 0 to High(Self) do
  if Self[i] <> 0 then Exit(True);
  Result := False;
end;

function TUSocketImpl.Make(
  const SockDomain: Int32;
  const SockType: Int32;
  const SockProtocol: Int32
): TUSocket;
begin
  Self := UNetSocket(SockDomain, SockType, SockProtocol);
  Result := Self;
end;

function TUSocketImpl.MakeTCP(
  const SockDomain: Int32;
  const SockProtocol: Int32
): TUSocket;
begin
  Result := Make(SockDomain, SOCK_STREAM, SockProtocol);
end;

function TUSocketImpl.MakeUDP(
  const SockDomain: Int32;
  const SockProtocol: Int32
): TUSocket;
begin
  Result := Make(SockDomain, SOCK_DGRAM, SockProtocol);
end;

function TUSocketImpl.Bind(
  const Addr: PUSockAddr; const AddrLen: TUSockLen
): Int32;
begin
  Result := UNetBind(Self, Addr, AddrLen);
end;

function TUSocketImpl.Listen(const Backlog: Int32): Int32;
begin
  Result := UNetListen(Self, Backlog);
end;

function TUSocketImpl.Accept(
  const Addr: PUSockAddr;
  const AddrLen: PUSockLen
): Int32;
begin
  Result := UNetAccept(Self, Addr, AddrLen);
end;

function TUSocketImpl.Connect(
  const Addr: PUSockAddr;
  const AddrLen: TUSockLen
): Int32;
begin
  Result := UNetConnect(Self, Addr, AddrLen);
end;

function TUSocketImpl.Send(
  const Buffer: Pointer;
  const BufferLen: UInt32;
  const Flags: Int32
): Int32;
begin
  Result := UNetSend(Self, Buffer, BufferLen, Flags);
end;

function TUSocketImpl.Send(const Msg: AnsiString): Int32;
  var ShortMsg: array[0..2048] of AnsiChar;
begin
  if Length(Msg) < 2048 then
  begin
    Exit(UNetSend(Self, @Msg[1], Length(Msg) + 1, 0));
  end;
  Move(Msg[1], ShortMsg[0], 2047);
  ShortMsg[2047] := #0;
  Result := UNetSend(Self, @ShortMsg, SizeOf(ShortMsg), 0);
end;

function TUSocketImpl.Recv(
  const Buffer: Pointer;
  const BufferLen: UInt32;
  const Flags: Int32
): Int32;
begin
  Result := UNetRecv(Self, Buffer, BufferLen, Flags);
end;

function TUSocketImpl.Recv: String;
  var Buffer: array[0..2047] of AnsiChar;
begin
  UNetRecv(Self, @Buffer, SizeOf(Buffer), 0);
  Result := Buffer;
end;

function TUSocketImpl.SendTo(
  const Buffer: Pointer;
  const BufferLen: UInt32;
  const Flags: Int32;
  const ToAddr: PUSockAddr;
  const ToLen: TUSockLen
): Int32;
begin
  Result := UNetSendTo(Self, Buffer, BufferLen, Flags, ToAddr, ToLen);
end;

function TUSocketImpl.RecvFrom(
  const Buffer: Pointer;
  const BufferLen: UInt32;
  const Flags: Int32;
  const AddrFrom: PUSockAddr;
  const AddrLen: PUSockLen
): Int32;
begin
  Result := UNetRecvFrom(Self, Buffer, BufferLen, Flags, AddrFrom, AddrLen);
end;

function TUSocketImpl.GetSockOpt(
  const Level: Int32;
  const OptName: Int32;
  const OptVal: Pointer;
  const OptLen: PUSockLen
): Int32;
begin
  Result := UNetGetSockOpt(Self, Level, OptName, OptVal, OptLen);
end;

function TUSocketImpl.SetSockOpt(
  const Level: Int32;
  const OptName: Int32;
  const OptVal: Pointer;
  const OptLen: TUSockLen
): Int32;
begin
  Result := UNetSetSockOpt(Self, Level, OptName, OptVal, OptLen);
end;

function TUSocketImpl.SetSockOpt(
  const OptName: Int32;
  const OptVal: Int32): Int32;
begin
  Result := SetSockOpt(SOL_SOCKET, OptName, @OptVal, SizeOf(OptVal));
end;

function TUSocketImpl.SelectRead(const TimeoutMs: UInt32): Int32;
  var tv: TTimeVal;
begin
  if TimeoutMs = UInt32(-1) then Exit(USelect([Self], [], [], nil));
  tv.tv_sec := TimeoutMs div 1000;
  tv.tv_usec := (TimeoutMs mod 1000) * 1000;
  Result := USelect([Self], [], [], @tv);
end;

function TUSocketImpl.SelectWrite(const TimeoutMs: UInt32): Int32;
  var tv: TTimeVal;
begin
  if TimeoutMs = UInt32(-1) then Exit(USelect([], [Self], [], nil));
  tv.tv_sec := TimeoutMs div 1000;
  tv.tv_usec := (TimeoutMs mod 1000) * 1000;
  Result := USelect([Self], [], [], @tv);
end;

function TUSocketImpl.SetBlocking(const Blocking: Boolean): Int32;
  var Value: UInt32;
begin
  Value := UInt32(not Blocking);
  Result := UNetIOCtl(Self, FIONBIO, @Value);
end;

function TUSocketImpl.Shutdown(const How: Int32): Int32;
begin
  Result := UNetShutDown(Self, How);
end;

function TUSocketImpl.Close: Int32;
begin
  Result := UNetClose(Self);
  Self := -1;
end;

function TUSocketImpl.IsValid: Boolean;
begin
  Result := Self > -1;
end;

class function TUSocketImpl.Create(
  const SockDomain: Int32;
  const SockType: Int32;
  const SockProtocol: Int32
): TUSocket;
begin
  Result := TUSocket.Invalid;
  Result.Make(SockDomain, SockType, SockProtocol);
end;

class function TUSocketImpl.CreateTCP(
  const SockDomain: Int32;
  const SockProtocol: Int32
): TUSocket;
begin
  Result := TUSocket.Invalid;
  Result.MakeTCP(SockDomain, SockProtocol);
end;

class function TUSocketImpl.CreateUDP(
  const SockDomain: Int32;
  const SockProtocol: Int32
): TUSocket;
begin
  Result := TUSocket.Invalid;
  Result.MakeUDP(SockDomain, SockProtocol);
end;

procedure TUNet.TBeacon.SetEnabled(const Value: Boolean);
begin
  if _Enabled = Value then Exit;
  if Assigned(_Listener) then
  begin
    _Listener.Terminate;
    _Listener.WaitFor;
    FreeAndNil(_Listener);
  end;
  if Assigned(_Broadcaster) then
  begin
    _Broadcaster.Terminate;
    _Broadcaster.WaitFor;
    FreeAndNil(_Broadcaster);
  end;
  _Enabled := Value;
  _Peers := nil;
  if _Enabled then
  begin
    _MessageJson.Ptr['message'].Value := _Message;
    _Listener := TListener.Create(True);
    _Listener.Beacon := Self;
    _MessageJson.Ptr['type'].Value := 'bounce';
    _Listener.Message := _MessageJson.Ptr.AsString;
    _Listener.Start;
    if _Active then
    begin
      _Broadcaster := TBroadcaster.Create(True);
      _Broadcaster.Beacon := Self;
      _MessageJson.Ptr['type'].Value := 'sonar';
      _Broadcaster.Message := _MessageJson.Ptr.AsString;
      _Broadcaster.Start;
    end;
  end;
end;

procedure TUNet.TBeacon.SetActive(const Value: Boolean);
begin
  if _Active = Value then Exit;
  _Active := Value;
  Reset;
end;

procedure TUNet.TBeacon.SetPort(const Value: UInt16);
begin
  if _Port = Value then Exit;
  _Port := Value;
  Reset;
end;

procedure TUNet.TBeacon.SetMessage(const Value: String);
begin
  if _Message = Value then Exit;
  if not Enabled then
  begin
    _Message := Value;
    Exit;
  end;
  Enabled := False;
  _Message := Value;
  Enabled := True;
end;

procedure TUNet.TBeacon.AddPeer(const Addr: TUInAddr; const Message: String);
  var i: Int32;
  var Peer: TPeer;
begin
  _PeersLock.Enter;
  try
    for i := 0 to High(_Peers) do
    if _Peers[i].Addr.Addr32 = Addr.Addr32 then
    begin
      _Peers[i].TimeStamp := GetTickCount64;
      _Peers[i].Message := Message;
      Exit;
    end;
    ClearOldPeers;
    Peer.Addr := Addr;
    Peer.TimeStamp := GetTickCount64;
    Peer.Message := Message;
    specialize UArrAppend<TPeer>(_Peers, Peer);
  finally
    _PeersLock.Leave;
  end;
end;

function TUNet.TBeacon.GetPeers: TPeerArray;
  var i: Int32;
begin
  Result := nil;
  _PeersLock.Enter;
  try
    ClearOldPeers;
    if Length(_Peers) = 0 then Exit;
    SetLength(Result, Length(_Peers));
    for i := 0 to High(_Peers) do
    begin
      Result[i] := _Peers[i];
    end;
  finally
    _PeersLock.Leave;
  end;
end;

procedure TUNet.TBeacon.ClearOldPeers;
  var i: Int32;
  var t: UInt64;
begin
  t := GetTickCount64;
  for i := High(_Peers) downto 0 do
  if t - _Peers[i].TimeStamp > 20000 then
  begin
    specialize UArrDelete<TPeer>(_Peers, i);
  end;
end;

procedure TUNet.TBeacon.Reset;
begin
  if not Enabled then Exit;
  SetEnabled(False);
  SetEnabled(True);
end;

constructor TUNet.TBeacon.Create;
begin
  _Enabled := False;
  _Active := False;
  _Port := 57210;
  _LocalAddr := UNetLocalAddr;
  _HostName := UNetHostName;
  _BroadcastInterval := 5000;
  _Message := '';
  _MessageJson := TUJson.Make;
  _MessageJson.Ptr.AddValue('id', _HostName);
  _MessageJson.Ptr.AddValue('type', '');
  _MessageJson.Ptr.AddValue('addr', UNetNetAddrToStr(_LocalAddr));
  _MessageJson.Ptr.AddValue('message', _Message);
end;

destructor TUNet.TBeacon.Destroy;
begin
  SetEnabled(False);
  inherited Destroy;
end;

procedure TUNet.TBeacon.TListener.Execute;
  var SockAddr, OtherAddr: TUSockAddr;
  var OtherAddrLen: TUSockLen;
  var Buffer: array[0..2047] of AnsiChar;
  var n: Int32;
  var Msg: String;
  var Json: TUJsonRef;
begin
  _Sock := TUSocket.CreateUDP();
  SockAddr := TUSockAddr.Default;
  SockAddr.sin_port := UNetHostToNetShort(Beacon.Port);
  _Sock.Bind(@SockAddr, SizeOf(SockAddr));
  OtherAddr := TUSockAddr.Default;
  OtherAddrLen := SizeOf(OtherAddr);
  while not Terminated do
  begin
    n := _Sock.RecvFrom(@Buffer, SizeOf(Buffer), 0, @OtherAddr, @OtherAddrLen);
    if n <= 0 then Break;
    if OtherAddr.sin_addr.Addr32 = Beacon.LocalAddr.Addr32 then Continue;
    Msg := Buffer;
    Json := TUJson.Load(Msg);
    if not Json.IsValid then Continue;
    Beacon.AddPeer(OtherAddr.sin_addr, Json.Ptr['message'].Value);
    if not (Json.Ptr.Content['type'].Value = 'sonar') then Continue;
    OtherAddr.sin_port := UNetHostToNetShort(Beacon.Port);
    n := _Sock.SendTo(
      @Message[1], Length(Message) + 1, 0,
      @OtherAddr, SizeOf(OtherAddr)
    );
  end;
  if _Sock.IsValid then
  begin
    _Sock.Shutdown();
    _Sock.Close;
  end;
end;

procedure TUNet.TBeacon.TListener.TerminatedSet;
begin
  _Sock.Shutdown();
  _Sock.Close;
  inherited TerminatedSet;
end;

procedure TUNet.TBeacon.TBroadcaster.Execute;
  var LocalAddr: TUInAddr;
  var SockAddr: TUSockAddr;
begin
  LocalAddr := Beacon.LocalAddr;
  _Sock := TUSocket.CreateUDP();
  _Sock.SetSockOpt(SO_BROADCAST, 1);
  SockAddr := TUSockAddr.Default;
  SockAddr.sin_addr := LocalAddr;
  SockAddr.sin_port := UNetHostToNetShort(Beacon.Port);
  _Event.Unsignal;
  while not Terminated do
  begin
    SockAddr.sin_addr.Addr8[3] := 255;
    if SockAddr.sin_addr.Addr32 = LocalAddr.Addr32 then Continue;
    _Sock.SendTo(
      @Message[1], Length(Message) + 1, 0,
      @SockAddr, SizeOf(SockAddr)
    );
    _Event.WaitFor(Beacon.BroadcastInterval);
  end;
end;

procedure TUNet.TBeacon.TBroadcaster.TerminatedSet;
begin
  _Event.Signal;
  inherited TerminatedSet;
end;

function TUNet.TBroker.QueryClient(const DomainId: TGuid; const ClientId: TGuid): TClient;
  var i: Int32;
  var Domain: TDomain;
  var Client: TClient;
begin
  Result := nil;
  Domain := nil;
  for i := 0 to High(_Domains) do
  if IsEqualGUID(_Domains[i].Id, DomainId) then
  begin
    Domain := _Domains[i];
    Break;
  end;
  if not Assigned(Domain) then
  begin
    Domain := TDomain.Create;
    specialize UArrAppend<TDomain>(_Domains, Domain);
  end;
  for Client in Domain.Clients do
  if IsEqualGuid(Client.Id, ClientId) then
  begin
    Result := Client;
    Break;
  end;
  if not Assigned(Result) then
  begin
    Result := TClient.Create;
    Result.Id := ClientId;
    specialize UArrAppend<TClient>(Domain.Clients, Result);
  end;
end;

procedure TUNet.TBroker.Connection(
  const ClientAddr: TUInAddr;
  const ClientPort: UInt16;
  const Message: TSessionMessage
);
  var Client: TClient;
begin
  if Message.Marker <> _Marker then Exit;
  _CS.Enter;
  try
    Client := QueryClient(Message.Domain, Message.Id);
    Client.AddrLocal := Message.LocalAddr;
    Client.Addr := ClientAddr;
    Client.Port := ClientPort;
    Client.TimeStamp := Now;
    Client.Visible := Message.Visible;
    Client.SeekId := Message.SeekId;
    _Updater.Resume;
  finally
    _CS.Leave;
  end;
end;

procedure TUNet.TBroker.Update;
  var Domain: TDomain;
  var Client, Seek: TClient;
  var PeerMessage: TPeerMessage;
  var SockAddr: TUSockAddr;
begin
  _CS.Enter;
  try
    PurgeStaleConnections;
    for Domain in _Domains do
    begin
      for Client in Domain.Clients do
      begin
        if IsEqualGUID(Client.SeekId, GUID_NULL) then Continue;
        Seek := Domain.FindClient(Client.SeekId);
        if not Assigned(Seek) then Continue;
        if not Seek.Visible then Continue;
        PeerMessage := Client.MakePeerMessage;
        SockAddr := Seek.MakeSockAddr;
        Sock.SendTo(@PeerMessage, SizeOf(PeerMessage), 0, @SockAddr, SizeOf(SockAddr));
        PeerMessage := Seek.MakePeerMessage;
        SockAddr := Client.MakeSockAddr;
        Sock.SendTo(@PeerMessage, SizeOf(PeerMessage), 0, @SockAddr, SizeOf(SockAddr));
        Client.SeekId := GUID_NULL;
      end;
    end;
  finally
    _CS.Leave;
  end;
end;

procedure TUNet.TBroker.PurgeStaleConnections(const ThresholdSec: Int32);
  var i, j: Int32;
  var CurTime: TDateTime;
begin
  CurTime := Now;
  for i := High(_Domains) downto 0 do
  begin
    for j := High(_Domains[i].Clients) downto 0 do
    begin
      if SecondsBetween(_Domains[i].Clients[j].TimeStamp, CurTime) < ThresholdSec then Continue;
      FreeAndNil(_Domains[i].Clients[j]);
      specialize UArrDelete<TClient>(_Domains[i].Clients, j);
    end;
    if Length(_Domains[i].Clients) > 0 then Continue;
    FreeAndNil(_Domains[i]);
    specialize UArrDelete<TDomain>(_Domains, i);
  end;
end;

procedure TUNet.TBroker.SetEnabled(const Value: Boolean);
  var SockAddr: TUSockAddr;
  var n: TUSockLen;
begin
  if _Enabled = Value then Exit;
  _Enabled := Value;
  if _Enabled then
  begin
    Sock := TUSocket.CreateUDP();
    SockAddr := TUSockAddr.Default;
    SockAddr.sin_port := UNetHostToNetShort(_Port);
    n := SizeOf(SockAddr);
    if Sock.Bind(@SockAddr, n) <> 0 then
    begin
      WriteLn('Bind socket failed');
      _Enabled := False;
      Sock.Close;
      Exit;
    end;
    _Listener := TListener.Create(True);
    _Listener.Broker := Self;
    _Listener.Start;
    _Updater := TUpdater.Create(True);
    _Updater.Broker := Self;
    _Updater.Start;
  end
  else
  begin
    Sock.Shutdown();
    Sock.Close;
    _Updater.Terminate;
    _Updater.WaitFor;
    _Listener.Terminate;
    _Listener.WaitFor;
  end;
end;

constructor TUNet.TBroker.Create;
begin
  _Enabled := False;
  _Marker := DefaultMarker;
  _Port := 61380;
end;

destructor TUNet.TBroker.Destroy;
begin
  Enabled := False;
  specialize UArrClear<TDomain>(_Domains);
  inherited Destroy;
end;

procedure TUNet.TBroker.TListener.Execute;
  var OtherAddr: TUSockAddr;
  var n, r: Int32;
  var Message: TSessionMessage;
begin
  if not Broker.Sock.IsValid then Exit;
  while not Terminated do
  begin
    n := SizeOf(OtherAddr);
    r := Broker.Sock.RecvFrom(@Message, SizeOf(Message), 0, @OtherAddr, @n);
    if r <> SizeOf(Message) then Continue;
    WriteLn('Connection: ', UNetNetAddrToStr(OtherAddr.sin_addr), ':', IntToStr(ntohs(OtherAddr.sin_port)));
    Broker.Connection(OtherAddr.sin_addr, OtherAddr.sin_port, Message);
  end;
end;

procedure TUNet.TBroker.TListener.TerminatedSet;
begin
  inherited TerminatedSet;
end;

procedure TUNet.TBroker.TUpdater.Resume;
begin
  _Event.Signal;
end;

procedure TUNet.TBroker.TUpdater.Execute;
begin
  while not Terminated do
  begin
    _Event.Unsignal;
    _Event.WaitFor(5000);
    Broker.Update;
  end;
end;

procedure TUNet.TBroker.TUpdater.TerminatedSet;
begin
  inherited TerminatedSet;
end;

function TUNet.TBroker.TClient.MakePeerMessage: TPeerMessage;
begin
  Result.PeerId := Id;
  Result.Addr := Addr;
  Result.Port := Port;
end;

function TUNet.TBroker.TClient.MakeSockAddr: TUSockAddr;
begin
  Result := TUSockAddr.Default;
  Result.sin_addr := Addr;
  Result.sin_port := Port;
end;

function TUNet.TBroker.TDomain.FindClient(const ClientId: TGuid): TClient;
  var Client: TClient;
begin
  for Client in Clients do
  if IsEqualGUID(Client.Id, ClientId) then Exit(Client);
  Result := nil;
end;

destructor TUNet.TBroker.TDomain.Destroy;
begin
  specialize UArrClear<TClient>(Clients);
  inherited Destroy;
end;

function USelect(
  const ReadFDs, WriteFDs, ExceptFDs: array of TUSocket;
  const Timeout: PTimeVal
): Int32;
  var MaxFD: Int32;
  function MakeSet(const FileDesc: array of TUSocket): TUFDSet;
    var i: Int32;
  begin
    UNetFDZero(Result);
    if Length(FileDesc) = 0 then Exit;
    for i := 0 to High(FileDesc) do
    begin
      UNetFDSet(Result, FileDesc[i]);
      if FileDesc[i] > MaxFD then MaxFD := FileDesc[i];
    end;
  end;
  var SetRead, SetWrite, SetExcept: TUFDSet;
  var PtrRead, PtrWrite, PtrExcept: PUFDSet;
begin
  MaxFD := 0;
  PtrRead := nil;
  if Length(ReadFDs) > 0 then
  begin
    SetRead := MakeSet(ReadFDs);
    PtrRead := @SetRead;
  end;
  PtrWrite := nil;
  if Length(WriteFDs) > 0 then
  begin
    SetWrite := MakeSet(WriteFDs);
    PtrWrite := @SetWrite;
  end;
  PtrExcept := nil;
  if Length(ExceptFDs) > 0 then
  begin
    SetExcept := MakeSet(ExceptFDs);
    PtrExcept := @SetExcept;
  end;
  Result := USelect(MaxFD + 1, @SetRead, nil, nil, Timeout);
  if Result = -1 then Result := WSAGetLastError;
end;

function UNetHostName: String;
  var Buffer: array[0..255] of AnsiChar;
begin
  UNetGetHostName(@Buffer, SizeOf(Buffer));
  Result := Buffer;
end;

function UNetLocalAddr: TUInAddr;
{$if defined(windows)}
  type TInAddrArr = array[UInt32] of TUInAddr;
  type PInAddrArr = ^TInAddrArr;
  var AddrArr: PInAddrArr;
  var Host: PUHostEnt;
  var i: Int32;
{$else}
  var r: Int32;
  var IfAddrs, a: PUIfAddrs;
{$endif}
  var Addr: TUInAddr;
begin
  Result := TUInAddr.Zero;
{$if defined(windows)}
  Host := UNetGetHostByName(PAnsiChar(UNetHostName));
  if not Assigned(Host) then Exit;
  AddrArr := PInAddrArr(Host^.h_Addr_List^);
  i := 0;
  while AddrArr^[i].Addr32 <> 0 do
  try
    Addr := PUInAddr(@AddrArr^[i].Addr32)^;
    if (Result.Addr32 = 0)
    or (Addr.Addr8[0] = 192) then
    begin
      Result := Addr;
    end;
  finally
    Inc(i);
  end;
{$else}
  r := UNetGetIfAddrs(@IfAddrs);
  if r <> 0 then Exit;
  try
    a := IfAddrs;
    while Assigned(a) do
    try
      if not Assigned(a^.ifa_addr) then Continue;
      if a^.ifa_addr^.sin_family <> AF_INET then Continue;
      Addr := a^.ifa_addr^.sin_addr;
      if (Result.Addr32 = 0)
      or (Addr.Addr8[0] = 192) then
      begin
        Result := Addr;
      end;
    finally
      a := a^.ifa_next;
    end;
  finally
    FreeIfAddrs(IfAddrs);
  end;
{$endif}
end;

function UNetLocalMacAddr: TUMacAddr;
{$if defined(windows)}
  function ViaAdapters: TUMacAddrArray;
    var Adapters, Adapter: PIP_ADAPTER_INFO;
    var Buffer: array[0..1024 * 16 - 1] of UInt8;
    var BufSize: UInt32;
    var i: Int32;
    var Addr: TUMacAddr;
  begin
    Result := nil;
    BufSize := SizeOf(Buffer);
    Adapters := PIP_ADAPTER_INFO(@Buffer);
    if GetAdaptersInfo(Adapters, @BufSize) <> 0 then Exit;
    Adapter := Adapters;
    while Assigned(Adapter) do
    try
      //WriteLn(Adapter^.AdapterName);
      //WriteLn(Adapter^.Description);
      if Adapter^.AddressLength <> 6 then Continue;
      Move(Adapter^.Address, Addr, SizeOf(TUMacAddr));
      specialize UArrAppend<TUMacAddr>(Result, Addr);
      //WriteLn(UNetMacAddrToStr(Addr));
    finally
      Adapter := Adapter^.Next;
    end;
  end;
{$else}
  function ViaSysCall: TUMacAddrArray;
    var Sock: TUSocket;
    var Conf: TUIfConf;
    var ReqList: PUIfReq;
    var Req: TUIfReq;
    var Buffer: array[0..1023] of UInt8;
    var r, n, i: Int32;
    var Addr: TUMacAddr;
  begin
    Result := nil;
    Sock := TUSocket.CreateUDP(AF_INET);
    if (not Sock.IsValid) then Exit;
    try
      UClear(Conf, SizeOf(Conf));
      Conf.ifc_len := SizeOf(Buffer);
      Conf.ifcu_buf := @Buffer;
      r := UNetIOCtl(Sock, UNET_SIOCGIFCONF, @Conf);
      if r <> 0 then Exit;
      i := SizeOf(TUIfReq);
      n := Conf.ifc_len div i;
      ReqList := Conf.ifcu_req;
      for i := 0 to n - 1 do
      begin
        //WriteLn(ReqList^.ifrn_name);
        try
          UClear(Req, SizeOf(Req));
          Req.ifrn_name := ReqList^.ifrn_name;
          if not (UNetIOCtl(Sock, UNET_SIOCGIFFLAGS, @Req) = 0) then Continue;
          if (Req.ifru_flags and UNET_IFF_LOOPBACK) > 0 then Continue;
          if UNetIOCtl(Sock, UNET_SIOCGIFHWADDR, @Req) <> 0 then Continue;
          Addr := PUMacAddr(@Req.ifru_hwaddr.sin_port)^;
          specialize UArrAppend<TUMacAddr>(Result, Addr);
        finally
          Inc(ReqList);
        end;
      end;
    finally
      Sock.Close;
    end;
  end;
  function ViaIfAddrs: TUMacAddrArray;
    var IfAddrs, a: PUIfAddrs;
    var r: Int32;
    var Addr: TUMacAddr;
  begin
    Result := nil;
    r := UNetGetIfAddrs(@IfAddrs);
    if r <> 0 then Exit;
    try
      a := IfAddrs;
      while Assigned(a) do
      try
        if not Assigned(a^.ifa_addr) then Continue;
        if (a^.ifa_flags and UNET_IFF_LOOPBACK) > 0 then Continue;
        if a^.ifa_addr^.sin_family <> AF_PACKET then Continue;
        if a^.ifa_addr^.sin_zero[3] <> 6 then Continue;
        //Write('Name = ', a^.ifa_name, ' Family = ', a^.ifa_addr^.sin_family, ' Flags = ', a^.ifa_flags, ' Addr = ');
        Addr := PUMacAddr(@a^.ifa_addr^.sin_zero[4])^;
        specialize UArrAppend<TUMacAddr>(Result, Addr);
        //WriteLn(UNetMacAddrToStr(Addr));
      finally
        a := a^.ifa_next;
      end;
    finally
      FreeIfAddrs(IfAddrs);
    end;
  end;
{$endif}
  var Addr: TUMacAddrArray;
begin
{$if defined(windows)}
  Addr := ViaAdapters;
  if Length(Addr) > 0 then Exit(Addr[0]);
{$else}
  Addr := ViaSysCall;
  if Length(Addr) > 0 then Exit(Addr[0]);
  Addr := ViaIfAddrs;
  if Length(Addr) > 0 then Exit(Addr[High(Addr)]);
{$endif}
  Result := TUMacAddr.Zero;
end;

function UNetWakeOnLan(const MacAddr: TUMacAddr): Boolean;
  var Sock: TUSocket;
  var Msg: packed record
    Sync: array[0..5] of UInt8;
    MacAddr: array[0..15] of TUMacAddr;
  end;
  var Addr: TUSockAddr;
  var i, BytesSent: Int32;
begin
  for i := 0 to High(Msg.Sync) do Msg.Sync[i] := $ff;
  for i := 0 to High(Msg.MacAddr) do Msg.MacAddr[i] := MacAddr;
  Sock := TUSocket.CreateUDP();
  if not Sock.IsValid then Exit(False);
  if Sock.SetSockOpt(SO_BROADCAST, 1) < 0 then Exit(False);
  try
    Addr := TUSockAddr.Default;
    Addr.sin_addr := TUInAddr.Broadcast;
    Addr.sin_port := UNetHostToNetShort(7);
    for i := 0 to 4 do
    begin
      BytesSent := Sock.SendTo(@Msg, SizeOf(Msg), 0, @Addr, SizeOf(Addr));
      if BytesSent <= 0 then Exit(False);
    end;
  finally
    Sock.Close;
  end;
  Result := True;
end;

function UNetPing(const InAddrN: TUInAddr): Boolean;
  type TICMPHeader = packed record
    RequestType: UInt8;
    Code: UInt8;
    Checksum: UInt16;
    Identifier: UInt16;
    SequenceNumber: UInt16;
  end;
  type PICMPHeader = ^TICMPHeader;
  function Checksum(const Buffer: Pointer; const BufferSize: UInt16): UInt16;
    var i: Int32;
  begin
    if BufferSize mod 2 = 1 then Exit(0);
    Result := 0;
    for i := 0 to (BufferSize shr 1) - 1 do
    begin
      Result += PUInt16Arr(Buffer)^[i];
    end;
    Result := (Result shr 16) + (Result and $ffff);
    Result += Result shr 16;
    Result := not Result;
  end;
  var Sock: TUSocket;
  var Addr: TUSockAddr;
  var Request: TICMPHeader;
  var Response: PICMPHeader;
  var r: Int32;
  var TimeOut: TTimeVal;
  var ResponseBuffer: array[0..255] of UInt8;
  var SockLen: TUSockLen;
  var ICMPOffset: Int32;
begin
  Sock := TUSocket.Create(AF_INET, SOCK_RAW, UNET_IPPROTO_ICMP);
  if Sock = INVALID_SOCKET then Exit(False);
  try
    Addr := TUSockAddr.Default;
    Addr.sin_addr := InAddrN;
    UClear(Request, SizeOf(Request));
    Request.RequestType := ICMP_ECHO;
    Request.Identifier := GetCurrentProcessId mod 32768;
    Request.SequenceNumber := 1;
    Request.Checksum := Checksum(@Request, SizeOf(Request));
    r := Sock.SendTo(@Request, SizeOf(Request), 0, @Addr, SizeOf(Addr));
    if r = SOCKET_ERROR then Exit(False);
    TimeOut.tv_sec := 1;
    TimeOut.tv_usec := 0;
    Sock.SetSockOpt(SOL_SOCKET, SO_RCVTIMEO, @TimeOut, SizeOf(TimeOut));
    SockLen := SizeOf(Addr);
    r := Sock.RecvFrom(@ResponseBuffer, SizeOf(ResponseBuffer), 0, @Addr, @SockLen);
    if r = SOCKET_ERROR then Exit(False);
    ICMPOffset := (ResponseBuffer[0] and $f) * 4;
    Response := PICMPHeader(@ResponseBuffer[ICMPOffset]);
    if Response^.RequestType <> 0 then Exit(False);
    if Response^.Identifier <> Request.Identifier then Exit(False);
    Result := True;
  finally
    Sock.Close;
  end;
end;

function HToNl(const Host: UInt32): UInt32;
begin
{$if defined(ENDIAN_BIG)}
  Result := Host;
{$else}
  Result := SwapEndian(Host);
{$endif}
end;

function NToHl(const Net: UInt32): UInt32;
begin
{$if defined(ENDIAN_BIG)}
  Result := Net;
{$else}
  Result := SwapEndian(Net);
{$endif}
end;

function HToNs(const Host: UInt16): UInt16;
begin
{$if defined(ENDIAN_BIG)}
  Result := Host;
{$else}
  Result := SwapEndian(Host);
{$endif}
end;

function NToHs(const Net: UInt16): UInt16;
begin
{$if defined(ENDIAN_BIG)}
  Result := Net;
{$else}
  Result := SwapEndian(Net);
{$endif}
end;

function UNetHostToNetLong(const Host: UInt32): UInt32;
begin
  Result := HToNl(Host);
end;

function UNetHostToNetShort(const Host: UInt16): UInt16;
begin
  Result := HToNs(Host);
end;

function UNetHostToNet(const Host: TUInAddr): TUInAddr;
begin
  Result.Addr32 := HToNl(Host.Addr32);
end;

function UNetHostToNet(const Host: TUSockAddr): TUSockAddr;
begin
  Result.sin_family := Host.sin_family;
  Result.sin_port := UNetHostToNetShort(Host.sin_port);
  Result.sin_addr := UNetHostToNet(Host.sin_addr);
  Result.sin_zero := Host.sin_zero;
end;

function UNetNetToHostLong(const Net: UInt32): UInt32;
begin
  Result := NToHl(Net);
end;

function UNetNetToHostShort(const Net: UInt16): UInt16;
begin
  Result := NToHs(Net);
end;

function UNetNetToHost(const Net: TUInAddr): TUInAddr;
begin
  Result.Addr32 := NToHl(Net.Addr32);
end;

function UNetNetToHost(const Net: TUSockAddr): TUSockAddr;
begin
  Result.sin_family := Net.sin_family;
  Result.sin_port := UNetNetToHostShort(Net.sin_port);
  Result.sin_addr := UNetNetToHost(Net.sin_addr);
  Result.sin_zero := Net.sin_zero;
end;

function UNetMacAddrToStr(const Addr: TUMacAddr): AnsiString;
begin
  Result := LowerCase(Format(
    '%0:.2X:%1:.2X:%2:.2X:%3:.2X:%4:.2X:%5:.2X',
    [Addr[0], Addr[1], Addr[2], Addr[3], Addr[4], Addr[5]]
  ));
end;

function UNetStrToMacAddr(const AddrStr: AnsiString): TUMacAddr;
  var i, j, n, r: Int32;
  var Hex: String;
begin
  Result := TUMacAddr.Zero;
  r := 0;
  j := 1;
  for i := 1 to Length(AddrStr) do
  if (AddrStr[i] in [':', '-']) or (i = Length(AddrStr)) then
  begin
    if i = j then Exit(TUMacAddr.Zero);
    if i = Length(AddrStr) then n := i + 1 else n := i;
    Hex := '$' + UStrSubStr(AddrStr, j, n - j);
    Result[r] := StrToIntDef(Hex, 0);
    if r = 5 then Exit;
    j := n + 1;
    Inc(r);
  end;
  Result := TUMacAddr.Zero;
end;

function UNetNetAddrToStr(const Addr: TUInAddr): AnsiString;
  var i: Int32;
begin
  Result := IntToStr(Addr.Addr8[0]);
  for i := 1 to 3 do
  begin
    Result += '.' + IntToStr(Addr.Addr8[i]);
  end;
end;

function UNetHostAddrToStr(const Addr: TUInAddr): AnsiString;
begin
  Result := UNetNetAddrToStr(UNetHostToNet(Addr));
end;

function UNetStrToHostAddr(const AddrStr: AnsiString): TUInAddr;
  var Addr: TUInAddr;
begin
  if not UNetTryStrToNetAddr(AddrStr, Addr) then Exit(TUInAddr.Zero);
  Result := UNetNetToHost(Addr);
end;

function UNetStrToNetAddr(const AddrStr: AnsiString): TUInAddr;
  var Addr: TUInAddr;
begin
  if not UNetTryStrToNetAddr(AddrStr, Addr) then Exit(TUInAddr.Zero);
  Result := Addr;
end;

function UNetTryStrToNetAddr(const AddrStr: AnsiString; out OutAddr: TUInAddr): Boolean;
  function ReadNum(var Pos: Int32; out Num: UInt8): Boolean;
    var i, d: Int32;
    var n: UInt32;
  begin
    Result := False;
    d := 1;
    n := 0;
    try
      while Pos >= 1 do
      begin
        i := Pos; Dec(Pos);
        if AddrStr[i] = '.' then Exit(True);
        if not Ord(AddrStr[i]) in [Ord('0')..Ord('9')] then Exit(False);
        n += (Ord(AddrStr[i]) - Ord('0')) * d;
        d := d * 10;
      end;
      Result := True;
    finally
      if Result then Num := n;
    end;
  end;
  var i, n: Int32;
  var Addr: TUInAddr;
begin
  if Length(AddrStr) < 7 then Exit(False);
  i := Length(AddrStr);
  for n := 3 downto 0 do
  begin
    if not ReadNum(i, Addr.Addr8[n]) then Exit(False);
  end;
  OutAddr := Addr;
  Result := True;
end;

function UNetHostAddrToStr6(const Addr: TUInAddr6): AnsiString;
begin

end;

function UNetStrToHostAddr6(const AddrStr: AnsiString): TUInAddr6;
begin

end;

function UNetNetAddrToStr6(const Addr: TUInAddr6): AnsiString;
begin

end;

function UNetStrToNetAddr6(const AddrStr: AnsiString): TUInAddr6;
begin

end;

function UNetTryStrToNetAddr6(const AddrStr: AnsiString; out OutAddr: TUInAddr6): Boolean;
begin

end;

procedure UNetFDZero(out FDSet: TUFDSet);
begin
  UClear(FDSet, SizeOf(FDSet));
end;

{$if defined(windows)}
procedure UNetFDClr(var FDSet: TUFDSet; const FileDesc: TUSocket);
  var i, j: Int32;
begin
  for i := 0 to FDSet.fd_count - 1 do
  begin
    if FDSet.fd_array[i] <> FileDesc then Continue;
    for j := i to FDSet.fd_count - 2 do
    begin
      FDSet.fd_array[j] := FDSet.fd_array[j + 1];
    end;
    Dec(FDSet.fd_count);
    Break;
  end;
end;

function UNetFDIsSet(const FDSet: TUFDSet; const FileDesc: TUSocket): Boolean;
  var i: Int32;
begin
  for i := 0 to FDSet.fd_count - 1 do
  if FDSet.fd_array[i] = FileDesc then Exit(True);
  Result := False;
end;

procedure UNetFDSet(var FDSet: TUFDSet; const FileDesc: TUSocket);
  var i: Int32;
begin
  if FDSet.fd_count = FDSet.SETSIZE then Exit;
  i := FDSet.fd_count;
  FDSet.fd_array[i] := FileDesc;
  Inc(FDSet.fd_count);
end;
{$else}
function UNetFDClr(var FDSet: TUFDSet; const FileDesc: TUSocket): Int32;
  var i, j: Int32;
begin
  i := FileDesc;
  if (i < 0) or (i >= UFD_MAXFDSET) then Exit(-1);
  j := i shr 5;
  FDSet[j] := FDSet[j] and UInt32(not (UInt32(1) shl (i and (1 shl 5 - 1))));
  Result := 0;
end;

function UNetFDIsSet(const FDSet: TUFDSet; const FileDesc: TUSocket): Int32;
  var i: Int32;
begin
  i := FileDesc;
  if (i < 0) or (i >= UFD_MAXFDSET) then Exit(-1);
  Result := Int32(((FDSet[i shr 5]) and (UInt32(1) shl (i and (1 shl 5 - 1)))) > 0);
end;

//#define FD_SET(fd, set) ((set)->fds_bits[((fd) / (8 * sizeof(long)))] |= (1UL << ((fd) % (8 * sizeof(long)))))
procedure UNetFDSet(var FDSet: TUFDSet; const FileDesc: TUSocket);
  var i, j: Int32;
  var n: TUFDSetUnit;
begin
  i := FileDesc;
  j := i div (8 * SizeOf(TUFDSetUnit));
  n := TUFDSetUnit(1) shl (i mod (8 * SizeOf(TUFDSetUnit)));
  FDSet[j] := FDSet[j] or n;
end;
{$endif}

procedure UNetInitialize;
{$if defined(windows)}
  var WSAData: TUNetWSAData;
begin
  UNetWSAStartup(2 or (2 shl 8), WSAData);
end;
{$else}
begin
end;
{$endif}

procedure UNetFinalize;
{$if defined(windows)}
begin
  UNetWSACleanup;
end;
{$else}
begin
end;
{$endif}

initialization
begin
  UNetInitialize;
end;

finalization
begin
  UNetFinalize;
end;

end.
