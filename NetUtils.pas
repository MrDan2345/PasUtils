unit NetUtils;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$modeswitch nestedprocvars}
{$modeswitch typehelpers}
{$optimization autoinline}
{$macro on}

interface

uses
  SysUtils, Classes, CommonUtils;

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

//shutdown options
const SHUT_RD = 0;
const SHUT_WR = 1;
const SHUT_RDWR = 2;

type TUSockLen = UInt32;
type PUSockLen = ^TUSockLen;

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

type TUSocket = Int32;
type PUSocket = ^TUSocket;

type TUInAddrImpl = type helper for TUInAddr
  const Zero: TUInAddr = (Addr32: 0);
  const LocalhostH: TUInAddr = (Addr8: (1, 0, 0, 127));
  const LocalhostN: TUInAddr = (Addr8: (127, 0, 0, 1));
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
  function MakeTCP(const SockDomain: Int32 = AF_INET): TUSocket;
  function MakeUDP(const SockDomain: Int32 = AF_INET): TUSocket;
  function Bind(const Addr: PUSockAddr; const AddrLen: TUSockLen): Int32;
  function Listen(const Backlog: Int32): Int32;
  function Accept(const Addr: PUSockAddr; const AddrLen: PUSockLen): Int32;
  function Connect(const Addr: PUSockAddr; const AddrLen: TUSockLen): Int32;
  function Send(
    const Buffer: Pointer;
    const BufferLen: UInt32;
    const Flags: Int32
  ): SizeInt;
  function Send(const Msg: AnsiString): SizeInt;
  function Recv(
    const Buffer: Pointer;
    const BufferLen: UInt32;
    const Flags: Int32
  ): SizeInt;
  function Recv: String;
  function SendTo(
    const Buffer: Pointer;
    const BufferLen: UInt32;
    const Flags: Int32;
    const ToAddr: PUSockAddr;
    const ToLen: TUSockLen
  ): SizeInt;
  function RecvFrom(
    const Buffer: Pointer;
    const BufferLen: UInt32;
    const Flags: Int32;
    const AddrFrom: PUSockAddr;
    const AddrLen: PUSockLen
  ): SizeInt;
  function Shutdown(const How: Int32 = SHUT_RDWR): Int32;
  function Close: Int32;
  function IsValid: Boolean;
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
    private
      var _Beacon: TBeacon;
    public
      property Beacon: TBeacon read _Beacon write _Beacon;
    end;
    type TListener = class (TBeaconThread)
    private
      var Sock: TUSocket;
    public
      procedure Execute; override;
      procedure TerminatedSet; override;
    end;
    type TBroadcaster = class (TBeaconThread)
    private
      var Sock: TUSocket;
      var Event: TUEvent;
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
function UNetWSAStartup(
  VersionRequired: word;
  var WSData: TUNetWSAData
): Longint; call_decl; external SockLib name 'WSAStartup';
function UNetWSACleanup: Longint; call_decl; external SockLib name 'WSACleanup';
{$else}
function UNetGetIfAddrs(
  const IfAddrs: PPUIfAddrs
): Int32; call_decl; external SockLib name 'getifaddrs';
procedure FreeIfAddrs(
  const IfAddrs: PUIfAddrs
); call_decl; external SockLib name 'freeifaddrs';
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
): SizeInt; call_decl; external SockLib name 'recv';
function UNetRecvFrom(
  Sock: Int32; Buf: Pointer; Len: UInt32; Flags: Int32;
  FromAddr: PUSockAddr; FromLen: PUSockLen
): SizeInt; call_decl; external SockLib name 'recvfrom';
function UNetSend(
  Sock: Int32; Msg: Pointer; Len: UInt32; Flags: Int32
): SizeInt; call_decl; external SockLib name 'send';
function UNetSendTo(
  Sock: Int32; Msg: Pointer; Len: UInt32; Flags: Int32;
  ToAddr: PUSockAddr; ToLen: TUSockLen
): SizeInt; call_decl; external SockLib name 'sendto';
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
{$if defined(windows)}
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

implementation

function TUSocketImpl.Make(
  const SockDomain: Int32;
  const SockType: Int32;
  const SockProtocol: Int32
): TUSocket;
begin
  Self := UNetSocket(SockDomain, SockType, SockProtocol);
  Result := Self;
end;

function TUSocketImpl.MakeTCP(const SockDomain: Int32): TUSocket;
begin
  Result := Make(SockDomain, SOCK_STREAM);
end;

function TUSocketImpl.MakeUDP(const SockDomain: Int32): TUSocket;
begin
  Result := Make(SockDomain, SOCK_DGRAM);
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
): SizeInt;
begin
  Result := UNetSend(Self, Buffer, BufferLen, Flags);
end;

function TUSocketImpl.Send(const Msg: AnsiString): SizeInt;
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
): SizeInt;
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
): SizeInt;
begin
  Result := UNetSendTo(Self, Buffer, BufferLen, Flags, ToAddr, ToLen);
end;

function TUSocketImpl.RecvFrom(
  const Buffer: Pointer;
  const BufferLen: UInt32;
  const Flags: Int32;
  const AddrFrom: PUSockAddr;
  const AddrLen: PUSockLen
): SizeInt;
begin
  Result := UNetRecvFrom(Self, Buffer, BufferLen, Flags, AddrFrom, AddrLen);
end;

function TUSocketImpl.Shutdown(const How: Int32): Int32;
begin
  Result := UNetShutDown(Self, How);
end;

function TUSocketImpl.Close: Int32;
begin
  Result := UNetClose(Self);
end;

function TUSocketImpl.IsValid: Boolean;
begin
  Result := Self > -1;
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
    _Listener := TListener.Create(True);
    _Listener.Beacon := Self;
    _Listener.Start;
    if _Active then
    begin
      _Broadcaster := TBroadcaster.Create(True);
      _Broadcaster.Beacon := Self;
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
  var Json: TUJsonRef;
begin
  _Enabled := False;
  _Active := False;
  _Port := 57210;
  _LocalAddr := UNetLocalAddr;
  _HostName := UNetHostName;
  _BroadcastInterval := 5000;
  Json := TUJson.Make;
  Json.Ptr.AddValue('id', _HostName);
  Json.Ptr.AddValue('addr', UNetNetAddrToStr(_LocalAddr));
  _Message := Json.Ptr.AsString;
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
  var n: SizeInt;
  var Msg: String;
begin
  Sock := TUSocket.Invalid;
  Sock.MakeUDP();
  SockAddr := TUSockAddr.Default;
  SockAddr.sin_port := UNetHostToNetShort(Beacon.Port);
  Sock.Bind(@SockAddr, SizeOf(SockAddr));
  OtherAddr := TUSockAddr.Default;
  OtherAddrLen := SizeOf(OtherAddr);
  while not Terminated do
  begin
    n := Sock.RecvFrom(@Buffer, SizeOf(Buffer), 0, @OtherAddr, @OtherAddrLen);
    if n <= 0 then Break;
    if OtherAddr.sin_addr.Addr32 = Beacon.LocalAddr.Addr32 then Continue;
    Msg := Buffer;
    Beacon.AddPeer(OtherAddr.sin_addr, Msg);
    OtherAddr.sin_port := UNetHostToNetShort(Beacon.Port);
    n := Sock.SendTo(
      @Beacon.Message[1], Length(Beacon.Message) + 1, 0,
      @OtherAddr, SizeOf(OtherAddr)
    );
  end;
  if Sock.IsValid then
  begin
    Sock.Shutdown();
    Sock.Close;
  end;
end;

procedure TUNet.TBeacon.TListener.TerminatedSet;
begin
  Sock.Shutdown();
  Sock.Close;
  inherited TerminatedSet;
end;

procedure TUNet.TBeacon.TBroadcaster.Execute;
  var LocalAddr: TUInAddr;
  var SockAddr: TUSockAddr;
  var i: Int32;
begin
  LocalAddr := Beacon.LocalAddr;
  Sock := TUSocket.Invalid;
  Sock.MakeUDP();
  SockAddr := TUSockAddr.Default;
  SockAddr.sin_addr := LocalAddr;
  SockAddr.sin_port := UNetHostToNetShort(Beacon.Port);
  Event.Unsignal;
  while not Terminated do
  begin
    for i := 1 to 255 do
    begin
      SockAddr.sin_addr.Addr8[3] := UInt8(i);
      if SockAddr.sin_addr.Addr32 = LocalAddr.Addr32 then Continue;
      Sock.SendTo(
        @Beacon.Message[1], Length(Beacon.Message) + 1, 0,
        @SockAddr, SizeOf(SockAddr)
      );
    end;
    Event.WaitFor(Beacon.BroadcastInterval);
  end;
end;

procedure TUNet.TBeacon.TBroadcaster.TerminatedSet;
begin
  Event.Signal;
  inherited TerminatedSet;
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
