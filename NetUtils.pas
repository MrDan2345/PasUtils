unit NetUtils;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$modeswitch nestedprocvars}
{$modeswitch typehelpers}
{$optimization autoinline}
{$macro on}

interface

uses
  SysUtils;

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
  a := IfAddrs;
  while Assigned(a) do
  try
    if not Assigned(a^.ifa_addr) then Continue;
    Addr := a^.ifa_addr^.sin_addr;
    if (Result.s_addr = 0)
    or (Addr.s_bytes[1] = 192) then
    begin
      Result := Addr;
    end;
  finally
    a := a^.ifa_next;
  end;
  FreeIfAddrs(IfAddrs);
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
  Result.sin_port := UNetHostToNetShort(Net.sin_port);
  Result.sin_addr := UNetHostToNet(Net.sin_addr);
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
    d := 100;
    n := 0;
    try
      for i := Pos to Length(AddrStr) do
      begin
        Pos := i + 1;
        if AddrStr[i] = '.' then Exit(True);
        if not Ord(AddrStr[i]) in [Ord('0')..Ord('9')] then Exit(False);
        n += Ord(AddrStr[i]) - Ord('0') * d;
        d := d div 10;
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
  i := 0;
  for n := 0 to 3 do
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
