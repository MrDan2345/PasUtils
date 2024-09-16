unit NetUtils;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$modeswitch nestedprocvars}
{$modeswitch typehelpers}
{$optimization autoinline}
{$macro on}

interface

//address families
const AF_UNSPEC = 0;
const AF_UNIX = 1;
const AF_INET = 2;
const AF_IPX = 4;
const AF_INET6 = 10;

//socket types
const SOCK_DGRAM = 1;
const SOCK_STREAM = 2;

const INADDR_ANY = UInt32(0);
const INADDR_NONE = UInt32(-1);

//shutdown options
const SHUT_RD = 0;
const SHUT_WR = 1;
const SHUT_RDWR = 2;

type TUSockLen = UInt32;
type PUSockLen = ^TUSockLen;

type TUInAddr = packed record
  case UInt8 of
  0: (s_addr: UInt32);
  1: (s_bytes: packed array[1..4] of UInt8);
end;
type PUInAddr = ^TUInAddr;

type TUSockAddr = packed record
  sa_len: UInt8;
  sin_family: UInt8;
  sin_port: UInt16;
  sin_addr: TUInAddr;
  sin_zero: packed array[0..7] of UInt8;
end;
type PUSockAddr = ^TUSockAddr;

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

type TUSocket = type Int32;
type PUSocket = ^TUSocket;

type TUSocketImpl = type helper for TUSocket
  const InvalidSocket = -1;
  function Make(
    const SockDomain: Int32 = AF_INET;
    const SockType: Int32 = SOCK_STREAM;
    const SockProtocol: Int32 = 0
  ): TUSocket;
  function MakeTCP(const SockDomain: Int32 = AF_INET): TUSocket;
  function MakeUDP(const SockDomain: Int32 = AF_INET): TUSocket;
  function Bind(const Addr: PUSockAddr; const AddrLen: TUSockLen): Int32;
  function SendTo(
    const Buffer: Pointer;
    const BufferLen: SizeUInt;
    const Flags: Int32;
    const ToAddr: PUSockAddr;
    const ToLen: TUSockLen
  ): Int32;
  function RecvFrom(
    const Buffer: Pointer;
    const BufferLen: SizeUInt;
    const Flags: Int32;
    const AddrFrom: PUSockAddr;
    const AddrLen: PUSockLen
  ): Int32;
  function Shutdown(const How: Int32 = SHUT_RDWR): Int32;
  function Close: Int32;
  function IsValid: Boolean;
end;

function UNetGetIfAddrs(
  const IfAddrs: PPUIfAddrs
): Int32; cdecl; external 'c' name 'getifaddrs';
procedure FreeIfAddrs(
  const IfAddrs: PUIfAddrs
); cdecl; external 'c' name 'freeifaddrs';
function UNetGetAddrInfo(
  const Node: PAnsiChar;
  const Service: PAnsiChar;
  const Hints: PUAddrInfo;
  const Results: PPUAddrInfo
): Int32; cdecl; external 'c' name 'getaddrinfo';
procedure UNetFreeAddrInfo(
  const AddrInfo: PUAddrInfo
); cdecl; external 'c' name 'freeaddrinfo';
function UNetGetHostName(
  const Name: PAnsiChar;
  const Len: Int32
): Int32; external 'c' name 'gethostname';
function GetHostByName(
  const Name: PAnsiChar
): PUHostEnt; external 'c' name 'gethostbyname';

function UNetSocket(
  Domain: Int32; SockType: Int32; Protocol: Int32
): Int32; cdecl; external 'c' name 'socket';
function UNetRecv(
  Sock: Int32; Buf: Pointer; Len: SizeUInt; Flags: Int32
): SizeInt; cdecl; external 'c' name 'recv';
function UNetRecvFrom(
  Sock: Int32; Buf: Pointer; Len: SizeUInt; Flags: Int32;
  FromAddr: PUSockAddr; FromLen: PUSockLen
): SizeInt; cdecl; external 'c' name 'recvfrom';
function UNetSend(
  Sock: Int32; Msg: Pointer; Len: SizeUInt; Flags: Int32
): SizeInt; cdecl; external 'c' name 'send';
function UNetSendTo(
  Sock: Int32; Msg: Pointer; Len: SizeUInt; Flags: Int32;
  ToAddr: PUSockAddr; ToLen: TUSockLen
): SizeInt; cdecl; external 'c' name 'sendto';
function UNetBind(
  Sock: Int32; Addr: PUSockAddr; AddrLen: TUSockLen
): Int32; cdecl; external 'c' name 'bind';
function UNetListen(
    Sock: Int32; Backlog: Int32
): Int32; cdecl; external 'c' name 'listen';
function UNetAccept(
    Sock: Int32; Addr: PUSockAddr; AddrLen: PUSockLen
): Int32; cdecl; external 'c' name 'accept';
function UNetConnect(
    Sock: Int32; Addr: PUSockAddr; AddrLen: TUSockLen
): Int32; cdecl; external 'c' name 'connect';
function UNetShutDown(
    Sock: Int32; How: Int32
): Int32; cdecl; external 'c' name 'shutdown';
function UNetGetSockName(
    Sock: Int32; Name: PUSockAddr; NameLen: PUSockLen
): Int32; cdecl; external 'c' name 'sockname';
function UNetGetPeerName(
    Sock: Int32; Name: PUSockAddr; NameLen: PUSockLen
): Int32; cdecl; external 'c' name 'peername';
function UNetGetSockOpt(
    Sock: Int32; Level: Int32; OptName: Int32;
    OptVal: Pointer; OptLen : PUSockLen
): Int32; cdecl; external 'c' name 'getsockopt';
function UNetSetSockOpt(
    Sock: Int32; Level: Int32; OptName: Int32;
    OptVal: Pointer; OptLen: TUSockLen
): Int32; cdecl; external 'c' name 'setsockopt';
function UNetSocketPair(
    Domain: Int32; SockType: Int32; Protocol: Int32; OutSock: PInt32
): Int32; cdecl; external 'c' name 'socketpair';
function UNetClose(
    Sock: Int32
): Int32; cdecl; external 'c' name 'close';

function UNetLocalAddr: TUInAddr;

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

function TUSocketImpl.SendTo(
  const Buffer: Pointer; const BufferLen: SizeUInt; const Flags: Int32;
  const ToAddr: PUSockAddr; const ToLen: TUSockLen
): Int32;
begin
  Result := UNetSendTo(Self, Buffer, BufferLen, Flags, ToAddr, ToLen);
end;

function TUSocketImpl.RecvFrom(const Buffer: Pointer;
  const BufferLen: SizeUInt; const Flags: Int32; const AddrFrom: PUSockAddr;
  const AddrLen: PUSockLen): Int32;
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

function UNetLocalAddr: TUInAddr;
{$if defined(WINDOWS)}
  type TInAddrArr = array[UInt32] of TInAddr;
  type PInAddrArr = ^TInAddrArr;
  var AddrArr: PInAddrArr;
  var Host: PHostEnt;
  var i: Int32;
{$else}
  var r: Int32;
  var IfAddrs, a: PUIfAddrs;
{$endif}
  var Addr: TUInAddr;
  var s: String;
begin
  Result.s_addr := 0;
{$if defined(WINDOWS)}
  Host := GetHostByName(PAnsiChar(GetMyName));
  if not Assigned(Host) then Exit;
  AddrArr := PInAddrArr(Host^.h_Addr_List^);
  i := 0;
  while AddrArr^[i].S_addr <> 0 do
  try
    Addr := Sockets.PInAddr(@AddrArr^[i].S_addr)^;
    if (Result.s_addr = 0)
    or (Addr.s_bytes[1] = 192) then
    begin
      Result := Addr;
    end;
    //s := NetAddrToStr(Addr);
    //WriteLn(s);
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
    //s := NetAddrToStr(Addr);
    //WriteLn(s);
  finally
    a := a^.ifa_next;
  end;
  FreeIfAddrs(IfAddrs);
{$endif}
end;

end.
