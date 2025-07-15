unit CryptoUtils;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$modeswitch nestedprocvars}
{$modeswitch typehelpers}
{$optimization autoinline}
{$macro on}
{$warn 6058 off}
{$warn 5024 off}
{$warn 3123 off}
{$warn 3124 off}
{$warn 5026 off}
{$warn 6018 off}

interface

uses
  SysUtils,
  Classes,
  CommonUtils;

type TURSAKey = record
  Size: UInt32; // bit size
  n: TUInt4096; // modulus
  e: TUInt4096; // public exponent
  d: TUInt4096; // private exponent
end;

type TUMontgomeryReduction = record
private
  N: TUInt4096;
  N_prime: UInt32;
  R2_mod_N: TUInt4096;
  NumItems: Int32;
public
  procedure Init(const Modulus: TUInt4096);
  function Conv(const Number: TUInt4096): TUInt4096;
  function Mul(const a, b: TUInt4096): TUInt4096;
end;

function UModInverse(const e, phi: TUInt4096): TUInt4096;
function UGCD(const a, b: TUInt4096): TUInt4096;
function UPowMod(const Base, Exp, Modulus: TUInt4096): TUInt4096;
function UMillerRabinTest(const Number: TUInt4096; const Iterations: Int32 = 50): Boolean;
function UMakePrime(const BitCount: Int32 = 1024): TUInt4096;
function UMakePrimes(
  const PrimeCount: Int32;
  const BitCount: Int32 = 2048;
  const ThreadCount: Int32 = 8
): TUInt4096Array;
function UMakeRSAKey(const BitCount: UInt32 = 2048; const Threads: Int32 = 16): TURSAKey;
function UPackData(
  const Data: Pointer;
  const DataSize: UInt32;
  const BlockSize: UInt32 = 2048
): TUInt4096;
function UPackData(
  const Data: TUInt8Array;
  const BlockSize: UInt32 = 2048
): TUInt4096;
function UUnpackData(
  const Block: TUInt4096;
  const BlockSize: UInt32 = 2048
): TUInt8Array;
function UUnpackStr(
  const Block: TUInt4096;
  const BlockSize: UInt32 = 2048
): String;
function UEncrypt(
  const Data: Pointer;
  const DataSize: UInt32;
  const Key: TURSAKey
): TUInt4096;
function UEncrypt(
  const Data: TUInt8Array;
  const Key: TURSAKey
): TUInt4096;
function UEncryptStr(
  const Str: String;
  const Key: TURSAKey
): TUInt4096;
function UDecryptStr(
  const Cipher: TUInt4096;
  const Key: TURSAKey
): String;
function UDecrypt(
  const Cipher: TUInt4096;
  const Key: TURSAKey
): TUInt8Array;

implementation

procedure TUMontgomeryReduction.Init(const Modulus: TUInt4096);
  var inv: UInt32;
  var i: Int32;
  var R_val, R2_val, R_mod_N: TUInt4096;
begin
  N := Modulus;
  NumItems := N.Top + 1;
  inv := 1;
  for i := 1 to 5 do
  begin
    inv := inv * (2 - N[0] * inv);
  end;
  N_prime := -inv;
  R_val := TUInt4096.Zero;
  if NumItems <= TUInt4096.MaxItem then
  begin
    R_val[NumItems] := 1;
  end;
  R_mod_N := R_val mod N;
  R2_val := R_mod_N * R_mod_N;
  R2_mod_N := R2_val mod N;
end;

function TUMontgomeryReduction.Conv(const Number: TUInt4096): TUInt4096;
begin
  Result := Mul(Number, R2_mod_N);
end;

function TUMontgomeryReduction.Mul(const a, b: TUInt4096): TUInt4096;
  var T: TUInt4096;
  var m: UInt32;
  var i, j, k: Int32;
  var Carry: UInt64;
  var Product, Sum: UInt64;
begin
  T := a * b;
  for i := 0 to NumItems - 1 do
  begin
    m := T[i] * N_prime;
    Carry := 0;
    for j := 0 to NumItems - 1 do
    begin
      Product := UInt64(m) * N[j];
      Sum := UInt64(T[i + j]) + (Product and $ffffffff) + Carry;
      T[i + j] := Sum and $ffffffff;
      Carry := (Product shr 32) + (Sum shr 32);
    end;
    k := i + NumItems;
    while (Carry > 0) and (k <= TUInt4096.MaxItem) do
    begin
      Sum := UInt64(T[k]) + Carry;
      T[k] := Sum and $ffffffff;
      Carry := Sum shr 32;
      Inc(k);
    end;
  end;
  for i := 0 to NumItems - 1 do
  begin
    Result[i] := T[i + NumItems];
  end;
  for i := NumItems to TUInt4096.MaxItem do
  begin
    Result[i] := 0;
  end;
  if Result >= N then
  begin
    Result := Result - N;
  end;
end;

function UModInverse(const e, phi: TUInt4096): TUInt4096;
  function GCD(const a, b: TUInt4096; var x: TUInt4096; var y: TUInt4096): TUInt4096;
    var x1, y1, gcd_val: TUInt4096;
  begin
    if a = TUInt4096.Zero then
    begin
      x := TUInt4096.Zero;
      y := TUInt4096.One;
      Exit(b);
    end;
    x1 := TUInt4096.Zero;
    y1 := TUInt4096.Zero;
    gcd_val := GCD(b mod a, a, x1, y1);
    x := y1 - (b div a) * x1;
    y := x1;
    Result := gcd_val;
  end;
  var x, y, gcd_val: TUInt4096;
begin
  x := TUInt4096.Zero;
  y := TUInt4096.Zero;
  gcd_val := GCD(e, phi, x, y);
  if gcd_val <> TUInt4096.One then Exit(TUInt4096.Invalid);
  if x < TUInt4096.Zero then x := x + phi;
  Result := x;
end;

function UGCD(const a, b: TUInt4096): TUInt4096;
  var Temp, Remainder: TUInt4096;
  var LocalA, LocalB: TUInt4096;
begin
  LocalA := a;
  LocalB := b;
  while not LocalB.IsZero do
  begin
    Remainder := LocalA mod LocalB;
    LocalA := LocalB;
    LocalB := Remainder;
  end;
  Result := LocalA;
end;

function UPowMod(const Base, Exp, Modulus: TUInt4096): TUInt4096;
  var Context: TUMontgomeryReduction;
  var BaseMont: TUInt4096;
  var ResultMont: TUInt4096;
  var CurrentExponent: TUInt4096;
begin
  Context.Init(Modulus);
  BaseMont := Context.Conv(Base);
  ResultMont := Context.Conv(TUInt4096.One);
  CurrentExponent := Exp;
  while not CurrentExponent.IsZero do
  begin
    if CurrentExponent.IsOdd then
    begin
      ResultMont := Context.Mul(ResultMont, BaseMont);
    end;
    BaseMont := Context.Mul(BaseMont, BaseMont);
    CurrentExponent := TUInt4096.ShrOne(CurrentExponent);
  end;
  Result := Context.Mul(ResultMont, TUInt4096.One);
end;

function UMillerRabinTest(const Number: TUInt4096; const Iterations: Int32): Boolean;
  var Two: TUInt4096;
  var Cmp: Int8;
  var i: Int32;
  var m, m_temp: UInt32;
  var d, a, x, n_minus_1, n_minus_2: TUInt4096;
begin
  Two := 2;
  Cmp := TUInt4096.Compare(Number, Two);
  if Cmp < 0 then Exit(False);
  if Cmp = 0 then Exit(True);
  if not Number.IsOdd then Exit(False);
  n_minus_1 := Number - TUInt4096.One;
  d := n_minus_1;
  m := 0;
  while not d.IsOdd and (d > TUInt4096.Zero) do
  begin
    d := TUInt4096.ShrOne(d);
    Inc(m);
  end;
  n_minus_2 := Number - Two;
  for i := 1 to Iterations do
  begin
    a := TUInt4096.MakeRandomRange(Two, n_minus_2);
    x := UPowMod(a, d, Number);
    if (x = TUInt4096.One) or (x = n_minus_1) then Continue;
    m_temp := m;
    while m_temp > 1 do
    begin
      x := UPowMod(x, Two, Number);
      if x = TUInt4096.One then Exit(False);
      if x = n_minus_1 then Break;
      Dec(m_temp);
    end;
    if not (x = n_minus_1) then Exit(False);
  end;
  Result := True;
end;

function UMakePrime(const BitCount: Int32): TUInt4096;
  var TestCount: Int32;
begin
  TestCount := 0;
  repeat
    Result := TUInt4096.MakeRandom(BitCount);
    Result := Result or (TUInt4096.One shl (BitCount - 1));
    Result[0] := Result[0] or 1;
    Inc(TestCount);
    if TestCount mod 100 = 0 then WriteLn(TestCount);
  until UMillerRabinTest(Result, 100);
end;

type TMakePrimeContext = record
  BitCount: Int32;
  Primes: array of TUInt4096;
  CurItem: Int32;
  Lock: TUCriticalSection;
  Done: Boolean;
end;
type PMakePrimeContext = ^TMakePrimeContext;

type TMakePrimeThread = class (TThread)
public
  var Context: PMakePrimeContext;
  procedure Execute; override;
end;

procedure TMakePrimeThread.Execute;
  var Number: TUInt4096;
begin
  with Context^ do
  repeat
    Number := TUInt4096.MakeRandom(BitCount);
    Number := Number or (TUInt4096.One shl (BitCount - 1));
    Number[0] := Number[0] or 1;
    if not UMillerRabinTest(Number, 50) then Continue;
    Lock.Enter;
    try
      if CurItem > High(Primes) then Break;
      Primes[CurItem] := Number;
      Inc(CurItem);
      if CurItem < Length(Primes) then Continue;
      Done := True;
    finally
      Lock.Leave;
    end;
  until Done;
end;

function UMakePrimes(
  const PrimeCount: Int32; const BitCount: Int32;
  const ThreadCount: Int32
): TUInt4096Array;
  var Threads: array of TMakePrimeThread;
  var Context: TMakePrimeContext;
  var i: Int32;
begin
  if (PrimeCount < 1) or (ThreadCount < 1) then Exit(nil);
  Threads := nil;
  SetLength(Threads, ThreadCount);
  Context.BitCount := BitCount;
  Context.CurItem := 0;
  Context.Done := False;
  SetLength(Context.Primes, PrimeCount);
  for i := 0 to High(Threads) do
  begin
    Threads[i] := TMakePrimeThread.Create(True);
    Threads[i].Context := @Context;
  end;
  try
    for i := 0 to High(Threads) do
    begin
      Threads[i].Start;
    end;
  finally
    for i := 0 to High(Threads) do
    begin
      Threads[i].WaitFor;
      FreeAndNil(Threads[i]);
    end;
    Result := Context.Primes;
  end;
end;

function UMakeRSAKey(const BitCount: UInt32; const Threads: Int32): TURSAKey;
  var p, q, n, phi, e, d: TUInt4096;
  var PrimeSizeInBits: Int32;
  var Primes: TUInt4096Array;
begin
  Result.Size := 0;
  Result.n := TUInt4096.Invalid;
  Result.e := TUInt4096.Invalid;
  Result.d := TUInt4096.Invalid;
  PrimeSizeInBits := BitCount shr 1;
  e := 65537;
  repeat
    Primes := UMakePrimes(2, PrimeSizeInBits, Threads);
    if Length(Primes) < 2 then Exit;
    p := Primes[0];
    q := Primes[1];
    if p = q then Continue;
    WriteLn('p: ', p.ToString);
    WriteLn('q: ', q.ToString);
    n := p * q;
    phi := (p - TUInt4096.One) * (q - TUInt4096.One);
  until UGCD(e, phi) = TUInt4096.One;
  d := UModInverse(e, phi);
  Result.Size := BitCount;
  Result.n := n;
  Result.e := e;
  Result.d := d;
  WriteLn('Key Modulus: ', n.ToString);
  WriteLn('Key Public: ', e.ToString);
  WriteLn('Key Private: ', d.ToString);
end;

function UPackData(
  const Data: Pointer; const DataSize: UInt32;
  const BlockSize: UInt32
): TUInt4096;
  const MinPadding = 11;
  var PaddingSize, BlockSizeInBytes: Int32;
  var PaddedData: array[0..(TUInt4096Impl.MaxItem + 1) * 4 - 1] of UInt8 absolute Result;
  var i: Int32;
begin
  BlockSizeInBytes := BlockSize shr 3;
  if DataSize > BlockSizeInBytes - MinPadding then Exit(TUInt4096.Invalid);
  Result := TUInt4096.Zero;
  PaddingSize := BlockSizeInBytes - DataSize;
  PaddedData[0] := $00;
  PaddedData[1] := $02;
  for i := 2 to PaddingSize - 2 do
  repeat
    PaddedData[i] := UInt8(Random(256));
  until PaddedData[i] > 0;
  PaddedData[PaddingSize - 1] := 0;
  Move(Data^, PaddedData[PaddingSize], DataSize);
end;

function UPackData(const Data: TUInt8Array; const BlockSize: UInt32): TUInt4096;
begin
  Result := UPackData(@Data[0], Length(Data), BlockSize);
end;

function UUnpackData(const Block: TUInt4096; const BlockSize: UInt32): TUInt8Array;
  var PaddingSize, BlockSizeInBytes: Int32;
  var PaddedData: array[0..(TUInt4096Impl.MaxItem + 1) * 4 - 1] of UInt8 absolute Block;
  var i: Int32;
begin
  if (PaddedData[0] <> 0) or (PaddedData[1] <> 2) then Exit(nil);
  BlockSizeInBytes := BlockSize shr 3;
  PaddingSize := 0;
  for i := 2 to High(PaddedData) do
  begin
    if PaddedData[i] <> 0 then Continue;
    PaddingSize := i + 1;
    Break;
  end;
  if PaddingSize = 0 then Exit(nil);
  Result := nil;
  SetLength(Result, BlockSizeInBytes - PaddingSize);
  Move(PaddedData[PaddingSize], Result[0], Length(Result));
end;

function UUnpackStr(const Block: TUInt4096; const BlockSize: UInt32): String;
  var PaddingSize, BlockSizeInBytes: Int32;
  var PaddedData: array[0..(TUInt4096Impl.MaxItem + 1) * 4 - 1] of UInt8 absolute Block;
  var i: Int32;
begin
  if (PaddedData[0] <> 0) or (PaddedData[1] <> 2) then Exit('');
  PaddingSize := 0;
  BlockSizeInBytes := BlockSize shr 3;
  for i := 2 to High(PaddedData) do
  begin
    if PaddedData[i] <> 0 then Continue;
    PaddingSize := i + 1;
    Break;
  end;
  if PaddingSize = 0 then Exit('');
  Result := '';
  SetLength(Result, BlockSizeInBytes - PaddingSize);
  for i := 0 to High(Result) do
  begin
    Result[i + 1] := AnsiChar(PaddedData[PaddingSize + i]);
  end;
end;

function UEncrypt(
  const Data: Pointer;
  const DataSize: UInt32;
  const Key: TURSAKey
): TUInt4096;
  var Block: TUInt4096;
begin
  Block := UPackData(Data, DataSize, Key.Size);
  if not Block.IsValid then Exit(TUInt4096.Invalid);
  Result := UPowMod(Block, Key.e, Key.n);
end;

function UEncrypt(
  const Data: TUInt8Array;
  const Key: TURSAKey
): TUInt4096;
begin
  Result := UEncrypt(@Data[0], Length(Data), Key);
end;

function UEncryptStr(
  const Str: String;
  const Key: TURSAKey
): TUInt4096;
begin
  Result := UEncrypt(@Str[1], Length(Str), Key);
end;

function UDecryptStr(
  const Cipher: TUInt4096;
  const Key: TURSAKey
): String;
  var Block: TUInt4096;
begin
  Block := UPowMod(Cipher, Key.d, Key.n);
  Result := UUnpackStr(Block, Key.Size);
end;

function UDecrypt(
  const Cipher: TUInt4096;
  const Key: TURSAKey
): TUInt8Array;
  var Block: TUInt4096;
begin
  Block := UPowMod(Cipher, Key.d, Key.n);
  Result := UUnpackData(Block, Key.Size);
end;

end.
