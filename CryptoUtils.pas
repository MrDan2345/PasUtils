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

type TUSHA256Digest = array[0..31] of UInt8;

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

type TUAES = record
public
  type TKey256 = array[0..31] of UInt8;
  type TInitVector = array[0..15] of UInt8;
private
  type TBlock = array[0..3, 0..3] of UInt8;
  type TExpandedKey = array[0..14] of TBlock;
  const SBox: array[0..255] of UInt8 = (
    $63, $7c, $77, $7b, $f2, $6b, $6f, $c5, $30, $01, $67, $2b, $fe, $d7, $ab, $76,
    $ca, $82, $c9, $7d, $fa, $59, $47, $f0, $ad, $d4, $a2, $af, $9c, $a4, $72, $c0,
    $b7, $fd, $93, $26, $36, $3f, $f7, $cc, $34, $a5, $e5, $f1, $71, $d8, $31, $15,
    $04, $c7, $23, $c3, $18, $96, $05, $9a, $07, $12, $80, $e2, $eb, $27, $b2, $75,
    $09, $83, $2c, $1a, $1b, $6e, $5a, $a0, $52, $3b, $d6, $b3, $29, $e3, $2f, $84,
    $53, $d1, $00, $ed, $20, $fc, $b1, $5b, $6a, $cb, $be, $39, $4a, $4c, $58, $cf,
    $d0, $ef, $aa, $fb, $43, $4d, $33, $85, $45, $f9, $02, $7f, $50, $3c, $9f, $a8,
    $51, $a3, $40, $8f, $92, $9d, $38, $f5, $bc, $b6, $da, $21, $10, $ff, $f3, $d2,
    $cd, $0c, $13, $ec, $5f, $97, $44, $17, $c4, $a7, $7e, $3d, $64, $5d, $19, $73,
    $60, $81, $4f, $dc, $22, $2a, $90, $88, $46, $ee, $b8, $14, $de, $5e, $0b, $db,
    $e0, $32, $3a, $0a, $49, $06, $24, $5c, $c2, $d3, $ac, $62, $91, $95, $e4, $79,
    $e7, $c8, $37, $6d, $8d, $d5, $4e, $a9, $6c, $56, $f4, $ea, $65, $7a, $ae, $08,
    $ba, $78, $25, $2e, $1c, $a6, $b4, $c6, $e8, $dd, $74, $1f, $4b, $bd, $8b, $8a,
    $70, $3e, $b5, $66, $48, $03, $f6, $0e, $61, $35, $57, $b9, $86, $c1, $1d, $9e,
    $e1, $f8, $98, $11, $69, $d9, $8e, $94, $9b, $1e, $87, $e9, $ce, $55, $28, $df,
    $8c, $a1, $89, $0d, $bf, $e6, $42, $68, $41, $99, $2d, $0f, $b0, $54, $bb, $16
  );
  const InvSBox: array[0..255] of UInt8 = (
    $52, $09, $6a, $d5, $30, $36, $a5, $38, $bf, $40, $a3, $9e, $81, $f3, $d7, $fb,
    $7c, $e3, $39, $82, $9b, $2f, $ff, $87, $34, $8e, $43, $44, $c4, $de, $e9, $cb,
    $54, $7b, $94, $32, $a6, $c2, $23, $3d, $ee, $4c, $95, $0b, $42, $fa, $c3, $4e,
    $08, $2e, $a1, $66, $28, $d9, $24, $b2, $76, $5b, $a2, $49, $6d, $8b, $d1, $25,
    $72, $f8, $f6, $64, $86, $68, $98, $16, $d4, $a4, $5c, $cc, $5d, $65, $b6, $92,
    $6c, $70, $48, $50, $fd, $ed, $b9, $da, $5e, $15, $46, $57, $a7, $8d, $9d, $84,
    $90, $d8, $ab, $00, $8c, $bc, $d3, $0a, $f7, $e4, $58, $05, $b8, $b3, $45, $06,
    $d0, $2c, $1e, $8f, $ca, $3f, $0f, $02, $c1, $af, $bd, $03, $01, $13, $8a, $6b,
    $3a, $91, $11, $41, $4f, $67, $dc, $ea, $97, $f2, $cf, $ce, $f0, $b4, $e6, $73,
    $96, $ac, $74, $22, $e7, $ad, $35, $85, $e2, $f9, $37, $e8, $1c, $75, $df, $6e,
    $47, $f1, $1a, $71, $1d, $29, $c5, $89, $6f, $b7, $62, $0e, $aa, $18, $be, $1b,
    $fc, $56, $3e, $4b, $c6, $d2, $79, $20, $9a, $db, $c0, $fe, $78, $cd, $5a, $f4,
    $1f, $dd, $a8, $33, $88, $07, $c7, $31, $b1, $12, $10, $59, $27, $80, $ec, $5f,
    $60, $51, $7f, $a9, $19, $b5, $4a, $0d, $2d, $e5, $7a, $9f, $93, $c9, $9c, $ef,
    $a0, $e0, $3b, $4d, $ae, $2a, $f5, $b0, $c8, $eb, $bb, $3c, $83, $53, $99, $61,
    $17, $2b, $04, $7e, $ba, $77, $d6, $26, $e1, $69, $14, $63, $55, $21, $0c, $7d
  );
  const Rcon: array[1..10] of UInt8 = (
    $01, $02, $04, $08, $10, $20, $40, $80, $1b, $36
  );
  class function KeyExpansion(const Key: TKey256): TExpandedKey; static;
  class function PadData_PKCS7(const Data: TUInt8Array; const BlockSize: Int32): TUInt8Array; static;
  class function UnpadData_PKCS7(const Data: TUInt8Array): TUInt8Array; static;
  class procedure AddRoundKey(var State: TBlock; const RoundKey: TBlock); static;
  class function GMul(a, b: UInt8): UInt8; static;
  class procedure SubBytes(var State: TBlock); static;
  class procedure ShiftRows(var State: TBlock); static;
  class procedure MixColumns(var State: TBlock); static;
  class procedure CipherBlock(var State: TBlock; const ExpandedKey: TExpandedKey); static;
  class procedure InvSubBytes(var State: TBlock); static;
  class procedure InvShiftRows(var State: TBlock); static;
  class procedure InvMixColumns(var State: TBlock); static;
  class procedure InvCipherBlock(var State: TBlock; const ExpandedKey: TExpandedKey); static;
  class function Encrypt_AES_PKCS7_ECB_256(
    const Data: TUInt8Array;
    const Key: TKey256
  ): TUInt8Array; static;
  class function Decrypt_AES_PKCS7_ECB_256(
    const Cipher: TUInt8Array;
    const Key: TKey256
  ): TUInt8Array; static;
  class function Encrypt_AES_PKCS7_CBC_256(
    const Data: TUInt8Array;
    const Key: TKey256;
    const IV: TInitVector
  ): TUInt8Array; static;
  class function Decrypt_AES_PKCS7_CBC_256(
    const Cipher: TUInt8Array;
    const Key: TKey256;
    const IV: TInitVector
  ): TUInt8Array; static;
  class function Process_AES_CTR_256(
    const Input: TUInt8Array;
    const Key: TKey256;
    const IV: TInitVector
  ): TUInt8Array; static;
end;

function USHA256(const Data: Pointer; const DataSize: UInt32): TUSHA256Digest;
function USHA256(const Data: TUInt8Array): TUSHA256Digest;
function USHA256(const Data: String): TUSHA256Digest;
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
function UEncryptRSA(
  const Data: Pointer;
  const DataSize: UInt32;
  const Key: TURSAKey
): TUInt4096;
function UEncryptRSA(
  const Data: TUInt8Array;
  const Key: TURSAKey
): TUInt4096;
function UEncryptStrRSA(
  const Str: String;
  const Key: TURSAKey
): TUInt4096;
function UDecryptStrRSA(
  const Cipher: TUInt4096;
  const Key: TURSAKey
): String;
function UDecryptRSA(
  const Cipher: TUInt4096;
  const Key: TURSAKey
): TUInt8Array;
function UEncrypt_AES_PKCS7_ECB_256(
  const Data: TUInt8Array;
  const Key: TUAES.TKey256
): TUInt8Array;
function UDecrypt_AES_PKCS7_ECB_256(
  const Cipher: TUInt8Array;
  const Key: TUAES.TKey256
): TUInt8Array;
function UEncrypt_AES_PKCS7_CBC_256(
  const Data: TUInt8Array;
  const Key: TUAES.TKey256;
  const IV: TUAES.TInitVector
): TUInt8Array;
function UDecrypt_AES_PKCS7_CBC_256(
  const Cipher: TUInt8Array;
  const Key: TUAES.TKey256;
  const IV: TUAES.TInitVector
): TUInt8Array;
function UEncrypt_AES_CTR_256(
  const Data: TUInt8Array;
  const Key: TUAES.TKey256;
  const IV: TUAES.TInitVector
): TUInt8Array;
function UDecrypt_AES_CTR_256(
  const Cipher: TUInt8Array;
  const Key: TUAES.TKey256;
  const IV: TUAES.TInitVector
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

function USHA256(const Data: Pointer; const DataSize: UInt32): TUSHA256Digest;
  function RightRotate(const Value: UInt32; const Amount: Int32): UInt32;
  begin
    Result := (Value shr Amount) or (Value shl (32 - Amount));
  end;
  const K: array[0..63] of UInt32 = (
    $428a2f98, $71374491, $b5c0fbcf, $e9b5dba5, $3956c25b, $59f111f1, $923f82a4, $ab1c5ed5,
    $d807aa98, $12835b01, $243185be, $550c7dc3, $72be5d74, $80deb1fe, $9bdc06a7, $c19bf174,
    $e49b69c1, $efbe4786, $0fc19dc6, $240ca1cc, $2de92c6f, $4a7484aa, $5cb0a9dc, $76f988da,
    $983e5152, $a831c66d, $b00327c8, $bf597fc7, $c6e00bf3, $d5a79147, $06ca6351, $14292967,
    $27b70a85, $2e1b2138, $4d2c6dfc, $53380d13, $650a7354, $766a0abb, $81c2c92e, $92722c85,
    $a2bfe8a1, $a81a664b, $c24b8b70, $c76c51a3, $d192e819, $d6990624, $f40e3585, $106aa070,
    $19a4c116, $1e376c08, $2748774c, $34b0bcb5, $391c0cb3, $4ed8aa4a, $5b9cca4f, $682e6ff3,
    $748f82ee, $78a5636f, $84c87814, $8cc70208, $90befffa, $a4506ceb, $bef9a3f7, $c67178f2
  );
  var Hash: array[0..7] of UInt32 = (
    $6a09e667, $bb67ae85, $3c6ef372, $a54ff53a,
    $510e527f, $9b05688c, $1f83d9ab, $5be0cd19
  );
  var DataBytes: PUInt8Arr absolute Data;
  var wv: array[0..7] of UInt32;
  var a: UInt32 absolute wv[0];
  var b: UInt32 absolute wv[1];
  var c: UInt32 absolute wv[2];
  var d: UInt32 absolute wv[3];
  var e: UInt32 absolute wv[4];
  var f: UInt32 absolute wv[5];
  var g: UInt32 absolute wv[6];
  var h: UInt32 absolute wv[7];
  var s0, s1, ch, maj, temp1, temp2: UInt32;
  var w: array[0..63] of UInt32;
  var PaddedMessage: TUInt8Array;
  var OriginalLength: UInt64;
  var PaddedLength: Integer;
  var i, j, ChunkStart: Integer;
begin
  OriginalLength := DataSize;
  PaddedLength := DataSize + 1;
  while (PaddedLength mod 64) <> 56 do
  begin
    Inc(PaddedLength);
  end;
  PaddedLength := PaddedLength + 8;
  PaddedMessage := nil;
  SetLength(PaddedMessage, PaddedLength);
  for i := 0 to DataSize - 1 do
  begin
    PaddedMessage[i] := DataBytes^[i];
  end;
  PaddedMessage[DataSize] := $80;
  for i := DataSize + 1 to PaddedLength - 9 do
  begin
    PaddedMessage[i] := 0;
  end;
  OriginalLength := OriginalLength * 8;
  for i := 0 to 7 do
  begin
    PaddedMessage[PaddedLength - 8 + i] := (OriginalLength shr (8 * (7 - i))) and $ff;
  end;
  ChunkStart := 0;
  while ChunkStart < PaddedLength do
  begin
    for i := 0 to 15 do
    begin
      j := ChunkStart + i * 4;
      w[i] := (
        (UInt32(PaddedMessage[j]) shl 24) or
        (UInt32(PaddedMessage[j + 1]) shl 16) or
        (UInt32(PaddedMessage[j + 2]) shl 8) or
        UInt32(PaddedMessage[j + 3])
      );
    end;
    for i := 16 to 63 do
    begin
      s0 := RightRotate(w[i - 15], 7) xor RightRotate(w[i - 15], 18) xor (w[i - 15] shr 3);
      s1 := RightRotate(w[i - 2], 17) xor RightRotate(w[i - 2], 19) xor (w[i - 2] shr 10);
      w[i] := w[i - 16] + s0 + w[i - 7] + s1;
    end;
    for i := 0 to High(Hash) do wv[i] := Hash[i];
    for i := 0 to 63 do
    begin
      s1 := RightRotate(e, 6) xor RightRotate(e, 11) xor RightRotate(e, 25);
      ch := (e and f) xor ((not e) and g);
      temp1 := h + s1 + ch + K[i] + w[i];
      s0 := RightRotate(a, 2) xor RightRotate(a, 13) xor RightRotate(a, 22);
      maj := (a and b) xor (a and c) xor (b and c);
      temp2 := s0 + maj;
      h := g;
      g := f;
      f := e;
      e := d + temp1;
      d := c;
      c := b;
      b := a;
      a := temp1 + temp2;
    end;
    for i := 0 to High(Hash) do Hash[i] += wv[i];
    Inc(ChunkStart, 64);
  end;
  for i := 0 to High(Hash) do
  begin
    j := i * 4;
    Result[j] := (Hash[i] shr 24) and $ff;
    Result[j + 1] := (Hash[i] shr 16) and $ff;
    Result[j + 2] := (Hash[i] shr 8) and $ff;
    Result[j + 3] := Hash[i] and $ff;
  end;
end;

function USHA256(const Data: TUInt8Array): TUSHA256Digest;
begin
  Result := USHA256(@Data[0], Length(Data));
end;

function USHA256(const Data: String): TUSHA256Digest;
begin
  Result := USHA256(@Data[1], Length(Data));
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

function UEncryptRSA(
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

function UEncryptRSA(
  const Data: TUInt8Array;
  const Key: TURSAKey
): TUInt4096;
begin
  Result := UEncryptRSA(@Data[0], Length(Data), Key);
end;

function UEncryptStrRSA(
  const Str: String;
  const Key: TURSAKey
): TUInt4096;
begin
  Result := UEncryptRSA(@Str[1], Length(Str), Key);
end;

function UDecryptStrRSA(
  const Cipher: TUInt4096;
  const Key: TURSAKey
): String;
  var Block: TUInt4096;
begin
  Block := UPowMod(Cipher, Key.d, Key.n);
  Result := UUnpackStr(Block, Key.Size);
end;

function UDecryptRSA(
  const Cipher: TUInt4096;
  const Key: TURSAKey
): TUInt8Array;
  var Block: TUInt4096;
begin
  Block := UPowMod(Cipher, Key.d, Key.n);
  Result := UUnpackData(Block, Key.Size);
end;

class function TUAES.KeyExpansion(const Key: TKey256): TExpandedKey;
  type TWord = array[0..3] of UInt8;
  var w: array[0..59] of TWord;
  var Temp: TWord;
  var i, j, k: Int32;
  var t_byte: UInt8;
begin
  for i := 0 to 7 do
  for j := 0 to 3 do
  begin
    w[i][j] := Key[i * 4 + j];
  end;
  i := 8;
  for i := 8 to 59 do
  begin
    Temp := w[i - 1];
    if (i mod 8) = 0 then
    begin
      t_byte := Temp[0];
      Temp[0] := Temp[1]; Temp[1] := Temp[2]; Temp[2] := Temp[3]; Temp[3] := t_byte;
      for j := 0 to 3 do Temp[j] := TUAES.SBox[Temp[j]];
      Temp[0] := Temp[0] xor TUAES.Rcon[i div 8];
    end
    else if (i mod 8) = 4 then
    begin
      for j := 0 to 3 do Temp[j] := TUAES.SBox[Temp[j]];
    end;
    for j := 0 to 3 do
    begin
      w[i][j] := w[i - 8][j] xor Temp[j];
    end;
  end;
  for i := 0 to 14 do
  for j := 0 to 3 do
  for k := 0 to 3 do
  begin
    Result[i][k, j] := w[i * 4 + j][k];
  end;
end;

class function TUAES.PadData_PKCS7(const Data: TUInt8Array; const BlockSize: Int32): TUInt8Array;
  var i, PadLen: Int32;
begin
  PadLen := BlockSize - (Length(Data) mod BlockSize);
  if PadLen = 0 then PadLen := BlockSize;
  Result := nil;
  SetLength(Result, Length(Data) + PadLen);
  if Length(Data) > 0 then
  begin
    Move(Data[0], Result[0], Length(Data));
  end;
  for i := Length(Data) to High(Result) do
  begin
    Result[i] := UInt8(PadLen);
  end;
end;

class function TUAES.UnpadData_PKCS7(const Data: TUInt8Array): TUInt8Array;
  var PadLen, i: Integer;
begin
  if Length(Data) = 0 then Exit(Data);
  PadLen := Data[High(Data)];
  if (PadLen = 0) or (PadLen > Length(Data)) then Exit(nil);
  for i := Length(Data) - PadLen to High(Data) - 1 do
  begin
    if Data[i] <> PadLen then Exit(nil);
  end;
  SetLength(Result, Length(Data) - PadLen);
  if Length(Result) = 0 then Exit;
  Move(Data[0], Result[0], Length(Result));
end;

class procedure TUAES.AddRoundKey(var State: TBlock; const RoundKey: TBlock);
  var r, c: Int32;
begin
  for r := 0 to 3 do
  for c := 0 to 3 do
  begin
    State[r, c] := State[r, c] xor RoundKey[r, c];
  end;
end;

class function TUAES.GMul(a, b: UInt8): UInt8;
  var p: UInt8;
  var i: Int32;
begin
  p := 0;
  for i := 0 to 7 do
  begin
    if (b and 1) <> 0 then
    begin
      p := p xor a;
    end;
    if (a and $80) <> 0 then
    begin
      a := (a shl 1) xor $1b
    end
    else
    begin
      a := a shl 1;
    end;
    b := b shr 1;
  end;
  Result := p;
end;

class procedure TUAES.SubBytes(var State: TBlock);
  var r, c: Int32;
begin
  for r := 0 to 3 do
  for c := 0 to 3 do
  begin
    State[r, c] := TUAES.SBox[State[r, c]];
  end;
end;

class procedure TUAES.ShiftRows(var State: TBlock);
  var Temp: UInt8;
begin
  Temp := State[1, 0];
  State[1, 0] := State[1, 1]; State[1, 1] := State[1, 2]; State[1, 2] := State[1, 3]; State[1, 3] := Temp;
  Temp := State[2, 0]; State[2, 0] := State[2, 2]; State[2, 2] := Temp;
  Temp := State[2, 1]; State[2, 1] := State[2, 3]; State[2, 3] := Temp;
  Temp := State[3, 3];
  State[3, 3] := State[3, 2]; State[3, 2] := State[3, 1]; State[3, 1] := State[3, 0]; State[3, 0] := Temp;
end;

class procedure TUAES.MixColumns(var State: TBlock);
  var c, r: Int32;
  var a: array[0..3] of UInt8;
begin
  for c := 0 to 3 do
  begin
    for r := 0 to 3 do a[r] := State[r, c];
    State[0, c] := GMul(a[0], 2) xor GMul(a[1], 3) xor GMul(a[2], 1) xor GMul(a[3], 1);
    State[1, c] := GMul(a[0], 1) xor GMul(a[1], 2) xor GMul(a[2], 3) xor GMul(a[3], 1);
    State[2, c] := GMul(a[0], 1) xor GMul(a[1], 1) xor GMul(a[2], 2) xor GMul(a[3], 3);
    State[3, c] := GMul(a[0], 3) xor GMul(a[1], 1) xor GMul(a[2], 1) xor GMul(a[3], 2);
  end;
end;

class procedure TUAES.CipherBlock(
  var State: TBlock;
  const ExpandedKey: TExpandedKey
);
  var Round: Int32;
begin
  AddRoundKey(State, ExpandedKey[0]);
  for Round := 1 to 13 do
  begin
    SubBytes(State);
    ShiftRows(State);
    MixColumns(State);
    AddRoundKey(State, ExpandedKey[Round]);
  end;
  SubBytes(State);
  ShiftRows(State);
  AddRoundKey(State, ExpandedKey[14]);
end;

class procedure TUAES.InvSubBytes(var State: TBlock);
  var r, c: Int32;
begin
  for r := 0 to 3 do
  for c := 0 to 3 do
  begin
    State[r, c] := InvSBox[State[r, c]];
  end;
end;

class procedure TUAES.InvShiftRows(var State: TBlock);
  var Temp: UInt8;
begin
  Temp := State[1, 3]; State[1, 3] := State[1, 2]; State[1, 2] := State[1, 1]; State[1, 1] := State[1, 0]; State[1, 0] := Temp;
  Temp := State[2, 0]; State[2, 0] := State[2, 2]; State[2, 2] := Temp; Temp := State[2, 1]; State[2, 1] := State[2, 3]; State[2, 3] := Temp;
  Temp := State[3, 0]; State[3, 0] := State[3, 1]; State[3, 1] := State[3, 2]; State[3, 2] := State[3, 3]; State[3, 3] := Temp;
end;

class procedure TUAES.InvMixColumns(var State: TBlock);
  var c, r: Int32;
  var a: array[0..3] of UInt8;
begin
  for c := 0 to 3 do
  begin
    for r := 0 to 3 do a[r] := State[r, c];
    State[0, c] := GMul(a[0], $0e) xor GMul(a[1], $0b) xor GMul(a[2], $0d) xor GMul(a[3], $09);
    State[1, c] := GMul(a[0], $09) xor GMul(a[1], $0e) xor GMul(a[2], $0b) xor GMul(a[3], $0d);
    State[2, c] := GMul(a[0], $0d) xor GMul(a[1], $09) xor GMul(a[2], $0e) xor GMul(a[3], $0b);
    State[3, c] := GMul(a[0], $0b) xor GMul(a[1], $0d) xor GMul(a[2], $09) xor GMul(a[3], $0e);
  end;
end;

class procedure TUAES.InvCipherBlock(
  var State: TBlock;
  const ExpandedKey: TExpandedKey
);
  var Round: Int32;
begin
  AddRoundKey(State, ExpandedKey[14]);
  for Round := 13 downto 1 do
  begin
    InvShiftRows(State);
    InvSubBytes(State);
    AddRoundKey(State, ExpandedKey[Round]);
    InvMixColumns(State);
  end;
  InvShiftRows(State);
  InvSubBytes(State);
  AddRoundKey(State, ExpandedKey[0]);
end;

class function TUAES.Encrypt_AES_PKCS7_ECB_256(
  const Data: TUInt8Array;
  const Key: TKey256
): TUInt8Array;
  var ExpandedKey: TExpandedKey;
  var PaddedData: TUInt8Array;
  var State: TBlock;
  var i, r, c: Int32;
begin
  ExpandedKey := KeyExpansion(Key);
  PaddedData := PadData_PKCS7(Data, 16);
  Result := nil;
  SetLength(Result, Length(PaddedData));
  for i := 0 to (Length(PaddedData) div 16) - 1 do
  begin
    for r := 0 to 3 do
    for c := 0 to 3 do
    begin
      State[r, c] := PaddedData[i * 16 + r + 4 * c];
    end;
    CipherBlock(State, ExpandedKey);
    for r := 0 to 3 do
    for c := 0 to 3 do
    begin
      Result[i * 16 + r + 4 * c] := State[r, c];
    end;
  end;
end;

class function TUAES.Decrypt_AES_PKCS7_ECB_256(
  const Cipher: TUInt8Array;
  const Key: TKey256
): TUInt8Array;
  var ExpandedKey: TExpandedKey;
  var State: TBlock;
  var DataPadded: TUInt8Array;
  var i, r, c: Int32;
begin
  ExpandedKey := KeyExpansion(Key);
  DataPadded := nil;
  SetLength(DataPadded, Length(Cipher));
  for i := 0 to (Length(Cipher) div 16) - 1 do
  begin
    for r := 0 to 3 do
    for c := 0 to 3 do
    begin
      State[r, c] := Cipher[i * 16 + r + 4 * c];
    end;
    InvCipherBlock(State, ExpandedKey);
    for r := 0 to 3 do
    for c := 0 to 3 do
    begin
      DataPadded[i * 16 + r + 4 * c] := State[r, c];
    end;
  end;
  Result := UnpadData_PKCS7(DataPadded);
end;

class function TUAES.Encrypt_AES_PKCS7_CBC_256(
  const Data: TUInt8Array;
  const Key: TKey256;
  const IV: TInitVector
): TUInt8Array;
  var ExpandedKey: TExpandedKey;
  var PaddedData: TUInt8Array;
  var PrevCipherBlock: TInitVector;
  var State: TBlock;
  var i, j, r: Int32;
begin
  ExpandedKey := KeyExpansion(Key);
  PaddedData := PadData_PKCS7(Data, 16);
  Result := nil;
  SetLength(Result, Length(PaddedData));
  PrevCipherBlock := IV;
  for i := 0 to (Length(PaddedData) div 16) - 1 do
  begin
    for j := 0 to 15 do
    begin
      State[j mod 4, j div 4] := PaddedData[i * 16 + j] xor PrevCipherBlock[j];
    end;
    CipherBlock(State, ExpandedKey);
    for j := 0 to 15 do
    begin
      r := i * 16 + j;
      Result[r] := State[j mod 4, j div 4];
      PrevCipherBlock[j] := Result[r];
    end;
  end;
end;

class function TUAES.Decrypt_AES_PKCS7_CBC_256(
  const Cipher: TUInt8Array;
  const Key: TKey256;
  const IV: TInitVector
): TUInt8Array;
  var ExpandedKey: TExpandedKey;
  var State: TBlock;
  var DataPadded: TUInt8Array;
  var PrevCipherBlock, CurCipherBlock: TInitVector;
  var i, j: Int32;
begin
  ExpandedKey := KeyExpansion(Key);
  DataPadded := nil;
  SetLength(DataPadded, Length(Cipher));
  PrevCipherBlock := IV;
  for i := 0 to (Length(Cipher) div 16) - 1 do
  begin
    for j := 0 to 15 do
    begin
      CurCipherBlock[j] := Cipher[i * 16 + j];
      State[j mod 4, j div 4] := CurCipherBlock[j];
    end;
    InvCipherBlock(State, ExpandedKey);
    for j := 0 to 15 do
    begin
      DataPadded[i * 16 + j] := State[j mod 4, j div 4] xor PrevCipherBlock[j];
    end;
    PrevCipherBlock := CurCipherBlock;
  end;
  Result := UnpadData_PKCS7(DataPadded);
end;

class function TUAES.Process_AES_CTR_256(
  const Input: TUInt8Array;
  const Key: TKey256;
  const IV: TInitVector
): TUInt8Array;
  var ExpandedKey: TExpandedKey;
  var State: TBlock;
  var CounterBlock, KeystreamBlock: TInitVector;
  var i, j: Int32;
begin
  ExpandedKey := KeyExpansion(Key);
  Result := nil;
  SetLength(Result, Length(Input));
  CounterBlock := IV;
  for i := 0 to High(Input) do
  begin
    if (i mod 16) = 0 then
    begin
      for j := 0 to 15 do
      begin
        State[j mod 4, j div 4] := CounterBlock[j];
      end;
      CipherBlock(State, ExpandedKey);
      for j := 0 to 15 do
      begin
        KeystreamBlock[j] := State[j mod 4, j div 4];
      end;
      for j := 15 downto 0 do
      begin
        Inc(CounterBlock[j]);
        if CounterBlock[j] <> 0 then Break;
      end;
    end;
    Result[i] := Input[i] xor KeystreamBlock[i mod 16];
  end;
end;

function UEncrypt_AES_PKCS7_ECB_256(
  const Data: TUInt8Array;
  const Key: TUAES.TKey256
): TUInt8Array;
begin
  Result := TUAES.Encrypt_AES_PKCS7_ECB_256(Data, Key);
end;

function UDecrypt_AES_PKCS7_ECB_256(
  const Cipher: TUInt8Array;
  const Key: TUAES.TKey256
): TUInt8Array;
begin
  Result := TUAES.Decrypt_AES_PKCS7_ECB_256(Cipher, Key);
end;

function UEncrypt_AES_PKCS7_CBC_256(
  const Data: TUInt8Array;
  const Key: TUAES.TKey256;
  const IV: TUAES.TInitVector
): TUInt8Array;
begin
  Result := TUAES.Encrypt_AES_PKCS7_CBC_256(Data, Key, IV);
end;

function UDecrypt_AES_PKCS7_CBC_256(
  const Cipher: TUInt8Array;
  const Key: TUAES.TKey256;
  const IV: TUAES.TInitVector
): TUInt8Array;
begin
  Result := TUAES.Decrypt_AES_PKCS7_CBC_256(Cipher, Key, IV);
end;

function UEncrypt_AES_CTR_256(
  const Data: TUInt8Array;
  const Key: TUAES.TKey256;
  const IV: TUAES.TInitVector
): TUInt8Array;
begin
  Result := TUAES.Process_AES_CTR_256(Data, Key, IV);
end;

function UDecrypt_AES_CTR_256(
  const Cipher: TUInt8Array;
  const Key: TUAES.TKey256;
  const IV: TUAES.TInitVector
): TUInt8Array;
begin
  Result := TUAES.Process_AES_CTR_256(Cipher, Key, IV);
end;

end.
