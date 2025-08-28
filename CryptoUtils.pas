unit CryptoUtils;

{$include PasUtilsMode.inc}

interface

uses
  SysUtils,
  Classes,
  CommonUtils;

type TUDigestMD5 = array[0..15] of UInt8;
type TUDigestSHA1 = array[0..19] of UInt8;
type TUDigestSHA2_256 = array[0..31] of UInt8;
type TUDigestSHA2_512 = array[0..63] of UInt8;
type TUDigestSHA3_224 = array[0..27] of UInt8;
type TUDigestSHA3_256 = array[0..31] of UInt8;
type TUDigestSHA3_384 = array[0..47] of UInt8;
type TUDigestSHA3_512 = array[0..63] of UInt8;
type TUFuncDigest = function (const Data: TUInt8Array): TUInt8Array;
type TUFuncAuth = function (const Key, Data: TUInt8Array): TUInt8Array;

type TUCryptoInt = TUInt8192;
type TUCryptoIntArray = array of TUCryptoInt;

type TUDigestMD5_Impl = type helper for TUDigestMD5
private
  class function GetFunc: TUFuncDigest; static; inline;
public
  class property Func: TUFuncDigest read GetFunc;
  class function Make(const Data: TUInt8Array): TUInt8Array; static;
end;

type TUDigestSHA1_Impl = type helper for TUDigestSHA1
private
  class function GetFunc: TUFuncDigest; static; inline;
public
  class property Func: TUFuncDigest read GetFunc;
  class function Make(const Data: TUInt8Array): TUInt8Array; static;
end;

type TUDigestSHA2_256_Impl = type helper for TUDigestSHA2_256
private
  class function GetFunc: TUFuncDigest; static; inline;
public
  class property Func: TUFuncDigest read GetFunc;
  class function Make(const Data: TUInt8Array): TUInt8Array; static;
end;

type TUDigestSHA2_512_Impl = type helper for TUDigestSHA2_512
private
  class function GetFunc: TUFuncDigest; static; inline;
public
  class property Func: TUFuncDigest read GetFunc;
  class function Make(const Data: TUInt8Array): TUInt8Array; static;
end;

type TUDigestSHA3_224_Impl = type helper for TUDigestSHA3_224
private
  class function GetFunc: TUFuncDigest; static; inline;
public
  class property Func: TUFuncDigest read GetFunc;
  class function Make(const Data: TUInt8Array): TUInt8Array; static;
end;

type TUDigestSHA3_256_Impl = type helper for TUDigestSHA3_256
private
  class function GetFunc: TUFuncDigest; static; inline;
public
  class property Func: TUFuncDigest read GetFunc;
  class function Make(const Data: TUInt8Array): TUInt8Array; static;
end;

type TUDigestSHA3_384_Impl = type helper for TUDigestSHA3_384
private
  class function GetFunc: TUFuncDigest; static; inline;
public
  class property Func: TUFuncDigest read GetFunc;
  class function Make(const Data: TUInt8Array): TUInt8Array; static;
end;

type TUDigestSHA3_512_Impl = type helper for TUDigestSHA3_512
private
  class function GetFunc: TUFuncDigest; static; inline;
public
  class property Func: TUFuncDigest read GetFunc;
  class function Make(const Data: TUInt8Array): TUInt8Array; static;
end;

type TURSA = record
public
  type TKey = record
    n: TUCryptoInt; // modulus
    e: TUCryptoInt; // public exponent
    d: TUCryptoInt; // private exponent
    p: TUCryptoInt; // first prime
    q: TUCryptoInt; // second prime
    exp1: TUCryptoInt; // d mod (p - 1)
    exp2: TUCryptoInt; // d mod (q - 1)
    c: TUCryptoInt; // q^-1 mod p
    function Size: Uint32; // bit size
    function IsPublic: Boolean;
    function IsPrivate: Boolean;
    function IsCRT: Boolean;
    function IsValid: Boolean;
    class function MakeInvalid: TKey; static;
  end;
private
  const OID_RSA: array[0..8] of UInt8 = (
    $2a, $86, $48, $86, $f7, $0d, $01, $01, $01
  ); // 1.2.840.113549.1.1.1
  const OID_PBES2: array[0..8] of UInt8 = (
    $2a, $86, $48, $86, $f7, $0d, $01, $05, $0d
  ); // 1.2.840.113549.1.5.13
  const OID_PBKDF2: array[0..8] of UInt8 = (
    $2a, $86, $48, $86, $f7, $0d, $01, $05, $0c
  ); // 1.2.840.113549.1.5.12
  const OID_SHA1: array[0..4] of UInt8 = (
    $2B, $0E, $03, $02, $1A
  ); // 1.3.14.3.2.26
  const OID_SHA_256: array[0..8] of UInt8 = (
    $60, $86, $48, $01, $65, $03, $04, $02, $01
  ); // 2.16.840.1.101.3.4.2.1
  const OID_SHA_512: array[0..8] of UInt8 = (
    $60, $86, $48, $01, $65, $03, $04, $02, $03
  ); // 2.16.840.1.101.3.4.2.3
  const OID_HMAC_SHA1: array[0..7] of UInt8 = (
    $2a, $86, $48, $86, $f7, $0d, $02, $07
  ); // 1.2.840.113549.2.7
  const OID_HMAC_SHA256: array[0..7] of UInt8 = (
    $2a, $86, $48, $86, $f7, $0d, $02, $09
  ); // 1.2.840.113549.2.9
  const OID_HMAC_SHA512: array[0..7] of UInt8 = (
    $2a, $86, $48, $86, $f7, $0d, $02, $0b
  ); // 1.2.840.113549.2.11
  const OID_DES_CBC: array[0..4] of UInt8 = (
    $2b, $0e, $03, $02, $07
  ); // 1.3.14.3.2.7
  const OID_DES_EDE3_CBC: array[0..7] of UInt8 = (
    $2a, $86, $48, $86, $f7, $0d, $03, $07
  ); // 1.2.840.113549.3.7
  const OID_AES128_ECB: array[0..8] of UInt8 = (
    $60, $86, $48, $01, $65, $03, $04, $01, $01
  ); // 2.16.840.1.101.3.4.1.1
  const OID_AES128_CBC: array[0..8] of UInt8 = (
    $60, $86, $48, $01, $65, $03, $04, $01, $02
  ); // 2.16.840.1.101.3.4.1.2
  const OID_AES192_ECB: array[0..8] of UInt8 = (
    $60, $86, $48, $01, $65, $03, $04, $01, $15
  ); // 2.16.840.1.101.3.4.1.21
  const OID_AES192_CBC: array[0..8] of UInt8 = (
    $60, $86, $48, $01, $65, $03, $04, $01, $16
  ); // 2.16.840.1.101.3.4.1.22
  const OID_AES256_ECB: array[0..8] of UInt8 = (
    $60, $86, $48, $01, $65, $03, $04, $01, $29
  ); // 2.16.840.1.101.3.4.1.41
  const OID_AES256_CBC: array[0..8] of UInt8 = (
    $60, $86, $48, $01, $65, $03, $04, $01, $2a
  ); // 2.16.840.1.101.3.4.1.42
  type TMakePrimeContext = record
    BitCount: Int32;
    Primes: array of TUCryptoInt;
    CurItem: Int32;
    Lock: TUCriticalSection;
    Done: Boolean;
  end;
  type PMakePrimeContext = ^TMakePrimeContext;
  type TMakePrimeThread = class (TThread)
  public
    var Context: PMakePrimeContext;
    var RandSeed: UInt32;
    procedure Execute; override;
  end;
  class function ModInverse(const e, phi: TUCryptoInt): TUCryptoInt; static;
  class function GCD(const a, b: TUCryptoInt): TUCryptoInt; static;
  class function PowMod(const Base, Exp, Modulus: TUCryptoInt): TUCryptoInt; static;
  class function MillerRabinTest(const Number: TUCryptoInt; const Iterations: Int32 = 50): Boolean; static;
  class function XORBytes(const a, b: TUInt8Array): TUInt8Array; static;
  class function PackDER(const Number: TUCryptoInt): TUInt8Array; static;
  class function UnpackDER(const Bytes: TUInt8Array): TUCryptoInt; static;
  class procedure DebugASN1(const Bytes: TUInt8Array; const Offset: String = ''); static;
  class function EncodeTLV(const Tag: UInt8; const Value: TUInt8Array): TUInt8Array; static;
  class function DecodeTLV(const Data: TUInt8Array; var Index: Int32; out Tag: UInt8): TUInt8Array; static;
  class function ASN1ToBase64(const DataASN: TUInt8Array; const Header, Footer: String): String; static;
  class function Base64ToASN1(const Base64: String; const Header, Footer: String): TUInt8Array; static;
  class function MakePrime(const BitCount: Int32 = 1024): TUCryptoInt; static;
  class function MakePrimes(
    const PrimeCount: Int32;
    const BitCount: Int32 = 1024;
    const ThreadCount: Int32 = 8
  ): TUCryptoIntArray; static;
  class function MakeKey(
    const BitCount: UInt32 = 2048;
    const Threads: Int32 = 16
  ): TURSA.TKey; static;
  class function PackData_PKCS1(
    const Data: Pointer;
    const DataSize: UInt32;
    const BlockSize: UInt32 = 2048
  ): TUCryptoInt; static;
  class function PackData_PKCS1(
    const Data: TUInt8Array;
    const BlockSize: UInt32 = 2048
  ): TUCryptoInt; static;
  class function UnpackData_PKCS1(
    const Block: TUCryptoInt;
    const BlockSize: UInt32 = 2048
  ): TUInt8Array; static;
  class function UnpackStr_PKCS1(
    const Block: TUCryptoInt;
    const BlockSize: UInt32 = 2048
  ): String; static;
  class function PackData_OAEP(
    const Data: Pointer;
    const DataSize: UInt32;
    const BlockSize: UInt32 = 2048
  ): TUCryptoInt; static;
  class function PackData_OAEP(
    const Data: TUInt8Array;
    const BlockSize: UInt32 = 2048
  ): TUCryptoInt; static;
  class function UnpackData_OAEP(
    const Block: TUCryptoInt;
    const BlockSize: UInt32 = 2048
  ): TUInt8Array; static;
  class function UnpackStr_OAEP(
    const Block: TUCryptoInt;
    const BlockSize: UInt32 = 2048
  ): String; static;
  class function PackData_Singature(
    const Data: Pointer;
    const DataSize: UInt32;
    const BlockSize: UInt32 = 2048
  ): TUCryptoInt; static;
  class function UnpackData_Singature(
    const Block: TUCryptoInt;
    const BlockSize: UInt32 = 2048
  ): TUInt8Array; static;
  class function Encrypt_PKCS1(
    const Data: Pointer;
    const DataSize: UInt32;
    const Key: TURSA.TKey
  ): TUCryptoInt; static;
  class function Decrypt_PKCS1_Str(
    const Cipher: TUCryptoInt;
    const Key: TURSA.TKey
  ): String; static;
  class function Decrypt_PKCS1(
    const Cipher: TUCryptoInt;
    const Key: TURSA.TKey
  ): TUInt8Array; static;
  class function Encrypt_OAEP(
    const Data: Pointer;
    const DataSize: UInt32;
    const Key: TURSA.TKey
  ): TUCryptoInt; static;
  class function Decrypt_OAEP_Str(
    const Cipher: TUCryptoInt;
    const Key: TURSA.TKey
  ): String; static;
  class function Decrypt_OAEP(
    const Cipher: TUCryptoInt;
    const Key: TURSA.TKey
  ): TUInt8Array; static;
  class function Decrypt_CRT(
    const Cipher: TUCryptoInt;
    const Key: TURSA.TKey
  ): TUCryptoInt; static;
  class function Decrypt(
    const Cipher: TUCryptoInt;
    const Key: TURSA.TKey
  ): TUCryptoInt; static;
  class function Sign_SHA256(
    const Data: TUInt8Array;
    const Key: TKey
  ): TUInt8Array; static;
  class function Sign_SHA512(
    const Data: TUInt8Array;
    const Key: TKey
  ): TUInt8Array; static;
  class function Verify(
    const Data, Signature: TUInt8Array;
    const Key: TKey
  ): Boolean; static;
  class function ExportKeyPrivate_PKCS1_DER(const Key: TURSA.TKey): TUInt8Array; static;
  class function ImportKeyPrivate_PKCS1_DER(const Key: TUInt8Array): TURSA.TKey; static;
  class function ExportKeyPublic_PKCS1_DER(const Key: TURSA.TKey): TUInt8Array; static;
  class function ImportKeyPublic_PKCS1_DER(const Key: TUInt8Array): TURSA.TKey; static;
  class function ExportKeyPrivate_PKCS8_DER(const Key: TURSA.TKey): TUInt8Array; static;
  class function ImportKeyPrivate_PKCS8_DER(const Key: TUInt8Array): TURSA.TKey; static;
  class function ExportKeyPrivate_PKCS1(const Key: TURSA.TKey): String; static;
  class function ExportKeyPublic_PKCS1(const Key: TURSA.TKey): String; static;
  class function ExportKeyPrivate_PKCS8(const Key: TURSA.TKey): String; static;
  class function ExportKeyPrivateEncrypted_PKCS8(
    const EncryptedKey: TUint8Array;
    const HMAC_OID: TUInt8Array;
    const ALG_OID: TUInt8Array;
    const Salt, IV: TUInt8Array;
    const IterationCount: Int32
  ): String; static;
  class function ExportKeyPrivateEncrypted_PKCS8_SHA256_AES128_CBC(
    const Key: TURSA.TKey;
    const Password: TUInt8Array;
    const IterationCount: Int32 = 600000
  ): String; static;
  class function ExportKeyPrivateEncrypted_PKCS8_SHA256_AES192_CBC(
    const Key: TURSA.TKey;
    const Password: TUInt8Array;
    const IterationCount: Int32 = 600000
  ): String; static;
  class function ExportKeyPrivateEncrypted_PKCS8_SHA256_AES256_CBC(
    const Key: TURSA.TKey;
    const Password: TUInt8Array;
    const IterationCount: Int32 = 600000
  ): String; static;
  class function ExportKeyPrivateEncrypted_PKCS8_SHA512_AES128_CBC(
    const Key: TURSA.TKey;
    const Password: TUInt8Array;
    const IterationCount: Int32 = 600000
  ): String; static;
  class function ExportKeyPrivateEncrypted_PKCS8_SHA512_AES192_CBC(
    const Key: TURSA.TKey;
    const Password: TUInt8Array;
    const IterationCount: Int32 = 600000
  ): String; static;
  class function ExportKeyPrivateEncrypted_PKCS8_SHA512_AES256_CBC(
    const Key: TURSA.TKey;
    const Password: TUInt8Array;
    const IterationCount: Int32 = 600000
  ): String; static;
  class function ExportKeyPublic_X509(const Key: TURSA.TKey): String; static;
  class function ImportKeyPrivate_PKCS1(const Key: String): TURSA.TKey; static;
  class function ImportKeyPublic_PKCS1(const Key: String): TURSA.TKey; static;
  class function ImportKeyPrivate_PKCS8(const Key: String): TURSA.TKey; static;
  class function ImportKeyPrivateEncrypted_PKCS5(
    const Key: String;
    const Password: TUInt8Array
  ): TURSA.TKey; static;
  class function ImportKeyPrivateEncrypted_PKCS8(
    const Key: String;
    const Password: TUInt8Array
  ): TURSA.TKey; static;
  class function ImportKeyPublic_X509(const Key: String): TURSA.TKey; static;
  class function ImportKey(const Key: String; const Password: TUInt8Array): TURSA.TKey; static;
end;

type TUMontgomeryReduction = record
private
  N: TUCryptoInt;
  N_prime: UInt32;
  R2_mod_N: TUCryptoInt;
  NumItems: Int32;
public
  procedure Init(const Modulus: TUCryptoInt);
  function Conv(const Number: TUCryptoInt): TUCryptoInt;
  function Mul(const a, b: TUCryptoInt): TUCryptoInt;
end;

type TUAES = record
public
  type TKey128 = array[0..15] of UInt8;
  type TKey192 = array[0..23] of UInt8;
  type TKey256 = array[0..31] of UInt8;
  type TInitVector = array[0..15] of UInt8;
  type TTag = array[0..15] of UInt8;
  class function MakeIV(const Bytes: TUInt8Array): TInitVector; static;
  class function MakeIV: TInitVector; static;
  class function MakeKey128(const Bytes: TUInt8Array): TKey128; static;
  class function MakeKey192(const Bytes: TUInt8Array): TKey192; static;
  class function MakeKey256(const Bytes: TUInt8Array): TKey256; static;
private
  type TBlock = array[0..3, 0..3] of UInt8;
  type TExpandedKey = array[0..14] of TBlock;
  const InitVectorZero: TInitVector = (
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
  );
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
  class procedure GF128_Mul(var X: TInitVector; const Y: TInitVector); static;
  class function KeyExpansion(const Key: TKey128): TExpandedKey; static;
  class function KeyExpansion(const Key: TKey192): TExpandedKey; static;
  class function KeyExpansion(const Key: TKey256): TExpandedKey; static;
  class function PadData_PKCS7(const Data: TUInt8Array; const BlockSize: Int32): TUInt8Array; static;
  class function UnpadData_PKCS7(const Data: TUInt8Array): TUInt8Array; static;
  class procedure AddRoundKey(var State: TBlock; const RoundKey: TBlock); static;
  class function GMul(a, b: UInt8): UInt8; static;
  class procedure SubBytes(var State: TBlock); static;
  class procedure ShiftRows(var State: TBlock); static;
  class procedure MixColumns(var State: TBlock); static;
  class procedure CipherBlock(var State: TBlock; const ExpandedKey: TExpandedKey; const NumRounds: Int32); static;
  class procedure InvSubBytes(var State: TBlock); static;
  class procedure InvShiftRows(var State: TBlock); static;
  class procedure InvMixColumns(var State: TBlock); static;
  class procedure InvCipherBlock(var State: TBlock; const ExpandedKey: TExpandedKey; const NumRounds: Int32); static;
  class function Encrypt_PKCS7_ECB(
    const Data: TUInt8Array;
    const ExpandedKey: TExpandedKey;
    const NumRounds: Int32
  ): TUInt8Array; static;
  class function Encrypt_PKCS7_ECB_128(
    const Data: TUInt8Array;
    const Key: TKey128
  ): TUInt8Array; static;
  class function Encrypt_PKCS7_ECB_192(
    const Data: TUInt8Array;
    const Key: TKey192
  ): TUInt8Array; static;
  class function Encrypt_PKCS7_ECB_256(
    const Data: TUInt8Array;
    const Key: TKey256
  ): TUInt8Array; static;
  class function Decrypt_PKCS7_ECB(
    const Cipher: TUInt8Array;
    const ExpandedKey: TExpandedKey;
    const NumRounds: Int32
  ): TUInt8Array; static;
  class function Decrypt_PKCS7_ECB_128(
    const Cipher: TUInt8Array;
    const Key: TKey128
  ): TUInt8Array; static;
  class function Decrypt_PKCS7_ECB_192(
    const Cipher: TUInt8Array;
    const Key: TKey192
  ): TUInt8Array; static;
  class function Decrypt_PKCS7_ECB_256(
    const Cipher: TUInt8Array;
    const Key: TKey256
  ): TUInt8Array; static;
  class function Encrypt_PKCS7_CBC(
    const Data: TUInt8Array;
    const ExpandedKey: TExpandedKey;
    const IV: TInitVector;
    const NumRounds: Int32
  ): TUInt8Array; static;
  class function Encrypt_PKCS7_CBC_128(
    const Data: TUInt8Array;
    const Key: TKey128;
    const IV: TInitVector
  ): TUInt8Array; static;
  class function Encrypt_PKCS7_CBC_192(
    const Data: TUInt8Array;
    const Key: TKey192;
    const IV: TInitVector
  ): TUInt8Array; static;
  class function Encrypt_PKCS7_CBC_256(
    const Data: TUInt8Array;
    const Key: TKey256;
    const IV: TInitVector
  ): TUInt8Array; static;
  class function Decrypt_PKCS7_CBC(
    const Cipher: TUInt8Array;
    const ExpandedKey: TExpandedKey;
    const IV: TInitVector;
    const NumRounds: Int32
  ): TUInt8Array; static;
  class function Decrypt_PKCS7_CBC_128(
    const Cipher: TUInt8Array;
    const Key: TKey128;
    const IV: TInitVector
  ): TUInt8Array; static;
  class function Decrypt_PKCS7_CBC_192(
    const Cipher: TUInt8Array;
    const Key: TKey192;
    const IV: TInitVector
  ): TUInt8Array; static;
  class function Decrypt_PKCS7_CBC_256(
    const Cipher: TUInt8Array;
    const Key: TKey256;
    const IV: TInitVector
  ): TUInt8Array; static;
  class function Process_CTR(
    const Input: TUInt8Array;
    const ExpandedKey: TExpandedKey;
    const IV: TInitVector;
    const NumRounds: Int32
  ): TUInt8Array; static;
  class function Process_CTR_128(
    const Input: TUInt8Array;
    const Key: TKey128;
    const IV: TInitVector
  ): TUInt8Array; static;
  class function Process_CTR_192(
    const Input: TUInt8Array;
    const Key: TKey192;
    const IV: TInitVector
  ): TUInt8Array; static;
  class function Process_CTR_256(
    const Input: TUInt8Array;
    const Key: TKey256;
    const IV: TInitVector
  ): TUInt8Array; static;
  class function Process_GCM(
    const Input: TUInt8Array;
    const ExpandedKey: TExpandedKey;
    const Nonce: TInitVector;
    const AAD: TUInt8Array;
    const IsEncrypting: Boolean;
    const NumRounds: Int32;
    out AuthTag: TTag
  ): TUInt8Array; static;
  class function Process_GCM_128(
    const Input: TUInt8Array;
    const Key: TKey128;
    const Nonce: TInitVector;
    const AAD: TUInt8Array;
    const IsEncrypting: Boolean;
    out AuthTag: TTag
  ): TUInt8Array; static;
  class function Process_GCM_192(
    const Input: TUInt8Array;
    const Key: TKey192;
    const Nonce: TInitVector;
    const AAD: TUInt8Array;
    const IsEncrypting: Boolean;
    out AuthTag: TTag
  ): TUInt8Array; static;
  class function Process_GCM_256(
    const Input: TUInt8Array;
    const Key: TKey256;
    const Nonce: TInitVector;
    const AAD: TUInt8Array;
    const IsEncrypting: Boolean;
    out AuthTag: TTag
  ): TUInt8Array; static;
end;

type TUDES = record
public
  type TKey = array[0..7] of UInt8;
  type TKey3 = array[0..23] of UInt8;
  type TInitVector = array[0..7] of UInt8;
  class function MakeIV(const Bytes: TUInt8Array): TInitVector; static;
  class function MakeIV: TInitVector; static;
  class function MakeKey(const Bytes: TUInt8Array): TKey; static;
  class function MakeKey3(const Bytes: TUInt8Array): TKey3; static;
private
  type T64BitBlock = UInt64;
  type T48BitKey = array[0..5] of UInt8;
  type TSubKeys = array[0..15] of T48BitKey;
  const IP: array[0..63] of UInt8 = ( // Initial Permutation
    58, 50, 42, 34, 26, 18, 10, 2, 60, 52, 44, 36, 28, 20, 12, 4,
    62, 54, 46, 38, 30, 22, 14, 6, 64, 56, 48, 40, 32, 24, 16, 8,
    57, 49, 41, 33, 25, 17, 9, 1, 59, 51, 43, 35, 27, 19, 11, 3,
    61, 53, 45, 37, 29, 21, 13, 5, 63, 55, 47, 39, 31, 23, 15, 7
  );
  const FP: array[0..63] of UInt8 = ( // Final Permutation (FP), the inverse of IP
    40, 8, 48, 16, 56, 24, 64, 32, 39, 7, 47, 15, 55, 23, 63, 31,
    38, 6, 46, 14, 54, 22, 62, 30, 37, 5, 45, 13, 53, 21, 61, 29,
    36, 4, 44, 12, 52, 20, 60, 28, 35, 3, 43, 11, 51, 19, 59, 27,
    34, 2, 42, 10, 50, 18, 58, 26, 33, 1, 41, 9, 49, 17, 57, 25
  );
  const PC1: array[0..55] of UInt8 = ( // Permuted Choice 1 (PC-1) for key schedule
    57, 49, 41, 33, 25, 17, 9, 1, 58, 50, 42, 34, 26, 18,
    10, 2, 59, 51, 43, 35, 27, 19, 11, 3, 60, 52, 44, 36,
    63, 55, 47, 39, 31, 23, 15, 7, 62, 54, 46, 38, 30, 22,
    14, 6, 61, 53, 45, 37, 29, 21, 13, 5, 28, 20, 12, 4
  );
  const PC2: array[0..47] of UInt8 = ( // Permuted Choice 2 (PC-2) for key schedule
    14, 17, 11, 24, 1, 5, 3, 28, 15, 6, 21, 10,
    23, 19, 12, 4, 26, 8, 16, 7, 27, 20, 13, 2,
    41, 52, 31, 37, 47, 55, 30, 40, 51, 45, 33, 48,
    44, 49, 39, 56, 34, 53, 46, 42, 50, 36, 29, 32
  );
  const E: array[0..47] of UInt8 = ( // Expansion Permutation (E) for round function
    32, 1, 2, 3, 4, 5, 4, 5, 6, 7, 8, 9,
    8, 9, 10, 11, 12, 13, 12, 13, 14, 15, 16, 17,
    16, 17, 18, 19, 20, 21, 20, 21, 22, 23, 24, 25,
    24, 25, 26, 27, 28, 29, 28, 29, 30, 31, 32, 1
  );
  const P: array[0..31] of UInt8 = ( // Permutation (P) for round function
    16, 7, 20, 21, 29, 12, 28, 17, 1, 15, 23, 26, 5, 18, 31, 10,
    2, 8, 24, 14, 32, 27, 3, 9, 19, 13, 30, 6, 22, 11, 4, 25
  );
  const SBoxes: array[0..7, 0..63] of UInt8 = ( // S-Boxes (8 tables)
    ( // S1
      14,4,13,1,2,15,11,8,3,10,6,12,5,9,0,7,
      0,15,7,4,14,2,13,1,10,6,12,11,9,5,3,8,
      4,1,14,8,13,6,2,11,15,12,9,7,3,10,5,0,
      15,12,8,2,4,9,1,7,5,11,3,14,10,0,6,13
    ), ( // S2
      15,1,8,14,6,11,3,4,9,7,2,13,12,0,5,10,
      3,13,4,7,15,2,8,14,12,0,1,10,6,9,11,5,
      0,14,7,11,10,4,13,1,5,8,12,6,9,3,2,15,
      13,8,10,1,3,15,4,2,11,6,7,12,0,5,14,9
    ), ( // S3
      10,0,9,14,6,3,15,5,1,13,12,7,11,4,2,8,
      13,7,0,9,3,4,6,10,2,8,5,14,12,11,15,1,
      13,6,4,9,8,15,3,0,11,1,2,12,5,10,14,7,
      1,10,13,0,6,9,8,7,4,15,14,3,11,5,2,12
    ), ( // S4
      7,13,14,3,0,6,9,10,1,2,8,5,11,12,4,15,
      13,8,11,5,6,15,0,3,4,7,2,12,1,10,14,9,
      10,6,9,0,12,11,7,13,15,1,3,14,5,2,8,4,
      3,15,0,6,10,1,13,8,9,4,5,11,12,7,2,14
    ), ( // S5
      2,12,4,1,7,10,11,6,8,5,3,15,13,0,14,9,
      14,11,2,12,4,7,13,1,5,0,15,10,3,9,8,6,
      4,2,1,11,10,13,7,8,15,9,12,5,6,3,0,14,
      11,8,12,7,1,14,2,13,6,15,0,9,10,4,5,3
    ), ( // S6
      12,1,10,15,9,2,6,8,0,13,3,4,14,7,5,11,
      10,15,4,2,7,12,9,5,6,1,13,14,0,11,3,8,
      9,14,15,5,2,8,12,3,7,0,4,10,1,13,11,6,
      4,3,2,12,9,5,15,10,11,14,1,7,6,0,8,13
    ), ( // S7
      4,11,2,14,15,0,8,13,3,12,9,7,5,10,6,1,
      13,0,11,7,4,9,1,10,14,3,5,12,2,15,8,6,
      1,4,11,13,12,3,7,14,10,15,6,8,0,5,9,2,
      6,11,13,8,1,4,10,7,9,5,0,15,14,2,3,12
    ), ( // S8
      13,2,8,4,6,15,11,1,10,9,3,14,5,0,12,7,
      1,15,13,8,10,3,7,4,12,5,6,11,0,14,9,2,
      7,11,4,1,9,12,14,2,0,6,10,13,15,3,5,8,
      2,1,14,7,4,10,8,13,15,12,9,0,3,5,6,11
    )
  );
  const KeyShifts: array[0..15] of UInt8 = ( // Key schedule shifts
    1,1,2,2,2,2,2,2,1,2,2,2,2,2,2,1
  );
  class function Permute(const Input: T64BitBlock; const Table: array of Byte; const InSize, OutSize: Int32): T64BitBlock; static;
  class function GenerateSubKeys(const Key: TKey): TSubKeys; static;
  class function FeistelF(R: UInt32; const SubKey: T48BitKey): UInt32; static;
  class procedure ProcessDESBlock(var Block: T64BitBlock; const SubKeys: TSubKeys; const IsEncrypting: Boolean); static;
  class function PadData_PKCS7(const Data: TUInt8Array; const BlockSize: Int32): TUInt8Array; static;
  class function UnpadData_PKCS7(const Data: TUInt8Array): TUInt8Array; static;
  class function Encrypt_PKCS7_ECB(const Data: TUInt8Array; const Key: TKey): TUInt8Array; static;
  class function Decrypt_PKCS7_ECB(const Cipher: TUInt8Array; const Key: TKey): TUInt8Array; static;
  class function Encrypt_PKCS7_CBC(const Data: TUInt8Array; const Key: TKey; const IV: TInitVector): TUInt8Array; static;
  class function Decrypt_PKCS7_CBC(const Cipher: TUInt8Array; const Key: TKey; const IV: TInitVector): TUInt8Array; static;
  class function Encrypt_PKCS7_Triple_ECB(const Data: TUInt8Array; const Key: TKey3): TUInt8Array; static;
  class function Decrypt_PKCS7_Triple_ECB(const Cipher: TUInt8Array; const Key: TKey3): TUInt8Array; static;
  class function Encrypt_PKCS7_Triple_CBC(const Data: TUInt8Array; const Key: TKey3; const IV: TInitVector): TUInt8Array; static;
  class function Decrypt_PKCS7_Triple_CBC(const Cipher: TUInt8Array; const Key: TKey3; const IV: TInitVector): TUInt8Array; static;
  class function Process_CTR(const Input: TUInt8Array; const Key: TKey; const Nonce: TInitVector): TUInt8Array; static;
  class function Process_Triple_CTR(const Input: TUInt8Array; const Key: TKey3; const Nonce: TInitVector): TUInt8Array; static;
end;

function UMD5(const Data: Pointer; const DataSize: UInt32): TUDigestMD5;
function UMD5(const Data: TUInt8Array): TUDigestMD5;
function UMD5(const Data: String): TUDigestMD5;

function USHA1(const Data: Pointer; const DataSize: UInt32): TUDigestSHA1;
function USHA1(const Data: TUInt8Array): TUDigestSHA1;
function USHA1(const Data: String): TUDigestSHA1;

function USHA2_256(const Data: Pointer; const DataSize: UInt32): TUDigestSHA2_256;
function USHA2_256(const Data: TUInt8Array): TUDigestSHA2_256;
function USHA2_256(const Data: String): TUDigestSHA2_256;

function USHA2_512(const Data: Pointer; const DataSize: UInt32): TUDigestSHA2_512;
function USHA2_512(const Data: TUInt8Array): TUDigestSHA2_512;
function USHA2_512(const Data: String): TUDigestSHA2_512;

function USHA3_224(const Data: Pointer; const DataSize: UInt32): TUDigestSHA3_224;
function USHA3_224(const Data: TUInt8Array): TUDigestSHA3_224;
function USHA3_224(const Data: String): TUDigestSHA3_224;

function USHA3_256(const Data: Pointer; const DataSize: UInt32): TUDigestSHA3_256;
function USHA3_256(const Data: TUInt8Array): TUDigestSHA3_256;
function USHA3_256(const Data: String): TUDigestSHA3_256;

function USHA3_384(const Data: Pointer; const DataSize: UInt32): TUDigestSHA3_384;
function USHA3_384(const Data: TUInt8Array): TUDigestSHA3_384;
function USHA3_384(const Data: String): TUDigestSHA3_384;

function USHA3_512(const Data: Pointer; const DataSize: UInt32): TUDigestSHA3_512;
function USHA3_512(const Data: TUInt8Array): TUDigestSHA3_512;
function USHA3_512(const Data: String): TUDigestSHA3_512;

function UDigestMD5(const Data: TUInt8Array): TUInt8Array;
function UDigestSHA1(const Data: TUInt8Array): TUInt8Array;
function UDigestSHA2_256(const Data: TUInt8Array): TUInt8Array;
function UDigestSHA2_512(const Data: TUInt8Array): TUInt8Array;
function UDigestSHA3_224(const Data: TUInt8Array): TUInt8Array;
function UDigestSHA3_256(const Data: TUInt8Array): TUInt8Array;
function UDigestSHA3_384(const Data: TUInt8Array): TUInt8Array;
function UDigestSHA3_512(const Data: TUInt8Array): TUInt8Array;

generic function UHMAC<TDigest>(
  const Key, Data: TUInt8Array
): TDigest;
function UHMAC_SHA1(const Key, Data: TUInt8Array): TUDigestSHA1;
function UHMAC_SHA256(const Key, Data: TUInt8Array): TUDigestSHA2_256;
function UHMAC_SHA512(const Key, Data: TUInt8Array): TUDigestSHA2_512;
function UHMAC_SHA3_224(const Key, Data: TUInt8Array): TUDigestSHA3_224;
function UHMAC_SHA3_256(const Key, Data: TUInt8Array): TUDigestSHA3_256;
function UHMAC_SHA3_384(const Key, Data: TUInt8Array): TUDigestSHA3_384;
function UHMAC_SHA3_512(const Key, Data: TUInt8Array): TUDigestSHA3_512;

function UAuthHMAC_SHA1(const Key, Data: TUInt8Array): TUInt8Array;
function UAuthHMAC_SHA2_256(const Key, Data: TUInt8Array): TUInt8Array;
function UAuthHMAC_SHA2_512(const Key, Data: TUInt8Array): TUInt8Array;
function UAuthHMAC_SHA3_224(const Key, Data: TUInt8Array): TUInt8Array;
function UAuthHMAC_SHA3_256(const Key, Data: TUInt8Array): TUInt8Array;
function UAuthHMAC_SHA3_384(const Key, Data: TUInt8Array): TUInt8Array;
function UAuthHMAC_SHA3_512(const Key, Data: TUInt8Array): TUInt8Array;

function USign_SHA256(const Data: TUInt8Array; const Key: TURSA.TKey): TUInt8Array;
function USign_SHA512(const Data: TUInt8Array; const Key: TURSA.TKey): TUInt8Array;
function UVerify(const Data, Signature: TUInt8Array; const Key: TURSA.TKey): Boolean;

function UPBKDF2(
  const FuncAuth: TUFuncAuth;
  const DigestSize: UInt32;
  const Password, Salt: TUInt8Array;
  const KeyLength: Int32;
  const Iterations: Int32 = 600000
): TUInt8Array;
function UPBKDF2_HMAC_SHA1(
  const Password, Salt: TUInt8Array;
  const KeyLength: Int32;
  const Iterations: Int32 = 600000
): TUInt8Array;
function UPBKDF2_HMAC_SHA256(
  const Password, Salt: TUInt8Array;
  const KeyLength: Int32;
  const Iterations: Int32 = 600000
): TUInt8Array;
function UPBKDF2_HMAC_SHA512(
  const Password, Salt: TUInt8Array;
  const KeyLength: Int32;
  const Iterations: Int32 = 600000
): TUInt8Array;

generic function UEvpKDF<TDigest>(
  const Password, Salt: TUInt8Array;
  const KeyLength: Int32;
  const IVLength: Int32;
  const Iterations: Int32;
  out IV: TUInt8Array
): TUInt8Array;
function UEvpKDF_MD5(
  const Password, Salt: TUInt8Array;
  const KeyLength: Int32;
  const IVLength: Int32;
  const Iterations: Int32;
  out IV: TUInt8Array
): TUInt8Array;
function UEvpKDF_SHA256(
  const Password, Salt: TUInt8Array;
  const KeyLength: Int32;
  const IVLength: Int32;
  const Iterations: Int32;
  out IV: TUInt8Array
): TUInt8Array;

function UMGF1_SHA256(const Seed: TUInt8Array; const MaskLen: Int32): TUInt8Array;

function UMakeRSAKey(
  const BitCount: UInt32;
  const Threads: Int32
): TURSA.TKey;
function UExportRSAKey_PKCS1(const Key: TURSA.TKey): String;
function UExportRSAKey_PKCS8(const Key: TURSA.TKey): String;
function UExportRSAKey_PKCS8(const Key: TURSA.TKey; const Password: String; const Iterations: UInt32 = 600000): String;
function UExportRSAKey_X509(const Key: TURSA.TKey): String;
function UImportRSAKey(const KeyASN1: String; const Password: String = ''): TURSA.TKey;
function UEncrypt_RSA_PKCS1(
  const Data: Pointer;
  const DataSize: UInt32;
  const Key: TURSA.TKey
): TUCryptoInt;
function UEncrypt_RSA_PKCS1(
  const Data: TUInt8Array;
  const Key: TURSA.TKey
): TUCryptoInt;
function UEncrypt_RSA_PKCS1_Str(
  const Str: String;
  const Key: TURSA.TKey
): TUCryptoInt;
function UDecrypt_RSA_PKCS1_Str(
  const Cipher: TUCryptoInt;
  const Key: TURSA.TKey
): String;
function UDecrypt_RSA_PKCS1(
  const Cipher: TUCryptoInt;
  const Key: TURSA.TKey
): TUInt8Array;
function UEncrypt_RSA_OAEP(
  const Data: Pointer;
  const DataSize: UInt32;
  const Key: TURSA.TKey
): TUCryptoInt;
function UEncrypt_RSA_OAEP(
  const Data: TUInt8Array;
  const Key: TURSA.TKey
): TUCryptoInt;
function UEncrypt_RSA_OAEP_Str(
  const Str: String;
  const Key: TURSA.TKey
): TUCryptoInt;
function UDecrypt_RSA_OAEP_Str(
  const Cipher: TUCryptoInt;
  const Key: TURSA.TKey
): String;
function UDecrypt_RSA_OAEP(
  const Cipher: TUCryptoInt;
  const Key: TURSA.TKey
): TUInt8Array;

function UEncrypt_AES_PKCS7_ECB_128(
  const Data: TUInt8Array;
  const Key: TUAES.TKey128
): TUInt8Array;
function UDecrypt_AES_PKCS7_ECB_128(
  const Cipher: TUInt8Array;
  const Key: TUAES.TKey128
): TUInt8Array;
function UEncrypt_AES_PKCS7_ECB_192(
  const Data: TUInt8Array;
  const Key: TUAES.TKey192
): TUInt8Array;
function UDecrypt_AES_PKCS7_ECB_192(
  const Cipher: TUInt8Array;
  const Key: TUAES.TKey192
): TUInt8Array;
function UEncrypt_AES_PKCS7_ECB_256(
  const Data: TUInt8Array;
  const Key: TUAES.TKey256
): TUInt8Array;
function UDecrypt_AES_PKCS7_ECB_256(
  const Cipher: TUInt8Array;
  const Key: TUAES.TKey256
): TUInt8Array;
function UEncrypt_AES_PKCS7_CBC_128(
  const Data: TUInt8Array;
  const Key: TUAES.TKey128;
  const IV: TUAES.TInitVector
): TUInt8Array;
function UDecrypt_AES_PKCS7_CBC_128(
  const Cipher: TUInt8Array;
  const Key: TUAES.TKey128;
  const IV: TUAES.TInitVector
): TUInt8Array;
function UEncrypt_AES_PKCS7_CBC_192(
  const Data: TUInt8Array;
  const Key: TUAES.TKey192;
  const IV: TUAES.TInitVector
): TUInt8Array;
function UDecrypt_AES_PKCS7_CBC_192(
  const Cipher: TUInt8Array;
  const Key: TUAES.TKey192;
  const IV: TUAES.TInitVector
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
function UEncrypt_AES_CTR_128(
  const Data: TUInt8Array;
  const Key: TUAES.TKey128;
  const IV: TUAES.TInitVector
): TUInt8Array;
function UDecrypt_AES_CTR_128(
  const Cipher: TUInt8Array;
  const Key: TUAES.TKey128;
  const IV: TUAES.TInitVector
): TUInt8Array;
function UEncrypt_AES_CTR_192(
  const Data: TUInt8Array;
  const Key: TUAES.TKey192;
  const IV: TUAES.TInitVector
): TUInt8Array;
function UDecrypt_AES_CTR_192(
  const Cipher: TUInt8Array;
  const Key: TUAES.TKey192;
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
function UEncrypt_AES_GCM_128(
  const Data: TUInt8Array;
  const Key: TUAES.TKey128;
  const Nonce: TUAES.TInitVector;
  const AAD: TUInt8Array;
  out AuthTag: TUAES.TTag
): TUInt8Array;
function UDecrypt_AES_GCM_128(
  const Cipher: TUInt8Array;
  const Key: TUAES.TKey128;
  const Nonce: TUAES.TInitVector;
  const AAD: TUInt8Array;
  out AuthTag: TUAES.TTag
): TUInt8Array;
function UEncrypt_AES_GCM_192(
  const Data: TUInt8Array;
  const Key: TUAES.TKey192;
  const Nonce: TUAES.TInitVector;
  const AAD: TUInt8Array;
  out AuthTag: TUAES.TTag
): TUInt8Array;
function UDecrypt_AES_GCM_192(
  const Cipher: TUInt8Array;
  const Key: TUAES.TKey192;
  const Nonce: TUAES.TInitVector;
  const AAD: TUInt8Array;
  out AuthTag: TUAES.TTag
): TUInt8Array;
function UEncrypt_AES_GCM_256(
  const Data: TUInt8Array;
  const Key: TUAES.TKey256;
  const Nonce: TUAES.TInitVector;
  const AAD: TUInt8Array;
  out AuthTag: TUAES.TTag
): TUInt8Array;
function UDecrypt_AES_GCM_256(
  const Cipher: TUInt8Array;
  const Key: TUAES.TKey256;
  const Nonce: TUAES.TInitVector;
  const AAD: TUInt8Array;
  out AuthTag: TUAES.TTag
): TUInt8Array;

function UEncrypt_DES_PKCS7_ECB(
  const Data: TUInt8Array;
  const Key: TUDES.TKey
): TUInt8Array;
function UDecrypt_DES_PKCS7_ECB(
  const Cipher: TUInt8Array;
  const Key: TUDES.TKey
): TUInt8Array;
function UEncrypt_DES_PKCS7_CBC(
  const Data: TUInt8Array;
  const Key: TUDES.TKey;
  const IV: TUDES.TInitVector
): TUInt8Array;
function UDecrypt_DES_PKCS7_CBC(
  const Cipher: TUInt8Array;
  const Key: TUDES.TKey;
  const IV: TUDES.TInitVector
): TUInt8Array;
function UEncrypt_DES_PKCS7_CTR(
  const Data: TUInt8Array;
  const Key: TUDES.TKey;
  const Nonce: TUDES.TInitVector
): TUInt8Array;
function UDecrypt_DES_PKCS7_CTR(
  const Cipher: TUInt8Array;
  const Key: TUDES.TKey;
  const Nonce: TUDES.TInitVector
): TUInt8Array;

function UEncrypt_DES_Triple_PKCS7_ECB(
  const Data: TUInt8Array;
  const Key: TUDES.TKey3
): TUInt8Array;
function UDecrypt_DES_Triple_PKCS7_ECB(
  const Cipher: TUInt8Array;
  const Key: TUDES.TKey3
): TUInt8Array;
function UEncrypt_DES_Triple_PKCS7_CBC(
  const Data: TUInt8Array;
  const Key: TUDES.TKey3;
  const IV: TUDES.TInitVector
): TUInt8Array;
function UDecrypt_DES_Triple_PKCS7_CBC(
  const Cipher: TUInt8Array;
  const Key: TUDES.TKey3;
  const IV: TUDES.TInitVector
): TUInt8Array;
function UEncrypt_DES_Triple_PKCS7_CTR(
  const Data: TUInt8Array;
  const Key: TUDES.TKey3;
  const Nonce: TUDES.TInitVector
): TUInt8Array;
function UDecrypt_DES_Triple_PKCS7_CTR(
  const Cipher: TUInt8Array;
  const Key: TUDES.TKey3;
  const Nonce: TUDES.TInitVector
): TUInt8Array;

implementation

function TURSA.TKey.Size: Uint32;
begin
  Result := n.BytesUsed * 8;
end;

function TURSA.TKey.IsPublic: Boolean;
begin
  Result := n.IsValid and e.IsValid;
end;

function TURSA.TKey.IsPrivate: Boolean;
begin
  Result := (n.IsValid and d.IsValid) or IsCRT;
end;

function TURSA.TKey.IsCRT: Boolean;
begin
  Result := p.IsValid and q.IsValid and exp1.IsValid and exp2.IsValid and c.IsValid;
end;

function TURSA.TKey.IsValid: Boolean;
begin
  Result := IsPublic or IsPrivate;
end;

class function TURSA.TKey.MakeInvalid: TKey;
begin
  Result.n := TUCryptoInt.Invalid;
  Result.e := TUCryptoInt.Invalid;
  Result.d := TUCryptoInt.Invalid;
  Result.p := TUCryptoInt.Invalid;
  Result.q := TUCryptoInt.Invalid;
  Result.exp1 := TUCryptoInt.Invalid;
  Result.exp2 := TUCryptoInt.Invalid;
  Result.c := TUCryptoInt.Invalid;
end;

procedure TURSA.TMakePrimeThread.Execute;
  var Number: TUCryptoInt;
  var i: Int32;
begin
  UThreadRandomSeed := RandSeed;
  with Context^ do
  repeat
    Number := TUCryptoInt.MakeRandom(BitCount);
    Number := Number or (TUCryptoInt.One shl (BitCount - 1));
    Number[0] := Number[0] or 1;
    if not TURSA.MillerRabinTest(Number, 50) then Continue;
    Lock.Enter;
    try
      if CurItem > High(Primes) then Break;
      for i := 0 to CurItem - 1 do
      begin
        if Primes[i] = Number then Continue;
      end;
      Primes[CurItem] := Number;
      Inc(CurItem);
      if CurItem < Length(Primes) then Continue;
      Done := True;
    finally
      Lock.Leave;
    end;
  until Done;
end;

class function TUDigestMD5_Impl.GetFunc: TUFuncDigest;
begin
  Result := @UDigestMD5;
end;

class function TUDigestMD5_Impl.Make(const Data: TUInt8Array): TUInt8Array;
begin
  Result := Func(Data);
end;

class function TUDigestSHA1_Impl.GetFunc: TUFuncDigest;
begin
  Result := @UDigestSHA1;
end;

class function TUDigestSHA1_Impl.Make(const Data: TUInt8Array): TUInt8Array;
begin
  Result := Func(Data);
end;

class function TUDigestSHA2_256_Impl.GetFunc: TUFuncDigest;
begin
  Result := @UDigestSHA2_256;
end;

class function TUDigestSHA2_256_Impl.Make(const Data: TUInt8Array): TUInt8Array;
begin
  Result := Func(Data);
end;

class function TUDigestSHA2_512_Impl.GetFunc: TUFuncDigest;
begin
  Result := @UDigestSHA2_512;
end;

class function TUDigestSHA2_512_Impl.Make(const Data: TUInt8Array): TUInt8Array;
begin
  Result := Func(Data);
end;

class function TUDigestSHA3_224_Impl.GetFunc: TUFuncDigest;
begin
  Result := @UDigestSHA3_224;
end;

class function TUDigestSHA3_224_Impl.Make(const Data: TUInt8Array): TUInt8Array;
begin
  Result := Func(Data);
end;

class function TUDigestSHA3_256_Impl.GetFunc: TUFuncDigest;
begin
  Result := @UDigestSHA3_256;
end;

class function TUDigestSHA3_256_Impl.Make(const Data: TUInt8Array): TUInt8Array;
begin
  Result := Func(Data);
end;

class function TUDigestSHA3_384_Impl.GetFunc: TUFuncDigest;
begin
  Result := @UDigestSHA3_384;
end;

class function TUDigestSHA3_384_Impl.Make(const Data: TUInt8Array): TUInt8Array;
begin
  Result := Func(Data);
end;

class function TUDigestSHA3_512_Impl.GetFunc: TUFuncDigest;
begin
  Result := @UDigestSHA3_512;
end;

class function TUDigestSHA3_512_Impl.Make(const Data: TUInt8Array): TUInt8Array;
begin
  Result := Func(Data);
end;

class function TURSA.ModInverse(const e, phi: TUCryptoInt): TUCryptoInt;
  var Zero: TUCryptoInt;
  var One: TUCryptoInt;
  var x0, x1, a, m, q, temp: TUCryptoInt;
begin
  Zero := TUCryptoInt.Zero;
  One := TUCryptoInt.One;
  if phi = One then Exit(Zero);
  x0 := Zero;
  x1 := One;
  a := ((e mod phi) + phi) mod phi;
  m := phi;
  while a > One do
  begin
    q := TUCryptoInt.Division(a, m, temp);
    a := m;
    m := temp;
    temp := x0;
    x0 := x1 - (q * x0);
    x1 := temp;
  end;
  if a <> One then Exit(TUCryptoInt.Invalid);
  if (x1 < 0) then x1 := x1 + phi;
  Result := x1;
end;

class function TURSA.GCD(const a, b: TUCryptoInt): TUCryptoInt;
  var Remainder: TUCryptoInt;
  var LocalA, LocalB: TUCryptoInt;
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

class function TURSA.PowMod(const Base, Exp, Modulus: TUCryptoInt): TUCryptoInt;
  var Context: TUMontgomeryReduction;
  var BaseMont: TUCryptoInt;
  var ResultMont: TUCryptoInt;
  var CurrentExponent: TUCryptoInt;
begin
  Context.Init(Modulus);
  BaseMont := Context.Conv(Base);
  ResultMont := Context.Conv(TUCryptoInt.One);
  CurrentExponent := Exp;
  while not CurrentExponent.IsZero do
  begin
    if CurrentExponent.IsOdd then
    begin
      ResultMont := Context.Mul(ResultMont, BaseMont);
    end;
    BaseMont := Context.Mul(BaseMont, BaseMont);
    CurrentExponent := TUCryptoInt.ShrOne(CurrentExponent);
  end;
  Result := Context.Mul(ResultMont, TUCryptoInt.One);
end;

class function TURSA.MillerRabinTest(const Number: TUCryptoInt; const Iterations: Int32): Boolean;
  var Two: TUCryptoInt;
  var Cmp: Int8;
  var i: Int32;
  var m, m_temp: UInt32;
  var d, a, x, n_minus_1, n_minus_2: TUCryptoInt;
begin
  Two := 2;
  Cmp := TUCryptoInt.Compare(Number, Two);
  if Cmp < 0 then Exit(False);
  if Cmp = 0 then Exit(True);
  if not Number.IsOdd then Exit(False);
  n_minus_1 := Number - TUCryptoInt.One;
  d := n_minus_1;
  m := 0;
  while not d.IsOdd and (d > TUCryptoInt.Zero) do
  begin
    d := TUCryptoInt.ShrOne(d);
    Inc(m);
  end;
  n_minus_2 := Number - Two;
  for i := 1 to Iterations do
  begin
    a := TUCryptoInt.MakeRandomRange(Two, n_minus_2);
    x := PowMod(a, d, Number);
    if (x = TUCryptoInt.One) or (x = n_minus_1) then Continue;
    m_temp := m;
    while m_temp > 1 do
    begin
      x := PowMod(x, Two, Number);
      if x = TUCryptoInt.One then Exit(False);
      if x = n_minus_1 then Break;
      Dec(m_temp);
    end;
    if not (x = n_minus_1) then Exit(False);
  end;
  Result := True;
end;

class function TURSA.XORBytes(const a, b: TUInt8Array): TUInt8Array;
  var i: Int32;
begin
  if Length(a) <> Length(b) then Exit(nil);
  Result := nil;
  SetLength(Result, Length(a));
  for i := 0 to High(Result) do
  Result[i] := a[i] xor b[i];
end;

class function TURSA.PackDER(const Number: TUCryptoInt): TUInt8Array;
  const ByteCount = (TUCryptoInt.Size32.Value) * 4;
  var AsBytes: array[0..ByteCount - 1] of UInt8 absolute Number;
  var i, j, TopByte: Int32;
begin
  Result := nil;
  TopByte := 0;
  for i := High(AsBytes) downto 0 do
  if AsBytes[i] > 0 then
  begin
    TopByte := i;
    Break;
  end;
  j := Int32(AsBytes[TopByte] and $80 > 0);
  SetLength(Result, TopByte + j + 1);
  if j = 1 then Result[0] := 0;
  for i := 0 to TopByte do
  begin
    Result[TopByte + j - i] := AsBytes[i];
  end;
end;

class function TURSA.UnpackDER(const Bytes: TUInt8Array): TUCryptoInt;
  const ByteCount = (TUCryptoInt.Size32.Value) * 4;
  var AsBytes: array[0..ByteCount - 1] of UInt8 absolute Result;
  var i, StartIndex, CopyCount: Int32;
begin
  Result := TUCryptoInt.Zero;
  if Length(Bytes) = 0 then Exit;
  StartIndex := Int32(Bytes[0] = 0);
  CopyCount := UMin(Length(Bytes) - StartIndex, ByteCount);
  for i := 0 to CopyCount - 1 do
  begin
    AsBytes[i] := Bytes[High(Bytes) - i];
  end;
end;

class procedure TURSA.DebugASN1(const Bytes: TUInt8Array; const Offset: String);
  var i: Int32;
  var Tag: UInt8;
  var Content: TUInt8Array;
begin
  i := 0;
  while i < Length(Bytes) do
  begin
    Content := DecodeTLV(Bytes, i, Tag);
    case Tag of
      $30:
      begin
        WriteLn(Offset, 'SEQUENCE');
        DebugASN1(Content, Offset + '  ');
      end;
      $02:
      begin
        WriteLn(Offset, 'NUMBER: ', UnpackDER(Content).ToString);
      end;
      $06:
      begin
        WriteLn(Offset, 'OID: ', UBytesToHex(Content));
      end;
      else
      begin
        WriteLn(Offset, 'Unknown Tag: ', IntToHex(Tag));
      end;
    end;
  end;
end;

class function TURSA.EncodeTLV(
  const Tag: UInt8;
  const Value: TUInt8Array
): TUInt8Array;
  var Len, TempLen, NumLenBytes, i: Int32;
  var LenBytes: TUInt8Array;
begin
  Len := Length(Value);
  if Len < 128 then
  begin
    SetLength(LenBytes, 1);
    LenBytes[0] := Byte(Len);
  end
  else
  begin
    TempLen := Len;
    NumLenBytes := 0;
    while TempLen > 0 do
    begin
      Inc(NumLenBytes);
      TempLen := TempLen shr 8;
    end;
    SetLength(LenBytes, 1 + NumLenBytes);
    LenBytes[0] := $80 or Byte(NumLenBytes);
    TempLen := Len;
    for i := NumLenBytes downto 1 do
    begin
      LenBytes[i] := Byte(TempLen and $FF);
      TempLen := TempLen shr 8;
    end;
  end;
  Result := specialize UArrConcat<TUInt8Array>([[Tag], LenBytes, Value]);
end;

class function TURSA.DecodeTLV(
  const Data: TUInt8Array;
  var Index: Int32;
  out Tag: UInt8
): TUInt8Array;
  var Len, NumLenBytes, i: Int32;
begin
  Result := nil;
  if Index >= Length(Data) then Exit;
  Tag := Data[Index]; Inc(Index);
  Len := Data[Index]; Inc(Index);
  if (Len and $80) <> 0 then
  begin
    NumLenBytes := Len and $7F;
    if (Index + NumLenBytes) > Length(Data) then Exit;
    Len := 0;
    for i := 1 to NumLenBytes do
    begin
      Len := (Len shl 8) or Data[Index];
      Inc(Index);
    end;
  end;
  if (Index + Len) > Length(Data) then Exit;
  SetLength(Result, Len);
  if Len > 0 then
  begin
    Move(Data[Index], Result[0], Len);
  end;
  Inc(Index, Len);
end;

class function TURSA.ASN1ToBase64(
  const DataASN: TUInt8Array;
  const Header, Footer: String
): String;
  var Base64: String;
  var i, j, Returns, ReturnsTotal, Remainder: Int32;
begin
  Base64 := UBytesToBase64(DataASN);
  Returns := Length(Base64) div 64;
  ReturnsTotal := Returns + 1;
  Remainder := Length(Base64) mod 64;
  if Remainder > 0 then Inc(ReturnsTotal);
  Result := '';
  SetLength(Result,
    Length(Header) + Length(Footer) + Length(Base64) + 2 * ReturnsTotal
  );
  i := 1;
  Move(Header[1], Result[i], Length(Header)); Inc(i, Length(Header));
  Result[i] := #$d; Inc(i);
  Result[i] := #$a; Inc(i);
  for j := 0 to Returns - 1 do
  begin
    Move(Base64[j * 64 + 1], Result[i], 64);
    Inc(i, 64);
    Result[i] := #$d; Inc(i);
    Result[i] := #$a; Inc(i);
  end;
  if Remainder > 0 then
  begin
    Move(Base64[Returns * 64 + 1], Result[i], Remainder);
    Inc(i, Remainder);
    Result[i] := #$d; Inc(i);
    Result[i] := #$a; Inc(i);
  end;
  Move(Footer[1], Result[i], Length(Footer));
end;

class function TURSA.Base64ToASN1(
  const Base64: String;
  const Header, Footer: String
): TUInt8Array;
  var Str: String;
  var KeyStart, KeyEnd: Int32;
begin
  KeyStart := Base64.IndexOf(Header);
  if KeyStart = -1 then Exit(nil);
  KeyStart += Length(Header);
  KeyEnd := Base64.IndexOf(Footer);
  if KeyEnd < -1 then Exit(nil);
  if KeyStart > KeyEnd then Exit(nil);
  Str := Base64.Substring(KeyStart, KeyEnd - KeyStart);
  Str := Str.Replace(#$d#$a, '', [rfReplaceAll]);
  Str := Str.Replace(#$a, '', [rfReplaceAll]);
  Result := UBase64ToBytes(Str);
end;

class function TURSA.MakePrime(const BitCount: Int32): TUCryptoInt;
begin
  repeat
    Result := TUCryptoInt.MakeRandom(BitCount);
    Result := Result or (TUCryptoInt.One shl (BitCount - 1));
    Result[0] := Result[0] or 1;
  until MillerRabinTest(Result, 100);
end;

class function TURSA.MakePrimes(
  const PrimeCount: Int32;
  const BitCount: Int32;
  const ThreadCount: Int32
): TUCryptoIntArray;
  var Threads: array of TMakePrimeThread;
  var Context: TMakePrimeContext;
  var i: Int32;
begin
  if (PrimeCount < 1) or (ThreadCount < 1) then Exit(nil);
  Result := nil;
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
    Threads[i].RandSeed := Random(High(UInt32));
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
    if Length(Context.Primes) > 0 then
    begin
      SetLength(Result, Length(Context.Primes));
      Move(Context.Primes[0], Result[0], Length(Result) * SizeOf(TUCryptoInt));
    end;
  end;
end;

class function TURSA.MakeKey(
  const BitCount: UInt32;
  const Threads: Int32
): TURSA.TKey;
  var p, q, n, phi, e, d, One, p_minus_1, q_minus_1: TUCryptoInt;
  var PrimeSizeInBits: Int32;
  var Primes: TUCryptoIntArray;
begin
  One := TUCryptoInt.One;
  Result := TKey.MakeInvalid;
  PrimeSizeInBits := BitCount shr 1;
  e := 65537;
  repeat
    Primes := MakePrimes(2, PrimeSizeInBits, Threads);
    if Length(Primes) < 2 then Exit;
    p := Primes[0];
    q := Primes[1];
    if p = q then Continue;
    n := p * q;
    p_minus_1 := p - One;
    q_minus_1 := q - One;
    phi := p_minus_1 * q_minus_1;
  until (p <> q) and (GCD(e, phi) = One);
  d := ModInverse(e, phi);
  Result.n := n;
  Result.e := e;
  Result.d := d;
  Result.q := q;
  Result.p := p;
  Result.exp1 := d mod p_minus_1;
  Result.exp2 := d mod q_minus_1;
  Result.c := ModInverse(q, p);
end;

class function TURSA.PackData_PKCS1(
  const Data: Pointer;
  const DataSize: UInt32;
  const BlockSize: UInt32
): TUCryptoInt;
  const MinPadding = 11;
  const ByteCount = (TUCryptoInt.Size32.Value) * 4;
  var PaddingSize, BlockSizeInBytes: Int32;
  var PaddedDataBE: array[0..ByteCount - 1] of UInt8;
  var PaddedData: array[0..ByteCount - 1] of UInt8 absolute Result;
  var i: Int32;
begin
  BlockSizeInBytes := BlockSize shr 3;
  if DataSize > BlockSizeInBytes - MinPadding then Exit(TUCryptoInt.Invalid);
  Result := TUCryptoInt.Zero;
  PaddingSize := BlockSizeInBytes - DataSize;
  PaddedDataBE[0] := $00;
  PaddedDataBE[1] := $02;
  for i := 2 to PaddingSize - 2 do
  repeat
    PaddedDataBE[i] := UInt8(UThreadRandom(256));
  until PaddedDataBE[i] > 0;
  PaddedDataBE[PaddingSize - 1] := 0;
  Move(Data^, PaddedDataBE[PaddingSize], DataSize);
  for i := 0 to BlockSizeInBytes - 1 do
  begin
    PaddedData[i] := PaddedDataBE[BlockSizeInBytes - i - 1];
  end;
end;

class function TURSA.PackData_PKCS1(
  const Data: TUInt8Array;
  const BlockSize: UInt32
): TUCryptoInt;
begin
  Result := PackData_PKCS1(@Data[0], Length(Data), BlockSize);
end;

class function TURSA.UnpackData_PKCS1(const Block: TUCryptoInt; const BlockSize: UInt32): TUInt8Array;
  const ByteCount = (TUCryptoInt.Size32.Value) * 4;
  var PaddingSize, BlockSizeInBytes: Int32;
  var PaddedDataBE: array[0..ByteCount - 1] of UInt8;
  var PaddedData: array[0..ByteCount - 1] of UInt8 absolute Block;
  var i: Int32;
begin
  BlockSizeInBytes := BlockSize shr 3;
  for i := 0 to BlockSizeInBytes - 1 do
  begin
    PaddedDataBE[i] := PaddedData[BlockSizeInBytes - i - 1];
  end;
  if (PaddedDataBE[0] <> 0) or (PaddedDataBE[1] <> 2) then Exit(nil);
  PaddingSize := 0;
  for i := 2 to High(PaddedDataBE) do
  begin
    if PaddedDataBE[i] <> 0 then Continue;
    PaddingSize := i + 1;
    Break;
  end;
  if PaddingSize = 0 then Exit(nil);
  Result := nil;
  SetLength(Result, BlockSizeInBytes - PaddingSize);
  Move(PaddedDataBE[PaddingSize], Result[0], Length(Result));
end;

class function TURSA.UnpackStr_PKCS1(const Block: TUCryptoInt; const BlockSize: UInt32): String;
  var Data: TUInt8Array;
begin
  Data := UnpackData_PKCS1(Block, BlockSize);
  Result := '';
  SetLength(Result, Length(Data));
  Move(Data[0], Result[1], Length(Data));
end;

class function TURSA.PackData_OAEP(
  const Data: Pointer;
  const DataSize: UInt32;
  const BlockSize: UInt32
): TUCryptoInt;
  const HashLen = SizeOf(TUDigestSHA2_256);
  const ByteCount = (TUCryptoInt.Size32.Value) * 4;
  var BlockSizeInBytes: UInt32;
  var PaddedDataBE: array[0..ByteCount - 1] of UInt8;
  var PaddedData: array[0..ByteCount - 1] of UInt8 absolute Result;
  var PadStrLen: Int32;
  var DB, Seed, DBMask, MaskedDB, SeedMask, MaskedSeed: TUInt8Array;
  var i, j: Int32;
  var LabelHash: TUDigestSHA2_256;
begin
  BlockSizeInBytes := BlockSize shr 3;
  if DataSize > BlockSizeInBytes - 2 * HashLen - 2 then Exit(TUCryptoInt.Invalid);
  LabelHash := USHA2_256(nil, 0);
  PadStrLen := BlockSizeInBytes - DataSize - 2 * HashLen - 2;
  DB := nil;
  SetLength(DB, HashLen + PadStrLen + 1 + DataSize);
  Move(LabelHash[0], DB[0], HashLen);
  i := Length(LabelHash);
  for j := 0 to PadStrLen - 1 do
  begin
    DB[i + j] := $00;
  end;
  Inc(i, PadStrLen);
  DB[i] := $01;
  Inc(i);
  Move(Data^, DB[i], DataSize);
  Seed := nil;
  SetLength(Seed, HashLen);
  for i := 0 to High(Seed) do
  begin
    Seed[i] := UInt8(Random(256));
  end;
  DBMask := UMGF1_SHA256(Seed, BlockSizeInBytes - HashLen - 1);
  MaskedDB := XORBytes(DB, DBMask);
  SeedMask := UMGF1_SHA256(MaskedDB, HashLen);
  MaskedSeed := XORBytes(Seed, SeedMask);
  Result := TUCryptoInt.Zero;
  PaddedDataBE[0] := $00;
  Move(MaskedSeed[0], PaddedDataBE[1], HashLen);
  Move(MaskedDB[0], PaddedDataBE[1 + HashLen], BlockSizeInBytes - HashLen - 1);
  for i := 0 to BlockSizeInBytes - 1 do
  begin
    PaddedData[i] := PaddedDataBE[BlockSizeInBytes - i - 1];
  end;
end;

class function TURSA.PackData_OAEP(
  const Data: TUInt8Array;
  const BlockSize: UInt32
): TUCryptoInt;
begin
  Result := PackData_OAEP(@Data[0], Length(Data));
end;

class function TURSA.UnpackData_OAEP(
  const Block: TUCryptoInt;
  const BlockSize: UInt32
): TUInt8Array;
  const HashLen = SizeOf(TUDigestSHA2_256);
  const ByteCount = (TUCryptoInt.Size32.Value) * 4;
  var BlockSizeInBytes: UInt32;
  var PaddedDataBE: array[0..ByteCount - 1] of UInt8;
  var PaddedData: array[0..ByteCount - 1] of UInt8 absolute Block;
  var i, SepIndex: Int32;
  var MaskedSeed, MaskedDB, SeedMask, Seed, DBMask, DB: TUint8Array;
  var Y: UInt8;
  var LabelHash: TUDigestSHA2_256;
begin
  Result := nil;
  BlockSizeInBytes := BlockSize shr 3;
  for i := 0 to BlockSizeInBytes - 1 do
  begin
    PaddedDataBE[i] := PaddedData[BlockSizeInBytes - i - 1];
  end;
  Y := PaddedDataBE[0];
  MaskedSeed := nil;
  SetLength(MaskedSeed, HashLen);
  MaskedDB := nil;
  SetLength(MaskedDB, BlockSizeInBytes - HashLen - 1);
  Move(PaddedDataBE[1], MaskedSeed[0], HashLen);
  Move(PaddedDataBE[1 + HashLen], MaskedDB[0], BlockSizeInBytes - HashLen - 1);
  SeedMask := UMGF1_SHA256(MaskedDB, HashLen);
  Seed := XORBytes(MaskedSeed, SeedMask);
  DBMask := UMGF1_SHA256(Seed, BlockSizeInBytes - HashLen - 1);
  DB := XORBytes(MaskedDB, DBMask);
  LabelHash := USHA2_256(nil, 0);
  if Y <> $00 then Exit;
  for i := 0 to HashLen - 1 do
  begin
    if DB[i] <> LabelHash[i] then Exit;
  end;
  SepIndex := -1;
  for i := HashLen to High(DB) do
  begin
    if DB[i] = $01 then
    begin
      SepIndex := i;
      Break;
    end;
    if DB[i] <> $00 then Exit;
  end;
  if SepIndex = -1 then Exit;
  SetLength(Result, High(DB) - SepIndex);
  if Length(Result) = 0 then Exit;
  Move(DB[SepIndex + 1], Result[0], Length(Result));
end;

class function TURSA.UnpackStr_OAEP(
  const Block: TUCryptoInt;
  const BlockSize: UInt32
): String;
  var Data: TUInt8Array;
begin
  Data := UnpackData_OAEP(Block, BlockSize);
  Result := '';
  SetLength(Result, Length(Data));
  Move(Data[0], Result[1], Length(Data));
end;

class function TURSA.PackData_Singature(
  const Data: Pointer;
  const DataSize: UInt32;
  const BlockSize: UInt32
): TUCryptoInt;
  const MinPadding = 11;
  const ByteCount = (TUCryptoInt.Size32.Value) * 4;
  var PaddingSize, BlockSizeInBytes: Int32;
  var PaddedDataBE: array[0..ByteCount - 1] of UInt8;
  var PaddedData: array[0..ByteCount - 1] of UInt8 absolute Result;
  var i: Int32;
begin
  BlockSizeInBytes := BlockSize shr 3;
  if DataSize > BlockSizeInBytes - MinPadding then Exit(TUCryptoInt.Invalid);
  Result := TUCryptoInt.Zero;
  PaddingSize := BlockSizeInBytes - DataSize;
  PaddedDataBE[0] := $00;
  PaddedDataBE[1] := $01;
  for i := 2 to PaddingSize - 2 do PaddedDataBE[i] := $ff;
  PaddedDataBE[PaddingSize - 1] := 0;
  Move(Data^, PaddedDataBE[PaddingSize], DataSize);
  for i := 0 to BlockSizeInBytes - 1 do
  begin
    PaddedData[i] := PaddedDataBE[BlockSizeInBytes - i - 1];
  end;
end;

class function TURSA.UnpackData_Singature(
  const Block: TUCryptoInt;
  const BlockSize: UInt32
): TUInt8Array;
  const MinPadding = 11;
  const ByteCount = (TUCryptoInt.Size32.Value) * 4;
  var PaddingSize, BlockSizeInBytes: Int32;
  var PaddedDataBE: array[0..ByteCount - 1] of UInt8;
  var PaddedData: array[0..ByteCount - 1] of UInt8 absolute Block;
  var i: Int32;
begin
  Result := nil;
  BlockSizeInBytes := BlockSize shr 3;
  for i := 0 to BlockSizeInBytes - 1 do
  begin
    PaddedDataBE[i] := PaddedData[BlockSizeInBytes - i - 1];
  end;
  if (PaddedDataBE[0] <> $00) or (PaddedDataBE[1] <> $01) then Exit;
  PaddingSize := 0;
  for i := 2 to High(PaddedDataBE) do
  begin
    if PaddedDataBE[i] <> 0 then
    begin
      if PaddedDataBE[i] <> $ff then Exit;
      Continue;
    end;
    PaddingSize := i + 1;
    Break;
  end;
  if PaddingSize < MinPadding then Exit;
  SetLength(Result, BlockSizeInBytes - PaddingSize);
  Move(PaddedDataBE[PaddingSize], Result[0], Length(Result));
end;

class function TURSA.Encrypt_PKCS1(
  const Data: Pointer;
  const DataSize: UInt32;
  const Key: TURSA.TKey
): TUCryptoInt;
  var Block: TUCryptoInt;
begin
  Block := PackData_PKCS1(Data, DataSize, Key.Size);
  if not Block.IsValid then Exit(TUCryptoInt.Invalid);
  Result := PowMod(Block, Key.e, Key.n);
end;

class function TURSA.Decrypt_PKCS1_Str(
  const Cipher: TUCryptoInt;
  const Key: TURSA.TKey
): String;
  var Block: TUCryptoInt;
begin
  Block := Decrypt(Cipher, Key);
  Result := UnpackStr_PKCS1(Block, Key.Size);
end;

class function TURSA.Decrypt_PKCS1(
  const Cipher: TUCryptoInt;
  const Key: TURSA.TKey
): TUInt8Array;
  var Block: TUCryptoInt;
begin
  Block := Decrypt(Cipher, Key);
  Result := UnpackData_PKCS1(Block, Key.Size);
end;

class function TURSA.Encrypt_OAEP(
  const Data: Pointer;
  const DataSize: UInt32;
  const Key: TURSA.TKey
): TUCryptoInt;
  var Block: TUCryptoInt;
begin
  Block := PackData_OAEP(Data, DataSize, Key.Size);
  if not Block.IsValid then Exit(TUCryptoInt.Invalid);
  Result := PowMod(Block, Key.e, Key.n);
end;

class function TURSA.Decrypt_OAEP_Str(
  const Cipher: TUCryptoInt;
  const Key: TURSA.TKey
): String;
  var Block: TUCryptoInt;
begin
  Block := Decrypt(Cipher, Key);
  Result := UnpackStr_OAEP(Block, Key.Size);
end;

class function TURSA.Decrypt_OAEP(
  const Cipher: TUCryptoInt;
  const Key: TURSA.TKey
): TUInt8Array;
  var Block: TUCryptoInt;
begin
  Block := Decrypt(Cipher, Key);
  Result := UnpackData_OAEP(Block, Key.Size);
end;

class function TURSA.Decrypt_CRT(
  const Cipher: TUCryptoInt;
  const Key: TURSA.TKey
): TUCryptoInt;
  var m1, m2, h, c_mod_p, c_mod_q: TUCryptoInt;
begin
  c_mod_p := Cipher mod Key.p;
  c_mod_q := Cipher mod Key.q;
  m1 := PowMod(c_mod_p, Key.exp1, Key.p);
  m2 := PowMod(c_mod_q, Key.exp2, Key.q);
  if m1 < m2 then
  begin
    h := m1 + Key.p;
    h := h - m2;
  end
  else
  begin
    h := m1 - m2;
  end;
  h := h * Key.c;
  h := h mod Key.p;
  Result := m2 + (h * Key.q);
end;

class function TURSA.Decrypt(
  const Cipher: TUCryptoInt;
  const Key: TURSA.TKey
): TUCryptoInt;
begin
  if Key.IsCRT then Exit(Decrypt_CRT(Cipher, Key));
  Result := PowMod(Cipher, Key.d, Key.n);
end;

class function TURSA.Sign_SHA256(const Data: TUInt8Array; const Key: TKey): TUInt8Array;
  var Hash: TUDigestSHA2_256;
  var HashDER: TUInt8Array;
  var PackedHash, Signature: TUCryptoInt;
begin
  Hash := USHA2_256(Data);
  HashDER := EncodeTLV($30, UBytesConcat([
    EncodeTLV($30, UBytesConcat([
      EncodeTLV($06, OID_SHA_256),
      EncodeTLV($05, [])
    ])),
    EncodeTLV($04, Hash)
  ]));
  PackedHash := PackData_Singature(@HashDER[0], Length(HashDER), Key.Size);
  Signature := PowMod(PackedHash, Key.d, Key.n);
  Result := Signature.ToBytes;
end;

class function TURSA.Sign_SHA512(const Data: TUInt8Array; const Key: TKey): TUInt8Array;
  var Hash: TUDigestSHA2_512;
  var HashDER: TUInt8Array;
  var PackedHash, Signature: TUCryptoInt;
begin
  Hash := USHA2_512(Data);
  HashDER := EncodeTLV($30, UBytesConcat([
    EncodeTLV($30, UBytesConcat([
      EncodeTLV($06, OID_SHA_512),
      EncodeTLV($05, [])
    ])),
    EncodeTLV($04, Hash)
  ]));
  PackedHash := PackData_Singature(@HashDER[0], Length(HashDER), Key.Size);
  Signature := PowMod(PackedHash, Key.d, Key.n);
  Result := Signature.ToBytes;
end;

class function TURSA.Verify(
  const Data, Signature: TUInt8Array;
  const Key: TKey
): Boolean;
  var Hash: TUInt8Array;
  var PackedHash: TUCryptoInt;
  var UnpackedDER, SignedHashDER, SignedHash, Alg, OID: TUInt8Array;
  var Index: Int32;
  var Tag: UInt8;
begin
  PackedHash := PowMod(TUCryptoInt(Signature), Key.e, Key.n);
  Index := 0;
  UnpackedDER := UnpackData_Singature(PackedHash, Key.Size);
  SignedHashDER := DecodeTLV(UnpackedDER, Index, Tag);
  if Tag <> $30 then Exit(False);
  Index := 0;
  Alg := DecodeTLV(SignedHashDER, Index, Tag);
  if Tag <> $30 then Exit(False);
  SignedHash := DecodeTLV(SignedHashDER, Index, Tag);
  if Tag <> $04 then Exit(False);
  Index := 0;
  OID := DecodeTLV(Alg, Index, Tag);
  if Tag <> $06 then Exit(False);
  DecodeTLV(Alg, Index, Tag);
  if Tag <> $05 then Exit(False);
  if UBytesEqual(OID, OID_SHA_256) then
  begin
    Hash := USHA2_256(Data);
  end
  else if UBytesEqual(OID, OID_SHA_512) then
  begin
    Hash := USHA2_512(Data);
  end
  else if UBytesEqual(OID, OID_SHA1) then
  begin
    Hash := USHA1(Data);
  end
  else
  begin
    Exit(False);
  end;
  Result := UBytesEqual(Hash, SignedHash);
end;

class function TURSA.ExportKeyPrivate_PKCS1_DER(const Key: TURSA.TKey): TUInt8Array;
begin
  Result := EncodeTLV($30,
    UBytesConcat([
      EncodeTLV($02, [0]),
      EncodeTLV($02, PackDER(Key.n)),
      EncodeTLV($02, PackDER(Key.e)),
      EncodeTLV($02, PackDER(Key.d)),
      EncodeTLV($02, PackDER(Key.p)),
      EncodeTLV($02, PackDER(Key.q)),
      EncodeTLV($02, PackDER(Key.exp1)),
      EncodeTLV($02, PackDER(Key.exp2)),
      EncodeTLV($02, PackDER(Key.c))
    ])
  );
end;

class function TURSA.ImportKeyPrivate_PKCS1_DER(const Key: TUInt8Array): TURSA.TKey;
  var Index, i: Int32;
  var Tag: UInt8;
  var SequenceContent, IntValue: TUInt8Array;
  var Components: array [0..7] of TUCryptoInt absolute Result;
begin
  Result := TKey.MakeInvalid;
  Index := 0;
  SequenceContent := DecodeTLV(Key, Index, Tag);
  if Tag <> $30 then Exit;
  Index := 0;
  IntValue := DecodeTLV(SequenceContent, Index, Tag);
  if (Tag <> $02) or (UnpackDER(IntValue).ToInt <> 0) then Exit;
  i := 0;
  while (Index < Length(SequenceContent))
  and (i <= Length(Components)) do
  try
    IntValue := DecodeTLV(SequenceContent, Index, Tag);
    if Tag <> $02 then Exit;
    Components[i] := UnpackDER(IntValue);
  finally
    Inc(i);
  end;
end;

class function TURSA.ExportKeyPublic_PKCS1_DER(const Key: TURSA.TKey): TUInt8Array;
begin
  Result := EncodeTLV($30,
    UBytesConcat([
      EncodeTLV($02, PackDER(Key.n)),
      EncodeTLV($02, PackDER(Key.e))
    ])
  );
end;

class function TURSA.ImportKeyPublic_PKCS1_DER(const Key: TUInt8Array): TURSA.TKey;
  var Index, i: Int32;
  var Tag: UInt8;
  var SequenceContent, IntValue: TUInt8Array;
  var Components: array [0..1] of TUCryptoInt absolute Result;
begin
  Index := 0;
  SequenceContent := DecodeTLV(Key, Index, Tag);
  if Tag <> $30 then Exit;
  Index := 0;
  i := 0;
  while (Index < Length(SequenceContent))
  and (i < Length(Components)) do
  try
    IntValue := DecodeTLV(SequenceContent, Index, Tag);
    if Tag <> $02 then Exit;
    Components[i] := UnpackDER(IntValue);
  finally
    Inc(i);
  end;
end;

class function TURSA.ExportKeyPrivate_PKCS8_DER(const Key: TURSA.TKey): TUInt8Array;
  var KeyPKCS1: TUInt8Array;
begin
  KeyPKCS1 := ExportKeyPrivate_PKCS1_DER(Key);
  Result := EncodeTLV($30,
    UBytesConcat([
      EncodeTLV($02, [0]),
      EncodeTLV($30, UBytesConcat([
        EncodeTLV($06, OID_RSA),
        EncodeTLV($05, [])
      ])),
      EncodeTLV($04, KeyPKCS1)
    ])
  );
end;

class function TURSA.ImportKeyPrivate_PKCS8_DER(const Key: TUInt8Array): TURSA.TKey;
  var Index, i: Int32;
  var Tag: UInt8;
  var MainContent, VersionContent, AlgIdContent, AlgId, PKCS1_Content: TUInt8Array;
begin
  Result := TKey.MakeInvalid;
  Index := 0;
  MainContent := DecodeTLV(Key, Index, Tag);
  if Tag <> $30 then Exit;
  Index := 0;
  VersionContent := DecodeTLV(MainContent, Index, Tag);
  if Tag <> $02 then Exit;
  if not UnpackDER(VersionContent).IsZero then Exit;
  AlgIdContent := DecodeTLV(MainContent, Index, Tag);
  if Tag <> $30 then Exit;
  i := 0;
  AlgId := DecodeTLV(AlgIdContent, i, Tag);
  if (Tag <> $06) or (Length(AlgId) <> Length(OID_RSA)) then Exit;
  for i := 0 to High(OID_RSA) do if AlgId[i] <> OID_RSA[i] then Exit;
  PKCS1_Content := DecodeTLV(MainContent, Index, Tag);
  if Tag <> $04 then Exit;
  Result := ImportKeyPrivate_PKCS1_DER(PKCS1_Content);
end;

class function TURSA.ExportKeyPrivate_PKCS1(const Key: TURSA.TKey): String;
  const Header: String = '-----BEGIN RSA PRIVATE KEY-----';
  const Footer: String = '-----END RSA PRIVATE KEY-----';
begin
  Result := ASN1ToBase64(ExportKeyPrivate_PKCS1_DER(Key), Header, Footer);
end;

class function TURSA.ExportKeyPublic_PKCS1(const Key: TURSA.TKey): String;
  const Header: String = '-----BEGIN RSA PUBLIC KEY-----';
  const Footer: String = '-----END RSA PUBLIC KEY-----';
begin
  Result := ASN1ToBase64(ExportKeyPublic_PKCS1_DER(Key), Header, Footer);
end;

class function TURSA.ExportKeyPrivate_PKCS8(const Key: TURSA.TKey): String;
  var KeyDER: TUInt8Array;
  const Header: String = '-----BEGIN PRIVATE KEY-----';
  const Footer: String = '-----END PRIVATE KEY-----';
begin
  KeyDER := ExportKeyPrivate_PKCS8_DER(Key);
  Result := ASN1ToBase64(KeyDER, Header, Footer);
end;

class function TURSA.ExportKeyPrivateEncrypted_PKCS8(
  const EncryptedKey: TUint8Array;
  const HMAC_OID: TUInt8Array;
  const ALG_OID: TUInt8Array;
  const Salt, IV: TUInt8Array;
  const IterationCount: Int32
): String;
  var SequenceContent: TUInt8Array;
  const Header: String = '-----BEGIN ENCRYPTED PRIVATE KEY-----';
  const Footer: String = '-----END ENCRYPTED PRIVATE KEY-----';
begin
  SequenceContent := EncodeTLV($30, UBytesConcat([
    EncodeTLV($30, UBytesConcat([
      EncodeTLV($06, OID_PBES2),
      EncodeTLV($30, UBytesConcat([
        EncodeTLV($30, UBytesConcat([
          EncodeTLV($06, OID_PBKDF2),
          EncodeTLV($30, UBytesConcat([
            EncodeTLV($04, Salt),
            EncodeTLV($02, PackDER(IterationCount)),
            EncodeTLV($30, UBytesConcat([
              EncodeTLV($06, HMAC_OID),
              EncodeTLV($05, [])
            ]))
          ]))
        ])),
        EncodeTLV($30, UBytesConcat([
          EncodeTLV($06, ALG_OID),
          EncodeTLV($04, IV)
        ]))
      ]))
    ])),
    EncodeTLV($04, EncryptedKey)
  ]));
  Result := ASN1ToBase64(SequenceContent, Header, Footer);
end;

class function TURSA.ExportKeyPrivateEncrypted_PKCS8_SHA256_AES128_CBC(
  const Key: TURSA.TKey;
  const Password: TUInt8Array;
  const IterationCount: Int32
): String;
  var KeyPKCS8, Salt, DerivedKey, EncryptedKey: TUInt8Array;
  var AESIV: TUAES.TInitVector;
  var AESKey: TUAES.TKey128;
  const SaltSize = 16;
begin
  KeyPKCS8 := ExportKeyPrivate_PKCS8_DER(Key);
  Salt := URandomBytes(SaltSize);
  DerivedKey := UPBKDF2_HMAC_SHA256(
    Password, Salt,
    SizeOf(AESKey), IterationCount
  );
  AESKey := TUAES.MakeKey128(DerivedKey);
  AESIV := TUAES.MakeIV;
  EncryptedKey := UEncrypt_AES_PKCS7_CBC_128(KeyPKCS8, AESKey, AESIV);
  Result := ExportKeyPrivateEncrypted_PKCS8(
    EncryptedKey, TURSA.OID_HMAC_SHA256, TURSA.OID_AES128_CBC,
    Salt, AESIV, IterationCount
  );
end;

class function TURSA.ExportKeyPrivateEncrypted_PKCS8_SHA256_AES192_CBC(
  const Key: TURSA.TKey;
  const Password: TUInt8Array;
  const IterationCount: Int32
): String;
  var KeyPKCS8, Salt, DerivedKey, EncryptedKey: TUInt8Array;
  var AESIV: TUAES.TInitVector;
  var AESKey: TUAES.TKey192;
  const SaltSize = 16;
begin
  KeyPKCS8 := ExportKeyPrivate_PKCS8_DER(Key);
  Salt := URandomBytes(SaltSize);
  DerivedKey := UPBKDF2_HMAC_SHA256(
    Password, Salt,
    SizeOf(AESKey), IterationCount
  );
  AESKey := TUAES.MakeKey192(DerivedKey);
  AESIV := TUAES.MakeIV;
  EncryptedKey := UEncrypt_AES_PKCS7_CBC_192(KeyPKCS8, AESKey, AESIV);
  Result := ExportKeyPrivateEncrypted_PKCS8(
    EncryptedKey, TURSA.OID_HMAC_SHA256, TURSA.OID_AES192_CBC,
    Salt, AESIV, IterationCount
  );
end;

class function TURSA.ExportKeyPrivateEncrypted_PKCS8_SHA256_AES256_CBC(
  const Key: TURSA.TKey;
  const Password: TUInt8Array;
  const IterationCount: Int32
): String;
  var KeyPKCS8, Salt, DerivedKey, EncryptedKey: TUInt8Array;
  var AESIV: TUAES.TInitVector;
  var AESKey: TUAES.TKey256;
  const SaltSize = 16;
begin
  KeyPKCS8 := ExportKeyPrivate_PKCS8_DER(Key);
  Salt := URandomBytes(SaltSize);
  DerivedKey := UPBKDF2_HMAC_SHA256(
    Password, Salt,
    SizeOf(AESKey), IterationCount
  );
  AESKey := TUAES.MakeKey256(DerivedKey);
  AESIV := TUAES.MakeIV;
  EncryptedKey := UEncrypt_AES_PKCS7_CBC_256(KeyPKCS8, AESKey, AESIV);
  Result := ExportKeyPrivateEncrypted_PKCS8(
    EncryptedKey, TURSA.OID_HMAC_SHA256, TURSA.OID_AES256_CBC,
    Salt, AESIV, IterationCount
  );
end;

class function TURSA.ExportKeyPrivateEncrypted_PKCS8_SHA512_AES128_CBC(
  const Key: TURSA.TKey;
  const Password: TUInt8Array;
  const IterationCount: Int32
): String;
  var KeyPKCS8, Salt, DerivedKey, EncryptedKey: TUInt8Array;
  var AESIV: TUAES.TInitVector;
  var AESKey: TUAES.TKey128;
  const SaltSize = 16;
begin
  KeyPKCS8 := ExportKeyPrivate_PKCS8_DER(Key);
  Salt := URandomBytes(SaltSize);
  DerivedKey := UPBKDF2_HMAC_SHA512(
    Password, Salt,
    SizeOf(AESKey), IterationCount
  );
  AESKey := TUAES.MakeKey128(DerivedKey);
  AESIV := TUAES.MakeIV;
  EncryptedKey := UEncrypt_AES_PKCS7_CBC_128(KeyPKCS8, AESKey, AESIV);
  Result := ExportKeyPrivateEncrypted_PKCS8(
    EncryptedKey, TURSA.OID_HMAC_SHA512, TURSA.OID_AES128_CBC,
    Salt, AESIV, IterationCount
  );
end;

class function TURSA.ExportKeyPrivateEncrypted_PKCS8_SHA512_AES192_CBC(
  const Key: TURSA.TKey;
  const Password: TUInt8Array;
  const IterationCount: Int32
): String;
  var KeyPKCS8, Salt, DerivedKey, EncryptedKey: TUInt8Array;
  var AESIV: TUAES.TInitVector;
  var AESKey: TUAES.TKey192;
  const SaltSize = 16;
begin
  KeyPKCS8 := ExportKeyPrivate_PKCS8_DER(Key);
  Salt := URandomBytes(SaltSize);
  DerivedKey := UPBKDF2_HMAC_SHA512(
    Password, Salt,
    SizeOf(AESKey), IterationCount
  );
  AESKey := TUAES.MakeKey192(DerivedKey);
  AESIV := TUAES.MakeIV;
  EncryptedKey := UEncrypt_AES_PKCS7_CBC_192(KeyPKCS8, AESKey, AESIV);
  Result := ExportKeyPrivateEncrypted_PKCS8(
    EncryptedKey, TURSA.OID_HMAC_SHA512, TURSA.OID_AES192_CBC,
    Salt, AESIV, IterationCount
  );
end;

class function TURSA.ExportKeyPrivateEncrypted_PKCS8_SHA512_AES256_CBC(
  const Key: TURSA.TKey;
  const Password: TUInt8Array;
  const IterationCount: Int32
): String;
  var KeyPKCS8, Salt, DerivedKey, EncryptedKey: TUInt8Array;
  var AESIV: TUAES.TInitVector;
  var AESKey: TUAES.TKey256;
  const SaltSize = 16;
begin
  KeyPKCS8 := ExportKeyPrivate_PKCS8_DER(Key);
  Salt := URandomBytes(SaltSize);
  DerivedKey := UPBKDF2_HMAC_SHA512(
    Password, Salt,
    SizeOf(AESKey), IterationCount
  );
  AESKey := TUAES.MakeKey256(DerivedKey);
  AESIV := TUAES.MakeIV;
  EncryptedKey := UEncrypt_AES_PKCS7_CBC_256(KeyPKCS8, AESKey, AESIV);
  Result := ExportKeyPrivateEncrypted_PKCS8(
    EncryptedKey, TURSA.OID_HMAC_SHA512, TURSA.OID_AES256_CBC,
    Salt, AESIV, IterationCount
);
end;

class function TURSA.ExportKeyPublic_X509(const Key: TURSA.TKey): String;
  var KeyContent: TUInt8Array;
  var BitStringWrapper: TUInt8Array;
  var AlgIdContent: TUInt8Array;
  var SequenceContent: TUInt8Array;
  const Header: String = '-----BEGIN PUBLIC KEY-----';
  const Footer: String = '-----END PUBLIC KEY-----';
begin
  KeyContent := specialize UArrConcat<TUInt8Array>([
    EncodeTLV($02, PackDER(Key.n)),
    EncodeTLV($02, PackDER(Key.e))
  ]);
  BitStringWrapper := specialize UArrConcat<TUInt8Array>([
    [$00],
    EncodeTLV($30, KeyContent)
  ]);
  AlgIdContent := specialize UArrConcat<TUInt8Array>([
    EncodeTLV($06, OID_RSA),
    EncodeTLV($05, [])
  ]);
  SequenceContent := EncodeTLV(
    $30,
    specialize UArrConcat<TUInt8Array>([
      EncodeTLV($30, AlgIdContent),
      EncodeTLV($03, BitStringWrapper)
    ])
  );
  Result := ASN1ToBase64(SequenceContent, Header, Footer);
end;

class function TURSA.ImportKeyPrivate_PKCS1(const Key: String): TURSA.TKey;
  const Header = '-----BEGIN RSA PRIVATE KEY-----';
  const Footer = '-----END RSA PRIVATE KEY-----';
  var DERData: TUInt8Array;
begin
  Result := TKey.MakeInvalid;
  DERData := Base64ToASN1(Key, Header, Footer);
  if Length(DERData) = 0 then Exit;
  Result := ImportKeyPrivate_PKCS1_DER(DERData);
end;

class function TURSA.ImportKeyPublic_PKCS1(const Key: String): TURSA.TKey;
  const Header = '-----BEGIN RSA PUBLIC KEY-----';
  const Footer = '-----END RSA PUBLIC KEY-----';
  var DERData: TUInt8Array;
begin
  Result := TKey.MakeInvalid;
  DERData := Base64ToASN1(Key, Header, Footer);
  if Length(DERData) = 0 then Exit;
  Result := ImportKeyPublic_PKCS1_DER(DERData);
end;

class function TURSA.ImportKeyPrivate_PKCS8(const Key: String): TURSA.TKey;
  var DataDER: TUInt8Array;
  const Header = '-----BEGIN PRIVATE KEY-----';
  const Footer = '-----END PRIVATE KEY-----';
begin
  DataDER := Base64ToASN1(Key, Header, Footer);
  Result := ImportKeyPrivate_PKCS8_DER(DataDER);
end;

class function TURSA.ImportKeyPrivateEncrypted_PKCS5(
  const Key: String;
  const Password: TUInt8Array
): TURSA.TKey;
  function SkipChars(var Pos: Int32; const Chars: array of AnsiChar): Boolean;
    var i: Int32;
  begin
    while Pos < Length(Key) do
    begin
      Result := True;
      for i := 0 to High(Chars) do
      if Key[Pos] = Chars[i] then
      begin
        Result := False;
        Break;
      end;
      if Result then Exit;
      Inc(Pos);
    end;
    Result := False;
  end;
  function ParseLine(const Pos: Int32): String;
    var i: Int32;
  begin
    Result := '';
    i := Pos;
    while (i <= Length(Key))
    and (Key[i] <> #$d)
    and (Key[i] <> #$a) do
    begin
      Inc(i);
    end;
    i := i - Pos;
    if i = 0 then Exit;
    SetLength(Result, i);
    Move(Key[Pos], Result[1], i);
  end;
  function FindPattern(const Pos: Int32; const Pattern: String; const AtStart: Boolean): Int32;
    var i, j: Int32;
    var Match: Boolean;
  begin
    for i := Pos to Length(Key) do
    begin
      Match := True;
      for j := 0 to Length(Pattern) - 1 do
      if Key[i + j] <> Pattern[j + 1] then
      begin
        Match := False;
        Break;
      end;
      if Match then
      begin
        if AtStart then Exit(i) else Exit(i + Length(Pattern));
      end;
    end;
    Result := -1;
  end;
  const Header: String = '-----BEGIN RSA PRIVATE KEY-----';
  const Footer: String = '-----END RSA PRIVATE KEY-----';
  const ProcTypeMark: String = 'Proc-Type';
  const DEKInfoMark: String = 'DEK-Info';
  var KeyStart, KeyEnd: Int32;
  var ProcTypePos, DEKInfoPos: Int32;
  var ProcTypeStr, DEKInfoStr: String;
  var ProcType, DEKInfo: TUStrArray;
  var Alg: String;
  var Salt: TUInt8Array;
  var i: Int32;
  var KeyASN1: String;
  var AESKey128: TUAES.TKey128;
  var AESKey192: TUAES.TKey192;
  var AESKey256: TUAES.TKey256;
  var DESKey3: TUDES.TKey3;
  var DESKey: TUDES.TKey;
  var AESIV: TUAES.TInitVector;
  var DESIV: TUDES.TInitVector;
  var KeyEncrypted, KeyDER, EncKey, EncIV: TUInt8Array;
begin
  Result := TURSA.TKey.MakeInvalid;
  KeyStart := Key.IndexOf(Header);
  if KeyStart = -1 then Exit;
  KeyStart += Length(Header);
  KeyEnd := Key.IndexOf(Footer);
  if KeyEnd = -1 then Exit;
  if KeyStart > KeyEnd then Exit;
  ProcTypePos := Key.IndexOf(ProcTypeMark);
  if ProcTypePos = -1 then Exit;
  if (ProcTypePos < KeyStart) or (ProcTypePos > KeyEnd) then Exit;
  DEKInfoPos := Key.IndexOf(DEKInfoMark);
  if DEKInfoPos = -1 then Exit;
  if (DEKInfoPos < KeyStart) or (DEKInfoPos > KeyEnd) then Exit;
  i := ProcTypePos + Length(ProcTypeMark) + 1;
  if not SkipChars(i, [':', ' ']) then Exit;
  ProcTypeStr := ParseLine(i);
  if Length(ProcTypeStr) = 0 then Exit;
  i := DEKInfoPos + Length(DEKInfoMark) + 1;
  if not SkipChars(i, [':', ' ']) then Exit;
  DEKInfoStr := ParseLine(i);
  ProcType := UStrExplode(ProcTypeStr, ',', False);
  if Length(ProcType) < 2 then Exit;
  DEKInfo := UStrExplode(DEKInfoStr, ',', False);
  if Length(DEKInfo) < 2 then Exit;
  if ProcType[0].Trim <> '4' then Exit;
  if ProcType[1].Trim <> 'ENCRYPTED' then Exit;
  Alg := LowerCase(DEKInfo[0].Trim);
  Salt := UHexToBytes(DEKInfo[1]);
  i := FindPattern(DEKInfoPos, #$d#$a#$d#$a, False);
  if i = -1 then
  begin
    i := FindPattern(DEKInfoPos, #$a#$a, False);
    if i = -1 then Exit;
  end;
  KeyStart := i;
  i := FindPattern(KeyStart, #$d#$a#$d#$a, True);
  if i = -1 then
  begin
    i := FindPattern(KeyStart, #$a#$a, False);
    if i = -1 then Exit;
  end;
  KeyEnd := i;
  WriteLn(Key[KeyStart]);
  WriteLn(Key[KeyEnd]);
  KeyASN1 := Key.Substring(KeyStart - 1, KeyEnd - KeyStart);
  KeyASN1 := StringReplace(KeyASN1, #$d#$a, '', [rfReplaceAll]);
  KeyASN1 := StringReplace(KeyASN1, #$a, '', [rfReplaceAll]);
  KeyEncrypted := UBase64ToBytes(KeyASN1);
  if Alg = 'des-cbc' then
  begin
    EncKey := UEvpKDF_MD5(
      Password, Salt,
      SizeOf(TUDES.TKey),
      SizeOf(TUAES.TInitVector),
      1, EncIV
    );
    DESKey := TUDES.MakeKey(EncKey);
    DESIV := TUDES.MakeIV(EncIV);
    KeyDER := UDecrypt_DES_PKCS7_CBC(KeyEncrypted, DESKey, DESIV);
  end
  else if Alg = 'des-ede3-cbc' then
  begin
    EncKey := UEvpKDF_MD5(
      Password, Salt,
      SizeOf(TUDES.TKey3),
      SizeOf(TUAES.TInitVector),
      1, EncIV
    );
    DESKey3 := TUDES.MakeKey3(EncKey);
    DESIV := TUDES.MakeIV(EncIV);
    KeyDER := UDecrypt_DES_Triple_PKCS7_CBC(KeyEncrypted, DESKey3, DESIV);
  end
  else if Alg = 'aes-128-cbc' then
  begin
    EncKey := UEvpKDF_SHA256(
      Password, Salt,
      SizeOf(TUAES.TKey128),
      SizeOf(TUAES.TInitVector),
      1, EncIV
    );
    AESKey128 := TUAES.MakeKey128(EncKey);
    AESIV := TUAES.MakeIV(EncIV);
    KeyDER := UDecrypt_AES_PKCS7_CBC_128(KeyEncrypted, AESKey128, AESIV);
  end
  else if Alg = 'aes-192-cbc' then
  begin
    EncKey := UEvpKDF_SHA256(
      Password, Salt,
      SizeOf(TUAES.TKey192),
      SizeOf(TUAES.TInitVector),
      1, EncIV
    );
    AESKey192 := TUAES.MakeKey192(EncKey);
    AESIV := TUAES.MakeIV(EncIV);
    KeyDER := UDecrypt_AES_PKCS7_CBC_192(KeyEncrypted, AESKey192, AESIV);
  end
  else if Alg = 'aes-256-cbc' then
  begin
    EncKey := UEvpKDF_SHA256(
      Password, Salt,
      SizeOf(TUAES.TKey256),
      SizeOf(TUAES.TInitVector),
      1, EncIV
    );
    AESKey256 := TUAES.MakeKey256(EncKey);
    AESIV := TUAES.MakeIV(EncIV);
    KeyDER := UDecrypt_AES_PKCS7_CBC_256(KeyEncrypted, AESKey256, AESIV);
  end;
  if Length(KeyDER) = 0 then Exit;
  Result := ImportKeyPrivate_PKCS1_DER(KeyDER);
end;

class function TURSA.ImportKeyPrivateEncrypted_PKCS8(
  const Key: String;
  const Password: TUInt8Array
): TURSA.TKey;
  function GetKeySize(const ALG_OID: TUInt8Array): Int32;
  begin
    if UBytesEqual(ALG_OID, OID_AES128_ECB)
    or UBytesEqual(ALG_OID, OID_AES128_CBC) then
    begin
      Exit(SizeOf(TUAES.TKey128));
    end
    else if UBytesEqual(ALG_OID, OID_AES192_ECB)
    or UBytesEqual(ALG_OID, OID_AES192_CBC) then
    begin
      Exit(SizeOf(TUAES.TKey192));
    end
    else if UBytesEqual(ALG_OID, OID_AES256_ECB)
    or UBytesEqual(ALG_OID, OID_AES256_CBC) then
    begin
      Exit(SizeOf(TUAES.TKey256));
    end
    else if UBytesEqual(ALG_OID, OID_DES_EDE3_CBC) then
    begin
      Exit(SizeOf(TUDES.TKey3));
    end
    else
    begin
      Exit(SizeOf(TUDES.TKey));
    end;
  end;
  var Index: Int32;
  var Tag: UInt8;
  var OID, DERData, MainContent: TUInt8Array;
  var Enc_AlgId, EncryptedData, PBES2_Params, PBKDF2_Params: TUInt8Array;
  var KDF_AlgId, EncScheme_AlgId, KeyPKCS8, PRF_AlgId: TUInt8Array;
  var IV, Salt, IterationCountDER, DerivedKey: TUInt8Array;
  var AESKey128: TUAES.TKey128;
  var AESKey192: TUAES.TKey192;
  var AESKey256: TUAES.TKey256;
  var AESIV: TUAES.TInitVector;
  var DESKey3: TUDES.TKey3;
  var DESKey: TUDES.TKey;
  var DESIV: TUDES.TInitVector;
  var IterationCount: Int32;
  var HMAC_OID, ALG_OID: TUInt8Array;
  const Header: String = '-----BEGIN ENCRYPTED PRIVATE KEY-----';
  const Footer: String = '-----END ENCRYPTED PRIVATE KEY-----';
begin
  Result := TKey.MakeInvalid;
  DERData := Base64ToASN1(Key, Header, Footer);
  Index := 0;
  MainContent := DecodeTLV(DERData, Index, Tag);
  if Tag <> $30 then Exit;
  Index := 0;
  Enc_AlgId := DecodeTLV(MainContent, Index, Tag);
  if Tag <> $30 then Exit;
  EncryptedData := DecodeTLV(MainContent, Index, Tag);
  if Tag <> $04 then Exit;
  Index := 0;
  OID := DecodeTLV(Enc_AlgId, Index, Tag);
  if Tag <> $06 then Exit;
  if not UBytesEqual(OID, OID_PBES2) then Exit;
  PBES2_Params := DecodeTLV(Enc_AlgId, Index, Tag);
  if Tag <> $30 then Exit;
  Index := 0;
  KDF_AlgId := DecodeTLV(PBES2_Params, Index, Tag);
  if Tag <> $30 then Exit;
  EncScheme_AlgId := DecodeTLV(PBES2_Params, Index, Tag);
  if Tag <> $30 then Exit;
  Index := 0;
  OID := DecodeTLV(KDF_AlgId, Index, Tag);
  if Tag <> $06 then Exit;
  if not UBytesEqual(OID, OID_PBKDF2) then Exit;
  PBKDF2_Params := DecodeTLV(KDF_AlgId, Index, Tag);
  if Tag <> $30 then Exit;
  Index := 0;
  Salt := DecodeTLV(PBKDF2_Params, Index, Tag);
  if Tag <> $04 then Exit;
  IterationCountDER := DecodeTLV(PBKDF2_Params, Index, Tag);
  if Tag <> $02 then Exit;
  IterationCount := UnpackDER(IterationCountDER).ToInt;
  PRF_AlgId := DecodeTLV(PBKDF2_Params, Index, Tag);
  if Length(PRF_AlgId) = 0 then
  begin
    HMAC_OID := nil;
  end
  else
  begin
    if (Tag <> $30) then Exit;
    Index := 0;
    HMAC_OID := DecodeTLV(PRF_AlgId, Index, Tag);
    if Tag <> $06 then Exit;
  end;
  Index := 0;
  ALG_OID := DecodeTLV(EncScheme_AlgId, Index, Tag);
  if Tag <> $06 then Exit;
  IV := DecodeTLV(EncScheme_AlgId, Index, Tag);
  if Tag <> $04 then Exit;
  if UBytesEqual(HMAC_OID, OID_HMAC_SHA256) then
  begin
    DerivedKey := UPBKDF2_HMAC_SHA256(Password, Salt, GetKeySize(ALG_OID), IterationCount);
  end
  else if UBytesEqual(HMAC_OID, OID_HMAC_SHA512) then
  begin
    DerivedKey := UPBKDF2_HMAC_SHA512(Password, Salt, GetKeySize(ALG_OID), IterationCount);
  end
  else
  begin
    DerivedKey := UPBKDF2_HMAC_SHA1(Password, Salt, GetKeySize(ALG_OID), IterationCount);
  end;
  if UBytesEqual(ALG_OID, OID_AES128_ECB) then
  begin
    AESKey128 := TUAES.MakeKey128(DerivedKey);
    KeyPKCS8 := UDecrypt_AES_PKCS7_ECB_128(EncryptedData, AESKey128);
  end
  else if UBytesEqual(ALG_OID, OID_AES128_CBC) then
  begin
    AESKey128 := TUAES.MakeKey128(DerivedKey);
    AESIV := TUAES.MakeIV(IV);
    KeyPKCS8 := UDecrypt_AES_PKCS7_CBC_128(EncryptedData, AESKey128, AESIV);
  end
  else if UBytesEqual(ALG_OID, OID_AES192_ECB) then
  begin
    AESKey192 := TUAES.MakeKey192(DerivedKey);
    KeyPKCS8 := UDecrypt_AES_PKCS7_CBC_192(EncryptedData, AESKey192, AESIV);
  end
  else if UBytesEqual(ALG_OID, OID_AES192_CBC) then
  begin
    AESKey192 := TUAES.MakeKey192(DerivedKey);
    AESIV := TUAES.MakeIV(IV);
    KeyPKCS8 := UDecrypt_AES_PKCS7_CBC_192(EncryptedData, AESKey192, AESIV);
  end
  else if UBytesEqual(ALG_OID, OID_AES256_ECB) then
  begin
    AESKey256 := TUAES.MakeKey256(DerivedKey);
    KeyPKCS8 := UDecrypt_AES_PKCS7_CBC_256(EncryptedData, AESKey256, AESIV);
  end
  else if UBytesEqual(ALG_OID, OID_AES256_CBC) then
  begin
    AESKey256 := TUAES.MakeKey256(DerivedKey);
    AESIV := TUAES.MakeIV(IV);
    KeyPKCS8 := UDecrypt_AES_PKCS7_CBC_256(EncryptedData, AESKey256, AESIV);
  end
  else if UBytesEqual(ALG_OID, OID_DES_EDE3_CBC) then
  begin
    DESKey3 := TUDES.MakeKey3(DerivedKey);
    DESIV := TUDES.MakeIV(IV);
    KeyPKCS8 := UDecrypt_DES_Triple_PKCS7_CBC(EncryptedData, DESKey3, DESIV);
  end
  else
  begin
    DESKey := TUDES.MakeKey(DerivedKey);
    DESIV := TUDES.MakeIV(IV);
    KeyPKCS8 := UDecrypt_DES_PKCS7_CBC(EncryptedData, DESKey, DESIV);
  end;
  Result := ImportKeyPrivate_PKCS8_DER(KeyPKCS8);
end;

class function TURSA.ImportKeyPublic_X509(const Key: String): TURSA.TKey;
  var Index, i: Int32;
  var Tag: UInt8;
  var DERData: TUInt8Array;
  var MainContent, AlgIdContent, AlgId, BitStringContent, PKCS1_Content: TUInt8Array;
  var n_bytes, e_bytes: TUInt8Array;
  const Header: String = '-----BEGIN PUBLIC KEY-----';
  const Footer: String = '-----END PUBLIC KEY-----';
begin
  Result := TKey.MakeInvalid;
  Index := 0;
  DERData := Base64ToASN1(Key, Header, Footer);
  MainContent := DecodeTLV(DERData, Index, Tag);
  if Tag <> $30 then Exit;
  Index := 0;
  AlgIdContent := DecodeTLV(MainContent, Index, Tag);
  if Tag <> $30 then Exit;
  i := 0;
  AlgId := DecodeTLV(AlgIdContent, i, Tag);
  if (Tag <> $06) or (Length(AlgId) <> Length(OID_RSA)) then Exit;
  for i := 0 to High(OID_RSA) do if AlgId[i] <> OID_RSA[i] then Exit;
  BitStringContent := DecodeTLV(MainContent, Index, Tag);
  if Tag <> $03 then Exit;
  if (Length(BitStringContent) = 0) or (BitStringContent[0] <> 0) then Exit;
  Index := 1;
  PKCS1_Content := DecodeTLV(BitStringContent, Index, Tag);
  if Tag <> $30 then Exit;
  Index := 0;
  n_bytes := DecodeTLV(PKCS1_Content, Index, Tag);
  if Tag <> $02 then Exit;
  e_bytes := DecodeTLV(PKCS1_Content, Index, Tag);
  if Tag <> $02 then Exit;
  Result.n := UnpackDER(n_bytes);
  Result.e := UnpackDER(e_bytes);
end;

class function TURSA.ImportKey(
  const Key: String;
  const Password: TUInt8Array
): TURSA.TKey;
  type TKeyType = record
    Header: String;
    Footer: String;
  end;
  function CheckKeyType(const KeyType: TKeyType): Boolean;
    var KeyStart, KeyEnd: Int32;
  begin
    KeyStart := Key.IndexOf(KeyType.Header);
    if KeyStart = -1 then Exit(False);
    KeyStart += Length(KeyType.Header);
    KeyEnd := Key.IndexOf(KeyType.Footer);
    if KeyEnd < -1 then Exit(False);
    if KeyStart > KeyEnd then Exit(False);
    Result := True;
  end;
  const KeyPrivatePKCS1: TKeyType = (
    Header: '-----BEGIN RSA PRIVATE KEY-----';
    Footer: '-----END RSA PRIVATE KEY-----';
  );
  const KeyPublicPKCS1: TKeyType = (
    Header: '-----BEGIN RSA PUBLIC KEY-----';
    Footer: '-----END RSA PUBLIC KEY-----';
  );
  const KeyPublicX509: TKeyType = (
    Header: '-----BEGIN PUBLIC KEY-----';
    Footer: '-----END PUBLIC KEY-----';
  );
  const KeyPrivatePKCS8: TKeyType = (
    Header: '-----BEGIN PRIVATE KEY-----';
    Footer: '-----END PRIVATE KEY-----';
  );
  const KeyPrivateEncryptedPKCS8: TKeyType = (
    Header: '-----BEGIN ENCRYPTED PRIVATE KEY-----';
    Footer: '-----END ENCRYPTED PRIVATE KEY-----';
  );
begin
  if CheckKeyType(KeyPrivatePKCS1) then
  begin
    Exit(TURSA.ImportKeyPrivate_PKCS1(Key));
  end
  else if CheckKeyType(KeyPublicPKCS1) then
  begin
    Exit(TURSA.ImportKeyPublic_PKCS1(Key));
  end
  else if CheckKeyType(KeyPublicX509) then
  begin
    Exit(TURSA.ImportKeyPublic_X509(Key));
  end
  else if CheckKeyType(KeyPrivatePKCS8) then
  begin
    Exit(TURSA.ImportKeyPrivate_PKCS8(Key));
  end
  else if CheckKeyType(KeyPrivateEncryptedPKCS8) then
  begin
    Exit(TURSA.ImportKeyPrivateEncrypted_PKCS8(Key, Password))
  end;
  Result := TURSA.TKey.MakeInvalid;
end;

procedure TUMontgomeryReduction.Init(const Modulus: TUCryptoInt);
  var inv: UInt32;
  var i: Int32;
  var R_val, R2_val, R_mod_N: TUCryptoInt;
begin
  N := Modulus;
  NumItems := N.Top + 1;
  inv := 1;
  for i := 1 to 5 do
  begin
    inv := inv * (2 - N[0] * inv);
  end;
  N_prime := -inv;
  R_val := TUCryptoInt.Zero;
  if NumItems <= TUCryptoInt.MaxItem then
  begin
    R_val[NumItems] := 1;
  end;
  R_mod_N := R_val mod N;
  R2_val := R_mod_N * R_mod_N;
  R2_mod_N := R2_val mod N;
end;

function TUMontgomeryReduction.Conv(const Number: TUCryptoInt): TUCryptoInt;
begin
  Result := Mul(Number, R2_mod_N);
end;

function TUMontgomeryReduction.Mul(const a, b: TUCryptoInt): TUCryptoInt;
  var T: TUCryptoInt;
  var m: UInt32;
  var i, j, k: Int32;
  var Carry: UInt64;
  var Product, Sum: UInt64;
begin
  Result := TUCryptoInt.Zero;
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
    while (Carry > 0) and (k <= TUCryptoInt.MaxItem) do
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
  if Result >= N then
  begin
    Result := Result - N;
  end;
end;

function UMD5(const Data: Pointer; const DataSize: UInt32): TUDigestMD5;
function RotateLeft(const Value: UInt32; const Amount: Int32): UInt32; inline;
  begin
    Result := (Value shl Amount) or (Value shr (32 - Amount));
  end;
  procedure FF(var a: UInt32; b, c, d, x: UInt32; s: Integer; ac: UInt32); inline;
    function F(const x, y, z: UInt32): UInt32; inline;
    begin
      Result := (x and y) or ((not x) and z);
    end;
  begin
    a := a + F(b, c, d) + x + ac;
    a := RotateLeft(a, s);
    a := a + b;
  end;
  procedure GG(var a: UInt32; b, c, d, x: UInt32; s: Integer; ac: UInt32); inline;
    function G(const x, y, z: UInt32): UInt32; inline;
    begin
      Result := (x and z) or (y and (not z));
    end;
  begin
    a := a + G(b, c, d) + x + ac;
    a := RotateLeft(a, s);
    a := a + b;
  end;
  procedure HH(var a: UInt32; b, c, d, x: UInt32; s: Integer; ac: UInt32); inline;
    function H(const x, y, z: UInt32): UInt32; inline;
    begin
      Result := x xor y xor z;
    end;
  begin
    a := a + H(b, c, d) + x + ac;
    a := RotateLeft(a, s);
    a := a + b;
  end;
  procedure II(var a: UInt32; b, c, d, x: UInt32; s: Integer; ac: UInt32); inline;
    function I(const x, y, z: UInt32): UInt32; inline;
    begin
      Result := y xor (x or (not z));
    end;
  begin
    a := a + I(b, c, d) + x + ac;
    a := RotateLeft(a, s);
    a := a + b;
  end;
  var h0, h1, h2, h3: UInt32;
  var a, b, c, d: UInt32;
  var PaddedData: TUInt8Array;
  var OriginalLength: UInt64;
  var PaddedLength: Int32;
  var i, j: Int32;
  var w: array[0..15] of UInt32;
begin
  h0 := $67452301;
  h1 := $efcdab89;
  h2 := $98badcfe;
  h3 := $10325476;
  OriginalLength := UInt64(DataSize) * 8;
  PaddedLength := DataSize;
  Inc(PaddedLength);
  while (PaddedLength mod 64) <> 56 do Inc(PaddedLength);
  Inc(PaddedLength, 8);
  PaddedData := nil;
  SetLength(PaddedData, PaddedLength);
  if DataSize > 0 then
  begin
    Move(Data^, PaddedData[0], DataSize);
  end;
  PaddedData[DataSize] := $80;
  for i := DataSize + 1 to PaddedLength - 9 do
  begin
    PaddedData[i] := 0;
  end;
  Move(OriginalLength, PaddedData[PaddedLength - 8], 8);
  i := 0;
  while i < PaddedLength do
  begin
    for j := 0 to 15 do
    begin
      w[j] := (
        PaddedData[i + j * 4] or
        (PaddedData[i + j * 4 + 1] shl 8) or
        (PaddedData[i + j * 4 + 2] shl 16) or
        (PaddedData[i + j * 4 + 3] shl 24)
      );
    end;
    a := h0;
    b := h1;
    c := h2;
    d := h3;
    // Round 1
    FF(a, b, c, d, w[0],  7,  $d76aa478);
    FF(d, a, b, c, w[1],  12, $e8c7b756);
    FF(c, d, a, b, w[2],  17, $242070db);
    FF(b, c, d, a, w[3],  22, $c1bdceee);
    FF(a, b, c, d, w[4],  7,  $f57c0faf);
    FF(d, a, b, c, w[5],  12, $4787c62a);
    FF(c, d, a, b, w[6],  17, $a8304613);
    FF(b, c, d, a, w[7],  22, $fd469501);
    FF(a, b, c, d, w[8],  7,  $698098d8);
    FF(d, a, b, c, w[9],  12, $8b44f7af);
    FF(c, d, a, b, w[10], 17, $ffff5bb1);
    FF(b, c, d, a, w[11], 22, $895cd7be);
    FF(a, b, c, d, w[12], 7,  $6b901122);
    FF(d, a, b, c, w[13], 12, $fd987193);
    FF(c, d, a, b, w[14], 17, $a679438e);
    FF(b, c, d, a, w[15], 22, $49b40821);
    // Round 2
    GG(a, b, c, d, w[1],  5,  $f61e2562);
    GG(d, a, b, c, w[6],  9,  $c040b340);
    GG(c, d, a, b, w[11], 14, $265e5a51);
    GG(b, c, d, a, w[0],  20, $e9b6c7aa);
    GG(a, b, c, d, w[5],  5,  $d62f105d);
    GG(d, a, b, c, w[10], 9,  $02441453);
    GG(c, d, a, b, w[15], 14, $d8a1e681);
    GG(b, c, d, a, w[4],  20, $e7d3fbc8);
    GG(a, b, c, d, w[9],  5,  $21e1cde6);
    GG(d, a, b, c, w[14], 9,  $c33707d6);
    GG(c, d, a, b, w[3],  14, $f4d50d87);
    GG(b, c, d, a, w[8],  20, $455a14ed);
    GG(a, b, c, d, w[13], 5,  $a9e3e905);
    GG(d, a, b, c, w[2],  9,  $fcefa3f8);
    GG(c, d, a, b, w[7],  14, $676f02d9);
    GG(b, c, d, a, w[12], 20, $8d2a4c8a);
    // Round 3
    HH(a, b, c, d, w[5],  4,  $fffa3942);
    HH(d, a, b, c, w[8],  11, $8771f681);
    HH(c, d, a, b, w[11], 16, $6d9d6122);
    HH(b, c, d, a, w[14], 23, $fde5380c);
    HH(a, b, c, d, w[1],  4,  $a4beea44);
    HH(d, a, b, c, w[4],  11, $4bdecfa9);
    HH(c, d, a, b, w[7],  16, $f6bb4b60);
    HH(b, c, d, a, w[10], 23, $bebfbc70);
    HH(a, b, c, d, w[13], 4,  $289b7ec6);
    HH(d, a, b, c, w[0],  11, $eaa127fa);
    HH(c, d, a, b, w[3],  16, $d4ef3085);
    HH(b, c, d, a, w[6],  23, $04881d05);
    HH(a, b, c, d, w[9],  4,  $d9d4d039);
    HH(d, a, b, c, w[12], 11, $e6db99e5);
    HH(c, d, a, b, w[15], 16, $1fa27cf8);
    HH(b, c, d, a, w[2],  23, $c4ac5665);
    // Round 4
    II(a, b, c, d, w[0],  6,  $f4292244);
    II(d, a, b, c, w[7],  10, $432aff97);
    II(c, d, a, b, w[14], 15, $ab9423a7);
    II(b, c, d, a, w[5],  21, $fc93a039);
    II(a, b, c, d, w[12], 6,  $655b59c3);
    II(d, a, b, c, w[3],  10, $8f0ccc92);
    II(c, d, a, b, w[10], 15, $ffeff47d);
    II(b, c, d, a, w[1],  21, $85845dd1);
    II(a, b, c, d, w[8],  6,  $6fa87e4f);
    II(d, a, b, c, w[15], 10, $fe2ce6e0);
    II(c, d, a, b, w[6],  15, $a3014314);
    II(b, c, d, a, w[13], 21, $4e0811a1);
    II(a, b, c, d, w[4],  6,  $f7537e82);
    II(d, a, b, c, w[11], 10, $bd3af235);
    II(c, d, a, b, w[2],  15, $2ad7d2bb);
    II(b, c, d, a, w[9],  21, $eb86d391);
    h0 := h0 + a;
    h1 := h1 + b;
    h2 := h2 + c;
    h3 := h3 + d;
    Inc(i, 64);
  end;
  Move(h0, Result[0], 4);
  Move(h1, Result[4], 4);
  Move(h2, Result[8], 4);
  Move(h3, Result[12], 4);
end;

function UMD5(const Data: TUInt8Array): TUDigestMD5;
begin
  Result := UMD5(@Data[0], Length(Data));
end;

function UMD5(const Data: String): TUDigestMD5;
begin
  Result := UMD5(@Data[1], Length(Data));
end;

function USHA1(const Data: Pointer; const DataSize: UInt32): TUDigestSHA1;
  function ROTL(const x: UInt32; const n: UInt8): UInt32;
  begin
    Result := (x shl n) or (x shr (32 - n));
  end;
  var H: array[0..4] of UInt32 = (
    $67452301,
    $efcdab89,
    $98badcfe,
    $10325476,
    $c3d2e1f0
  );
  var W: array[0..79] of UInt32;
  var a, b, c, d, e, f, temp: UInt32;
  var k: UInt32;
  var i, t: Int32;
  var PaddedData: TUInt8Array;
  var OriginalLengthInBits: UInt64;
  var PadLen: Int32;
begin
  OriginalLengthInBits := UInt64(DataSize) * 8;
  PadLen := 55 - (DataSize mod 64);
  if PadLen < 0 then PadLen := PadLen + 64;
  PaddedData := nil;
  SetLength(PaddedData, DataSize + PadLen + 1 + 8);
  if DataSize > 0 then
  begin
    Move(Data^, PaddedData[0], DataSize);
  end;
  PaddedData[DataSize] := $80;
  for i := DataSize + 1 to High(PaddedData) - 8 do
  begin
    PaddedData[i] := $00;
  end;
  PaddedData[High(PaddedData) - 7] := Byte((OriginalLengthInBits shr 56) and $ff);
  PaddedData[High(PaddedData) - 6] := Byte((OriginalLengthInBits shr 48) and $ff);
  PaddedData[High(PaddedData) - 5] := Byte((OriginalLengthInBits shr 40) and $ff);
  PaddedData[High(PaddedData) - 4] := Byte((OriginalLengthInBits shr 32) and $ff);
  PaddedData[High(PaddedData) - 3] := Byte((OriginalLengthInBits shr 24) and $ff);
  PaddedData[High(PaddedData) - 2] := Byte((OriginalLengthInBits shr 16) and $ff);
  PaddedData[High(PaddedData) - 1] := Byte((OriginalLengthInBits shr 8) and $ff);
  PaddedData[High(PaddedData) - 0] := Byte(OriginalLengthInBits and $ff);
  for i := 0 to (Length(PaddedData) div 64) - 1 do
  begin
    for t := 0 to 15 do
    begin
      W[t] := (
        (UInt32(PaddedData[i * 64 + t * 4 + 0]) shl 24) or
        (UInt32(PaddedData[i * 64 + t * 4 + 1]) shl 16) or
        (UInt32(PaddedData[i * 64 + t * 4 + 2]) shl 8) or
        (UInt32(PaddedData[i * 64 + t * 4 + 3]) shl 0)
      );
    end;
    for t := 16 to 79 do
    begin
      W[t] := ROTL(W[t - 3] xor W[t - 8] xor W[t - 14] xor W[t - 16], 1);
    end;
    a := H[0]; b := H[1]; c := H[2]; d := H[3]; e := H[4];
    for t := 0 to 79 do
    begin
      if t < 20 then
      begin
        f := (b and c) or ((not b) and d);
        k := $5a827999;
      end
      else if t < 40 then
      begin
        f := b xor c xor d;
        k := $6ed9eba1;
      end
      else if t < 60 then
      begin
        f := (b and c) or (b and d) or (c and d);
        k := $8f1bbcdc;
      end
      else
      begin
        f := b xor c xor d;
        k := $ca62c1d6;
      end;
      temp := ROTL(a, 5) + f + e + k + W[t];
      e := d;
      d := c;
      c := ROTL(b, 30);
      b := a;
      a := temp;
    end;
    H[0] := H[0] + a;
    H[1] := H[1] + b;
    H[2] := H[2] + c;
    H[3] := H[3] + d;
    H[4] := H[4] + e;
  end;
  for i := 0 to 4 do
  begin
    Result[i * 4 + 0] := Byte((H[i] shr 24) and $ff);
    Result[i * 4 + 1] := Byte((H[i] shr 16) and $ff);
    Result[i * 4 + 2] := Byte((H[i] shr 8) and $ff);
    Result[i * 4 + 3] := Byte(H[i] and $ff);
  end;
end;

function USHA1(const Data: TUInt8Array): TUDigestSHA1;
begin
  Result := USHA1(@Data[0], Length(Data));
end;

function USHA1(const Data: String): TUDigestSHA1;
begin
  Result := USHA1(@Data[1], Length(Data));
end;

function USHA2_256(const Data: Pointer; const DataSize: UInt32): TUDigestSHA2_256;
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
  var PaddedLength: Int32;
  var i, j, ChunkStart: Int32;
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

function USHA2_256(const Data: TUInt8Array): TUDigestSHA2_256;
begin
  Result := USHA2_256(@Data[0], Length(Data));
end;

function USHA2_256(const Data: String): TUDigestSHA2_256;
begin
  Result := USHA2_256(@Data[1], Length(Data));
end;

function UModInverse(const e, phi: TUCryptoInt): TUCryptoInt;
  function GCD(const a, b: TUCryptoInt; var x: TUCryptoInt; var y: TUCryptoInt): TUCryptoInt;
    var x1, y1, gcd_val: TUCryptoInt;
  begin
    if a = TUCryptoInt.Zero then
    begin
      x := TUCryptoInt.Zero;
      y := TUCryptoInt.One;
      Exit(b);
    end;
    x1 := TUCryptoInt.Zero;
    y1 := TUCryptoInt.Zero;
    gcd_val := GCD(b mod a, a, x1, y1);
    x := y1 - (b div a) * x1;
    y := x1;
    Result := gcd_val;
  end;
  var x, y, gcd_val: TUCryptoInt;
begin
  x := TUCryptoInt.Zero;
  y := TUCryptoInt.Zero;
  gcd_val := GCD(e, phi, x, y);
  if gcd_val <> TUCryptoInt.One then Exit(TUCryptoInt.Invalid);
  if x < TUCryptoInt.Zero then x := x + phi;
  Result := x;
end;

function UPowMod(const Base, Exp, Modulus: TUCryptoInt): TUCryptoInt;
  var Context: TUMontgomeryReduction;
  var BaseMont: TUCryptoInt;
  var ResultMont: TUCryptoInt;
  var CurrentExponent: TUCryptoInt;
begin
  Context.Init(Modulus);
  BaseMont := Context.Conv(Base);
  ResultMont := Context.Conv(TUCryptoInt.One);
  CurrentExponent := Exp;
  while not CurrentExponent.IsZero do
  begin
    if CurrentExponent.IsOdd then
    begin
      ResultMont := Context.Mul(ResultMont, BaseMont);
    end;
    BaseMont := Context.Mul(BaseMont, BaseMont);
    CurrentExponent := TUCryptoInt.ShrOne(CurrentExponent);
  end;
  Result := Context.Mul(ResultMont, TUCryptoInt.One);
end;

function UMillerRabinTest(const Number: TUCryptoInt; const Iterations: Int32): Boolean;
  var Two: TUCryptoInt;
  var Cmp: Int8;
  var i: Int32;
  var m, m_temp: UInt32;
  var d, a, x, n_minus_1, n_minus_2: TUCryptoInt;
begin
  Two := 2;
  Cmp := TUCryptoInt.Compare(Number, Two);
  if Cmp < 0 then Exit(False);
  if Cmp = 0 then Exit(True);
  if not Number.IsOdd then Exit(False);
  n_minus_1 := Number - TUCryptoInt.One;
  d := n_minus_1;
  m := 0;
  while not d.IsOdd and (d > TUCryptoInt.Zero) do
  begin
    d := TUCryptoInt.ShrOne(d);
    Inc(m);
  end;
  n_minus_2 := Number - Two;
  for i := 1 to Iterations do
  begin
    a := TUCryptoInt.MakeRandomRange(Two, n_minus_2);
    x := UPowMod(a, d, Number);
    if (x = TUCryptoInt.One) or (x = n_minus_1) then Continue;
    m_temp := m;
    while m_temp > 1 do
    begin
      x := UPowMod(x, Two, Number);
      if x = TUCryptoInt.One then Exit(False);
      if x = n_minus_1 then Break;
      Dec(m_temp);
    end;
    if not (x = n_minus_1) then Exit(False);
  end;
  Result := True;
end;

function UMakePrime(const BitCount: Int32): TUCryptoInt;
  var TestCount: Int32;
begin
  TestCount := 0;
  repeat
    Result := TUCryptoInt.MakeRandom(BitCount);
    Result := Result or (TUCryptoInt.One shl (BitCount - 1));
    Result[0] := Result[0] or 1;
    Inc(TestCount);
    if TestCount mod 100 = 0 then WriteLn(TestCount);
  until UMillerRabinTest(Result, 100);
end;

function USHA2_512(const Data: Pointer; const DataSize: UInt32): TUDigestSHA2_512;
  const K: array[0..79] of UInt64 = (
    UInt64($428a2f98d728ae22), UInt64($7137449123ef65cd), UInt64($b5c0fbcfec4d3b2f), UInt64($e9b5dba58189dbbc),
    UInt64($3956c25bf348b538), UInt64($59f111f1b605d019), UInt64($923f82a4af194f9b), UInt64($ab1c5ed5da6d8118),
    UInt64($d807aa98a3030242), UInt64($12835b0145706fbe), UInt64($243185be4ee4b28c), UInt64($550c7dc3d5ffb4e2),
    UInt64($72be5d74f27b896f), UInt64($80deb1fe3b1696b1), UInt64($9bdc06a725c71235), UInt64($c19bf174cf692694),
    UInt64($e49b69c19ef14ad2), UInt64($efbe4786384f25e3), UInt64($0fc19dc68b8cd5b5), UInt64($240ca1cc77ac9c65),
    UInt64($2de92c6f592b0275), UInt64($4a7484aa6ea6e483), UInt64($5cb0a9dcbd41fbd4), UInt64($76f988da831153b5),
    UInt64($983e5152ee66dfab), UInt64($a831c66d2db43210), UInt64($b00327c898fb213f), UInt64($bf597fc7beef0ee4),
    UInt64($c6e00bf33da88fc2), UInt64($d5a79147930aa725), UInt64($06ca6351e003826f), UInt64($142929670a0e6e70),
    UInt64($27b70a8546d22ffc), UInt64($2e1b21385c26c926), UInt64($4d2c6dfc5ac42aed), UInt64($53380d139d95b3df),
    UInt64($650a73548baf63de), UInt64($766a0abb3c77b2a8), UInt64($81c2c92e47edaee6), UInt64($92722c851482353b),
    UInt64($a2bfe8a14cf10364), UInt64($a81a664bbc423001), UInt64($c24b8b70d0f89791), UInt64($c76c51a30654be30),
    UInt64($d192e819d6ef5218), UInt64($d69906245565a910), UInt64($f40e35855771202a), UInt64($106aa07032bbd1b8),
    UInt64($19a4c116b8d2d0c8), UInt64($1e376c085141ab53), UInt64($2748774cdf8eeb99), UInt64($34b0bcb5e19b48a8),
    UInt64($391c0cb3c5c95a63), UInt64($4ed8aa4ae3418acb), UInt64($5b9cca4f7763e373), UInt64($682e6ff3d6b2b8a3),
    UInt64($748f82ee5defb2fc), UInt64($78a5636f43172f60), UInt64($84c87814a1f0ab72), UInt64($8cc702081a6439ec),
    UInt64($90befffa23631e28), UInt64($a4506cebde82bde9), UInt64($bef9a3f7b2c67915), UInt64($c67178f2e372532b),
    UInt64($ca273eceea26619c), UInt64($d186b8c721c0c207), UInt64($eada7dd6cde0eb1e), UInt64($f57d4f7fee6ed178),
    UInt64($06f067aa72176fba), UInt64($0a637dc5a2c898a6), UInt64($113f9804bef90dae), UInt64($1b710b35131c471b),
    UInt64($28db77f523047d84), UInt64($32caab7b40c72493), UInt64($3c9ebe0a15c9bebc), UInt64($431d67c49c100d4c),
    UInt64($4cc5d4becb3e42b6), UInt64($597f299cfc657e2a), UInt64($5fcb6fab3ad6faec), UInt64($6c44198c4a475817)
  );
  function ROTR64(x: UInt64; n: Byte): UInt64;
  begin
    Result := (x shr n) or (x shl (64 - n));
  end;
  function Ch(x, y, z: UInt64): UInt64;
  begin
    Result := (x and y) xor ((not x) and z);
  end;
  function Maj(x, y, z: UInt64): UInt64;
  begin
    Result := (x and y) xor (x and z) xor (y and z);
  end;
  function Sigma0(x: UInt64): UInt64;
  begin
    Result := ROTR64(x, 28) xor ROTR64(x, 34) xor ROTR64(x, 39);
  end;
  function Sigma1(x: UInt64): UInt64;
  begin
    Result := ROTR64(x, 14) xor ROTR64(x, 18) xor ROTR64(x, 41);
  end;
  function Sigma0Small(x: UInt64): UInt64;
  begin
    Result := ROTR64(x, 1) xor ROTR64(x, 8) xor (x shr 7);
  end;
  function Sigma1Small(x: UInt64): UInt64;
  begin
    Result := ROTR64(x, 19) xor ROTR64(x, 61) xor (x shr 6);
  end;
  var H: array[0..7] of UInt64 = (
    UInt64($6a09e667f3bcc908),
    UInt64($bb67ae8584caa73b),
    UInt64($3c6ef372fe94f82b),
    UInt64($a54ff53a5f1d36f1),
    UInt64($510e527fade682d1),
    UInt64($9b05688c2b3e6c1f),
    UInt64($1f83d9abfb41bd6b),
    UInt64($5be0cd19137e2179)
  );
  var W: array[0..79] of UInt64;
  var wv_a, wv_b, wv_c, wv_d, wv_e, wv_f, wv_g, wv_h: UInt64;
  var T1, T2: UInt64;
  var i, j, t: Int32;
  var PaddedData: TUInt8Array;
  var OriginalLengthInBits: UInt64;
  var PadLen: Int32;
begin
  OriginalLengthInBits := DataSize * 8;
  PadLen := 111 - (DataSize mod 128);
  if PadLen < 0 then PadLen := PadLen + 128;
  PaddedData := nil;
  SetLength(PaddedData, DataSize + PadLen + 1 + 16);
  if DataSize > 0 then Move(Data^, PaddedData[0], DataSize);
  PaddedData[DataSize] := $80;
  for i := DataSize + 1 to High(PaddedData) - 16 do PaddedData[i] := $00;
  for i := 0 to 7 do PaddedData[High(PaddedData) - 15 + i] := 0;
  for i := 0 to 7 do PaddedData[High(PaddedData) - 7 + i] := PUInt8Arr(@OriginalLengthInBits)^[7 - i];
  for i := 0 to (Length(PaddedData) div 128) - 1 do
  begin
    for t := 0 to 15 do
    begin
      W[t] := 0;
      for j := 0 to 7 do
      begin
        W[t] := (W[t] shl 8) or PaddedData[i * 128 + t * 8 + j];
      end;
    end;
    for t := 16 to 79 do
    begin
      W[t] := Sigma1Small(W[t - 2]) + W[t - 7] + Sigma0Small(W[t - 15]) + W[t - 16];
    end;
    wv_a := H[0]; wv_b := H[1]; wv_c := H[2]; wv_d := H[3];
    wv_e := H[4]; wv_f := H[5]; wv_g := H[6]; wv_h := H[7];
    for t := 0 to 79 do
    begin
      T1 := wv_h + Sigma1(wv_e) + Ch(wv_e, wv_f, wv_g) + K[t] + W[t];
      T2 := Sigma0(wv_a) + Maj(wv_a, wv_b, wv_c);
      wv_h := wv_g; wv_g := wv_f; wv_f := wv_e;
      wv_e := wv_d + T1;
      wv_d := wv_c; wv_c := wv_b; wv_b := wv_a;
      wv_a := T1 + T2;
    end;
    H[0] := H[0] + wv_a; H[1] := H[1] + wv_b; H[2] := H[2] + wv_c;
    H[3] := H[3] + wv_d; H[4] := H[4] + wv_e; H[5] := H[5] + wv_f;
    H[6] := H[6] + wv_g; H[7] := H[7] + wv_h;
  end;
  for i := 0 to 7 do
  for j := 0 to 7 do
  begin
    Result[i * 8 + j] := Byte(H[i] shr (56 - j * 8));
  end;
end;

function USHA2_512(const Data: TUInt8Array): TUDigestSHA2_512;
begin
  Result := USHA2_512(@Data[0], Length(Data));
end;

function USHA2_512(const Data: String): TUDigestSHA2_512;
begin
  Result := USHA2_512(@Data[1], Length(Data));
end;

function SHA3(const Data: Pointer; const DataSize: UInt32; const DigestSize: UInt32): TUInt8Array;
  type TKeccakState = array[0..4, 0..4] of UInt64;
  procedure KeccakF1600(var State: TKeccakState);
    function ROTL64(const x: UInt64; const n: UInt8): UInt64;
    begin
      Result := (x shl n) or (x shr (64 - n));
    end;
    const RoundConstants: array[0..23] of UInt64 = (
      $0000000000000001, $0000000000008082, $800000000000808A, $8000000080008000,
      $000000000000808B, $0000000080000001, $8000000080008081, $8000000000008009,
      $000000000000008A, $0000000000000088, $0000000080008009, $000000008000000A,
      $000000008000808B, $800000000000008B, $8000000000008089, $8000000000008003,
      $8000000000008002, $8000000000000080, $000000000000800A, $800000008000000A,
      $8000000080008081, $8000000000008080, $0000000080000001, $8000000080008008
    );
    RotationOffsets: array[0..4, 0..4] of UInt8 = (
      (0, 36, 3, 41, 18),
      (1, 44, 10, 45, 2),
      (62, 6, 43, 15, 61),
      (28, 55, 25, 21, 56),
      (27, 20, 39, 8, 14)
    );
    var r: Int32;
    var x, y: Int32;
    var C, D: array[0..4] of UInt64;
    var B: TKeccakState;
  begin
    for r := 0 to 23 do
    begin
      for x := 0 to 4 do
      begin
        C[x] := State[x, 0] xor State[x, 1] xor State[x, 2] xor State[x, 3] xor State[x, 4];
      end;
      for x := 0 to 4 do
      begin
        D[x] := C[(x + 4) mod 5] xor ROTL64(C[(x + 1) mod 5], 1);
      end;
      for x := 0 to 4 do
      for y := 0 to 4 do
      begin
        State[x, y] := State[x, y] xor D[x];
      end;
      for x := 0 to 4 do
      for y := 0 to 4 do
      begin
        B[y, (2 * x + 3 * y) mod 5] := ROTL64(State[x, y], RotationOffsets[x, y]);
      end;
      for x := 0 to 4 do
      for y := 0 to 4 do
      begin
        State[x, y] := B[x, y] xor ((not B[(x + 1) mod 5, y]) and B[(x + 2) mod 5, y]);
      end;
      State[0, 0] := State[0, 0] xor RoundConstants[r];
    end;
  end;
  var Rate: UInt32;
  var State: TKeccakState;
  var PaddedData: TUInt8Array;
  var Digest: array[0..200 - 1] of UInt8;
  var PadLen, x, y, i, j, n, BlockPos: Int32;
  var w: UInt64;
begin
  Result := nil;
  SetLength(Result, DigestSize);
  Rate := 200 - (2 * DigestSize);
  UClear(State, SizeOf(State));
  PadLen := Rate - (DataSize mod Rate);
  if PadLen = 0 then PadLen := Rate;
  PaddedData := nil;
  SetLength(PaddedData, DataSize + PadLen);
  if DataSize > 0 then
  begin
    Move(Data^, PaddedData[0], DataSize);
  end;
  PaddedData[DataSize] := $06;
  for i := DataSize + 1 to DataSize + PadLen - 2 do
  begin
    PaddedData[i] := $00;
  end;
  i := High(PaddedData);
  PaddedData[i] := PaddedData[i] or $80;
  for i := 0 to (Length(PaddedData) div Rate) - 1 do
  begin
    BlockPos := i * Rate;
    for y := 0 to 4 do
    for x := 0 to 4 do
    begin
      if (x + 5 * y) * 8 < Rate then
      begin
        w := 0;
        for j := 0 to 7 do
        begin
          w := w or (UInt64(PaddedData[BlockPos + (x + 5 * y) * 8 + j]) shl (j * 8));
        end;
        State[x, y] := State[x, y] xor w;
      end;
    end;
    KeccakF1600(State);
  end;
  n := 0;
  while n < DigestSize do
  begin
    for y := 0 to 4 do
    for x := 0 to 4 do
    begin
      if (x + 5 * y) * 8 < Rate then
      for j := 0 to 7 do
      begin
        Digest[n] := Byte((State[x, y] shr (j * 8)) and $ff);
        Inc(n);
      end;
    end;
  end;
  Move(Digest, Result[0], DigestSize);
end;

function USHA3_224(const Data: Pointer; const DataSize: UInt32): TUDigestSHA3_224;
  var Digest: TUInt8Array;
begin
  Digest := SHA3(Data, DataSize, SizeOf(Result));
  Move(Digest[0], Result, SizeOf(Result));
end;

function USHA3_224(const Data: TUInt8Array): TUDigestSHA3_224;
begin
  Result := USHA3_224(@Data[0], Length(Data));
end;

function USHA3_224(const Data: String): TUDigestSHA3_224;
begin
  Result := USHA3_224(@Data[1], Length(Data));
end;

function USHA3_256(const Data: Pointer; const DataSize: UInt32): TUDigestSHA3_256;
  var Digest: TUInt8Array;
begin
  Digest := SHA3(Data, DataSize, SizeOf(Result));
  Move(Digest[0], Result, SizeOf(Result));
end;

function USHA3_256(const Data: TUInt8Array): TUDigestSHA3_256;
begin
  Result := USHA3_256(@Data[0], Length(Data));
end;

function USHA3_256(const Data: String): TUDigestSHA3_256;
begin
  Result := USHA3_256(@Data[1], Length(Data));
end;

function USHA3_384(const Data: Pointer; const DataSize: UInt32): TUDigestSHA3_384;
  var Digest: TUInt8Array;
begin
  Digest := SHA3(Data, DataSize, SizeOf(Result));
  Move(Digest[0], Result, SizeOf(Result));
end;

function USHA3_384(const Data: TUInt8Array): TUDigestSHA3_384;
begin
  Result := USHA3_384(@Data[0], Length(Data));
end;

function USHA3_384(const Data: String): TUDigestSHA3_384;
begin
  Result := USHA3_384(@Data[1], Length(Data));
end;

function USHA3_512(const Data: Pointer; const DataSize: UInt32): TUDigestSHA3_512;
  var Digest: TUInt8Array;
begin
  Digest := SHA3(Data, DataSize, SizeOf(Result));
  Move(Digest[0], Result, SizeOf(Result));
end;

function USHA3_512(const Data: TUInt8Array): TUDigestSHA3_512;
begin
  Result := USHA3_512(@Data[0], Length(Data));
end;

function USHA3_512(const Data: String): TUDigestSHA3_512;
begin
  Result := USHA3_512(@Data[1], Length(Data));
end;

function UDigestMD5(const Data: TUInt8Array): TUInt8Array;
begin
  Result := UMD5(Data);
end;

function UDigestSHA1(const Data: TUInt8Array): TUInt8Array;
begin
  Result := USHA1(Data);
end;

function UDigestSHA2_256(const Data: TUInt8Array): TUInt8Array;
begin
  Result := USHA2_256(Data);
end;

function UDigestSHA2_512(const Data: TUInt8Array): TUInt8Array;
begin
  Result := USHA2_512(Data);
end;

function UDigestSHA3_224(const Data: TUInt8Array): TUInt8Array;
begin
  Result := USHA3_224(Data);
end;

function UDigestSHA3_256(const Data: TUInt8Array): TUInt8Array;
begin
  Result := USHA3_256(Data);
end;

function UDigestSHA3_384(const Data: TUInt8Array): TUInt8Array;
begin
  Result := USHA3_384(Data);
end;

function UDigestSHA3_512(const Data: TUInt8Array): TUInt8Array;
begin
  Result := USHA3_512(Data);
end;

generic function UHMAC<TDigest>(
  const Key, Data: TUInt8Array
): TDigest;
  const DigestSize = SizeOf(TDigest);
  const BlockSize = DigestSize * 2;
  var PaddedKey, o_key_pad, i_key_pad: array[0..BlockSize - 1] of UInt8;
  var i: Int32;
  var HashedKeyDigest, InnerHashDigest, Digest: TUInt8Array;
begin
  UClear(PaddedKey, SizeOf(PaddedKey));
  if Length(Key) > BlockSize then
  begin
    HashedKeyDigest := TDigest.Func(Key);
    Move(HashedKeyDigest[0], PaddedKey[0], DigestSize);
  end
  else if Length(Key) > 0 then
  begin
    Move(Key[0], PaddedKey[0], Length(Key));
  end;
  for i := 0 to BlockSize - 1 do
  begin
    o_key_pad[i] := PaddedKey[i] xor $5C;
    i_key_pad[i] := PaddedKey[i] xor $36;
  end;
  InnerHashDigest := TDigest.Func(UBytesJoin(i_key_pad, Data));
  Digest := TDigest.Func(UBytesJoin(o_key_pad, InnerHashDigest));
  Move(Digest[0], Result, DigestSize);
end;

function UHMAC_SHA1(const Key, Data: TUInt8Array): TUDigestSHA1;
begin
  Result := specialize UHMAC<TUDigestSHA1>(Key, Data);
end;

function UHMAC_SHA256(const Key, Data: TUInt8Array): TUDigestSHA2_256;
begin
  Result := specialize UHMAC<TUDigestSHA2_256>(Key, Data);
end;

function UHMAC_SHA512(const Key, Data: TUInt8Array): TUDigestSHA2_512;
begin
  Result := specialize UHMAC<TUDigestSHA2_512>(Key, Data);
end;

function UHMAC_SHA3_224(const Key, Data: TUInt8Array): TUDigestSHA3_224;
begin
  Result := specialize UHMAC<TUDigestSHA3_224>(Key, Data);
end;

function UHMAC_SHA3_256(const Key, Data: TUInt8Array): TUDigestSHA3_256;
begin
  Result := specialize UHMAC<TUDigestSHA3_256>(Key, Data);
end;

function UHMAC_SHA3_384(const Key, Data: TUInt8Array): TUDigestSHA3_384;
begin
  Result := specialize UHMAC<TUDigestSHA3_384>(Key, Data);
end;

function UHMAC_SHA3_512(const Key, Data: TUInt8Array): TUDigestSHA3_512;
begin
  Result := specialize UHMAC<TUDigestSHA3_512>(Key, Data);
end;

function UAuthHMAC_SHA1(const Key, Data: TUInt8Array): TUInt8Array;
begin
  Result := UHMAC_SHA1(Key, Data);
end;

function UAuthHMAC_SHA2_256(const Key, Data: TUInt8Array): TUInt8Array;
begin
  Result := UHMAC_SHA256(Key, Data);
end;

function UAuthHMAC_SHA2_512(const Key, Data: TUInt8Array): TUInt8Array;
begin
  Result := UHMAC_SHA512(Key, Data);
end;

function UAuthHMAC_SHA3_224(const Key, Data: TUInt8Array): TUInt8Array;
begin
  Result := UHMAC_SHA3_224(Key, Data);
end;

function UAuthHMAC_SHA3_256(const Key, Data: TUInt8Array): TUInt8Array;
begin
  Result := UHMAC_SHA3_256(Key, Data);
end;

function UAuthHMAC_SHA3_384(const Key, Data: TUInt8Array): TUInt8Array;
begin
  Result := UHMAC_SHA3_384(Key, Data);
end;

function UAuthHMAC_SHA3_512(const Key, Data: TUInt8Array): TUInt8Array;
begin
  Result := UHMAC_SHA3_512(Key, Data);
end;

function USign_SHA256(const Data: TUInt8Array; const Key: TURSA.TKey): TUInt8Array;
begin
  Result := TURSA.Sign_SHA256(Data, Key);
end;

function USign_SHA512(const Data: TUInt8Array; const Key: TURSA.TKey): TUInt8Array;
begin
  Result := TURSA.Sign_SHA512(Data, Key);
end;

function UVerify(const Data, Signature: TUInt8Array; const Key: TURSA.TKey): Boolean;
begin
  Result := TURSA.Verify(Data, Signature, Key);
end;

function UPBKDF2(
  const FuncAuth: TUFuncAuth;
  const DigestSize: UInt32;
  const Password, Salt: TUInt8Array;
  const KeyLength: Int32; const Iterations: Int32
): TUInt8Array;
  var l, i, j, k: UInt32;
  var T, U: TUInt8Array;
  var BlockIndexBytes: array[0..3] of UInt8;
  var HmacResult: TUInt8Array;
begin
  Result := nil;
  U := TUInt8Array.Make(DigestSize);
  T := TUInt8Array.Make(DigestSize);
  l := (KeyLength + DigestSize - 1) div DigestSize;
  for i := 1 to l do
  begin
    BlockIndexBytes[0] := Byte((i shr 24) and $ff);
    BlockIndexBytes[1] := Byte((i shr 16) and $ff);
    BlockIndexBytes[2] := Byte((i shr 8) and $ff);
    BlockIndexBytes[3] := Byte(i and $ff);
    HmacResult := FuncAuth(Password, UBytesJoin(Salt, BlockIndexBytes));
    Move(HmacResult[0], U[0], DigestSize);
    Move(U[0], T[0], DigestSize);
    for j := 2 to Iterations do
    begin
      HmacResult := FuncAuth(Password, U);
      Move(HmacResult[0], U[0], DigestSize);
      for k := 0 to DigestSize - 1 do T[k] := T[k] xor U[k];
    end;
    Result := UBytesJoin(Result, T);
  end;
  SetLength(Result, KeyLength);
end;

function UPBKDF2_HMAC_SHA1(
  const Password, Salt: TUInt8Array;
  const KeyLength: Int32;
  const Iterations: Int32
): TUInt8Array;
begin
  Result := UPBKDF2(
    @UAuthHMAC_SHA1, SizeOf(TUDigestSHA1),
    Password, Salt, KeyLength, Iterations
  );
end;

function UPBKDF2_HMAC_SHA256(
  const Password, Salt: TUInt8Array;
  const KeyLength: Int32;
  const Iterations: Int32 = 600000
): TUInt8Array;
begin
  Result := UPBKDF2(
    @UAuthHMAC_SHA2_256, SizeOf(TUDigestSHA2_256),
    Password, Salt, KeyLength, Iterations
  );
end;

function UPBKDF2_HMAC_SHA512(
  const Password, Salt: TUInt8Array;
  const KeyLength: Int32;
  const Iterations: Int32
): TUInt8Array;
begin
  Result := UPBKDF2(
    @UAuthHMAC_SHA2_512, SizeOf(TUDigestSHA2_512),
    Password, Salt, KeyLength, Iterations
  );
end;

generic function UEvpKDF<TDigest>(
  const Password, Salt: TUInt8Array;
  const KeyLength: Int32;
  const IVLength: Int32;
  const Iterations: Int32;
  out IV: TUInt8Array
): TUInt8Array;
  var TotalLength: Int32;
  var HashInput: TUInt8Array;
  var CurrentHash: TUInt8Array;
  var i: Int32;
begin
  Result := nil;
  IV := nil;
  if Iterations < 1 then Exit;
  TotalLength := KeyLength + IVLength;
  HashInput := nil;
  while Length(Result) < TotalLength do
  begin
    HashInput := UBytesConcat([HashInput, Password, Salt]);
    CurrentHash := TDigest.Func(HashInput);
    for i := 2 to Iterations do CurrentHash := TDigest.Func(CurrentHash);
    Result := UBytesJoin(Result, CurrentHash);
    HashInput := CurrentHash;
  end;
  if IVLength > 0 then
  begin
    SetLength(IV, IVLength);
    Move(Result[KeyLength], IV[0], IVLength);
  end;
  SetLength(Result, KeyLength);
end;

function UEvpKDF_MD5(
  const Password, Salt: TUInt8Array;
  const KeyLength: Int32;
  const IVLength: Int32;
  const Iterations: Int32;
  out IV: TUInt8Array
): TUInt8Array;
begin
  Result := specialize UEvpKDF<TUDigestMD5>(
    Password, Salt, KeyLength, IVLength, Iterations, IV
  );
end;

function UEvpKDF_SHA256(
  const Password, Salt: TUInt8Array;
  const KeyLength: Int32;
  const IVLength: Int32;
  const Iterations: Int32;
  out IV: TUInt8Array
): TUInt8Array;
begin
  Result := specialize UEvpKDF<TUDigestSHA2_256>(
    Password, Salt, KeyLength, IVLength, Iterations, IV
  );
end;

function UMGF1_SHA256(const Seed: TUInt8Array; const MaskLen: Int32): TUInt8Array;
  const HashLen = SizeOf(TUDigestSHA2_256);
  var Counter, i: UInt32;
  var C, T: TUInt8Array;
  var Digest: TUDigestSHA2_256;
begin
  Counter := 0;
  C := nil;
  SetLength(C, Length(Seed) + 4);
  Move(Seed[0], C[0], Length(Seed));
  T := nil;
  SetLength(T, (MaskLen div HashLen + 1) * HashLen);
  i := 0;
  while i < MaskLen do
  begin
    C[Length(Seed) + 0] := UInt8((Counter shr 24) and $ff);
    C[Length(Seed) + 1] := UInt8((Counter shr 16) and $ff);
    C[Length(Seed) + 2] := UInt8((Counter shr 8) and $ff);
    C[Length(Seed) + 3] := UInt8(Counter and $ff);
    Digest := USHA2_256(C);
    Move(Digest, T[i], HashLen);
    Inc(i, HashLen);
    Inc(Counter);
  end;
  Result := nil;
  SetLength(Result, MaskLen);
  Move(T[0], Result[0], MaskLen);
end;

function UMakeRSAKey(const BitCount: UInt32; const Threads: Int32): TURSA.TKey;
begin
  Result := TURSA.MakeKey(BitCount, Threads);
end;

function UExportRSAKey_PKCS1(const Key: TURSA.TKey): String;
begin
  Result := TURSA.ExportKeyPrivate_PKCS1(Key);
end;

function UExportRSAKey_PKCS8(const Key: TURSA.TKey): String;
begin
  Result := TURSA.ExportKeyPrivate_PKCS8(Key);
end;

function UExportRSAKey_PKCS8(const Key: TURSA.TKey; const Password: String; const Iterations: UInt32): String;
begin
  Result := TURSA.ExportKeyPrivateEncrypted_PKCS8_SHA256_AES256_CBC(Key, UStrToBytes(Password), Iterations);
end;

function UExportRSAKey_X509(const Key: TURSA.TKey): String;
begin
  Result := TURSA.ExportKeyPublic_X509(Key);
end;

function UImportRSAKey(const KeyASN1: String; const Password: String): TURSA.TKey;
begin
  Result := TURSA.ImportKey(KeyASN1, UStrToBytes(Password));
end;

function UEncrypt_RSA_PKCS1(
  const Data: Pointer;
  const DataSize: UInt32;
  const Key: TURSA.TKey
): TUCryptoInt;
begin
  Result := TURSA.Encrypt_PKCS1(Data, DataSize, Key);
end;

function UEncrypt_RSA_PKCS1(
  const Data: TUInt8Array;
  const Key: TURSA.TKey
): TUCryptoInt;
begin
  Result := TURSA.Encrypt_PKCS1(@Data[0], Length(Data), Key);
end;

function UEncrypt_RSA_PKCS1_Str(
  const Str: String;
  const Key: TURSA.TKey
): TUCryptoInt;
begin
  Result := UEncrypt_RSA_PKCS1(@Str[1], Length(Str), Key);
end;

function UDecrypt_RSA_PKCS1_Str(
  const Cipher: TUCryptoInt;
  const Key: TURSA.TKey
): String;
begin
  Result := TURSA.Decrypt_PKCS1_Str(Cipher, Key);
end;

function UDecrypt_RSA_PKCS1(
  const Cipher: TUCryptoInt;
  const Key: TURSA.TKey
): TUInt8Array;
begin
  Result := TURSA.Decrypt_PKCS1(Cipher, Key);
end;

function UEncrypt_RSA_OAEP(
  const Data: Pointer;
  const DataSize: UInt32;
  const Key: TURSA.TKey
): TUCryptoInt;
begin
  Result := TURSA.Encrypt_OAEP(Data, DataSize, Key);
end;

function UEncrypt_RSA_OAEP(
  const Data: TUInt8Array;
  const Key: TURSA.TKey
): TUCryptoInt;
begin
  Result := TURSA.Encrypt_OAEP(@Data[0], Length(Data), Key);
end;

function UEncrypt_RSA_OAEP_Str(
  const Str: String;
  const Key: TURSA.TKey
): TUCryptoInt;
begin
  Result := TURSA.Encrypt_OAEP(@Str[1], Length(Str), Key);
end;

function UDecrypt_RSA_OAEP_Str(
  const Cipher: TUCryptoInt;
  const Key: TURSA.TKey
): String;
begin
  Result := TURSA.Decrypt_OAEP_Str(Cipher, Key);
end;

function UDecrypt_RSA_OAEP(
  const Cipher: TUCryptoInt;
  const Key: TURSA.TKey
): TUInt8Array;
begin
  Result := TURSA.Decrypt_OAEP(Cipher, Key);
end;

class function TUAES.MakeIV(const Bytes: TUInt8Array): TInitVector;
  var i, n: Int32;
begin
  n := UMin(Length(Result), Length(Bytes));
  for i := 0 to n - 1 do Result[i] := Bytes[i];
  for i := n to High(Result) do Result[i] := 0;
end;

class function TUAES.MakeIV: TInitVector;
  var i: Int32;
begin
  for i := 0 to High(Result) do Result[i] := UThreadRandom(256);
end;

class function TUAES.MakeKey128(const Bytes: TUInt8Array): TKey128;
  var i, n: Int32;
begin
  n := UMin(Length(Result), Length(Bytes));
  for i := 0 to n - 1 do Result[i] := Bytes[i];
  for i := n to High(Result) do Result[i] := 0;
end;

class function TUAES.MakeKey192(const Bytes: TUInt8Array): TKey192;
  var i, n: Int32;
begin
  n := UMin(Length(Result), Length(Bytes));
  for i := 0 to n - 1 do Result[i] := Bytes[i];
  for i := n to High(Result) do Result[i] := 0;
end;

class function TUAES.MakeKey256(const Bytes: TUInt8Array): TKey256;
  var i, n: Int32;
begin
  n := UMin(Length(Result), Length(Bytes));
  for i := 0 to n - 1 do Result[i] := Bytes[i];
  for i := n to High(Result) do Result[i] := 0;
end;

class procedure TUAES.GF128_Mul(var X: TInitVector; const Y: TInitVector);
  const R_REDUCTION_BYTE = $E1;
  var V: TUAES.TInitVector;
  var Z: TUAES.TInitVector;
  var i, j: Int32;
  var lsb_set: Boolean;
begin
  Z := InitVectorZero;
  V := Y;
  for i := 0 to 127 do
  begin
    if (X[i div 8] and (1 shl (7 - (i mod 8)))) <> 0 then
    begin
      for j := 0 to 15 do Z[j] := Z[j] xor V[j];
    end;
    lsb_set := (V[15] and 1) <> 0;
    for j := 15 downto 1 do
    begin
      V[j] := (V[j] shr 1) or (V[j - 1] shl 7);
    end;
    V[0] := V[0] shr 1;
    if lsb_set then
    begin
      V[0] := V[0] xor R_REDUCTION_BYTE;
    end;
  end;
  X := Z;
end;

class function TUAES.KeyExpansion(const Key: TKey128): TExpandedKey;
  type TWord = array[0..3] of UInt8;
  var w: array[0..43] of TWord;
  var Temp: TWord;
  var i, j, k: Int32;
  var t_byte: UInt8;
begin
  for i := 0 to 3 do
  for j := 0 to 3 do
  begin
    w[i][j] := Key[i * 4 + j];
  end;
  i := 4;
  while i < 44 do
  begin
    Temp := w[i - 1];
    if (i mod 4) = 0 then
    begin
      t_byte := Temp[0];
      Temp[0] := Temp[1]; Temp[1] := Temp[2]; Temp[2] := Temp[3]; Temp[3] := t_byte;
      for j := 0 to 3 do Temp[j] := SBox[Temp[j]];
      Temp[0] := Temp[0] xor Rcon[i div 4];
    end;
    for j := 0 to 3 do
    begin
      w[i][j] := w[i - 4][j] xor Temp[j];
    end;
    Inc(i);
  end;
  for i := 0 to 10 do
  for j := 0 to 3 do
  for k := 0 to 3 do
  begin
    Result[i][k, j] := w[i * 4 + j][k];
  end;
end;

class function TUAES.KeyExpansion(const Key: TKey192): TExpandedKey;
  type TWord = array[0..3] of UInt8;
  var w: array[0..51] of TWord;
  var Temp: TWord;
  var i, j, k: Int32;
  var t_byte: UInt8;
begin
  for i := 0 to 5 do
  for j := 0 to 3 do
  begin
    w[i][j] := Key[i * 4 + j];
  end;
  i := 6;
  while i < 52 do
  begin
    Temp := w[i - 1];
    if (i mod 6) = 0 then
    begin
      t_byte := Temp[0];
      Temp[0] := Temp[1]; Temp[1] := Temp[2]; Temp[2] := Temp[3]; Temp[3] := t_byte;
      for j := 0 to 3 do Temp[j] := SBox[Temp[j]];
      Temp[0] := Temp[0] xor Rcon[i div 6];
    end;
    for j := 0 to 3 do
    begin
      w[i][j] := w[i - 6][j] xor Temp[j];
    end;
    Inc(i);
  end;
  for i := 0 to 12 do
  for j := 0 to 3 do
  for k := 0 to 3 do
  begin
    Result[i][k, j] := w[i * 4 + j][k];
  end;
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
  var PadLen, i: Int32;
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
  const ExpandedKey: TExpandedKey;
  const NumRounds: Int32
);
  var Round: Int32;
begin
  AddRoundKey(State, ExpandedKey[0]);
  for Round := 1 to NumRounds - 1 do
  begin
    SubBytes(State);
    ShiftRows(State);
    MixColumns(State);
    AddRoundKey(State, ExpandedKey[Round]);
  end;
  SubBytes(State);
  ShiftRows(State);
  AddRoundKey(State, ExpandedKey[NumRounds]);
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
  const ExpandedKey: TExpandedKey;
  const NumRounds: Int32
);
  var Round: Int32;
begin
  AddRoundKey(State, ExpandedKey[NumRounds]);
  for Round := NumRounds - 1 downto 1 do
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

class function TUAES.Encrypt_PKCS7_ECB(
  const Data: TUInt8Array;
  const ExpandedKey: TExpandedKey;
  const NumRounds: Int32
): TUInt8Array;
  var PaddedData: TUInt8Array;
  var State: TBlock;
  var i, r, c: Int32;
begin
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
    CipherBlock(State, ExpandedKey, NumRounds);
    for r := 0 to 3 do
    for c := 0 to 3 do
    begin
      Result[i * 16 + r + 4 * c] := State[r, c];
    end;
  end;
end;

class function TUAES.Encrypt_PKCS7_ECB_128(
  const Data: TUInt8Array;
  const Key: TKey128
): TUInt8Array;
  var ExpandedKey: TExpandedKey;
begin
  ExpandedKey := KeyExpansion(Key);
  Result := Encrypt_PKCS7_ECB(Data, ExpandedKey, 10);
end;

class function TUAES.Encrypt_PKCS7_ECB_192(
  const Data: TUInt8Array;
  const Key: TKey192
): TUInt8Array;
  var ExpandedKey: TExpandedKey;
begin
  ExpandedKey := KeyExpansion(Key);
  Result := Encrypt_PKCS7_ECB(Data, ExpandedKey, 12);
end;

class function TUAES.Encrypt_PKCS7_ECB_256(
  const Data: TUInt8Array;
  const Key: TKey256
): TUInt8Array;
  var ExpandedKey: TExpandedKey;
begin
  ExpandedKey := KeyExpansion(Key);
  Result := Encrypt_PKCS7_ECB(Data, ExpandedKey, 14);
end;

class function TUAES.Decrypt_PKCS7_ECB(
  const Cipher: TUInt8Array;
  const ExpandedKey: TExpandedKey;
  const NumRounds: Int32
): TUInt8Array;
  var State: TBlock;
  var DataPadded: TUInt8Array;
  var i, r, c: Int32;
begin
  DataPadded := nil;
  SetLength(DataPadded, Length(Cipher));
  for i := 0 to (Length(Cipher) div 16) - 1 do
  begin
    for r := 0 to 3 do
    for c := 0 to 3 do
    begin
      State[r, c] := Cipher[i * 16 + r + 4 * c];
    end;
    InvCipherBlock(State, ExpandedKey, NumRounds);
    for r := 0 to 3 do
    for c := 0 to 3 do
    begin
      DataPadded[i * 16 + r + 4 * c] := State[r, c];
    end;
  end;
  Result := UnpadData_PKCS7(DataPadded);
end;

class function TUAES.Decrypt_PKCS7_ECB_128(
  const Cipher: TUInt8Array;
  const Key: TKey128
): TUInt8Array;
  var ExpandedKey: TExpandedKey;
begin
  ExpandedKey := KeyExpansion(Key);
  Result := Decrypt_PKCS7_ECB(Cipher, ExpandedKey, 10);
end;

class function TUAES.Decrypt_PKCS7_ECB_192(
  const Cipher: TUInt8Array;
  const Key: TKey192
): TUInt8Array;
  var ExpandedKey: TExpandedKey;
begin
  ExpandedKey := KeyExpansion(Key);
  Result := Decrypt_PKCS7_ECB(Cipher, ExpandedKey, 12);
end;

class function TUAES.Decrypt_PKCS7_ECB_256(
  const Cipher: TUInt8Array;
  const Key: TKey256
): TUInt8Array;
  var ExpandedKey: TExpandedKey;
begin
  ExpandedKey := KeyExpansion(Key);
  Result := Decrypt_PKCS7_ECB(Cipher, ExpandedKey, 14);
end;

class function TUAES.Encrypt_PKCS7_CBC(
  const Data: TUInt8Array;
  const ExpandedKey: TExpandedKey;
  const IV: TInitVector;
  const NumRounds: Int32
): TUInt8Array;
  var PaddedData: TUInt8Array;
  var PrevCipherBlock: TInitVector;
  var State: TBlock;
  var i, j, r: Int32;
begin
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
    CipherBlock(State, ExpandedKey, NumRounds);
    for j := 0 to 15 do
    begin
      r := i * 16 + j;
      Result[r] := State[j mod 4, j div 4];
      PrevCipherBlock[j] := Result[r];
    end;
  end;
end;

class function TUAES.Encrypt_PKCS7_CBC_128(
  const Data: TUInt8Array;
  const Key: TKey128;
  const IV: TInitVector
): TUInt8Array;
  var ExpandedKey: TExpandedKey;
begin
  ExpandedKey := KeyExpansion(Key);
  Result := Encrypt_PKCS7_CBC(Data, ExpandedKey, IV, 10);
end;

class function TUAES.Encrypt_PKCS7_CBC_192(
  const Data: TUInt8Array;
  const Key: TKey192;
  const IV: TInitVector
): TUInt8Array;
  var ExpandedKey: TExpandedKey;
begin
  ExpandedKey := KeyExpansion(Key);
  Result := Encrypt_PKCS7_CBC(Data, ExpandedKey, IV, 12);
end;

class function TUAES.Encrypt_PKCS7_CBC_256(
  const Data: TUInt8Array;
  const Key: TKey256;
  const IV: TInitVector
): TUInt8Array;
  var ExpandedKey: TExpandedKey;
begin
  ExpandedKey := KeyExpansion(Key);
  Result := Encrypt_PKCS7_CBC(Data, ExpandedKey, IV, 14);
end;

class function TUAES.Decrypt_PKCS7_CBC(
  const Cipher: TUInt8Array;
  const ExpandedKey: TExpandedKey;
  const IV: TInitVector;
  const NumRounds: Int32
): TUInt8Array;
  var State: TBlock;
  var DataPadded: TUInt8Array;
  var PrevCipherBlock, CurCipherBlock: TInitVector;
  var i, j: Int32;
begin
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
    InvCipherBlock(State, ExpandedKey, NumRounds);
    for j := 0 to 15 do
    begin
      DataPadded[i * 16 + j] := State[j mod 4, j div 4] xor PrevCipherBlock[j];
    end;
    PrevCipherBlock := CurCipherBlock;
  end;
  Result := UnpadData_PKCS7(DataPadded);
end;

class function TUAES.Decrypt_PKCS7_CBC_128(
  const Cipher: TUInt8Array;
  const Key: TKey128;
  const IV: TInitVector
): TUInt8Array;
  var ExpandedKey: TExpandedKey;
begin
  ExpandedKey := KeyExpansion(Key);
  Result := Decrypt_PKCS7_CBC(Cipher, ExpandedKey, IV, 10);
end;

class function TUAES.Decrypt_PKCS7_CBC_192(
  const Cipher: TUInt8Array;
  const Key: TKey192;
  const IV: TInitVector
): TUInt8Array;
  var ExpandedKey: TExpandedKey;
begin
  ExpandedKey := KeyExpansion(Key);
  Result := Decrypt_PKCS7_CBC(Cipher, ExpandedKey, IV, 12);
end;

class function TUAES.Decrypt_PKCS7_CBC_256(
  const Cipher: TUInt8Array;
  const Key: TKey256;
  const IV: TInitVector
): TUInt8Array;
  var ExpandedKey: TExpandedKey;
begin
  ExpandedKey := KeyExpansion(Key);
  Result := Decrypt_PKCS7_CBC(Cipher, ExpandedKey, IV, 14);
end;

class function TUAES.Process_CTR(
  const Input: TUInt8Array;
  const ExpandedKey: TExpandedKey;
  const IV: TInitVector;
  const NumRounds: Int32
): TUInt8Array;
  var State: TBlock;
  var CounterBlock, KeystreamBlock: TInitVector;
  var i, j: Int32;
begin
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
      CipherBlock(State, ExpandedKey, NumRounds);
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

class function TUAES.Process_CTR_128(
  const Input: TUInt8Array;
  const Key: TKey128;
  const IV: TInitVector
): TUInt8Array;
  var ExpandedKey: TExpandedKey;
begin
  ExpandedKey := KeyExpansion(Key);
  Result := Process_CTR(Input, ExpandedKey, IV, 10);
end;

class function TUAES.Process_CTR_192(
  const Input: TUInt8Array;
  const Key: TKey192;
  const IV: TInitVector
): TUInt8Array;
  var ExpandedKey: TExpandedKey;
begin
  ExpandedKey := KeyExpansion(Key);
  Result := Process_CTR(Input, ExpandedKey, IV, 12);
end;

class function TUAES.Process_CTR_256(
  const Input: TUInt8Array;
  const Key: TKey256;
  const IV: TInitVector
): TUInt8Array;
  var ExpandedKey: TExpandedKey;
begin
  ExpandedKey := KeyExpansion(Key);
  Result := Process_CTR(Input, ExpandedKey, IV, 14);
end;

class function TUAES.Process_GCM(
  const Input: TUInt8Array;
  const ExpandedKey: TExpandedKey;
  const Nonce: TInitVector;
  const AAD: TUInt8Array;
  const IsEncrypting: Boolean;
  const NumRounds: Int32;
  out AuthTag: TTag
): TUInt8Array;
  procedure IncrementCounterBlock(var Block: TInitVector);
    var i: Int32;
  begin
    for i := 15 downto 12 do
    begin
      Inc(Block[i]);
      if Block[i] <> 0 then Break;
    end;
  end;
  var H, J0, S, Counter, Keystream, LastBlock: TInitVector;
  var State: TBlock;
  var i, j, BlockCount: Int32;
  var DataToHash: TUInt8Array;
  var LenBlock: TInitVector;
  var LenAAD: UInt64;
  var LenC: UInt64;
begin
  for i := 0 to 3 do
  for j := 0 to 3 do
  begin
    State[i, j] := 0;
  end;
  CipherBlock(State, ExpandedKey, NumRounds);
  for i := 0 to 15 do
  begin
    H[i] := State[i mod 4, i div 4];
  end;
  for i := 0 to 11 do J0[i] := Nonce[i];
  J0[12] := 0; J0[13] := 0; J0[14] := 0; J0[15] := 1;
  S := InitVectorZero;
  Counter := J0;
  IncrementCounterBlock(Counter);
  BlockCount := Length(AAD) div 16;
  for i := 0 to BlockCount - 1 do
  begin
    for j := 0 to 15 do
    begin
      S[j] := S[j] xor AAD[i * 16 + j];
    end;
    GF128_Mul(S, H);
  end;
  if (Length(AAD) mod 16) <> 0 then
  begin
    LastBlock := InitVectorZero;
    for j := 0 to (Length(AAD) mod 16) - 1 do
    begin
      LastBlock[j] := AAD[BlockCount * 16 + j];
    end;
    for j := 0 to 15 do
    begin
      S[j] := S[j] xor LastBlock[j];
    end;
    GF128_Mul(S, H);
  end;
  Result := nil;
  SetLength(Result, Length(Input));
  for i := 0 to Length(Input) - 1 do
  begin
    if (i mod 16) = 0 then
    begin
      for j := 0 to 15 do
      begin
        State[j mod 4, j div 4] := Counter[j];
      end;
      CipherBlock(State, ExpandedKey, NumRounds);
      for j := 0 to 15 do
      begin
        Keystream[j] := State[j mod 4, j div 4];
      end;
      IncrementCounterBlock(Counter);
    end;
    Result[i] := Input[i] xor Keystream[i mod 16];
  end;
  if IsEncrypting then DataToHash := Result else DataToHash := Input;
  BlockCount := Length(DataToHash) div 16;
  for i := 0 to BlockCount - 1 do
  begin
    for j := 0 to 15 do
    begin
      S[j] := S[j] xor DataToHash[i * 16 + j];
    end;
    GF128_Mul(S, H);
  end;
  if (Length(DataToHash) mod 16) <> 0 then
  begin
    LastBlock := InitVectorZero;
    for j := 0 to (Length(DataToHash) mod 16) - 1 do
    begin
      LastBlock[j] := DataToHash[BlockCount * 16 + j];
    end;
    for j := 0 to 15 do
    begin
      S[j] := S[j] xor LastBlock[j];
    end;
    GF128_Mul(S, H);
  end;
  LenAAD := UInt64(Length(AAD)) * 8;
  LenC := UInt64(Length(DataToHash)) * 8;
  for i := 0 to 7 do
  begin
    LenBlock[i] := UInt8(LenAAD shr (56 - i * 8));
  end;
  for i := 0 to 7 do
  begin
    LenBlock[8 + i] := UInt8(LenC shr (56 - i * 8));
  end;
  for j := 0 to 15 do
  begin
    S[j] := S[j] xor LenBlock[j];
  end;
  GF128_Mul(S, H);
  for j := 0 to 15 do
  begin
    State[j mod 4, j div 4] := J0[j];
  end;
  CipherBlock(State, ExpandedKey, NumRounds);
  for j := 0 to 15 do
  begin
    AuthTag[j] := State[j mod 4, j div 4] xor S[j];
  end;
end;

class function TUAES.Process_GCM_128(
  const Input: TUInt8Array;
  const Key: TKey128;
  const Nonce: TInitVector;
  const AAD: TUInt8Array;
  const IsEncrypting: Boolean;
  out AuthTag: TTag
): TUInt8Array;
  var ExpandedKey: TExpandedKey;
begin
  ExpandedKey := KeyExpansion(Key);
  Result := Process_GCM(
    Input, ExpandedKey, Nonce, AAD, IsEncrypting, 10, AuthTag
  );
end;

class function TUAES.Process_GCM_192(
  const Input: TUInt8Array;
  const Key: TKey192;
  const Nonce: TInitVector;
  const AAD: TUInt8Array;
  const IsEncrypting: Boolean;
  out AuthTag: TTag
): TUInt8Array;
  var ExpandedKey: TExpandedKey;
begin
  ExpandedKey := KeyExpansion(Key);
  Result := Process_GCM(
    Input, ExpandedKey, Nonce, AAD, IsEncrypting, 12, AuthTag
  );
end;

class function TUAES.Process_GCM_256(
  const Input: TUInt8Array;
  const Key: TKey256;
  const Nonce: TInitVector;
  const AAD: TUInt8Array;
  const IsEncrypting: Boolean;
  out AuthTag: TTag
): TUInt8Array;
  var ExpandedKey: TExpandedKey;
begin
  ExpandedKey := KeyExpansion(Key);
  Result := Process_GCM(
    Input, ExpandedKey, Nonce, AAD, IsEncrypting, 14, AuthTag
  );
end;

class function TUDES.MakeIV(const Bytes: TUInt8Array): TInitVector;
  var i, n: Int32;
begin
  n := UMin(Length(Result), Length(Bytes));
  for i := 0 to n - 1 do Result[i] := Bytes[i];
  for i := n to High(Result) do Result[i] := 0;
end;

class function TUDES.MakeIV: TInitVector;
  var i: Int32;
begin
  for i := 0 to High(Result) do Result[i] := UThreadRandom(256);
end;

class function TUDES.MakeKey(const Bytes: TUInt8Array): TKey;
  var i, n: Int32;
begin
  n := UMin(Length(Result), Length(Bytes));
  for i := 0 to n - 1 do Result[i] := Bytes[i];
  for i := n to High(Result) do Result[i] := 0;
end;

class function TUDES.MakeKey3(const Bytes: TUInt8Array): TKey3;
  var i, n: Int32;
begin
  n := UMin(Length(Result), Length(Bytes));
  for i := 0 to n - 1 do Result[i] := Bytes[i];
  for i := n to High(Result) do Result[i] := 0;
end;

class function TUDES.Permute(const Input: T64BitBlock;
  const Table: array of Byte; const InSize, OutSize: Int32): T64BitBlock;
  var i: Int32;
begin
  Result := 0;
  for i := 0 to OutSize - 1 do
  begin
    if (Input shr (InSize - Table[i])) and 1 = 1 then
    begin
      Result := Result or (UInt64(1) shl (OutSize - 1 - i));
    end;
  end;
end;

class function TUDES.GenerateSubKeys(const Key: TKey): TSubKeys;
  var i, j: Int32;
  var Key64: T64BitBlock;
  var Key56: T64BitBlock;
  var CombinedKey: T64BitBlock;
  var SubKey48: T64BitBlock;
  var C, D: UInt32;
begin
  Key64 := 0;
  for i := 0 to 7 do
  begin
    Key64 := (Key64 shl 8) or Key[i];
  end;
  Key56 := Permute(Key64, PC1, 64, 56);
  C := (Key56 shr 28) and $fffffff;
  D := Key56 and $fffffff;
  for i := 0 to 15 do
  begin
    for j := 1 to KeyShifts[i] do
    begin
      C := ((C shl 1) and $fffffff) or (C shr 27);
      D := ((D shl 1) and $fffffff) or (D shr 27);
    end;
    CombinedKey := (UInt64(C) shl 28) or D;
    SubKey48 := Permute(CombinedKey, PC2, 56, 48);
    for j := 0 to 5 do
    begin
      Result[i][j] := (SubKey48 shr (40 - j * 8)) and $ff;
    end;
  end;
end;

class function TUDES.FeistelF(R: UInt32; const SubKey: T48BitKey): UInt32;
  var i, row, col: Int32;
  var ExpandedR, XorResult: UInt64;
  var SBoxInput: UInt8;
  var SBoxOutput: UInt32;
  var SubKey64: UInt64;
begin
  ExpandedR := Permute(R, E, 32, 48);
  SubKey64 := 0;
  for i := 0 to 5 do
  begin
    SubKey64 := (SubKey64 shl 8) or SubKey[i];
  end;
  XorResult := ExpandedR xor SubKey64;
  SBoxOutput := 0;
  for i := 0 to 7 do
  begin
    SBoxInput := (XorResult shr (42 - i * 6)) and $3f;
    row := ((SBoxInput and $20) shr 4) or (SBoxInput and $01);
    col := (SBoxInput and $1e) shr 1;
    SBoxOutput := (SBoxOutput shl 4) or SBoxes[i, row * 16 + col];
  end;
  Result := UInt32(Permute(SBoxOutput, P, 32, 32));
end;

class procedure TUDES.ProcessDESBlock(
  var Block: T64BitBlock;
  const SubKeys: TSubKeys;
  const IsEncrypting: Boolean
);
  var i: Int32;
  var L, R, Temp: UInt32;
begin
  Block := Permute(Block, IP, 64, 64);
  L := (Block shr 32) and $ffffffff;
  R := Block and $ffffffff;
  for i := 0 to 15 do
  begin
    Temp := R;
    if IsEncrypting then
    begin
      R := L xor FeistelF(R, SubKeys[i])
    end
    else
    begin
      R := L xor FeistelF(R, SubKeys[15 - i]);
    end;
    L := Temp;
  end;
  Block := (UInt64(R) shl 32) or L;
  Block := Permute(Block, FP, 64, 64);
end;

class function TUDES.PadData_PKCS7(
  const Data: TUInt8Array;
  const BlockSize: Int32
): TUInt8Array;
  var i, PadLen: Int32;
begin
  Result := nil;
  PadLen := BlockSize - (Length(Data) mod BlockSize);
  if (PadLen = 0) and (Length(Data) > 0) then PadLen := BlockSize;
  if Length(Data) = 0 then PadLen := BlockSize;
  SetLength(Result, Length(Data) + PadLen);
  if Length(Data) > 0 then Move(Data[0], Result[0], Length(Data));
  for i := Length(Data) to High(Result) do Result[i] := Byte(PadLen);
end;

class function TUDES.UnpadData_PKCS7(const Data: TUInt8Array): TUInt8Array;
  var PadLen, i: Int32;
begin
  Result := nil;
  if Length(Data) = 0 then Exit;
  PadLen := Data[High(Data)];
  if (PadLen = 0) or (PadLen > 8) or (PadLen > Length(Data)) then Exit;
  for i := Length(Data) - PadLen to High(Data) do
  begin
    if Data[i] <> PadLen then Exit;
  end;
  SetLength(Result, Length(Data) - PadLen);
  if Length(Result) = 0 then Exit;
  Move(Data[0], Result[0], Length(Result));
end;

class function TUDES.Encrypt_PKCS7_ECB(
  const Data: TUInt8Array;
  const Key: TKey
): TUInt8Array;
  var SubKeys: TSubKeys;
  var PaddedData: TUInt8Array;
  var Block: T64BitBlock;
  var i, j: Int32;
begin
  Result := nil;
  SubKeys := GenerateSubKeys(Key);
  PaddedData := PadData_PKCS7(Data, 8);
  SetLength(Result, Length(PaddedData));
  for i := 0 to (Length(PaddedData) div 8) - 1 do
  begin
    Block := 0;
    for j := 0 to 7 do
    begin
      Block := (Block shl 8) or PaddedData[i * 8 + j];
    end;
    ProcessDESBlock(Block, SubKeys, True);
    for j := 0 to 7 do
    begin
      Result[i * 8 + j] := (Block shr (56 - j * 8)) and $ff;
    end;
  end;
end;

class function TUDES.Decrypt_PKCS7_ECB(
  const Cipher: TUInt8Array;
  const Key: TKey
): TUInt8Array;
  var SubKeys: TSubKeys;
  var DataPadded: TUInt8Array;
  var Block: T64BitBlock;
  var i, j: Int32;
begin
  Result := nil;
  if (Length(Cipher) mod 8) <> 0 then Exit;
  SubKeys := GenerateSubKeys(Key);
  DataPadded := nil;
  SetLength(DataPadded, Length(Cipher));
  for i := 0 to (Length(Cipher) div 8) - 1 do
  begin
    Block := 0;
    for j := 0 to 7 do
    begin
      Block := (Block shl 8) or Cipher[i * 8 + j];
    end;
    ProcessDESBlock(Block, SubKeys, False);
    for j := 0 to 7 do
    begin
      DataPadded[i * 8 + j] := (Block shr (56 - j * 8)) and $ff;
    end;
  end;
  Result := UnpadData_PKCS7(DataPadded);
end;

class function TUDES.Encrypt_PKCS7_CBC(
  const Data: TUInt8Array;
  const Key: TKey;
  const IV: TInitVector
): TUInt8Array;
  var SubKeys: TSubKeys;
  var PaddedData: TUInt8Array;
  var Block, PrevCipherBlock: T64BitBlock;
  var i, j: Int32;
begin
  Result := nil;
  SubKeys := GenerateSubKeys(Key);
  PaddedData := PadData_PKCS7(Data, 8);
  SetLength(Result, Length(PaddedData));
  PrevCipherBlock := 0;
  for i := 0 to 7 do PrevCipherBlock := (PrevCipherBlock shl 8) or IV[i];
  for i := 0 to (Length(PaddedData) div 8) - 1 do
  begin
    Block := 0;
    for j := 0 to 7 do Block := (Block shl 8) or PaddedData[i * 8 + j];
    Block := Block xor PrevCipherBlock;
    ProcessDESBlock(Block, SubKeys, True);
    PrevCipherBlock := Block;
    for j := 0 to 7 do
    begin
      Result[i * 8 + j] := (Block shr (56 - j * 8)) and $ff;
    end;
  end;
end;

class function TUDES.Decrypt_PKCS7_CBC(
  const Cipher: TUInt8Array;
  const Key: TKey;
  const IV: TInitVector
): TUInt8Array;
  var SubKeys: TSubKeys;
  var DataPadded: TUInt8Array;
  var Block, PrevCipherBlock, PlaintextBlock: T64BitBlock;
  var i, j: Int32;
begin
  Result := nil;
  if (Length(Cipher) mod 8) <> 0 then Exit;
  SubKeys := GenerateSubKeys(Key);
  DataPadded := nil;
  SetLength(DataPadded, Length(Cipher));
  PrevCipherBlock := 0;
  for i := 0 to 7 do PrevCipherBlock := (PrevCipherBlock shl 8) or IV[i];
  for i := 0 to (Length(Cipher) div 8) - 1 do
  begin
    Block := 0;
    for j := 0 to 7 do Block := (Block shl 8) or Cipher[i * 8 + j];
    PlaintextBlock := Block;
    ProcessDESBlock(PlaintextBlock, SubKeys, False);
    PlaintextBlock := PlaintextBlock xor PrevCipherBlock;
    PrevCipherBlock := Block;
    for j := 0 to 7 do
    begin
      DataPadded[i * 8 + j] := (PlaintextBlock shr (56 - j * 8)) and $ff;
    end;
  end;
  Result := UnpadData_PKCS7(DataPadded);
end;

class function TUDES.Encrypt_PKCS7_Triple_ECB(
  const Data: TUInt8Array;
  const Key: TKey3
): TUInt8Array;
  var SubKeys1, SubKeys2, SubKeys3: TSubKeys;
  var Key1: TKey absolute Key[0];
  var Key2: TKey absolute Key[8];
  var Key3: TKey absolute Key[16];
  var PaddedData: TUInt8Array;
  var Block: T64BitBlock;
  var i, j: Int32;
begin
  SubKeys1 := GenerateSubKeys(Key1);
  SubKeys2 := GenerateSubKeys(Key2);
  SubKeys3 := GenerateSubKeys(Key3);
  PaddedData := PadData_PKCS7(Data, 8);
  Result := nil;
  SetLength(Result, Length(PaddedData));
  for i := 0 to (Length(PaddedData) div 8) - 1 do
  begin
    Block := 0;
    for j := 0 to 7 do Block := (Block shl 8) or PaddedData[i * 8 + j];
    ProcessDESBlock(Block, SubKeys1, True);
    ProcessDESBlock(Block, SubKeys2, False);
    ProcessDESBlock(Block, SubKeys3, True);
    for j := 0 to 7 do Result[i * 8 + j] := (Block shr (56 - j * 8)) and $ff;
  end;
end;

class function TUDES.Decrypt_PKCS7_Triple_ECB(
  const Cipher: TUInt8Array;
  const Key: TKey3
): TUInt8Array;
  var SubKeys1, SubKeys2, SubKeys3: TSubKeys;
  var Key1: TKey absolute Key[0];
  var Key2: TKey absolute Key[8];
  var Key3: TKey absolute Key[16];
  var PaddedData: TUInt8Array;
  var Block: T64BitBlock;
  var i, j: Int32;
begin
  Result := nil;
  if (Length(Cipher) mod 8) <> 0 then Exit;
  SubKeys1 := GenerateSubKeys(Key1);
  SubKeys2 := GenerateSubKeys(Key2);
  SubKeys3 := GenerateSubKeys(Key3);
  PaddedData := nil;
  SetLength(PaddedData, Length(Cipher));
  for i := 0 to (Length(Cipher) div 8) - 1 do
  begin
    Block := 0;
    for j := 0 to 7 do Block := (Block shl 8) or Cipher[i * 8 + j];
    ProcessDESBlock(Block, SubKeys3, False);
    ProcessDESBlock(Block, SubKeys2, True);
    ProcessDESBlock(Block, SubKeys1, False);
    for j := 0 to 7 do PaddedData[i * 8 + j] := (Block shr (56 - j * 8)) and $ff;
  end;
  Result := UnpadData_PKCS7(PaddedData);
end;

class function TUDES.Encrypt_PKCS7_Triple_CBC(
  const Data: TUInt8Array;
  const Key: TKey3;
  const IV: TInitVector
): TUInt8Array;
  var SubKeys1, SubKeys2, SubKeys3: TSubKeys;
  var Key1: TKey absolute Key[0];
  var Key2: TKey absolute Key[8];
  var Key3: TKey absolute Key[16];
  var PaddedData: TUInt8Array;
  var Block, PrevCipherBlock: T64BitBlock;
  var i, j: Int32;
begin
  SubKeys1 := GenerateSubKeys(Key1);
  SubKeys2 := GenerateSubKeys(Key2);
  SubKeys3 := GenerateSubKeys(Key3);
  PaddedData := PadData_PKCS7(Data, 8);
  Result := nil;
  SetLength(Result, Length(PaddedData));
  PrevCipherBlock := 0;
  for i := 0 to 7 do PrevCipherBlock := (PrevCipherBlock shl 8) or IV[i];
  for i := 0 to (Length(PaddedData) div 8) - 1 do
  begin
    Block := 0;
    for j := 0 to 7 do Block := (Block shl 8) or PaddedData[i * 8 + j];
    Block := Block xor PrevCipherBlock;
    ProcessDESBlock(Block, SubKeys1, True);
    ProcessDESBlock(Block, SubKeys2, False);
    ProcessDESBlock(Block, SubKeys3, True);
    PrevCipherBlock := Block;
    for j := 0 to 7 do Result[i * 8 + j] := (Block shr (56 - j * 8)) and $ff;
  end;
end;

class function TUDES.Decrypt_PKCS7_Triple_CBC(
  const Cipher: TUInt8Array;
  const Key: TKey3;
  const IV: TInitVector
): TUInt8Array;
  var SubKeys1, SubKeys2, SubKeys3: TSubKeys;
  var Key1: TKey absolute Key[0];
  var Key2: TKey absolute Key[8];
  var Key3: TKey absolute Key[16];
  var DecryptedPadded: TUInt8Array;
  var Block, PrevCipherBlock, PlaintextBlock: T64BitBlock;
  var i, j: Int32;
begin
  Result := nil;
  if (Length(Cipher) mod 8) <> 0 then Exit;
  SubKeys1 := GenerateSubKeys(Key1);
  SubKeys2 := GenerateSubKeys(Key2);
  SubKeys3 := GenerateSubKeys(Key3);
  DecryptedPadded := nil;
  SetLength(DecryptedPadded, Length(Cipher));
  PrevCipherBlock := 0;
  for i := 0 to 7 do PrevCipherBlock := (PrevCipherBlock shl 8) or IV[i];
  for i := 0 to (Length(Cipher) div 8) - 1 do
  begin
    Block := 0;
    for j := 0 to 7 do Block := (Block shl 8) or Cipher[i * 8 + j];
    PlaintextBlock := Block;
    ProcessDESBlock(PlaintextBlock, SubKeys3, False);
    ProcessDESBlock(PlaintextBlock, SubKeys2, True);
    ProcessDESBlock(PlaintextBlock, SubKeys1, False);
    PlaintextBlock := PlaintextBlock xor PrevCipherBlock;
    PrevCipherBlock := Block;
    for j := 0 to 7 do DecryptedPadded[i * 8 + j] := (PlaintextBlock shr (56 - j * 8)) and $ff;
  end;
  Result := UnpadData_PKCS7(DecryptedPadded);
end;

class function TUDES.Process_CTR(
  const Input: TUInt8Array;
  const Key: TKey;
  const Nonce: TInitVector
): TUInt8Array;
  var SubKeys: TSubKeys;
  var CounterBlock, KeystreamBlock: T64BitBlock;
  var i: Int32;
begin
  Result := nil;
  SubKeys := GenerateSubKeys(Key);
  SetLength(Result, Length(Input));
  CounterBlock := 0;
  for i := 0 to 7 do
  begin
    CounterBlock := (CounterBlock shl 8) or Nonce[i];
  end;
  for i := 0 to Length(Input) - 1 do
  begin
    if (i mod 8) = 0 then
    begin
      KeystreamBlock := CounterBlock;
      ProcessDESBlock(KeystreamBlock, SubKeys, True);
      Inc(CounterBlock);
    end;
    Result[i] := Input[i] xor Byte((KeystreamBlock shr (56 - (i mod 8) * 8)) and $ff);
  end;
end;

class function TUDES.Process_Triple_CTR(
  const Input: TUInt8Array;
  const Key: TKey3;
  const Nonce: TInitVector
): TUInt8Array;
  var SubKeys1, SubKeys2, SubKeys3: TSubKeys;
  var Key1: TKey absolute Key[0];
  var Key2: TKey absolute Key[8];
  var Key3: TKey absolute Key[16];
  var CounterBlock, KeystreamBlock: T64BitBlock;
  var i, j: Int32;
begin
  SubKeys1 := GenerateSubKeys(Key1);
  SubKeys2 := GenerateSubKeys(Key2);
  SubKeys3 := GenerateSubKeys(Key3);
  Result := nil;
  SetLength(Result, Length(Input));
  CounterBlock := 0;
  for i := 0 to 7 do CounterBlock := (CounterBlock shl 8) or Nonce[i];
  for i := 0 to Length(Input) - 1 do
  begin
    if (i mod 8) = 0 then
    begin
      KeystreamBlock := CounterBlock;
      ProcessDESBlock(KeystreamBlock, SubKeys1, True);
      ProcessDESBlock(KeystreamBlock, SubKeys2, False);
      ProcessDESBlock(KeystreamBlock, SubKeys3, True);
      Inc(CounterBlock);
    end;
    Result[i] := Input[i] xor Byte((KeystreamBlock shr (56 - (i mod 8) * 8)) and $ff);
  end;
end;

function UEncrypt_AES_PKCS7_ECB_128(
  const Data: TUInt8Array;
  const Key: TUAES.TKey128
): TUInt8Array;
begin
  Result := TUAES.Encrypt_PKCS7_ECB_128(Data, Key);
end;

function UDecrypt_AES_PKCS7_ECB_128(
  const Cipher: TUInt8Array;
  const Key: TUAES.TKey128
): TUInt8Array;
begin
  Result := TUAES.Decrypt_PKCS7_ECB_128(Cipher, Key);
end;

function UEncrypt_AES_PKCS7_ECB_192(
  const Data: TUInt8Array;
  const Key: TUAES.TKey192
): TUInt8Array;
begin
  Result := TUAES.Encrypt_PKCS7_ECB_192(Data, Key);
end;

function UDecrypt_AES_PKCS7_ECB_192(
  const Cipher: TUInt8Array;
  const Key: TUAES.TKey192
): TUInt8Array;
begin
  Result := TUAES.Decrypt_PKCS7_ECB_192(Cipher, Key);
end;

function UEncrypt_AES_PKCS7_ECB_256(
  const Data: TUInt8Array;
  const Key: TUAES.TKey256
): TUInt8Array;
begin
  Result := TUAES.Encrypt_PKCS7_ECB_256(Data, Key);
end;

function UDecrypt_AES_PKCS7_ECB_256(
  const Cipher: TUInt8Array;
  const Key: TUAES.TKey256
): TUInt8Array;
begin
  Result := TUAES.Decrypt_PKCS7_ECB_256(Cipher, Key);
end;

function UEncrypt_AES_PKCS7_CBC_128(
  const Data: TUInt8Array;
  const Key: TUAES.TKey128;
  const IV: TUAES.TInitVector
): TUInt8Array;
begin
  Result := TUAES.Encrypt_PKCS7_CBC_128(Data, Key, IV);
end;

function UDecrypt_AES_PKCS7_CBC_128(
  const Cipher: TUInt8Array;
  const Key: TUAES.TKey128;
  const IV: TUAES.TInitVector
): TUInt8Array;
begin
  Result := TUAES.Decrypt_PKCS7_CBC_128(Cipher, Key, IV);
end;

function UEncrypt_AES_PKCS7_CBC_192(
  const Data: TUInt8Array;
  const Key: TUAES.TKey192;
  const IV: TUAES.TInitVector
): TUInt8Array;
begin
  Result := TUAES.Encrypt_PKCS7_CBC_192(Data, Key, IV);
end;

function UDecrypt_AES_PKCS7_CBC_192(
  const Cipher: TUInt8Array;
  const Key: TUAES.TKey192;
  const IV: TUAES.TInitVector
): TUInt8Array;
begin
  Result := TUAES.Decrypt_PKCS7_CBC_192(Cipher, Key, IV);
end;

function UEncrypt_AES_PKCS7_CBC_256(
  const Data: TUInt8Array;
  const Key: TUAES.TKey256;
  const IV: TUAES.TInitVector
): TUInt8Array;
begin
  Result := TUAES.Encrypt_PKCS7_CBC_256(Data, Key, IV);
end;

function UDecrypt_AES_PKCS7_CBC_256(
  const Cipher: TUInt8Array;
  const Key: TUAES.TKey256;
  const IV: TUAES.TInitVector
): TUInt8Array;
begin
  Result := TUAES.Decrypt_PKCS7_CBC_256(Cipher, Key, IV);
end;

function UEncrypt_AES_CTR_128(
  const Data: TUInt8Array;
  const Key: TUAES.TKey128;
  const IV: TUAES.TInitVector
): TUInt8Array;
begin
  Result := TUAES.Process_CTR_128(Data, Key, IV);
end;

function UDecrypt_AES_CTR_128(
  const Cipher: TUInt8Array;
  const Key: TUAES.TKey128;
  const IV: TUAES.TInitVector
): TUInt8Array;
begin
  Result := TUAES.Process_CTR_128(Cipher, Key, IV);
end;

function UEncrypt_AES_CTR_192(
  const Data: TUInt8Array;
  const Key: TUAES.TKey192;
  const IV: TUAES.TInitVector
): TUInt8Array;
begin
  Result := TUAES.Process_CTR_192(Data, Key, IV);
end;

function UDecrypt_AES_CTR_192(
  const Cipher: TUInt8Array;
  const Key: TUAES.TKey192;
  const IV: TUAES.TInitVector
): TUInt8Array;
begin
  Result := TUAES.Process_CTR_192(Cipher, Key, IV);
end;

function UEncrypt_AES_CTR_256(
  const Data: TUInt8Array;
  const Key: TUAES.TKey256;
  const IV: TUAES.TInitVector
): TUInt8Array;
begin
  Result := TUAES.Process_CTR_256(Data, Key, IV);
end;

function UDecrypt_AES_CTR_256(
  const Cipher: TUInt8Array;
  const Key: TUAES.TKey256;
  const IV: TUAES.TInitVector
): TUInt8Array;
begin
  Result := TUAES.Process_CTR_256(Cipher, Key, IV);
end;

function UEncrypt_AES_GCM_128(
  const Data: TUInt8Array;
  const Key: TUAES.TKey128;
  const Nonce: TUAES.TInitVector;
  const AAD: TUInt8Array;
  out AuthTag: TUAES.TTag
): TUInt8Array;
begin
  Result := TUAES.Process_GCM_128(Data, Key, Nonce, AAD, True, AuthTag);
end;

function UDecrypt_AES_GCM_128(
  const Cipher: TUInt8Array;
  const Key: TUAES.TKey128;
  const Nonce: TUAES.TInitVector;
  const AAD: TUInt8Array;
  out AuthTag: TUAES.TTag
): TUInt8Array;
begin
  Result := TUAES.Process_GCM_128(Cipher, Key, Nonce, AAD, False, AuthTag);
end;

function UEncrypt_AES_GCM_192(
  const Data: TUInt8Array;
  const Key: TUAES.TKey192;
  const Nonce: TUAES.TInitVector;
  const AAD: TUInt8Array;
  out AuthTag: TUAES.TTag
): TUInt8Array;
begin
  Result := TUAES.Process_GCM_192(Data, Key, Nonce, AAD, True, AuthTag);
end;

function UDecrypt_AES_GCM_192(
  const Cipher: TUInt8Array;
  const Key: TUAES.TKey192;
  const Nonce: TUAES.TInitVector;
  const AAD: TUInt8Array;
  out AuthTag: TUAES.TTag
): TUInt8Array;
begin
  Result := TUAES.Process_GCM_192(Cipher, Key, Nonce, AAD, False, AuthTag);
end;

function UEncrypt_AES_GCM_256(
  const Data: TUInt8Array;
  const Key: TUAES.TKey256;
  const Nonce: TUAES.TInitVector;
  const AAD: TUInt8Array;
  out AuthTag: TUAES.TTag
): TUInt8Array;
begin
  Result := TUAES.Process_GCM_256(Data, Key, Nonce, AAD, True, AuthTag);
end;

function UDecrypt_AES_GCM_256(
  const Cipher: TUInt8Array;
  const Key: TUAES.TKey256;
  const Nonce: TUAES.TInitVector;
  const AAD: TUInt8Array;
  out AuthTag: TUAES.TTag
): TUInt8Array;
begin
  Result := TUAES.Process_GCM_256(Cipher, Key, Nonce, AAD, False, AuthTag);
end;

function UEncrypt_DES_PKCS7_ECB(
  const Data: TUInt8Array;
  const Key: TUDES.TKey
): TUInt8Array;
begin
  Result := TUDES.Encrypt_PKCS7_ECB(Data, Key);
end;

function UDecrypt_DES_PKCS7_ECB(
  const Cipher: TUInt8Array;
  const Key: TUDES.TKey
): TUInt8Array;
begin
  Result := TUDES.Decrypt_PKCS7_ECB(Cipher, Key);
end;

function UEncrypt_DES_PKCS7_CBC(
  const Data: TUInt8Array;
  const Key: TUDES.TKey;
  const IV: TUDES.TInitVector
): TUInt8Array;
begin
  Result := TUDES.Encrypt_PKCS7_CBC(Data, Key, IV);
end;

function UDecrypt_DES_PKCS7_CBC(
  const Cipher: TUInt8Array;
  const Key: TUDES.TKey;
  const IV: TUDES.TInitVector
): TUInt8Array;
begin
  Result := TUDES.Decrypt_PKCS7_CBC(Cipher, Key, IV);
end;

function UEncrypt_DES_PKCS7_CTR(
  const Data: TUInt8Array;
  const Key: TUDES.TKey;
  const Nonce: TUDES.TInitVector
): TUInt8Array;
begin
  Result := TUDES.Process_CTR(Data, Key, Nonce);
end;

function UDecrypt_DES_PKCS7_CTR(
  const Cipher: TUInt8Array;
  const Key: TUDES.TKey;
  const Nonce: TUDES.TInitVector
): TUInt8Array;
begin
  Result := TUDES.Process_CTR(Cipher, Key, Nonce);
end;

function UEncrypt_DES_Triple_PKCS7_ECB(
  const Data: TUInt8Array;
  const Key: TUDES.TKey3
): TUInt8Array;
begin
  Result := TUDES.Encrypt_PKCS7_Triple_ECB(Data, Key);
end;

function UDecrypt_DES_Triple_PKCS7_ECB(
  const Cipher: TUInt8Array;
  const Key: TUDES.TKey3
): TUInt8Array;
begin
  Result := TUDES.Decrypt_PKCS7_Triple_ECB(Cipher, Key);
end;

function UEncrypt_DES_Triple_PKCS7_CBC(
  const Data: TUInt8Array;
  const Key: TUDES.TKey3;
  const IV: TUDES.TInitVector
): TUInt8Array;
begin
  Result := TUDES.Encrypt_PKCS7_Triple_CBC(Data, Key, IV);
end;

function UDecrypt_DES_Triple_PKCS7_CBC(
  const Cipher: TUInt8Array;
  const Key: TUDES.TKey3;
  const IV: TUDES.TInitVector
): TUInt8Array;
begin
  Result := TUDES.Decrypt_PKCS7_Triple_CBC(Cipher, Key, IV);
end;

function UEncrypt_DES_Triple_PKCS7_CTR(
  const Data: TUInt8Array;
  const Key: TUDES.TKey3;
  const Nonce: TUDES.TInitVector
): TUInt8Array;
begin
  Result := TUDES.Process_Triple_CTR(Data, Key, Nonce);
end;

function UDecrypt_DES_Triple_PKCS7_CTR(
  const Cipher: TUInt8Array;
  const Key: TUDES.TKey3;
  const Nonce: TUDES.TInitVector
): TUInt8Array;
begin
  Result := TUDES.Process_Triple_CTR(Cipher, Key, Nonce);
end;

end.
