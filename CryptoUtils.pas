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
  type TBigInt = TUInt8192;
  type TUBigIntArray = array of TBigInt;
  type TKey = record
    var n: TBigInt; // modulus
    var e: TBigInt; // public exponent
    var d: TBigInt; // private exponent
    var p: TBigInt; // first prime
    var q: TBigInt; // second prime
    var exp1: TBigInt; // d mod (p - 1)
    var exp2: TBigInt; // d mod (q - 1)
    var c: TBigInt; // q^-1 mod p
    function Size: Uint32; // bit size
    function IsPublic: Boolean;
    function IsPrivate: Boolean;
    function IsCRT: Boolean;
    function IsValid: Boolean;
    class function MakeInvalid: TKey; static;
  end;
  type TKeyPublic = record
    var n: TBigInt;
    var e: TBigInt;
    function Size: Uint32;
    function IsValid: Boolean;
    class function MakeInvalid: TKey; static;
  end;
//public
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
    Primes: array of TBigInt;
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
  class function GCD(const a, b: TBigInt): TBigInt; static;
  class function SimplePrimeTest(const Number: TBigInt; const MaxValue: UInt32 = 1000): Boolean; static;
  class function MillerRabinTest(const Number: TBigInt; const Iterations: Int32 = 50): Boolean; static;
  class function XORBytes(const a, b: TUInt8Array): TUInt8Array; static;
  class function PackDER(const Number: TBigInt): TUInt8Array; static;
  class function UnpackDER(const Bytes: TUInt8Array): TBigInt; static;
  class procedure DebugASN1(const Bytes: TUInt8Array; const Offset: String = ''); static;
  class function EncodeTLV(const Tag: UInt8; const Value: array of UInt8): TUInt8Array; static;
  class function DecodeTLV(const Data: TUInt8Array; var Index: Int32; out Tag: UInt8): TUInt8Array; static;
  class function ASN1ToBase64(const DataASN: TUInt8Array; const Header, Footer: String): String; static;
  class function Base64ToASN1(const Base64: String; const Header, Footer: String): TUInt8Array; static;
  class function MakePrime(const BitCount: Int32 = 1024): TBigInt; static;
  class function MakePrimes(
    const PrimeCount: Int32;
    const BitCount: Int32 = 1024;
    const ThreadCount: Int32 = 8
  ): TUBigIntArray; static;
  class function MakeKey(
    const BitCount: UInt32 = 2048;
    const Threads: Int32 = 16
  ): TURSA.TKey; static;
  class function PackData_PKCS1(
    const Data: Pointer;
    const DataSize: UInt32;
    const BlockSize: UInt32 = 2048
  ): TBigInt; static;
  class function PackData_PKCS1(
    const Data: TUInt8Array;
    const BlockSize: UInt32 = 2048
  ): TBigInt; static;
  class function UnpackData_PKCS1(
    const Block: TBigInt;
    const BlockSize: UInt32 = 2048
  ): TUInt8Array; static;
  class function UnpackStr_PKCS1(
    const Block: TBigInt;
    const BlockSize: UInt32 = 2048
  ): String; static;
  class function PackData_OAEP(
    const Data: Pointer;
    const DataSize: UInt32;
    const BlockSize: UInt32 = 2048
  ): TBigInt; static;
  class function PackData_OAEP(
    const Data: TUInt8Array;
    const BlockSize: UInt32 = 2048
  ): TBigInt; static;
  class function UnpackData_OAEP(
    const Block: TBigInt;
    const BlockSize: UInt32 = 2048
  ): TUInt8Array; static;
  class function UnpackStr_OAEP(
    const Block: TBigInt;
    const BlockSize: UInt32 = 2048
  ): String; static;
  class function PackData_Singature(
    const Data: Pointer;
    const DataSize: UInt32;
    const BlockSize: UInt32 = 2048
  ): TBigInt; static;
  class function UnpackData_Singature(
    const Block: TBigInt;
    const BlockSize: UInt32 = 2048
  ): TUInt8Array; static;
  class function Encrypt_PKCS1(
    const Data: Pointer;
    const DataSize: UInt32;
    const Key: TURSA.TKey
  ): TBigInt; static;
  class function Decrypt_PKCS1_Str(
    const Cipher: TBigInt;
    const Key: TURSA.TKey
  ): String; static;
  class function Decrypt_PKCS1(
    const Cipher: TBigInt;
    const Key: TURSA.TKey
  ): TUInt8Array; static;
  class function Encrypt_OAEP(
    const Data: Pointer;
    const DataSize: UInt32;
    const Key: TURSA.TKey
  ): TBigInt; static;
  class function Decrypt_OAEP_Str(
    const Cipher: TBigInt;
    const Key: TURSA.TKey
  ): String; static;
  class function Decrypt_OAEP(
    const Cipher: TBigInt;
    const Key: TURSA.TKey
  ): TUInt8Array; static;
  class function Decrypt_CRT(
    const Cipher: TBigInt;
    const Key: TURSA.TKey
  ): TBigInt; static;
  class function Decrypt(
    const Cipher: TBigInt;
    const Key: TURSA.TKey
  ): TBigInt; static;
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
    const EncryptedKey: array of UInt8;
    const HMAC_OID: array of UInt8;
    const ALG_OID: array of UInt8;
    const Salt, IV: array of UInt8;
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

type TUECC = record
public
  type TBigInt = TUInt1024;
  type Weierstrass = record
    type TPoint = record
      var x: TBigInt;
      var y: TBigInt;
      class function AtInfinity: TPoint; static;
      function IsAtInfinity: Boolean;
      class operator = (const a, b: TPoint): Boolean;
    end;
    type TCurve = record
      var p: TBigInt; // prime modulus
      var a: TBigInt; // param a
      var b: TBigInt; // param b
      var n: TBigInt; // order modulus
      var g: TPoint; // generation point
      function IsOnCurve(const Point: TPoint): Boolean;
      class function Make_SECP256R1: TCurve; static;
      class function Make_SECP256K1: TCurve; static;
    end;
    type TKey = record
      var d: TBigInt;
      var q: TPoint;
      function IsValid: Boolean;
    end;
    type TSignature = record
      var r: TBigInt;
      var s: TBigInt;
      function IsValid: Boolean;
      function IsValid(const n: TBigInt): Boolean;
    end;
    class var Curve_SECP256R1: TCurve;
    class var Curve_SECP256K1: TCurve;
    class function PointAdd(const Curve: TCurve; const a, b: TPoint): TPoint; static;
    class function PointMultiply(const Curve: TCurve; const a: TPoint; const b: TBigInt): TPoint; static;
    class function MakeKey(const Curve: TCurve): TKey; static;
    class function DerivePublicKey(const Curve: TCurve; const PrivateKey: TBigInt): TPoint; static;
    class function DeriveKey(const Curve: TCurve; const BaseData, Context: TUInt8Array): TKey; static;
    class function Sign(
      const Curve: TCurve;
      const PrivateKey: TBigInt;
      const MessageHash: TBigInt
    ): TSignature; static;
    class function Verify(
      const Curve: TCurve;
      const PublicKey: TPoint;
      const MessageHash: TBigInt;
      const Signature: TSignature
    ): Boolean; static;
    class function SharedKey(
      const Curve: TCurve;
      const PublicKey: TPoint;
      const PrivateKey: TBigInt
    ): TBigInt; static;
    class constructor CreateClass;
  end;
  type Montgomery = record
    type TCurve = record
      var p: TBigInt; // prime modulus
      var a: TBigInt; // param a
      var b: TBigInt; // param b
      var n: TBigInt; // order
      var h: TBigInt; // cofactor
      var u: TBigInt; // base point
      function Add(const v1, v2: TBigInt): TBigInt;
      function Sub(const v1, v2: TBigInt): TBigInt;
      function Mul(const v1, v2: TBigInt): TBigInt;
      function Inv(const v: TBigInt): TBigInt;
    end;
    type TKey = record
      var d: TBigInt;
      var q: TBigInt;
      function IsValid: Boolean;
    end;
    class var Curve_25519: TCurve;
    class var LowOrderPoints: array [0..4] of TBigInt;
    class function IsLowOrderPoint(const p: TBigInt): Boolean; static;
    class function ScalarClamp(const k: TBigInt): TBigInt; static;
    class function ScalarMultiply(
      const Curve: TCurve;
      const k, u: TBigInt
    ): TBigInt; static;
    class function X25519(
      const Curve: TCurve;
      const k, u: TBigInt
    ): TBigInt; static;
    class function MakeKey(const Curve: TCurve): TKey; static;
    class function DerivePublicKey(const Curve: TCurve; const PrivateKey: TBigInt): TBigInt; static;
    class function SharedKey(
      const Curve: TCurve;
      const PublicKey: TBigInt;
      const PrivateKey: TBigInt
    ): TBigInt; static;
    class constructor CreateClass;
  end;
  type Edwards = record
  private
    class procedure Clamp(
      const Bytes: Pointer;
      const First, Last: UInt32
    ); static;
  public
    type TPoint = record
      var x: TBigInt;
      var y: TBigInt;
      var z: TBigInt;
      var t: TBigInt;
      class function Neutral: TPoint; static;
      function IsNeutral: Boolean;
      class function Invalid: TPoint; static;
      function IsValid: Boolean;
      class operator = (const a, b: TPoint): Boolean;
    end;
    type TUInt8Arr32 = array[0..31] of UInt8;
    type TPointCompressed = TUInt8Arr32;
    type TCurve = record
      var p: TBigInt; // prime modulus
      var a: TBigInt; // param a
      var d: TBigInt; // param d
      var n: TBigInt; // order
      var h: TBigInt; // cofactor
      var b: TPoint; // base point
      var SqrtM1: TBigInt;
      function IsOnCurve(const Point: TPoint): Boolean;
      function ToAffine(const Point: TPoint): TPoint;
      function Add(const v1, v2: TBigInt): TBigInt;
      function Sub(const v1, v2: TBigInt): TBigInt;
      function Mul(const v1, v2: TBigInt): TBigInt;
      function Inv(const v: TBigInt): TBigInt;
    end;
    type TKey = record
      var d: TUInt8Arr32;
      var q: TPointCompressed;
      function IsValid: Boolean;
      class function Invalid: TKey; static;
    end;
    type TSignature = record
      var r: TPointCompressed;
      var s: TBigInt;
      function IsValid(const Curve: TCurve): Boolean;
      class function Invalid: TSignature; static;
      function ToHex: String;
    end;
    class var Curve_Ed25519: TCurve;
    class function PointAdd(const Curve: TCurve; const p, q: TPoint): TPoint; static;
    class function PointDouble(const Curve: TCurve; const p: TPoint): TPoint; static;
    class function ScalarMultiply(const Curve: TCurve; const k: TBigInt; const p: TPoint): TPoint; static;
    class function PointCompress(const Curve: TCurve; const p: TPoint): TPointCompressed; static;
    class function PointDecompress(const Curve: TCurve; const pc: TPointCompressed): TPoint; static;
    class function MakeKey(const Curve: TCurve; const Seed: array of UInt8): TKey; static;
    class function MakeKey(const Curve: TCurve): TKey; static;
    class function Sign_Ed25519(
      const Curve: TCurve;
      const Key: TKey;
      const Message: TUInt8Array
    ): TSignature; static;
    class function Verify_Ed25519(
      const Curve: TCurve;
      const PublicKey: TPointCompressed;
      const Message: TUInt8Array;
      const Signature: TSignature
    ): Boolean; static;
    class function MakeKey_BLAKE3(const Curve: TCurve; const Seed: array of UInt8): TKey; static;
    class function MakeKey_BLAKE3(const Curve: TCurve): TKey; static;
    class function Sign_Ed25519_BLAKE3(
      const Curve: TCurve;
      const Key: TKey;
      const Message: TUInt8Array
    ): TSignature; static;
    class function Verify_Ed25519_BLAKE3(
      const Curve: TCurve;
      const PublicKey: TPointCompressed;
      const Message: TUInt8Array;
      const Signature: TSignature
    ): Boolean; static;
    class function MakeKey_SHAKE(const Curve: TCurve; const Seed: array of UInt8): TKey; static;
    class function MakeKey_SHAKE(const Curve: TCurve): TKey; static;
    class function Sign_Ed25519_SHAKE(
      const Curve: TCurve;
      const Key: TKey;
      const Message: TUInt8Array
    ): TSignature; static;
    class function Verify_Ed25519_SHAKE(
      const Curve: TCurve;
      const PublicKey: TPointCompressed;
      const Message: TUInt8Array;
      const Signature: TSignature
    ): Boolean; static;
    class constructor CreateClass;
  end;
end;

type TUBLAKE3 = record
public
  type TKey = array[0..31] of UInt8;
  class function KeyFromHex(const Hex: String): TKey; static;
private
  const BLOCK_LEN = 64;
  const CHUNK_LEN = 1024;
  const CHUNK_START = 1 shl 0;
  const CHUNK_END = 1 shl 1;
  const PARENT = 1 shl 2;
  const ROOT = 1 shl 3;
  const KEYED_HASH = 1 shl 4;
  const DERIVE_KEY_CONTEXT = 1 shl 5;
  const DERIVE_KEY_MATERIAL = 1 shl 6;
  const IV: array[0..7] of UInt32 = (
    $6a09e667, $bb67ae85, $3c6ef372, $a54ff53a,
    $510e527f, $9b05688c, $1f83d9ab, $5be0cd19
  );
  const MSG_PERMUTATION: array[0..15] of UInt8 = (
    2, 6, 3, 10, 7, 0, 4, 13, 1, 11, 12, 5, 9, 14, 15, 8
  );
  type TUInt32Arr8 = array[0..7] of UInt32;
  type TUInt32Arr16 = array[0..15] of UInt32;
  type TState = TUInt32Arr16;
  type TBlock = TUInt32Arr16;
  type TOutput = record
    var InputChainingValue: TUInt32Arr8;
    var BlockWords: TBlock;
    var Counter: UInt64;
    var BlockLen: UInt32;
    var Flags: UInt32;
    function ChainingValue: TUInt32Arr8;
    function RootBytes(const OutLen: UInt32): TUInt8Array;
  end;
  type TChunkState = record
    var ChainingValue: TUInt32Arr8;
    var ChunkCounter: UInt64;
    var Block: array[0..BLOCK_LEN - 1] of UInt8;
    var BlockLen: UInt8;
    var BlocksCompressed: UInt8;
    var Flags: UInt32;
    function Len: UInt32;
    function StartFlag: UInt32;
    function Output: TOutput;
    procedure Init(
      const AKey: TUInt32Arr8;
      const AChunkCounter: UInt64;
      const AFlags: UInt32
    );
    procedure Update(
      const Input: TUInt8Array;
      var InputOffset: Int32;
      const InputLen: Int32
    );
  end;
  type THasher = record
    var ChunkState: TChunkState;
    var Key: TUInt32Arr8;
    var CVStack: array of TUInt32Arr8;
    var CVStackLen: Int32;
    var Flags: UInt32;
    procedure Init(const AFlags: UInt32 = 0);
    procedure Init(const AKey: TUBLAKE3.TKey; const AFlags: UInt32 = KEYED_HASH);
    procedure Update(const Input: TUInt8Array);
    function Finalize(const OutLen: UInt32): TUInt8Array;
    procedure PushStack(const CV: TUInt32Arr8);
    function PopStack: TUInt32Arr8;
    procedure AddChunkChainingValue(
      var NewCV: TUInt32Arr8;
      const TotalChunks: UInt64
    );
  end;
  class function ROTR32(const x: UInt32; const n: UInt8): UInt32; static;
  class procedure G(var State: TState; const a, b, c, d, mx, my: UInt32); static;
  class function BytesToWords(const BlockBytes: array of UInt8): TUInt32Arr16; static;
  class procedure Round(var State: TState; const Msg: TBlock); static;
  class procedure Permute(var Msg: TBlock); static;
  class function Compress(
    const ChainingValue: TUInt32Arr8;
    const BlockWords: TBlock;
    const Counter: UInt64;
    const BlockLen: UInt32;
    const Flags: UInt32
  ): TBlock; static;
  class function ParentOutput(
    const LeftChildCV, RightChildCV: TUInt32Arr8;
    const Key: TUInt32Arr8;
    const Flags: UInt32
  ): TOutput; static;
public
  class function Hash(
    const Data: TUInt8Array;
    const OutputSize: UInt32
  ): TUInt8Array; static;
  class function Hash(
    const Data: TUInt8Array;
    const Key: TKey;
    const OutputSize: UInt32
  ): TUInt8Array; static;
  class function KDF(
    const Context, Password: TUInt8Array;
    const OutputSize: Uint32
  ): TUInt8Array; static;
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

function USHAKE_128(const Data: Pointer; const DataSize: UInt32; const OutputSize: Int32): TUInt8Array;
function USHAKE_128(const Data: TUInt8Array; const OutputSize: Int32): TUInt8Array;
function USHAKE_128(const Data: String; const OutputSize: Int32): TUInt8Array;

function USHAKE_256(const Data: Pointer; const DataSize: UInt32; const OutputSize: Int32): TUInt8Array;
function USHAKE_256(const Data: TUInt8Array; const OutputSize: Int32): TUInt8Array;
function USHAKE_256(const Data: String; const OutputSize: Int32): TUInt8Array;

function UcSHAKE_128(const Data: Pointer; const DataSize, OutputSize: UInt32; const FunctionName, Customization: TUInt8Array): TUInt8Array;
function UcSHAKE_128(const Data: TUInt8Array; const OutputSize: UInt32; const FunctionName, Customization: TUInt8Array): TUInt8Array;
function UcSHAKE_128(const Data: String; const OutputSize: UInt32; const FunctionName, Customization: String): TUInt8Array;

function UcSHAKE_256(const Data: Pointer; const DataSize, OutputSize: UInt32; const FunctionName, Customization: TUInt8Array): TUInt8Array;
function UcSHAKE_256(const Data: TUInt8Array; const OutputSize: UInt32; const FunctionName, Customization: TUInt8Array): TUInt8Array;
function UcSHAKE_256(const Data: String; const OutputSize: UInt32; const FunctionName, Customization: String): TUInt8Array;

function UBLAKE3_Hash(const Data: TUInt8Array; const OutputSize: UInt32): TUInt8Array;
function UBLAKE3_Hash(const Data: TUInt8Array; const Key: TUBLAKE3.TKey; const OutputSize: UInt32): TUInt8Array;
function UBLAKE3_KDF(const Context, Password: TUInt8Array; const OutputSize: UInt32): TUInt8Array;
function UBLAKE3_KDF(const Context, Password: String; const OutputSize: UInt32): TUInt8Array;

function UDigestMD5(const Data: TUInt8Array): TUInt8Array;
function UDigestSHA1(const Data: TUInt8Array): TUInt8Array;
function UDigestSHA2_256(const Data: TUInt8Array): TUInt8Array;
function UDigestSHA2_512(const Data: TUInt8Array): TUInt8Array;
function UDigestSHA3_224(const Data: TUInt8Array): TUInt8Array;
function UDigestSHA3_256(const Data: TUInt8Array): TUInt8Array;
function UDigestSHA3_384(const Data: TUInt8Array): TUInt8Array;
function UDigestSHA3_512(const Data: TUInt8Array): TUInt8Array;

generic function UHMAC<TDigest>(const Key, Data: TUInt8Array): TDigest;
function UHMAC_SHA1(const Key, Data: TUInt8Array): TUDigestSHA1;
function UHMAC_SHA2_256(const Key, Data: TUInt8Array): TUDigestSHA2_256;
function UHMAC_SHA2_512(const Key, Data: TUInt8Array): TUDigestSHA2_512;
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

function UKMAC_128(
  const Data: Pointer;
  const DataSize: UInt32;
  const OutputSize: UInt32;
  const Key: TUInt8Array;
  const Customization: TUInt8Array
): TUInt8Array;
function UKMAC_128(
  const Data: TUInt8Array;
  const OutputSize: UInt32;
  const Key: TUInt8Array;
  const Customization: TUInt8Array
): TUInt8Array;
function UKMAC_128(
  const Data: String;
  const DataSize: UInt32;
  const OutputSize: UInt32;
  const Key: TUInt8Array;
  const Customization: TUInt8Array
): TUInt8Array;
function UKMAC_256(
  const Data: Pointer;
  const DataSize: UInt32;
  const OutputSize: UInt32;
  const Key: TUInt8Array;
  const Customization: TUInt8Array
): TUInt8Array;
function UKMAC_256(
  const Data: TUInt8Array;
  const OutputSize: UInt32;
  const Key: TUInt8Array;
  const Customization: TUInt8Array
): TUInt8Array;
function UKMAC_256(
  const Data: String;
  const DataSize: UInt32;
  const OutputSize: UInt32;
  const Key: TUInt8Array;
  const Customization: TUInt8Array
): TUInt8Array;

function USign_RSA_SHA256(const Data: TUInt8Array; const Key: TURSA.TKey): TUInt8Array;
function USign_RSA_SHA512(const Data: TUInt8Array; const Key: TURSA.TKey): TUInt8Array;
function UVerify_RSA(const Data, Signature: TUInt8Array; const Key: TURSA.TKey): Boolean;

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
function UPBKDF2_HMAC_SHA2_256(
  const Password, Salt: TUInt8Array;
  const KeyLength: Int32;
  const Iterations: Int32 = 600000
): TUInt8Array;
function UPBKDF2_HMAC_SHA2_512(
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
function UEvpKDF_SHA2_256(
  const Password, Salt: TUInt8Array;
  const KeyLength: Int32;
  const IVLength: Int32;
  const Iterations: Int32;
  out IV: TUInt8Array
): TUInt8Array;

function UMGF1_SHA2_256(const Seed: TUInt8Array; const MaskLen: Int32): TUInt8Array;

function UMakeRSAKey(
  const BitCount: UInt32;
  const Threads: Int32
): TURSA.TKey;
function UExportRSAKey_PKCS1(const Key: TURSA.TKey): String;
function UExportRSAKey_PKCS8(const Key: TURSA.TKey): String;
function UExportRSAKey_PKCS8(const Key: TURSA.TKey; const Password: String; const Iterations: UInt32 = 600000): String;
function UExportRSAKey_PKCS8(const Key: TURSA.TKey; const Password: TUInt8Array; const Iterations: UInt32 = 600000): String;
function UExportRSAKey_X509(const Key: TURSA.TKey): String;
function UImportRSAKey(const KeyASN1: String; const Password: String = ''): TURSA.TKey;
function UImportRSAKey(const KeyASN1: String; const Password: TUInt8Array): TURSA.TKey;
function UEncrypt_RSA_PKCS1(
  const Data: Pointer;
  const DataSize: UInt32;
  const Key: TURSA.TKey
): TURSA.TBigInt;
function UEncrypt_RSA_PKCS1(
  const Data: TUInt8Array;
  const Key: TURSA.TKey
): TURSA.TBigInt;
function UEncrypt_RSA_PKCS1_Str(
  const Str: String;
  const Key: TURSA.TKey
): TURSA.TBigInt;
function UDecrypt_RSA_PKCS1_Str(
  const Cipher: TURSA.TBigInt;
  const Key: TURSA.TKey
): String;
function UDecrypt_RSA_PKCS1(
  const Cipher: TURSA.TBigInt;
  const Key: TURSA.TKey
): TUInt8Array;
function UEncrypt_RSA_OAEP(
  const Data: Pointer;
  const DataSize: UInt32;
  const Key: TURSA.TKey
): TURSA.TBigInt;
function UEncrypt_RSA_OAEP(
  const Data: TUInt8Array;
  const Key: TURSA.TKey
): TURSA.TBigInt;
function UEncrypt_RSA_OAEP_Str(
  const Str: String;
  const Key: TURSA.TKey
): TURSA.TBigInt;
function UDecrypt_RSA_OAEP_Str(
  const Cipher: TURSA.TBigInt;
  const Key: TURSA.TKey
): String;
function UDecrypt_RSA_OAEP(
  const Cipher: TURSA.TBigInt;
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

function UMakeECCKey: TUECC.Weierstrass.TKey;
function USign_ECDSA(
  const MessageHash: TUECC.TBigInt;
  const PrivateKey: TUECC.TBigInt
): TUECC.Weierstrass.TSignature;
function USign_ECDSA_SHA2_256(
  const Message: TUInt8Array;
  const PrivateKey: TUECC.TBigInt
): TUECC.Weierstrass.TSignature;
function UVerify_ECDSA(
  const MessageHash: TUECC.TBigInt;
  const PublicKey: TUECC.Weierstrass.TPoint;
  const Signature: TUECC.Weierstrass.TSignature
): Boolean;
function USharedKey_ECDH(
  const PublicKey: TUECC.Weierstrass.TPoint;
  const PrivateKey: TUECC.TBigInt
): TUECC.TBigInt;

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
  Result.n := TBigInt.Invalid;
  Result.e := TBigInt.Invalid;
  Result.d := TBigInt.Invalid;
  Result.p := TBigInt.Invalid;
  Result.q := TBigInt.Invalid;
  Result.exp1 := TBigInt.Invalid;
  Result.exp2 := TBigInt.Invalid;
  Result.c := TBigInt.Invalid;
end;

function TURSA.TKeyPublic.Size: Uint32;
begin
  Result := n.BytesUsed * 8;
end;

function TURSA.TKeyPublic.IsValid: Boolean;
begin
  Result := n.IsValid and e.IsValid;
end;

class function TURSA.TKeyPublic.MakeInvalid: TKey;
begin
  Result.n := TBigInt.Invalid;
  Result.e := TBigInt.Invalid;
end;

procedure TURSA.TMakePrimeThread.Execute;
  var Number: TBigInt;
  var i: Int32;
begin
  UThreadRandomSeed := RandSeed;
  with Context^ do
  repeat
    Number := TBigInt.MakeRandom(BitCount);
    Number := Number or (TBigInt.One shl (BitCount - 1));
    Number[0] := Number[0] or 1;
    if not TURSA.SimplePrimeTest(Number, 2000) then Continue;
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

class function TURSA.GCD(const a, b: TBigInt): TBigInt;
  var Remainder: TBigInt;
  var LocalA, LocalB: TBigInt;
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

class function TURSA.SimplePrimeTest(
  const Number: TBigInt;
  const MaxValue: UInt32
): Boolean;
  const PrimeTable: array[0..1228] of UInt32 = (
    2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53,
    59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131,
    137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 211, 223,
    227, 229, 233, 239, 241, 251, 257, 263, 269, 271, 277, 281, 283, 293, 307, 311,
    313, 317, 331, 337, 347, 349, 353, 359, 367, 373, 379, 383, 389, 397, 401, 409,
    419, 421, 431, 433, 439, 443, 449, 457, 461, 463, 467, 479, 487, 491, 499, 503,
    509, 521, 523, 541, 547, 557, 563, 569, 571, 577, 587, 593, 599, 601, 607, 613,
    617, 619, 631, 641, 643, 647, 653, 659, 661, 673, 677, 683, 691, 701, 709, 719,
    727, 733, 739, 743, 751, 757, 761, 769, 773, 787, 797, 809, 811, 821, 823, 827,
    829, 839, 853, 857, 859, 863, 877, 881, 883, 887, 907, 911, 919, 929, 937, 941,
    947, 953, 967, 971, 977, 983, 991, 997, 1009, 1013, 1019, 1021, 1031, 1033, 1039, 1049,
    1051, 1061, 1063, 1069, 1087, 1091, 1093, 1097, 1103, 1109, 1117, 1123, 1129, 1151, 1153, 1163,
    1171, 1181, 1187, 1193, 1201, 1213, 1217, 1223, 1229, 1231, 1237, 1249, 1259, 1277, 1279, 1283,
    1289, 1291, 1297, 1301, 1303, 1307, 1319, 1321, 1327, 1361, 1367, 1373, 1381, 1399, 1409, 1423,
    1427, 1429, 1433, 1439, 1447, 1451, 1453, 1459, 1471, 1481, 1483, 1487, 1489, 1493, 1499, 1511,
    1523, 1531, 1543, 1549, 1553, 1559, 1567, 1571, 1579, 1583, 1597, 1601, 1607, 1609, 1613, 1619,
    1621, 1627, 1637, 1657, 1663, 1667, 1669, 1693, 1697, 1699, 1709, 1721, 1723, 1733, 1741, 1747,
    1753, 1759, 1777, 1783, 1787, 1789, 1801, 1811, 1823, 1831, 1847, 1861, 1867, 1871, 1873, 1877,
    1879, 1889, 1901, 1907, 1913, 1931, 1933, 1949, 1951, 1973, 1979, 1987, 1993, 1997, 1999, 2003,
    2011, 2017, 2027, 2029, 2039, 2053, 2063, 2069, 2081, 2083, 2087, 2089, 2099, 2111, 2113, 2129,
    2131, 2137, 2141, 2143, 2153, 2161, 2179, 2203, 2207, 2213, 2221, 2237, 2239, 2243, 2251, 2267,
    2269, 2273, 2281, 2287, 2293, 2297, 2309, 2311, 2333, 2339, 2341, 2347, 2351, 2357, 2371, 2377,
    2381, 2383, 2389, 2393, 2399, 2411, 2417, 2423, 2437, 2441, 2447, 2459, 2467, 2473, 2477, 2503,
    2521, 2531, 2539, 2543, 2549, 2551, 2557, 2579, 2591, 2593, 2609, 2617, 2621, 2633, 2647, 2657,
    2659, 2663, 2671, 2677, 2683, 2687, 2689, 2693, 2699, 2707, 2711, 2713, 2719, 2729, 2731, 2741,
    2749, 2753, 2767, 2777, 2789, 2791, 2797, 2801, 2803, 2819, 2833, 2837, 2843, 2851, 2857, 2861,
    2879, 2887, 2897, 2903, 2909, 2917, 2927, 2939, 2953, 2957, 2963, 2969, 2971, 2999, 3001, 3011,
    3019, 3023, 3037, 3041, 3049, 3061, 3067, 3079, 3083, 3089, 3109, 3119, 3121, 3137, 3163, 3167,
    3169, 3181, 3187, 3191, 3203, 3209, 3217, 3221, 3229, 3251, 3253, 3257, 3259, 3271, 3299, 3301,
    3307, 3313, 3319, 3323, 3329, 3331, 3343, 3347, 3359, 3361, 3371, 3373, 3389, 3391, 3407, 3413,
    3433, 3449, 3457, 3461, 3463, 3467, 3469, 3491, 3499, 3511, 3517, 3527, 3529, 3533, 3539, 3541,
    3547, 3557, 3559, 3571, 3581, 3583, 3593, 3607, 3613, 3617, 3623, 3631, 3637, 3643, 3659, 3671,
    3673, 3677, 3691, 3697, 3701, 3709, 3719, 3727, 3733, 3739, 3761, 3767, 3769, 3779, 3793, 3797,
    3803, 3821, 3823, 3833, 3847, 3851, 3853, 3863, 3877, 3881, 3889, 3907, 3911, 3917, 3919, 3923,
    3929, 3931, 3943, 3947, 3967, 3989, 4001, 4003, 4007, 4013, 4019, 4021, 4027, 4049, 4051, 4057,
    4073, 4079, 4091, 4093, 4099, 4111, 4127, 4129, 4133, 4139, 4153, 4157, 4159, 4177, 4201, 4211,
    4217, 4219, 4229, 4231, 4241, 4243, 4253, 4259, 4261, 4271, 4273, 4283, 4289, 4297, 4327, 4337,
    4339, 4349, 4357, 4363, 4373, 4391, 4397, 4409, 4421, 4423, 4441, 4447, 4451, 4457, 4463, 4481,
    4483, 4493, 4507, 4513, 4517, 4519, 4523, 4547, 4549, 4561, 4567, 4583, 4591, 4597, 4603, 4621,
    4637, 4639, 4643, 4649, 4651, 4657, 4663, 4673, 4679, 4691, 4703, 4721, 4723, 4729, 4733, 4751,
    4759, 4783, 4787, 4789, 4793, 4799, 4801, 4813, 4817, 4831, 4861, 4871, 4877, 4889, 4903, 4909,
    4919, 4931, 4933, 4937, 4943, 4951, 4957, 4967, 4969, 4973, 4987, 4993, 4999, 5003, 5009, 5011,
    5021, 5023, 5039, 5051, 5059, 5077, 5081, 5087, 5099, 5101, 5107, 5113, 5119, 5147, 5153, 5167,
    5171, 5179, 5189, 5197, 5209, 5227, 5231, 5233, 5237, 5261, 5273, 5279, 5281, 5297, 5303, 5309,
    5323, 5333, 5347, 5351, 5381, 5387, 5393, 5399, 5407, 5413, 5417, 5419, 5431, 5437, 5441, 5443,
    5449, 5471, 5477, 5479, 5483, 5501, 5503, 5507, 5519, 5521, 5527, 5531, 5557, 5563, 5569, 5573,
    5581, 5591, 5623, 5639, 5641, 5647, 5651, 5653, 5657, 5659, 5669, 5683, 5689, 5693, 5701, 5711,
    5717, 5737, 5741, 5743, 5749, 5779, 5783, 5791, 5801, 5807, 5813, 5821, 5827, 5839, 5843, 5849,
    5851, 5857, 5861, 5867, 5869, 5879, 5881, 5897, 5903, 5923, 5927, 5939, 5953, 5981, 5987, 6007,
    6011, 6029, 6037, 6043, 6047, 6053, 6067, 6073, 6079, 6089, 6091, 6101, 6113, 6121, 6131, 6133,
    6143, 6151, 6163, 6173, 6197, 6199, 6203, 6211, 6217, 6221, 6229, 6247, 6257, 6263, 6269, 6271,
    6277, 6287, 6299, 6301, 6311, 6317, 6323, 6329, 6337, 6343, 6353, 6359, 6361, 6367, 6373, 6379,
    6389, 6397, 6421, 6427, 6449, 6451, 6469, 6473, 6481, 6491, 6521, 6529, 6547, 6551, 6553, 6563,
    6569, 6571, 6577, 6581, 6599, 6607, 6619, 6637, 6653, 6659, 6661, 6673, 6679, 6689, 6691, 6701,
    6703, 6709, 6719, 6733, 6737, 6761, 6763, 6779, 6781, 6791, 6793, 6803, 6823, 6827, 6829, 6833,
    6841, 6857, 6863, 6869, 6871, 6883, 6899, 6907, 6911, 6917, 6947, 6949, 6959, 6961, 6967, 6971,
    6977, 6983, 6991, 6997, 7001, 7013, 7019, 7027, 7039, 7043, 7057, 7069, 7079, 7103, 7109, 7121,
    7127, 7129, 7151, 7159, 7177, 7187, 7193, 7207, 7211, 7213, 7219, 7229, 7237, 7243, 7247, 7253,
    7283, 7297, 7307, 7309, 7321, 7331, 7333, 7349, 7351, 7369, 7393, 7411, 7417, 7433, 7451, 7457,
    7459, 7477, 7481, 7487, 7489, 7499, 7507, 7517, 7523, 7529, 7537, 7541, 7547, 7549, 7559, 7561,
    7573, 7577, 7583, 7589, 7591, 7603, 7607, 7621, 7639, 7643, 7649, 7669, 7673, 7681, 7687, 7691,
    7699, 7703, 7717, 7723, 7727, 7741, 7753, 7757, 7759, 7789, 7793, 7817, 7823, 7829, 7841, 7853,
    7867, 7873, 7877, 7879, 7883, 7901, 7907, 7919, 7927, 7933, 7937, 7949, 7951, 7963, 7993, 8009,
    8011, 8017, 8039, 8053, 8059, 8069, 8081, 8087, 8089, 8093, 8101, 8111, 8117, 8123, 8147, 8161,
    8167, 8171, 8179, 8191, 8209, 8219, 8221, 8231, 8233, 8237, 8243, 8263, 8269, 8273, 8287, 8291,
    8293, 8297, 8311, 8317, 8329, 8353, 8363, 8369, 8377, 8387, 8389, 8419, 8423, 8429, 8431, 8443,
    8447, 8461, 8467, 8501, 8513, 8521, 8527, 8537, 8539, 8543, 8563, 8573, 8581, 8597, 8599, 8609,
    8623, 8627, 8629, 8641, 8647, 8663, 8669, 8677, 8681, 8689, 8693, 8699, 8707, 8713, 8719, 8731,
    8737, 8741, 8747, 8753, 8761, 8779, 8783, 8803, 8807, 8819, 8821, 8831, 8837, 8839, 8849, 8861,
    8863, 8867, 8887, 8893, 8923, 8929, 8933, 8941, 8951, 8963, 8969, 8971, 8999, 9001, 9007, 9011,
    9013, 9029, 9041, 9043, 9049, 9059, 9067, 9091, 9103, 9109, 9127, 9133, 9137, 9151, 9157, 9161,
    9173, 9181, 9187, 9199, 9203, 9209, 9221, 9227, 9239, 9241, 9257, 9277, 9281, 9283, 9293, 9311,
    9319, 9323, 9337, 9341, 9343, 9349, 9371, 9377, 9391, 9397, 9403, 9413, 9419, 9421, 9431, 9433,
    9437, 9439, 9461, 9463, 9467, 9473, 9479, 9491, 9497, 9511, 9521, 9533, 9539, 9547, 9551, 9587,
    9601, 9613, 9619, 9623, 9629, 9631, 9643, 9649, 9661, 9677, 9679, 9689, 9697, 9719, 9721, 9733,
    9739, 9743, 9749, 9767, 9769, 9781, 9787, 9791, 9803, 9811, 9817, 9829, 9833, 9839, 9851, 9857,
    9859, 9871, 9883, 9887, 9901, 9907, 9923, 9929, 9931, 9941, 9949, 9967, 9973
  );
  var i: Int32;
  var Zero: TBigInt;
begin
  Zero := TBigInt.Zero;
  Result := True;
  for i := 0 to High(PrimeTable) do
  begin
    if PrimeTable[i] > MaxValue then Exit;
    if Number mod PrimeTable[i] = Zero then Exit(False);
  end;
end;

class function TURSA.MillerRabinTest(const Number: TBigInt; const Iterations: Int32): Boolean;
  var Two: TBigInt;
  var Cmp: Int8;
  var i: Int32;
  var m, m_temp: UInt32;
  var d, a, x, n_minus_1, n_minus_2: TBigInt;
begin
  Two := 2;
  Cmp := TBigInt.Compare(Number, Two);
  if Cmp < 0 then Exit(False);
  if Cmp = 0 then Exit(True);
  if not Number.IsOdd then Exit(False);
  n_minus_1 := Number - TBigInt.One;
  d := n_minus_1;
  m := 0;
  while not d.IsOdd and (d > TBigInt.Zero) do
  begin
    d := TBigInt.ShrOne(d);
    Inc(m);
  end;
  n_minus_2 := Number - Two;
  for i := 1 to Iterations do
  begin
    a := TBigInt.MakeRandomRange(Two, n_minus_2);
    x := TBigInt.ModPow(a, d, Number);
    if (x = TBigInt.One) or (x = n_minus_1) then Continue;
    m_temp := m;
    while m_temp > 1 do
    begin
      x := TBigInt.ModPow(x, Two, Number);
      if x = TBigInt.One then Exit(False);
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

class function TURSA.PackDER(const Number: TBigInt): TUInt8Array;
  const ByteCount = (TBigInt.Size32.Value) * 4;
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

class function TURSA.UnpackDER(const Bytes: TUInt8Array): TBigInt;
  const ByteCount = (TBigInt.Size32.Value) * 4;
  var AsBytes: array[0..ByteCount - 1] of UInt8 absolute Result;
  var i, StartIndex, CopyCount: Int32;
begin
  Result := TBigInt.Zero;
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
  const Value: array of UInt8
): TUInt8Array;
  var Len, TempLen, NumLenBytes, i: Int32;
  var LenBytes: TUInt8Array;
begin
  Len := Length(Value);
  LenBytes := nil;
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
  Result := UBytesConcat([[Tag], LenBytes, Value]);
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

class function TURSA.MakePrime(const BitCount: Int32): TBigInt;
begin
  repeat
    Result := TBigInt.MakeRandom(BitCount);
    Result := Result or (TBigInt.One shl (BitCount - 1));
    Result[0] := Result[0] or 1;
  until TURSA.SimplePrimeTest(Result, 2000) and MillerRabinTest(Result, 50);
end;

class function TURSA.MakePrimes(
  const PrimeCount: Int32;
  const BitCount: Int32;
  const ThreadCount: Int32
): TUBigIntArray;
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
      Move(Context.Primes[0], Result[0], Length(Result) * SizeOf(TBigInt));
    end;
  end;
end;

class function TURSA.MakeKey(
  const BitCount: UInt32;
  const Threads: Int32
): TURSA.TKey;
  var p, q, n, phi, e, d, One, p_minus_1, q_minus_1: TBigInt;
  var PrimeSizeInBits: Int32;
  var Primes: TUBigIntArray;
begin
  One := TBigInt.One;
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
  d := TBigInt.ModInv(e, phi);
  Result.n := n;
  Result.e := e;
  Result.d := d;
  Result.q := q;
  Result.p := p;
  Result.exp1 := d mod p_minus_1;
  Result.exp2 := d mod q_minus_1;
  Result.c := TBigInt.ModInv(q, p);
end;

class function TURSA.PackData_PKCS1(
  const Data: Pointer;
  const DataSize: UInt32;
  const BlockSize: UInt32
): TBigInt;
  const MinPadding = 11;
  const ByteCount = (TBigInt.Size32.Value) * 4;
  var PaddingSize, BlockSizeInBytes: Int32;
  var PaddedDataBE: array[0..ByteCount - 1] of UInt8;
  var PaddedData: array[0..ByteCount - 1] of UInt8 absolute Result;
  var i: Int32;
begin
  BlockSizeInBytes := BlockSize shr 3;
  if DataSize > BlockSizeInBytes - MinPadding then Exit(TBigInt.Invalid);
  Result := TBigInt.Zero;
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
): TBigInt;
begin
  Result := PackData_PKCS1(@Data[0], Length(Data), BlockSize);
end;

class function TURSA.UnpackData_PKCS1(const Block: TBigInt; const BlockSize: UInt32): TUInt8Array;
  const ByteCount = (TBigInt.Size32.Value) * 4;
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

class function TURSA.UnpackStr_PKCS1(const Block: TBigInt; const BlockSize: UInt32): String;
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
): TBigInt;
  const HashLen = SizeOf(TUDigestSHA2_256);
  const ByteCount = (TBigInt.Size32.Value) * 4;
  var BlockSizeInBytes: UInt32;
  var PaddedDataBE: array[0..ByteCount - 1] of UInt8;
  var PaddedData: array[0..ByteCount - 1] of UInt8 absolute Result;
  var PadStrLen: Int32;
  var DB, Seed, DBMask, MaskedDB, SeedMask, MaskedSeed: TUInt8Array;
  var i, j: Int32;
  var LabelHash: TUDigestSHA2_256;
begin
  BlockSizeInBytes := BlockSize shr 3;
  if DataSize > BlockSizeInBytes - 2 * HashLen - 2 then Exit(TBigInt.Invalid);
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
  DBMask := UMGF1_SHA2_256(Seed, BlockSizeInBytes - HashLen - 1);
  MaskedDB := XORBytes(DB, DBMask);
  SeedMask := UMGF1_SHA2_256(MaskedDB, HashLen);
  MaskedSeed := XORBytes(Seed, SeedMask);
  Result := TBigInt.Zero;
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
): TBigInt;
begin
  Result := PackData_OAEP(@Data[0], Length(Data));
end;

class function TURSA.UnpackData_OAEP(
  const Block: TBigInt;
  const BlockSize: UInt32
): TUInt8Array;
  const HashLen = SizeOf(TUDigestSHA2_256);
  const ByteCount = (TBigInt.Size32.Value) * 4;
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
  SeedMask := UMGF1_SHA2_256(MaskedDB, HashLen);
  Seed := XORBytes(MaskedSeed, SeedMask);
  DBMask := UMGF1_SHA2_256(Seed, BlockSizeInBytes - HashLen - 1);
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
  const Block: TBigInt;
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
): TBigInt;
  const MinPadding = 11;
  const ByteCount = (TBigInt.Size32.Value) * 4;
  var PaddingSize, BlockSizeInBytes: Int32;
  var PaddedDataBE: array[0..ByteCount - 1] of UInt8;
  var PaddedData: array[0..ByteCount - 1] of UInt8 absolute Result;
  var i: Int32;
begin
  BlockSizeInBytes := BlockSize shr 3;
  if DataSize > BlockSizeInBytes - MinPadding then Exit(TBigInt.Invalid);
  Result := TBigInt.Zero;
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
  const Block: TBigInt;
  const BlockSize: UInt32
): TUInt8Array;
  const MinPadding = 11;
  const ByteCount = (TBigInt.Size32.Value) * 4;
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
): TBigInt;
  var Block: TBigInt;
begin
  Block := PackData_PKCS1(Data, DataSize, Key.Size);
  if not Block.IsValid then Exit(TBigInt.Invalid);
  Result := TBigInt.ModPow(Block, Key.e, Key.n);
end;

class function TURSA.Decrypt_PKCS1_Str(
  const Cipher: TBigInt;
  const Key: TURSA.TKey
): String;
  var Block: TBigInt;
begin
  Block := Decrypt(Cipher, Key);
  Result := UnpackStr_PKCS1(Block, Key.Size);
end;

class function TURSA.Decrypt_PKCS1(
  const Cipher: TBigInt;
  const Key: TURSA.TKey
): TUInt8Array;
  var Block: TBigInt;
begin
  Block := Decrypt(Cipher, Key);
  Result := UnpackData_PKCS1(Block, Key.Size);
end;

class function TURSA.Encrypt_OAEP(
  const Data: Pointer;
  const DataSize: UInt32;
  const Key: TURSA.TKey
): TBigInt;
  var Block: TBigInt;
begin
  Block := PackData_OAEP(Data, DataSize, Key.Size);
  if not Block.IsValid then Exit(TBigInt.Invalid);
  Result := TBigInt.ModPow(Block, Key.e, Key.n);
end;

class function TURSA.Decrypt_OAEP_Str(
  const Cipher: TBigInt;
  const Key: TURSA.TKey
): String;
  var Block: TBigInt;
begin
  Block := Decrypt(Cipher, Key);
  Result := UnpackStr_OAEP(Block, Key.Size);
end;

class function TURSA.Decrypt_OAEP(
  const Cipher: TBigInt;
  const Key: TURSA.TKey
): TUInt8Array;
  var Block: TBigInt;
begin
  Block := Decrypt(Cipher, Key);
  Result := UnpackData_OAEP(Block, Key.Size);
end;

class function TURSA.Decrypt_CRT(
  const Cipher: TBigInt;
  const Key: TURSA.TKey
): TBigInt;
  var m1, m2, h, c_mod_p, c_mod_q: TBigInt;
begin
  c_mod_p := Cipher mod Key.p;
  c_mod_q := Cipher mod Key.q;
  m1 := TBigInt.ModPow(c_mod_p, Key.exp1, Key.p);
  m2 := TBigInt.ModPow(c_mod_q, Key.exp2, Key.q);
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
  const Cipher: TBigInt;
  const Key: TURSA.TKey
): TBigInt;
begin
  if Key.IsCRT then Exit(Decrypt_CRT(Cipher, Key));
  Result := TBigInt.ModPow(Cipher, Key.d, Key.n);
end;

class function TURSA.Sign_SHA256(const Data: TUInt8Array; const Key: TKey): TUInt8Array;
  var Hash: TUDigestSHA2_256;
  var HashDER: TUInt8Array;
  var PackedHash, Signature: TBigInt;
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
  Signature := TBigInt.ModPow(PackedHash, Key.d, Key.n);
  Result := Signature.ToBytes;
end;

class function TURSA.Sign_SHA512(const Data: TUInt8Array; const Key: TKey): TUInt8Array;
  var Hash: TUDigestSHA2_512;
  var HashDER: TUInt8Array;
  var PackedHash, Signature: TBigInt;
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
  Signature := TBigInt.ModPow(PackedHash, Key.d, Key.n);
  Result := Signature.ToBytes;
end;

class function TURSA.Verify(
  const Data, Signature: TUInt8Array;
  const Key: TKey
): Boolean;
  var Hash: TUInt8Array;
  var PackedHash: TBigInt;
  var UnpackedDER, SignedHashDER, SignedHash, Alg, OID: TUInt8Array;
  var Index: Int32;
  var Tag: UInt8;
begin
  PackedHash := TBigInt.ModPow(TBigInt(Signature), Key.e, Key.n);
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
  var Components: array [0..7] of TBigInt absolute Result;
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
  var Components: array [0..1] of TBigInt absolute Result;
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
  const EncryptedKey: array of UInt8;
  const HMAC_OID: array of UInt8;
  const ALG_OID: array of UInt8;
  const Salt, IV: array of UInt8;
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
  DerivedKey := UPBKDF2_HMAC_SHA2_256(
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
  DerivedKey := UPBKDF2_HMAC_SHA2_256(
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
  DerivedKey := UPBKDF2_HMAC_SHA2_256(
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
  DerivedKey := UPBKDF2_HMAC_SHA2_512(
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
  DerivedKey := UPBKDF2_HMAC_SHA2_512(
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
  DerivedKey := UPBKDF2_HMAC_SHA2_512(
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
  var SequenceContent: TUInt8Array;
  const Header: String = '-----BEGIN PUBLIC KEY-----';
  const Footer: String = '-----END PUBLIC KEY-----';
begin
  SequenceContent := EncodeTLV(
    $30,
    UBytesConcat([
      EncodeTLV($30,
        UBytesConcat([
          EncodeTLV($06, OID_RSA),
          EncodeTLV($05, [])
        ])
      ),
      EncodeTLV($03,
        UBytesConcat([
          [$00],
          EncodeTLV($30,
            UBytesConcat([
              EncodeTLV($02, PackDER(Key.n)),
              EncodeTLV($02, PackDER(Key.e))
            ])
          )
        ])
      )
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
    EncKey := UEvpKDF_SHA2_256(
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
    EncKey := UEvpKDF_SHA2_256(
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
    EncKey := UEvpKDF_SHA2_256(
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
    DerivedKey := UPBKDF2_HMAC_SHA2_256(Password, Salt, GetKeySize(ALG_OID), IterationCount);
  end
  else if UBytesEqual(HMAC_OID, OID_HMAC_SHA512) then
  begin
    DerivedKey := UPBKDF2_HMAC_SHA2_512(Password, Salt, GetKeySize(ALG_OID), IterationCount);
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
  UMove(Result[0], h0, 4);
  UMove(Result[4], h1, 4);
  UMove(Result[8], h2, 4);
  UMove(Result[12], h3, 4);
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

function UMillerRabinTest(const Number: TURSA.TBigInt; const Iterations: Int32): Boolean;
  var Two: TURSA.TBigInt;
  var Cmp: Int8;
  var i: Int32;
  var m, m_temp: UInt32;
  var d, a, x, n_minus_1, n_minus_2: TURSA.TBigInt;
begin
  Two := 2;
  Cmp := TURSA.TBigInt.Compare(Number, Two);
  if Cmp < 0 then Exit(False);
  if Cmp = 0 then Exit(True);
  if not Number.IsOdd then Exit(False);
  n_minus_1 := Number - TURSA.TBigInt.One;
  d := n_minus_1;
  m := 0;
  while not d.IsOdd and (d > TURSA.TBigInt.Zero) do
  begin
    d := TURSA.TBigInt.ShrOne(d);
    Inc(m);
  end;
  n_minus_2 := Number - Two;
  for i := 1 to Iterations do
  begin
    a := TURSA.TBigInt.MakeRandomRange(Two, n_minus_2);
    x := TURSA.TBigInt.ModPow(a, d, Number);
    if (x = TURSA.TBigInt.One) or (x = n_minus_1) then Continue;
    m_temp := m;
    while m_temp > 1 do
    begin
      x := TURSA.TBigInt.ModPow(x, Two, Number);
      if x = TURSA.TBigInt.One then Exit(False);
      if x = n_minus_1 then Break;
      Dec(m_temp);
    end;
    if not (x = n_minus_1) then Exit(False);
  end;
  Result := True;
end;

function UMakePrime(const BitCount: Int32): TURSA.TBigInt;
  var TestCount: Int32;
begin
  TestCount := 0;
  repeat
    Result := TURSA.TBigInt.MakeRandom(BitCount);
    Result := Result or (TURSA.TBigInt.One shl (BitCount - 1));
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

type TKeccakState = array[0..4, 0..4] of UInt64;
procedure KeccakF1600(var State: TKeccakState);
  function ROTL64(const x: UInt64; const n: UInt8): UInt64;
  begin
    Result := (x shl n) or (x shr (64 - n));
  end;
  const RoundConstants: array[0..23] of UInt64 = (
    UInt64($0000000000000001), UInt64($0000000000008082), UInt64($800000000000808A), UInt64($8000000080008000),
    UInt64($000000000000808B), UInt64($0000000080000001), UInt64($8000000080008081), UInt64($8000000000008009),
    UInt64($000000000000008A), UInt64($0000000000000088), UInt64($0000000080008009), UInt64($000000008000000A),
    UInt64($000000008000808B), UInt64($800000000000008B), UInt64($8000000000008089), UInt64($8000000000008003),
    UInt64($8000000000008002), UInt64($8000000000000080), UInt64($000000000000800A), UInt64($800000008000000A),
    UInt64($8000000080008081), UInt64($8000000000008080), UInt64($0000000080000001), UInt64($8000000080008008)
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

function SHA3(const Data: Pointer; const DataSize: UInt32; const DigestSize: UInt32): TUInt8Array;
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
  UMove(Result, Digest[0], SizeOf(Result));
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
  UMove(Result, Digest[0], SizeOf(Result));
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
  UMove(Result, Digest[0], SizeOf(Result));
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
  UMove(Result, Digest[0], SizeOf(Result));
end;

function USHA3_512(const Data: TUInt8Array): TUDigestSHA3_512;
begin
  Result := USHA3_512(@Data[0], Length(Data));
end;

function USHA3_512(const Data: String): TUDigestSHA3_512;
begin
  Result := USHA3_512(@Data[1], Length(Data));
end;

function KeccakSponge(
  const Data: Pointer;
  const DataSize: UInt32;
  const OutputSize: Int32;
  const SecLevel: UInt32;
  const PaddingByte: UInt8 = $1f
): TUInt8Array;
  var Rate: UInt32;
  var State: TKeccakState;
  var PaddedData: TUInt8Array;
  var i, j, BlockPos, SqueezedBytes: Int32;
  var BlockCount, LastBlockLen, x, y: Int32;
  var w: UInt64;
begin
  UClear(State, SizeOf(State));
  Rate := 200 - (2 * SecLevel);
  BlockCount := (DataSize * 8 + 2 + (Rate * 8 - 1)) div (Rate * 8);
  LastBlockLen := (DataSize * 8 + 2) mod (Rate * 8);
  if LastBlockLen = 0 then LastBlockLen := Rate * 8;
  PaddedData := nil;
  SetLength(PaddedData, BlockCount * Rate);
  if DataSize > 0 then Move(Data^, PaddedData[0], DataSize);
  PaddedData[DataSize] := PaddingByte;
  for i := DataSize + 1 to High(PaddedData) do PaddedData[i] := $00;
  PaddedData[High(PaddedData)] := PaddedData[High(PaddedData)] or $80;
  for i := 0 to (Length(PaddedData) div Rate) - 1 do
  begin
    BlockPos := i * Rate;
    for y := 0 to 4 do
    for x := 0 to 4 do
    if (x + 5 * y) * 8 < Rate then
    begin
      w := 0;
      for j := 0 to 7 do
      begin
        w := w or (UInt64(PaddedData[BlockPos + (x + 5 * y) * 8 + j]) shl (j * 8));
      end;
      State[x, y] := State[x, y] xor w;
    end;
    KeccakF1600(State);
  end;
  Result := nil;
  SetLength(Result, OutputSize);
  SqueezedBytes := 0;
  while SqueezedBytes < OutputSize do
  begin
    for y := 0 to 4 do
    for x := 0 to 4 do
    begin
      if (x + 5 * y) * 8 < Rate then
      for j := 0 to 7 do
      begin
        Result[SqueezedBytes] := UInt8((State[x, y] shr (j * 8)) and $ff);
        Inc(SqueezedBytes);
        if SqueezedBytes >= OutputSize then Exit;
      end;
    end;
    KeccakF1600(State);
  end;
end;

function SHAKE(
  const Data: Pointer;
  const DataSize: UInt32;
  const OutputSize: Int32;
  const SecLevel: UInt32
): TUInt8Array;
begin
  Result := KeccakSponge(Data, DataSize, OutputSize, SecLevel, $1f);
end;

function USHAKE_128(const Data: Pointer; const DataSize: UInt32; const OutputSize: Int32): TUInt8Array;
begin
  Result := SHAKE(Data, DataSize, OutputSize, 16);
end;

function USHAKE_128(const Data: TUInt8Array; const OutputSize: Int32): TUInt8Array;
begin
  Result := SHAKE(@Data[0], Length(Data), OutputSize, 16);
end;

function USHAKE_128(const Data: String; const OutputSize: Int32): TUInt8Array;
begin
  Result := SHAKE(@Data[1], Length(Data), OutputSize, 16);
end;

function USHAKE_256(const Data: Pointer; const DataSize: UInt32; const OutputSize: Int32): TUInt8Array;
begin
  Result := SHAKE(Data, DataSize, OutputSize, 32);
end;

function USHAKE_256(const Data: TUInt8Array; const OutputSize: Int32): TUInt8Array;
begin
  Result := SHAKE(@Data[0], Length(Data), OutputSize, 32);
end;

function USHAKE_256(const Data: String; const OutputSize: Int32): TUInt8Array;
begin
  Result := SHAKE(@Data[1], Length(Data), OutputSize, 32);
end;

function RightEncode(const x: UInt64): TUInt8Array;
  var i, n: Int32;
  var temp: UInt64;
begin
  if x = 0 then
  begin
    n := 1;
  end
  else
  begin
    n := 0;
    temp := x;
    while temp > 0 do
    begin
      temp := temp shr 8;
      Inc(n);
    end;
  end;
  Result := nil;
  SetLength(Result, n + 1);
  temp := x;
  for i := n - 1 downto 0 do
  begin
    Result[i] := UInt8(temp and $ff);
    temp := temp shr 8;
  end;
  Result[n] := UInt8(n);
end;

function LeftEncode(const x: UInt64): TUInt8Array;
  var i, n: Int32;
  var temp: UInt64;
begin
  if x = 0 then
  begin
    n := 1;
  end
  else
  begin
    n := 0;
    temp := x;
    while temp > 0 do
    begin
      temp := temp shr 8;
      Inc(n);
    end;
  end;
  Result := nil;
  SetLength(Result, n + 1);
  Result[0] := UInt8(n);
  temp := x;
  for i := n downto 1 do
  begin
    Result[i] := UInt8(temp and $ff);
    temp := temp shr 8;
  end;
end;

function EncodeString(const Str: TUInt8Array): TUInt8Array;
begin
  Result := UBytesJoin(LeftEncode(UInt64(Length(Str)) * 8), Str);
end;

function BytePad(const X: TUInt8Array; const W: UInt32): TUInt8Array;
  var LeftEnc: TUInt8Array;
  var Pad: UInt32;
  var i: Int32;
begin
  LeftEnc := LeftEncode(W);
  Result := UBytesJoin(LeftEnc, X);
  Pad := W - (UInt32(Length(Result)) mod W);
  if Pad = W then Pad := 0;
  if Pad = 0 then Exit;
  SetLength(Result, Length(Result) + Pad);
  for i := Length(Result) - Pad to High(Result) do
  begin
    Result[i] := 0;
  end;
end;

function cSHAKE(
  const Data: Pointer;
  const DataSize: UInt32;
  const OutputSize: UInt32;
  const SecLevel: UInt32;
  const FunctionName: TUInt8Array;
  const Customization: TUInt8Array
): TUInt8Array;
  var Rate: UInt32;
  var EncName, EncCustom, Prefix, Input: TUInt8Array;
begin
  if (Length(FunctionName) = 0)
  and (Length(Customization) = 0) then
  begin
    Exit(SHAKE(Data, DataSize, OutputSize, SecLevel));
  end;
  Rate := 200 - (2 * SecLevel);
  EncName := EncodeString(FunctionName);
  EncCustom := EncodeString(Customization);
  Prefix := UBytesJoin(EncName, EncCustom);
  Prefix := BytePad(Prefix, Rate);
  Input := nil;
  SetLength(Input, Length(Prefix) + DataSize);
  Move(Prefix[0], Input[0], Length(Prefix));
  if DataSize > 0 then
  begin
    Move(Data^, Input[Length(Prefix)], DataSize);
  end;
  Result := KeccakSponge(@Input[0], Length(Input), OutputSize, SecLevel, $04);
end;

function UcSHAKE_128(
  const Data: Pointer;
  const DataSize, OutputSize: UInt32;
  const FunctionName, Customization: TUInt8Array
): TUInt8Array;
begin
  Result := cSHAKE(Data, DataSize, OutputSize, 16, FunctionName, Customization);
end;

function UcSHAKE_128(
  const Data: TUInt8Array;
  const OutputSize: UInt32;
  const FunctionName, Customization: TUInt8Array
): TUInt8Array;
begin
  Result := cSHAKE(@Data[0], Length(Data), OutputSize, 16, FunctionName, Customization);
end;

function UcSHAKE_128(
  const Data: String;
  const OutputSize: UInt32;
  const FunctionName, Customization: String
): TUInt8Array;
begin
  Result := cSHAKE(@Data[1], Length(Data), OutputSize, 16, UStrToBytes(FunctionName), UStrToBytes(Customization));
end;

function UcSHAKE_256(
  const Data: Pointer;
  const DataSize, OutputSize: UInt32;
  const FunctionName, Customization: TUInt8Array
): TUInt8Array;
begin
  Result := cSHAKE(Data, DataSize, OutputSize, 32, FunctionName, Customization);
end;

function UcSHAKE_256(
  const Data: TUInt8Array;
  const OutputSize: UInt32;
  const FunctionName, Customization: TUInt8Array
): TUInt8Array;
begin
  Result := cSHAKE(@Data[0], Length(Data), OutputSize, 32, FunctionName, Customization);
end;

function UcSHAKE_256(
  const Data: String;
  const OutputSize: UInt32;
  const FunctionName, Customization: String
): TUInt8Array;
begin
  Result := cSHAKE(@Data[1], Length(Data), OutputSize, 32, UStrToBytes(FunctionName), UStrToBytes(Customization));
end;

function UBLAKE3_Hash(const Data: TUInt8Array; const OutputSize: UInt32): TUInt8Array;
begin
  Result := TUBLAKE3.Hash(Data, OutputSize);
end;

function UBLAKE3_Hash(const Data: TUInt8Array; const Key: TUBLAKE3.TKey; const OutputSize: UInt32): TUInt8Array;
begin
  Result := TUBLAKE3.Hash(Data, Key, OutputSize);
end;

function UBLAKE3_KDF(
  const Context, Password: TUInt8Array;
  const OutputSize: UInt32
): TUInt8Array;
begin
  Result := TUBLAKE3.KDF(Context, Password, OutputSize);
end;

function UBLAKE3_KDF(
  const Context, Password: String;
  const OutputSize: UInt32
): TUInt8Array;
begin
  Result := UBLAKE3_KDF(UStrToBytes(Context), UStrToBytes(Password), OutputSize);
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
  UMove(Result, Digest[0], DigestSize);
end;

function UHMAC_SHA1(const Key, Data: TUInt8Array): TUDigestSHA1;
begin
  Result := specialize UHMAC<TUDigestSHA1>(Key, Data);
end;

function UHMAC_SHA2_256(const Key, Data: TUInt8Array): TUDigestSHA2_256;
begin
  Result := specialize UHMAC<TUDigestSHA2_256>(Key, Data);
end;

function UHMAC_SHA2_512(const Key, Data: TUInt8Array): TUDigestSHA2_512;
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
  Result := UHMAC_SHA2_256(Key, Data);
end;

function UAuthHMAC_SHA2_512(const Key, Data: TUInt8Array): TUInt8Array;
begin
  Result := UHMAC_SHA2_512(Key, Data);
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

function KMAC(
  const Data: Pointer;
  const DataSize: UInt32;
  const OutputSize: UInt32;
  const Key: TUInt8Array;
  const Customization: TUInt8Array;
  const SecLevel: UInt32
): TUInt8Array;
  var Rate: UInt32;
  var Input, PaddedKey, EncOutputSize: TUInt8Array;
begin
  Rate := 200 - (2 * SecLevel);
  PaddedKey := BytePad(EncodeString(Key), Rate);
  EncOutputSize := RightEncode(UInt64(OutputSize) * 8);
  Input := UBytesConcat([
    PaddedKey,
    UBytesMake(Data, DataSize),
    EncOutputSize
  ]);
  Result := cSHAKE(
    @Input[0], Length(Input),
    OutputSize, SecLevel,
    UStrToBytes('KMAC'), Customization
  );
end;

function UKMAC_128(
  const Data: Pointer;
  const DataSize: UInt32;
  const OutputSize: UInt32;
  const Key: TUInt8Array;
  const Customization: TUInt8Array
): TUInt8Array;
begin
  Result := KMAC(Data, DataSize, OutputSize, Key, Customization, 16);
end;

function UKMAC_128(
  const Data: TUInt8Array;
  const OutputSize: UInt32;
  const Key: TUInt8Array;
  const Customization: TUInt8Array
): TUInt8Array;
begin
  Result := KMAC(@Data[0], Length(Data), OutputSize, Key, Customization, 16);
end;

function UKMAC_128(
  const Data: String;
  const DataSize: UInt32;
  const OutputSize: UInt32;
  const Key: TUInt8Array;
  const Customization: TUInt8Array
): TUInt8Array;
begin
  Result := KMAC(@Data[1], Length(Data), OutputSize, Key, Customization, 16);
end;

function UKMAC_256(
  const Data: Pointer;
  const DataSize: UInt32;
  const OutputSize: UInt32;
  const Key: TUInt8Array;
  const Customization: TUInt8Array
): TUInt8Array;
begin
  Result := KMAC(Data, DataSize, OutputSize, Key, Customization, 32);
end;

function UKMAC_256(
  const Data: TUInt8Array;
  const OutputSize: UInt32;
  const Key: TUInt8Array;
  const Customization: TUInt8Array
): TUInt8Array;
begin
  Result := KMAC(@Data[0], Length(Data), OutputSize, Key, Customization, 32);
end;

function UKMAC_256(
  const Data: String;
  const DataSize: UInt32;
  const OutputSize: UInt32;
  const Key: TUInt8Array;
  const Customization: TUInt8Array
): TUInt8Array;
begin
  Result := KMAC(@Data[1], Length(Data), OutputSize, Key, Customization, 32);
end;

function USign_RSA_SHA256(const Data: TUInt8Array; const Key: TURSA.TKey): TUInt8Array;
begin
  Result := TURSA.Sign_SHA256(Data, Key);
end;

function USign_RSA_SHA512(const Data: TUInt8Array; const Key: TURSA.TKey): TUInt8Array;
begin
  Result := TURSA.Sign_SHA512(Data, Key);
end;

function UVerify_RSA(const Data, Signature: TUInt8Array; const Key: TURSA.TKey): Boolean;
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

function UPBKDF2_HMAC_SHA2_256(
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

function UPBKDF2_HMAC_SHA2_512(
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

function UEvpKDF_SHA2_256(
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

function UMGF1_SHA2_256(const Seed: TUInt8Array; const MaskLen: Int32): TUInt8Array;
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

function UExportRSAKey_PKCS8(const Key: TURSA.TKey; const Password: TUInt8Array; const Iterations: UInt32): String;
begin
  Result := TURSA.ExportKeyPrivateEncrypted_PKCS8_SHA256_AES256_CBC(Key, Password, Iterations);
end;

function UExportRSAKey_X509(const Key: TURSA.TKey): String;
begin
  Result := TURSA.ExportKeyPublic_X509(Key);
end;

function UImportRSAKey(const KeyASN1: String; const Password: String): TURSA.TKey;
begin
  Result := TURSA.ImportKey(KeyASN1, UStrToBytes(Password));
end;

function UImportRSAKey(const KeyASN1: String; const Password: TUInt8Array): TURSA.TKey;
begin
  Result := TURSA.ImportKey(KeyASN1, Password);
end;

function UEncrypt_RSA_PKCS1(
  const Data: Pointer;
  const DataSize: UInt32;
  const Key: TURSA.TKey
): TURSA.TBigInt;
begin
  Result := TURSA.Encrypt_PKCS1(Data, DataSize, Key);
end;

function UEncrypt_RSA_PKCS1(
  const Data: TUInt8Array;
  const Key: TURSA.TKey
): TURSA.TBigInt;
begin
  Result := TURSA.Encrypt_PKCS1(@Data[0], Length(Data), Key);
end;

function UEncrypt_RSA_PKCS1_Str(
  const Str: String;
  const Key: TURSA.TKey
): TURSA.TBigInt;
begin
  Result := UEncrypt_RSA_PKCS1(@Str[1], Length(Str), Key);
end;

function UDecrypt_RSA_PKCS1_Str(
  const Cipher: TURSA.TBigInt;
  const Key: TURSA.TKey
): String;
begin
  Result := TURSA.Decrypt_PKCS1_Str(Cipher, Key);
end;

function UDecrypt_RSA_PKCS1(
  const Cipher: TURSA.TBigInt;
  const Key: TURSA.TKey
): TUInt8Array;
begin
  Result := TURSA.Decrypt_PKCS1(Cipher, Key);
end;

function UEncrypt_RSA_OAEP(
  const Data: Pointer;
  const DataSize: UInt32;
  const Key: TURSA.TKey
): TURSA.TBigInt;
begin
  Result := TURSA.Encrypt_OAEP(Data, DataSize, Key);
end;

function UEncrypt_RSA_OAEP(
  const Data: TUInt8Array;
  const Key: TURSA.TKey
): TURSA.TBigInt;
begin
  Result := TURSA.Encrypt_OAEP(@Data[0], Length(Data), Key);
end;

function UEncrypt_RSA_OAEP_Str(
  const Str: String;
  const Key: TURSA.TKey
): TURSA.TBigInt;
begin
  Result := TURSA.Encrypt_OAEP(@Str[1], Length(Str), Key);
end;

function UDecrypt_RSA_OAEP_Str(
  const Cipher: TURSA.TBigInt;
  const Key: TURSA.TKey
): String;
begin
  Result := TURSA.Decrypt_OAEP_Str(Cipher, Key);
end;

function UDecrypt_RSA_OAEP(
  const Cipher: TURSA.TBigInt;
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
  var i: Int32;
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

class function TUECC.Weierstrass.TPoint.AtInfinity: TPoint;
begin
  Result.x := TBigInt.Zero;
  Result.y := TBigInt.Zero;
end;

function TUECC.Weierstrass.TPoint.IsAtInfinity: Boolean;
begin
  Result := (x = TBigInt.Zero) and (y = TBigInt.Zero);
end;

class operator TUECC.Weierstrass.TPoint.=(const a, b: TPoint): Boolean;
begin
  Result := (a.x = b.x) and (a.y = b.y);
end;

function TUECC.Weierstrass.TCurve.IsOnCurve(const Point: TPoint): Boolean;
  var LeftSide, RightSide: TBigInt;
begin
  if Point.IsAtInfinity then Exit(True);
  LeftSide := (Point.y * Point.y) mod p;
  RightSide := (Point.x * Point.x) mod p;
  RightSide := (RightSide * Point.x) mod p;
  RightSide := (RightSide + (a * Point.x)) mod p;
  RightSide := (RightSide + b) mod p;
  Result := (LeftSide = RightSide);
end;

class function TUECC.Weierstrass.TCurve.Make_SECP256R1: TCurve;
begin
  Result.p := '$ffffffff00000001000000000000000000000000ffffffffffffffffffffffff';
  Result.a := '$ffffffff00000001000000000000000000000000fffffffffffffffffffffffc';
  Result.b := '$5ac635d8aa3a93e7b3ebbd55769886bc651d06b0cc53b0f63bce3c3e27d2604b';
  Result.n := '$ffffffff00000000ffffffffffffffffbce6faada7179e84f3b9cac2fc632551';
  Result.g.x := '$6b17d1f2e12c4247f8bce6e563a440f277037d812deb33a0f4a13945d898c296';
  Result.g.y := '$4fe342e2fe1a7f9b8ee7eb4a7c0f9e162bce33576b315ececbb6406837bf51f5';
end;

class function TUECC.Weierstrass.TCurve.Make_SECP256K1: TCurve;
begin
  Result.p := '$fffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2f';
  Result.a := 0;
  Result.b := 7;
  Result.n := '$fffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141';
  Result.g.x := '$79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798';
  Result.g.y := '$483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8';
end;

function TUECC.Weierstrass.TKey.IsValid: Boolean;
begin
  Result := d.IsValid and not q.IsAtInfinity;
end;

function TUECC.Weierstrass.TSignature.IsValid: Boolean;
begin
  Result := (r > TBigInt.Zero) and (s > TBigInt.Zero);
end;

function TUECC.Weierstrass.TSignature.IsValid(const n: TBigInt): Boolean;
begin
  Result := (r > TBigInt.Zero) and (s > TBigInt.Zero) and (r < n) and (s < n);
end;

class function TUECC.Weierstrass.PointAdd(const Curve: TCurve; const a, b: TPoint): TPoint;
  var Lambda, Temp: TBigInt;
  var x3, y3: TBigInt;
  var Two, Three: TBigInt;
begin
  if a.IsAtInfinity then Exit(b);
  if b.IsAtInfinity then Exit(a);
  if a.x = b.x then
  begin
    if a.y = b.y then
    begin
      Temp := a.y mod Curve.p;
      if Temp = 0 then Exit(TPoint.AtInfinity);
      Three := 3;
      Temp := (a.x * a.x) mod Curve.p;
      Temp := (Three * Temp) mod Curve.p;
      Temp := (Temp + Curve.A) mod Curve.p;
      Two := 2;
      Lambda := (Two * a.y) mod Curve.p;
      Lambda := TBigInt.ModInv(Lambda, Curve.p);
      Lambda := (Temp * Lambda) mod Curve.p;
    end
    else
    begin
      Exit(TPoint.AtInfinity);
    end;
  end
  else
  begin
    Temp := (b.y - a.y + Curve.p) mod Curve.p;
    Lambda := (b.x - a.x + Curve.p) mod Curve.p;
    Lambda := TBigInt.ModInv(Lambda, Curve.p);
    Lambda := (Temp * Lambda) mod Curve.p;
  end;
  x3 := (Lambda * Lambda) mod Curve.p;
  x3 := (x3 - a.x + Curve.p) mod Curve.p;
  x3 := (x3 - b.x + Curve.p) mod Curve.p;
  y3 := (a.x - x3 + Curve.p) mod Curve.p;
  y3 := (Lambda * y3) mod Curve.p;
  y3 := (y3 - a.y + Curve.p) mod Curve.p;
  Result.x := x3;
  Result.y := y3;
end;

class function TUECC.Weierstrass.PointMultiply(
  const Curve: TCurve;
  const a: TPoint;
  const b: TBigInt
): TPoint;
  var R: TPoint;  // Result accumulator
  var Q: TPoint;  // Current power of point
  var Temp: TPoint;
  var k: TBigInt;
  var i, j: Int32;
  var Bit: UInt32;
begin
  if a.IsAtInfinity then Exit(TPoint.AtInfinity);
  if b = TBigInt.Zero then Exit(TPoint.AtInfinity);
  k := b mod Curve.n;
  R := TPoint.AtInfinity;
  Q := a;
  for i := 0 to k.Top do
  begin
    Bit := k[i];
    for j := 0 to 31 do
    begin
      Temp := PointAdd(Curve, R, Q);
      if (Bit and 1) = 1 then R := Temp;
      if not ((i = 15) and (j = 31)) then
      begin
        Q := PointAdd(Curve, Q, Q);
      end;
      Bit := Bit shr 1;
    end;
  end;
  Result := R;
end;

class function TUECC.Weierstrass.MakeKey(const Curve: TCurve): TKey;
begin
  repeat
    Result.d := TBigInt.MakeRandomRange(TBigInt.One, Curve.n - TBigInt.One);
    Result.q := PointMultiply(Curve, Curve.g, Result.d);
  until not Result.q.IsAtInfinity and Curve.IsOnCurve(Result.q);
end;

class function TUECC.Weierstrass.DerivePublicKey(
  const Curve: TCurve;
  const PrivateKey: TBigInt
): TPoint;
begin
  Result := PointMultiply(Curve, Curve.g, PrivateKey);
end;

class function TUECC.Weierstrass.DeriveKey(
  const Curve: TCurve;
  const BaseData, Context: TUInt8Array
): TKey;
  var PrivateKey: TBigInt;
begin
  PrivateKey := TBigInt.Make(
    UBytesReverse(
      UBLAKE3_KDF(Context, BaseData, 32)
    )
  );
  Result.d := (PrivateKey mod (Curve.n - TBigInt.One)) + TBigInt.One;
  Result.q := PointMultiply(Curve, Curve.g, Result.d);
end;

class function TUECC.Weierstrass.Sign(const Curve: TCurve; const PrivateKey: TBigInt;
  const MessageHash: TBigInt): TSignature;
  function GenerateK: TBigInt;
    function FixedSizeBytes(
      const Num: TBigInt;
      const NumBytes: UInt32
    ): TUInt8Array;
      var Bytes: TUInt8Array;
      var PadCount: UInt32;
    begin
      Bytes := Num.ToBytesBE;
      PadCount := NumBytes - Length(Bytes);
      Result := nil;
      SetLength(Result, NumBytes);
      FillChar(Result[0], PadCount, 0);
      Move(Bytes[0], Result[PadCount], Length(Bytes));
    end;
    var K, V, T: TUInt8Array;
    var RLen, HashLen: Int32;
    var DBytes, ZBytes, TempBytes: TUInt8Array;
    var KCandidate: TBigInt;
  begin
    HashLen := SizeOf(TUDigestSHA2_256);
    RLen := Curve.n.BytesUsed;
    DBytes := FixedSizeBytes(PrivateKey, RLen);
    ZBytes := FixedSizeBytes(MessageHash mod Curve.n, RLen);
    K := nil;
    SetLength(K, HashLen);
    FillChar(K[0], HashLen, 0);
    V := nil;
    SetLength(V, HashLen);
    FillChar(V[0], HashLen, 1);
    TempBytes := UBytesConcat([V, [0], DBytes, ZBytes]);
    K := UHMAC_SHA2_256(K, TempBytes);
    V := UHMAC_SHA2_256(K, V);
    TempBytes := UBytesConcat([V, [1], DBytes, ZBytes]);
    K := UHMAC_SHA2_256(K, TempBytes);
    V := UHMAC_SHA2_256(K, V);
    while True do
    begin
      T := nil;
      while Length(T) < RLen do
      begin
        V := UHMAC_SHA2_256(K, V);
        T := UBytesJoin(T, V);
      end;
      SetLength(T, RLen);
      KCandidate := TBigInt.Make(UBytesReverse(T));
      if (KCandidate > TBigInt.Zero) and (KCandidate < Curve.n) then
      begin
        Exit(KCandidate);
      end;
      TempBytes := UBytesJoin(V, [0]);
      K := UHMAC_SHA2_256(K, TempBytes);
      V := UHMAC_SHA2_256(K, V);
    end;
  end;
  var k, kInv, r, s: TBigInt;
  var kG: TPoint;
  var n: TBigInt;
  var z: TBigInt;
begin
  Result.r := TBigInt.Zero;
  Result.s := TBigInt.Zero;
  if (PrivateKey <= TBigInt.Zero) or (PrivateKey >= Curve.n) then Exit;
  n := Curve.n;
  z := MessageHash mod Curve.n;
  k := GenerateK;
  kG := PointMultiply(Curve, Curve.g, k);
  if kG.IsAtInfinity then Exit;
  r := kG.x mod n;
  kInv := TBigInt.ModInv(k, n);
  s := (r * PrivateKey) mod n;
  s := (z + s) mod n;
  s := (kInv * s) mod n;
  if s > (n div 2) then s := n - s;
  Result.r := r;
  Result.s := s;
end;

class function TUECC.Weierstrass.Verify(
  const Curve: TCurve;
  const PublicKey: TPoint;
  const MessageHash: TBigInt;
  const Signature: TSignature
): Boolean;
  var r: TBigInt absolute Signature.r;
  var s: TBigInt absolute Signature.s;
  var n: TBigInt absolute Curve.n;
  var z, sInv, u1, u2, x1: TBigInt;
  var P1, P2, PF: TPoint;
begin
  if not Signature.IsValid(n) then Exit(False);
  if PublicKey.IsAtInfinity then Exit(False);
  if not Curve.IsOnCurve(PublicKey) then Exit(False);
  if s > (n div 2) then Exit(False);
  z := MessageHash mod Curve.n;
  sInv := TBigInt.ModInv(s, n);
  u1 := (z * sInv) mod n;
  u2 := (r * sInv) mod n;
  P1 := PointMultiply(Curve, Curve.g, u1);
  P2 := PointMultiply(Curve, PublicKey, u2);
  PF := PointAdd(Curve, P1, P2);
  if PF.IsAtInfinity then Exit(False);
  x1 := PF.x mod n;
  Result := (x1 = r);
end;

class function TUECC.Weierstrass.SharedKey(
  const Curve: TCurve;
  const PublicKey: TPoint;
  const PrivateKey: TBigInt
): TBigInt;
  var SharedPoint: TPoint;
begin
  if PublicKey.IsAtInfinity then Exit(TBigInt.Invalid);
  if not Curve.IsOnCurve(PublicKey) then Exit(TBigInt.Invalid);
  if (PrivateKey <= TBigInt.Zero) or (PrivateKey >= Curve.n) then Exit(TBigInt.Invalid);
  SharedPoint := PointMultiply(Curve, PublicKey, PrivateKey);
  if SharedPoint.IsAtInfinity then Exit(TBigInt.Invalid);
  Result := SharedPoint.x;
end;

class constructor TUECC.Weierstrass.CreateClass;
begin
  Curve_SECP256R1 := TCurve.Make_SECP256R1;
  Curve_SECP256K1 := TCurve.Make_SECP256K1;
end;

function TUECC.Montgomery.TCurve.Add(const v1, v2: TBigInt): TBigInt;
begin
  Result := (v1 + v2) mod p;
end;

function TUECC.Montgomery.TCurve.Sub(const v1, v2: TBigInt): TBigInt;
begin
  Result := (v1 - v2 + p) mod p;
end;

function TUECC.Montgomery.TCurve.Mul(const v1, v2: TBigInt): TBigInt;
begin
  Result := (v1 * v2) mod p;
end;

function TUECC.Montgomery.TCurve.Inv(const v: TBigInt): TBigInt;
begin
  Result := TBigInt.ModPow(v, (p - 2), p);
end;

function TUECC.Montgomery.TKey.IsValid: Boolean;
begin
  Result := d.IsValid and q.IsValid;
end;

class function TUECC.Montgomery.IsLowOrderPoint(const p: TBigInt): Boolean;
  var i: Int32;
begin
  for i := 0 to High(LowOrderPoints) do
  if p = LowOrderPoints[i] then
  begin
    Exit(True);
  end;
  Result := False;
end;

class function TUECC.Montgomery.ScalarClamp(const k: TBigInt): TBigInt;
begin
  Result := k;
  Result.Data[0] := k.Data[0] and $fffffff8;
  Result.ClearBit(255);
  Result.SetBit(254);
end;

class function TUECC.Montgomery.ScalarMultiply(
  const Curve: TCurve;
  const k, u: TBigInt
): TBigInt;
  procedure CSwap(var a, b: TBigInt; const Swap: Boolean);
    var Mask: TBigInt;
    var Temp: TBigInt;
  begin
    if Swap then
    begin
      Mask := TBigInt.MaxValue;
    end
    else
    begin
      Mask := TBigInt.Zero;
    end;
    Temp := a xor b;
    Temp := Temp and Mask;
    a := a xor Temp;
    b := b xor Temp;
  end;
  var x1, x2, z2, x3, z3: TBigInt;
  var Tmp0, Tmp1: TBigInt;
  var A, AA, B, BB, E, C, D, DA, CB: TBigInt;
  var Swap, Bit: Boolean;
  var i: Int32;
  var a24: TBigInt;
begin
  a24 := 121665;
  x1 := u;
  x2 := TBigInt.One;
  z2 := TBigInt.Zero;
  x3 := u;
  z3 := TBigInt.One;
  Swap := False;
  for i := 254 downto 0 do
  begin
    Bit := k.GetBit(i);
    Swap := Swap xor Bit;
    CSwap(x2, x3, Swap);
    CSwap(z2, z3, Swap);
    Swap := Bit;
    A := Curve.Add(x2, z2);
    AA := Curve.Mul(A, A);
    B := Curve.Sub(x2, z2);
    BB := Curve.Mul(B, B);
    E := Curve.Sub(AA, BB);
    C := Curve.Add(x3, z3);
    D := Curve.Sub(x3, z3);
    DA := Curve.Mul(D, A);
    CB := Curve.Mul(C, B);
    Tmp0 := Curve.Add(DA, CB);
    x3 := Curve.Mul(Tmp0, Tmp0);
    Tmp0 := Curve.Sub(DA, CB);
    Tmp1 := Curve.Mul(Tmp0, Tmp0);
    z3 := Curve.Mul(x1, Tmp1);
    x2 := Curve.Mul(AA, BB);
    Tmp0 := Curve.Mul(a24, E);
    Tmp1 := Curve.Add(AA, Tmp0);
    z2 := Curve.Mul(E, Tmp1);
  end;
  CSwap(x2, x3, Swap);
  CSwap(z2, z3, Swap);
  Result := Curve.Mul(x2, Curve.Inv(z2));
end;

class function TUECC.Montgomery.X25519(
  const Curve: TCurve;
  const k, u: TBigInt
): TBigInt;
begin
  Result := ScalarMultiply(Curve, ScalarClamp(k), u);
end;

class function TUECC.Montgomery.MakeKey(const Curve: TCurve): TKey;
begin
  repeat
    Result.d := ScalarClamp(TBigInt.Make(USysRandom(32)));
    Result.q := ScalarMultiply(Curve, Result.d, Curve.u);
  until not IsLowOrderPoint(Result.q);
end;

class function TUECC.Montgomery.DerivePublicKey(
  const Curve: TCurve;
  const PrivateKey: TBigInt
): TBigInt;
begin
  Result := X25519(Curve, PrivateKey, Curve.u);
end;

class function TUECC.Montgomery.SharedKey(
  const Curve: TCurve;
  const PublicKey: TBigInt;
  const PrivateKey: TBigInt
): TBigInt;
begin
  if not PublicKey.IsValid then Exit(TBigInt.Invalid);
  if not PrivateKey.IsValid then Exit(TBigInt.Invalid);
  if (PublicKey < TBigInt.Zero) then Exit(TBigInt.Invalid);
  if (PublicKey >= Curve.p) then Exit(TBigInt.Invalid);
  if (PrivateKey < TBigInt.Zero) then Exit(TBigInt.Invalid);
  if IsLowOrderPoint(PublicKey) then Exit(TBigInt.Invalid);
  Result := X25519(Curve, PrivateKey, PublicKey);
  if Result.IsZero then Exit(TBigInt.Invalid);
end;

class constructor TUECC.Montgomery.CreateClass;
begin
  Curve_25519.p := '$7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffed';
  Curve_25519.a := 486662;
  Curve_25519.b := 1;
  Curve_25519.u := 9;
  Curve_25519.n := '$1000000000000000000000000000000014def9dea2f79cd65812631a5cf5d3ed';
  Curve_25519.h := 8;
  LowOrderPoints[0] := TBigInt.Zero;
  LowOrderPoints[1] := TBigInt.One;
  LowOrderPoints[2] := '$5f9c95bcbca5804c120b1d5bc9835efb04445cc4581c8e86d8224eddd09f1157';
  LowOrderPoints[3] := '$e0eb7a7c3b41b8ae1656e3faf19fc46ada098deb9c32b1fd866205165f49b800';
  LowOrderPoints[4] := '$ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f';
end;

class function TUECC.Edwards.TPoint.Neutral: TPoint;
begin
  Result.x := TBigInt.Zero;
  Result.y := TBigInt.One;
  Result.z := TBigInt.One;
  Result.t := TBigInt.Zero;
end;

function TUECC.Edwards.TPoint.IsNeutral: Boolean;
begin
  Result := (x.IsZero) and (y = z) and (t.IsZero);
end;

class function TUECC.Edwards.TPoint.Invalid: TPoint;
begin
  Result.x := TBigInt.Invalid;
  Result.y := TBigInt.Invalid;
  Result.z := TBigInt.Invalid;
  Result.t := TBigInt.Invalid;
end;

function TUECC.Edwards.TPoint.IsValid: Boolean;
begin
  Result := x.IsValid and y.IsValid and z.IsValid and t.IsValid;
end;

class operator TUECC.Edwards.TPoint.=(const a, b: TPoint): Boolean;
begin
  Result := (a.x = b.x) and (a.y = b.y) and (a.z = b.z) and (a.t = b.t);
end;

function TUECC.Edwards.TCurve.IsOnCurve(const Point: TPoint): Boolean;
  var Affine: TPoint;
  var lhs, rhs, x2, y2, x2y2: TBigInt;
begin
  Affine := ToAffine(Point);
  x2 := Mul(Affine.x, Affine.x);
  y2 := Mul(Affine.y, Affine.y);
  x2y2 := Mul(x2, y2);
  lhs := Sub(y2, x2);
  rhs := Mul(d, x2y2);
  rhs := Add(rhs, TBigInt.One);
  Result := lhs = rhs;
end;

function TUECC.Edwards.TCurve.ToAffine(const Point: TPoint): TPoint;
  var InvZ: TBigInt;
begin
  if not Point.IsValid then Exit(TPoint.Invalid);
  if Point.z = TBigInt.Zero then Exit(TPoint.Invalid);
  InvZ := Inv(Point.z);
  Result.x := Mul(Point.x, InvZ);
  Result.y := Mul(Point.y, InvZ);
  Result.z := TBigInt.One;
  Result.t := Mul(Result.x, Result.y);
end;

function TUECC.Edwards.TCurve.Add(const v1, v2: TBigInt): TBigInt;
begin
  Result := (v1 + v2) mod p;
end;

function TUECC.Edwards.TCurve.Sub(const v1, v2: TBigInt): TBigInt;
begin
  Result := (v1 - v2 + p) mod p;
end;

function TUECC.Edwards.TCurve.Mul(const v1, v2: TBigInt): TBigInt;
begin
  Result := (v1 * v2) mod p;
end;

function TUECC.Edwards.TCurve.Inv(const v: TBigInt): TBigInt;
begin
  Result := TBigInt.ModPow(v, (p - 2), p);
end;

function TUECC.Edwards.TKey.IsValid: Boolean;
  function IsAllZero(const Arr: array of UInt8): Boolean;
    var i: Int32;
  begin
    for i := 0 to High(Arr) do
    if Arr[i] > 0 then
    begin
      Exit(False);
    end;
    Result := True;
  end;
begin
  Result := not IsAllZero(d) and not IsAllZero(q);
end;

class function TUECC.Edwards.TKey.Invalid: TKey;
begin
  UClear(Result, SizeOf(Result));
end;

function TUECC.Edwards.TSignature.IsValid(const Curve: TCurve): Boolean;
  var i: Int32;
begin
  if not s.IsValid then Exit(False);
  if s >= Curve.n then Exit(False);
  for i := 0 to High(r) do if r[i] > 0 then Exit(True);
  Result := False;
end;

class function TUECC.Edwards.TSignature.Invalid: TSignature;
begin
  UClear(Result.r, SizeOf(Result.r));
  Result.s := TBigInt.Invalid;
end;

function TUECC.Edwards.TSignature.ToHex: String;
begin
  Result := UBytesToHexLC(UBytesJoin(r, s.ToBytes));
end;

class procedure TUECC.Edwards.Clamp(
  const Bytes: Pointer;
  const First, Last: UInt32
);
begin
  PUInt8Arr(Bytes)^[First] := PUInt8Arr(Bytes)^[First] and $f8;
  PUInt8Arr(Bytes)^[Last] := (PUInt8Arr(Bytes)^[Last] and $7f) or $40;
end;

class function TUECC.Edwards.PointAdd(
  const Curve: TCurve;
  const p, q: TPoint
): TPoint;
  var A, B, C, D, E, F, G, H: TBigInt;
begin
  A := Curve.Mul(P.x, Q.x);
  B := Curve.Mul(P.y, Q.y);
  C := Curve.Mul(P.t, Q.t);
  C := Curve.Mul(Curve.d, C);
  D := Curve.Mul(P.z, Q.z);
  E := Curve.Add(P.x, P.y);
  E := Curve.Mul(E, Curve.Add(Q.x, Q.y));
  E := Curve.Sub(E, A);
  E := Curve.Sub(E, B);
  F := Curve.Sub(D, C);
  G := Curve.Add(D, C);
  H := Curve.Mul(Curve.a, A);
  H := Curve.Sub(B, H);
  Result.x := Curve.Mul(E, F);
  Result.y := Curve.Mul(G, H);
  Result.z := Curve.Mul(F, G);
  Result.t := Curve.Mul(E, H);
end;

class function TUECC.Edwards.PointDouble(
  const Curve: TCurve;
  const p: TPoint
): TPoint;
  var A, B, C, D, E, F, H, G: TBigInt;
begin
  A := Curve.Mul(P.x, P.x);
  B := Curve.Mul(P.y, P.y);
  C := Curve.Mul(P.z, P.z);
  C := Curve.Add(C, C);
  D := Curve.Mul(Curve.a, A);
  E := Curve.Add(P.x, P.y);
  E := Curve.Mul(E, E);
  E := Curve.Sub(E, A);
  E := Curve.Sub(E, B);
  G := Curve.Add(D, B);
  F := Curve.Sub(G, C);
  H := Curve.Sub(D, B);
  Result.x := Curve.Mul(E, F);
  Result.y := Curve.Mul(G, H);
  Result.z := Curve.Mul(F, G);
  Result.t := Curve.Mul(E, H);
end;

class function TUECC.Edwards.ScalarMultiply(
  const Curve: TCurve;
  const k: TBigInt;
  const p: TPoint
): TPoint;
  var R0, R1: TPoint;
  var i: Int32;
  var TempK: TBigInt;
begin
  TempK := k mod Curve.n;
  R0 := TPoint.Neutral;
  R1 := P;
  for i := 254 downto 0 do
  begin
    if TempK.GetBit(i) then
    begin
      R0 := PointAdd(Curve, R0, R1);
      R1 := PointDouble(Curve, R1);
    end
    else
    begin
      R1 := PointAdd(Curve, R0, R1);
      R0 := PointDouble(Curve, R0);
    end;
  end;
  Result := R0;
end;

class function TUECC.Edwards.PointCompress(
  const Curve: TCurve;
  const p: TPoint
): TPointCompressed;
  var Affine: TPoint;
  var BytesY: TUInt8Array;
  var i: Int32;
begin
  Affine := Curve.ToAffine(p);
  BytesY := Affine.y.ToBytes;
  UInit(Result[0], BytesY[0], UMin(Length(BytesY), SizeOf(Result)));
  for i := Length(BytesY) to High(Result) do Result[i] := 0;
  if Affine.x.GetBit(0) then
  begin
    Result[31] := Result[31] or $80;
  end
  else
  begin
    Result[31] := Result[31] and $7F;
  end;
end;

class function TUECC.Edwards.PointDecompress(
  const Curve: TCurve;
  const pc: TPointCompressed
): TPoint;
var
  BytesY: TUInt8Array;
  y, x, x2, y2, u, v, v3, v7, Candidate: TBigInt;
  SignBit: Boolean;
  Five, Eight: TBigInt;
begin
  SignBit := (pc[31] and $80) > 0;
  BytesY := pc;
  BytesY[31] := BytesY[31] and $7f;
  y := BytesY;
  if y >= Curve.p then Exit(TPoint.Invalid);
  y2 := Curve.Mul(y, y);
  u := Curve.Sub(y2, TBigInt.One);
  v := Curve.Mul(Curve.d, y2);
  v := Curve.Add(v, TBigInt.One);
  v3 := Curve.Mul(Curve.Mul(v, v), v);
  v7 := Curve.Mul(Curve.Mul(v3, v3), v);
  Five := 5;
  Eight := 8;
  Candidate := Curve.Mul(u, v7);
  Candidate := TBigInt.ModPow(Candidate, (Curve.p - Five) div Eight, Curve.p);
  x := Curve.Mul(Curve.Mul(u, v3), Candidate);
  x2 := Curve.Mul(x, x);
  if Curve.Mul(v, x2) <> u then
  begin
    x := Curve.Mul(x, Curve.SqrtM1);
    x2 := Curve.Mul(x, x);
    if Curve.Mul(v, x2) <> u then Exit(TPoint.Invalid);
  end;
  if x.GetBit(0) <> SignBit then
  begin
    x := Curve.Sub(TBigInt.Zero, x);
  end;
  Result.x := x;
  Result.y := y;
  Result.z := TBigInt.One;
  Result.t := Curve.Mul(x, y);
end;

class function TUECC.Edwards.MakeKey(
  const Curve: TCurve;
  const Seed: array of UInt8
): TKey;
  var Hash: TUDigestSHA2_512;
  var Scalar: TBigInt;
  var ScalarBytes: TUInt8Array;
  var PublicPoint: TPoint;
begin
  if Length(Seed) < 32 then Exit(TKey.Invalid);
  UInit(Result.d, Seed[0], SizeOf(Result.d));
  Hash := USHA2_512(Seed);
  ScalarBytes := TUInt8Array.Make(@Hash[0], 32);
  Clamp(@ScalarBytes[0], 0, 31);
  Scalar := ScalarBytes;
  PublicPoint := ScalarMultiply(Curve, Scalar, Curve.b);
  Result.q := PointCompress(Curve, PublicPoint);
end;

class function TUECC.Edwards.MakeKey(const Curve: TCurve): TKey;
begin
  Result := MakeKey(Curve, USysRandom(32));
end;

class function TUECC.Edwards.Sign_Ed25519(
  const Curve: TCurve;
  const Key: TKey;
  const Message: TUInt8Array
): TSignature;
  var Hash: TUDigestSHA2_512;
  var ScalarBytes: TUInt8Array;
  var a: TBigInt;
  var Prefix: TUInt8Array;
  var Nonce: TBigInt;
  var R: TPoint;
  var Hram: TBigInt;
  var S: TBigInt;
  var Temp: TUInt8Array;
begin
  if not Key.IsValid then Exit(TSignature.Invalid);
  Hash := USHA2_512(Key.d);
  ScalarBytes := TUInt8Array.Make(@Hash[0], 32);
  Clamp(@ScalarBytes[0], 0, 31);
  a := ScalarBytes;
  Prefix := TUInt8Array.Make(@Hash[32], 32);
  Temp := UBytesConcat([Prefix, Message]);
  Nonce := TBigInt.Make(USHA2_512(Temp)) mod Curve.n;
  R := ScalarMultiply(Curve, Nonce, Curve.b);
  Result.r := PointCompress(Curve, R);
  Temp := UBytesConcat([Result.r, Key.q, Message]);
  Hram := TBigInt.Make(USHA2_512(Temp)) mod Curve.n;
  S := (Hram * a) mod Curve.n;
  S := (Nonce + S) mod Curve.n;
  Result.s := S;
end;

class function TUECC.Edwards.Verify_Ed25519(
  const Curve: TCurve;
  const PublicKey: TPointCompressed;
  const Message: TUInt8Array;
  const Signature: TSignature
): Boolean;
  var A: TPoint;
  var R: TPoint;
  var Hram: TBigInt;
  var SB: TPoint;
  var HramA: TPoint;
  var RHramA: TPoint;
  var AffineSB, AaffineRHram: TPoint;
  var Temp: TUInt8Array;
begin
  if not Signature.IsValid(Curve) then Exit(False);
  A := PointDecompress(Curve, PublicKey);
  if not A.IsValid then Exit(False);
  R := PointDecompress(Curve, Signature.r);
  if not R.IsValid then Exit(False);
  Temp := UBytesConcat([Signature.r, PublicKey, Message]);
  Hram := TBigInt.Make(USHA2_512(Temp)) mod Curve.n;
  SB := ScalarMultiply(Curve, Signature.s, Curve.b);
  HramA := ScalarMultiply(Curve, Hram, A);
  RHramA := PointAdd(Curve, R, HramA);
  AffineSB := Curve.ToAffine(SB);
  AaffineRHram := Curve.ToAffine(RHramA);
  Result := AffineSB = AaffineRHram;
end;

class function TUECC.Edwards.MakeKey_BLAKE3(
  const Curve: TCurve;
  const Seed: array of UInt8
): TKey;
  var Hash: TUInt8Array;
  var Scalar: TBigInt;
  var ScalarBytes: TUInt8Array;
  var PublicPoint: TPoint;
begin
  if Length(Seed) < 32 then Exit(TKey.Invalid);
  UInit(Result.d, Seed[0], SizeOf(Result.d));
  Hash := UBLAKE3_Hash(Seed, 64);
  ScalarBytes := TUInt8Array.Make(@Hash[0], 32);
  Clamp(@ScalarBytes[0], 0, 31);
  Scalar := ScalarBytes;
  PublicPoint := ScalarMultiply(Curve, Scalar, Curve.b);
  Result.q := PointCompress(Curve, PublicPoint);
end;

class function TUECC.Edwards.MakeKey_BLAKE3(const Curve: TCurve): TKey;
begin
  Result := MakeKey_BLAKE3(Curve, USysRandom(32));
end;

class function TUECC.Edwards.Sign_Ed25519_BLAKE3(
  const Curve: TCurve;
  const Key: TKey;
  const Message: TUInt8Array
): TSignature;
  var Hash: TUInt8Array;
  var ScalarBytes: TUInt8Array;
  var a: TBigInt;
  var Prefix: TUInt8Array;
  var Nonce: TBigInt;
  var R: TPoint;
  var Hram: TBigInt;
  var S: TBigInt;
  var Temp: TUInt8Array;
begin
  if not Key.IsValid then Exit(TSignature.Invalid);
  Hash := UBLAKE3_Hash(Key.d, 64);
  ScalarBytes := TUInt8Array.Make(@Hash[0], 32);
  Clamp(@ScalarBytes[0], 0, 31);
  a := ScalarBytes;
  Prefix := TUInt8Array.Make(@Hash[32], 32);
  Temp := UBytesConcat([Prefix, Message]);
  Nonce := TBigInt.Make(UBLAKE3_Hash(Temp, 64)) mod Curve.n;
  R := ScalarMultiply(Curve, Nonce, Curve.b);
  Result.r := PointCompress(Curve, R);
  Temp := UBytesConcat([Result.r, Key.q, Message]);
  Hram := TBigInt.Make(UBLAKE3_Hash(Temp, 64)) mod Curve.n;
  S := (Hram * a) mod Curve.n;
  S := (Nonce + S) mod Curve.n;
  Result.s := S;
end;

class function TUECC.Edwards.Verify_Ed25519_BLAKE3(
  const Curve: TCurve;
  const PublicKey: TPointCompressed;
  const Message: TUInt8Array;
  const Signature: TSignature
): Boolean;
  var A: TPoint;
  var R: TPoint;
  var Hram: TBigInt;
  var SB: TPoint;
  var HramA: TPoint;
  var RHramA: TPoint;
  var AffineSB, AaffineRHram: TPoint;
  var Temp: TUInt8Array;
begin
  if not Signature.IsValid(Curve) then Exit(False);
  A := PointDecompress(Curve, PublicKey);
  if not A.IsValid then Exit(False);
  R := PointDecompress(Curve, Signature.r);
  if not R.IsValid then Exit(False);
  Temp := UBytesConcat([Signature.r, PublicKey, Message]);
  Hram := TBigInt.Make(UBLAKE3_Hash(Temp, 64)) mod Curve.n;
  SB := ScalarMultiply(Curve, Signature.s, Curve.b);
  HramA := ScalarMultiply(Curve, Hram, A);
  RHramA := PointAdd(Curve, R, HramA);
  AffineSB := Curve.ToAffine(SB);
  AaffineRHram := Curve.ToAffine(RHramA);
  Result := AffineSB = AaffineRHram;
end;

class function TUECC.Edwards.MakeKey_SHAKE(
  const Curve: TCurve;
  const Seed: array of UInt8
): TKey;
  var Hash: TUInt8Array;
  var Scalar: TBigInt;
  var ScalarBytes: TUInt8Array;
  var PublicPoint: TPoint;
begin
  if Length(Seed) < 32 then Exit(TKey.Invalid);
  UInit(Result.d, Seed[0], SizeOf(Result.d));
  Hash := USHAKE_256(Seed, 64);
  ScalarBytes := TUInt8Array.Make(@Hash[0], 32);
  Clamp(@ScalarBytes[0], 0, 31);
  Scalar := ScalarBytes;
  PublicPoint := ScalarMultiply(Curve, Scalar, Curve.b);
  Result.q := PointCompress(Curve, PublicPoint);
end;

class function TUECC.Edwards.MakeKey_SHAKE(const Curve: TCurve): TKey;
begin
  Result := MakeKey_SHAKE(Curve, USysRandom(32));
end;

class function TUECC.Edwards.Sign_Ed25519_SHAKE(
  const Curve: TCurve;
  const Key: TKey;
  const Message: TUInt8Array
): TSignature;
  var Hash: TUInt8Array;
  var ScalarBytes: TUInt8Array;
  var a: TBigInt;
  var Prefix: TUInt8Array;
  var Nonce: TBigInt;
  var R: TPoint;
  var Hram: TBigInt;
  var S: TBigInt;
  var Temp: TUInt8Array;
begin
  if not Key.IsValid then Exit(TSignature.Invalid);
  Hash := USHAKE_256(Key.d, 64);
  ScalarBytes := TUInt8Array.Make(@Hash[0], 32);
  Clamp(@ScalarBytes[0], 0, 31);
  a := ScalarBytes;
  Prefix := TUInt8Array.Make(@Hash[32], 32);
  Temp := UBytesConcat([Prefix, Message]);
  Nonce := TBigInt.Make(USHAKE_256(Temp, 64)) mod Curve.n;
  R := ScalarMultiply(Curve, Nonce, Curve.b);
  Result.r := PointCompress(Curve, R);
  Temp := UBytesConcat([Result.r, Key.q, Message]);
  Hram := TBigInt.Make(USHAKE_256(Temp, 64)) mod Curve.n;
  S := (Hram * a) mod Curve.n;
  S := (Nonce + S) mod Curve.n;
  Result.s := S;
end;

class function TUECC.Edwards.Verify_Ed25519_SHAKE(
  const Curve: TCurve;
  const PublicKey: TPointCompressed;
  const Message: TUInt8Array;
  const Signature: TSignature
): Boolean;
  var A: TPoint;
  var R: TPoint;
  var Hram: TBigInt;
  var SB: TPoint;
  var HramA: TPoint;
  var RHramA: TPoint;
  var AffineSB, AaffineRHram: TPoint;
  var Temp: TUInt8Array;
begin
  if not Signature.IsValid(Curve) then Exit(False);
  A := PointDecompress(Curve, PublicKey);
  if not A.IsValid then Exit(False);
  R := PointDecompress(Curve, Signature.r);
  if not R.IsValid then Exit(False);
  Temp := UBytesConcat([Signature.r, PublicKey, Message]);
  Hram := TBigInt.Make(USHAKE_256(Temp, 64)) mod Curve.n;
  SB := ScalarMultiply(Curve, Signature.s, Curve.b);
  HramA := ScalarMultiply(Curve, Hram, A);
  RHramA := PointAdd(Curve, R, HramA);
  AffineSB := Curve.ToAffine(SB);
  AaffineRHram := Curve.ToAffine(RHramA);
  Result := AffineSB = AaffineRHram;
end;

class constructor TUECC.Edwards.CreateClass;
begin
  Curve_Ed25519.p := '$7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffed';
  Curve_Ed25519.a := Curve_Ed25519.p - TBigInt.One;
  Curve_Ed25519.d := '$52036cee2b6ffe738cc740797779e89800700a4d4141d8ab75eb4dca135978a3';
  Curve_Ed25519.n := '$1000000000000000000000000000000014def9dea2f79cd65812631a5cf5d3ed';
  Curve_Ed25519.h := 8;
  Curve_Ed25519.b.x := '$216936d3cd6e53fec0a4e231fdd6dc5c692cc7609525a7b2c9562d608f25d51a';
  Curve_Ed25519.b.y := '$6666666666666666666666666666666666666666666666666666666666666658';
  Curve_Ed25519.b.z := TBigInt.One;
  Curve_Ed25519.b.t := '$67875f0fd78604e27eb9e7db11961b2c8c9f37cc6f0d87fe4569dc00c6e5b2a4';
  Curve_Ed25519.SqrtM1 := TBigInt.ModPow(2, (Curve_Ed25519.p - TBigInt.One) div 4, Curve_Ed25519.p);
end;

class function TUBLAKE3.KeyFromHex(const Hex: String): TKey;
  var ByteCount: Int32;
  var i, j: Int32;
begin
  UClear(Result, SizeOf(Result));
  for i := 1 to Length(Hex) do
  if not (Hex[i] in ['0'..'9', 'A'..'Z', 'a'..'z']) then
  begin
    Exit;
  end;
  if Length(Hex) mod 2 <> 0 then Exit;
  ByteCount := UMin(Length(Hex) div 2, Length(TKey));
  for i := 0 to ByteCount - 1 do
  begin
    j := i * 2 + 1;
    Result[i] := UInt8(StrToInt('$' + Hex[j] + Hex[j + 1]));
  end;
end;

class function TUBLAKE3.ROTR32(const x: UInt32; const n: UInt8): UInt32;
begin
  Result := (x shr n) or (x shl (32 - n));
end;

class procedure TUBLAKE3.G(var State: TState; const a, b, c, d, mx, my: UInt32);
begin
  State[a] := State[a] + State[b] + mx;
  State[d] := ROTR32(State[d] xor State[a], 16);
  State[c] := State[c] + State[d];
  State[b] := ROTR32(State[b] xor State[c], 12);
  State[a] := State[a] + State[b] + my;
  State[d] := ROTR32(State[d] xor State[a], 8);
  State[c] := State[c] + State[d];
  State[b] := ROTR32(State[b] xor State[c], 7);
end;

class function TUBLAKE3.BytesToWords(const BlockBytes: array of UInt8): TUInt32Arr16;
  var i: Int32;
begin
  for i := 0 to 15 do
  begin
    Result[i] := PUInt32(@BlockBytes[i * 4])^;
  end;
end;

class procedure TUBLAKE3.Round(
  var State: TState;
  const Msg: TBlock
);
begin
  G(State, 0, 4, 8, 12, Msg[0], Msg[1]);
  G(State, 1, 5, 9, 13, Msg[2], Msg[3]);
  G(State, 2, 6, 10, 14, Msg[4], Msg[5]);
  G(State, 3, 7, 11, 15, Msg[6], Msg[7]);
  G(State, 0, 5, 10, 15, Msg[8], Msg[9]);
  G(State, 1, 6, 11, 12, Msg[10], Msg[11]);
  G(State, 2, 7, 8, 13, Msg[12], Msg[13]);
  G(State, 3, 4, 9, 14, Msg[14], Msg[15]);
end;

class procedure TUBLAKE3.Permute(var Msg: TBlock);
  var Permuted: TBlock;
  var i: Int32;
begin
  for i := 0 to 15 do
  begin
    Permuted[i] := Msg[MSG_PERMUTATION[i]];
  end;
  Msg := Permuted;
end;

class function TUBLAKE3.Compress(
  const ChainingValue: TUInt32Arr8;
  const BlockWords: TBlock;
  const Counter: UInt64;
  const BlockLen: UInt32;
  const Flags: UInt32
): TBlock;
  var State: TState;
  var i: Int32;
  var Block: TBlock;
begin
  Block := BlockWords;
  UMove(State, ChainingValue, SizeOf(ChainingValue));
  State[8] := IV[0];
  State[9] := IV[1];
  State[10] := IV[2];
  State[11] := IV[3];
  State[12] := UInt32(Counter and $ffffffff);
  State[13] := UInt32(Counter shr 32);
  State[14] := BlockLen;
  State[15] := Flags;
  for i := 0 to 5 do
  begin
    Round(State, Block);
    Permute(Block);
  end;
  Round(State, Block);
  for i := 0 to 7 do
  begin
    Result[i] := State[i] xor State[i + 8];
    Result[i + 8] := State[i + 8] xor ChainingValue[i];
  end;
end;

class function TUBLAKE3.ParentOutput(
  const LeftChildCV, RightChildCV: TUInt32Arr8;
  const Key: TUInt32Arr8;
  const Flags: UInt32
): TOutput;
  var BlockWords: TBlock;
  var i: Integer;
begin
  for i := 0 to 7 do
  begin
    BlockWords[i] := LeftChildCV[i];
    BlockWords[i + 8] := RightChildCV[i];
  end;
  Result.InputChainingValue := Key;
  Result.BlockWords := BlockWords;
  Result.Counter := 0;
  Result.BlockLen := BLOCK_LEN;
  Result.Flags := PARENT or Flags;
end;

class function TUBLAKE3.Hash(
  const Data: TUInt8Array;
  const OutputSize: UInt32
): TUInt8Array;
  var Hasher: THasher;
begin
  Hasher.Init;
  Hasher.Update(Data);
  Result := Hasher.Finalize(OutputSize);
end;

class function TUBLAKE3.Hash(
  const Data: TUInt8Array;
  const Key: TKey;
  const OutputSize: UInt32
): TUInt8Array;
  var Hasher: THasher;
begin
  Hasher.Init(Key);
  Hasher.Update(Data);
  Result := Hasher.Finalize(OutputSize);
end;

class function TUBLAKE3.KDF(
  const Context: TUInt8Array;
  const Password: TUInt8Array;
  const OutputSize: Uint32
): TUInt8Array;
  var Hasher: THasher;
  var ContextKey: TUInt8Array;
  var Key: TKey;
begin
  Hasher.Init(DERIVE_KEY_CONTEXT);
  Hasher.Update(Context);
  ContextKey := Hasher.Finalize(32);
  UMove(Key, ContextKey[0], SizeOf(Key));
  Hasher.Init(Key, DERIVE_KEY_MATERIAL);
  Hasher.Update(Password);
  Result := Hasher.Finalize(OutputSize);
end;

function TUBLAKE3.TOutput.ChainingValue: TUInt32Arr8;
  var Out16: TUInt32Arr16;
  var i: Int32;
begin
  Out16 := TUBLAKE3.Compress(InputChainingValue, BlockWords, Counter, BlockLen, Flags);
  for i := 0 to 7 do Result[i] := Out16[i];
end;

function TUBLAKE3.TOutput.RootBytes(
  const OutLen: UInt32
): TUInt8Array;
  var OutputBlockCounter: UInt64;
  var OutputWords: TUInt32Arr16;
  var i, j: Int32;
  var Offset: UInt32;
begin
  Result := nil;
  SetLength(Result, OutLen);
  OutputBlockCounter := 0;
  Offset := 0;
  while Offset < OutLen do
  begin
    OutputWords := TUBLAKE3.Compress(
      InputChainingValue, BlockWords, OutputBlockCounter, BlockLen,
      Flags or ROOT
    );
    for i := 0 to 15 do
    begin
      for j := 0 to 3 do
      begin
        if Offset < OutLen then
        begin
          Result[Offset] := UInt8((OutputWords[i] shr (j * 8)) and $ff);
          Inc(Offset);
        end;
      end;
    end;
    Inc(OutputBlockCounter);
  end;
end;

function TUBLAKE3.TChunkState.Len: UInt32;
begin
  Result := BLOCK_LEN * BlocksCompressed + BlockLen;
end;

function TUBLAKE3.TChunkState.StartFlag: UInt32;
begin
  if BlocksCompressed = 0 then Exit(CHUNK_START) else Exit(0);
end;

function TUBLAKE3.TChunkState.Output: TOutput;
  var BlockWords: TBlock;
begin
  BlockWords := TUBLAKE3.BytesToWords(Block);
  Result.InputChainingValue := ChainingValue;
  Result.BlockWords := BlockWords;
  Result.Counter := ChunkCounter;
  Result.BlockLen := BlockLen;
  Result.Flags := Flags or StartFlag or CHUNK_END;
end;

procedure TUBLAKE3.TChunkState.Init(
  const AKey: TUInt32Arr8;
  const AChunkCounter: UInt64;
  const AFlags: UInt32
);
begin
  ChainingValue := AKey;
  ChunkCounter := AChunkCounter;
  UClear(Block, SizeOf(Block));
  BlockLen := 0;
  BlocksCompressed := 0;
  Flags := AFlags;
end;

procedure TUBLAKE3.TChunkState.Update(
  const Input: TUInt8Array;
  var InputOffset: Int32;
  const InputLen: Int32
);
  var Take: Int32;
  var BlockWords: TBlock;
  var Out16: TUInt32Arr16;
  var CV: TUInt32Arr8;
begin
  while InputOffset < InputLen do
  begin
    if BlockLen = BLOCK_LEN then
    begin
      BlockWords := TUBLAKE3.BytesToWords(Block);
      Out16 := TUBLAKE3.Compress(
        ChainingValue, BlockWords, ChunkCounter, BLOCK_LEN,
        Flags or StartFlag
      );
      UMove(CV, Out16, SizeOf(CV));
      ChainingValue := CV;
      UClear(Block, SizeOf(Block));
      BlockLen := 0;
      Inc(BlocksCompressed);
    end;
    Take := BLOCK_LEN - BlockLen;
    if Take > InputLen - InputOffset then
    begin
      Take := InputLen - InputOffset;
    end;
    Move(Input[InputOffset], Block[BlockLen], Take);
    BlockLen := BlockLen + Take;
    Inc(InputOffset, Take);
  end;
end;

procedure TUBLAKE3.THasher.Init(const AFlags: UInt32);
begin
  Move(IV, Key, SizeOf(Key));
  ChunkState.Init(Key, 0, AFlags);
  SetLength(CVStack, 54);
  CVStackLen := 0;
  Flags := AFlags;
end;

procedure TUBLAKE3.THasher.Init(const AKey: TUBLAKE3.TKey; const AFlags: UInt32);
begin
  Move(AKey, Key, SizeOf(Key));
  ChunkState.Init(Key, 0, AFlags);
  SetLength(CVStack, 54);
  CVStackLen := 0;
  Flags := AFlags;
end;

procedure TUBLAKE3.THasher.Update(const Input: TUInt8Array);
  var InputOffset, InputLen: Int32;
  var CV: TUInt32Arr8;
  var ChunkCounter: UInt64;
  var O: TOutput;
begin
  InputOffset := 0;
  InputLen := Length(Input);
  while InputOffset < InputLen do
  begin
    if ChunkState.Len = CHUNK_LEN then
    begin
      O := ChunkState.Output;
      CV := O.ChainingValue;
      ChunkCounter := ChunkState.ChunkCounter + 1;
      AddChunkChainingValue(CV, ChunkCounter);
      ChunkState.Init(Key, ChunkCounter, Flags);
    end;
    ChunkState.Update(Input, InputOffset, InputLen);
  end;
end;

function TUBLAKE3.THasher.Finalize(const OutLen: UInt32): TUInt8Array;
  var Output: TOutput;
  var ParentNodesRemaining: Integer;
  var CV: TUInt32Arr8;
begin
  Output := ChunkState.Output;
  ParentNodesRemaining := CVStackLen;
  while ParentNodesRemaining > 0 do
  begin
    Dec(ParentNodesRemaining);
    CV := Output.ChainingValue;
    Output := TUBLAKE3.ParentOutput(CVStack[ParentNodesRemaining], CV, Key, Flags);
  end;
  Result := Output.RootBytes(OutLen);
end;

procedure TUBLAKE3.THasher.PushStack(const CV: TUInt32Arr8);
begin
  Move(CV, CVStack[CVStackLen], SizeOf(TUInt32Arr8));
  Inc(CVStackLen);
end;

function TUBLAKE3.THasher.PopStack: TUInt32Arr8;
begin
  Dec(CVStackLen);
  UMove(Result, CVStack[CVStackLen], SizeOf(TUInt32Arr8));
end;

procedure TUBLAKE3.THasher.AddChunkChainingValue(
  var NewCV: TUInt32Arr8;
  const TotalChunks: UInt64
);
  var RightChild, LeftChild: TUInt32Arr8;
  var TotalChunksTmp: UInt64;
begin
  TotalChunksTmp := TotalChunks;
  while (TotalChunksTmp and 1) = 0 do
  begin
    RightChild := NewCV;
    LeftChild := PopStack;
    NewCV := TUBLAKE3.ParentOutput(LeftChild, RightChild, Key, Flags).ChainingValue;
    TotalChunksTmp := TotalChunksTmp shr 1;
  end;
  PushStack(NewCV);
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

function UMakeECCKey: TUECC.Weierstrass.TKey;
begin
  Result := TUECC.Weierstrass.MakeKey(TUECC.Weierstrass.Curve_SECP256R1);
end;

function USign_ECDSA(
  const MessageHash: TUECC.TBigInt;
  const PrivateKey: TUECC.TBigInt
): TUECC.Weierstrass.TSignature;
begin
  Result := TUECC.Weierstrass.Sign(
    TUECC.Weierstrass.Curve_SECP256R1,
    PrivateKey,
    MessageHash
  );
end;

function USign_ECDSA_SHA2_256(
  const Message: TUInt8Array;
  const PrivateKey: TUECC.TBigInt
): TUECC.Weierstrass.TSignature;
  var MessageHash: TUDigestSHA2_256;
begin
  MessageHash := USHA2_256(Message);
  Result := TUECC.Weierstrass.Sign(
    TUECC.Weierstrass.Curve_SECP256R1,
    PrivateKey,
    TUECC.TBigInt.Make(MessageHash)
  );
end;

function UVerify_ECDSA(
  const MessageHash: TUECC.TBigInt;
  const PublicKey: TUECC.Weierstrass.TPoint;
  const Signature: TUECC.Weierstrass.TSignature
): Boolean;
begin
  Result := TUECC.Weierstrass.Verify(
    TUECC.Weierstrass.Curve_SECP256R1,
    PublicKey,
    MessageHash,
    Signature
  );
end;

function USharedKey_ECDH(
  const PublicKey: TUECC.Weierstrass.TPoint;
  const PrivateKey: TUECC.TBigInt
): TUECC.TBigInt;
begin
  Result := TUECC.Weierstrass.SharedKey(
    TUECC.Weierstrass.Curve_SECP256R1,
    PublicKey,
    PrivateKey
  );
end;

end.
