unit CommonMedia;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$warn 6058 off}
{$warn 5024 off}

interface

uses
  SysUtils, Classes, CommonUtils, ZBase, ZInflate, ZDeflate;

type
  TUImageDataFormat = (
    uif_none,
    uif_g8,
    uif_g16,
    uif_g8a8,
    uif_g16a16,
    uif_r8g8b8,
    uif_r16g16b16,
    uif_r8g8b8a8,
    uif_r16g16b16a16,
    uif_r32g32b32_f
  );

  type TUImageData = class (TURefClass)
  protected
    type TPixelReadProc = function (const x, y: Integer): TUColor of object;
    type TPixelWriteProc = procedure (const x, y: Integer; const Value: TUColor) of object;
    var _Width: Int32;
    var _Height: Int32;
    var _BPP: Int32;
    var _Data: Pointer;
    var _DataSize: UInt32;
    var _Format: TUImageDataFormat;
    var _ReadProc: TPixelReadProc;
    var _WriteProc: TPixelWriteProc;
    procedure SetFormat(const f: TUImageDataFormat);
    function GetPixel(const x, y: Int32): TUColor;
    function ReadNone(const x, y: Int32): TUColor;
    function ReadG8(const x, y: Int32): TUColor;
    function ReadG16(const x, y: Int32): TUColor;
    function ReadG8A8(const x, y: Int32): TUColor;
    function ReadG16A16(const x, y: Int32): TUColor;
    function ReadR8G8B8(const x, y: Int32): TUColor;
    function ReadR16G16B16(const x, y: Int32): TUColor;
    function ReadR8G8B8A8(const x, y: Int32): TUColor;
    function ReadR16G16B16A16(const x, y: Int32): TUColor;
    function ReadR32G32B32_F(const x, y: Int32): TUColor;
    procedure SetPixel(const x, y: Int32; const Value: TUColor);
    procedure WriteNone(const x, y: Int32; const Value: TUColor);
    procedure WriteG8(const x, y: Int32; const Value: TUColor);
    procedure WriteG16(const x, y: Int32; const Value: TUColor);
    procedure WriteG8A8(const x, y: Int32; const Value: TUColor);
    procedure WriteG16A16(const x, y: Int32; const Value: TUColor);
    procedure WriteR8G8B8(const x, y: Int32; const Value: TUColor);
    procedure WriteR16G16B16(const x, y: Int32; const Value: TUColor);
    procedure WriteR8G8B8A8(const x, y: Int32; const Value: TUColor);
    procedure WriteR16G16B16A16(const x, y: Int32; const Value: TUColor);
    procedure WriteR32G32B32_F(const x, y: Int32; const Value: TUColor);
    procedure DataAlloc; overload;
    procedure DataAlloc(const Size: UInt32); overload;
    procedure DataFree;
    class procedure RegisterImageClass;
  public
    property Width: Int32 read _Width;
    property Height: Int32 read _Height;
    property Data: Pointer read _Data;
    property BPP: Int32 read _BPP;
    property DataSize: UInt32 read _DataSize;
    property Format: TUImageDataFormat read _Format;
    property Pixels[const x, y: Int32]: TUColor read GetPixel write SetPixel; default;
    function DataAt(const x, y: Int32): Pointer; inline;
    class function CanLoad(const Stream: TStream): Boolean; virtual; overload;
    class function CanLoad(const FileName: String): Boolean; virtual; overload;
    class function CanLoad(const Buffer: Pointer; const Size: UInt32): Boolean; virtual; overload;
    class function CanLoad(const StreamHelper: TUStreamHelper): Boolean; virtual; abstract; overload;
    procedure Load(const Stream: TStream); virtual; overload;
    procedure Load(const FileName: String); virtual; overload;
    procedure Load(const Buffer: Pointer; const Size: UInt32); virtual; overload;
    procedure Load(const StreamHelper: TUStreamHelper); virtual; abstract; overload;
    procedure Save(const Stream: TStream); virtual; overload;
    procedure Save(const FileName: String); virtual; overload;
    procedure Save(const StreamHelper: TUStreamHelper); virtual; abstract; overload;
    procedure Allocate(const NewFormat: TUImageDataFormat; const NewWidth, NewHeight: Int32);
    constructor Create; virtual;
    destructor Destroy; override;
  end;
  type TUImageDataShared = specialize TUSharedRef<TUImageData>;
  type TUImageDataClass = class of TUImageData;

  type TUImageDataPNG = class(TUImageData)
  protected
    {$push}
    {$minenumsize 1}
    type TColorType = (
      ctGrayscale = 0,
      ctTrueColor = 2,
      ctIndexedColor = 3,
      ctGrayscaleAlpha = 4,
      ctTrueColorAlpha = 6
    );
    {$minenumsize 4}
    type TFilter = (
      flNone = 0,
      flSub = 1,
      flUp = 2,
      flAverage = 3,
      flPaeth = 4
    );
    type TInterlace = (
      inNone = 0,
      inAdam7 = 1
    );
    {$pop}
    type TChunk = packed record
      ChunkLength: UInt32;
      ChunkType: array[0..3] of AnsiChar;
      ChunkData: Pointer;
      ChunkCRC: UInt32;
    end;
    type TChunkIHDR = packed record
      Width: UInt32;
      Height: UInt32;
      BitDepth: UInt8;
      ColorType: TColorType;
      CompMethod: UInt8;
      FilterMethod: UInt8;
      InterlaceMethod: TInterlace;
    end;
    type TChunkPLTE = packed record
      Entries: array of record r, g, b: UInt8; end;
    end;
    type TChunkIDAT = array of UInt8;
    const PNGHeader: AnsiString = (#137#80#78#71#13#10#26#10);
    const CRCTable: array[0..255] of UInt32 = (
      $00000000, $77073096, $EE0E612C, $990951BA, $076DC419, $706AF48F, $E963A535, $9E6495A3,
      $0EDB8832, $79DCB8A4, $E0D5E91E, $97D2D988, $09B64C2B, $7EB17CBD, $E7B82D07, $90BF1D91,
      $1DB71064, $6AB020F2, $F3B97148, $84BE41DE, $1ADAD47D, $6DDDE4EB, $F4D4B551, $83D385C7,
      $136C9856, $646BA8C0, $FD62F97A, $8A65C9EC, $14015C4F, $63066CD9, $FA0F3D63, $8D080DF5,
      $3B6E20C8, $4C69105E, $D56041E4, $A2677172, $3C03E4D1, $4B04D447, $D20D85FD, $A50AB56B,
      $35B5A8FA, $42B2986C, $DBBBC9D6, $ACBCF940, $32D86CE3, $45DF5C75, $DCD60DCF, $ABD13D59,
      $26D930AC, $51DE003A, $C8D75180, $BFD06116, $21B4F4B5, $56B3C423, $CFBA9599, $B8BDA50F,
      $2802B89E, $5F058808, $C60CD9B2, $B10BE924, $2F6F7C87, $58684C11, $C1611DAB, $B6662D3D,
      $76DC4190, $01DB7106, $98D220BC, $EFD5102A, $71B18589, $06B6B51F, $9FBFE4A5, $E8B8D433,
      $7807C9A2, $0F00F934, $9609A88E, $E10E9818, $7F6A0DBB, $086D3D2D, $91646C97, $E6635C01,
      $6B6B51F4, $1C6C6162, $856530D8, $F262004E, $6C0695ED, $1B01A57B, $8208F4C1, $F50FC457,
      $65B0D9C6, $12B7E950, $8BBEB8EA, $FCB9887C, $62DD1DDF, $15DA2D49, $8CD37CF3, $FBD44C65,
      $4DB26158, $3AB551CE, $A3BC0074, $D4BB30E2, $4ADFA541, $3DD895D7, $A4D1C46D, $D3D6F4FB,
      $4369E96A, $346ED9FC, $AD678846, $DA60B8D0, $44042D73, $33031DE5, $AA0A4C5F, $DD0D7CC9,
      $5005713C, $270241AA, $BE0B1010, $C90C2086, $5768B525, $206F85B3, $B966D409, $CE61E49F,
      $5EDEF90E, $29D9C998, $B0D09822, $C7D7A8B4, $59B33D17, $2EB40D81, $B7BD5C3B, $C0BA6CAD,
      $EDB88320, $9ABFB3B6, $03B6E20C, $74B1D29A, $EAD54739, $9DD277AF, $04DB2615, $73DC1683,
      $E3630B12, $94643B84, $0D6D6A3E, $7A6A5AA8, $E40ECF0B, $9309FF9D, $0A00AE27, $7D079EB1,
      $F00F9344, $8708A3D2, $1E01F268, $6906C2FE, $F762575D, $806567CB, $196C3671, $6E6B06E7,
      $FED41B76, $89D32BE0, $10DA7A5A, $67DD4ACC, $F9B9DF6F, $8EBEEFF9, $17B7BE43, $60B08ED5,
      $D6D6A3E8, $A1D1937E, $38D8C2C4, $4FDFF252, $D1BB67F1, $A6BC5767, $3FB506DD, $48B2364B,
      $D80D2BDA, $AF0A1B4C, $36034AF6, $41047A60, $DF60EFC3, $A867DF55, $316E8EEF, $4669BE79,
      $CB61B38C, $BC66831A, $256FD2A0, $5268E236, $CC0C7795, $BB0B4703, $220216B9, $5505262F,
      $C5BA3BBE, $B2BD0B28, $2BB45A92, $5CB36A04, $C2D7FFA7, $B5D0CF31, $2CD99E8B, $5BDEAE1D,
      $9B64C2B0, $EC63F226, $756AA39C, $026D930A, $9C0906A9, $EB0E363F, $72076785, $05005713,
      $95BF4A82, $E2B87A14, $7BB12BAE, $0CB61B38, $92D28E9B, $E5D5BE0D, $7CDCEFB7, $0BDBDF21,
      $86D3D2D4, $F1D4E242, $68DDB3F8, $1FDA836E, $81BE16CD, $F6B9265B, $6FB077E1, $18B74777,
      $88085AE6, $FF0F6A70, $66063BCA, $11010B5C, $8F659EFF, $F862AE69, $616BFFD3, $166CCF45,
      $A00AE278, $D70DD2EE, $4E048354, $3903B3C2, $A7672661, $D06016F7, $4969474D, $3E6E77DB,
      $AED16A4A, $D9D65ADC, $40DF0B66, $37D83BF0, $A9BCAE53, $DEBB9EC5, $47B2CF7F, $30B5FFE9,
      $BDBDF21C, $CABAC28A, $53B39330, $24B4A3A6, $BAD03605, $CDD70693, $54DE5729, $23D967BF,
      $B3667A2E, $C4614AB8, $5D681B02, $2A6F2B94, $B40BBE37, $C30C8EA1, $5A05DF1B, $2D02EF8D
    );
    class procedure Decompress(const Buffer: Pointer; const Size: Int32; const Output: TStream);
    class procedure Compress(const Buffer: Pointer; const Size: Int32; const Output: TStream);
    class function Swap16(const n: UInt16): UInt16;
    class function Swap32(const n: UInt32): UInt32;
    class function GetCRC(const Buffer: Pointer; const Size: Int32): UInt32;
    class function CheckCRC(const Chunk: TChunk): Boolean;
  public
    class constructor CreateClass;
    class function CanLoad(const StreamHelper: TUStreamHelper): Boolean; override;
    procedure Load(const StreamHelper: TUStreamHelper); override;
    procedure Save(const StreamHelper: TUStreamHelper); override;
  end;

function ULoadImageData(const Stream: TStream): TUImageDataShared; overload;
function ULoadImageData(const FileName: String): TUImageDataShared; overload;
function ULoadImageData(const Buffer: Pointer; const Size: UInt32): TUImageDataShared; overload;
function ULoadImageData(const StreamHelper: TUStreamHelper): TUImageDataShared; overload;

implementation

var ImageFormats: array of TUImageDataClass;
procedure RegisterImageFormat(const ImageType: TUImageDataClass);
  var f: TUImageDataClass;
begin
  for f in ImageFormats do
  if f = ImageType then Exit;
  specialize UArrAppend<TUImageDataClass>(ImageFormats, ImageType);
end;

function ULoadImageData(const Stream: TStream): TUImageDataShared;
  var f: TUImageDataClass;
begin
  for f in ImageFormats do
  if f.CanLoad(Stream) then
  begin
    Result := f.Create;
    Result.Ptr.Load(Stream);
    Exit;
  end;
  Result := nil;
end;

function ULoadImageData(const FileName: String): TUImageDataShared;
  var f: TUImageDataClass;
begin
  for f in ImageFormats do
  if f.CanLoad(FileName) then
  begin
    Result := f.Create;
    Result.Ptr.Load(FileName);
    Exit;
  end;
  Result := nil;
end;

function ULoadImageData(const Buffer: Pointer; const Size: UInt32): TUImageDataShared;
  var f: TUImageDataClass;
begin
  for f in ImageFormats do
  if f.CanLoad(Buffer, Size) then
  begin
    Result := f.Create;
    Result.Ptr.Load(Buffer, Size);
    Exit;
  end;
  Result := nil;
end;

function ULoadImageData(const StreamHelper: TUStreamHelper): TUImageDataShared;
  var f: TUImageDataClass;
begin
  for f in ImageFormats do
  if f.CanLoad(StreamHelper) then
  begin
    Result := f.Create;
    Result.Ptr.Load(StreamHelper);
    Exit;
  end;
  Result := nil;
end;

// TUImageData begin
procedure TUImageData.SetFormat(const f: TUImageDataFormat);
begin
  _Format := f;
  case _Format of
    uif_none:
    begin
      _BPP := 0;
      _ReadProc := @ReadNone;
      _WriteProc := @WriteNone;
    end;
    uif_g8:
    begin
      _BPP := 1;
      _ReadProc := @ReadG8;
      _WriteProc := @WriteG8;
    end;
    uif_g16:
    begin
      _BPP := 2;
      _ReadProc := @ReadG16;
      _WriteProc := @WriteG16;
    end;
    uif_g8a8:
    begin
      _BPP := 2;
      _ReadProc := @ReadG8A8;
      _WriteProc := @WriteG8A8;
    end;
    uif_g16a16:
    begin
      _BPP := 4;
      _ReadProc := @ReadG16A16;
      _WriteProc := @WriteG16A16;
    end;
    uif_r8g8b8:
    begin
      _BPP := 3;
      _ReadProc := @ReadR8G8B8;
      _WriteProc := @WriteR8G8B8;
    end;
    uif_r16g16b16:
    begin
      _BPP := 6;
      _ReadProc := @ReadR16G16B16;
      _WriteProc := @WriteR16G16B16;
    end;
    uif_r8g8b8a8:
    begin
      _BPP := 4;
      _ReadProc := @ReadR8G8B8A8;
      _WriteProc := @WriteR8G8B8A8;
    end;
    uif_r16g16b16a16:
    begin
      _BPP := 8;
      _ReadProc := @ReadR16G16B16A16;
      _WriteProc := @WriteR16G16B16A16;
    end;
    uif_r32g32b32_f:
    begin
      _BPP := 12;
      _ReadProc := @ReadR32G32B32_F;
      _WriteProc := @WriteR32G32B32_F;
    end;
  end;
end;

function TUImageData.GetPixel(const x, y: Int32): TUColor;
begin
  Result := _ReadProc(x, y);
end;

{$push}
{$hints off}
function TUImageData.ReadNone(const x, y: Int32): TUColor;
begin
  Result := 0;
end;
{$pop}

function TUImageData.ReadG8(const x, y: Int32): TUColor;
  var c: UInt8;
begin
  c := PUInt8(_Data + y * _Width + x)^;
  Result := TUColor.Make(c, c, c, $ff);
end;

function TUImageData.ReadG16(const x, y: Int32): TUColor;
  var c: UInt8;
begin
  c := PUInt8(_Data + (y * _Width + x) * _BPP + 1)^;
  Result := TUColor.Make(c, c, c, $ff);
end;

function TUImageData.ReadG8A8(const x, y: Int32): TUColor;
  var d: PUInt8Arr;
begin
  d := PUInt8Arr(_Data + (y * _Width + x) * _BPP);
  Result := TUColor.Make(d^[0], d^[0], d^[0], d^[1]);
end;

function TUImageData.ReadG16A16(const x, y: Int32): TUColor;
  var d: PUInt8Arr;
begin
  d := PUInt8Arr(_Data + (y * _Width + x) * _BPP);
  Result := TUColor.Make(d^[0], d^[0], d^[0], d^[1]);
end;

function TUImageData.ReadR8G8B8(const x, y: Int32): TUColor;
begin
  Result := (PUInt32(_Data + (y * _Width + x) * _BPP)^ and $ffffff) or ($ff shl 24);
end;

function TUImageData.ReadR16G16B16(const x, y: Int32): TUColor;
  var b: PUInt8Arr;
begin
  b := PUInt8Arr(_Data + (y * _Width + x) * _BPP + 1);
  Result := TUColor.Make(b^[0], b^[2], b^[4], $ff);
end;

function TUImageData.ReadR8G8B8A8(const x, y: Int32): TUColor;
begin
  Result := PUColor(_Data + (y * _Width + x) * _BPP)^;
end;

function TUImageData.ReadR16G16B16A16(const x, y: Int32): TUColor;
  var d: PUInt8Arr;
begin
  d := PUInt8Arr(_Data + (y * _Width + x) * _BPP + 1);
  Result := TUColor.Make(d^[0], d^[2], d^[4], d^[6]);
end;

function TUImageData.ReadR32G32B32_F(const x, y: Int32): TUColor;
  function ftob(const f: TUFloat): UInt8;
    var i: Int32;
    const gamma = 1.0 / 2.2;
  begin
    i := Round(UPow(f, gamma) * $ff);
    if i < 0 then i := 0 else if i > $ff then i := $ff;
    Result := i;
  end;
  var f: PUFloatArr;
begin
  f := PUFloatArr(DataAt(x, y));
  Result := TUColor.Make(ftob(f^[0]), ftob(f^[1]), ftob(f^[2]), $ff);
end;

procedure TUImageData.SetPixel(const x, y: Int32; const Value: TUColor);
begin
  _WriteProc(x, y, Value);
end;

{$push}
{$hints off}
procedure TUImageData.WriteNone(const x, y: Int32; const Value: TUColor);
begin

end;
{$pop}

procedure TUImageData.WriteG8(const x, y: Int32; const Value: TUColor);
begin
  PUInt8(_Data + (y * _Width + x) * _BPP)^ := Value.r;
end;

procedure TUImageData.WriteG16(const x, y: Int32; const Value: TUColor);
  var d: PUInt8Arr;
begin
  d := PUInt8Arr(_Data + (y * _Width + x) * _BPP);
  d^[0] := 0; d^[1] := Value.r;
end;

procedure TUImageData.WriteG8A8(const x, y: Int32; const Value: TUColor);
  var d: PUInt8Arr;
begin
  d := PUInt8Arr(_Data + (y * _Width + x) * _BPP);
  d^[0] := Value.r; d^[1] := Value.a;
end;

procedure TUImageData.WriteG16A16(const x, y: Int32; const Value: TUColor);
  var d: PUInt8Arr;
begin
  d := PUInt8Arr(_Data + (y * _Width + x) * _BPP);
  d^[0] := 0; d^[1] := Value.r;
  d^[2] := 0; d^[3] := Value.a;
end;

procedure TUImageData.WriteR8G8B8(const x, y: Int32; const Value: TUColor);
  var d: PUInt8Arr;
begin
  d := PUInt8Arr(_Data + (y * _Width + x) * _BPP);
  d^[0] := Value.r; d^[1] := Value.g; d^[2] := Value.b;
end;

procedure TUImageData.WriteR16G16B16(const x, y: Int32; const Value: TUColor);
  var d: PUInt8Arr;
begin
  d := PUInt8Arr(_Data + (y * _Width + x) * _BPP);
  d^[0] := 0; d^[1] := Value.r;
  d^[2] := 0; d^[3] := Value.g;
  d^[4] := 0; d^[5] := Value.b;
end;

procedure TUImageData.WriteR8G8B8A8(const x, y: Int32; const Value: TUColor);
begin
  PUColor(_Data + (y * _Width + x) * _BPP)^ := Value;
end;

procedure TUImageData.WriteR16G16B16A16(const x, y: Int32; const Value: TUColor);
  var d: PUInt8Arr;
begin
  d := PUInt8Arr(_Data + (y * _Width + x) * _BPP);
  d^[0] := 0; d^[1] := Value.r;
  d^[2] := 0; d^[3] := Value.g;
  d^[4] := 0; d^[5] := Value.b;
  d^[6] := 0; d^[7] := Value.a;
end;

procedure TUImageData.WriteR32G32B32_F(const x, y: Int32; const Value: TUColor);
  var f: PUFloatArr;
  const gamma = 2.2;
  const rcp_ff = 1 / $ff;
begin
  f := PUFloatArr(DataAt(x, y));
  f^[0] := UPow(Value.r * rcp_ff, gamma);
  f^[1] := UPow(Value.g * rcp_ff, gamma);
  f^[2] := UPow(Value.b * rcp_ff, gamma);
end;

procedure TUImageData.DataAlloc;
begin
  if _BPP > 0 then
  begin
    DataAlloc(_Width * _Height * _BPP);
  end;
end;

procedure TUImageData.DataAlloc(const Size: UInt32);
begin
  if _DataSize > 0 then DataFree;
  _DataSize := Size;
  GetMem(_Data, _DataSize);
end;

procedure TUImageData.DataFree;
begin
  if _DataSize > 0 then
  begin
    Freemem(_Data, _DataSize);
    _DataSize := 0;
  end;
end;

function TUImageData.DataAt(const x, y: Int32): Pointer;
begin
  Result := _Data + (y * _Width + x) * _BPP;
end;

class procedure TUImageData.RegisterImageClass;
begin
  RegisterImageFormat(TUImageDataClass(ClassType));
end;

class function TUImageData.CanLoad(const Stream: TStream): Boolean;
  var sh: TUStreamHelper;
begin
  sh := TUStreamHelper.Create(Stream);
  try
    Result := CanLoad(sh);
  finally
    sh.Free;
  end;
end;

class function TUImageData.CanLoad(const FileName: String): Boolean;
  var fs: TFileStream;
begin
  fs := TFileStream.Create(FileName, fmOpenRead);
  try
    Result := CanLoad(fs);
  finally
    fs.Free;
  end;
end;

class function TUImageData.CanLoad(const Buffer: Pointer; const Size: UInt32): Boolean;
  var ms: TUConstMemoryStream;
begin
  ms := TUConstMemoryStream.Create(Buffer, Size);
  try
    Result := CanLoad(ms);
  finally
    ms.Free;
  end;
end;

procedure TUImageData.Load(const Stream: TStream);
  var sh: TUStreamHelper;
begin
  sh := TUStreamHelper.Create(Stream);
  try
    Load(sh);
  finally
    sh.Free;
  end;
end;

procedure TUImageData.Load(const FileName: String);
  var fs: TFileStream;
begin
  fs := TFileStream.Create(FileName, fmOpenRead);
  try
    Load(fs);
  finally
    fs.Free;
  end;
end;

procedure TUImageData.Load(const Buffer: Pointer; const Size: UInt32);
  var ms: TUConstMemoryStream;
begin
  ms := TUConstMemoryStream.Create(Buffer, Size);
  try
    Load(ms);
  finally
    ms.Free;
  end;
end;

procedure TUImageData.Save(const Stream: TStream);
  var sh: TUStreamHelper;
begin
  sh := TUStreamHelper.Create(Stream);
  try
    Save(sh);
  finally
    sh.Free;
  end;
end;

procedure TUImageData.Save(const FileName: String);
  var fs: TFileStream;
begin
  fs := TFileStream.Create(FileName, fmCreate);
  try
    Save(fs);
  finally
    fs.Free;
  end;
end;

procedure TUImageData.Allocate(const NewFormat: TUImageDataFormat; const NewWidth, NewHeight: Integer);
begin
  SetFormat(NewFormat);
  _Width := NewWidth;
  _Height := NewHeight;
  DataAlloc;
end;

constructor TUImageData.Create;
begin
  inherited Create;
  _Width := 0;
  _Height := 0;
  _BPP := 0;
  _Data := nil;
  _DataSize := 0;
  SetFormat(uif_none);
end;

destructor TUImageData.Destroy;
begin
  DataFree;
  inherited Destroy;
end;
// TUImageData end

// TUImageDataPNG begin
{$push}
{$hints off}
class procedure TUImageDataPNG.Decompress(const Buffer: Pointer; const Size: Int32; const Output: TStream);
  var ZStreamRec: z_stream;
  var ZResult: Int32;
  var TempBuffer: Pointer;
  const BufferSize = $8000;
begin
  FillChar(ZStreamRec, SizeOf(z_stream), 0);
  ZStreamRec.next_in := Buffer;
  ZStreamRec.avail_in := Size;
  if inflateInit(ZStreamRec) < 0 then Exit;
  GetMem(TempBuffer, BufferSize);
  try
    while ZStreamRec.avail_in > 0 do
    begin
      ZStreamRec.next_out := TempBuffer;
      ZStreamRec.avail_out := BufferSize;
      inflate(ZStreamRec, Z_NO_FLUSH);
      Output.Write(TempBuffer^, BufferSize - ZStreamRec.avail_out);
    end;
    repeat
      ZStreamRec.next_out := TempBuffer;
      ZStreamRec.avail_out := BufferSize;
      ZResult := inflate(ZStreamRec, Z_FINISH);
      Output.Write(TempBuffer^, BufferSize - ZStreamRec.avail_out);
    until (ZResult = Z_STREAM_END) and (ZStreamRec.avail_out > 0);
  finally
    FreeMem(TempBuffer, BufferSize);
    inflateEnd(ZStreamRec);
  end;
end;

class procedure TUImageDataPNG.Compress(const Buffer: Pointer; const Size: Int32; const Output: TStream);
  var ZStreamRec: z_stream;
  var ZResult: Int32;
  var TempBuffer: Pointer;
  const BufferSize = $8000;
begin
  GetMem(TempBuffer, BufferSize);
  FillChar(ZStreamRec, SizeOf(z_stream), 0);
  ZStreamRec.next_in := Buffer;
  ZStreamRec.avail_in := Size;
  if DeflateInit(ZStreamRec, Z_BEST_COMPRESSION) < 0 then
  begin
    FreeMem(TempBuffer, BufferSize);
    Exit;
  end;
  try
    while ZStreamRec.avail_in > 0 do
    begin
      ZStreamRec.next_out := TempBuffer;
      ZStreamRec.avail_out := BufferSize;
      Deflate(ZStreamRec, Z_NO_FLUSH);
      Output.WriteBuffer(TempBuffer^, BufferSize - ZStreamRec.avail_out);
    end;
    repeat
      ZStreamRec.next_out := TempBuffer;
      ZStreamRec.avail_out := BufferSize;
      ZResult := Deflate(ZStreamRec, Z_FINISH);
      Output.WriteBuffer(TempBuffer^, BufferSize - ZStreamRec.avail_out);
    until (ZResult = Z_STREAM_END) and (ZStreamRec.avail_out > 0);
  finally
    FreeMem(TempBuffer, BufferSize);
    DeflateEnd(ZStreamRec);
  end;
end;

class function TUImageDataPNG.Swap16(const n: UInt16): UInt16;
  type TByte2 = array[0..1] of UInt8;
  var t: UInt16;
begin
  TByte2(t)[0] := TByte2(n)[1];
  TByte2(t)[1] := TByte2(n)[0];
  Result := t;
end;

class function TUImageDataPNG.Swap32(const n: UInt32): UInt32;
  type TByte4 = array[0..3] of Byte;
  var t: LongWord;
begin
  TByte4(t)[0] := TByte4(n)[3];
  TByte4(t)[1] := TByte4(n)[2];
  TByte4(t)[2] := TByte4(n)[1];
  TByte4(t)[3] := TByte4(n)[0];
  Result := t;
end;
{$pop}

class function TUImageDataPNG.GetCRC(const Buffer: Pointer; const Size: Int32): UInt32;
  var i: Int32;
  var pb: PUInt8;
begin
  Result := $ffffffff;
  pb := Buffer;
  for i := 0 to Size - 1 do
  begin
    Result:= CRCTable[(Result xor pb^) and $ff] xor (Result shr 8);
    Inc(pb);
  end;
  Result := Result xor $ffffffff;
end;

class function TUImageDataPNG.CheckCRC(const Chunk: TChunk): Boolean;
  var i: Int32;
  var CRC: UInt32;
  var ChunkData: PUInt8;
begin
  CRC := $ffffffff;
  for i := 0 to 3 do
  CRC := CRCTable[(CRC xor Byte(Chunk.ChunkType[i])) and $ff] xor (CRC shr 8);
  ChunkData := Chunk.ChunkData;
  for i := 0 to Chunk.ChunkLength - 1 do
  begin
    CRC := CRCTable[(CRC xor ChunkData^) and $ff] xor (CRC shr 8);
    Inc(ChunkData);
  end;
  CRC := CRC xor $ffffffff;
  Result := CRC = Chunk.ChunkCRC;
end;

class constructor TUImageDataPNG.CreateClass;
begin
  RegisterImageClass;
end;

class function TUImageDataPNG.CanLoad(const StreamHelper: TUStreamHelper): Boolean;
  var Header: array[0..7] of AnsiChar;
begin
  Result := False;
  if StreamHelper.Remaining < 8 then Exit;
  StreamHelper.PosPush;
  {$push}
  {$hints off}
  StreamHelper.ReadBuffer(@Header, 8);
  {$hints on}
  Result := Header = PNGHeader;
  StreamHelper.PosPop;
end;

procedure TUImageDataPNG.Load(const StreamHelper: TUStreamHelper);
  var sh: TUStreamHelper absolute StreamHelper;
  var Header: array[0..7] of AnsiChar;
  var ChunkData: array of UInt8;
  var Chunk: TChunk;
  var ChunkIHDR: TChunkIHDR;
  var ChunkPLTE: TChunkPLTE;
  var ChunkIDAT: array of TChunkIDAT;
  var TranspG: UInt16;
  var TranspRGB: array[0..2] of UInt16;
  var TranspPalette: array of UInt8;
  var Transp: Boolean;
  var KeepReading: Boolean;
  procedure ReadChunk;
  begin
    sh.ReadBuffer(@Chunk.ChunkLength, 4); Chunk.ChunkLength := Swap32(Chunk.ChunkLength);
    sh.ReadBuffer(@Chunk.ChunkType, 4);
    if Length(ChunkData) < Integer(Chunk.ChunkLength) then
    SetLength(ChunkData, Chunk.ChunkLength);
    Chunk.ChunkData := @ChunkData[0];
    sh.ReadBuffer(Chunk.ChunkData, Chunk.ChunkLength);
    sh.ReadBuffer(@Chunk.ChunkCRC, 4); Chunk.ChunkCRC := Swap32(Chunk.ChunkCRC);
  end;
  var i, j, Pass: Int32;
  var PixelDataSize: Int32;
  var CompressedData: TMemoryStream;
  var DecompressedData: TMemoryStream;
  var CurFilter: TFilter;
  var ScanLineCur: PUInt8Arr;
  var ScanLinePrev: PUInt8Arr;
  var ScanLineSize: UInt32;
  var BitPerPixel: UInt8;
  var BitStep: UInt8;
  var BitMask: UInt8;
  var BitCur: UInt8;
  function UnpackBits(const Bits: UInt8): UInt8;
  begin
    Result := Round((Bits / BitMask) * $ff);
  end;
  function GetA(const Pos: Int32): UInt8;
  begin
    if ChunkIHDR.BitDepth < 8 then
    begin
      if Pos > 0 then
      Result := ScanlineCur^[Pos - 1]
      else
      Result := 0;
    end
    else
    begin
      if Pos >= PixelDataSize then
      Result := ScanlineCur^[Pos - PixelDataSize]
      else
      Result := 0;
    end;
  end;
  function GetB(const Pos: Int32): UInt8;
  begin
    if ScanlinePrev <> nil then
    Result := ScanlinePrev^[Pos]
    else
    Result := 0;
  end;
  function GetC(const Pos: Int32): UInt8;
  begin
    if ScanlinePrev <> nil then
    begin
      if ChunkIHDR.BitDepth < 8 then
      begin
        if Pos > 0 then
        Result := ScanlinePrev^[Pos - 1]
        else
        Result := 0;
      end
      else
      begin
        if Pos >= PixelDataSize then
        Result := ScanlinePrev^[Pos - PixelDataSize]
        else
        Result := 0;
      end;
    end
    else
    Result := 0;
  end;
  function PaethPredictor(const a, b, c: UInt8): UInt8;
    var p, pa, pb, pc: Int32;
  begin
    p := Int32(a) + Int32(b) - Int32(c);
    pa := Abs(p - a);
    pb := Abs(p - b);
    pc := Abs(p - c);
    if (pa <= pb) and (pa <= pc) then Result := a
    else if (pb <= pc) then Result := b
    else Result := c;
  end;
  function FilterSub(const x: UInt8; const Pos: Int32): UInt8;
  begin
    Result := (x + GetA(Pos)) and $ff;
  end;
  function FilterUp(const x: UInt8; const Pos: Int32): UInt8;
  begin
    Result := (x + GetB(Pos)) and $ff;
  end;
  function FilterAverage(const x: UInt8; const Pos: Int32): UInt8;
  begin
    Result := (x + (GetA(Pos) + GetB(Pos)) div 2) and $ff;
  end;
  function FilterPaeth(const x: UInt8; const Pos: Int32): UInt8;
  begin
    Result := (x + PaethPredictor(GetA(Pos), GetB(Pos), GetC(Pos))) and $ff;
  end;
  const RowStart: array[0..7] of Int32 = (0, 0, 0, 4, 0, 2, 0, 1);
  const ColStart: array[0..7] of Int32 = (0, 0, 4, 0, 2, 0, 1, 0);
  const RowOffset: array[0..7] of Int32 = (1, 8, 8, 8, 4, 4, 2, 2);
  const ColOffset: array[0..7] of Int32 = (1, 8, 8, 4, 4, 2, 2, 1);
  var PassRows: Int32;
  var PassCols: Int32;
  var PassStart: Int32;
  var PassEnd: Int32;
  var x, y, b: Int32;
  var DataPtr: Pointer;
begin
  ChunkPLTE.Entries := nil;
  TranspPalette := nil;
  {$push}
  {$hints off}
  sh.ReadBuffer(@Header, 8);
  {$pop}
  if Header = PNGHeader then
  begin
    ChunkIDAT := nil;
    Transp := False;
    KeepReading := True;
    while KeepReading do
    begin
      ReadChunk;
      if CheckCRC(Chunk) then
      begin
        if (Chunk.ChunkType = 'IHDR') then
        begin
          ChunkIHDR.Width := Swap32(PUInt32(@PUInt8Arr(Chunk.ChunkData)^[0])^);
          ChunkIHDR.Height := Swap32(PUInt32(@PUInt8Arr(Chunk.ChunkData)^[4])^);
          ChunkIHDR.BitDepth := PUInt8Arr(Chunk.ChunkData)^[8];
          ChunkIHDR.ColorType := TColorType(PByteArray(Chunk.ChunkData)^[9]);
          ChunkIHDR.CompMethod := PUInt8Arr(Chunk.ChunkData)^[10];
          ChunkIHDR.FilterMethod := PUInt8Arr(Chunk.ChunkData)^[11];
          ChunkIHDR.InterlaceMethod := TInterlace(PUInt8Arr(Chunk.ChunkData)^[12]);
          if ChunkIHDR.CompMethod <> 0 then Exit;
          if ChunkIHDR.FilterMethod <> 0 then Exit;
          if Byte(ChunkIHDR.InterlaceMethod) > 1 then Exit;
          case ChunkIHDR.ColorType of
            ctGrayscale:
            begin
              if not (ChunkIHDR.BitDepth in [1, 2, 4, 8, 16]) then Exit;
              if ChunkIHDR.BitDepth = 16 then
              PixelDataSize := 2 else PixelDataSize := 1;
              case ChunkIHDR.BitDepth of
                1:
                begin
                  BitPerPixel := 8;
                  BitStep := 1;
                  BitMask := 1;
                  SetFormat(uif_g8);
                end;
                2:
                begin
                  BitPerPixel := 4;
                  BitStep := 2;
                  BitMask := 3;
                  SetFormat(uif_g8);
                end;
                4:
                begin
                  BitPerPixel := 2;
                  BitStep := 4;
                  BitMask := 15;
                  SetFormat(uif_g8);
                end;
                8: SetFormat(uif_g8);
                16: SetFormat(uif_g16);
              end;
            end;
            ctTrueColor:
            begin
              if not (ChunkIHDR.BitDepth in [8, 16]) then Exit;
              PixelDataSize := 3 * ChunkIHDR.BitDepth div 8;
              case ChunkIHDR.BitDepth of
                8: SetFormat(uif_r8g8b8);
                16: SetFormat(uif_r16g16b16);
              end;
            end;
            ctIndexedColor:
            begin
              if not (ChunkIHDR.BitDepth in [1, 2, 4, 8]) then Exit;
              PixelDataSize := 1;
              SetFormat(uif_r8g8b8);
              case ChunkIHDR.BitDepth of
                1:
                begin
                  BitPerPixel := 8;
                  BitStep := 1;
                  BitMask := 1;
                end;
                2:
                begin
                  BitPerPixel := 4;
                  BitStep := 2;
                  BitMask := 3;
                end;
                4:
                begin
                  BitPerPixel := 2;
                  BitStep := 4;
                  BitMask := 15;
                end;
              end;
            end;
            ctGrayscaleAlpha:
            begin
              if not (ChunkIHDR.BitDepth in [8, 16]) then Exit;
              PixelDataSize := 2 * ChunkIHDR.BitDepth div 8;
              case ChunkIHDR.BitDepth of
                8: SetFormat(uif_g8a8);
                16: SetFormat(uif_g16a16);
              end;
            end;
            ctTrueColorAlpha:
            begin
              if not (ChunkIHDR.BitDepth in [8, 16]) then Exit;
              PixelDataSize := 4 * ChunkIHDR.BitDepth div 8;
              case ChunkIHDR.BitDepth of
                8: SetFormat(uif_r8g8b8a8);
                16: SetFormat(uif_r16g16b16a16);
              end;
            end;
            else
            Exit;
          end;
        end
        else if (Chunk.ChunkType = 'IEND') then
        begin
          KeepReading := False;
        end
        else if (Chunk.ChunkType = 'PLTE') then
        begin
          SetLength(ChunkPLTE.Entries, Chunk.ChunkLength div 3);
          Move(Chunk.ChunkData^, ChunkPLTE.Entries[0], Chunk.ChunkLength);
        end
        else if (Chunk.ChunkType = 'IDAT') then
        begin
          SetLength(ChunkIDAT, Length(ChunkIDAT) + 1);
          SetLength(ChunkIDAT[High(ChunkIDAT)], Chunk.ChunkLength);
          Move(Chunk.ChunkData^, ChunkIDAT[High(ChunkIDAT)][0], Chunk.ChunkLength);
        end
        else if (Chunk.ChunkType = 'tRNS') then
        begin
          Transp := True;
          case ChunkIHDR.ColorType of
            ctGrayscale:
            begin
              TranspG := Swap16(PUInt16(Chunk.ChunkData)^);
              if Format = uif_g8 then
              SetFormat(uif_g8a8)
              else
              SetFormat(uif_g16a16);
            end;
            ctTrueColor:
            begin
              for i := 0 to 2 do
              TranspRGB[i] := Swap16(PUInt16(Chunk.ChunkData)^);
              if Format = uif_r8g8b8 then
              SetFormat(uif_r8g8b8a8)
              else
              SetFormat(uif_r16g16b16a16);
            end;
            ctIndexedColor:
            begin
              SetLength(TranspPalette, Chunk.ChunkLength);
              Move(Chunk.ChunkData^, TranspPalette[0], Chunk.ChunkLength);
              SetFormat(uif_r8g8b8a8);
            end;
            ctGrayscaleAlpha, ctTrueColorAlpha: Exit;
          end;
        end;
      end;
    end;
    CompressedData := TMemoryStream.Create;
    DecompressedData := TMemoryStream.Create;
    try
      CompressedData.Position := 0;
      DecompressedData.Position := 0;
      for i := 0 to High(ChunkIDAT) do
      begin
        CompressedData.Write(ChunkIDAT[i][0], Length(ChunkIDAT[i]));
      end;
      Decompress(CompressedData.Memory, CompressedData.Size, DecompressedData);
      _Width := ChunkIHDR.Width;
      _Height := ChunkIHDR.Height;
      DataAlloc;
      DecompressedData.Position := 0;
      case ChunkIHDR.InterlaceMethod of
        inAdam7: begin PassStart := 1; PassEnd := 7; end;
        else begin PassStart := 0; PassEnd := 0; end;
      end;
      DataPtr := DecompressedData.Memory;
      for Pass := PassStart to PassEnd do
      begin
        PassRows := _Height div RowOffset[Pass];
        if (_Height mod RowOffset[Pass]) > RowStart[Pass] then Inc(PassRows);
        PassCols := _Width div ColOffset[Pass];
        if (_Width mod ColOffset[Pass]) > ColStart[Pass] then Inc(PassCols);
        if (PassRows > 0) and (PassCols > 0) then
        begin
          ScanlineSize := PixelDataSize * PassCols;
          ScanlinePrev := nil;
          ScanlineCur := DataPtr;
          if ChunkIHDR.BitDepth < 8 then
          begin
            if ScanlineSize mod BitPerPixel > 0 then
            ScanlineSize := (ScanlineSize div BitPerPixel + 1)
            else
            ScanlineSize := (ScanlineSize div BitPerPixel);
          end;
          Inc(DataPtr, (Integer(ScanlineSize) + 1) * PassRows);
          y := RowStart[Pass];
          for j := 0 to PassRows - 1 do
          begin
            CurFilter := TFilter(ScanlineCur^[0]);
            {$push}
            {$hints off}
            ScanlineCur := PUInt8Arr(PtrUInt(ScanlineCur) + 1);
            {$pop}
            if ChunkIHDR.BitDepth > 8 then
            for i := 0 to ScanlineSize div 2 - 1 do
            PWord(@ScanlineCur^[i * 2])^ := Swap16(PUInt16(@ScanlineCur^[i * 2])^);
            x := ColStart[Pass];
            case CurFilter of
              flSub:
              for i := 0 to ScanlineSize - 1 do
              ScanlineCur^[i] := FilterSub(ScanlineCur^[i], i);
              flUp:
              for i := 0 to ScanlineSize - 1 do
              ScanlineCur^[i] := FilterUp(ScanlineCur^[i], i);
              flAverage:
              for i := 0 to ScanlineSize - 1 do
              ScanlineCur^[i] := FilterAverage(ScanlineCur^[i], i);
              flPaeth:
              for i := 0 to ScanlineSize - 1 do
              ScanlineCur^[i] := FilterPaeth(ScanlineCur^[i], i);
            end;
            if ChunkIHDR.ColorType = ctIndexedColor then
            begin
              if ChunkIHDR.BitDepth < 8 then
              begin
                for i := 0 to PassCols - 1 do
                begin
                  BitCur := (ScanlineCur^[i div BitPerPixel] shr (8 - (i mod BitPerPixel + 1) * BitStep)) and BitMask;
                  PUInt8(_Data + (y * _Width + x) * _BPP + 0)^ := ChunkPLTE.Entries[BitCur].r;
                  PUInt8(_Data + (y * _Width + x) * _BPP + 1)^ := ChunkPLTE.Entries[BitCur].g;
                  PUInt8(_Data + (y * _Width + x) * _BPP + 2)^ := ChunkPLTE.Entries[BitCur].b;
                  if Transp then
                  begin
                    if BitCur > High(TranspPalette) then
                    PByte(_Data + (y * _Width + x) * _BPP + 3)^ := $ff
                    else
                    PByte(_Data + (y * _Width + x) * _BPP + 3)^ := TranspPalette[BitCur];
                  end;
                  x := x + ColOffset[Pass];
                end;
              end
              else //8 or 16 bit
              begin
                for i := 0 to PassCols - 1 do
                begin
                  PUInt8(_Data + (y * _Width + x) * _BPP + 0)^ := ChunkPLTE.Entries[ScanlineCur^[i]].r;
                  PUInt8(_Data + (y * _Width + x) * _BPP + 1)^ := ChunkPLTE.Entries[ScanlineCur^[i]].g;
                  PUInt8(_Data + (y * _Width + x) * _BPP + 2)^ := ChunkPLTE.Entries[ScanlineCur^[i]].b;
                  if Transp then
                  begin
                    if ScanlineCur^[i] > High(TranspPalette) then
                    PUInt8(_Data + (y * _Width + x) * _BPP + 3)^ := $ff
                    else
                    PUInt8(_Data + (y * _Width + x) * _BPP + 3)^ := TranspPalette[ScanlineCur^[i]];
                  end;
                  x := x + ColOffset[Pass];
                end;
              end;
            end
            else //non indexed
            begin
              if ChunkIHDR.BitDepth < 8 then
              begin
                for i := 0 to PassCols - 1 do
                begin
                  BitCur := (ScanlineCur^[i div BitPerPixel] shr (8 - (i mod BitPerPixel + 1) * BitStep)) and BitMask;
                  if Transp then
                  begin
                    if ChunkIHDR.ColorType = ctGrayscale then
                    begin
                      if BitCur = TranspG and BitMask then
                      PUInt8(_Data + (y * _Width + x) * _BPP + 1)^ := 0
                      else
                      PUInt8(_Data + (y * _Width + x) * _BPP + 1)^ := $ff;
                    end;
                  end;
                  BitCur := UnpackBits(BitCur);
                  PUInt8(_Data + (y * _Width + x) * _BPP)^ := BitCur;
                  x := x + ColOffset[Pass];
                end;
              end
              else //8 or 16 bit
              begin
                for i := 0 to PassCols - 1 do
                begin
                  for b := 0 to PixelDataSize - 1 do
                  PUInt8(_Data + (y * _Width + x) * _BPP + b)^ := ScanlineCur^[i * PixelDataSize + b];
                  if Transp then
                  begin
                    if ChunkIHDR.ColorType = ctGrayscale then
                    begin
                      if ChunkIHDR.BitDepth = 8 then
                      begin
                        if ScanlineCur^[i * PixelDataSize] = UInt8(TranspG and $ff) then
                        PUInt8Arr(_Data)^[(y * _Width + x) * _BPP + PixelDataSize] := 0
                        else
                        PUInt8Arr(_Data)^[(y * _Width + x) * _BPP + PixelDataSize] := $ff;
                      end
                      else
                      begin
                        if PUInt16(@ScanlineCur^[i * PixelDataSize])^ = TranspG then
                        PUInt16(@PByteArray(_Data)^[(y * _Width + x) * _BPP + PixelDataSize])^ := 0
                        else
                        PUInt16(@PByteArray(_Data)^[(y * _Width + x) * _BPP + PixelDataSize])^ := $ffff;
                      end;
                    end
                    else
                    begin
                      if ChunkIHDR.BitDepth = 8 then
                      begin
                        if (ScanlineCur^[i * PixelDataSize + 0] = UInt8(TranspRGB[0] and $ff))
                        and (ScanlineCur^[i * PixelDataSize + 1] = UInt8(TranspRGB[1] and $ff))
                        and (ScanlineCur^[i * PixelDataSize + 2] = UInt8(TranspRGB[2] and $ff)) then
                        PUInt8(_Data + (y * _Width + x) * _BPP + 3)^ := 0
                        else
                        PUInt8(_Data + (y * _Width + x) * _BPP + 3)^ := $ff;
                      end
                      else
                      begin
                        if (PUInt16(@ScanlineCur^[i * PixelDataSize + 0])^ = TranspRGB[0])
                        and (PUInt16(@ScanlineCur^[i * PixelDataSize + 2])^ = TranspRGB[1])
                        and (PUInt16(@ScanlineCur^[i * PixelDataSize + 4])^ = TranspRGB[2]) then
                        PUInt16(_Data + (y * _Width + x) * _BPP + 6)^ := 0
                        else
                        PUInt16(_Data + (y * _Width + x) * _BPP + 6)^ := $ffff;
                      end;
                    end;
                  end;
                  x := x + ColOffset[Pass];
                end;
              end;
            end;
            ScanlinePrev := ScanlineCur;
            {$Hints off}
            ScanlineCur := PUInt8Arr(PtrUInt(ScanlineCur) + ScanlineSize);
            {$Hints on}
            y := y + RowOffset[Pass];
          end;
        end;
      end;
    finally
      CompressedData.Free;
      DecompressedData.Free;
    end;
  end;
end;

procedure TUImageDataPNG.Save(const StreamHelper: TUStreamHelper);
  var sh: TUStreamHelper absolute StreamHelper;
  var ChunkType: array[0..3] of AnsiChar;
  var ChunkCompress: Boolean;
  var ChunkStreamDecompressed: TMemoryStream;
  var ChunkStreamCompressed: TMemoryStream;
  procedure ChunkBegin(const ChunkName: AnsiString);
  begin
    ChunkType := ChunkName[1] + ChunkName[2] + ChunkName[3] + ChunkName[4];
    ChunkCompress := ChunkType = 'IDAT';
    ChunkStreamDecompressed := TMemoryStream.Create;
    ChunkStreamCompressed := TMemoryStream.Create;
  end;
  procedure ChunkEnd;
  begin
    ChunkStreamDecompressed.Position := 0;
    ChunkStreamCompressed.Write(ChunkType, 4);
    if ChunkStreamDecompressed.Size > 0 then
    begin
      if ChunkCompress then
      Compress(ChunkStreamDecompressed.Memory, ChunkStreamDecompressed.Size, ChunkStreamCompressed)
      else
      ChunkStreamCompressed.Write(ChunkStreamDecompressed.Memory^, ChunkStreamDecompressed.Size);
    end;
    sh.WriteInt32(Swap32(ChunkStreamCompressed.Size - 4));
    ChunkStreamCompressed.Position := 0;
    sh.WriteBuffer(ChunkStreamCompressed.Memory, ChunkStreamCompressed.Size);
    ChunkStreamCompressed.Position := 0;
    sh.WriteUInt32(Swap32(GetCRC(ChunkStreamCompressed.Memory, ChunkStreamCompressed.Size)));
    ChunkStreamDecompressed.Free;
    ChunkStreamCompressed.Free;
  end;
  procedure ChunkWrite(const Buffer: Pointer; const Size: Int64);
  begin
    ChunkStreamDecompressed.WriteBuffer(Buffer^, Size);
  end;
  procedure ChunkWriteInt4U(const v: UInt32);
  begin
    ChunkWrite(@v, 4);
  end;
  procedure ChunkWriteInt4S(const v: Int32);
  begin
    ChunkWrite(@v, 4);
  end;
  procedure ChunkWriteInt2U(const v: UInt16);
  begin
    ChunkWrite(@v, 2);
  end;
  procedure ChunkWriteInt2S(const v: Int16);
  begin
    ChunkWrite(@v, 2);
  end;
  procedure ChunkWriteInt1U(const v: UInt8);
  begin
    ChunkWrite(@v, 1);
  end;
  procedure ChunkWriteInt1S(const v: Int8);
  begin
    ChunkWrite(@v, 1);
  end;
  var ImageData: Pointer;
  var ImageDataSize: Int32;
  var pb: PUInt8;
  var i, j: Int32;
begin
  sh.WriteBuffer(@PNGHeader[1], 8);
  ChunkBegin('IHDR');
  ChunkWriteInt4S(Swap32(_Width));
  ChunkWriteInt4S(Swap32(_Height));
  ChunkWriteInt1U(8);
  ChunkWriteInt1U(UInt8(ctTrueColorAlpha));
  ChunkWriteInt1U(0);
  ChunkWriteInt1U(0);
  ChunkWriteInt1U(0);
  ChunkEnd;
  ChunkBegin('IDAT');
  ImageDataSize := (_Width * 4 + 1) * _Height;
  GetMem(ImageData, ImageDataSize);
  pb := ImageData;
  for j := 0 to _Height - 1 do
  begin
    pb^ := 0; Inc(pb);
    for i := 0 to _Width - 1 do
    begin
      PUColor(pb)^ := Pixels[i, j];
      Inc(pb, 4);
    end;
  end;
  ChunkWrite(ImageData, ImageDataSize);
  FreeMem(ImageData, ImageDataSize);
  ChunkEnd;
  ChunkBegin('IEND');
  ChunkEnd;
end;
// TUImageDataPNG end

end.
