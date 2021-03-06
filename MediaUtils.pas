unit MediaUtils;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$modeswitch typehelpers}
{$optimization autoinline}
{$macro on}
{$warn 6058 off}
{$warn 5024 off}
{$warn 3123 off}
{$warn 3124 off}

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

  type TUVertexAttributeSemantic = (
    as_invalid,
    as_position,
    as_normal,
    as_tangent,
    as_binormal,
    as_color,
    as_texcoord
  );
  type TUVertexDataType = (
    dt_invalid,
    dt_bool,
    dt_int,
    dt_float
  );
  type TUVertexAttribute = record
    Semantic: TUVertexAttributeSemantic;
    DataType: TUVertexDataType;
    DataCount: UInt8;
    SetNumber: UInt8;
    class function Make(
      const ASemantic: TUVertexAttributeSemantic;
      const ADataType: TUVertexDataType;
      const ADataCount: UInt8;
      const ASetNumber: UInt8 = 0
    ): TUVertexAttribute; static;
    function Size: Int32;
  end;
  type TUVertexDescriptor = array of TUVertexAttribute;

  type TSceneDataOptions = (sdo_optimize, sdo_gen_normals, sdo_gen_tangents);
  type TSceneDataOptionsSet = set of TSceneDataOptions;

  type TUSceneData = class (TURefClass)
  public
    type TMeshSubsetInterface = class
    protected
      var _VertexData: Pointer;
      var _IndexData: Pointer;
      var _VertexCount: Int32;
      var _IndexCount: Int32;
      var _VertexSize: Int32;
      var _IndexSize: Int32;
      function GetVertexDescriptor: TUVertexDescriptor; virtual;
      function GetVertexBufferSize: Int32;
      function GetIndexBufferSize: Int32;
      function GetIndex(const Id: Int32): UInt32;
    public
      property VertexDescriptor: TUVertexDescriptor read GetVertexDescriptor;
      property VertexData: Pointer read _VertexData;
      property IndexData: Pointer read _IndexData;
      property VertexCount: Int32 read _VertexCount;
      property IndexCount: Int32 read _IndexCount;
      property VertexSize: Int32 read _VertexSize;
      property IndexSize: Int32 read _IndexSize;
      property VertexBufferSize: Int32 read GetVertexBufferSize;
      property IndexBufferSize: Int32 read GetIndexBufferSize;
      property Index[const Id: Int32]: UInt32 read GetIndex;
      destructor Destroy; override;
    end;
    type TMeshSubsetInterfaceList = array of TMeshSubsetInterface;
    type TMeshInterface = class
    private
      var _Subsets: TMeshSubsetInterfaceList;
    public
      property Subsets: TMeshSubsetInterfaceList read _Subsets;
      destructor Destroy; override;
    end;
    type TMeshInterfaceList = array of TMeshInterface;
    var _MeshList: TMeshInterfaceList;
  protected
    var _Options: TSceneDataOptionsSet;
  public
    property MeshList: TMeshInterfaceList read _MeshList;
    property Options: TSceneDataOptionsSet read _Options;
    class function CanLoad(const Stream: TStream): Boolean; virtual; overload;
    class function CanLoad(const FileName: String): Boolean; virtual; overload;
    class function CanLoad(const Buffer: Pointer; const Size: UInt32): Boolean; virtual; overload;
    class function CanLoad(const StreamHelper: TUStreamHelper): Boolean; virtual; abstract; overload;
    destructor Destroy; override;
    procedure Load(const Stream: TStream); virtual; overload;
    procedure Load(const FileName: String); virtual; overload;
    procedure Load(const Buffer: Pointer; const Size: UInt32); virtual; overload;
    procedure Load(const StreamHelper: TUStreamHelper); virtual; abstract; overload;
    constructor Create(const AOptions: TSceneDataOptionsSet = []);
  end;

  TUSceneDataDAE = class (TUSceneData)
  public
    type TColladaObject = class
    public
      type TObjectList = array of TColladaObject;
      type TClass = class of TColladaObject;
    private
      var _Tag: String;
      var _id: String;
      var _sid: String;
      var _Name: String;
      var _Scoped: Boolean;
      var _Parent: TColladaObject;
      var _Children: TObjectList;
      var _UserData: TObject;
      var _AutoFreeUserData: Boolean;
      procedure AddChild(const Child: TColladaObject); inline;
      procedure RemoveChild(const Child: TColladaObject); inline;
      procedure SetParent(const Value: TColladaObject); inline;
    protected
      procedure DumpBegin;
      procedure DumpEnd;
      procedure DumpData; virtual;
      procedure Resolve;
      procedure ResolveLinks; virtual;
      procedure Initialize;
      procedure InitializeObject; virtual;
      function ResolveObject(
        const Path: String;
        const ObjectClass: TClass
      ): TColladaObject;
    public
      property Tag: String read _Tag;
      property id: String read _id;
      property sid: String read _sid;
      property Name: String read _Name;
      property IsScoped: Boolean read _Scoped;
      property Parent: TColladaObject read _Parent write SetParent;
      property Children: TObjectList read _Children;
      property UserData: TObject read _UserData write _UserData;
      property AutoFreeUserData: Boolean read _AutoFreeUserData write _AutoFreeUserData;
      constructor Create(const XMLNode: TUXML; const AParent: TColladaObject);
      destructor Destroy; override;
      function GetRoot: TColladaObject;
      function Find(const Path: String): TColladaObject;
      function FindChild(const NodeID: String): TColladaObject;
      function FindChildRecursive(const NodeID: String): TColladaObject;
      procedure Dump;
    end;
    type TColladaInstance = class (TColladaObject)
    private
      var _Url: String;
    public
      property Url: String read _Url;
      constructor Create(const XMLNode: TUXML; const AParent: TColladaObject);
      destructor Destroy; override;
    end;
    type TColladaInstanceList = array of TColladaInstance;
    type TColladaNodeType = (nt_invalid, nt_node, nt_joint);
    type TColladaNode = class (TColladaObject)
    public
      type TNodeList = array of TColladaNode;
    private
      var _NodeType: TColladaNodeType;
      var _Layers: TUStrArr;
      var _Nodes: TNodeList;
      var _Instances: TColladaInstanceList;
    public
      var Matrix: TUMat;
      property NodeType: TColladaNodeType read _NodeType;
      property Layers: TUStrArr read _Layers;
      property Nodes: TNodeList read _Nodes;
      property Instances: TColladaInstanceList read _Instances;
      constructor Create(const XMLNode: TUXML; const AParent: TColladaObject);
      destructor Destroy; override;
      class function StringToNodeType(const NodeTypeName: String): TColladaNodeType;
    end;
    type TColladaNodeList = TColladaNode.TNodeList;
    type TColladaInput = class (TColladaObject)
    private
      var _Semantic: String;
      var _SourceRef: String;
      var _Source: TColladaObject;
      var _Offset: Int32;
      var _Set: Int32;
      function GetSize: UInt32; inline;
    protected
      procedure ResolveLinks; override;
    public
      property Semantic: String read _Semantic;
      property Source: TColladaObject read _Source;
      property Offset: Int32 read _Offset;
      property InputSet: Int32 read _Set;
      property Size: UInt32 read GetSize;
      constructor Create(const XMLNode: TUXML; const AParent: TColladaObject);
      destructor Destroy; override;
    end;
    type TColladaInputList = array of TColladaInput;
    type TColladaArrayType = (
      at_invalid,
      at_bool,
      at_float,
      at_idref,
      at_int,
      at_name,
      at_sidref,
      at_token
    );
    type TColladaDataArray = class (TColladaObject)
    private
      var _Data: array of UInt8;
      var _DataString: array of String;
      var _Count: Int32;
      var _ItemSize: Int32;
      var _ArrayType: TColladaArrayType;
      function GetAsBool(const Index: Int32): PBoolean; inline;
      function GetAsInt(const Index: Int32): PInt32; inline;
      function GetAsFloat(const Index: Int32): PUFloat; inline;
      function GetAsString(const Index: Int32): String; inline;
      function GetRawData(const Offset: Int32): Pointer; inline;
    public
      property ArrayType: TColladaArrayType read _ArrayType;
      property Count: Int32 read _Count;
      property ItemSize: Int32 read _ItemSize;
      property AsBool[const Index: Int32]: PBoolean read GetAsBool;
      property AsInt[const Index: Int32]: PInt32 read GetAsInt;
      property AsFloat[const Index: Int32]: PUFloat read GetAsFloat;
      property AsString[const Index: Int32]: String read GetAsString;
      property RawData[const Offset: Int32]: Pointer read GetRawData;
      constructor Create(const XMLNode: TUXML; const AParent: TColladaObject);
      destructor Destroy; override;
      class function NodeNameToArrayType(const NodeName: String): TColladaArrayType;
      class function TypeNameToArrayType(const TypeName: String): TColladaArrayType;
      class function IsDataArrayNode(const XMLNode: TUXML): Boolean;
    end;
    type TColladaAccessor = class (TColladaObject)
    public
      type TParam = record
        Name: String;
        ParamType: TColladaArrayType;
      end;
      type TParamArr = array[0..High(UInt16)] of TParam;
      type PParamArr = ^TParamArr;
    private
      var _SourceRef: String;
      var _Source: TColladaDataArray;
      var _Count: Int32;
      var _Stride: Int32;
      var _Params: array of TParam;
      function GetParams: PParamArr;
    protected
      procedure ResolveLinks; override;
    public
      property Source: TColladaDataArray read _Source;
      property Count: Int32 read _Count;
      property Stride: Int32 read _Stride;
      property Params: PParamArr read GetParams;
      constructor Create(const XMLNode: TUXML; const AParent: TColladaObject);
      destructor Destroy; override;
    end;
    type TColladaSource = class (TColladaObject)
    private
      var _DataArray: TColladaDataArray;
      var _Accessor: TColladaAccessor;
    public
      property DataArray: TColladaDataArray read _DataArray;
      property Accessor: TColladaAccessor read _Accessor;
      constructor Create(const XMLNode: TUXML; const AParent: TColladaObject);
      destructor Destroy; override;
    end;
    type TColladaSourceList = array of TColladaSource;
    type TColladaVertices = class (TColladaObject)
    private
      var _Inputs: TColladaInputList;
    public
      property Inputs: TColladaInputList read _Inputs;
      constructor Create(const XMLNode: TUXML; const AParent: TColladaObject);
      destructor Destroy; override;
    end;
    type TColladaTriangles = class (TColladaObject)
    private
      var _MaterialRef: String;
      var _Count: Int32;
      var _Inputs: TColladaInputList;
      var _Indices: array of Int32;
      var _VertexLayout: TColladaInputList;
      var _InputStride: Int32;
      function GetVertexSize: Int32;
      function GetIndices: PUInt32Arr; inline;
      function GetVertexDescriptor: TUVertexDescriptor;
      function GetInputSourceCount(const Index: UInt32): UInt32; inline;
    protected
      procedure InitializeObject; override;
    public
      property Count: Int32 read _Count;
      property Inputs: TColladaInputList read _Inputs;
      property Indices: PUInt32Arr read GetIndices;
      property VertexLayout: TColladaInputList read _VertexLayout;
      property VertexSize: Int32 read GetVertexSize;
      property VertexDescriptor: TUVertexDescriptor read GetVertexDescriptor;
      property Material: String read _MaterialRef;
      property InputSourceCount[const Index: UInt32]: UInt32 read GetInputSourceCount;
      property InputStride: Int32 read _InputStride;
      constructor Create(const XMLNode: TUXML; const AParent: TColladaObject);
      destructor Destroy; override;
      function CopyInputData(const Target: Pointer; const Input: TColladaInput; const Index: Int32): Pointer;
    end;
    type TColladaTrianglesList = array of TColladaTriangles;
    type TColladaMesh = class (TColladaObject)
    private
      var _Sources: TColladaSourceList;
      var _Vertices: TColladaVertices;
      var _TrianglesList: TColladaTrianglesList;
    public
      property Sources: TColladaSourceList read _Sources;
      property Vertices: TColladaVertices read _Vertices;
      property TrianglesList: TColladaTrianglesList read _TrianglesList;
      constructor Create(const XMLNode: TUXML; const AParent: TColladaObject);
      destructor Destroy; override;
    end;
    type TColladaMeshList = array of TColladaMesh;
    type TColladaImage = class (TColladaObject)
    private
      var _Source: String;
    public
      property Source: String read _Source;
      constructor Create(const XMLNode: TUXML; const AParent: TColladaObject);
      destructor Destroy; override;
    end;
    type TColladaImageList = array of TColladaImage;
    type TColladaEffectProfileParamType = (pt_invalid, pt_surface, pt_sampler, pt_float, pt_float2, pt_float3, pt_float4);
    type TColladaEffectProfileParam = class (TColladaObject)
    public
      type TDataSurface = class
        var InitFrom: String;
        var Image: TColladaImage;
      end;
      type TDataSampler = class
        var Source: String;
        var Surface: TDataSurface;
      end;
      type TDataFloat = class
        var Value: TUFloat;
      end;
      type TDataFloat2 = class
        var Value: TUVec2;
      end;
      type TDataFloat3 = class
        var Value: TUVec3;
      end;
      type TDataFloat4 = class
        var Value: TUVec4;
      end;
    private
      var _ParamType: TColladaEffectProfileParamType;
      var _Data: TObject;
    public
      property ParamType: TColladaEffectProfileParamType read _ParamType;
      function AsSurface: TDataSurface; inline;
      function AsSampler: TDataSampler; inline;
      function AsFloat: TDataFloat; inline;
      function AsFloat2: TDataFloat2; inline;
      function AsFloat3: TDataFloat3; inline;
      function AsFloat4: TDataFloat4; inline;
      constructor Create(const XMLNode: TUXML; const AParent: TColladaObject);
      destructor Destroy; override;
    end;
    type TColladaEffectProfileParamList = array of TColladaEffectProfileParam;
    type TColladaEffectProfile = class (TColladaObject)
    private
      var _Params: TColladaEffectProfileParamList;
    protected
      procedure ResolveLinks; override;
    public
      property Params: TColladaEffectProfileParamList read _Params;
      constructor Create(const XMLNode: TUXML; const AParent: TColladaObject);
      destructor Destroy; override;
    end;
    type TColladaEffect = class (TColladaObject)
    private
      var _Profile: TColladaEffectProfile;
    public
      property Profile: TColladaEffectProfile read _Profile;
      constructor Create(const XMLNode: TUXML; const AParent: TColladaObject);
      destructor Destroy; override;
    end;
    type TColladaEffectList = array of TColladaEffect;
    type TColladaInstanceEffect = class (TColladaInstance)
    private
      var _Effect: TColladaEffect;
    protected
      procedure ResolveLinks; override;
    public
      property Effect: TColladaEffect read _Effect;
      constructor Create(const XMLNode: TUXML; const AParent: TColladaObject);
      destructor Destroy; override;
    end;
    type TColladaMaterial = class (TColladaObject)
    private
      var _InstanceEffect: TColladaInstanceEffect;
    public
      property InstanceEffect: TColladaInstanceEffect read _InstanceEffect;
      constructor Create(const XMLNode: TUXML; const AParent: TColladaObject);
      destructor Destroy; override;
    end;
    type TColladaMaterialList = array of TColladaMaterial;
    type TColladaGeometry = class (TColladaObject)
    private
      var _Meshes: TColladaMeshList;
    public
      property Meshes: TColladaMeshList read _Meshes;
      constructor Create(const XMLNode: TUXML; const AParent: TColladaObject);
      destructor Destroy; override;
    end;
    type TColladaGeometryList = array of TColladaGeometry;
    type TColladaMorph = class (TColladaObject)
    public
      constructor Create(const XMLNode: TUXML; const AParent: TColladaObject);
      destructor Destroy; override;
    end;
    type TColladaJoints = class (TColladaObject)
    public
      type TJoint = record
        JointName: String;
        BindPose: TUMat;
      end;
      type TJoints = array of TJoint;
    private
      var _Inputs: TColladaInputList;
      var _Joints: TJoints;
      function FindInput(const Semantic: String): TColladaInput;
    protected
      procedure ResolveLinks; override;
    public
      property Inputs: TColladaInputList read _Inputs;
      property Joints: TJoints read _Joints;
      constructor Create(const XMLNode: TUXML; const AParent: TColladaObject);
      destructor Destroy; override;
    end;
    type TColladaVertexWeights = class (TColladaObject)
    public
      type TVertexJointReference = record
        JointIndex: Int32;
        JointWeight: TUFloat;
      end;
      type TVertexJointReferenceArr = array of array of TVertexJointReference;
    private
      var _VCount: Int32;
      var _Inputs: TColladaInputList;
      var _VertexWeights: TVertexJointReferenceArr;
      var _Indices: array of Int32;
      function FindInput(const Semantic: String): TColladaInput;
    protected
      procedure ResolveLinks; override;
    public
      property VCount: Int32 read _VCount;
      property Inputs: TColladaInputList read _Inputs;
      property Weights: TVertexJointReferenceArr read _VertexWeights;
      constructor Create(const XMLNode: TUXML; const AParent: TColladaObject);
      destructor Destroy; override;
    end;
    type TColladaSkin = class (TColladaObject)
    private
      var _GeometryRef: String;
      var _Geometry: TColladaGeometry;
      var _BindShapeMatrix: TUMat;
      var _Sources: TColladaSourceList;
      var _Joints: TColladaJoints;
      var _VertexWeights: TColladaVertexWeights;
    protected
      procedure ResolveLinks; override;
    public
      property Geometry: TColladaGeometry read _Geometry;
      property BindShapeMatrix: TUMat read _BindShapeMatrix;
      property Joints: TColladaJoints read _Joints;
      property VertexWeights: TColladaVertexWeights read _VertexWeights;
      constructor Create(const XMLNode: TUXML; const AParent: TColladaObject);
      destructor Destroy; override;
    end;
    type TColladaControllerType = (ct_invalid, ct_skin, ct_morph);
    type TColladaController = class (TColladaObject)
    private
      var _ControllerType: TColladaControllerType;
      var _Controller: TColladaObject;
      function GetAsSkin: TColladaSkin; inline;
      function GetAsMorph: TColladaMorph; inline;
    public
      property Controller: TColladaObject read _Controller;
      property ControllerType: TColladaControllerType read _ControllerType;
      property AsSkin: TColladaSkin read GetAsSkin;
      property AsMorph: TColladaMorph read GetAsMorph;
      constructor Create(const XMLNode: TUXML; const AParent: TColladaObject);
      destructor Destroy; override;
    end;
    type TColladaControllerList = array of TColladaController;
    type TColladaAnimationInterpolation = (ai_step, ai_linear, ai_bezier);
    type TColladaAnimationSampler = class (TColladaObject)
    public
      type TKey = record
        Time: TUFloat;
        Value: Pointer;
        TangentIn: array of TUFloat;
        TangentOut: array of TUFloat;
        Interpolation: TColladaAnimationInterpolation;
      end;
      type PKey = ^TKey;
    private
      var _Data: Pointer;
      var _Inputs: TColladaInputList;
      var _Keys: array of TKey;
      var _DataType: TColladaArrayType;
      var _DataStride: UInt32;
      var _DataSize: UInt32;
      function GetKey(const Index: Int32): PKey; inline;
      function GetKeyCount: Int32; inline;
      function FindKey(const Time: TUFloat): Int32;
      function GetMaxTime: TUFloat; inline;
      function GetSampleSize: UInt32; inline;
    protected
      procedure ResolveLinks; override;
      procedure DumpData; override;
    public
      property Inputs: TColladaInputList read _Inputs;
      property MaxTime: TUFloat read GetMaxTime;
      property SampleSize: UInt32 read GetSampleSize;
      property DataType: TColladaArrayType read _DataType;
      property Keys[const Index: Int32]: PKey read GetKey;
      property KeyCount: Int32 read GetKeyCount;
      procedure SampleData(const Output: Pointer; const Time: TUFloat; const Loop: Boolean = False);
      function SampleAsFloat(const Time: TUFloat; const Loop: Boolean = false): TUFloat;
      function SampleAsFloat2(const Time: TUFloat; const Loop: Boolean = false): TUVec2;
      function SampleAsFloat3(const Time: TUFloat; const Loop: Boolean = false): TUVec3;
      function SampleAsFloat4(const Time: TUFloat; const Loop: Boolean = false): TUVec4;
      function SampleAsMatrix(const Time: TUFloat; const Loop: Boolean = false): TUMat;
      constructor Create(const XMLNode: TUXML; const AParent: TColladaObject);
      destructor Destroy; override;
    end;
    type TColladaAnimationSamplerList = array of TColladaAnimationSampler;
    type TColladaAnimationChannel = class (TColladaObject)
    private
      var _SourceRef: String;
      var _TargetRef: String;
      var _Sampler: TColladaAnimationSampler;
      var _Target: TColladaObject;
      var _TargetProperty: String;
      function GetMaxTime: TUFloat; inline;
    protected
      procedure ResolveLinks; override;
    public
      property Sampler: TColladaAnimationSampler read _Sampler;
      property Target: TColladaObject read _Target;
      property TargetProperty: String read _TargetProperty;
      property MaxTime: TUFloat read GetMaxTime;
      constructor Create(const XMLNode: TUXML; const AParent: TColladaObject);
      destructor Destroy; override;
    end;
    type TColladaAnimationChannelList = array of TColladaAnimationChannel;
    type TColladaAnimation = class (TColladaObject)
    public
      type TAnimationList = array of TColladaAnimation;
    private
      var _Animations: TAnimationList;
      var _Sources: TColladaSourceList;
      var _Samplers: TColladaAnimationSamplerList;
      var _Channels: TColladaAnimationChannelList;
    public
      property Animations: TAnimationList read _Animations;
      property Sources: TColladaSourceList read _Sources;
      property Samplers: TColladaAnimationSamplerList read _Samplers;
      property Channels: TColladaAnimationChannelList read _Channels;
      constructor Create(const XMLNode: TUXML; const AParent: TColladaObject);
      destructor Destroy; override;
    end;
    type TColladaAnimationList = TColladaAnimation.TAnimationList;
    type TColladaCamera = class (TColladaObject)
    private
      var _FOV: TUFloat;
      var _Aspect: TUFloat;
      var _Near: TUFloat;
      var _Far: TUFloat;
    public
      property FOV: TUFloat read _FOV;
      property Aspect: TUFloat read _Aspect;
      property ClipNear: TUFloat read _Near;
      property ClipFar: TUFloat read _Far;
      constructor Create(const XMLNode: TUXML; const AParent: TColladaObject);
      destructor Destroy; override;
    end;
    type TColladaCameraList = array of TColladaCamera;
    type TColladaLightType = (lt_ambient, lt_directional, lt_point, lt_spot);
    type TColladaLight = class (TColladaObject)
    private
      var _LightType: TColladaLightType;
      var _Color: TUVec3;
      var _AttenuationConstant: TUFloat;
      var _AttenuationLinear: TUFloat;
      var _AttenuationQuadratic: TUFloat;
      var _FalloffAngle: TUFloat;
      var _FalloffExponent: TUFloat;
    public
      property LightType: TColladaLightType read _LightType;
      property Color: TUVec3 read _Color;
      property AttenuationConstant: TUFloat read _AttenuationConstant;
      property AttenuationLinear: TUFloat read _AttenuationLinear;
      property AttenuationQuadratic: TUFloat read _AttenuationQuadratic;
      property FalloffAngle: TUFloat read _FalloffAngle;
      property FalloffExponent: TUFloat read _FalloffExponent;
      constructor Create(const XMLNode: TUXML; const AParent: TColladaObject);
      destructor Destroy; override;
    end;
    type TColladaLightList = array of TColladaLight;
    type TColladaInstanceMaterial = class (TColladaObject)
    private
      var _Symbol: String;
      var _Target: String;
      var _Material: TColladaMaterial;
    protected
      procedure ResolveLinks; override;
    public
      property Material: TColladaMaterial read _Material;
      property Symbol: String read _Symbol;
      constructor Create(const XMLNode: TUXML; const AParent: TColladaObject);
      destructor Destroy; override;
    end;
    type TColladaInstanceMaterialList = array of TColladaInstanceMaterial;
    type TColladaInstanceGeometry = class (TColladaInstance)
    private
      var _Geometry: TColladaGeometry;
      var _MaterialBindings: TColladaInstanceMaterialList;
    protected
      procedure ResolveLinks; override;
    public
      property Geometry: TColladaGeometry read _Geometry;
      property MaterialBindings: TColladaInstanceMaterialList read _MaterialBindings;
      constructor Create(const XMLNode: TUXML; const AParent: TColladaObject);
      destructor Destroy; override;
    end;
    type TColladaInstanceController = class (TColladaInstance)
    private
      var _SkeletonRef: String;
      var _Skeleton: TColladaNode;
      var _Controller: TColladaController;
      var _MaterialBindings: TColladaInstanceMaterialList;
    protected
      procedure ResolveLinks; override;
    public
      property Skeleton: TColladaNode read _Skeleton;
      property Controller: TColladaController read _Controller;
      property MaterialBindings: TColladaInstanceMaterialList read _MaterialBindings;
      constructor Create(const XMLNode: TUXML; const AParent: TColladaObject);
      destructor Destroy; override;
    end;
    type TColladaInstanceCamera = class (TColladaInstance)
    private
      var _Camera: TColladaCamera;
    protected
      procedure ResolveLinks; override;
    public
      property Camera: TColladaCamera read _Camera;
      constructor Create(const XMLNode: TUXML; const AParent: TColladaObject);
      destructor Destroy; override;
    end;
    type TColladaInstanceLight = class (TColladaInstance)
    private
      var _Light: TColladaLight;
    protected
      procedure ResolveLinks; override;
    public
      property Light: TColladaLight read _Light;
      constructor Create(const XMLNode: TUXML; const AParent: TColladaObject);
      destructor Destroy; override;
    end;
    type TColladaVisualScene = class (TColladaObject)
    private
      var _Nodes: TColladaNodeList;
    public
      property Nodes: TColladaNodeList read _Nodes;
      constructor Create(const XMLNode: TUXML; const AParent: TColladaObject);
      destructor Destroy; override;
    end;
    type TColladaVisualSceneList = array of TColladaVisualScene;
    type TColladaLibraryAnimations = class (TColladaObject)
    private
      var _Animations: TColladaAnimationList;
    public
      property Animations: TColladaAnimationList read _Animations;
      constructor Create(const XMLNode: TUXML; const AParent: TColladaObject);
      destructor Destroy; override;
    end;
    type TColladaLibraryMaterials = class (TColladaObject)
    private
      var _Materials: TColladaMaterialList;
    public
      property Materials: TColladaMaterialList read _Materials;
      constructor Create(const XMLNode: TUXML; const AParent: TColladaObject);
      destructor Destroy; override;
    end;
    type TColladaLibraryEffects = class (TColladaObject)
    private
      var _Effects: TColladaEffectList;
    public
      property Effects: TColladaEffectList read _Effects;
      constructor Create(const XMLNode: TUXML; const AParent: TColladaObject);
      destructor Destroy; override;
    end;
    type TColladaLibraryImages = class (TColladaObject)
    private
      var _Images: TColladaImageList;
    public
      property Images: TColladaImageList read _Images;
      constructor Create(const XMLNode: TUXML; const AParent: TColladaObject);
      destructor Destroy; override;
    end;
    type TColladaLibraryGeometries = class (TColladaObject)
    private
      var _Geometries: TColladaGeometryList;
    public
      property Geometries: TColladaGeometryList read _Geometries;
      constructor Create(const XMLNode: TUXML; const AParent: TColladaObject);
      destructor Destroy; override;
    end;
    type TColladaLibraryControllers = class (TColladaObject)
    private
      var _Controllers: TColladaControllerList;
    public
      property Controllers: TColladaControllerList read _Controllers;
      constructor Create(const XMLNode: TUXML; const AParent: TColladaObject);
      destructor Destroy; override;
    end;
    type TColladaLibraryCameras = class (TColladaObject)
    private
      var _Cameras: TColladaCameraList;
    public
      property Cameras: TColladaCameraList read _Cameras;
      constructor Create(const XMLNode: TUXML; const AParent: TColladaObject);
      destructor Destroy; override;
    end;
    type TColladaLibraryLights = class (TColladaObject)
    private
      var _Lights: TColladaLightList;
    public
      property Lights: TColladaLightList read _Lights;
      constructor Create(const XMLNode: TUXML; const AParent: TColladaObject);
      destructor Destroy; override;
    end;
    type TColladaLibraryVisualScenes = class (TColladaObject)
    private
      var _VisualScenes: TColladaVisualSceneList;
    public
      property VisualScenes: TColladaVisualSceneList read _VisualScenes;
      constructor Create(const XMLNode: TUXML; const AParent: TColladaObject);
      destructor Destroy; override;
    end;
    type TColladaInstanceVisualScene = class (TColladaInstance)
    private
      var _VisualScene: TColladaVisualScene;
    protected
      procedure ResolveLinks; override;
    public
      property VisualScene: TColladaVisualScene read _VisualScene;
      constructor Create(const XMLNode: TUXML; const AParent: TColladaObject);
      destructor Destroy; override;
    end;
    type TColladaScene = class (TColladaObject)
    private
      var _VisualScene: TColladaInstanceVisualScene;
    public
      property VisualScene: TColladaInstanceVisualScene read _VisualScene;
      constructor Create(const XMLNode: TUXML; const AParent: TColladaObject);
      destructor Destroy; override;
    end;
    type TColladaAsset = class (TColladaObject)
    private
      var _UpAxis: TUSwizzle;
    public
      property UpAxis: TUSwizzle read _UpAxis;
      constructor Create(const XMLNode: TUXML; const AParent: TColladaObject);
      destructor Destroy; override;
    end;
    type TColladaRoot = class (TColladaObject)
    private
      var _Asset: TColladaAsset;
      var _LibMaterials: TColladaLibraryMaterials;
      var _LibEffects: TColladaLibraryEffects;
      var _LibImages: TColladaLibraryImages;
      var _LibGeometries: TColladaLibraryGeometries;
      var _LibControllers: TColladaLibraryControllers;
      var _LibAnimations: TColladaLibraryAnimations;
      var _LibCameras: TColladaLibraryCameras;
      var _LibLights: TColladaLibraryLights;
      var _LibVisualScenes: TColladaLibraryVisualScenes;
      var _Scene: TColladaScene;
      var _Options: TSceneDataOptionsSet;
    public
      property Asset: TColladaAsset read _Asset;
      property LibMaterials: TColladaLibraryMaterials read _LibMaterials;
      property LibEffects: TColladaLibraryEffects read _LibEffects;
      property LibImages: TColladaLibraryImages read _LibImages;
      property LibGeometries: TColladaLibraryGeometries read _LibGeometries;
      property LibControllers: TColladaLibraryControllers read _LibControllers;
      property LibAnimations: TColladaLibraryAnimations read _LibAnimations;
      property LibCameras: TColladaLibraryCameras read _LibCameras;
      property LibLights: TColladaLibraryLights read _LibLights;
      property LibVisualScenes: TColladaLibraryVisualScenes read _LibVisualScenes;
      property Scene: TColladaScene read _Scene;
      property Options: TSceneDataOptionsSet read _Options;
      constructor Create(const XMLNode: TUXML; const AOptions: TSceneDataOptionsSet);
      destructor Destroy; override;
    end;
  private
    type TMeshSubsetInterfaceCollada = class (TMeshSubsetInterface)
    private
      var _ColladaTriangles: TColladaTriangles;
      var _VertexDescriptor: TUVertexDescriptor;
    protected
      function GetVertexDescriptor: TUVertexDescriptor; override;
    public
      constructor Create(const ColladaTriangles: TColladaTriangles);
    end;
    type TMeshInterfaceCollada = class (TMeshInterface)
    private
      var _ColladaMesh: TColladaMesh;
    public
      constructor Create(const ColladaMesh: TColladaMesh);
    end;
    var _Root: TColladaRoot;
    class function FindNextValue(const Str: String; var CurPos: Int32): String;
    class function LoadMatrix(
      const Node: TUXML
    ): TUMat;
    class function LoadMatrix(
      const Src: TColladaSource;
      const Index: Int32
    ): TUMat;
    procedure Read(const XML: TUXML);
  public
    property Root: TColladaRoot read _Root;
    class function CanLoad(const StreamHelper: TUStreamHelper): Boolean; override;
    procedure Load(const StreamHelper: TUStreamHelper); override;
    destructor Destroy; override;
  end;

function ULoadImageData(const Stream: TStream): TUImageDataShared; overload;
function ULoadImageData(const FileName: String): TUImageDataShared; overload;
function ULoadImageData(const Buffer: Pointer; const Size: UInt32): TUImageDataShared; overload;
function ULoadImageData(const StreamHelper: TUStreamHelper): TUImageDataShared; overload;

function UCmpVertexDescriptors(const vd0, vd1: TUVertexDescriptor): Boolean;

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

function UCmpVertexDescriptors(const vd0, vd1: TUVertexDescriptor): Boolean;
  var i: Int32;
begin
  if Length(vd0) <> Length(vd1) then Exit(False);
  for i := 0 to High(vd0) do
  begin
    if (vd0[i].Semantic <> vd1[i].Semantic)
    or (vd0[i].DataType <> vd1[i].DataType)
    or (vd0[i].DataCount <> vd1[i].DataCount)
    or (vd0[i].SetNumber <> vd1[i].SetNumber) then
    begin
      Exit(False);
    end;
  end;
  Result := True;
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

// TUVertexAttribute begin
class function TUVertexAttribute.Make(
  const ASemantic: TUVertexAttributeSemantic;
  const ADataType: TUVertexDataType;
  const ADataCount: UInt8;
  const ASetNumber: UInt8
): TUVertexAttribute;
begin
  Result.Semantic := ASemantic;
  Result.DataType := ADataType;
  Result.DataCount := ADataCount;
  Result.SetNumber := ASetNumber;
end;

function TUVertexAttribute.Size: Int32;
begin
  case DataType of
    dt_bool: Result := 1 * DataCount;
    dt_int, dt_float: Result := 4 * DataCount;
  else Result := 0;
  end;
end;
// TUVertexAttribute end

// TUSceneData begin
function TUSceneData.TMeshSubsetInterface.GetVertexDescriptor: TUVertexDescriptor;
begin
  Result := nil;
end;

function TUSceneData.TMeshSubsetInterface.GetVertexBufferSize: Int32;
begin
  Result := _VertexCount * _VertexSize;
end;

function TUSceneData.TMeshSubsetInterface.GetIndexBufferSize: Int32;
begin
  Result := _IndexCount * _IndexSize;
end;

function TUSceneData.TMeshSubsetInterface.GetIndex(const Id: Int32): UInt32;
begin
  if (_IndexSize = 2) then Exit(PUInt16(_IndexData + Id * _IndexSize)^)
  else Exit(PUInt32(_IndexData + Id * _IndexSize)^);
end;

destructor TUSceneData.TMeshSubsetInterface.Destroy;
begin
  FreeMemAndNil(_IndexData);
  FreeMemAndNil(_VertexData);
  inherited Destroy;
end;

destructor TUSceneData.TMeshInterface.Destroy;
begin
  specialize UArrClear<TMeshSubsetInterface>(_Subsets);
  inherited Destroy;
end;

class function TUSceneData.CanLoad(const Stream: TStream): Boolean;
  var sh: TUStreamHelper;
begin
  sh := TUStreamHelper.Create(Stream);
  try
    Result := CanLoad(sh);
  finally
    sh.Free;
  end;
end;

class function TUSceneData.CanLoad(const FileName: String): Boolean;
  var fs: TFileStream;
begin
  fs := TFileStream.Create(FileName, fmOpenRead);
  try
    Result := CanLoad(fs);
  finally
    fs.Free;
  end;
end;

class function TUSceneData.CanLoad(const Buffer: Pointer; const Size: UInt32): Boolean;
  var ms: TUConstMemoryStream;
begin
  ms := TUConstMemoryStream.Create(Buffer, Size);
  try
    Result := CanLoad(ms);
  finally
    ms.Free;
  end;
end;

destructor TUSceneData.Destroy;
begin
  specialize UArrClear<TMeshInterface>(_MeshList);
  inherited Destroy;
end;

procedure TUSceneData.Load(const Stream: TStream);
  var sh: TUStreamHelper;
begin
  sh := TUStreamHelper.Create(Stream);
  try
    Load(sh);
  finally
    sh.Free;
  end;
end;

procedure TUSceneData.Load(const FileName: String);
  var fs: TFileStream;
begin
  fs := TFileStream.Create(FileName, fmOpenRead);
  try
    Load(fs);
  finally
    fs.Free;
  end;
end;

procedure TUSceneData.Load(const Buffer: Pointer; const Size: UInt32);
  var ms: TUConstMemoryStream;
begin
  ms := TUConstMemoryStream.Create(Buffer, Size);
  try
    Load(ms);
  finally
    ms.Free;
  end;
end;

constructor TUSceneData.Create(const AOptions: TSceneDataOptionsSet);
begin
  inherited Create;
  _Options := AOptions;
end;
// TUSceneData end

// TUSceneDataDAE begin
procedure TUSceneDataDAE.TColladaObject.AddChild(const Child: TColladaObject);
begin
  specialize UArrAppend<TColladaObject>(_Children, Child);
end;

procedure TUSceneDataDAE.TColladaObject.RemoveChild(const Child: TColladaObject);
begin
  specialize UArrRemove<TColladaObject>(_Children, Child);
end;

procedure TUSceneDataDAE.TColladaObject.SetParent(const Value: TColladaObject);
begin
  if _Parent = Value then Exit;
  if Assigned(_Parent) then _Parent.RemoveChild(Self);
  _Parent := Value;
  if Assigned(_Parent) then _Parent.AddChild(Self);
end;

procedure TUSceneDataDAE.TColladaObject.DumpBegin;
begin
  //LabLog(AnsiString(_Tag) + ': {', 2);
end;

procedure TUSceneDataDAE.TColladaObject.DumpEnd;
begin
  //LabLog('}', -2);
end;

procedure TUSceneDataDAE.TColladaObject.DumpData;
begin
  //if Length(_id) > 0 then LabLog('id: ' + AnsiString(_id));
  //if Length(_sid) > 0 then LabLog('sid: ' + AnsiString(_sid));
  //if Length(_Name) > 0 then LabLog('name: ' + AnsiString(_Name));
end;

procedure TUSceneDataDAE.TColladaObject.Resolve;
  var i: Int32;
begin
  for i := 0 to High(_Children) do
  begin
    _Children[i].Resolve;
  end;
  ResolveLinks;
end;

procedure TUSceneDataDAE.TColladaObject.ResolveLinks;
begin

end;

procedure TUSceneDataDAE.TColladaObject.Initialize;
  var i: Int32;
begin
  for i := 0 to High(_Children) do
  begin
    _Children[i].Initialize;
  end;
  InitializeObject;
end;

procedure TUSceneDataDAE.TColladaObject.InitializeObject;
begin

end;

function TUSceneDataDAE.TColladaObject.ResolveObject(
  const Path: String; const ObjectClass: TClass
): TColladaObject;
begin
  Result := Find(Path);
  if Assigned(Result)
  and not (Result is ObjectClass) then
  begin
    Result := nil;
  end;
end;

constructor TUSceneDataDAE.TColladaObject.Create(
  const XMLNode: TUXML;
  const AParent: TColladaObject
);
begin
  _Scoped := False;
  _Tag := LowerCase(XMLNode.Name);
  _id := XMLNode.AttributeValue['id'];
  _sid := XMLNode.AttributeValue['sid'];
  _Name := XMLNode.AttributeValue['name'];
  _UserData := nil;
  _AutoFreeUserData := False;
  if Length(_id) > 0 then _Scoped := True;
  Parent := AParent;
end;

destructor TUSceneDataDAE.TColladaObject.Destroy;
begin
  if _AutoFreeUserData then FreeAndNil(_UserData);
  specialize UArrClear<TColladaObject>(_Children);
  inherited Destroy;
end;

function TUSceneDataDAE.TColladaObject.GetRoot: TColladaObject;
begin
  if _Parent = nil then Exit(Self);
  Result := _Parent.GetRoot;
end;

function TUSceneDataDAE.TColladaObject.Find(const Path: String): TColladaObject;
  var SearchPath: String;
  var i: Int32;
  var PathArr: TUStrArr;
begin
  if Length(Path) = 0 then Exit(nil);
  SearchPath := Path;
  if SearchPath[1] = '#' then Delete(SearchPath, 1, 1);
  if Length(SearchPath) = 0 then Exit(nil);
  PathArr := UStrExplode(SearchPath, '/');
  Result := GetRoot;
  for i := 0 to High(PathArr) do
  begin
    Result := Result.FindChildRecursive(PathArr[i]);
    if not Assigned(Result) then Break;
  end;
  if not Assigned(Result) then
  begin
    //LabLog('Unresolved link: ' + AnsiString(Path));
  end;
end;

function TUSceneDataDAE.TColladaObject.FindChild(
  const NodeID: String
): TColladaObject;
  var i: Int32;
begin
  for i := 0 to High(_Children) do
  if (_Children[i].id = NodeID)
  or (_Children[i].sid = NodeID) then
  begin
    Exit(_Children[i]);
  end;
  Result := nil;
end;

function TUSceneDataDAE.TColladaObject.FindChildRecursive(
  const NodeID: String
): TColladaObject;
  var i: Int32;
begin
  for i := 0 to High(_Children) do
  begin
    if (_Children[i].id = NodeID)
    or (_Children[i].sid = NodeID) then
    begin
      Exit(_Children[i]);
    end
    else
    begin
      Result := _Children[i].FindChildRecursive(NodeID);
      if Assigned(Result) then Exit;
    end;
  end;
  Result := nil;
end;

procedure TUSceneDataDAE.TColladaObject.Dump;
  var i: Int32;
begin
  DumpBegin;
  DumpData;
  for i := 0 to High(_Children) do
  begin
    _Children[i].Dump;
  end;
  DumpEnd;
end;

constructor TUSceneDataDAE.TColladaInstance.Create(
  const XMLNode: TUXML;
  const AParent: TColladaObject
);
begin
  inherited Create(XMLNode, AParent);
  _Url := XMLNode.AttributeValue['url'];
end;

destructor TUSceneDataDAE.TColladaInstance.Destroy;
begin
  inherited Destroy;
end;

constructor TUSceneDataDAE.TColladaNode.Create(
  const XMLNode: TUXML;
  const AParent: TColladaObject
);
  var Data, CurLayer: String;
  var p, i: Int32;
  var Node: TUXML;
  var NodeName: String;
  var XfLookAt: array [0..8] of TUFloat;
  var XfRotate: array [0..3] of TUFloat;
  var XfScale: TUVec3;
  var XfTranslate: TUVec3;
  var XfSkew: array [0..6] of TUFloat;
begin
  inherited Create(XMLNode, AParent);
  _NodeType := StringToNodeType(XMLNode.AttributeValue['type']);
  Data := XMLNode.AttributeValue['layer'];
  if Length(Data) > 0 then
  begin
    p := 1;
    repeat
      CurLayer := FindNextValue(Data, p);
      if Length(CurLayer) > 0 then
      begin
        specialize UArrAppend<String>(_Layers, CurLayer);
      end;
    until Length(CurLayer) = 0;
  end;
  Node := XMLNode.FindChild('matrix');
  if Assigned(Node) then
  begin
    Matrix := LoadMatrix(Node);
  end
  else
  begin
    Node := XMLNode.FindChild('lookat');
    if Assigned(Node) then
    begin
      Data := Node.Content;
      p := 1;
      for i := 0 to 8 do
      begin
        XfLookAt[i] := StrToFloatDef(AnsiString(FindNextValue(Data, p)), 0);
      end;
      Matrix := TUMat.View(
        TUVec3.Make(XfLookAt[0], XfLookAt[1], XfLookAt[2]),
        TUVec3.Make(XfLookAt[3], XfLookAt[4], XfLookAt[5]),
        TUVec3.Make(XfLookAt[6], XfLookAt[7], XfLookAt[8])
      );
    end
    else
    begin
      Matrix := TUMat.Identity;
      for Node in XMLNode do
      begin
        NodeName := LowerCase(Node.Name);
        if NodeName = 'rotate' then
        begin
          Data := Node.Content;
          p := 1;
          for i := 0 to 3 do
          begin
            XfRotate[i] := StrToFloatDef(FindNextValue(Data, p), 0);
          end;
          Matrix := TUMat.Rotation(
            TUVec3.Make(XfRotate[0], XfRotate[1], XfRotate[2]),
            XfRotate[3] * UDegToRad
          ) * Matrix;
        end
        else if NodeName = 'scale' then
        begin
          Data := Node.Content;
          p := 1;
          for i := 0 to 2 do
          begin
            XfScale[i] := StrToFloatDef(FindNextValue(Data, p), 0);
          end;
          Matrix := TUMat.Scaling(XfScale) * Matrix;
        end
        else if NodeName = 'translate' then
        begin
          Data := Node.Content;
          p := 1;
          for i := 0 to 2 do
          begin
            XfTranslate[i] := StrToFloatDef(FindNextValue(Data, p), 0);
          end;
          Matrix := TUMat.Translation(XfTranslate) * Matrix;
        end
        else if NodeName = 'skew' then
        begin
          Data := Node.Content;
          p := 1;
          for i := 0 to 6 do
          begin
            XfSkew[i] := StrToFloatDef(FindNextValue(Data, p), 0);
          end;
          Matrix := TUMat.Skew(
            TUVec3.Make(XfSkew[4], XfSkew[5], XfSkew[6]),
            TUVec3.Make(XfSkew[1], XfSkew[2], XfSkew[3]),
            XfSkew[0] * UDegToRad
          ) * Matrix;
        end;
      end;
    end;
  end;
  for Node in XMLNode do
  begin
    NodeName := LowerCase(Node.Name);
    if NodeName = 'node' then
    begin
      specialize UArrAppend<TColladaNode>(
        _Nodes, TColladaNode.Create(Node, Self)
      );
    end
    else if NodeName = 'instance_geometry' then
    begin
      specialize UArrAppend<TColladaInstance>(
        _Instances, TColladaInstanceGeometry.Create(Node, Self)
      );
    end
    else if NodeName = 'instance_controller' then
    begin
      specialize UArrAppend<TColladaInstance>(
        _Instances, TColladaInstanceController.Create(Node, Self)
      );
    end
    else if NodeName = 'instance_camera' then
    begin
      specialize UArrAppend<TColladaInstance>(
        _Instances, TColladaInstanceCamera.Create(Node, Self)
      );
    end
    else if NodeName = 'instance_light' then
    begin
      specialize UArrAppend<TColladaInstance>(
        _Instances, TColladaInstanceLight.Create(Node, Self)
      );
    end;
  end;
end;

destructor TUSceneDataDAE.TColladaNode.Destroy;
begin
  inherited Destroy;
end;

class function TUSceneDataDAE.TColladaNode.StringToNodeType(
  const NodeTypeName: String
): TColladaNodeType;
  var NodeTypeNameLC: String;
begin
  NodeTypeNameLC := LowerCase(NodeTypeName);
  if NodeTypeNameLC = 'node' then Exit(nt_node);
  if NodeTypeNameLC = 'joint' then Exit(nt_joint);
  Result := nt_invalid;
end;

function TUSceneDataDAE.TColladaInput.GetSize: UInt32;
  var Src: TColladaSource;
begin
  if _Source is TColladaSource then
  begin
    Src := TColladaSource(Source);
  end
  else if _Source is TColladaVertices then
  begin
    Src := TColladaSource(TColladaVertices(_Source).Inputs[0].Source);
  end
  else
  begin
    Exit(0);
  end;
  Result := Src.DataArray.ItemSize * Src.Accessor.Stride;
end;

procedure TUSceneDataDAE.TColladaInput.ResolveLinks;
  var Obj: TColladaObject;
begin
  inherited ResolveLinks;
  Obj := Find(_SourceRef);
  if Assigned(Obj) and ((Obj is TColladaSource) or (Obj is TColladaVertices)) then
  begin
    _Source := Obj;
  end;
end;

constructor TUSceneDataDAE.TColladaInput.Create(
  const XMLNode: TUXML;
  const AParent: TColladaObject
);
begin
  inherited Create(XMLNode, AParent);
  _Semantic := UpperCase(XMLNode.AttributeValue['semantic']);
  _SourceRef := XMLNode.AttributeValue['source'];
  _Offset := StrToIntDef(XMLNode.AttributeValue['offset'], 0);
  _Set := StrToIntDef(XMLNode.AttributeValue['set'], 0);
end;

destructor TUSceneDataDAE.TColladaInput.Destroy;
begin
  inherited Destroy;
end;

function TUSceneDataDAE.TColladaDataArray.GetAsBool(const Index: Int32): PBoolean;
  var i: Int32;
begin
  i := _ItemSize * Index;
  if (i < 0) or (i + _ItemSize > Length(_Data)) then Exit(nil);
  Result := PBoolean(@_Data[i]);
end;

function TUSceneDataDAE.TColladaDataArray.GetAsInt(const Index: Int32): PInt32;
  var i: Int32;
begin
  i := _ItemSize * Index;
  if (i < 0) or (i + _ItemSize > Length(_Data)) then Exit(nil);
  Result := PInt32(@_Data[i]);
end;

function TUSceneDataDAE.TColladaDataArray.GetAsFloat(const Index: Int32): PUFloat;
  var i: Int32;
begin
  i := _ItemSize * Index;
  if (i < 0) or (i + _ItemSize > Length(_Data)) then Exit(nil);
  Result := PUFloat(@_Data[i]);
end;

function TUSceneDataDAE.TColladaDataArray.GetAsString(const Index: Int32): String;
begin
  if (Index < 0) or (Index > High(_DataString)) then Exit('');
  Result := _DataString[Index];
end;

function TUSceneDataDAE.TColladaDataArray.GetRawData(const Offset: Int32): Pointer;
begin
  Result := @_Data[Offset];
end;

constructor TUSceneDataDAE.TColladaDataArray.Create(
  const XMLNode: TUXML;
  const AParent: TColladaObject
);
  var Data: String;
  var i, p: Int32;
begin
  inherited Create(XMLNode, AParent);
  _ArrayType := NodeNameToArrayType(XMLNode.Name);
  _Count := StrToIntDef(XMLNode.AttributeValue['count'], 0);
  Data := XMLNode.Content;
  p := 1;
  case _ArrayType of
    at_bool:
    begin
      _ItemSize := SizeOf(Boolean);
      SetLength(_Data, _Count * _ItemSize);
      for i := 0 to _Count - 1 do
      begin
        AsBool[i]^ := StrToBoolDef(FindNextValue(Data, p), False);
      end;
    end;
    at_int:
    begin
      _ItemSize := SizeOf(Int32);
      SetLength(_Data, _Count * _ItemSize);
      for i := 0 to _Count - 1 do
      begin
        AsInt[i]^ := StrToIntDef(FindNextValue(Data, p), 0);
      end;
    end;
    at_float:
    begin
      _ItemSize := SizeOf(TUFloat);
      SetLength(_Data, _Count * _ItemSize);
      for i := 0 to _Count - 1 do
      begin
        AsFloat[i]^ := StrToFloatDef(FindNextValue(Data, p), 0);
      end;
    end;
    at_sidref,
    at_idref,
    at_name:
    begin
      _ItemSize := 0;
      SetLength(_DataString, _Count);
      for i := 0 to _Count - 1 do
      begin
        _DataString[i] := FindNextValue(Data, p);
      end;
    end;
  end;
end;

destructor TUSceneDataDAE.TColladaDataArray.Destroy;
begin
  inherited Destroy;
end;

class function TUSceneDataDAE.TColladaDataArray.NodeNameToArrayType(
  const NodeName: String
): TColladaArrayType;
  var NodeNameLC: String;
begin
  NodeNameLC := LowerCase(NodeName);
  if NodeNameLC = 'bool_array' then Exit(at_bool);
  if NodeNameLC = 'float_array' then Exit(at_float);
  if NodeNameLC = 'idref_array' then Exit(at_idref);
  if NodeNameLC = 'int_array' then Exit(at_int);
  if NodeNameLC = 'name_array' then Exit(at_name);
  if NodeNameLC = 'sidref_array' then Exit(at_sidref);
  if NodeNameLC = 'token_array' then Exit(at_token);
  Result := at_invalid;
end;

class function TUSceneDataDAE.TColladaDataArray.TypeNameToArrayType(
  const TypeName: String
): TColladaArrayType;
  var TypeNameLC: String;
begin
  TypeNameLC := LowerCase(TypeName);
  if TypeNameLC = 'bool' then Exit(at_bool);
  if TypeNameLC = 'float' then Exit(at_float);
  if TypeNameLC = 'idref' then Exit(at_idref);
  if TypeNameLC = 'int' then Exit(at_int);
  if TypeNameLC = 'name' then Exit(at_name);
  if TypeNameLC = 'sidref' then Exit(at_sidref);
  if TypeNameLC = 'token' then Exit(at_token);
  Result := at_invalid;
end;

class function TUSceneDataDAE.TColladaDataArray.IsDataArrayNode(
  const XMLNode: TUXML
): Boolean;
begin
  Result := NodeNameToArrayType(XMLNode.Name) <> at_invalid;
end;

function TUSceneDataDAE.TColladaAccessor.GetParams: PParamArr;
begin
  Result := @_Params[0];
end;

procedure TUSceneDataDAE.TColladaAccessor.ResolveLinks;
begin
  inherited ResolveLinks;
  _Source := ResolveObject(_SourceRef, TColladaDataArray) as TColladaDataArray;
end;

constructor TUSceneDataDAE.TColladaAccessor.Create(
  const XMLNode: TUXML;
  const AParent: TColladaObject
);
  var Node: TUXML;
  var NodeName: String;
  var i: Int32;
begin
  inherited Create(XMLNode, AParent);
  _SourceRef := XMLNode.AttributeValue['source'];
  _Count := StrToIntDef(XMLNode.AttributeValue['count'], 0);
  _Stride := StrToIntDef(XMLNode.AttributeValue['stride'], 1);
  SetLength(_Params, _Stride);
  for i := 0 to High(_Params) do _Params[i].ParamType := at_invalid;
  i := 0;
  for Node in XMLNode do
  begin
    NodeName := LowerCase(Node.Name);
    if NodeName = 'param' then
    begin
      _Params[i].Name := Node.AttributeValue['name'];
      _Params[i].ParamType := TColladaDataArray.TypeNameToArrayType(Node.AttributeValue['type']);
      Inc(i);
    end;
  end;
end;

destructor TUSceneDataDAE.TColladaAccessor.Destroy;
begin
  inherited Destroy;
end;

constructor TUSceneDataDAE.TColladaSource.Create(
  const XMLNode: TUXML;
  const AParent: TColladaObject
);
  var Node: TUXML;
begin
  inherited Create(XMLNode, AParent);
  for Node in XMLNode do
  begin
    if not Assigned(_DataArray)
    and TColladaDataArray.IsDataArrayNode(Node) then
    begin
      _DataArray := TColladaDataArray.Create(Node, Self);
    end;
  end;
  _Accessor := nil;
  Node := XMLNode.FindChild('technique_common');
  if Assigned(Node) then
  begin
    Node := Node.FindChild('accessor');
    if Assigned(Node) then
    begin
      _Accessor := TColladaAccessor.Create(Node, Self);
    end;
  end;
end;

destructor TUSceneDataDAE.TColladaSource.Destroy;
begin
  inherited Destroy;
end;

constructor TUSceneDataDAE.TColladaVertices.Create(
  const XMLNode: TUXML;
  const AParent: TColladaObject
);
  var Node: TUXML;
  var NodeName: String;
begin
  inherited Create(XMLNode, AParent);
  for Node in XMLNode do
  begin
    NodeName := LowerCase(Node.Name);
    if NodeName = 'input' then
    begin
      specialize UArrAppend<TColladaInput>(
        _Inputs, TColladaInput.Create(Node, Self)
      );
    end;
  end;
end;

destructor TUSceneDataDAE.TColladaVertices.Destroy;
begin
  inherited Destroy;
end;

function TUSceneDataDAE.TColladaTriangles.GetVertexSize: Int32;
  var i: Int32;
  var Src: TColladaSource;
begin
  Result := 0;
  for i := 0 to High(_VertexLayout) do
  begin
    Src := TColladaSource(_VertexLayout[i].Source);
    Result += Src.Accessor.Source.ItemSize * Src.Accessor.Stride;
  end;
end;

function TUSceneDataDAE.TColladaTriangles.GetIndices: PUInt32Arr;
begin
  Result := @_Indices[0];
end;

function TUSceneDataDAE.TColladaTriangles.GetVertexDescriptor: TUVertexDescriptor;
  var CurAttr: Int32;
  procedure AddInput(const Input: TColladaInput);
    const SemanticMap: array[0..5] of record
      Name: String;
      Value: TUVertexAttributeSemantic;
    end = (
      (Name: 'POSITION'; Value: as_position),
      (Name: 'COLOR'; Value: as_color),
      (Name: 'NORMAL'; Value: as_normal),
      (Name: 'TEXTANGENT'; Value: as_tangent),
      (Name: 'TEXBINORMAL'; Value: as_binormal),
      (Name: 'TEXCOORD'; Value: as_texcoord)
    );
    var Vertices: TColladaVertices;
    var Source: TColladaSource;
    var i: Int32;
  begin
    if not Assigned(Input) or not Assigned(Input.Source) then Exit;
    if Input.Source is TColladaVertices then
    begin
      Vertices := TColladaVertices(Input.Source);
      for i := 0 to High(Vertices.Inputs) do
      begin
        AddInput(Vertices.Inputs[i]);
      end;
    end
    else if Input.Source is TColladaSource then
    begin
      Source := TColladaSource(Input.Source);
      if Source.DataArray.ArrayType in [at_bool, at_float, at_int] then
      begin
        for i := 0 to High(SemanticMap) do
        if SemanticMap[i].Name = Input.Semantic then
        begin
          Result[CurAttr].Semantic := SemanticMap[i].Value;
          case Source.DataArray.ArrayType of
            at_bool: Result[CurAttr].DataType := dt_bool;
            at_int: Result[CurAttr].DataType := dt_int;
            at_float: Result[CurAttr].DataType := dt_float;
            else Result[CurAttr].DataType := dt_invalid;
          end;
          Result[CurAttr].DataCount := Source.Accessor.Stride;
          Result[CurAttr].SetNumber := Input.InputSet;
          Inc(CurAttr);
          Break;
        end;
      end;
    end;
  end;
  var i: Int32;
begin
  Result := nil;
  SetLength(Result, Length(Inputs));
  CurAttr := 0;
  for i := 0 to High(Inputs) do
  begin
    AddInput(Inputs[i]);
  end;
  if Length(Result) <> CurAttr then
  begin
    SetLength(Result, CurAttr);
  end;
end;

function TUSceneDataDAE.TColladaTriangles.GetInputSourceCount(
  const Index: UInt32
): UInt32;
  var Source: TColladaSource;
begin
  if _Inputs[Index].Source is TColladaSource then
  begin
    Source := TColladaSource(_Inputs[Index].Source);
  end
  else if _Inputs[Index].Source is TColladaVertices then
  begin
    Source := TColladaSource(TColladaVertices(_Inputs[Index].Source).Inputs[0].Source);
  end
  else
  begin
    Exit(0);
  end;
  Result := Source.Accessor.Count;
end;

procedure TUSceneDataDAE.TColladaTriangles.InitializeObject;
  procedure ProcessInput(const Input: TColladaInput);
    var i: Int32;
    var Vertices: TColladaVertices;
  begin
    if not Assigned(Input) or not Assigned(Input.Source) then Exit;
    if Input.Source is TColladaVertices then
    begin
      Vertices := TColladaVertices(Input.Source);
      for i := 0 to High(Vertices.Inputs) do
      begin
        ProcessInput(Vertices.Inputs[i]);
      end;
    end
    else
    begin
      specialize UArrAppend<TColladaInput>(
        _VertexLayout, Input
      );
    end;
  end;
  procedure OptimizeSource(const Source: TColladaSource; const Offset: Int32);
    var i, j, n: Int32;
    var Match: Boolean;
    var f0, f1: TUFloat;
    var Remap: array of array [0..1] of Int32;
  begin
    Remap := nil;
    if Source.DataArray.ArrayType <> at_float then Exit;
    for i := 0 to Source.Accessor.Count - 1 do
    begin
      for j := 0 to i - 1 do
      begin
        Match := True;
        for n := 0 to Source.Accessor.Stride - 1 do
        begin
          f0 := Source.DataArray.AsFloat[i * Source.Accessor.Stride + n]^;
          f1 := Source.DataArray.AsFloat[j * Source.Accessor.Stride + n]^;
          if f0 <> f1 then
          begin
            Match := False;
            Break;
          end;
        end;
        if Match then
        begin
          n := Length(Remap);
          SetLength(Remap, n + 1);
          Remap[n][0] := i;
          Remap[n][1] := j;
          Break;
        end;
      end;
    end;
    if Length(Remap) > 0 then
    begin
      for i := 0 to Count * 3 - 1 do
      begin
        for j := 0 to High(Remap) do
        if Indices^[i * _InputStride + Offset] = Remap[j][0] then
        begin
          Indices^[i * _InputStride + Offset] := Remap[j][1];
        end;
      end;
    end;
  end;
  var i: Int32;
  var Root: TColladaRoot;
begin
  inherited InitializeObject;
  _VertexLayout := nil;
  for i := 0 to High(_Inputs) do
  begin
    ProcessInput(_Inputs[i]);
  end;
  Root := GetRoot as TColladaRoot;
  if Assigned(Root) and (sdo_optimize in Root.Options) then
  begin
    for i := 0 to High(_Inputs) do
    if Assigned(_Inputs[i].Source)
    and (_Inputs[i].Source is TColladaSource) then
    begin
      OptimizeSource(TColladaSource(_Inputs[i].Source), _Inputs[i].Offset);
    end;
  end;
end;

constructor TUSceneDataDAE.TColladaTriangles.Create(
  const XMLNode: TUXML;
  const AParent: TColladaObject
);
  var Node: TUXML;
  var NodeName: String;
  var IndexStr: String;
  var i, p, MaxOffset: Int32;
begin
  inherited Create(XMLNode, AParent);
  _MaterialRef := XMLNode.AttributeValue['material'];
  _Count := StrToIntDef(XMLNode.AttributeValue['count'], 0);
  IndexStr := '';
  for Node in XMLNode do
  begin
    NodeName := LowerCase(Node.Name);
    if NodeName = 'input' then
    begin
      specialize UArrAppend<TColladaInput>(
        _Inputs, TColladaInput.Create(Node, Self)
      );
    end
    else if NodeName = 'p' then
    begin
      IndexStr += Node.Content;
    end;
  end;
  MaxOffset := 0;
  for i := 0 to High(_Inputs) do
  if _Inputs[i].Offset > MaxOffset then
  begin
    MaxOffset := _Inputs[i].Offset;
  end;
  _InputStride := MaxOffset + 1;
  if Length(IndexStr) > 0 then
  begin
    SetLength(_Indices, _Count * 3 * (_InputStride));
    p := 1;
    for i := 0 to High(_Indices) do
    begin
      _Indices[i] := StrToIntDef(FindNextValue(IndexStr, p), 0);
    end;
  end;
end;

destructor TUSceneDataDAE.TColladaTriangles.Destroy;
begin
  inherited Destroy;
end;

function TUSceneDataDAE.TColladaTriangles.CopyInputData(
  const Target: Pointer;
  const Input: TColladaInput;
  const Index: Int32
): Pointer;
  var Dest: PUInt8;
  var Vertices: TColladaVertices;
  var Source: TColladaSource;
  var i: Int32;
begin
  if not Assigned(Input) or not Assigned(Input.Source) then Exit;
  Dest := Target;
  if Input.Source is TColladaVertices then
  begin
    Vertices := TColladaVertices(Input.Source);
    for i := 0 to High(Vertices.Inputs) do
    begin
      Dest := CopyInputData(Dest, Vertices.Inputs[i], Index);
    end;
  end
  else if Input.Source is TColladaSource then
  begin
    Source := TColladaSource(Input.Source);
    if Source.DataArray.ArrayType in [at_bool, at_float, at_int] then
    begin
      Move(
        Source.DataArray.RawData[Source.DataArray.ItemSize * Source.Accessor.Stride * Index]^,
        Dest^,
        Source.DataArray.ItemSize * Source.Accessor.Stride
      );
      Inc(Dest, Source.DataArray.ItemSize * Source.Accessor.Stride);
    end;
  end;
  Result := Dest;
end;

constructor TUSceneDataDAE.TColladaMesh.Create(
  const XMLNode: TUXML;
  const AParent: TColladaObject
);
  var Node: TUXML;
  var NodeName: String;
begin
  inherited Create(XMLNode, AParent);
  _Vertices := nil;
  for Node in XMLNode do
  begin
    NodeName := LowerCase(Node.Name);
    if NodeName = 'source' then
    begin
      specialize UArrAppend<TColladaSource>(
        _Sources, TColladaSource.Create(Node, Self)
      );
    end
    else if NodeName = 'vertices' then
    begin
      if not Assigned(_Vertices) then
      begin
        _Vertices := TColladaVertices.Create(Node, Self);
      end;
    end
    else if (NodeName = 'triangles') or (NodeName = 'polygons') then
    begin
      specialize UArrAppend<TColladaTriangles>(
        _TrianglesList, TColladaTriangles.Create(Node, Self)
      );
    end;
  end;
end;

destructor TUSceneDataDAE.TColladaMesh.Destroy;
begin
  inherited Destroy;
end;

constructor TUSceneDataDAE.TColladaImage.Create(
  const XMLNode: TUXML;
  const AParent: TColladaObject
);
  var Node: TUXML;
begin
  inherited Create(XMLNode, AParent);
  Node := XMLNode.FindChild('init_from');
  if Assigned(Node) then
  begin
    _Source := Node.Content;
  end;
end;

destructor TUSceneDataDAE.TColladaImage.Destroy;
begin
  inherited Destroy;
end;

function TUSceneDataDAE.TColladaEffectProfileParam.AsSurface: TDataSurface;
begin
  Result := _Data as TDataSurface;
end;

function TUSceneDataDAE.TColladaEffectProfileParam.AsSampler: TDataSampler;
begin
  Result := _Data as TDataSampler;
end;

function TUSceneDataDAE.TColladaEffectProfileParam.AsFloat: TDataFloat;
begin
  Result := _Data as TDataFloat;
end;

function TUSceneDataDAE.TColladaEffectProfileParam.AsFloat2: TDataFloat2;
begin
  Result := _Data as TDataFloat2;
end;

function TUSceneDataDAE.TColladaEffectProfileParam.AsFloat3: TDataFloat3;
begin
  Result := _Data as TDataFloat3;
end;

function TUSceneDataDAE.TColladaEffectProfileParam.AsFloat4: TDataFloat4;
begin
  Result := _Data as TDataFloat4;
end;

constructor TUSceneDataDAE.TColladaEffectProfileParam.Create(
  const XMLNode: TUXML;
  const AParent: TColladaObject
);
  var Node, SubNode: TUXML;
  var NodeName, VecData: String;
  var DataSurface: TDataSurface;
  var DataSampler: TDataSampler;
  var DataFloat: TDataFloat;
  var DataFloat2: TDataFloat2;
  var DataFloat3: TDataFloat3;
  var DataFloat4: TDataFloat4;
  var i, p: Int32;
begin
  inherited Create(XMLNode, AParent);
  for Node in XMLNode do
  begin
    NodeName := LowerCase(Node.Name);
    if NodeName = 'surface' then
    begin
      DataSurface := TDataSurface.Create;
      SubNode := Node.FindChild('init_from');
      if Assigned(SubNode) then
      begin
        DataSurface.InitFrom := SubNode.Content;
      end;
      _Data := DataSurface;
      _ParamType := pt_surface;
    end
    else if (NodeName = 'sampler1d')
    or (NodeName = 'sampler2d')
    or (NodeName = 'sampler3d') then
    begin
      DataSampler := TDataSampler.Create;
      SubNode := Node.FindChild('source');
      if Assigned(SubNode) then
      begin
        DataSampler.Source := SubNode.Content;
      end;
      _Data := DataSampler;
      _ParamType := pt_sampler;
    end
    else if (NodeName = 'float') then
    begin
      DataFloat := TDataFloat.Create;
      VecData := Node.Content;
      p := 1;
      DataFloat.Value := StrToFloatDef(FindNextValue(VecData, p), 0);
      _Data := DataFloat;
      _ParamType := pt_float;
    end
    else if (NodeName = 'float2') then
    begin
      DataFloat2 := TDataFloat2.Create;
      VecData := Node.Content;
      p := 1;
      for i := 0 to 1 do
      begin
        DataFloat2.Value[i] := StrToFloatDef(FindNextValue(VecData, p), 0);
      end;
      _Data := DataFloat2;
      _ParamType := pt_float2;
    end
    else if (NodeName = 'float3') then
    begin
      DataFloat3 := TDataFloat3.Create;
      VecData := Node.Content;
      p := 1;
      for i := 0 to 2 do
      begin
        DataFloat3.Value[i] := StrToFloatDef(FindNextValue(VecData, p), 0);
      end;
      _Data := DataFloat3;
      _ParamType := pt_float3;
    end
    else if (NodeName = 'float4') then
    begin
      DataFloat4 := TDataFloat4.Create;
      VecData := Node.Content;
      p := 1;
      for i := 0 to 3 do
      begin
        DataFloat4.Value[i] := StrToFloatDef(FindNextValue(VecData, p), 0);
      end;
      _Data := DataFloat4;
      _ParamType := pt_float4;
    end;
  end;
end;

destructor TUSceneDataDAE.TColladaEffectProfileParam.Destroy;
begin
  FreeAndNil(_Data);
  inherited Destroy;
end;

procedure TUSceneDataDAE.TColladaEffectProfile.ResolveLinks;
  var i: Int32;
  var Obj: TColladaObject;
begin
  for i := 0 to High(_Params) do
  begin
    case _Params[i].ParamType of
      pt_sampler:
      begin
        Obj := Find(_Params[i].AsSampler.Source);
        if (Obj is TColladaEffectProfileParam)
        and (TColladaEffectProfileParam(Obj).ParamType = pt_surface) then
        begin
          _Params[i].AsSampler.Surface := TColladaEffectProfileParam(Obj).AsSurface;
        end;
      end;
      pt_surface:
      begin
        _Params[i].AsSurface.Image := TColladaImage(Find(_Params[i].AsSurface.InitFrom));
      end;
    end;
  end;
end;

constructor TUSceneDataDAE.TColladaEffectProfile.Create(
  const XMLNode: TUXML;
  const AParent: TColladaObject
);
  var Node: TUXML;
  var NodeName: String;
begin
  inherited Create(XMLNode, AParent);
  for Node in XMLNode do
  begin
    NodeName := LowerCase(Node.Name);
    if NodeName = 'newparam' then
    begin
      specialize UArrAppend<TColladaEffectProfileParam>(
        _Params, TColladaEffectProfileParam.Create(Node, Self)
      );
    end;
  end;
end;

destructor TUSceneDataDAE.TColladaEffectProfile.Destroy;
begin
  inherited Destroy;
end;

constructor TUSceneDataDAE.TColladaEffect.Create(
  const XMLNode: TUXML;
  const AParent: TColladaObject
);
  var Node: TUXML;
  var NodeName: String;
begin
  inherited Create(XMLNode, AParent);
  for Node in XMLNode do
  begin
    NodeName := LowerCase(Node.Name);
    if NodeName = 'profile_common' then
    begin
      _Profile := TColladaEffectProfile.Create(Node, Self);
      Break;
    end;
  end;
end;

destructor TUSceneDataDAE.TColladaEffect.Destroy;
begin
  inherited Destroy;
end;

procedure TUSceneDataDAE.TColladaInstanceEffect.ResolveLinks;
  var Obj: TColladaObject;
begin
  inherited ResolveLinks;
  Obj := Find(Url);
  if Assigned(Obj) and (Obj is TColladaEffect) then
  begin
    _Effect := TColladaEffect(Obj);
  end;
end;

constructor TUSceneDataDAE.TColladaInstanceEffect.Create(
  const XMLNode: TUXML;
  const AParent: TColladaObject
);
begin
  inherited Create(XMLNode, AParent);
end;

destructor TUSceneDataDAE.TColladaInstanceEffect.Destroy;
begin
  inherited Destroy;
end;

constructor TUSceneDataDAE.TColladaMaterial.Create(
  const XMLNode: TUXML;
  const AParent: TColladaObject
);
  var Node: TUXML;
begin
  inherited Create(XMLNode, AParent);
  Node := XMLNode.FindChild('instance_effect');
  if Assigned(Node) then
  begin
    _InstanceEffect := TColladaInstanceEffect.Create(Node, Self);
  end;
end;

destructor TUSceneDataDAE.TColladaMaterial.Destroy;
begin
  inherited Destroy;
end;

constructor TUSceneDataDAE.TColladaGeometry.Create(
  const XMLNode: TUXML;
  const AParent: TColladaObject
);
  var Node: TUXML;
  var NodeName: String;
begin
  inherited Create(XMLNode, AParent);
  for Node in XMLNode do
  begin
    NodeName := LowerCase(Node.Name);
    if NodeName = 'mesh' then
    begin
      specialize UArrAppend<TColladaMesh>(
        _Meshes, TColladaMesh.Create(Node, Self)
      );
    end;
  end;
end;

destructor TUSceneDataDAE.TColladaGeometry.Destroy;
begin
  inherited Destroy;
end;

constructor TUSceneDataDAE.TColladaMorph.Create(
  const XMLNode: TUXML;
  const AParent: TColladaObject
);
begin
  inherited Create(XMLNode, AParent);
end;

destructor TUSceneDataDAE.TColladaMorph.Destroy;
begin
  inherited Destroy;
end;

function TUSceneDataDAE.TColladaJoints.FindInput(
  const Semantic: String
): TColladaInput;
  var i: Int32;
begin
  for i := 0 to High(_Inputs) do
  begin
    if _Inputs[i].Semantic = Semantic then Exit(_Inputs[i]);
  end;
  Result := nil;
end;

procedure TUSceneDataDAE.TColladaJoints.ResolveLinks;
  var InputJoints: TColladaInput;
  var InputBinds: TColladaInput;
  var SrcJoints: TColladaSource;
  var SrcBinds: TColladaSource;
  var i, JointCount: Int32;
begin
  inherited ResolveLinks;
  InputJoints := FindInput('JOINT');
  InputBinds := FindInput('INV_BIND_MATRIX');
  if Assigned(InputJoints) and Assigned(InputBinds) then
  begin
    SrcJoints := TColladaSource(InputJoints.Source);
    SrcBinds := TColladaSource(InputBinds.Source);
    JointCount := SrcJoints.Accessor.Count;
    SetLength(_Joints, JointCount);
    for i := 0 to JointCount - 1 do
    begin
      _Joints[i].JointName := SrcJoints.DataArray.AsString[i];
      _Joints[i].BindPose := LoadMatrix(SrcBinds, i);
    end;
  end;
end;

constructor TUSceneDataDAE.TColladaJoints.Create(
  const XMLNode: TUXML;
  const AParent: TColladaObject
);
  var Node: TUXML;
  var NodeName: String;
begin
  inherited Create(XMLNode, AParent);
  for Node in XMLNode do
  begin
    NodeName := LowerCase(Node.Name);
    if NodeName = 'input' then
    begin
      specialize UArrAppend<TColladaInput>(
        _Inputs, TColladaInput.Create(Node, Self)
      );
    end;
  end;
end;

destructor TUSceneDataDAE.TColladaJoints.Destroy;
begin
  inherited Destroy;
end;

function TUSceneDataDAE.TColladaVertexWeights.FindInput(
  const Semantic: String
): TColladaInput;
  var i: Int32;
begin
  for i := 0 to High(_Inputs) do
  begin
    if _Inputs[i].Semantic = Semantic then Exit(_Inputs[i]);
  end;
  Result := nil;
end;

procedure TUSceneDataDAE.TColladaVertexWeights.ResolveLinks;
  var InputIndex, InputWeight: TColladaInput;
  var SrcWeight: TColladaSource;
  var i, j, p: Int32;
  var tw, w: TUFloat;
begin
  inherited ResolveLinks;
  InputIndex := FindInput('JOINT');
  InputWeight := FindInput('WEIGHT');
  if not Assigned(InputIndex)
  or not Assigned(InputWeight) then Exit;
  SrcWeight := TColladaSource(InputWeight.Source);
  p := 0;
  for i := 0 to High(_VertexWeights) do
  begin
    tw := 0;
    for j := 0 to High(_VertexWeights[i]) do
    begin
      w := SrcWeight.DataArray.AsFloat[_Indices[p + InputWeight.Offset]]^;
      tw += w;
      _VertexWeights[i][j].JointIndex := _Indices[p + InputIndex.Offset];
      _VertexWeights[i][j].JointWeight := w;
      Inc(p, 2);
    end;
    tw := 1 / tw;
    for j := 0 to High(_VertexWeights[i]) do
    with _VertexWeights[i][j] do
    begin
      JointWeight *= tw;
    end;
  end;
end;

constructor TUSceneDataDAE.TColladaVertexWeights.Create(
  const XMLNode: TUXML;
  const AParent: TColladaObject
);
  var Node: TUXML;
  var NodeName, Str: String;
  var i, p, n, ic: Int32;
begin
  inherited Create(XMLNode, AParent);
  _VCount := StrToIntDef(XMLNode.AttributeValue['count'], 0);
  SetLength(_VertexWeights, _VCount);
  for Node in XMLNode do
  begin
    NodeName := LowerCase(Node.Name);
    if NodeName = 'input' then
    begin
      specialize UArrAppend<TColladaInput>(
        _Inputs, TColladaInput.Create(Node, Self)
      );
    end
    else if NodeName = 'vcount' then
    begin
      ic := 0;
      Str := Node.Content;
      p := 1;
      for i := 0 to _VCount - 1 do
      begin
        n := StrToIntDef(FindNextValue(str, p), 0);
        SetLength(_VertexWeights[i], n);
        Inc(ic, n * 2);
      end;
    end;
  end;
  Node := XMLNode.FindChild('v');
  if Assigned(Node) then
  begin
    SetLength(_Indices, ic);
    Str := Node.Content;
    p := 1;
    for i := 0 to ic - 1 do
    begin
      _Indices[i] := StrToIntDef(FindNextValue(str, p), 0);
    end;
  end;
end;

destructor TUSceneDataDAE.TColladaVertexWeights.Destroy;
begin
  inherited Destroy;
end;

procedure TUSceneDataDAE.TColladaSkin.ResolveLinks;
  var Obj: TColladaObject;
begin
  inherited ResolveLinks;
  Obj := Find(_GeometryRef);
  if Assigned(Obj) and (Obj is TColladaGeometry) then
  begin
    _Geometry := TColladaGeometry(Obj);
  end;
end;

constructor TUSceneDataDAE.TColladaSkin.Create(
  const XMLNode: TUXML;
  const AParent: TColladaObject
);
  var Node: TUXML;
  var NodeName: String;
begin
  inherited Create(XMLNode, AParent);
  _GeometryRef := XMLNode.AttributeValue['source'];
  _BindShapeMatrix := TUMat.Identity;
  for Node in XMLNode do
  begin
    NodeName := LowerCase(Node.Name);
    if NodeName = 'bind_shape_matrix' then
    begin
      _BindShapeMatrix := LoadMatrix(Node);
    end
    else if NodeName = 'source' then
    begin
      specialize UArrAppend<TColladaSource>(
        _Sources, TColladaSource.Create(Node, Self)
      );
    end
    else if NodeName = 'joints' then
    begin
      _Joints := TColladaJoints.Create(Node, Self);
    end
    else if NodeName = 'vertex_weights' then
    begin
      _VertexWeights := TColladaVertexWeights.Create(Node, Self);
    end;
  end;
end;

destructor TUSceneDataDAE.TColladaSkin.Destroy;
begin
  inherited Destroy;
end;

function TUSceneDataDAE.TColladaController.GetAsSkin: TColladaSkin;
begin
  Result := TColladaSkin(_Controller);
end;

function TUSceneDataDAE.TColladaController.GetAsMorph: TColladaMorph;
begin
  Result := TColladaMorph(_Controller);
end;

constructor TUSceneDataDAE.TColladaController.Create(
  const XMLNode: TUXML;
  const AParent: TColladaObject
);
  var Node: TUXML;
begin
  inherited Create(XMLNode, AParent);
  _ControllerType := ct_invalid;
  Node := XMLNode.FindChild('skin');
  if Assigned(Node) then
  begin
    _ControllerType := ct_skin;
    _Controller := TColladaSkin.Create(Node, Self);
  end
  else
  begin
    Node := XMLNode.FindChild('morph');
    if Assigned(Node) then
    begin
      _ControllerType := ct_morph;
      _Controller := TColladaMorph.Create(Node, Self);
    end;
  end;
end;

destructor TUSceneDataDAE.TColladaController.Destroy;
begin
  inherited Destroy;
end;

function TUSceneDataDAE.TColladaAnimationSampler.GetKey(const Index: Int32): PKey;
begin
  Result := @_Keys[Index];
end;

function TUSceneDataDAE.TColladaAnimationSampler.GetKeyCount: Int32;
begin
  Result := Length(_Keys);
end;

function TUSceneDataDAE.TColladaAnimationSampler.FindKey(const Time: TUFloat): Int32;
  var i: Int32;
begin
  for i := 0 to High(_Keys) do
  if _Keys[i].Time <= Time then
  begin
    Exit(i);
  end;
  Result := High(_Keys);
end;

function TUSceneDataDAE.TColladaAnimationSampler.GetMaxTime: TUFloat;
begin
  if Length(_Keys) > 0 then Exit(_Keys[High(_Keys)].Time) else Result := 0;
end;

function TUSceneDataDAE.TColladaAnimationSampler.GetSampleSize: UInt32;
begin
  Result := _DataSize * _DataStride;
end;

procedure TUSceneDataDAE.TColladaAnimationSampler.ResolveLinks;
  function FindInput(const Semantic: String): TColladaInput;
    var i: Int32;
  begin
    for i := 0 to High(_Inputs) do
    if _Inputs[i].Semantic = Semantic then
    begin
      Exit(_Inputs[i]);
    end;
    Exit(nil);
  end;
  var InputTime, InputValue, InputInterpolation, InputTangentIn, InputTangentOut: TColladaInput;
  var src: TColladaSource;
  var i, j, n: Int32;
begin
  InputTime := FindInput('INPUT');
  InputValue := FindInput('OUTPUT');
  InputInterpolation := FindInput('INTERPOLATION');
  InputTangentIn := FindInput('IN_TANGENT');
  InputTangentOut := FindInput('OUT_TANGENT');
  if (Assigned(InputTime))
  or (Assigned(InputValue)) then
  begin
    src := TColladaSource(InputValue.Source);
    GetMemory(_Data, src.Accessor.Count * src.Accessor.Stride * src.DataArray.ItemSize);
    _DataType := src.DataArray.ArrayType;
    _DataStride := src.Accessor.Stride;
    _DataSize := src.DataArray.ItemSize;
    SetLength(_Keys, TColladaSource(InputTime.Source).Accessor.Count);
    for i := 0 to High(_Keys) do
    begin
      src := TColladaSource(InputTime.Source);
      _Keys[i].Time := src.DataArray.AsFloat[src.Accessor.Stride * i]^;
      src := TColladaSource(InputValue.Source);
      n := i * src.Accessor.Stride * src.DataArray.ItemSize;
      _Keys[i].Value := _Data + n;
      Move(src.DataArray.RawData[n]^, _Keys[i].Value^, src.Accessor.Stride * src.DataArray.ItemSize);
      if Assigned(InputInterpolation) then
      begin
        src := TColladaSource(InputInterpolation.Source);
        if src.DataArray.AsString[i] = 'STEP' then
        begin
          _Keys[i].Interpolation := ai_step;
        end
        else if src.DataArray.AsString[i] = 'BEZIER' then
        begin
          if Assigned(InputTangentIn)
          and Assigned(InputTangentOut) then
          begin
            src := TColladaSource(InputTangentIn.Source);
            SetLength(_Keys[i].TangentIn, src.Accessor.Stride);
            for j := 0 to src.Accessor.Stride - 1 do
            begin
              _Keys[i].TangentIn[j] := src.DataArray.AsFloat[i * src.Accessor.Stride + j]^;
            end;
            src := TColladaSource(InputTangentOut.Source);
            SetLength(_Keys[i].TangentOut, src.Accessor.Stride);
            for j := 0 to src.Accessor.Stride - 1 do
            begin
              _Keys[i].TangentOut[j] := src.DataArray.AsFloat[i * src.Accessor.Stride + j]^;
            end;
            _Keys[i].Interpolation := ai_bezier;
          end
          else
          begin
            _Keys[i].Interpolation := ai_linear;
          end;
        end
        else
        begin
          _Keys[i].Interpolation := ai_linear;
        end;
      end
      else
      begin
        _Keys[i].Interpolation := ai_linear;
      end;
    end;
  end;
end;

procedure TUSceneDataDAE.TColladaAnimationSampler.DumpData;
  //var i, j: TVkInt32;
  //var val_str: AnsiString;
begin
  inherited DumpData;
  (*
  if _DataType <> at_float then Exit;
  LabLog('Keys[' + IntToStr(Length(_Keys)) + '] {', 2);
  for i := 0 to High(_Keys) do
  begin
    val_str := '{';
    for j := 0 to _DataStride - 1 do
    begin
      val_str += ' ' + FormatFloat('0.###', PLabFloatArr(_Keys[i].Value)^[j]);
      if j < _DataStride - 1 then val_str += ',';
    end;
    val_str += ' }';
    LabLog('Time = ' + FormatFloat('0.###', _Keys[i].Time) + '; Value = ' + val_str);
  end;
  LabLog('}', -2);
  //*)
end;

procedure TUSceneDataDAE.TColladaAnimationSampler.SampleData(
  const Output: Pointer;
  const Time: TUFloat;
  const Loop: Boolean
);
  var InFloat0, InFloat1: PUFloat;
  var OutFloat: PUFloat;
  var k0, k1, i: Int32;
  var t, dt: TUFloat;
begin
  if _DataType = at_float then
  begin
    if not Loop then
    begin
      if Time <= _Keys[0].Time then
      begin
        Move(_Keys[0].Value^, Output^, _DataSize * _DataStride);
        Exit;
      end;
      if Time >= _Keys[High(_Keys)].Time then
      begin
        Move(_Keys[High(_Keys)].Value^, Output^, _DataSize * _DataStride);
        Exit;
      end;
    end;
    k0 := FindKey(Time);
    k1 := (k0 + 1) mod Length(_Keys);
    OutFloat := PUFloat(Output);
    InFloat0 := PUFloat(_Keys[k0].Value);
    InFloat1 := PUFloat(_Keys[k1].Value);
    t := Time mod _Keys[High(_Keys)].Time;
    if k1 < k0 then dt := 1 / _Keys[0].Time else dt := 1 / (_Keys[k1].Time - _Keys[k0].Time);
    t := t * dt;
    case _Keys[k0].Interpolation of
      ai_step:
      begin
        for i := 0 to _DataStride - 1 do
        begin
          OutFloat^ := InFloat0^;
          Inc(OutFloat); Inc(InFloat0);
        end;
      end;
      ai_linear:
      begin
        for i := 0 to _DataStride - 1 do
        begin
          OutFloat^ := ULerp(InFloat0^, InFloat1^, t);
          Inc(OutFloat); Inc(InFloat0); Inc(InFloat1);
        end;
      end;
      ai_bezier:
      begin
        for i := 0 to _DataStride - 1 do
        begin
          OutFloat^ := UBezier(
            InFloat0^,
            _Keys[k0].TangentOut[i],
            _Keys[k0].TangentIn[i],
            InFloat1^,
            t
          );
          Inc(OutFloat); Inc(InFloat0); Inc(InFloat1);
        end;
      end;
    end;
  end;
end;

function TUSceneDataDAE.TColladaAnimationSampler.SampleAsFloat(
  const Time: TUFloat; const Loop: Boolean
): TUFloat;
begin
  SampleData(@Result, Time, Loop);
end;

function TUSceneDataDAE.TColladaAnimationSampler.SampleAsFloat2(
  const Time: TUFloat; const Loop: Boolean): TUVec2;
begin
  SampleData(@Result, Time, Loop);
end;

function TUSceneDataDAE.TColladaAnimationSampler.SampleAsFloat3(
  const Time: TUFloat; const Loop: Boolean): TUVec3;
begin
  SampleData(@Result, Time, Loop);
end;

function TUSceneDataDAE.TColladaAnimationSampler.SampleAsFloat4(
  const Time: TUFloat; const Loop: Boolean): TUVec4;
begin
  SampleData(@Result, Time, Loop);
end;

function TUSceneDataDAE.TColladaAnimationSampler.SampleAsMatrix(
  const Time: TUFloat; const Loop: Boolean): TUMat;
begin
  SampleData(@Result, Time, Loop);
end;

constructor TUSceneDataDAE.TColladaAnimationSampler.Create(
  const XMLNode: TUXML;
  const AParent: TColladaObject
);
  var Node: TUXML;
  var NodeName: String;
begin
  inherited Create(XMLNode, AParent);
  for Node in XMLNode do
  begin
    NodeName := LowerCase(Node.Name);
    if NodeName = 'input' then
    begin
      specialize UArrAppend<TColladaInput>(
        _Inputs, TColladaInput.Create(Node, Self)
      );
    end;
  end;
end;

destructor TUSceneDataDAE.TColladaAnimationSampler.Destroy;
begin
  if Assigned(_Data) then FreeMemory(_Data);
  inherited Destroy;
end;

function TUSceneDataDAE.TColladaAnimationChannel.GetMaxTime: TUFloat;
begin
  if Assigned(_Sampler) then Exit(_Sampler.MaxTime) else Result := 0;
end;

procedure TUSceneDataDAE.TColladaAnimationChannel.ResolveLinks;
  var Obj: TColladaObject;
begin
  inherited ResolveLinks;
  Obj := Find(_SourceRef);
  if Assigned(Obj)
  and (Obj is TColladaAnimationSampler) then
  begin
    _Sampler := TColladaAnimationSampler(Obj);
  end;
  _Target := Find(_TargetRef);
end;

constructor TUSceneDataDAE.TColladaAnimationChannel.Create(
  const XMLNode: TUXML;
  const AParent: TColladaObject
);
  var TargetObject, TargetProp: String;
  var i: Int32;
begin
  inherited Create(XMLNode, AParent);
  _SourceRef := XMLNode.AttributeValue['source'];
  _TargetRef := XMLNode.AttributeValue['target'];
  TargetObject := _TargetRef;
  for i := Length(TargetObject) downto 1 do
  if _TargetRef[i] = '/' then
  begin
    Delete(TargetObject, i, Length(_TargetRef) - i + 1);
    Break;
  end;
  TargetProp := _TargetRef;
  Delete(TargetProp, 1, Length(TargetObject) + 1);
  _TargetRef := TargetObject;
  _TargetProperty := TargetProp;
end;

destructor TUSceneDataDAE.TColladaAnimationChannel.Destroy;
begin
  inherited Destroy;
end;

constructor TUSceneDataDAE.TColladaAnimation.Create(
  const XMLNode: TUXML;
  const AParent: TColladaObject
);
  var Node: TUXML;
  var NodeName: String;
begin
  inherited Create(XMLNode, AParent);
  for Node in XMLNode do
  begin
    NodeName := LowerCase(Node.Name);
    if NodeName = 'animation' then
    begin
      specialize UArrAppend<TColladaAnimation>(
        _Animations, TColladaAnimation.Create(Node, Self)
      );
    end
    else if NodeName = 'source' then
    begin
      specialize UArrAppend<TColladaSource>(
        _Sources, TColladaSource.Create(Node, Self)
      );
    end
    else if NodeName = 'sampler' then
    begin
      specialize UArrAppend<TColladaAnimationSampler>(
        _Samplers, TColladaAnimationSampler.Create(Node, Self)
      );
    end
    else if NodeName = 'channel' then
    begin
      specialize UArrAppend<TColladaAnimationChannel>(
        _Channels, TColladaAnimationChannel.Create(Node, Self)
      );
    end;
  end;
end;

destructor TUSceneDataDAE.TColladaAnimation.Destroy;
begin
  inherited Destroy;
end;

constructor TUSceneDataDAE.TColladaCamera.Create(
  const XMLNode: TUXML;
  const AParent: TColladaObject
);
  var Node, SubNode: TUXML;
begin
  inherited Create(XMLNode, AParent);
  _FOV := 60 * UDegToRad;
  _Aspect := 1;
  _Near := 0.1;
  _Far := 100;
  Node := XMLNode.FindChild('optics');
  if Assigned(Node) then Node := Node.FindChild('technique_common');
  if Assigned(Node) then Node := Node.FindChild('perspective');
  if Assigned(Node) then
  begin
    SubNode := Node.FindChild('xfov');
    if Assigned(SubNode) then _FOV := StrToFloatDef(SubNode.Content, 60) * UDegToRad;
    SubNode := Node.FindChild('aspect_ratio');
    if Assigned(SubNode) then _Aspect := StrToFloatDef(SubNode.Content, 1);
    SubNode := Node.FindChild('znear');
    if Assigned(SubNode) then _Near := StrToFloatDef(SubNode.Content, 0.1);
    SubNode := Node.FindChild('zfar');
    if Assigned(SubNode) then _Far := StrToFloatDef(SubNode.Content, 100);
  end;
end;

destructor TUSceneDataDAE.TColladaCamera.Destroy;
begin
  inherited Destroy;
end;

constructor TUSceneDataDAE.TColladaLight.Create(
  const XMLNode: TUXML;
  const AParent: TColladaObject
);
  var Node, NodeLight, NodeParams: TUXML;
  var NodeName: String;
  var NodeValue: TUStrArr;
  var i: Int32;
begin
  inherited Create(XMLNode, AParent);
  _LightType := lt_ambient;
  _Color := TUVec3.Make(1);
  _AttenuationConstant := 1;
  _AttenuationLinear := 0;
  _AttenuationQuadratic := 0;
  _FalloffAngle := UPi;
  _FalloffExponent := 0;
  Node := XMLNode.FindChild('technique_common');
  for NodeLight in Node do
  begin
    NodeName := LowerCase(NodeLight.Name);
    if NodeName = 'ambient' then
    begin
      _LightType := lt_ambient;
    end
    else if NodeName = 'directional' then
    begin
      _LightType := lt_directional;
    end
    else if NodeName = 'point' then
    begin
      _LightType := lt_point;
    end
    else if NodeName = 'spot' then
    begin
      _LightType := lt_spot;
    end;
    for NodeParams in NodeLight do
    begin
      NodeName := LowerCase(NodeParams.Name);
      if NodeName = 'color' then
      begin
        NodeValue := UStrExplode(NodeParams.Content, ' ');
        for i := 0 to specialize UMin<Int32>(2, High(NodeValue)) do
        begin
          _Color[i] := StrToFloatDef(NodeValue[i], 0);
        end;
      end
      else if NodeName = 'constant_attenuation' then
      begin
        _AttenuationConstant := StrToFloatDef(NodeParams.Content, 1);
      end
      else if NodeName = 'linear_attenuation' then
      begin
        _AttenuationLinear := StrToFloatDef(NodeParams.Content, 0);
      end
      else if NodeName = 'quadratic_attenuation' then
      begin
        _AttenuationQuadratic := StrToFloatDef(NodeParams.Content, 0);
      end
      else if NodeName = 'falloff_angle' then
      begin
        _FalloffAngle := StrToFloatDef(NodeParams.Content, 180) * UDegToRad;
      end
      else if NodeName = 'falloff_exponent' then
      begin
        _FalloffExponent := StrToFloatDef(NodeParams.Content, 0);
      end;
    end;
  end;
end;

destructor TUSceneDataDAE.TColladaLight.Destroy;
begin
  inherited Destroy;
end;

procedure TUSceneDataDAE.TColladaInstanceMaterial.ResolveLinks;
  var Obj: TColladaObject;
begin
  inherited ResolveLinks;
  Obj := Find(_Target);
  if Assigned(Obj) and (Obj is TColladaMaterial) then
  begin
    _Material := TColladaMaterial(Obj);
  end;
end;

constructor TUSceneDataDAE.TColladaInstanceMaterial.Create(
  const XMLNode: TUXML;
  const AParent: TColladaObject
);
begin
  inherited Create(XMLNode, AParent);
  _Symbol := XMLNode.AttributeValue['symbol'];
  _Target := XMLNode.AttributeValue['target'];
end;

destructor TUSceneDataDAE.TColladaInstanceMaterial.Destroy;
begin
  inherited Destroy;
end;

procedure TUSceneDataDAE.TColladaInstanceGeometry.ResolveLinks;
  var Obj: TColladaObject;
begin
  inherited ResolveLinks;
  Obj := Find(Url);
  if Assigned(Obj) and (Obj is TColladaGeometry) then
  begin
    _Geometry := TColladaGeometry(Obj);
  end;
end;

constructor TUSceneDataDAE.TColladaInstanceGeometry.Create(
  const XMLNode: TUXML;
  const AParent: TColladaObject
);
  var Node, NodeInstMat: TUXML;
begin
  inherited Create(XMLNode, AParent);
  Node := XMLNode.FindChild('bind_material');
  if Assigned(Node) then
  begin
    Node := Node.FindChild('technique_common');
    if Assigned(Node) then
    begin
      for NodeInstMat in Node do
      if LowerCase(NodeInstMat.Name) = 'instance_material' then
      begin
        specialize UArrAppend<TColladaInstanceMaterial>(
          _MaterialBindings, TColladaInstanceMaterial.Create(NodeInstMat, Self)
        );
      end;
    end;
  end;
end;

destructor TUSceneDataDAE.TColladaInstanceGeometry.Destroy;
begin
  inherited Destroy;
end;

procedure TUSceneDataDAE.TColladaInstanceController.ResolveLinks;
  var Obj: TColladaObject;
begin
  inherited ResolveLinks;
  Obj := Find(Url);
  if Assigned(Obj) and (Obj is TColladaController) then
  begin
    _Controller := TColladaController(Obj);
  end;
  Obj := Find(_SkeletonRef);
  if Assigned(Obj) and (Obj is TColladaNode) then
  begin
    _Skeleton := TColladaNode(Obj);
  end;
end;

constructor TUSceneDataDAE.TColladaInstanceController.Create(
  const XMLNode: TUXML;
  const AParent: TColladaObject
);
  var Node, NodeTech, NodeInstMat: TUXML;
  var NodeName: String;
begin
  inherited Create(XMLNode, AParent);
  for Node in XMLNode do
  begin
    NodeName := LowerCase(Node.Name);
    if NodeName = 'skeleton' then
    begin
      _SkeletonRef := Node.Content;
    end
    else if NodeName = 'bind_material' then
    begin
      NodeTech := Node.FindChild('technique_common');
      if Assigned(NodeTech) then
      begin
        for NodeInstMat in NodeTech do
        if LowerCase(NodeInstMat.Name) = 'instance_material' then
        begin
          specialize UArrAppend<TColladaInstanceMaterial>(
            _MaterialBindings, TColladaInstanceMaterial.Create(NodeInstMat, Self)
          );
        end;
      end;
    end;
  end;
end;

destructor TUSceneDataDAE.TColladaInstanceController.Destroy;
begin
  inherited Destroy;
end;

procedure TUSceneDataDAE.TColladaInstanceCamera.ResolveLinks;
  var Obj: TColladaObject;
begin
  Obj := Find(Url);
  if Assigned(Obj) and (Obj is TColladaCamera) then
  begin
    _Camera := TColladaCamera(Obj);
  end;
end;

constructor TUSceneDataDAE.TColladaInstanceCamera.Create(
  const XMLNode: TUXML;
  const AParent: TColladaObject
);
begin
  inherited Create(XMLNode, AParent);
end;

destructor TUSceneDataDAE.TColladaInstanceCamera.Destroy;
begin
  inherited Destroy;
end;

procedure TUSceneDataDAE.TColladaInstanceLight.ResolveLinks;
  var Obj: TColladaObject;
begin
  Obj := Find(Url);
  if Assigned(Obj) and (Obj is TColladaLight) then
  begin
    _Light := TColladaLight(Obj);
  end;
end;

constructor TUSceneDataDAE.TColladaInstanceLight.Create(
  const XMLNode: TUXML;
  const AParent: TColladaObject
);
begin
  inherited Create(XMLNode, AParent);
end;

destructor TUSceneDataDAE.TColladaInstanceLight.Destroy;
begin
  inherited Destroy;
end;

constructor TUSceneDataDAE.TColladaVisualScene.Create(
  const XMLNode: TUXML;
  const AParent: TColladaObject
);
  var Node: TUXML;
  var NodeName: String;
begin
  inherited Create(XMLNode, AParent);
  for Node in XMLNode do
  begin
    NodeName := LowerCase(Node.Name);
    if NodeName = 'node' then
    begin
      specialize UArrAppend<TColladaNode>(
        _Nodes, TColladaNode.Create(Node, Self)
      );
    end;
  end;
end;

destructor TUSceneDataDAE.TColladaVisualScene.Destroy;
begin
  inherited Destroy;
end;

constructor TUSceneDataDAE.TColladaLibraryAnimations.Create(
  const XMLNode: TUXML;
  const AParent: TColladaObject
);
  var Node: TUXML;
  var NodeName: String;
begin
  inherited Create(XMLNode, AParent);
  for Node in XMLNode do
  begin
    NodeName := LowerCase(Node.Name);
    if NodeName = 'animation' then
    begin
      specialize UArrAppend<TColladaAnimation>(
        _Animations, TColladaAnimation.Create(Node, Self)
      );
    end;
  end;
end;

destructor TUSceneDataDAE.TColladaLibraryAnimations.Destroy;
begin
  inherited Destroy;
end;

constructor TUSceneDataDAE.TColladaLibraryMaterials.Create(
  const XMLNode: TUXML;
  const AParent: TColladaObject
);
  var Node: TUXML;
  var NodeName: String;
begin
  inherited Create(XMLNode, AParent);
  for Node in XMLNode do
  begin
    NodeName := LowerCase(Node.Name);
    if NodeName = 'material' then
    begin
      specialize UArrAppend<TColladaMaterial>(
        _Materials, TColladaMaterial.Create(Node, Self)
      );
    end;
  end;
end;

destructor TUSceneDataDAE.TColladaLibraryMaterials.Destroy;
begin
  inherited Destroy;
end;

constructor TUSceneDataDAE.TColladaLibraryEffects.Create(
  const XMLNode: TUXML;
  const AParent: TColladaObject
);
  var Node: TUXML;
  var NodeName: String;
begin
  inherited Create(XMLNode, AParent);
  for Node in XMLNode do
  begin
    NodeName := LowerCase(Node.Name);
    if NodeName = 'effect' then
    begin
      specialize UArrAppend<TColladaEffect>(
        _Effects, TColladaEffect.Create(Node, Self)
      );
    end;
  end;
end;

destructor TUSceneDataDAE.TColladaLibraryEffects.Destroy;
begin
  inherited Destroy;
end;

constructor TUSceneDataDAE.TColladaLibraryImages.Create(
  const XMLNode: TUXML;
  const AParent: TColladaObject
);
  var Node: TUXML;
  var NodeName: String;
begin
  inherited Create(XMLNode, AParent);
  for Node in XMLNode do
  begin
    NodeName := LowerCase(Node.Name);
    if NodeName = 'image' then
    begin
      specialize UArrAppend<TColladaImage>(
        _Images, TColladaImage.Create(Node, Self)
      );
    end;
  end;
end;

destructor TUSceneDataDAE.TColladaLibraryImages.Destroy;
begin
  inherited Destroy;
end;

constructor TUSceneDataDAE.TColladaLibraryGeometries.Create(
  const XMLNode: TUXML;
  const AParent: TColladaObject
);
  var Node: TUXML;
  var NodeName: String;
begin
  inherited Create(XMLNode, AParent);
  for Node in XMLNode do
  begin
    NodeName := LowerCase(Node.Name);
    if NodeName = 'geometry' then
    begin
      specialize UArrAppend<TColladaGeometry>(
        _Geometries, TColladaGeometry.Create(Node, Self)
      );
    end;
  end;
end;

destructor TUSceneDataDAE.TColladaLibraryGeometries.Destroy;
begin
  inherited Destroy;
end;

constructor TUSceneDataDAE.TColladaLibraryControllers.Create(
  const XMLNode: TUXML;
  const AParent: TColladaObject
);
  var Node: TUXML;
  var NodeName: String;
begin
  inherited Create(XMLNode, AParent);
  for Node in XMLNode do
  begin
    NodeName := LowerCase(Node.Name);
    if NodeName = 'controller' then
    begin
      specialize UArrAppend<TColladaController>(
        _Controllers, TColladaController.Create(Node, Self)
      );
    end;
  end;
end;

destructor TUSceneDataDAE.TColladaLibraryControllers.Destroy;
begin
  inherited Destroy;
end;

constructor TUSceneDataDAE.TColladaLibraryCameras.Create(
  const XMLNode: TUXML;
  const AParent: TColladaObject
);
  var Node: TUXML;
  var NodeName: String;
begin
  inherited Create(XMLNode, AParent);
  for Node in XMLNode do
  begin
    NodeName := LowerCase(Node.Name);
    if NodeName = 'camera' then
    begin
      specialize UArrAppend<TColladaCamera>(
        _Cameras, TColladaCamera.Create(Node, Self)
      );
    end;
  end;
end;

destructor TUSceneDataDAE.TColladaLibraryCameras.Destroy;
begin
  inherited Destroy;
end;

constructor TUSceneDataDAE.TColladaLibraryLights.Create(
  const XMLNode: TUXML;
  const AParent: TColladaObject
);
  var Node: TUXML;
  var NodeName: String;
begin
  inherited Create(XMLNode, AParent);
  for Node in XMLNode do
  begin
    NodeName := LowerCase(Node.Name);
    if NodeName = 'light' then
    begin
      specialize UArrAppend<TColladaLight>(
        _Lights, TColladaLight.Create(Node, Self)
      );
    end;
  end;
end;

destructor TUSceneDataDAE.TColladaLibraryLights.Destroy;
begin
  inherited Destroy;
end;

constructor TUSceneDataDAE.TColladaLibraryVisualScenes.Create(
  const XMLNode: TUXML;
  const AParent: TColladaObject
);
  var Node: TUXML;
  var NodeName: String;
begin
  inherited Create(XMLNode, AParent);
  for Node in XMLNode do
  begin
    NodeName := LowerCase(Node.Name);
    if NodeName = 'visual_scene' then
    begin
      specialize UArrAppend<TColladaVisualScene>(
        _VisualScenes, TColladaVisualScene.Create(Node, Self)
      );
    end;
  end;
end;

destructor TUSceneDataDAE.TColladaLibraryVisualScenes.Destroy;
begin
  inherited Destroy;
end;

procedure TUSceneDataDAE.TColladaInstanceVisualScene.ResolveLinks;
  var Obj: TColladaObject;
begin
  inherited ResolveLinks;
  Obj := Find(Url);
  if Assigned(Obj) and (Obj is TColladaVisualScene) then
  begin
    _VisualScene := TColladaVisualScene(Obj);
  end;
end;

constructor TUSceneDataDAE.TColladaInstanceVisualScene.Create(
  const XMLNode: TUXML;
  const AParent: TColladaObject
);
begin
  inherited Create(XMLNode, AParent);
end;

destructor TUSceneDataDAE.TColladaInstanceVisualScene.Destroy;
begin
  inherited Destroy;
end;

constructor TUSceneDataDAE.TColladaScene.Create(
  const XMLNode: TUXML;
  const AParent: TColladaObject
);
  var Node: TUXML;
begin
  inherited Create(XMLNode, AParent);
  Node := XMLNode.FindChild('instance_visual_scene');
  if Assigned(Node) then
  begin
    _VisualScene := TColladaInstanceVisualScene.Create(Node, Self);
  end;
end;

destructor TUSceneDataDAE.TColladaScene.Destroy;
begin
  inherited Destroy;
end;

constructor TUSceneDataDAE.TColladaAsset.Create(
  const XMLNode: TUXML;
  const AParent: TColladaObject
);
  var Node: TUXML;
  var Str: String;
begin
  inherited Create(XMLNode, AParent);
  _UpAxis.SetIdentity;
  Node := XMLNode.FindChild('up_axis');
  if Assigned(Node) then
  begin
    Str := LowerCase(Node.Content);
    if Str = 'x_up' then
    begin
      _UpAxis.SetValue(1, 0);
    end
    else if Str = 'z_up' then
    begin
      _UpAxis.SetValue(0, 2, 1);
    end;
  end;
end;

destructor TUSceneDataDAE.TColladaAsset.Destroy;
begin
  inherited Destroy;
end;

constructor TUSceneDataDAE.TColladaRoot.Create(const XMLNode: TUXML; const AOptions: TSceneDataOptionsSet);
  var Node: TUXML;
  var NodeName: String;
begin
  inherited Create(XMLNode, nil);
  Node := XMLNode.FindChild('asset');
  if Assigned(Node) then
  begin
    _Asset := TColladaAsset.Create(Node, Self);
  end;
  for Node in XMLNode do
  begin
    NodeName := LowerCase(Node.Name);
    if NodeName = 'library_cameras' then
    begin
      _LibCameras := TColladaLibraryCameras.Create(Node, Self);
    end
    else if NodeName = 'library_lights' then
    begin
      _LibLights := TColladaLibraryLights.Create(Node, Self);
    end
    else if NodeName = 'library_images' then
    begin
      _LibImages := TColladaLibraryImages.Create(Node, Self);
    end
    else if NodeName = 'library_effects' then
    begin
      _LibEffects := TColladaLibraryEffects.Create(Node, Self);
    end
    else if NodeName = 'library_materials' then
    begin
      _LibMaterials := TColladaLibraryMaterials.Create(Node, Self);
    end
    else if NodeName = 'library_geometries' then
    begin
      _LibGeometries := TColladaLibraryGeometries.Create(Node, Self);
    end
    else if NodeName = 'library_controllers' then
    begin
      _LibControllers := TColladaLibraryControllers.Create(Node, Self);
    end
    else if NodeName = 'library_animations' then
    begin
      _LibAnimations := TColladaLibraryAnimations.Create(Node, Self);
    end
    else if NodeName = 'library_visual_scenes' then
    begin
      _LibVisualScenes := TColladaLibraryVisualScenes.Create(Node, Self);
    end
    else if NodeName = 'scene' then
    begin
      _Scene := TColladaScene.Create(Node, Self);
    end;
  end;
end;

destructor TUSceneDataDAE.TColladaRoot.Destroy;
begin
  inherited Destroy;
end;

function TUSceneDataDAE.TMeshSubsetInterfaceCollada.GetVertexDescriptor: TUVertexDescriptor;
begin
  Result := _VertexDescriptor;
end;

constructor TUSceneDataDAE.TMeshSubsetInterfaceCollada.Create(
  const ColladaTriangles: TColladaTriangles
);
  var PositionInd: Int32;
  var TexCoordInd: Int32;
  var IndexInputStride: Int32;
  var FaceNormals: array of TUVec3;
  var Normals: array of TUVec3;
  var RemapNormals: array of Int32;
  procedure GenerateNormals;
    var i, j, Ind, VCount: Int32;
    var Pos: array[0..2] of TUVec4;
  begin
    if Length(Normals) > 0 then Exit;
    SetLength(FaceNormals, _ColladaTriangles.Count);
    SetLength(RemapNormals, _ColladaTriangles.Count * 3);
    VCount := _ColladaTriangles.InputSourceCount[PositionInd];
    SetLength(Normals, VCount);
    UClear(FaceNormals[0], SizeOf(FaceNormals[0]) * Length(FaceNormals));
    UClear(RemapNormals[0], SizeOf(RemapNormals[0]) * Length(RemapNormals));
    UClear(Normals[0], SizeOf(Normals[0]) * Length(Normals));
    for i := 0 to _ColladaTriangles.Count - 1 do
    begin
      for j := 0 to 2 do
      begin
        Ind := _ColladaTriangles.Indices^[IndexInputStride * (i * 3 + j) + _ColladaTriangles.Inputs[PositionInd].Offset];
        _ColladaTriangles.CopyInputData(@Pos[j], _ColladaTriangles.Inputs[PositionInd], Ind);
        RemapNormals[i * 3 + j] := Ind;
      end;
      FaceNormals[i] := UTriangleNormal(Pos[0].xyz, Pos[1].xyz, Pos[2].xyz);
      for j := 0 to 2 do
      begin
        Ind := RemapNormals[i * 3 + j];
        Normals[Ind] := Normals[Ind] + FaceNormals[i];
      end;
    end;
    for i := 0 to High(Normals) - 1 do
    begin
      Normals[i] := Normals[i].Norm;
    end;
  end;
  type TVecTangent = record
    Tangent: TUVec3;
    Binormal: TUVec3;
  end;
  var Tangents: array of TVecTangent;
  var RemapTangents: array of Int32;
  procedure GenerateTangents;
    var TangentCount: Int32;
    function AddTangentVectors(const v: TVecTangent; const Bias: TUFloat): Int32;
      var i: Int32;
    begin
      for i := 0 to TangentCount - 1 do
      if ((Tangents[i].Tangent - v.Tangent).LenSq < Bias)
      and ((Tangents[i].Binormal - v.Binormal).LenSq < Bias) then
      begin
        Exit(i);
      end;
      Result := TangentCount;
      Tangents[TangentCount] := v;
      Inc(TangentCount);
    end;
    function CalculateFaceTB(
      const v1, v2, v3: TUVec3;
      const uv1, uv2, uv3: TUVec2
    ): TVecTangent;
    var
      Side1, Side2, cp: TUVec3;
    begin
      Result := Default(TVecTangent);
      Side1 := TUVec3.Make(v2.x - v1.x, uv2.x - uv1.x, uv2.y - uv1.y);
      Side2 := TUVec3.Make(v3.x - v1.x, uv3.x - uv1.x, uv3.y - uv1.y);
      cp := Side1.Cross(Side2);
      if cp.x = 0 then
      begin
        Result.Tangent.x := 0;
        Result.Binormal.x := 0;
      end
      else
      begin
        Result.Tangent.x := -cp.y / cp.x;
        Result.Binormal.x := -cp.z / cp.x;
      end;
      Side1.x := v2.y - v1.y;
      Side2.x := v3.y - v1.y;
      cp := Side1.Cross(Side2);
      if cp.x = 0 then
      begin
        Result.Tangent.y := 0;
        Result.Binormal.y := 0;
      end
      else
      begin
        Result.Tangent.y := -cp.y / cp.x;
        Result.Binormal.y := -cp.z / cp.x;
      end;
      Side1.x := v2.z - v1.z;
      Side2.x := v3.z - v1.z;
      cp := Side1.Cross(Side2);
      if cp.x = 0 then
      begin
        Result.Tangent.z := 0;
        Result.Binormal.z := 0;
      end
      else
      begin
        Result.Tangent.z := -cp.y / cp.x;
        Result.Binormal.z := -cp.z / cp.x;
      end;
      if not Result.Tangent.IsZero then Result.Tangent := Result.Tangent.Norm;
      if not Result.Binormal.IsZero then Result.Binormal := Result.Binormal.Norm;
    end;
    var FaceTangents: array of TVecTangent;
    var i, j, Ind: Int32;
    var Pos: array[0..2] of TUVec4;
    var uv: array[0..2] of TUVec4;
    var q: TUQuat;
    var tv: TVecTangent;
  begin
    if Length(Tangents) > 0 then Exit;
    GenerateNormals;
    FaceTangents := nil;
    SetLength(FaceTangents, _ColladaTriangles.Count);
    SetLength(Tangents, _ColladaTriangles.Count * 3);
    SetLength(RemapTangents, _ColladaTriangles.Count * 3);
    UClear(FaceTangents[0], SizeOf(FaceTangents[0]) * Length(FaceTangents));
    UClear(Tangents[0], SizeOf(Tangents[0]) * Length(Tangents));
    UClear(RemapTangents[0], SizeOf(RemapTangents[0]) * Length(RemapTangents));
    TangentCount := 0;
    for i := 0 to _ColladaTriangles.Count - 1 do
    begin
      for j := 0 to 2 do
      begin
        Ind := _ColladaTriangles.Indices^[IndexInputStride * (i * 3 + j) + _ColladaTriangles.Inputs[PositionInd].Offset];
        _ColladaTriangles.CopyInputData(@Pos[j], _ColladaTriangles.Inputs[PositionInd], ind);
        ind := _ColladaTriangles.Indices^[IndexInputStride * (i * 3 + j) + _ColladaTriangles.Inputs[TexCoordInd].Offset];
        _ColladaTriangles.CopyInputData(@uv[j], _ColladaTriangles.Inputs[TexCoordInd], ind);
      end;
      FaceTangents[i] := CalculateFaceTB(
        Pos[0].xyz, Pos[1].xyz, Pos[2].xyz,
        uv[0].xy, uv[1].xy, uv[2].xy
      );
      for j := 0 to 2 do
      begin
        q := FaceNormals[i].RotationTo(Normals[RemapNormals[i * 3 + j]]);
        tv.Tangent := FaceTangents[i].Tangent.TransformQuat(q);
        tv.Binormal := FaceTangents[i].Binormal.TransformQuat(q);
        RemapTangents[i * 3 + j] := AddTangentVectors(tv, 0.01);
      end;
    end;
    SetLength(Tangents, TangentCount);
  end;
  var VertexBuffer: array of array of Int32;
  var VertexRemap: array of Int32;
  function AddVertex(const AttribIndices: array of Int32): Int32;
    var i, j: Int32;
    var Match: Boolean;
  begin
    for i := 0 to High(VertexBuffer) do
    begin
      Match := True;
      for j := 0 to High(_VertexDescriptor) do
      if VertexBuffer[i][j] <> AttribIndices[j] then
      begin
        Match := False;
        Break;
      end;
      if Match then
      begin
        Exit(i);
      end;
    end;
    i := Length(VertexBuffer);
    SetLength(VertexBuffer, i + 1);
    SetLength(VertexBuffer[i], Length(_VertexDescriptor));
    for j := 0 to High(_VertexDescriptor) do
    begin
      VertexBuffer[i][j] := AttribIndices[j];
    end;
    Result := i;
  end;
  var NormalInd: Int32;
  var TangentInd: Int32;
  var BinormalInd: Int32;
  var GenAttribs: Int32;
  var GenNormals: Boolean;
  var GenTangents: Boolean;
  var AttribOffsets: array of Int32;
  var AttribIndices: array of Int32;
  var i, j, ai, Ind: Int32;
  var Root: TColladaRoot;
begin
  _ColladaTriangles := ColladaTriangles;
  Root := _ColladaTriangles.GetRoot as TColladaRoot;
  _VertexDescriptor := _ColladaTriangles.VertexDescriptor;
  PositionInd := -1;
  NormalInd := -1;
  TangentInd := -1;
  BinormalInd := -1;
  TexCoordInd := -1;
  IndexInputStride := _ColladaTriangles.InputStride;
  for i := 0 to High(_VertexDescriptor) do
  begin
    case _VertexDescriptor[i].Semantic of
      as_position: if (PositionInd = -1) then PositionInd := i;
      as_normal: if (NormalInd = -1) then NormalInd := i;
      as_tangent: if (TangentInd = -1) then TangentInd := i;
      as_binormal: if (BinormalInd = -1) then BinormalInd := i;
      as_texcoord: if (TexCoordInd = -1) then TexCoordInd := i;
    end;
  end;
  GenAttribs := 0;
  GenNormals := (
    (NormalInd = -1) and (PositionInd > -1)
    and (Assigned(Root) and (sdo_gen_normals in Root.Options))
  );
  GenTangents := (
    ((TangentInd = -1) or (BinormalInd = -1)) and (TexCoordInd > -1)
    and (Assigned(Root) and (sdo_gen_tangents in Root.Options))
  );
  if GenNormals then
  begin
    NormalInd := Length(_VertexDescriptor);
    specialize UArrAppend<TUVertexAttribute>(
      _VertexDescriptor, TUVertexAttribute.Make(as_normal, dt_float, 3)
    );
    Inc(GenAttribs);
    GenerateNormals;
  end;
  if GenTangents then
  begin
    if (TangentInd = -1) then
    begin
      TangentInd := Length(_VertexDescriptor);
      specialize UArrAppend<TUVertexAttribute>(
        _VertexDescriptor, TUVertexAttribute.Make(as_tangent, dt_float, 3)
      );
      Inc(GenAttribs);
    end;
    if (BinormalInd = -1) then
    begin
      BinormalInd := Length(_VertexDescriptor);
      specialize UArrAppend<TUVertexAttribute>(
        _VertexDescriptor, TUVertexAttribute.Make(as_binormal, dt_float, 3)
      );
      Inc(GenAttribs);
    end;
    GenerateTangents;
  end;
  AttribOffsets := nil;
  SetLength(AttribOffsets, Length(_VertexDescriptor));
  _VertexSize := 0;
  for i := 0 to High(_VertexDescriptor) do
  begin
    AttribOffsets[i] := VertexSize;
    _VertexSize += _VertexDescriptor[i].Size;
  end;
  if _ColladaTriangles.Count * 3 > High(UInt16) then
  begin
    _IndexSize := 4;
  end
  else
  begin
    _IndexSize := 2;
  end;
  VertexRemap := nil;
  SetLength(VertexRemap, _ColladaTriangles.Count * 3);
  AttribIndices := nil;
  SetLength(AttribIndices, Length(_VertexDescriptor));
  for i := 0 to _ColladaTriangles.Count * 3 - 1 do
  begin
    for j := 0 to High(_ColladaTriangles.VertexLayout) do
    begin
      Ind := _ColladaTriangles.Indices^[IndexInputStride * i + _ColladaTriangles.Inputs[j].Offset];
      AttribIndices[j] := Ind;
    end;
    for j := 0 to GenAttribs - 1 do
    begin
      ai := Length(_ColladaTriangles.VertexLayout) + j;
      case _VertexDescriptor[ai].Semantic of
        as_normal: Ind := RemapNormals[i];
        as_tangent, as_binormal: Ind := RemapTangents[i];
      end;
      AttribIndices[ai] := Ind;
    end;
    VertexRemap[i] := AddVertex(AttribIndices);
  end;
  _VertexCount := Length(VertexBuffer);
  _VertexData := GetMem(VertexSize * _VertexCount);
  _IndexCount := _ColladaTriangles.Count * 3;
  _IndexData := GetMem(IndexSize * _IndexCount);
  for i := 0 to High(VertexBuffer) do
  begin
    for j := 0 to High(_ColladaTriangles.VertexLayout) do
    begin
      _ColladaTriangles.CopyInputData(
        _VertexData + i * VertexSize + AttribOffsets[j],
        _ColladaTriangles.VertexLayout[j], VertexBuffer[i][j]
      );
    end;
    for j := 0 to GenAttribs - 1 do
    begin
      ai := Length(_ColladaTriangles.VertexLayout) + j;
      case _VertexDescriptor[ai].Semantic of
        as_normal: PUVec3(_VertexData + i * VertexSize + AttribOffsets[ai])^ := Normals[VertexBuffer[i][ai]];
        as_tangent: PUVec3(_VertexData + i * VertexSize + AttribOffsets[ai])^ := Tangents[VertexBuffer[i][ai]].Tangent;
        as_binormal: PUVec3(_VertexData + i * VertexSize + AttribOffsets[ai])^ := Tangents[VertexBuffer[i][ai]].Binormal;
      end;
    end;
  end;
  if _IndexSize = 2 then
  begin
    for i := 0 to High(VertexRemap) do
    begin
      PUInt16(_IndexData + i * IndexSize)^ := UInt16(VertexRemap[i]);
    end;
  end
  else
  begin
    for i := 0 to High(VertexRemap) do
    begin
      PUInt32(_IndexData + i * IndexSize)^ := VertexRemap[i];
    end;
  end;
end;

constructor TUSceneDataDAE.TMeshInterfaceCollada.Create(
  const ColladaMesh: TColladaMesh
);
  var Tris: TColladaTriangles;
  var Intf: TMeshSubsetInterfaceCollada;
begin
  _ColladaMesh := ColladaMesh;
  for Tris in _ColladaMesh.TrianglesList do
  begin
    Intf := TMeshSubsetInterfaceCollada.Create(Tris);
    specialize UArrAppend<TMeshSubsetInterface>(
      _Subsets, Intf
    );
  end;
end;

class function TUSceneDataDAE.FindNextValue(
  const Str: String; var CurPos: Int32
): String;
begin
  Result := '';
  while (
    (CurPos <= Length(Str)) and (
      (Str[CurPos] = ' ')
      or (Str[CurPos] = #$D)
      or (Str[CurPos] = #$A)
    )
  ) do Inc(CurPos);
  while CurPos <= Length(Str) do
  begin
    if (
      (Str[CurPos] = ' ')
      or (Str[CurPos] = #$D)
      or (Str[CurPos] = #$A)
    ) then Break
    else
    begin
      Result += Str[CurPos];
      Inc(CurPos);
    end;
  end;
end;

class function TUSceneDataDAE.LoadMatrix(
  const Node: TUXML
): TUMat;
  var Data: String;
  var x, y, p: Int32;
begin
  Data := Node.Content;
  p := 1;
  for y := 0 to 3 do
  for x := 0 to 3 do
  begin
    Result[x, y] := StrToFloatDef(FindNextValue(Data, p), 0);
  end;
end;

class function TUSceneDataDAE.LoadMatrix(
  const Src: TColladaSource;
  const Index: Int32
): TUMat;
  var i, x, y: Int32;
begin
  i := Index * Src.Accessor.Stride;
  for y := 0 to 3 do
  for x := 0 to 3 do
  begin
    Result[x, y] := Src.DataArray.AsFloat[i]^;
    Inc(i);
  end;
end;

procedure TUSceneDataDAE.Read(const XML: TUXML);
  var Geom: TColladaGeometry;
  var Mesh: TColladaMesh;
  var Intf: TMeshInterfaceCollada;
begin
  if LowerCase(XML.Name) <> 'collada' then Exit;
  if Assigned(_Root) then FreeAndNil(_Root);
  _Root := TColladaRoot.Create(XML, _Options);
  _Root.Resolve;
  _Root.Initialize;
  for Geom in _Root.LibGeometries.Geometries do
  begin
    for Mesh in Geom.Meshes do
    begin
      Intf := TMeshInterfaceCollada.Create(Mesh);
      specialize UArrAppend<TMeshInterface>(
        _MeshList, Intf
      );
    end;
  end;
end;

class function TUSceneDataDAE.CanLoad(const StreamHelper: TUStreamHelper): Boolean;
  var xml: TUXML;
begin
  xml := TUXML.Load(StreamHelper.ToString);
  try
    if LowerCase(xml.Name) <> 'collada' then Exit(False);
    Result := True;
  finally
    xml.Free;
  end;
end;

procedure TUSceneDataDAE.Load(const StreamHelper: TUStreamHelper);
  var xml: TUXML;
begin
  xml := TUXML.Load(StreamHelper.ToString);
  try
    Read(xml);
  finally
    xml.Free;
  end;
end;

destructor TUSceneDataDAE.Destroy;
begin
  FreeAndNil(_Root);
  inherited Destroy;
end;

// TUSceneDataDAE end

end.
