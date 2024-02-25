unit CommonUtils;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$modeswitch typehelpers}
{$optimization autoinline}
{$macro on}
{$warn 6058 off}
{$warn 5024 off}
{$warn 3123 off}
{$warn 3124 off}
{$warn 5026 off}
{$warn 6018 off}

{$ifndef WINDOWS}
  {$define call := cdecl}
{$else}
  {$define call := stdcall}
{$endif}

interface

uses
  SysUtils, Classes, TypInfo;

type TUProcedure = procedure of object;
type TUFunction = function: Boolean of object;

type TUStrArr = array of String;
type TUStrArrArr = array of array of String;
type TUDateTimeArr = array of TDateTime;
type TUVarRecArr = array of TVarRec;

type TUFloat = type Single;
type PUFloat = ^TUFloat;
type TUFloatArr = array[UInt16] of TUFloat;
type PUFloatArr = ^TUFloatArr;
type TUDouble = type Double;
type PUDouble = ^TUDouble;
type TUDoubleArr = array[UInt16] of TUDouble;
type PUDoubleArr = ^TUDoubleArr;

type TUInt8 = type UInt8;
type TUInt8Arr = array[UInt16] of TUInt8;
type PUInt8Arr = ^TUInt8Arr;
type TUInt16 = type UInt16;
type TUInt16Arr = array[UInt16] of TUInt16;
type PUInt16Arr = ^TUInt16Arr;
type TUInt32 = type UInt32;
type TUInt32Arr = array[UInt16] of TUInt32;
type PUInt32Arr = ^TUInt32Arr;
type TUInt64 = type UInt64;
type TUInt64Arr = array[UInt16] of TUInt64;
type PUInt64Arr = ^TUInt64Arr;
type TInt8 = type Int8;
type TInt8Arr = array[UInt16] of TInt8;
type PInt8Arr = ^TInt8Arr;
type TInt16 = type Int16;
type TInt16Arr = array[UInt16] of TInt16;
type PInt16Arr = ^TInt16Arr;
type TInt32 = type Int32;
type TInt32Arr = array[UInt16] of TInt32;
type PInt32Arr = ^TInt32Arr;
type TInt64 = type Int64;
type TInt64Arr = array[UInt16] of TInt64;
type PInt64Arr = ^TInt64Arr;

type TUColor = UInt32;
type PUColor = ^TUColor;
type TUColorArr = array[UInt16] of TUColor;
type PUColorArr = ^TUColorArr;
type TUColorArray = array of TUColor;
type TUMat = array[0..3, 0..3] of TUFloat;
type PUMat = ^TUMat;
type TUMatArr = array[UInt16] of TUMat;
type PUMatArr = ^TUMatArr;
type TUMatArray = array of TUMat;
type TUVec2 = array[0..1] of TUFloat;
type PUVec2 = ^TUVec2;
type TUVec2Arr = array[UInt16] of TUVec2;
type PUVec2Arr = ^TUVec2Arr;
type TUVec2Array = array of TUVec2;
type TUVec3 = array[0..2] of TUFloat;
type PUVec3 = ^TUVec3;
type TUVec3Arr = array[UInt16] of TUVec3;
type PUVec3Arr = ^TUVec3Arr;
type TUVec3Array = array of TUVec3;
type TUVec4 = array[0..3] of TUFloat;
type PUVec4 = ^TUVec4;
type TUVec4Arr = array[UInt16] of TUVec3;
type PUVec4Arr = ^TUVec4Arr;
type TUQuat = array[0..3] of TUFloat;
type PUQuat = ^TUQuat;
type TUQuatArr = array[UInt16] of TUQuat;
type PUQuatArr = ^TUQuatArr;
type TUQuatArray = array of TUQuat;
type TURot2 = array[0..1] of TUFloat;
type PURot2 = ^TURot2;
type TURot2Arr = array[UInt16] of TURot2;
type PURot2Arr = ^TURot2;

type TUFloatImpl = type helper for TUFloat
public
  function ToString: String; inline;
end;

type TUDpubleImpl = type helper for TUDouble
public
  function ToString: String; inline;
end;

type TUInt8Impl = type helper for TUInt8
private
  function GetBit(const Index: TUInt8): TUInt8;
  procedure SetBit(const Index: TUInt8; const Value: TUInt8);
public
  property Bit[const Index: TUInt8]: TUInt8 read GetBit write SetBit; default;
  function ToString: String; inline;
  function ToBool: Boolean; inline;
end;

type TUInt16Impl = type helper for TUInt16
private
  function GetBit(const Index: TUInt8): TUInt8;
  procedure SetBit(const Index: TUInt8; const Value: TUInt8);
public
  property Bit[const Index: TUInt8]: TUInt8 read GetBit write SetBit; default;
  function ToString: String; inline;
  function ToBool: Boolean; inline;
end;

type TUInt32Impl = type helper for TUInt32
private
  function GetBit(const Index: TUInt8): TUInt8;
  procedure SetBit(const Index: TUInt8; const Value: TUInt8);
public
  property Bit[const Index: TUInt8]: TUInt8 read GetBit write SetBit; default;
  function ToString: String; inline;
  function ToBool: Boolean; inline;
end;

type TUInt64Impl = type helper for TUInt64
private
  function GetBit(const Index: TUInt8): TUInt8;
  procedure SetBit(const Index: TUInt8; const Value: TUInt8);
public
  property Bit[const Index: TUInt8]: TUInt8 read GetBit write SetBit; default;
  function ToString: String; inline;
  function ToBool: Boolean; inline;
end;

type TInt8Impl = type helper for TInt8
private
  function GetBit(const Index: TUInt8): TUInt8;
  procedure SetBit(const Index: TUInt8; const Value: TUInt8);
public
  property Bit[const Index: TUInt8]: TUInt8 read GetBit write SetBit; default;
  function ToString: String; inline;
  function ToBool: Boolean; inline;
end;

type TInt16Impl = type helper for TInt16
private
  function GetBit(const Index: TUInt8): TUInt8;
  procedure SetBit(const Index: TUInt8; const Value: TUInt8);
public
  property Bit[const Index: TUInt8]: TUInt8 read GetBit write SetBit; default;
  function ToString: String; inline;
  function ToBool: Boolean; inline;
end;

type TInt32Impl = type helper for TInt32
private
  function GetBit(const Index: TUInt8): TUInt8;
  procedure SetBit(const Index: TUInt8; const Value: TUInt8);
public
  property Bit[const Index: TUInt8]: TUInt8 read GetBit write SetBit; default;
  function ToString: String; inline;
  function ToBool: Boolean; inline;
end;

type TInt64Impl = type helper for TInt64
private
  function GetBit(const Index: TUInt8): TUInt8;
  procedure SetBit(const Index: TUInt8; const Value: TUInt8);
public
  property Bit[const Index: TUInt8]: TUInt8 read GetBit write SetBit; default;
  function ToString: String; inline;
  function ToBool: Boolean; inline;
end;

type TUColorImpl = type helper for TUColor
private
  function GetChannel(const Index: UInt8): UInt8; inline;
  procedure SetChannel(const Index: UInt8; const Value: UInt8); inline;
  function GetR: UInt8; inline;
  procedure SetR(const Value: UInt8); inline;
  function GetG: UInt8; inline;
  procedure SetG(const Value: UInt8); inline;
  function GetB: UInt8; inline;
  procedure SetB(const Value: UInt8); inline;
  function GetA: UInt8; inline;
  procedure SetA(const Value: UInt8); inline;
public
  property Channel[const Index: UInt8]: UInt8 read GetChannel write SetChannel; default;
  property r: UInt8 read GetR write SetR;
  property g: UInt8 read GetG write SetG;
  property b: UInt8 read GetB write SetB;
  property a: UInt8 read GetA write SetA;
  class function Make(const Ar, Ag, Ab, Aa: UInt8): TUColor; static;
  class function Black: TUColor; static;
  class function White: TUColor; static;
  class function Red: TUColor; static;
  class function Blue: TUColor; static;
  class function Green: TUColor; static;
  procedure SetValue(const Ar, Ag, Ab, Aa: UInt8);
  function ToString: String; inline;
end;

type TUMatImpl = type helper for TUMat
private
  function GetElement(const Index: UInt32): TUFloat; inline;
  procedure SetElement(const Index: UInt32; const Value: TUFloat); inline;
  function GetAxisX: TUVec3; inline;
  procedure SetAxisX(const Value: TUVec3); inline;
  function GetAxisY: TUVec3; inline;
  procedure SetAxisY(const Value: TUVec3); inline;
  function GetAxisZ: TUVec3; inline;
  procedure SetAxisZ(const Value: TUVec3); inline;
  function GetPosition: TUVec3; inline;
  procedure SetPosition(const Value: TUVec3); inline;
public
  property Element[const Index: UInt32]: TUFloat read GetElement write SetElement; default;
  property AxisX: TUVec3 read GetAxisX write SetAxisX;
  property AxisY: TUVec3 read GetAxisY write SetAxisY;
  property AxisZ: TUVec3 read GetAxisZ write SetAxisZ;
  property Position: TUVec3 read GetPosition write SetPosition;
  class function Make(
    const e00, e10, e20, e30: TUFloat;
    const e01, e11, e21, e31: TUFloat;
    const e02, e12, e22, e32: TUFloat;
    const e03, e13, e23, e33: TUFloat
  ): TUMat; static;
  class function Zero: TUMat; static; inline;
  class function Identity: TUMat; static; inline;
  class function Scaling(const x, y, z: TUFloat): TUMat; static; overload; inline;
  class function Scaling(const v: TUVec3): TUMat; static; overload; inline;
  class function Scaling(const s: TUFloat): TUMat; static; overload; inline;
  class function Translation(const x, y, z: TUFloat): TUMat; static; overload; inline;
  class function Translation(const v: TUVec3): TUMat; static; overload; inline;
  class function RotationX(const a: TUFloat): TUMat; static; inline;
  class function RotationY(const a: TUFloat): TUMat; static; inline;
  class function RotationZ(const a: TUFloat): TUMat; static; inline;
  class function Rotation(const x, y, z, a: TUFloat): TUMat; static; overload; inline;
  class function Rotation(const v: TUVec3; const a: TUFloat): TUMat; static; overload; inline;
  class function Rotation(const q: TUQuat): TUMat; static; overload; inline;
  class function View(const Origin, Target, Up: TUVec3): TUMat; static; inline;
  class function Proj(const FoV, Aspect, ZNear, ZFar: TUFloat): TUMat; static; inline;
  class function Orth(const Width, Height, ZNear, ZFar: TUFloat): TUMat; static; inline;
  class function Skew(const Amount, Axis: TUVec3; const Angle: TUFloat): TUMat; static; inline;
  class function Inverse(const m: TUMat): TUMat; static; overload;
  class function Transpose(const m: TUMat): TUMat; static; overload;
  class function Normalize(const m: TUMat): TUMat; static; overload;
  procedure SetValue(
    const e00, e10, e20, e30: TUFloat;
    const e01, e11, e21, e31: TUFloat;
    const e02, e12, e22, e32: TUFloat;
    const e03, e13, e23, e33: TUFloat
  ); inline;
  function Inverse: TUMat; overload; inline;
  function Transpose: TUMat; overload; inline;
  function Normalize: TUMat; overload; inline;
  function ToString: String; inline;
end;

type TUVec2Impl = type helper for TUVec2
private
  function GetX: TUFloat; inline;
  procedure SetX(const Value: TUFloat); inline;
  function GetY: TUFloat; inline;
  procedure SetY(const Value: TUFloat); inline;
public
  property x: TUFloat read GetX write SetX;
  property y: TUFloat read GetY write SetY;
  class function Zero: TUVec2; static; inline;
  class function Make(const Ax, Ay: TUFloat): TUVec2; static; overload; inline;
  class function Make(const S: TUFloat): TUVec2; static; overload; inline;
  class function Dot(const v0, v1: TUVec2): TUFloat; static; overload; inline;
  class function Cross(const v0, v1: TUVec2): TUFloat; static; overload; inline;
  class function Norm(const v: TUVec2): TUVec2; static; overload; inline;
  procedure SetValue(const Ax, Ay: TUFloat); inline;
  function Transform(const r: TURot2): TUVec2; inline;
  function TransformInv(const r: TURot2): TUVec2; inline;
  function Dot(const v: TUVec2): TUFloat; overload; inline;
  function Cross(const v: TUVec2): TUFloat; overload; inline;
  function Norm: TUVec2; overload; inline;
  function IsZero: Boolean; inline;
  function ToString: String; inline;
end;

type TUVec3Impl = type helper for TUVec3
private
  function GetX: TUFloat; inline;
  procedure SetX(const Value: TUFloat); inline;
  function GetY: TUFloat; inline;
  procedure SetY(const Value: TUFloat); inline;
  function GetZ: TUFloat; inline;
  procedure SetZ(const Value: TUFloat); inline;
public
  property x: TUFloat read GetX write SetX;
  property y: TUFloat read GetY write SetY;
  property z: TUFloat read GetZ write SetZ;
  class function Zero: TUVec3; static; inline;
  class function Make(const Ax, Ay, Az: TUFloat): TUVec3; static; overload; inline;
  class function Make(const v2: TUVec2; const Az: TUFloat): TUVec3; static; overload; inline;
  class function Make(const s: TUFloat): TUVec3; static; overload;
  class function Len(const v: TUVec3): TUFloat; static; overload; inline;
  class function LenSq(const v: TUVec3): TUFloat; static; overload; inline;
  class function Dot(const v0, v1: TUVec3): TUFloat; static; overload; inline;
  class function Cross(const v0, v1: TUVec3): TUVec3; static; overload; inline;
  class function Norm(const v: TUVec3): TUVec3; static; overload; inline;
  procedure SetValue(const Ax, Ay, Az: TUFloat); inline;
  function Transform3x3(const m: TUMat): TUVec3;
  function Transform4x3(const m: TUMat): TUVec3;
  function Transform4x4(const m: TUMat): TUVec3;
  function TransformQuat(const q: TUQuat): TUVec3; inline;
  function Len: TUFloat;
  function LenSq: TUFloat;
  function Dot(const v: TUVec3): TUFloat; overload;
  function Cross(const v: TUVec3): TUVec3; overload;
  function Norm: TUVec3; overload;
  function xy: TUVec2; inline;
  function AngleTo(const v: TUVec3): TUFloat; inline;
  function RotationTo(const v: TUVec3): TUQuat; inline;
  function IsZero: Boolean; inline;
  function ToString: String; inline;
end;

type TUVec4Impl = type helper for TUVec4
private
  function GetX: TUFloat; inline;
  procedure SetX(const Value: TUFloat); inline;
  function GetY: TUFloat; inline;
  procedure SetY(const Value: TUFloat); inline;
  function GetZ: TUFloat; inline;
  procedure SetZ(const Value: TUFloat); inline;
  function GetW: TUFloat; inline;
  procedure SetW(const Value: TUFloat); inline;
public
  property x: TUFloat read GetX write SetX;
  property y: TUFloat read GetY write SetY;
  property z: TUFloat read GetZ write SetZ;
  property w: TUFloat read GetW write SetW;
  class function Zero: TUVec4; static; inline;
  class function Make(const Ax, Ay, Az, Aw: TUFloat): TUVec4; static; overload; inline;
  class function Make(const v: TUVec3; const Aw: TUFloat): TUVec4; static; overload; inline;
  class function Make(const v0, v1: TUVec2): TUVec4; static; overload; inline;
  class function Make(const v: TUVec2; const Az, Aw: TUFloat): TUVec4; static; overload; inline;
  class function Make(const s: TUFloat): TUVec4; static; overload; inline;
  class function Dot(const v0, v1: TUVec4): TUFloat; static; overload; inline;
  class function Norm(const v: TUVec4): TUVec4; static; overload; inline;
  procedure SetValue(const Ax, Ay, Az, Aw: TUFloat); inline;
  function Dot(const v: TUVec4): TUFloat; overload; inline;
  function Norm: TUVec4; overload; inline;
  function xyz: TUVec3; inline;
  function xy: TUVec2; inline;
  function IsZero: Boolean; inline;
  function ToString: String; inline;
end;

type TUQuatImpl = type helper for TUQuat
private
  function GetX: TUFloat; inline;
  procedure SetX(const Value: TUFloat); inline;
  function GetY: TUFloat; inline;
  procedure SetY(const Value: TUFloat); inline;
  function GetZ: TUFloat; inline;
  procedure SetZ(const Value: TUFloat); inline;
  function GetW: TUFloat; inline;
  procedure SetW(const Value: TUFloat); inline;
public
  property x: TUFloat read GetX write SetX;
  property y: TUFloat read GetY write SetY;
  property z: TUFloat read GetZ write SetZ;
  property w: TUFloat read GetW write SetW;
  class function Identity: TUQuat; static; inline;
  class function Make(const Ax, Ay, Az, Aw: TUFloat): TUQuat; static; inline;
  class function Norm(const v: TUQuat): TUQuat; static; overload; inline;
  function Norm: TUQuat; inline;
  function ToString: String; inline;
end;

type TUSwizzle = object
private
  const DefaultSwizzle: UInt8 = (0 or (1 shl 2) or (2 shl 4) or (3 shl 6));
  var _Remap: UInt8;
  function GetOffset(const Index: UInt8): UInt8; inline;
  procedure SetOffset(const Index: UInt8; const Value: UInt8); inline;
public
  property Remap: UInt8 read _Remap;
  property Offset[const Index: UInt8]: UInt8 read GetOffset write SetOffset; default;
  class function Make(
    const ord0: UInt8 = 0;
    const ord1: UInt8 = 1;
    const ord2: UInt8 = 2;
    const ord3: UInt8 = 3
  ): TUSwizzle;
  procedure SetIdentity; inline;
  procedure SetValue(
    const ord0: UInt8 = 0;
    const ord1: UInt8 = 1;
    const ord2: UInt8 = 2;
    const ord3: UInt8 = 3
  );
end;

type TURot2Impl = type helper for TURot2
strict private
  function GetAngSin: TUFloat; inline;
  function GetAngCos: TUFloat; inline;
  function GetAngle: TUFloat; inline;
  function GetAxisX: TUVec2; inline;
  function GetAxisY: TUVec2; inline;
  procedure SetAngSin(const Value: TUFloat); inline;
  procedure SetAngCos(const Value: TUFloat); inline;
  procedure SetAngle(const Angle: TUFloat); inline;
  procedure SetAxisX(const Value: TUVec2); inline;
  procedure SetAxisY(const Value: TUVec2); inline;
public
  property AngSin: TUFloat read GetAngSin write SetAngSin;
  property AngCos: TUFloat read GetAngCos write SetAngCos;
  property Angle: TUFloat read GetAngle write SetAngle;
  property AxisX: TUVec2 read GetAxisX write SetAxisX;
  property AxisY: TUVec2 read GetAxisY write SetAxisY;
  class function Identity: TURot2; static; inline;
  class function Make(const Angle: TUFloat): TURot2; static; inline;
  class function Make(const Rc, Rs: TUFloat): TURot2; static; inline;
  class function Norm(const Value: TURot2): TURot2; static; overload; inline;
  procedure SetValue(const Rc, Rs: TUFloat); inline;
  function Norm: TURot2; inline;
  function Transform(const v: TUVec2): TUVec2; inline;
  function TransformInv(const v: TUVec2): TUVec2; inline;
  function ToString: String; inline;
end;

type TUCriticalSection = record
strict private
  var _cs: TRTLCriticalSection;
  procedure Initialize; inline;
  procedure Finalize; inline;
public
  procedure Enter; inline;
  function TryEnter: Boolean; inline;
  procedure Leave; inline;
  class operator Initialize(var v: TUCriticalSection);
  class operator Finalize(var v: TUCriticalSection);
end;

type TUAtomicLock = record
strict private
  var _Lock: LongWord;
  procedure Initialize; inline;
  procedure Finalize; inline;
public
  function TryLock: Boolean; inline;
  procedure Lock; inline;
  procedure Unlock; inline;
  class operator Initialize(var v: TUAtomicLock);
  class operator Finalize(var v: TUAtomicLock);
end;

type TUReadWriteLock = record
strict private
  var _ReadLock: TUAtomicLock;
  var _WriteLock: TUAtomicLock;
  var _ReadCount: LongWord;
  procedure Initialize; inline;
  procedure Finalize; inline;
public
  function TryReadLock: Boolean; inline;
  procedure ReadLock; inline;
  procedure ReadUnlock; inline;
  function TryWriteLock: Boolean; inline;
  procedure WriteLock; inline;
  procedure WriteUnlock; inline;
  class operator Initialize(var v: TUReadWriteLock);
  class operator Finalize(var v: TUReadWriteLock);
end;

type TUEvent = record
strict private
  var _Event: PRTLEvent;
  procedure Initialize; inline;
  procedure Finalize; inline;
public
  procedure Signal; inline;
  procedure Unsignal; inline;
  procedure WaitFor; inline;
  procedure WaitFor(const Timeout: LongWord); inline;
  class operator Initialize(var v: TUEvent);
  class operator Finalize(var v: TUEvent);
end;

type generic TUTask<T> = record
public
  type TRes = T;
  type TFunc = function (const Args: array of const): TRes of object;
strict private
  type TTaskThread = class (TThread)
  private
    var _StartTime: QWord;
  public
    var Ref: Integer;
    var Proc: TFunc;
    var Args: array of TVarRec;
    var Res: TRes;
    procedure AfterConstruction; override;
    procedure Execute; override;
    function TimeRunning: QWord;
    function RefInc: Integer;
    function RefDec: Integer;
    function RefDecKillThread: Integer;
  end;
  var _Thread: TTaskThread;
  procedure Initialize; inline;
  procedure Finalize; inline;
  procedure SetThread(const TaskThread: TTaskThread);
public
  function TimeRunning: QWord;
  function IsComplete: Boolean;
  function IsStarted: Boolean;
  function TaskResult: TRes;
  procedure Reset;
  procedure WaitFor;
  procedure Kill;
  procedure TimeoutKill(const Timeout: QWord);
  class function StartTask(const Proc: TFunc; const Args: array of const): TUTask; static;
  class operator Initialize(var v: TUTask);
  class operator Finalize(var v: TUTask);
  class operator Copy(constref Src: TUTask; var Dst: TUTask);
end;
type TUTaskString = specialize TUTask<String>;
type TUTaskBool = specialize TUTask<Boolean>;
type TUTaskInt = specialize TUTask<Integer>;

type generic TUSharedRef<T> = record
public
  type TPtr = T;
  type TSelf = specialize TUSharedRef<T>;
private
  var _Ptr: IInterface;
  function GetPtr: TPtr; inline;
  procedure SetPtr(const Value: TPtr); inline;
  procedure Initialize;
public
  property Ptr: TPtr read GetPtr write SetPtr;
  function IsValid: Boolean; inline;
  class operator Initialize(var v: TSelf);
  class operator := (const Value: TPtr): TSelf; inline;
  class operator := (const Value: Pointer): TSelf; inline;
  class operator = (v1, v2: TSelf): Boolean; inline;
end;

type generic TUWeakRef<T> = record
public
  type TPtr = T;
  type TSelf = specialize TUWeakRef<T>;
  type TShared = specialize TUSharedRef<T>;
private
  var _Weak: IInterface;
  procedure Assign(const Value: TPtr);
  function GetPtr: TPtr; inline;
public
  property Ptr: TPtr read GetPtr;
  function IsValid: Boolean; inline;
  function AsShared: TShared; inline;
  class operator := (const Value: TPtr): TSelf; inline;
  class operator := (const Value: TShared): TSelf; inline;
end;

type TURefClass = class;
type TUWeakCounter = class (TInterfacedObject)
private
  var _Obj: TURefClass;
public
  property Obj: TURefClass read _Obj write _Obj;
  constructor Create(const AObj: TURefClass);
  destructor Destroy; override;
end;

type TURefClass = class (TObject, IUnknown)
public
  type TShared = specialize TUSharedRef<TURefClass>;
protected
  var _RefCount: UInt32;
  var _Weak: TUWeakCounter;
  var _References: array of TShared;
  function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid : tguid;out obj) : longint; call;
  function _AddRef : longint; call;
  function _Release : longint; call;
protected
  property Weak: TUWeakCounter read _Weak write _Weak;
public
  procedure AfterConstruction; override;
  procedure BeforeDestruction; override;
  class function NewInstance: TObject; override;
  procedure AddReference(const Obj: TURefClass);
  procedure RemoveReference(const Obj: TURefClass);
  property RefCount: UInt32 read _RefCount;
end;

type TUStreamHelper = class (TURefClass)
private
  var _Stream: TStream;
  var _PosStack: array of Int64;
  function GetSize: Int64; inline;
  function GetPosition: Int64; inline;
  procedure SetPosition(const Value: Int64); inline;
  function GetRemaining: Int64; inline;
public
  property Stream: TStream read _Stream;
  property Size: Int64 read GetSize;
  property Position: Int64 read GetPosition write SetPosition;
  property Remaining: Int64 read GetRemaining;
  procedure PosPush;
  procedure PosPop;
  procedure PosDiscard;
  function IsEoF: Boolean; inline;
  function ReadBuffer(const Buffer: Pointer; const Count: Int64): Int64; inline;
  function ReadBool: Boolean; inline;
  function ReadUInt8: UInt8; inline;
  function ReadUInt16: UInt16; inline;
  function ReadUInt32: UInt32; inline;
  function ReadUInt64: UInt64; inline;
  function ReadInt8: Int8; inline;
  function ReadInt16: Int16; inline;
  function ReadInt32: Int32; inline;
  function ReadInt64: Int64; inline;
  function ReadFloat: Single; inline;
  function ReadDouble: Double; inline;
  function ReadString: String; inline;
  function ReadStringNT: String; inline;
  generic function Read<T>: T; inline;
  function WriteBuffer(const Buffer: Pointer; const Count: Int64): Int64; inline;
  procedure WriteBool(const Value: Boolean); inline;
  procedure WriteUInt8(const value: UInt8); inline;
  procedure WriteUInt16(const value: UInt16); inline;
  procedure WriteUInt32(const value: UInt32); inline;
  procedure WriteUInt64(const value: UInt64); inline;
  procedure WriteInt8(const value: Int8); inline;
  procedure WriteInt16(const value: Int16); inline;
  procedure WriteInt32(const value: Int32); inline;
  procedure WriteInt64(const value: Int64); inline;
  procedure WriteFloat(const value: Single); inline;
  procedure WriteDouble(const value: Double); inline;
  procedure WriteStringRaw(const Value: String); inline;
  procedure WriteString(const Value: String); inline;
  procedure WriteStringNT(const Value: String); inline;
  generic procedure Write<T>(const Value: T); inline;
  procedure Skip(const Count: Int64); inline;
  procedure SkipString; inline;
  function ToString: String; override;
  constructor Create(const AStream: TStream);
end;
type TUStreamHelperShared = specialize TUSharedRef<TUStreamHelper>;

type TUConstMemoryStream = class (TStream)
private
  var _Memory: Pointer;
  var _Size: Int64;
  var _Position: Int64;
protected
  function GetSize: Int64; override;
  function GetPosition: Int64; override;
public
  function Read(var Buffer; Count: LongInt): LongInt; override;
  function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  constructor Create(const Buffer: Pointer; const BufferSize: UInt32);
end;

{$push}
{$m+}
type TUSerializable = class (TURefClass)
protected
  var _TypeInfo: PTypeInfo;
  var _TypeData: PTypeData;
  var _PropList: array of PPropInfo;
  function VerifyProp(const Index: Integer; const PropType: TTypeKinds = tkAny): Boolean;
  function VerifyArrayProp(const Index, ArrayIndex: Integer; const PropType: TTypeKinds = tkAny): Boolean;
  function GetArrayData(const Index: Integer): Pointer;
  function GetOrdSize(const OrdType: TOrdType): UInt32; inline;
  function GetArrayElementTypeInfo(const Index: Integer): PTypeInfo;
  function GetPropCount: Integer; inline;
  function GetPropInfo(const Index: Integer): PPropInfo; inline;
  function GetPropArrayInfo(const Index: Integer): PTypeInfo; inline;
  function GetPropEnum(const Index: Integer): Int32; inline;
  procedure SetPropEnum(const Index: Integer; const Value: Int32); inline;
  function GetPropBool(const Index: Integer): Boolean; inline;
  procedure SetPropBool(const Index: Integer; const Value: Boolean); inline;
  function GetPropInt8(const Index: Integer): Int8; inline;
  procedure SetPropInt8(const Index: Integer; const Value: Int8); inline;
  function GetPropInt16(const Index: Integer): Int16; inline;
  procedure SetPropInt16(const Index: Integer; const Value: Int16); inline;
  function GetPropInt32(const Index: Integer): Int32; inline;
  procedure SetPropInt32(const Index: Integer; const Value: Int32); inline;
  function GetPropInt64(const Index: Integer): Int64; inline;
  procedure SetPropInt64(const Index: Integer; const Value: Int64); inline;
  function GetPropUInt8(const Index: Integer): UInt8; inline;
  procedure SetPropUInt8(const Index: Integer; const Value: UInt8); inline;
  function GetPropUInt16(const Index: Integer): UInt16; inline;
  procedure SetPropUInt16(const Index: Integer; const Value: UInt16); inline;
  function GetPropUInt32(const Index: Integer): UInt32; inline;
  procedure SetPropUInt32(const Index: Integer; const Value: UInt32); inline;
  function GetPropUInt64(const Index: Integer): UInt64; inline;
  procedure SetPropUInt64(const Index: Integer; const Value: UInt64); inline;
  function GetPropFloat(const Index: Integer): Single; inline;
  procedure SetPropFloat(const Index: Integer; const Value: Single); inline;
  function GetPropDouble(const Index: Integer): Double; inline;
  procedure SetPropDouble(const Index: Integer; const Value: Double); inline;
  function GetPropString(const Index: Integer): String; inline;
  procedure SetPropString(const Index: Integer; const Value: String); inline;
  function GetPropClass(const Index: Integer): TObject; inline;
  procedure SetPropClass(const Index: Integer; const Value: TObject); inline;
  function GetPropArrayLength(const Index: Integer): Integer; inline;
  procedure SetPropArrayLength(const Index: Integer; const Value: Integer);
  function GetPropArrayEnum(const Index, ArrayIndex: Integer): Int32; inline;
  procedure SetPropArrayEnum(const Index, ArrayIndex: Integer; const Value: Int32); inline;
  function GetPropArrayBool(const Index, ArrayIndex: Integer): Boolean; inline;
  procedure SetPropArrayBool(const Index, ArrayIndex: Integer; const Value: Boolean); inline;
  function GetPropArrayInt8(const Index, ArrayIndex: Integer): Int8; inline;
  procedure SetPropArrayInt8(const Index, ArrayIndex: Integer; const Value: Int8); inline;
  function GetPropArrayInt16(const Index, ArrayIndex: Integer): Int16; inline;
  procedure SetPropArrayInt16(const Index, ArrayIndex: Integer; const Value: Int16); inline;
  function GetPropArrayInt32(const Index, ArrayIndex: Integer): Int32; inline;
  procedure SetPropArrayInt32(const Index, ArrayIndex: Integer; const Value: Int32); inline;
  function GetPropArrayInt64(const Index, ArrayIndex: Integer): Int64; inline;
  procedure SetPropArrayInt64(const Index, ArrayIndex: Integer; const Value: Int64); inline;
  function GetPropArrayUInt8(const Index, ArrayIndex: Integer): UInt8; inline;
  procedure SetPropArrayUInt8(const Index, ArrayIndex: Integer; const Value: UInt8); inline;
  function GetPropArrayUInt16(const Index, ArrayIndex: Integer): UInt16; inline;
  procedure SetPropArrayUInt16(const Index, ArrayIndex: Integer; const Value: UInt16); inline;
  function GetPropArrayUInt32(const Index, ArrayIndex: Integer): UInt32; inline;
  procedure SetPropArrayUInt32(const Index, ArrayIndex: Integer; const Value: UInt32); inline;
  function GetPropArrayUInt64(const Index, ArrayIndex: Integer): UInt64; inline;
  procedure SetPropArrayUInt64(const Index, ArrayIndex: Integer; const Value: UInt64); inline;
  function GetPropArrayFloat(const Index, ArrayIndex: Integer): Single; inline;
  procedure SetPropArrayFloat(const Index, ArrayIndex: Integer; const Value: Single); inline;
  function GetPropArrayDouble(const Index, ArrayIndex: Integer): Double; inline;
  procedure SetPropArrayDouble(const Index, ArrayIndex: Integer; const Value: Double); inline;
  function GetPropArrayString(const Index, ArrayIndex: Integer): String; inline;
  procedure SetPropArrayString(const Index, ArrayIndex: Integer; const Value: String); inline;
  function GetPropArrayClass(const Index, ArrayIndex: Integer): TObject; inline;
  procedure SetPropArrayClass(const Index, ArrayIndex: Integer; const Value: TObject); inline;
  generic function GetDynArrayLength<T>(const Index: Integer): Integer;
  generic procedure SetDynArrayLength<T>(const Index: Integer; const Value: Integer);
  generic function GetDynArrayElement<T>(const Index, ArrayIndex: Integer): T;
  generic procedure SetDynArrayElement<T>(const Index, ArrayIndex: Integer; const Value: T);
public
  property PropCount: Integer read GetPropCount;
  property PropInfo[const Index: Integer]: PPropInfo read GetPropInfo;
  property PropArrayInfo[const Index: Integer]: PTypeInfo read GetPropArrayInfo;
  property PropEnum[const Index: Integer]: Int32 read GetPropEnum write SetPropEnum;
  property PropBool[const Index: Integer]: Boolean read GetPropBool write SetPropBool;
  property PropInt8[const Index: Integer]: Int8 read GetPropInt8 write SetPropInt8;
  property PropInt16[const Index: Integer]: Int16 read GetPropInt16 write SetPropInt16;
  property PropInt32[const Index: Integer]: Int32 read GetPropInt32 write SetPropInt32;
  property PropInt64[const Index: Integer]: Int64 read GetPropInt64 write SetPropInt64;
  property PropUInt8[const Index: Integer]: UInt8 read GetPropUInt8 write SetPropUInt8;
  property PropUInt16[const Index: Integer]: UInt16 read GetPropUInt16 write SetPropUInt16;
  property PropUInt32[const Index: Integer]: UInt32 read GetPropUInt32 write SetPropUInt32;
  property PropUInt64[const Index: Integer]: UInt64 read GetPropUInt64 write SetPropUInt64;
  property PropFloat[const Index: Integer]: Single read GetPropFloat write SetPropFloat;
  property PropDouble[const Index: Integer]: Double read GetPropDouble write SetPropDouble;
  property PropString[const Index: Integer]: String read GetPropString write SetPropString;
  property PropClass[const Index: Integer]: TObject read GetPropClass write SetPropClass;
  property PropArrayEnum[const Index, ArrayIndex: Integer]: Int32 read GetPropArrayEnum write SetPropArrayEnum;
  property PropArrayBool[const Index, ArrayIndex: Integer]: Boolean read GetPropArrayBool write SetPropArrayBool;
  property PropArrayInt8[const Index, ArrayIndex: Integer]: Int8 read GetPropArrayInt8 write SetPropArrayInt8;
  property PropArrayInt16[const Index, ArrayIndex: Integer]: Int16 read GetPropArrayInt16 write SetPropArrayInt16;
  property PropArrayInt32[const Index, ArrayIndex: Integer]: Int32 read GetPropArrayInt32 write SetPropArrayInt32;
  property PropArrayInt64[const Index, ArrayIndex: Integer]: Int64 read GetPropArrayInt64 write SetPropArrayInt64;
  property PropArrayUInt8[const Index, ArrayIndex: Integer]: UInt8 read GetPropArrayUInt8 write SetPropArrayUInt8;
  property PropArrayUInt16[const Index, ArrayIndex: Integer]: UInt16 read GetPropArrayUInt16 write SetPropArrayUInt16;
  property PropArrayUInt32[const Index, ArrayIndex: Integer]: UInt32 read GetPropArrayUInt32 write SetPropArrayUInt32;
  property PropArrayUInt64[const Index, ArrayIndex: Integer]: UInt64 read GetPropArrayUInt64 write SetPropArrayUInt64;
  property PropArrayFloat[const Index, ArrayIndex: Integer]: Single read GetPropArrayFloat write SetPropArrayFloat;
  property PropArrayDouble[const Index, ArrayIndex: Integer]: Double read GetPropArrayDouble write SetPropArrayDouble;
  property PropArrayString[const Index, ArrayIndex: Integer]: String read GetPropArrayString write SetPropArrayString;
  property PropArrayClass[const Index, ArrayIndex: Integer]: TObject read GetPropArrayClass write SetPropArrayClass;
  property PropArrayLength[const Index: Integer]: Integer read GetPropArrayLength write SetPropArrayLength;
  function FindProp(const Name: String; const PropType: TTypeKinds = tkAny): Integer;
  procedure AfterConstruction; override;
  procedure BeforeDestruction; override;
  procedure SerializeTo(const Stream: TStream); virtual; overload;
  procedure SerializeTo(const FileName: String); virtual; overload;
  procedure SerializeFrom(const Stream: TStream); virtual; overload;
  procedure SerializeFrom(const FileName: String); virtual; overload;
  procedure Assign(const Serializable: TUSerializable); virtual;
  procedure Dump(const Offset: String = '');
end;
{$pop}

type TUTokenType = (tt_eof, tt_error, tt_symbol, tt_word, tt_keyword, tt_string, tt_number);
type TUTokenTypes = set of TUTokenType;
type TUParserToken = record
  Value: String;
  TokenType: TUTokenType;
  class operator = (a, b: TUParserToken): Boolean; inline;
  class operator in (a: TUParserToken; b: array of TUParserToken): Boolean;
  class operator = (a: TUParserToken; b: String): Boolean; inline;
  class operator = (a: TUParserToken; b: array of String): Boolean;
  class operator = (a: TUParserToken; b: TUTokenType): Boolean; inline;
  class operator in (a: TUParserToken; b: TUTokenTypes): Boolean; inline;
  class operator := (a: TUParserToken): String; inline;
end;
function UParserToken(const Value: String; const TokenType: TUTokenType): TUParserToken; inline;

type TUParserSyntax = object
public
  Comment: array of array[0..1] of String;
  CommentLine: array of String;
  Strings: array of String;
  Symbols: array of String;
  Keywords: array of String;
  CaseSensitive: Boolean;
  procedure AddComment(const ACommentStart, ACommentEnd: String);
  procedure AddCommentLine(const ACommentLine: String);
  procedure AddString(const AStringStartEnd: String);
  procedure AddSymbol(const ASymbol: String);
  procedure AddSymbols(const ASymbols: array of String);
  procedure AddKeyword(const AKeyword: String);
  procedure AddKeywords(const AKeywords: array of String);
  procedure Reset;
end;
type PUParserSyntax = ^TUParserSyntax;

type TUParser = class (TURefClass)
strict private
  type TState = record
    Line, Position: Int32;
  end;
  var _Position: Int32;
  var _Text: array of AnsiChar;
  var _Line: Int32;
  var _DefaultSyntax: TUParserSyntax;
  var _Syntax: PUParserSyntax;
  var _States: array of TState;
  var _SyntaxStack: array of PUParserSyntax;
  function GetComment(const Index: Int32): String; inline;
  function GetCommentCount: Int32; inline;
  function GetCommentLine(const index: Int32): String; inline;
  function GetCommentLineCount: Int32; inline;
  function GetKeyword(const Index: Int32): String; inline;
  function GetKeywordCount: Int32; inline;
  function GetString(const Index: Int32): String; inline;
  function GetStringCount: Int32; inline;
  function GetSymbol(const Index: Int32): String; inline;
  function GetSymbolCount: Int32; inline;
  function GetText: String; inline;
  function GetTextLength: Int32; inline;
  procedure SetSyntax(const Value: PUParserSyntax); inline;
public
  property Text: String read GetText;
  property TextLength: Int32 read GetTextLength;
  property Position: Int32 read _Position write _Position;
  property Line: Int32 read _Line;
  property CommentCount: Int32 read GetCommentCount;
  property Comments[const Index: Int32]: String read GetComment;
  property CommnetLineCount: Int32 read GetCommentLineCount;
  property CommentLines[const Index: Int32]: String read GetCommentLine;
  property StringCount: Int32 read GetStringCount;
  property Strings[const Index: Int32]: String read GetString;
  property SymbolCount: Int32 read GetSymbolCount;
  property Symbols[const Index: Int32]: String read GetSymbol;
  property KeywordCount: Int32 read GetKeywordCount;
  property Keywords[const Index: Int32]: String read GetKeyword;
  property Syntax: PUParserSyntax read _Syntax write SetSyntax;
  constructor Create;
  constructor Create(const ParseText: String; const CaseSensitive: Boolean = False); virtual;
  destructor Destroy; override;
  procedure Parse(const ParseText: String);
  procedure AddComment(const CommentStart, CommentEnd: String);
  procedure AddCommentLine(const CommentLine: String);
  procedure AddString(const StringStartEnd: String);
  procedure AddSymbol(const Symbol: String);
  procedure AddKeyword(const Keyword: String);
  procedure SkipSpaces;
  procedure StatePush;
  procedure StatePop;
  procedure StateDiscard;
  procedure StateLoad;
  procedure SyntaxPush;
  procedure SyntaxPop;
  function Read(const Count: Int32): String; overload;
  function Read(const Pos: Int32; const Count: Int32): String; overload;
  function IsAtSymbol: Int32;
  function IsAtKeyword: Int32;
  function IsAtCommentLine: Int32;
  function IsAtCommentStart: Int32;
  function IsAtCommentEnd: Int32;
  function IsAtString: Int32;
  function IsAtEoF: Boolean;
  function IsAtNewLine: Boolean;
  function NextToken: TUParserToken; overload;
  function NextToken(out TokenType: TUTokenType): String; overload;
  function CheckToken: TUParserToken;
  function OptionalToken(const Token: String): Boolean;
end;

type TUShortStringReader = object
private
  var _Ptr: Pointer;
public
  procedure Setup(const Str: ShortString);
  function ReadShortString: String;
  generic function Read<T>: T;
end;

generic TUMap<TKey, TValue> = record
public
  type TItem = record
    Key: TKey;
    Value: TValue;
  end;
private
  var _Items: array of TItem;
  function GetValue(const Index: Int32): TValue; inline;
public
  property Get[const Index: Int32]: TValue read GetValue; default;
  function GetKey(const Index: Int32): TKey; inline;
  function Add(const Key: TKey; const Value: TValue): Int32;
  function HasKey(const Key: TKey): Boolean;
  procedure RemoveByKey(const Key: TKey);
  procedure RemoveByValue(const Value: TValue);
  procedure RemoveByIndex(const Index: Int32);
  function FindIndexByKey(const Key: TKey): Int32;
  function FindIndexByValue(const Value: TValue): Int32;
  function FindValueByKey(const Key: TKey): TValue;
  function FindValueByIndex(const Index: Int32): TValue;
  function Count: Int32; inline;
  procedure Clear;
end;

generic TUArray<T> = array of T;

type TUXML = class (TURefClass)
public
  type TAttribute = class
  public
    var Name: String;
    var Value: String;
  end;
  type TEnumerator = class
  private
    var n: TUXML;
    var i: Int32;
    function GetCurrent: TUXML;
  public
    constructor Create(const Node: TUXML);
    function MoveNext: Boolean;
    property Current: TUXML read GetCurrent;
  end;
protected
  class var _SyntaxTags: TUParserSyntax;
  class var _SyntaxContent: TUParserSyntax;
private
  var _Name: String;
  var _Content: String;
  var _Attributes: array of TAttribute;
  var _Children: array of TUXML;
  function ReadXML(const p: TUParser): Boolean;
  function WriteXML(const Offset: String = ''): String;
  function GetContent: String;
  function GetAttribute(const Index: Integer): TAttribute; inline;
  function GetAttributeValue(const AttName: String): String;
  function GetAttributeCount: Integer; inline;
  function GetChild(const Index: Integer): TUXML; inline;
  function GetChildCount: Integer; inline;
  function GetChildContent(const NodeName: String): String; inline;
public
  property Name: String read _Name;
  property Content: String read GetContent write _Content;
  property Attributes[const Index: Integer]: TAttribute read GetAttribute;
  property AttributeValue[const AttName: String]: String read GetAttributeValue;
  property AttributeCount: Integer read GetAttributeCount;
  property Children[const Index: Integer]: TUXML read GetChild; default;
  property ChildCount: Integer read GetChildCount;
  property ChildContent[const NodeName: String]: String read GetChildContent;
  function GetEnumerator: TEnumerator;
  class constructor CreateClass;
  constructor Create(const NodeName: String);
  destructor Destroy; override;
  function IsPlainText: Boolean;
  procedure AddAttribute(const AttName, AttValue: String);
  function FindAttribute(const AttName: String): TAttribute;
  function FindChild(const NodeName: String): TUXML;
  class function Load(const XML: String): TUXML;
  class function Load(const Stream: TStream): TUXML;
  class function LoadFromFile(const FileName: String): TUXML;
  function Save: String;
  procedure Save(const Stream: TStream);
  procedure SaveToFile(const FileName: String);
end;
type TUXMLRef = specialize TUSharedRef<TUXML>;

type TUJson = class (TURefClass)
public
  type TNodeType = (nt_invalid, nt_value, nt_object, nt_array);
  type TNamedNode = record
    Name: String;
    Node: TUJson;
  end;
  type TContent = array of TNamedNode;
  type TElements = array of TUJson;
  type TEnumerator = class
  private
    var n: TUJson;
    var i: Int32;
    function GetCurrent: TUJson;
  public
    constructor Create(const Node: TUJson);
    function MoveNext: Boolean;
    property Current: TUJson read GetCurrent;
  end;
protected
  class var _Syntax: TUParserSyntax;
private
  var _NodeType: TNodeType;
  var _Value: String;
  var _Content: TContent;
  var _Elements: TElements;
  function ReadJson(const p: TUParser): Boolean;
  procedure SetNodeType(const Value: TNodeType);
  function GetValue: String;
  function GetContent(const Key: String): TUJson; inline;
  function GetName(const Index: Int32): String; inline;
  function GetElement(const Index: Int32): TUJson; inline;
  function GetCount: Int32; inline;
  function GetIsSingleValue: Boolean; inline;
  function GetIsObject: Boolean; inline;
  function GetIsArray: Boolean; inline;
  function GetIsNumber: Boolean; inline;
  function GetIsNull: Boolean; inline;
public
  property NodeType: TNodeType read _NodeType write SetNodeType;
  property Value: String read GetValue;
  property Content[const Key: String]: TUJson read GetContent; default;
  property Name[const Index: Int32]: String read GetName;
  property Element[const Index: Int32]: TUJson read GetElement;
  property Count: Int32 read GetCount;
  property IsSingleValue: Boolean read GetIsSingleValue;
  property IsObject: Boolean read GetIsObject;
  property IsArray: Boolean read GetIsArray;
  property IsNumber: Boolean read GetIsNumber;
  property IsNull: Boolean read GetIsNull;
  function GetEnumerator: TEnumerator;
  function FormatJson(const Offset: String = ''): String;
  class constructor CreateClass;
  constructor Create;
  destructor Destroy; override;
  class function Load(const Json: String): TUJson;
  class function Load(const Stream: TStream): TUJson;
  class function LoadFromFile(const FileName: String): TUJson;
  function Save: String;
  procedure Save(const Stream: TStream);
  procedure SaveToFile(const FileName: String);
end;
type TUJsonRef = specialize TUSharedRef<TUJson>;

procedure UClear(out x; const Size: UInt32);
procedure UMove(out Dest; const Src; const Size: UInt32);
function USignOf(const v: Int64): Int64;
function UIntToPtr(const i: PtrUInt): Pointer;
function UCopyVarRec(constref src: TVarRec): TVarRec;
function UCopyVarRecArr(constref src: array of TVarRec): TUVarRecArr;
procedure UFinalizeVarRec(var vr: TVarRec);
procedure UFinalizeVarRecArr(var arr: array of TVarRec);
function UIntToBool(const i: Integer): Boolean;
function UBoolToInt(const b: Boolean): Integer;
function UBoolToStr(const b: Boolean): String;
generic function UMin<T>(const a, b: T): T; inline; overload;
function UMin(const a, b: Int8): Int8; inline; overload;
function UMin(const a, b: Int16): Int16; inline; overload;
function UMin(const a, b: Int32): Int32; inline; overload;
function UMin(const a, b: Int64): Int64; inline; overload;
function UMin(const a, b: UInt8): UInt8; inline; overload;
function UMin(const a, b: UInt16): UInt16; inline; overload;
function UMin(const a, b: UInt32): UInt32; inline; overload;
function UMin(const a, b: UInt64): UInt64; inline; overload;
function UMin(const a, b: TUFloat): TUFloat; inline; overload;
function UMin(const a, b: TUDouble): TUDouble; inline; overload;
function UMin(const a, b: TUVec2): TUVec2; inline; overload;
function UMin(const a, b: TUVec3): TUVec3; inline; overload;
function UMin(const a, b: TUVec4): TUVec4; inline; overload;
generic function UMax<T>(const a, b: T): T; inline; overload;
function UMax(const a, b: Int8): Int8; inline; overload;
function UMax(const a, b: Int16): Int16; inline; overload;
function UMax(const a, b: Int32): Int32; inline; overload;
function UMax(const a, b: Int64): Int64; inline; overload;
function UMax(const a, b: UInt8): UInt8; inline; overload;
function UMax(const a, b: UInt16): UInt16; inline; overload;
function UMax(const a, b: UInt32): UInt32; inline; overload;
function UMax(const a, b: UInt64): UInt64; inline; overload;
function UMax(const a, b: TUFloat): TUFloat; inline; overload;
function UMax(const a, b: TUDouble): TUDouble; inline; overload;
function UMax(const a, b: TUVec2): TUVec2; inline; overload;
function UMax(const a, b: TUVec3): TUVec3; inline; overload;
function UMax(const a, b: TUVec4): TUVec4; inline; overload;
generic function UClamp<T>(const v, MinV, MaxV: T): T; inline; overload;
function UClamp(const v, MinV, MaxV: Int8): Int8; inline; overload;
function UClamp(const v, MinV, MaxV: Int16): Int16; inline; overload;
function UClamp(const v, MinV, MaxV: Int32): Int32; inline; overload;
function UClamp(const v, MinV, MaxV: Int64): Int64; inline; overload;
function UClamp(const v, MinV, MaxV: UInt8): UInt8; inline; overload;
function UClamp(const v, MinV, MaxV: UInt16): UInt16; inline; overload;
function UClamp(const v, MinV, MaxV: UInt32): UInt32; inline; overload;
function UClamp(const v, MinV, MaxV: UInt64): UInt64; inline; overload;
function UClamp(const v, MinV, MaxV: TUFloat): TUFloat; inline; overload;
function UClamp(const v, MinV, MaxV: TUDouble): TUDouble; inline; overload;
function UClamp(const v, MinV, MaxV: TUVec2): TUVec2; inline; overload;
function UClamp(const v, MinV, MaxV: TUVec3): TUVec3; inline; overload;
function UClamp(const v, MinV, MaxV: TUVec4): TUVec4; inline; overload;
generic function ULerp<T>(const a, b: T; const s: TUFloat): T; inline; overload;
function ULerp(const a, b: TUFloat; const s: TUFloat): TUFloat; inline; overload;
function ULerp(const a, b: TUVec2; const s: TUFloat): TUVec2; inline; overload;
function ULerp(const a, b: TUVec3; const s: TUFloat): TUVec3; inline; overload;
function ULerp(const a, b: TUVec4; const s: TUFloat): TUVec4; inline; overload;
function ULerp(const a, b: TUMat; const s: TUFloat): TUMat; inline; overload;
function USmoothStep(const v, MinV, MaxV: TUFloat): TUFloat; inline;
generic function UBezier<T>(const f0, f1, f2, f3: T; const s: TUFloat): T; inline; overload;
function UBezier(const v0, v1, v2, v3: TUFloat; const s: TUFloat): TUFloat; inline; overload;
function UBezier(const v0, v1, v2, v3: TUDouble; const s: TUFloat): TUDouble; inline; overload;
function UBezier(const v0, v1, v2, v3: TUVec2; const s: TUFloat): TUVec2; inline; overload;
function UBezier(const v0, v1, v2, v3: TUVec3; const s: TUFloat): TUVec3; inline; overload;
function UBezier(const v0, v1, v2, v3: TUVec4; const s: TUFloat): TUVec4; inline; overload;
generic function UCatmullRom<T>(const v0, v1, v2, v3: T; const s: TUFloat): T; inline; overload;
function UCatmullRom(const v0, v1, v2, v3: TUFloat; const s: TUFloat): TUFloat; inline; overload;
function UCatmullRom(const v0, v1, v2, v3: TUDouble; const s: TUFloat): TUDouble; inline; overload;
function UCatmullRom(const v0, v1, v2, v3: TUVec2; const s: TUFloat): TUVec2; inline; overload;
function UCatmullRom(const v0, v1, v2, v3: TUVec3; const s: TUFloat): TUVec3; inline; overload;
function UCatmullRom(const v0, v1, v2, v3: TUVec4; const s: TUFloat): TUVec4; inline; overload;
generic function UEndianSwap<T>(const v: T): T; inline; overload;
function UEndianSwap(const v: UInt16): UInt16; inline; overload;
function UEndianSwap(const v: UInt32): UInt32; inline; overload;
function UEndianSwap(const v: UInt64): UInt64; inline; overload;
generic procedure USwap<T>(var a: T; var b: T); inline; overload;
procedure USwap(var a: Int8; var b: Int8); inline; overload;
procedure USwap(var a: Int16; var b: Int16); inline; overload;
procedure USwap(var a: Int32; var b: Int32); inline; overload;
procedure USwap(var a: Int64; var b: Int64); inline; overload;
procedure USwap(var a: UInt8; var b: UInt8); inline; overload;
procedure USwap(var a: UInt16; var b: UInt16); inline; overload;
procedure USwap(var a: UInt32; var b: UInt32); inline; overload;
procedure USwap(var a: UInt64; var b: UInt64); inline; overload;
procedure USwap(var a: TUFloat; var b: TUFloat); inline; overload;
procedure USwap(var a: TUDouble; var b: TUDouble); inline; overload;
procedure USwap(var a: TUVec2; var b: TUVec2); inline; overload;
procedure USwap(var a: TUVec3; var b: TUVec3); inline; overload;
procedure USwap(var a: TUVec4; var b: TUVec4); inline; overload;
procedure USwap(var a: TUMat; var b: TUMat); inline; overload;
generic function UEnumSetToStr<T>(const EnumSet: T): String;
generic function USelect<T>(const Cond: Boolean; constref IfTrue: T; constref IfFalse: T): T; inline;
function UCRC32(const CRC: UInt32; const Value: Pointer; const Count: UInt32): UInt32;
function UCRC64(const CRC: UInt64; const Value: Pointer; const Count: UInt32): UInt64;
procedure USinCos(const a: TUFloat; out s: TUFloat; out c: TUFloat);
function UCoTan(const x: TUFloat): TUFloat;
function UArcCos(const x: TUFloat): TUFloat;
function UArcTan2(const y, x: TUFloat): TUFloat;
function UPow(const b, e: TUFloat): TUFloat;
function UAddMat(const m0, m1: TUMat): TUMat;
function UAddMatFloat(const m: TUMat; const s: TUFloat): TUMat;
function USubMat(const m0, m1: TUMat): TUMat;
function USubMatFloat(const m: TUMat; const s: TUFloat): TUMat;
function UMulMat(const m0, m1: TUMat): TUMat;
function UMulMatFloat(const m: TUMat; const s: TUFloat): TUMat;
function UMulVec2Mat3x3(const v: TUVec2; const m: TUMat): TUVec2;
function UMulVec2Mat4x3(const v: TUVec2; const m: TUMat): TUVec2;
function UMulVec2Mat4x4(const v: TUVec2; const m: TUMat): TUVec2;
function UMulVec3Mat3x3(const v: TUVec3; const m: TUMat): TUVec3;
function UMulVec3Mat4x3(const v: TUVec3; const m: TUMat): TUVec3;
function UMulVec3Mat4x4(const v: TUVec3; const m: TUMat): TUVec3;
function UMulVec3Quat(const v: TUVec3; const q: TUQuat): TUVec3;
function UMulVec4Mat(const v: TUVec4; const m: TUMat): TUVec4;
function UTriangleNormal(const v0, v1, v2: TUVec3): TUVec3;
function UXc2DLineCircle(const v0, v1, c: TUVec2; const r: TUFloat; out x0, x1: TUVec2): Boolean;

function UStrExplode(const Str: String; const Separator: String): TUStrArr;
function UStrIsNumber(const Str: String): Boolean;
procedure UStrToFile(const FileName: String; const Str: String);
function UFileToStr(const FileName: String): String;
procedure ULog(const Text: String; const Offset: Int32 = 0);
procedure ULogOffset(const Offset: Int32);

generic procedure UArrSort<T>(var Arr: array of T);
generic procedure UArrAppend<T>(var Arr: specialize TUArray<T>; const Item: T); overload;
generic procedure UArrAppend<T>(var Arr: specialize TUArray<T>; const Other: specialize TUArray<T>); overload;
generic procedure UArrInsert<T>(var Arr: specialize TUArray<T>; const Item: T; const Position: Int32);
generic procedure UArrDelete<T>(var Arr: specialize TUArray<T>; const DelStart: Int32; const DelCount: Int32 = 1);
generic procedure UArrRemove<T>(var Arr: specialize TUArray<T>; const Item: T);
generic function UArrPop<T>(var Arr: specialize TUArray<T>): T;
generic function UArrFind<T>(const Arr: specialize TUArray<T>; const Item: T): Int32;
generic procedure UArrClear<T>(var Arr: specialize TUArray<T>);

operator + (const v0, v1: TUVec2): TUVec2;
operator - (const v0, v1: TUVec2): TUVec2;
operator * (const v0, v1: TUVec2): TUVec2;
operator / (const v0, v1: TUVec2): TUVec2;
operator * (const v: TUVec2; const f: TUFloat): TUVec2;
operator * (const f: TUFloat; const v: TUVec2): TUVec2;
operator - (const v: TUVec2): TUVec2;
operator + (const v0, v1: TUVec3): TUVec3;
operator - (const v0, v1: TUVec3): TUVec3;
operator * (const v0, v1: TUVec3): TUVec3;
operator / (const v0, v1: TUVec3): TUVec3;
operator * (const v: TUVec3; const f: TUFloat): TUVec3;
operator * (const f: TUFloat; const v: TUVec3): TUVec3;
operator - (const v: TUVec3): TUVec3;
operator + (const v0, v1: TUVec4): TUVec4;
operator - (const v0, v1: TUVec4): TUVec4;
operator * (const v0, v1: TUVec4): TUVec4;
operator / (const v0, v1: TUVec4): TUVec4;
operator * (const v: TUVec4; const f: TUFloat): TUVec4;
operator * (const f: TUFloat; const v: TUVec4): TUVec4;
operator - (const v: TUVec4): TUVec4;
operator + (const m0, m1: TUMat): TUMat;
operator - (const m0, m1: TUMat): TUMat;
operator * (const m0, m1: TUMat): TUMat;
operator * (const m: TUMat; const f: TUFloat): TUMat;
operator mod (const a, b: TUDouble): TUDouble;
operator mod (const a, b: TUFloat): TUFloat;

const tt_any = [tt_error, tt_eof, tt_symbol, tt_word, tt_keyword, tt_string, tt_number];
const UPi = 3.14159265359;
const UTwoPi = UPi * 2;
const UHalfPi = UPi * 0.5;
const URcp255 = 1 / 255;
const UEps = 1E-5;
const UDegToRad = UPi / 180;
const URadToDeg = 180 / UPi;

implementation

// TUFloatImpl begin
function TUFloatImpl.ToString: String;
begin
  System.Str(Self, Result);
end;
// TUFloatImpl end

// TUFloatImpl begin
function TUDpubleImpl.ToString: String;
begin
  System.Str(Self, Result);
end;
// TUFloatImpl end

// TUInt8Impl begin
function TUInt8Impl.GetBit(const Index: TUInt8): TUInt8;
begin
  Result := (Self shr Index) and 1;
end;

procedure TUInt8Impl.SetBit(const Index: TUInt8; const Value: TUInt8);
  var b: TUInt8;
begin
  b := (Value and 1) shl Index;
  Self := (Self and (not b)) or b;
end;

function TUInt8Impl.ToString: String;
begin
  System.Str(Self, Result);
end;

function TUInt8Impl.ToBool: Boolean;
begin
  Result := Self <> 0;
end;
// TUInt8Impl end

// TUInt16Impl begin
function TUInt16Impl.GetBit(const Index: TUInt8): TUInt8;
begin
  Result := (Self shr Index) and 1;
end;

procedure TUInt16Impl.SetBit(const Index: TUInt8; const Value: TUInt8);
  var b: TUInt8;
begin
  b := (Value and 1) shl Index;
  Self := (Self and (not b)) or b;
end;

function TUInt16Impl.ToString: String;
begin
  System.Str(Self, Result);
end;

function TUInt16Impl.ToBool: Boolean;
begin
  Result := Self <> 0;
end;
// TUInt16Impl end

// TUInt32Impl begin
function TUInt32Impl.GetBit(const Index: TUInt8): TUInt8;
begin
  Result := (Self shr Index) and 1;
end;

procedure TUInt32Impl.SetBit(const Index: TUInt8; const Value: TUInt8);
  var b: TUInt8;
begin
  b := (Value and 1) shl Index;
  Self := (Self and (not b)) or b;
end;

function TUInt32Impl.ToString: String;
begin
  System.Str(Self, Result);
end;

function TUInt32Impl.ToBool: Boolean;
begin
  Result := Self <> 0;
end;
// TUInt32Impl end

// TUInt64Impl begin
function TUInt64Impl.GetBit(const Index: TUInt8): TUInt8;
begin
  Result := (Self shr Index) and 1;
end;

procedure TUInt64Impl.SetBit(const Index: TUInt8; const Value: TUInt8);
  var b: TUInt8;
begin
  b := (Value and 1) shl Index;
  Self := (Self and (not b)) or b;
end;

function TUInt64Impl.ToString: String;
begin
  System.Str(Self, Result);
end;

function TUInt64Impl.ToBool: Boolean;
begin
  Result := Self <> 0;
end;
// TUInt64Impl end

// TInt8Impl begin
function TInt8Impl.GetBit(const Index: TUInt8): TUInt8;
begin
  Result := (Self shr Index) and 1;
end;

procedure TInt8Impl.SetBit(const Index: TUInt8; const Value: TUInt8);
  var b: TUInt8;
begin
  b := (Value and 1) shl Index;
  Self := (Self and (not b)) or b;
end;

function TInt8Impl.ToString: String;
begin
  System.Str(Self, Result);
end;

function TInt8Impl.ToBool: Boolean;
begin
  Result := Self <> 0;
end;
// TInt8Impl end

// TInt16Impl begin
function TInt16Impl.GetBit(const Index: TUInt8): TUInt8;
begin
  Result := (Self shr Index) and 1;
end;

procedure TInt16Impl.SetBit(const Index: TUInt8; const Value: TUInt8);
  var b: TUInt8;
begin
  b := (Value and 1) shl Index;
  Self := (Self and (not b)) or b;
end;

function TInt16Impl.ToString: String;
begin
  System.Str(Self, Result);
end;

function TInt16Impl.ToBool: Boolean;
begin
  Result := Self <> 0;
end;
// TInt16Impl end

// TInt32Impl begin
function TInt32Impl.GetBit(const Index: TUInt8): TUInt8;
begin
  Result := (Self shr Index) and 1;
end;

procedure TInt32Impl.SetBit(const Index: TUInt8; const Value: TUInt8);
  var b: TUInt8;
begin
  b := (Value and 1) shl Index;
  Self := (Self and (not b)) or b;
end;

function TInt32Impl.ToString: String;
begin
  System.Str(Self, Result);
end;

function TInt32Impl.ToBool: Boolean;
begin
  Result := Self <> 0;
end;
// TInt32Impl end

// TInt64Impl begin
function TInt64Impl.GetBit(const Index: TUInt8): TUInt8;
begin
  Result := (Self shr Index) and 1;
end;

procedure TInt64Impl.SetBit(const Index: TUInt8; const Value: TUInt8);
  var b: TUInt8;
begin
  b := (Value and 1) shl Index;
  Self := (Self and (not b)) or b;
end;

function TInt64Impl.ToString: String;
begin
  System.Str(Self, Result);
end;

function TInt64Impl.ToBool: Boolean;
begin
  Result := Self <> 0;
end;
// TInt64Impl end

// TUColorImpl begin
function TUColorImpl.GetChannel(const Index: UInt8): UInt8;
  var Arr: array[0..3] of UInt8 absolute Self;
begin
  Result := Arr[Index];
end;

procedure TUColorImpl.SetChannel(const Index: UInt8; const Value: UInt8);
  var Arr: array[0..3] of UInt8 absolute Self;
begin
  Arr[Index] := Value;
end;

function TUColorImpl.GetR: UInt8;
begin
  Result := Self.Channel[0];
end;

procedure TUColorImpl.SetR(const Value: UInt8);
begin
  Channel[0] := Value;
end;

function TUColorImpl.GetG: UInt8;
begin
  Result := Channel[1];
end;

procedure TUColorImpl.SetG(const Value: UInt8);
begin
  Channel[1] := Value;
end;

function TUColorImpl.GetB: UInt8;
begin
  Result := Channel[2];
end;

procedure TUColorImpl.SetB(const Value: UInt8);
begin
  Channel[2] := Value;
end;

function TUColorImpl.GetA: UInt8;
begin
  Result := Channel[3];
end;

procedure TUColorImpl.SetA(const Value: UInt8);
begin
  Channel[3] := Value;
end;

class function TUColorImpl.Make(const Ar, Ag, Ab, Aa: UInt8): TUColor;
begin
  Result := Ar or (Ag shl 8) or (Ab shl 16) or (Aa shl 24);
end;

class function TUColorImpl.Black: TUColor;
begin
  Result := $ff000000;
end;

class function TUColorImpl.White: TUColor;
begin
  Result := $ffffffff;
end;

class function TUColorImpl.Red: TUColor;
begin
  Result := $ff0000ff;
end;

class function TUColorImpl.Blue: TUColor;
begin
  Result := $ffff0000;
end;

class function TUColorImpl.Green: TUColor;
begin
  Result := $ff00ff00;
end;

procedure TUColorImpl.SetValue(const Ar, Ag, Ab, Aa: UInt8);
begin
  Self := Ab or (Ag shl 8) or (Ar shl 16) or (Aa shl 24);
end;

function TUColorImpl.ToString: String;
begin
  Result := '#' + IntToHex(r, 2) + IntToHex(g, 2) + IntToHex(b, 2) + IntToHex(a, 2);
end;
// TUColorImpl end

// TUMatImpl begin
function TUMatImpl.GetElement(const Index: UInt32): TUFloat;
begin
  Result := Self[Index shr 2, Index mod 4];
end;

procedure TUMatImpl.SetElement(const Index: UInt32; const Value: TUFloat);
begin
  Self[Index shr 2, Index mod 4] := Value;
end;

function TUMatImpl.GetAxisX: TUVec3;
begin
  Result := PUVec3(@Self[0, 0])^;
end;

procedure TUMatImpl.SetAxisX(const Value: TUVec3);
begin
  PUVec3(@Self[0, 0])^ := Value;
end;

function TUMatImpl.GetAxisY: TUVec3;
begin
  Result := PUVec3(@Self[1, 0])^;
end;

procedure TUMatImpl.SetAxisY(const Value: TUVec3);
begin
  PUVec3(@Self[1, 0])^ := Value;
end;

function TUMatImpl.GetAxisZ: TUVec3;
begin
  Result := PUVec3(@Self[2, 0])^;
end;

procedure TUMatImpl.SetAxisZ(const Value: TUVec3);
begin
  PUVec3(@Self[2, 0])^ := Value;
end;

function TUMatImpl.GetPosition: TUVec3;
begin
  Result := PUVec3(@Self[3, 0])^;
end;

procedure TUMatImpl.SetPosition(const Value: TUVec3);
begin
  PUVec3(@Self[3, 0])^ := Value;
end;

class function TUMatImpl.Make(
  const e00, e10, e20, e30: TUFloat;
  const e01, e11, e21, e31: TUFloat;
  const e02, e12, e22, e32: TUFloat;
  const e03, e13, e23, e33: TUFloat
): TUMat;
begin
  Result[0, 0] := e00; Result[1, 0] := e10; Result[2, 0] := e20; Result[3, 0] := e30;
  Result[0, 1] := e01; Result[1, 1] := e11; Result[2, 1] := e21; Result[3, 1] := e31;
  Result[0, 2] := e02; Result[1, 2] := e12; Result[2, 2] := e22; Result[3, 2] := e32;
  Result[0, 3] := e03; Result[1, 3] := e13; Result[2, 3] := e23; Result[3, 3] := e33;
end;


class function TUMatImpl.Zero: TUMat;
begin
  Result := Make(
    0, 0, 0, 0,
    0, 0, 0, 0,
    0, 0, 0, 0,
    0, 0, 0, 0
  );
end;

class function TUMatImpl.Identity: TUMat;
begin
  Result := Make(
    1, 0, 0, 0,
    0, 1, 0, 0,
    0, 0, 1, 0,
    0, 0, 0, 1
  );
end;

class function TUMatImpl.Scaling(const x, y, z: TUFloat): TUMat;
begin
  Result := Make(
    x, 0, 0, 0,
    0, y, 0, 0,
    0, 0, z, 0,
    0, 0, 0, 1
  );
end;

class function TUMatImpl.Scaling(const v: TUVec3): TUMat;
begin
  Result := Make(
    v.x, 0, 0, 0,
    0, v.y, 0, 0,
    0, 0, v.z, 0,
    0, 0, 0, 1
  );
end;

class function TUMatImpl.Scaling(const s: TUFloat): TUMat;
begin
  Result := Make(
    s, 0, 0, 0,
    0, s, 0, 0,
    0, 0, s, 0,
    0, 0, 0, 1
  );
end;

class function TUMatImpl.Translation(const x, y, z: TUFloat): TUMat;
begin
  Result := Make(
    1, 0, 0, x,
    0, 1, 0, y,
    0, 0, 1, z,
    0, 0, 0, 1
  );
end;

class function TUMatImpl.Translation(const v: TUVec3): TUMat;
begin
  Result := Make(
    1, 0, 0, v.x,
    0, 1, 0, v.y,
    0, 0, 1, v.z,
    0, 0, 0, 1
  );
end;

class function TUMatImpl.RotationX(const a: TUFloat): TUMat;
  var s, c: TUFloat;
begin
  s := Sin(a);
  c := Cos(a);
  Result := Make(
    1, 0, 0, 0,
    0, c, -s, 0,
    0, s, c, 0,
    0, 0, 0, 1
  );
end;

class function TUMatImpl.RotationY(const a: TUFloat): TUMat;
  var s, c: TUFloat;
begin
  s := Sin(a);
  c := Cos(a);
  Result := Make(
    c, 0, s, 0,
    0, 1, 0, 0,
    -s, 0, c, 0,
    0, 0, 0, 1
  );
end;

class function TUMatImpl.RotationZ(const a: TUFloat): TUMat;
  var s, c: TUFloat;
begin
  s := Sin(a);
  c := Cos(a);
  Result := Make(
    c, -s, 0, 0,
    s, c, 0, 0,
    0, 0, 1, 0,
    0, 0, 0, 1
  );
end;

class function TUMatImpl.Rotation(const x, y, z, a: TUFloat): TUMat;
begin
  Result := Rotation(TUVec3.Make(x, y, z), a);
end;

class function TUMatImpl.Rotation(const v: TUVec3; const a: TUFloat): TUMat;
  var vr: TUVec3;
  var s, c, cr, xs, ys, zs, crxy, crxz, cryz: TUFloat;
begin
  vr := v.Norm;
  s := Sin(a);
  c := Cos(a);
  cr := 1 - c;
  xs := vr.x * s;
  ys := vr.y * s;
  zs := vr.z * s;
  crxy := cr * vr.x * vr.y;
  crxz := cr * vr.x * vr.z;
  cryz := cr * vr.y * vr.z;
  Result := Make(
    cr * v.x * v.x + c, -zs + crxy, ys + crxz, 0,
    zs + crxy, cr * v.y * v.y + c, -xs + cryz, 0,
    -ys + crxz, xs + cryz, cr * v.z * v.z + c, 0,
    0, 0, 0, 1
  );
end;

class function TUMatImpl.Rotation(const q: TUQuat): TUMat;
  var xx, yy, zz, xy, xz, yz, wx, wy, wz: TUFloat;
begin
  xx := 2 * q.x * q.x;
  yy := 2 * q.y * q.y;
  zz := 2 * q.z * q.z;
  xy := 2 * q.x * q.y;
  xz := 2 * q.x * q.z;
  yz := 2 * q.y * q.z;
  wx := 2 * q.w * q.x;
  wy := 2 * q.w * q.y;
  wz := 2 * q.w * q.z;
  Result := Make(
    1 - yy - zz, xy - wz, xz + wy, 0,
    xy + wz, 1 - xx - zz, yz - wx, 0,
    xz - wy, yz + wx, 1 - xx - yy, 0,
    0, 0, 0, 1
  );
end;

class function TUMatImpl.View(const Origin, Target, Up: TUVec3): TUMat;
  var vx, vy, vz: TUVec3;
begin
  vz := (Target - Origin).Norm;
  vx := up.Cross(vz).Norm;
  vy := vz.Cross(vx).Norm;
  Result := Make(
    vx.x, vx.y, vx.z, -vx.Dot(Origin),
    vy.x, vy.y, vy.z, -vy.Dot(Origin),
    vz.x, vz.y, vz.z, -vz.Dot(Origin),
    0, 0, 0, 1
  );
end;

class function TUMatImpl.Proj(const FoV, Aspect, ZNear, ZFar: TUFloat): TUMat;
  var ct, q: TUFloat;
begin
  ct := UCoTan(FoV * 0.5);
  q := ZFar / (ZFar - ZNear);
  Result := Make(
    ct / Aspect, 0, 0, 0,
    0, ct, 0, 0,
    0, 0, q, -q * ZNear,
    0, 0, 1, 0
  );
end;

class function TUMatImpl.Orth(const Width, Height, ZNear, ZFar: TUFloat): TUMat;
  var RcpD: TUFloat;
begin
  RcpD := 1 / (ZFar - ZNear);
  Result := Make(
    2 / Width, 0, 0, 0,
    0, 2 / Height, 0, 0,
    0, 0, RcpD, -ZNear * RcpD,
    0, 0, 0, 1
  );
end;

class function TUMatImpl.Skew(const Amount, Axis: TUVec3; const Angle: TUFloat): TUMat;
  var vr: TUVec3;
  var s, c, cr, xs, ys, zs, crxy, crxz, cryz: TUFloat;
begin
  vr := Axis.Norm;
  USinCos(Angle, s, c);
  cr := 1 - c;
  xs := vr.x * s;
  ys := vr.y * s;
  zs := vr.z * s;
  crxy := cr * vr.x * vr.y;
  crxz := cr * vr.x * vr.z;
  cryz := cr * vr.y * vr.z;
  Result := TUMat.Make(
    ULerp(1, cr * Axis.x * Axis.x + c, Amount.x), ULerp(1, -zs + crxy, Amount.y), ULerp(1, ys + crxz, Amount.z), 0,
    ULerp(1, zs + crxy, Amount.x), ULerp(1, cr * Axis.y * Axis.y + c, Amount.y), ULerp(1, -xs + cryz, Amount.z), 0,
    ULerp(1, -ys + crxz, Amount.x), ULerp(1, xs + cryz, Amount.y), ULerp(1, cr * Axis.z * Axis.z + c, Amount.z), 0,
    0, 0, 0, 1
  );
end;

class function TUMatImpl.Inverse(const m: TUMat): TUMat;
  var Det: TUFloat;
begin
  Det := (m[0, 0] * m[1, 1] * m[2, 2] * m[3, 3]) + (m[0, 0] * m[1, 2] * m[2, 3] * m[3, 1]) + (m[0, 0] * m[1, 3] * m[2, 1] * m[3, 2])
  - (m[0, 0] * m[1, 3] * m[2, 2] * m[3, 1]) - (m[0, 0] * m[1, 2] * m[2, 1] * m[3, 3]) - (m[0, 0] * m[1, 1] * m[2, 3] * m[3, 2])
  - (m[0, 1] * m[1, 0] * m[2, 2] * m[3, 3]) - (m[0, 2] * m[1, 0] * m[2, 3] * m[3, 1]) - (m[0, 3] * m[1, 0] * m[2, 1] * m[3, 2])
  + (m[0, 3] * m[1, 0] * m[2, 2] * m[3, 1]) + (m[0, 2] * m[1, 0] * m[2, 1] * m[3, 3]) + (m[0, 1] * m[1, 0] * m[2, 3] * m[3, 2])
  + (m[0, 1] * m[1, 2] * m[2, 0] * m[3, 3]) + (m[0, 2] * m[1, 3] * m[2, 0] * m[3, 1]) + (m[0, 3] * m[1, 1] * m[2, 0] * m[3, 2])
  - (m[0, 3] * m[1, 2] * m[2, 0] * m[3, 1]) - (m[0, 2] * m[1, 1] * m[2, 0] * m[3, 3]) - (m[0, 1] * m[1, 3] * m[2, 0] * m[3, 2])
  - (m[0, 1] * m[1, 2] * m[2, 3] * m[3, 0]) - (m[0, 2] * m[1, 3] * m[2, 1] * m[3, 0]) - (m[0, 3] * m[1, 1] * m[2, 2] * m[3, 0])
  + (m[0, 3] * m[1, 2] * m[2, 1] * m[3, 0]) + (m[0, 2] * m[1, 1] * m[2, 3] * m[3, 0]) + (m[0, 1] * m[1, 3] * m[2, 2] * m[3, 0]);
  Det := 1 / Det;
  Result[0, 0] := (m[1,1]*m[2,2]*m[3,3] + m[1,2]*m[2,3]*m[3,1] + m[1,3]*m[2,1]*m[3,2] - m[1,3]*m[2,2]*m[3,1] - m[1,2]*m[2,1]*m[3,3] - m[1,1]*m[2,3]*m[3,2]) * det;
  Result[0, 1] := (-m[0,1]*m[2,2]*m[3,3] - m[0,2]*m[2,3]*m[3,1] - m[0,3]*m[2,1]*m[3,2] + m[0,3]*m[2,2]*m[3,1] + m[0,2]*m[2,1]*m[3,3] + m[0,1]*m[2,3]*m[3,2]) * det;
  Result[0, 2] := (m[0,1]*m[1,2]*m[3,3] + m[0,2]*m[1,3]*m[3,1] + m[0,3]*m[1,1]*m[3,2] - m[0,3]*m[1,2]*m[3,1] - m[0,2]*m[1,1]*m[3,3] - m[0,1]*m[1,3]*m[3,2]) * det;
  Result[0, 3] := (-m[0,1]*m[1,2]*m[2,3] - m[0,2]*m[1,3]*m[2,1] - m[0,3]*m[1,1]*m[2,2] + m[0,3]*m[1,2]*m[2,1] + m[0,2]*m[1,1]*m[2,3] + m[0,1]*m[1,3]*m[2,2]) * det;
  Result[1, 0] := (-m[1,0]*m[2,2]*m[3,3] - m[1,2]*m[2,3]*m[3,0] - m[1,3]*m[2,0]*m[3,2] + m[1,3]*m[2,2]*m[3,0] + m[1,2]*m[2,0]*m[3,3] + m[1,0]*m[2,3]*m[3,2]) * det;
  Result[1, 1] := (m[0,0]*m[2,2]*m[3,3] + m[0,2]*m[2,3]*m[3,0] + m[0,3]*m[2,0]*m[3,2] - m[0,3]*m[2,2]*m[3,0] - m[0,2]*m[2,0]*m[3,3] - m[0,0]*m[2,3]*m[3,2]) * det;
  Result[1, 2] := (-m[0,0]*m[1,2]*m[3,3] - m[0,2]*m[1,3]*m[3,0] - m[0,3]*m[1,0]*m[3,2] + m[0,3]*m[1,2]*m[3,0] + m[0,2]*m[1,0]*m[3,3] + m[0,0]*m[1,3]*m[3,2]) * det;
  Result[1, 3] := (m[0,0]*m[1,2]*m[2,3] + m[0,2]*m[1,3]*m[2,0] + m[0,3]*m[1,0]*m[2,2] - m[0,3]*m[1,2]*m[2,0] - m[0,2]*m[1,0]*m[2,3] - m[0,0]*m[1,3]*m[2,2]) * det;
  Result[2, 0] := (m[1,0]*m[2,1]*m[3,3] + m[1,1]*m[2,3]*m[3,0] + m[1,3]*m[2,0]*m[3,1] - m[1,3]*m[2,1]*m[3,0] - m[1,1]*m[2,0]*m[3,3] - m[1,0]*m[2,3]*m[3,1]) * det;
  Result[2, 1] := (-m[0,0]*m[2,1]*m[3,3] - m[0,1]*m[2,3]*m[3,0] - m[0,3]*m[2,0]*m[3,1] + m[0,3]*m[2,1]*m[3,0] + m[0,1]*m[2,0]*m[3,3] + m[0,0]*m[2,3]*m[3,1]) * det;
  Result[2, 2] := (m[0,0]*m[1,1]*m[3,3] + m[0,1]*m[1,3]*m[3,0] + m[0,3]*m[1,0]*m[3,1] - m[0,3]*m[1,1]*m[3,0] - m[0,1]*m[1,0]*m[3,3] - m[0,0]*m[1,3]*m[3,1]) * det;
  Result[2, 3] := (-m[0,0]*m[1,1]*m[2,3] - m[0,1]*m[1,3]*m[2,0] - m[0,3]*m[1,0]*m[2,1] + m[0,3]*m[1,1]*m[2,0] + m[0,1]*m[1,0]*m[2,3] + m[0,0]*m[1,3]*m[2,1]) * det;
  Result[3, 0] := (-m[1,0]*m[2,1]*m[3,2] - m[1,1]*m[2,2]*m[3,0] - m[1,2]*m[2,0]*m[3,1] + m[1,2]*m[2,1]*m[3,0] + m[1,1]*m[2,0]*m[3,2] + m[1,0]*m[2,2]*m[3,1]) * det;
  Result[3, 1] := (m[0,0]*m[2,1]*m[3,2] + m[0,1]*m[2,2]*m[3,0] + m[0,2]*m[2,0]*m[3,1] - m[0,2]*m[2,1]*m[3,0] - m[0,1]*m[2,0]*m[3,2] - m[0,0]*m[2,2]*m[3,1]) * det;
  Result[3, 2] := (-m[0,0]*m[1,1]*m[3,2] - m[0,1]*m[1,2]*m[3,0] - m[0,2]*m[1,0]*m[3,1] + m[0,2]*m[1,1]*m[3,0] + m[0,1]*m[1,0]*m[3,2] + m[0,0]*m[1,2]*m[3,1]) * det;
  Result[3, 3] := (m[0,0]*m[1,1]*m[2,2] + m[0,1]*m[1,2]*m[2,0] + m[0,2]*m[1,0]*m[2,1] - m[0,2]*m[1,1]*m[2,0] - m[0,1]*m[1,0]*m[2,2] - m[0,0]*m[1,2]*m[2,1]) * det;
end;

class function TUMatImpl.Transpose(const m: TUMat): TUMat;
  var x, y: Int32;
begin
  for x := 0 to 3 do
  for y := 0 to 3 do
  begin
    Result[x, y] := m[y, x];
  end;
end;

class function TUMatImpl.Normalize(const m: TUMat): TUMat;
begin
  Result := m;
  Result.AxisX := m.AxisX.Norm;
  Result.AxisY := m.AxisY.Norm;
  Result.AxisZ := m.AxisZ.Norm;
end;

procedure TUMatImpl.SetValue(
  const e00, e10, e20, e30: TUFloat;
  const e01, e11, e21, e31: TUFloat;
  const e02, e12, e22, e32: TUFloat;
  const e03, e13, e23, e33: TUFloat
);
begin
  Self[0, 0] := e00; Self[1, 0] := e10; Self[2, 0] := e20; Self[3, 0] := e30;
  Self[0, 1] := e01; Self[1, 1] := e11; Self[2, 1] := e21; Self[3, 1] := e31;
  Self[0, 2] := e02; Self[1, 2] := e12; Self[2, 2] := e22; Self[3, 2] := e32;
  Self[0, 3] := e03; Self[1, 3] := e13; Self[2, 3] := e23; Self[3, 3] := e33;
end;

function TUMatImpl.Inverse: TUMat;
begin
  Result := Inverse(Self);
end;

function TUMatImpl.Transpose: TUMat;
begin
  Result := Transpose(Self);
end;

function TUMatImpl.Normalize: TUMat;
begin
  Result := TUMat.Normalize(Self);
end;

function TUMatImpl.ToString: String;
begin
  Result := Format(
    '{'#$A +
    '  %0:0.2f, %1:0.2f, %2:0.2f, %3:0.2f'#$A +
    '  %4:0.2f, %5:0.2f, %6:0.2f, %7:0.2f'#$A +
    '  %8:0.2f, %9:0.2f, %10:0.2f, %11:0.2f'#$A +
    '  %12:0.2f, %13:0.2f, %14:0.2f, %15:0.2f'#$A +
    '}',
    [
      Self[0, 0], Self[1, 0], Self[2, 0], Self[3, 0],
      Self[0, 1], Self[1, 1], Self[2, 1], Self[3, 1],
      Self[0, 2], Self[1, 2], Self[2, 2], Self[3, 2],
      Self[0, 3], Self[1, 3], Self[2, 3], Self[3, 3]
    ]
  );
end;
// TUMatImpl end

// TUVec2Impl begin
function TUVec2Impl.GetX: TUFloat;
begin
  Result := Self[0];
end;

procedure TUVec2Impl.SetX(const Value: TUFloat);
begin
  Self[0] := Value;
end;

function TUVec2Impl.GetY: TUFloat;
begin
  Result := Self[1];
end;

procedure TUVec2Impl.SetY(const Value: TUFloat);
begin
  Self[1] := Value;
end;

class function TUVec2Impl.Zero: TUVec2;
begin
  Result := Make(0);
end;

class function TUVec2Impl.Make(const Ax, Ay: TUFloat): TUVec2;
begin
  Result[0] := Ax;
  Result[1] := Ay;
end;

class function TUVec2Impl.Make(const S: TUFloat): TUVec2;
begin
  Result := Make(s, s);
end;

class function TUVec2Impl.Dot(const v0, v1: TUVec2): TUFloat;
begin
  Result := v0[0] * v1[0] + v0[1] * v1[1];
end;

class function TUVec2Impl.Cross(const v0, v1: TUVec2): TUFloat;
begin
  Result := v0[0] * v1[1] - v0[1] * v1[0];
end;

class function TUVec2Impl.Norm(const v: TUVec2): TUVec2;
  var d: TUFloat;
begin
  d := Sqrt(Dot(v, v));
  if d > 0 then
  begin
    d := 1 / d;
    Result := Make(v.x * d, v.y * d);
  end
  else
  begin
    Result := Zero;
  end;
end;

procedure TUVec2Impl.SetValue(const Ax, Ay: TUFloat);
begin
  Self[0] := Ax;
  Self[1] := Ay;
end;

function TUVec2Impl.Transform(const r: TURot2): TUVec2;
begin
  Result := r.Transform(Self);
end;

function TUVec2Impl.TransformInv(const r: TURot2): TUVec2;
begin
  Result := r.TransformInv(Self);
end;

function TUVec2Impl.Dot(const v: TUVec2): TUFloat;
begin
  Result := Dot(Self, v);
end;

function TUVec2Impl.Cross(const v: TUVec2): TUFloat;
begin
  Result := Cross(Self, v);
end;

function TUVec2Impl.Norm: TUVec2;
begin
  Result := Norm(Self);
end;

function TUVec2Impl.IsZero: Boolean;
begin
  Result := (x = 0) and (y = 0);
end;

function TUVec2Impl.ToString: String;
begin
  Result := Format('{%0:0.2f, %1:0.2f}', [x, y]);
end;
// TUVec2Impl end

// TUVec3Impl begin
function TUVec3Impl.GetX: TUFloat;
begin
  Result := Self[0];
end;

procedure TUVec3Impl.SetX(const Value: TUFloat);
begin
  Self[0] := Value;
end;

function TUVec3Impl.GetY: TUFloat;
begin
  Result := Self[1];
end;

procedure TUVec3Impl.SetY(const Value: TUFloat);
begin
  Self[1] := Value;
end;

function TUVec3Impl.GetZ: TUFloat;
begin
  Result := Self[2];
end;

procedure TUVec3Impl.SetZ(const Value: TUFloat);
begin
  Self[2] := Value;
end;

class function TUVec3Impl.Zero: TUVec3;
begin
  Result := Make(0);
end;

class function TUVec3Impl.Make(const Ax, Ay, Az: TUFloat): TUVec3;
begin
  Result[0] := Ax;
  Result[1] := Ay;
  Result[2] := Az;
end;

class function TUVec3Impl.Make(const v2: TUVec2; const Az: TUFloat): TUVec3;
begin
  Result := Make(v2[0], v2[1], Az);
end;

class function TUVec3Impl.Make(const s: TUFloat): TUVec3;
begin
  Result := Make(s, s, s);
end;

class function TUVec3Impl.Len(const v: TUVec3): TUFloat;
begin
  Result := Sqrt(Dot(v, v));
end;

class function TUVec3Impl.LenSq(const v: TUVec3): TUFloat;
begin
  Result := Dot(v, v)
end;

class function TUVec3Impl.Dot(const v0, v1: TUVec3): TUFloat;
begin
  Result := v0[0] * v1[0] + v0[1] * v1[1] + v0[2] * v1[2];
end;

class function TUVec3Impl.Cross(const v0, v1: TUVec3): TUVec3;
begin
  Result[0] := v0[1] * v1[2] - v0[2] * v1[1];
  Result[1] := v0[2] * v1[0] - v0[0] * v1[2];
  Result[2] := v0[0] * v1[1] - v0[1] * v1[0];
end;

class function TUVec3Impl.Norm(const v: TUVec3): TUVec3;
  var d: TUFloat;
begin
  d := Sqrt(Dot(v, v));
  if d > 0 then
  begin
    d := 1 / d;
    Result := Make(v.x * d, v.y * d, v.z * d);
  end
  else
  begin
    Result := Zero;
  end;
end;

procedure TUVec3Impl.SetValue(const Ax, Ay, Az: TUFloat);
begin
  Self[0] := Ax;
  Self[1] := Ay;
  Self[2] := Az;
end;

function TUVec3Impl.Transform3x3(const m: TUMat): TUVec3;
begin
  Result := UMulVec3Mat3x3(Self, m);
end;

function TUVec3Impl.Transform4x3(const m: TUMat): TUVec3;
begin
  Result := UMulVec3Mat4x3(Self, m);
end;

function TUVec3Impl.Transform4x4(const m: TUMat): TUVec3;
begin
  Result := UMulVec3Mat4x4(Self, m);
end;

function TUVec3Impl.TransformQuat(const q: TUQuat): TUVec3;
begin
  Result := UMulVec3Quat(Self, q);
end;

function TUVec3Impl.Len: TUFloat;
begin
  Result := Len(Self);
end;

function TUVec3Impl.LenSq: TUFloat;
begin
  Result := LenSq(Self);
end;

function TUVec3Impl.Dot(const v: TUVec3): TUFloat;
begin
  Result := Dot(Self, v);
end;

function TUVec3Impl.Cross(const v: TUVec3): TUVec3;
begin
  Result := Cross(Self, v);
end;

function TUVec3Impl.Norm: TUVec3;
begin
  Result := Norm(Self);
end;

function TUVec3Impl.xy: TUVec2;
begin
  Result[0] := x;
  Result[1] := y;
end;

function TUVec3Impl.AngleTo(const v: TUVec3): TUFloat;
  var VecLen: TUFloat;
begin
  VecLen := Len * v.Len;
  if VecLen > 0 then Result := UArcCos(Dot(v) / VecLen) else Result := 0;
end;

function TUVec3Impl.RotationTo(const v: TUVec3): TUQuat;
  var q: TUQuat;
  var cp: TUVec3;
begin
  cp := Cross(v);
  q := TUQuat.Make(
    cp.x, cp.y, cp.z,
    Sqrt((LenSq) * (v.LenSq)) + Dot(v)
  );
  Result := q.Norm;
end;

function TUVec3Impl.IsZero: Boolean;
begin
  Result := (x = 0) and (y = 0) and (z = 0);
end;

function TUVec3Impl.ToString: String;
begin
  Result := Format('{%0:0.2f, %1:0.2f, %2:0.2f}', [x, y, z]);
end;
// TUVec3Impl end

// TUVec4Impl begin
function TUVec4Impl.GetX: TUFloat;
begin
  Result := Self[0];
end;

procedure TUVec4Impl.SetX(const Value: TUFloat);
begin
  Self[0] := Value;
end;

function TUVec4Impl.GetY: TUFloat;
begin
  Result := Self[1];
end;

procedure TUVec4Impl.SetY(const Value: TUFloat);
begin
  Self[1] := Value;
end;

function TUVec4Impl.GetZ: TUFloat;
begin
  Result := Self[2];
end;

procedure TUVec4Impl.SetZ(const Value: TUFloat);
begin
  Self[2] := Value;
end;

function TUVec4Impl.GetW: TUFloat;
begin
  Result := Self[3];
end;

procedure TUVec4Impl.SetW(const Value: TUFloat);
begin
  Self[3] := Value;
end;

class function TUVec4Impl.Zero: TUVec4;
begin
  Result := Make(0);
end;

class function TUVec4Impl.Make(const Ax, Ay, Az, Aw: TUFloat): TUVec4;
begin
  Result[0] := Ax;
  Result[1] := Ay;
  Result[2] := Az;
  Result[3] := Aw;
end;

class function TUVec4Impl.Make(const v: TUVec3; const Aw: TUFloat): TUVec4;
begin
  Result := Make(v.x, v.y, v.z, Aw);
end;

class function TUVec4Impl.Make(const v0, v1: TUVec2): TUVec4;
begin
  Result := Make(v0.x, v0.y, v1.x, v1.y);
end;

class function TUVec4Impl.Make(const v: TUVec2; const Az, Aw: TUFloat): TUVec4;
begin
  Result := Make(v.x, v.y, Az, Aw);
end;

class function TUVec4Impl.Make(const s: TUFloat): TUVec4;
begin
  Result := Make(s, s, s, s);
end;

class function TUVec4Impl.Dot(const v0, v1: TUVec4): TUFloat;
begin
  Result := v0[0] * v1[0] + v0[1] * v1[1] + v0[2] * v1[2] + v0[3] * v1[3];
end;

class function TUVec4Impl.Norm(const v: TUVec4): TUVec4;
  var d: TUFloat;
begin
  d := Sqrt(Dot(v, v));
  if d > 0 then
  begin
    d := 1 / d;
    Result := Make(v.x * d, v.y * d, v.z * d, v.w * d);
  end
  else
  begin
    Result := Zero;
  end;
end;

procedure TUVec4Impl.SetValue(const Ax, Ay, Az, Aw: TUFloat);
begin
  Self[0] := Ax;
  Self[1] := Ay;
  Self[2] := Az;
  Self[3] := Aw;
end;

function TUVec4Impl.Dot(const v: TUVec4): TUFloat;
begin
  Result := Dot(Self, v);
end;

function TUVec4Impl.Norm: TUVec4;
begin
  Result := Norm(Self);
end;

function TUVec4Impl.xyz: TUVec3;
begin
  Result[0] := x;
  Result[1] := y;
  Result[2] := z;
end;

function TUVec4Impl.xy: TUVec2;
begin
  Result[0] := x;
  Result[1] := y;
end;

function TUVec4Impl.IsZero: Boolean;
begin
  Result := (x = 0) and (y = 0) and (z = 0) and (w = 0);
end;

function TUVec4Impl.ToString: String;
begin
  Result := Format('{%0:0.2f, %1:0.2f, %2:0.2f, %3:0.2f}', [x, y, z, w]);
end;
// TUVec4Impl end

// TUQuatImpl begin
function TUQuatImpl.GetX: TUFloat;
begin
  Result := Self[0];
end;

procedure TUQuatImpl.SetX(const Value: TUFloat);
begin
  Self[0] := Value;
end;

function TUQuatImpl.GetY: TUFloat;
begin
  Result := Self[1];
end;

procedure TUQuatImpl.SetY(const Value: TUFloat);
begin
  Self[1] := Value;
end;

function TUQuatImpl.GetZ: TUFloat;
begin
  Result := Self[2];
end;

procedure TUQuatImpl.SetZ(const Value: TUFloat);
begin
  Self[2] := Value;
end;

function TUQuatImpl.GetW: TUFloat;
begin
  Result := Self[3];
end;

procedure TUQuatImpl.SetW(const Value: TUFloat);
begin
  Self[3] := Value;
end;

class function TUQuatImpl.Identity: TUQuat;
begin
  Result[0] := 0;
  Result[1] := 0;
  Result[2] := 0;
  Result[3] := 1;
end;

class function TUQuatImpl.Make(const Ax, Ay, Az, Aw: TUFloat): TUQuat;
begin
  Result[0] := Ax;
  Result[1] := Ay;
  Result[2] := Az;
  Result[3] := Aw;
end;

class function TUQuatImpl.Norm(const v: TUQuat): TUQuat;
  var d: TUFloat;
begin
  d := Sqrt(TUVec4.Dot(PUVec4(@v)^, PUVec4(@v)^));
  if d > 0 then
  begin
    d := 1 / d;
    Result := Make(v.x * d, v.y * d, v.z * d, v.w * d);
  end
  else
  begin
    Result := Identity;
  end;
end;

function TUQuatImpl.Norm: TUQuat;
begin
  Result := Norm(Self);
end;

function TUQuatImpl.ToString: String;
begin
  Result := Format('{%0:0.2f, %1:0.2f, %2:0.2f, %3:0.2f}', [x, y, z, w]);
end;
// TUQuatImpl end

// TUSwizzle begin
function TUSwizzle.GetOffset(const Index: UInt8): UInt8;
begin
  Result := (_Remap shr (Index * 2)) and 3;
end;

procedure TUSwizzle.SetOffset(const Index: UInt8; const Value: UInt8);
  var i: UInt8;
begin
  i := Index * 2;
  _Remap := (_Remap and (not (3 shl i))) or (Value shl i);
end;

class function TUSwizzle.Make(
  const ord0: UInt8; const ord1: UInt8;
  const ord2: UInt8; const ord3: UInt8
): TUSwizzle;
begin
  {$push}
  {$warnings off}
  Result.SetValue(ord0, ord1, ord2, ord3);
  {$pop}
end;

procedure TUSwizzle.SetIdentity;
begin
  _Remap := DefaultSwizzle;
end;

procedure TUSwizzle.SetValue(
  const ord0: UInt8; const ord1: UInt8;
  const ord2: UInt8; const ord3: UInt8
);
begin
  _Remap := ord0 or (ord1 shl 2) or (ord2 shl 4) or (ord3 shl 6);
end;
// TUSwizzle end

// TURot2 begin
function TURot2Impl.GetAngSin: TUFloat;
begin
  Result := Self[1];
end;

function TURot2Impl.GetAngCos: TUFloat;
begin
  Result := Self[0];
end;

function TURot2Impl.GetAngle: TUFloat;
begin
  Result := UArcTan2(Self[1], Self[0]);
end;

function TURot2Impl.GetAxisX: TUVec2;
begin
  Result := TUVec2.Make(Self[0], Self[1]);
end;

function TURot2Impl.GetAxisY: TUVec2;
begin
  Result := TUVec2.Make(-Self[1], Self[0]);
end;

procedure TURot2Impl.SetAngSin(const Value: TUFloat);
begin
  Self[0] := Value;
end;

procedure TURot2Impl.SetAngCos(const Value: TUFloat);
begin
  Self[1] := Value;
end;

procedure TURot2Impl.SetAngle(const Angle: TUFloat);
begin
  USinCos(Angle, Self[1], Self[0]);
end;

procedure TURot2Impl.SetAxisX(const Value: TUVec2);
begin
  Self[0] := Value.x;
  Self[1] := Value.y;
end;

procedure TURot2Impl.SetAxisY(const Value: TUVec2);
begin
  Self[0] := Value.y;
  Self[1] := -Value.x;
end;

class function TURot2Impl.Identity: TURot2;
begin
  Result := TURot2.Make(1, 0);
end;

class function TURot2Impl.Make(const Angle: TUFloat): TURot2;
begin
  USinCos(Angle, Result[1], Result[0]);
end;

class function TURot2Impl.Make(const Rc, Rs: TUFloat): TURot2;
begin
  Result[0] := Rc;
  Result[1] := Rs;
end;

class function TURot2Impl.Norm(const Value: TURot2): TURot2;
  var ValVec2: TUVec2 absolute Value;
  var ResVec2: TUVec2 absolute Result;
begin
  ResVec2 := ValVec2.Norm;
end;

procedure TURot2Impl.SetValue(const Rc, Rs: TUFloat);
begin
  Self[0] := Rc;
  Self[1] := Rs;
end;

function TURot2Impl.Norm: TURot2;
begin
  Result := Norm(Self);
end;

function TURot2Impl.Transform(const v: TUVec2): TUVec2;
begin
  Result := TUVec2.Make(
    v.x * AngCos - v.y * AngSin,
    v.x * AngSin + v.y * AngCos
  );
end;

function TURot2Impl.TransformInv(const v: TUVec2): TUVec2;
begin
  Result := TUVec2.Make(
    v.x * AngCos + v.y * AngSin,
    v.y * AngCos - v.x * AngSin
  );
end;

function TURot2Impl.ToString: String;
begin
  Result := Format('{%0:0.2f}', [Angle * URadToDeg]);
end;
// TURot2 end

// TUCriticalSection begin
procedure TUCriticalSection.Initialize;
begin
  InitCriticalSection(_cs);
end;

procedure TUCriticalSection.Finalize;
begin
  DoneCriticalSection(_cs);
end;

procedure TUCriticalSection.Enter;
begin
  EnterCriticalSection(_cs);
end;

function TUCriticalSection.TryEnter: Boolean;
begin
  Result := TryEnterCriticalsection(_cs) > 0;
end;

procedure TUCriticalSection.Leave;
begin
  LeaveCriticalSection(_cs);
end;

class operator TUCriticalSection.Initialize(var v: TUCriticalSection);
begin
  v.Initialize;
end;

class operator TUCriticalSection.Finalize(var v: TUCriticalSection);
begin
  v.Finalize;
end;
// TUCriticalSection end

// TUAtomicLock begin
procedure TUAtomicLock.Initialize;
begin
  _Lock := 0;
end;

procedure TUAtomicLock.Finalize;
begin
end;

function TUAtomicLock.TryLock: Boolean;
begin
  Result := InterlockedCompareExchange(_Lock, 1, 0) = 0;
end;

procedure TUAtomicLock.Lock;
begin
  while not TryLock do;
end;

procedure TUAtomicLock.Unlock;
begin
  InterlockedCompareExchange(_Lock, 0, 1);
end;

class operator TUAtomicLock.Initialize(var v: TUAtomicLock);
begin
  v.Initialize;
end;

class operator TUAtomicLock.Finalize(var v: TUAtomicLock);
begin
  v.Finalize;
end;
// TUAtomicLock end

// TUReadWriteLock begin
procedure TUReadWriteLock.Initialize;
begin
  _ReadCount := 0;
end;

procedure TUReadWriteLock.Finalize;
begin
end;

function TUReadWriteLock.TryReadLock: Boolean;
begin
  _ReadLock.Lock;
  Result := (_ReadCount > 0) or _WriteLock.TryLock;
  if Result then Inc(_ReadCount);
  _ReadLock.Unlock;
end;

procedure TUReadWriteLock.ReadLock;
begin
  _ReadLock.Lock;
  if _ReadCount = 0 then _WriteLock.Lock;
  Inc(_ReadCount);
  _ReadLock.Unlock;
end;

procedure TUReadWriteLock.ReadUnlock;
begin
  _ReadLock.Lock;
  Dec(_ReadCount);
  if _ReadCount = 0 then _WriteLock.Unlock;
  _ReadLock.Unlock;
end;

function TUReadWriteLock.TryWriteLock: Boolean;
begin
  Result := _WriteLock.TryLock;
end;

procedure TUReadWriteLock.WriteLock;
begin
  _WriteLock.Lock;
end;

procedure TUReadWriteLock.WriteUnlock;
begin
  _WriteLock.Unlock;
end;

class operator TUReadWriteLock.Initialize(var v: TUReadWriteLock);
begin
  v.Initialize;
end;

class operator TUReadWriteLock.Finalize(var v: TUReadWriteLock);
begin
  v.Finalize;
end;
// TUReadWriteLock end

// TUEvent begin
procedure TUEvent.Initialize;
begin
  _Event := RTLEventCreate;
  RTLEventResetEvent(_Event);
end;

procedure TUEvent.Finalize;
begin
  RTLEventDestroy(_Event);
end;

procedure TUEvent.Signal;
begin
  RTLEventSetEvent(_Event);
end;

procedure TUEvent.Unsignal;
begin
  RTLEventResetEvent(_Event);
end;

procedure TUEvent.WaitFor;
begin
  RTLEventWaitFor(_Event);
end;

procedure TUEvent.WaitFor(const Timeout: LongWord);
begin
  RTLEventWaitFor(_Event, Timeout);
end;

class operator TUEvent.Initialize(var v: TUEvent);
begin
  v.Initialize;
end;

class operator TUEvent.Finalize(var v: TUEvent);
begin
  v.Finalize;
end;
// TUEvent end

// TUTask.TTaskThread begin
procedure TUTask.TTaskThread.AfterConstruction;
begin
  inherited AfterConstruction;
  _StartTime := GetTickCount64;
end;

procedure TUTask.TTaskThread.Execute;
begin
  _StartTime := GetTickCount64;
  if Assigned(Proc) then Res := Proc(Args);
  UFinalizeVarRecArr(Args);
end;

function TUTask.TTaskThread.TimeRunning: QWord;
begin
  Result := GetTickCount64 - _StartTime;
end;

function TUTask.TTaskThread.RefInc: Integer;
begin
  Inc(Ref);
  Result := Ref;
end;

function TUTask.TTaskThread.RefDec: Integer;
begin
  Dec(Ref);
  Result := Ref;
  if Ref <= 0 then
  begin
    WaitFor;
    Free;
  end;
end;

function TUTask.TTaskThread.RefDecKillThread: Integer;
begin
  Dec(Ref);
  Result := Ref;
  if Ref <= 0 then
  begin
    WaitForThreadTerminate(Handle, 100);
    KillThread(Handle);
    Free;
  end;
end;
// TUTask.TTaskThread end

// TUTask begin
procedure TUTask.Initialize;
begin
  _Thread := nil;
end;

procedure TUTask.Finalize;
begin
  SetThread(nil);
end;

procedure TUTask.SetThread(const TaskThread: TTaskThread);
begin
  if _Thread = TaskThread then Exit;
  if Assigned(_Thread) then _Thread.RefDec;
  _Thread := TaskThread;
  if Assigned(_Thread) then _Thread.RefInc;
end;

function TUTask.TimeRunning: QWord;
begin
  if not IsStarted or IsComplete then Exit(0);
  Result := _Thread.TimeRunning;
end;

function TUTask.IsComplete: Boolean;
begin
  Result := Assigned(_Thread) and _Thread.Finished;
end;

function TUTask.IsStarted: Boolean;
begin
  Result := Assigned(_Thread);
end;

function TUTask.TaskResult: TRes;
begin
  if Assigned(_Thread) then Exit(_Thread.Res);
  Result := Default(TRes);
end;

procedure TUTask.Reset;
begin
  WaitFor;
  SetThread(nil);
end;

procedure TUTask.WaitFor;
begin
  if Assigned(_Thread) then _Thread.WaitFor;
end;

procedure TUTask.Kill;
begin
  if not IsStarted or IsComplete then Exit;
  _Thread.RefDecKillThread;
  _Thread := nil;
end;

procedure TUTask.TimeoutKill(const Timeout: QWord);
begin
  if not IsStarted or IsComplete then Exit;
  WaitForThreadTerminate(_Thread.Handle, Timeout);
  if not IsComplete then Kill;
end;

class function TUTask.StartTask(
  const Proc: TFunc;
  const Args: array of const
): TUTask;
  var Thread: TTaskThread;
begin
  Thread := TTaskThread.Create(True);
  Thread.Ref := 0;
  Thread.Proc := Proc;
  SetLength(Thread.Args, Length(Args));
  if Length(Args) > 0 then
  begin
    Thread.Args := UCopyVarRecArr(Args);
  end;
  Thread.Res := Default(TRes);
  Thread.Start;
  Result.SetThread(Thread);
end;

class operator TUTask.Initialize(var v: TUTask);
begin
  v.Initialize;
end;

class operator TUTask.Finalize(var v: TUTask);
begin
  v.Finalize;
end;

class operator TUTask.Copy(constref Src: TUTask; var Dst: TUTask);
begin
  Dst.SetThread(Src._Thread);
end;
// TUTask end

// TUSharedRef begin
function TUSharedRef.GetPtr: TPtr;
begin
  Result := TPtr(_Ptr as TURefClass);
end;

procedure TUSharedRef.SetPtr(const Value: TPtr);
begin
  if Pointer(_Ptr) = Pointer(Value) then Exit;
  _Ptr := IInterface(Value);
end;

procedure TUSharedRef.Initialize;
begin
  _Ptr := nil;
end;

function TUSharedRef.IsValid: Boolean;
begin
  Result := _Ptr <> nil;
end;

class operator TUSharedRef.Initialize(var v: TSelf);
begin
  v.Initialize;
end;

class operator TUSharedRef.:=(const Value: TPtr): TSelf;
begin
  {$push}
  {$warnings off}
  Result.Ptr := Value;
  {$pop}
end;

class operator TUSharedRef.:=(const Value: Pointer): TSelf;
begin
  {$push}
  {$warnings off}
  Result.Ptr := TPtr(Value);
  {$pop}
end;

class operator TUSharedRef.=(v1, v2: TSelf): Boolean;
begin
  Result := v1._Ptr = v2._Ptr;
end;
// TUSharedRef end

// TUWeakRef begin
procedure TUWeakRef.Assign(const Value: TPtr);
begin
  if Assigned(Value) then
  begin
    if Assigned(Value.Weak) then
    begin
      _Weak := Value.Weak;
    end
    else
    begin
      _Weak := TUWeakCounter.Create(Value);
    end;
  end
  else
  begin
    _Weak := nil;
  end;
end;

function TUWeakRef.GetPtr: TPtr;
begin
  Result := TPtr((_Weak as TUWeakCounter).Obj);
end;

function TUWeakRef.IsValid: Boolean;
begin
  Result := Assigned(_Weak) and Assigned((_Weak as TUWeakCounter).Obj);
end;

function TUWeakRef.AsShared: TShared;
begin
  if IsValid then Result := TPtr((_Weak as TUWeakCounter).Obj) else Result := nil;
end;

class operator TUWeakRef.:= (const Value: TPtr): TSelf;
begin
  Result.Assign(Value);
end;

class operator TUWeakRef.:= (const Value: TShared): TSelf;
begin
  Result.Assign(Value.Ptr);
end;
// TUWeakRef end

// TUWeakCounter begin
constructor TUWeakCounter.Create(const AObj: TURefClass);
begin
  _Obj := AObj;
  _Obj._Weak := Self;
end;

destructor TUWeakCounter.Destroy;
begin
  if Assigned(_Obj) then _Obj._Weak := nil;
  inherited Destroy;
end;
// TUWeakCounter end

// TURefClass begin
function TURefClass.QueryInterface(constref IID: tguid; out Obj): Longint; call;
begin
  if GetInterface(IID, Obj) then Result := S_OK else Result := Longint(E_NOINTERFACE);
end;

function TURefClass._AddRef: Longint; call;
begin
  Result := InterlockedIncrement(_RefCount);
end;

function TURefClass._Release: Longint; call;
begin
   Result := InterlockedDecrement(_RefCount);
   if Result = 0 then Self.Destroy;
end;

procedure TURefClass.AfterConstruction;
begin
   InterlockedDecrement(_RefCount);
end;

procedure TURefClass.BeforeDestruction;
begin
  if Assigned(_Weak) then _Weak._Obj := nil;
end;

procedure TURefClass.AddReference(const Obj: TURefClass);
  var i: Integer;
begin
  for i := 0 to High(_References) do
  begin
    if _References[i].Ptr = Obj then Exit;
  end;
  SetLength(_References, Length(_References) + 1);
  i := High(_References);
  _References[i] := Obj;
end;

procedure TURefClass.RemoveReference(const Obj: TURefClass);
  var i, j: Integer;
begin
  for i := 0 to High(_References) do
  if _References[i].Ptr = Obj then
  begin
    for j := i to High(_References) - 1 do
    begin
      _References[j] := _References[j + 1];
    end;
    SetLength(_References, Length(_References) - 1);
    Exit;
  end;
end;

class function TURefClass.NewInstance: TObject;
begin
   Result := inherited NewInstance;
   if Assigned(Result) then TURefClass(Result)._RefCount := 1;
end;
// TURefClass end

// TUStreamHelper begin
function TUStreamHelper.GetSize: Int64;
begin
  Result := _Stream.Size;
end;

function TUStreamHelper.GetPosition: Int64;
begin
  Result := _Stream.Position;
end;

procedure TUStreamHelper.SetPosition(const Value: Int64);
begin
  _Stream.Position := Value;
end;

function TUStreamHelper.GetRemaining: Int64;
begin
  Result := _Stream.Size - _Stream.Position;
end;

procedure TUStreamHelper.PosPush;
begin
  SetLength(_PosStack, Length(_PosStack) + 1);
  _PosStack[High(_PosStack)] := _Stream.Position;
end;

procedure TUStreamHelper.PosPop;
begin
  if Length(_PosStack) <= 0 then Exit;
  _Stream.Seek(_PosStack[High(_PosStack)], soFromBeginning);
  SetLength(_PosStack, Length(_PosStack) - 1);
end;

procedure TUStreamHelper.PosDiscard;
begin
  if Length(_PosStack) <= 0 then Exit;
  SetLength(_PosStack, Length(_PosStack) - 1);
end;

function TUStreamHelper.IsEoF: Boolean;
begin
  Result := _Stream.Position >= _Stream.Size;
end;

function TUStreamHelper.ReadBuffer(const Buffer: Pointer; const Count: Int64): Int64;
begin
  Result := _Stream.Read(Buffer^, Count);
end;

{$push}
{$hints off}
function TUStreamHelper.ReadBool: Boolean;
begin
  _Stream.Read(Result, SizeOf(Result));
end;

function TUStreamHelper.ReadUInt8: UInt8;
begin
  _Stream.Read(Result, SizeOf(Result));
end;

function TUStreamHelper.ReadUInt16: UInt16;
begin
  _Stream.Read(Result, SizeOf(Result))
end;

function TUStreamHelper.ReadUInt32: UInt32;
begin
  _Stream.Read(Result, SizeOf(Result));
end;

function TUStreamHelper.ReadUInt64: UInt64;
begin
  _Stream.Read(Result, SizeOf(Result));
end;

function TUStreamHelper.ReadInt8: Int8;
begin
  _Stream.Read(Result, SizeOf(Result));
end;

function TUStreamHelper.ReadInt16: Int16;
begin
  _Stream.Read(Result, SizeOf(Result));
end;

function TUStreamHelper.ReadInt32: Int32;
begin
  _Stream.Read(Result, SizeOf(Result));
end;

function TUStreamHelper.ReadInt64: Int64;
begin
  _Stream.Read(Result, SizeOf(Result));
end;

function TUStreamHelper.ReadFloat: Single;
begin
  _Stream.Read(Result, SizeOf(Result));
end;

function TUStreamHelper.ReadDouble: Double;
begin
  _Stream.Read(Result, SizeOf(Result));
end;

function TUStreamHelper.ReadString: String;
  var l: UInt32;
begin
  l := ReadUInt32;
  SetLength(Result, l);
  ReadBuffer(@Result[1], l);
end;

function TUStreamHelper.ReadStringNT: String;
  var b: UInt8;
begin
  Result := '';
  b := ReadUInt8;
  while b <> 0 do
  begin
    Result += AnsiChar(b);
    b := ReadUInt8;
  end;
end;

generic function TUStreamHelper.Read<T>: T; inline;
begin
  ReadBuffer(@Result, SizeOf(T));
end;
{$pop}

function TUStreamHelper.WriteBuffer(const Buffer: Pointer; const Count: Int64): Int64;
begin
  Result := _Stream.Write(Buffer^, Count);
end;

procedure TUStreamHelper.WriteBool(const Value: Boolean);
begin
  _Stream.Write(Value, SizeOf(Value));
end;

procedure TUStreamHelper.WriteUInt8(const value: UInt8);
begin
  _Stream.Write(Value, SizeOf(Value));
end;

procedure TUStreamHelper.WriteUInt16(const value: UInt16);
begin
  _Stream.Write(Value, SizeOf(Value));
end;

procedure TUStreamHelper.WriteUInt32(const value: UInt32);
begin
  _Stream.Write(Value, SizeOf(Value));
end;

procedure TUStreamHelper.WriteUInt64(const value: UInt64);
begin
  _Stream.Write(Value, SizeOf(Value));
end;

procedure TUStreamHelper.WriteInt8(const value: Int8);
begin
  _Stream.Write(Value, SizeOf(Value));
end;

procedure TUStreamHelper.WriteInt16(const value: Int16);
begin
  _Stream.Write(Value, SizeOf(Value));
end;

procedure TUStreamHelper.WriteInt32(const value: Int32);
begin
  _Stream.Write(Value, SizeOf(Value));
end;

procedure TUStreamHelper.WriteInt64(const value: Int64);
begin
  _Stream.Write(Value, SizeOf(Value));
end;

procedure TUStreamHelper.WriteFloat(const value: Single);
begin
  _Stream.Write(Value, SizeOf(Value));
end;

procedure TUStreamHelper.WriteDouble(const value: Double);
begin
  _Stream.Write(Value, SizeOf(Value));
end;

procedure TUStreamHelper.WriteStringRaw(const Value: String);
begin
  WriteBuffer(@Value[1], Length(Value));
end;

procedure TUStreamHelper.WriteString(const Value: String);
begin
  WriteUInt32(Length(Value));
  WriteBuffer(@Value[1], Length(Value));
end;

procedure TUStreamHelper.WriteStringNT(const Value: String);
begin
  WriteBuffer(@Value[1], Length(Value));
  WriteUInt8(0);
end;

generic procedure TUStreamHelper.Write<T>(const Value: T);
begin
  WriteBuffer(@Value, SizeOf(T));
end;

procedure TUStreamHelper.Skip(const Count: Int64);
begin
  _Stream.Seek(Count, soFromCurrent);
end;

procedure TUStreamHelper.SkipString;
  var l: UInt32;
begin
  l := ReadUInt32;
  Skip(l);
end;

function TUStreamHelper.ToString: String;
begin
  {$push}
  {$hints off}
  SetLength(Result, Remaining);
  {$pop}
  ReadBuffer(@Result[1], Remaining);
end;

constructor TUStreamHelper.Create(const AStream: TStream);
begin
  _Stream := AStream;
end;
// TUStreamHelper end

// TUConstMemoryStream begin
function TUConstMemoryStream.GetSize: Int64;
begin
  Result := _Size;
end;

function TUConstMemoryStream.GetPosition: Int64;
begin
  Result := _Position;
end;

function TUConstMemoryStream.Read(var Buffer; Count: LongInt): LongInt;
begin
  Result := 0;
  if (_Size > 0) and (_Position < _size) and (_Position >= 0) then
  begin
    Result := Count;
    if (Result > (_Size - _Position)) then
    begin
      Result := (_Size - _Position);
    end;
    Move((_Memory + _Position)^, Buffer, Result);
    _Position += Result;
  end;
end;

function TUConstMemoryStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  case Word(Origin) of
    soFromBeginning: _Position := Offset;
    soFromEnd: _Position := _Size + Offset;
    soFromCurrent: _Position := _Position + Offset;
  end;
  Result := _Position;
end;

constructor TUConstMemoryStream.Create(const Buffer: Pointer; const BufferSize: UInt32);
begin
  inherited Create;
  _Memory := Buffer;
  _Size := BufferSize;
  _Position := 0;
end;
// TUConstMemoryStream end

// TUSerializable begin
function TUSerializable.VerifyProp(const Index: Integer; const PropType: TTypeKinds): Boolean;
begin
  Result := (
    (Index >= 0) and (Index <= High(_PropList))
    and (_PropList[Index]^.PropType^.Kind in PropType)
  );
end;

function TUSerializable.VerifyArrayProp(const Index, ArrayIndex: Integer; const PropType: TTypeKinds = tkAny): Boolean;
  var td: PTypeData;
  var ti: PTypeInfo;
begin
  if not VerifyProp(Index, [tkDynArray]) then Exit;
  td := GetTypeData(_PropList[Index]^.PropType);
  ti := td^.ElType;
  if not Assigned(ti) then ti := td^.ElType2;
  Result := (
    Assigned(ti) and (ti^.Kind in PropType)
    and (ArrayIndex >= 0) and (ArrayIndex < GetPropArrayLength(Index))
  );
end;

function TUSerializable.GetArrayData(const Index: Integer): Pointer;
  type TArr = array of Pointer;
begin
  if GetPropArrayLength(Index) < 1 then Exit(nil);
  Result := @TArr(GetDynArrayProp(Self, _PropList[Index]))[0];
end;

function TUSerializable.GetOrdSize(const OrdType: TOrdType): UInt32;
  const Sizes: array[Ord(Low(TOrdType))..Ord(High(TOrdType))] of UInt32 = (
    1, 1, 2, 2, 4, 4, 8, 8
  );
begin
  Result := Sizes[Ord(OrdType)];
end;

function TUSerializable.GetArrayElementTypeInfo(const Index: Integer): PTypeInfo;
  var td: PTypeData;
begin
  td := GetTypeData(_PropList[Index]^.PropType);
  Result := td^.ElType;
  if not Assigned(Result) then Result := td^.ElType2;
end;

function TUSerializable.GetPropCount: Integer;
begin
  Result := _TypeData^.PropCount;
end;

function TUSerializable.GetPropInfo(const Index: Integer): PPropInfo;
begin
  if not VerifyProp(Index) then Exit(nil);
  Result := _PropList[Index];
end;

function TUSerializable.GetPropArrayInfo(const Index: Integer): PTypeInfo;
  var td: PTypeData;
begin
  if not VerifyProp(Index, [tkDynArray]) then Exit(nil);
  td := GetTypeData(_PropList[Index]^.PropType);
  Result := td^.ElType;
  if not Assigned(Result) then Result := td^.ElType2;
end;

function TUSerializable.GetPropEnum(const Index: Integer): Int32;
begin
  if not VerifyProp(Index, [tkEnumeration]) then Exit(0);
  Result := GetOrdProp(Self, _PropList[Index]);
end;

procedure TUSerializable.SetPropEnum(const Index: Integer; const Value: Int32);
begin
  if not VerifyProp(Index, [tkEnumeration]) then Exit;
  SetOrdProp(Self, _PropList[Index], Value);
end;

function TUSerializable.GetPropBool(const Index: Integer): Boolean;
begin
  if not VerifyProp(Index, [tkBool]) then Exit(False);
  Result := UIntToBool(GetOrdProp(Self, _PropList[Index]));
end;

procedure TUSerializable.SetPropBool(const Index: Integer; const Value: Boolean);
begin
  if not VerifyProp(Index, [tkBool]) then Exit;
  SetOrdProp(Self, _PropList[Index], UBoolToInt(Value));
end;

function TUSerializable.GetPropInt8(const Index: Integer): Int8;
begin
  if not VerifyProp(Index, [tkInteger]) then Exit(0);
  Result := Int8(GetOrdProp(Self, _PropList[Index]));
end;

procedure TUSerializable.SetPropInt8(const Index: Integer; const Value: Int8);
begin
  if not VerifyProp(Index, [tkInteger]) then Exit;
  SetOrdProp(Self, _PropList[Index], Int64(Value));
end;

function TUSerializable.GetPropInt16(const Index: Integer): Int16;
begin
  if not VerifyProp(Index, [tkInteger]) then Exit(0);
  Result := Int16(GetOrdProp(Self, _PropList[Index]));
end;

procedure TUSerializable.SetPropInt16(const Index: Integer; const Value: Int16);
begin
  if not VerifyProp(Index, [tkInteger]) then Exit;
  SetOrdProp(Self, _PropList[Index], Int64(Value));
end;

function TUSerializable.GetPropInt32(const Index: Integer): Int32;
begin
  if not VerifyProp(Index, [tkInteger]) then Exit(0);
  Result := Int32(GetOrdProp(Self, _PropList[Index]));
end;

procedure TUSerializable.SetPropInt32(const Index: Integer; const Value: Int32);
begin
  if not VerifyProp(Index, [tkInteger]) then Exit;
  SetOrdProp(Self, _PropList[Index], Int64(Value));
end;

function TUSerializable.GetPropInt64(const Index: Integer): Int64;
begin
  if not VerifyProp(Index, [tkInteger, tkInt64]) then Exit(0);
  Result := Int64(GetOrdProp(Self, _PropList[Index]));
end;

procedure TUSerializable.SetPropInt64(const Index: Integer; const Value: Int64);
begin
  if not VerifyProp(Index, [tkInteger, tkInt64]) then Exit;
  SetOrdProp(Self, _PropList[Index], Int64(Value));
end;

function TUSerializable.GetPropUInt8(const Index: Integer): UInt8;
begin
  if not VerifyProp(Index, [tkInteger]) then Exit(0);
  Result := UInt8(GetOrdProp(Self, _PropList[Index]));
end;

procedure TUSerializable.SetPropUInt8(const Index: Integer; const Value: UInt8);
begin
  if not VerifyProp(Index, [tkInteger]) then Exit;
  SetOrdProp(Self, _PropList[Index], Int64(Value));
end;

function TUSerializable.GetPropUInt16(const Index: Integer): UInt16;
begin
  if not VerifyProp(Index, [tkInteger]) then Exit(0);
  Result := UInt16(GetOrdProp(Self, _PropList[Index]));
end;

procedure TUSerializable.SetPropUInt16(const Index: Integer; const Value: UInt16);
begin
  if not VerifyProp(Index, [tkInteger]) then Exit;
  SetOrdProp(Self, _PropList[Index], Int64(Value));
end;

function TUSerializable.GetPropUInt32(const Index: Integer): UInt32;
begin
  if not VerifyProp(Index, [tkInteger]) then Exit(0);
  Result := UInt32(GetOrdProp(Self, _PropList[Index]));
end;

procedure TUSerializable.SetPropUInt32(const Index: Integer; const Value: UInt32);
begin
  if not VerifyProp(Index, [tkInteger]) then Exit;
  SetOrdProp(Self, _PropList[Index], Int64(Value));
end;

function TUSerializable.GetPropUInt64(const Index: Integer): UInt64;
begin
  if not VerifyProp(Index, [tkInteger, tkQWord]) then Exit(0);
  Result := UInt64(GetOrdProp(Self, _PropList[Index]));
end;

procedure TUSerializable.SetPropUInt64(const Index: Integer; const Value: UInt64);
begin
  if not VerifyProp(Index, [tkInteger, tkQWord]) then Exit;
  SetOrdProp(Self, _PropList[Index], Int64(Value));
end;

function TUSerializable.GetPropFloat(const Index: Integer): Single;
begin
  if not VerifyProp(Index, [tkFloat]) then Exit(0);
  Result := GetFloatProp(Self, _PropList[Index]);
end;

procedure TUSerializable.SetPropFloat(const Index: Integer; const Value: Single);
begin
  if not VerifyProp(Index, [tkFloat]) then Exit;
  SetFloatProp(Self, _PropList[Index], Value);
end;

function TUSerializable.GetPropDouble(const Index: Integer): Double;
begin
  if not VerifyProp(Index, [tkFloat]) then Exit(0);
  Result := GetFloatProp(Self, _PropList[Index]);
end;

procedure TUSerializable.SetPropDouble(const Index: Integer; const Value: Double);
begin
  if not VerifyProp(Index, [tkFloat]) then Exit;
  SetFloatProp(Self, _PropList[Index], Value);
end;

function TUSerializable.GetPropString(const Index: Integer): String;
begin
  if not VerifyProp(Index, [tkAString]) then Exit;
  Result := GetStrProp(Self, _PropList[Index]);
end;

procedure TUSerializable.SetPropString(const Index: Integer; const Value: String);
begin
  if not VerifyProp(Index, [tkAString]) then Exit;
  SetStrProp(Self, _PropList[Index], Value);
end;

function TUSerializable.GetPropClass(const Index: Integer): TObject;
begin
  if not VerifyProp(Index, [tkClass]) then Exit(nil);
  Result := GetObjectProp(Self, _PropList[Index]);
end;

procedure TUSerializable.SetPropClass(const Index: Integer; const Value: TObject);
begin
  if not VerifyProp(Index, [tkClass]) then Exit;
  SetObjectProp(Self, _PropList[Index], Value);
end;

function TUSerializable.GetPropArrayLength(const Index: Integer): Integer;
  var td: PTypeData;
  var ti: PTypeInfo;
begin
  if not VerifyProp(Index, [tkDynArray]) then Exit(0);
  td := GetTypeData(_PropList[Index]^.PropType);
  ti := td^.ElType;
  if not Assigned(ti) then ti := td^.ElType2;
  case ti^.Kind of
    tkBool: Result := specialize GetDynArrayLength<Boolean>(Index);
    tkInteger, tkInt64, tkQWord, tkEnumeration:
    begin
      case td^.OrdType of
        otSByte: Result := specialize GetDynArrayLength<Int8>(Index);
        otSWord: Result := specialize GetDynArrayLength<Int16>(Index);
        otSLong: Result := specialize GetDynArrayLength<Int32>(Index);
        otSQWord: Result := specialize GetDynArrayLength<Int64>(Index);
        otUByte: Result := specialize GetDynArrayLength<UInt8>(Index);
        otUWord: Result := specialize GetDynArrayLength<UInt16>(Index);
        otULong: Result := specialize GetDynArrayLength<UInt32>(Index);
        otUQWord: Result := specialize GetDynArrayLength<UInt64>(Index);
      end;
    end;
    tkFloat:
    begin
      case td^.FloatType of
        ftSingle: Result := specialize GetDynArrayLength<Single>(Index);
        else Result := specialize GetDynArrayLength<Single>(Index);
      end;
    end;
    tkAString: Result := specialize GetDynArrayLength<String>(Index);
    tkClass: Result := specialize GetDynArrayLength<TObject>(Index);
    else Result := 0;
  end;
end;

procedure TUSerializable.SetPropArrayLength(const Index: Integer; const Value: Integer);
  var td, tde: PTypeData;
  var tie: PTypeInfo;
  var i, OldSize: Integer;
begin
  if not VerifyProp(Index, [tkDynArray]) then Exit;
  td := GetTypeData(_PropList[Index]^.PropType);
  tie := td^.ElType;
  if not Assigned(tie) then tie := td^.ElType2;
  tde := GetTypeData(tie);
  if not Assigned(tie) then tie := td^.ElType2;
  case tie^.Kind of
    tkBool: specialize SetDynArrayLength<Boolean>(Index, Value);
    tkInteger, tkInt64, tkQWord, tkEnumeration:
    begin
      case tde^.OrdType of
        otSByte: specialize SetDynArrayLength<Int8>(Index, Value);
        otSWord: specialize SetDynArrayLength<Int16>(Index, Value);
        otSLong: specialize SetDynArrayLength<Int32>(Index, Value);
        otSQWord: specialize SetDynArrayLength<Int64>(Index, Value);
        otUByte: specialize SetDynArrayLength<UInt8>(Index, Value);
        otUWord: specialize SetDynArrayLength<UInt16>(Index, Value);
        otULong: specialize SetDynArrayLength<UInt32>(Index, Value);
        otUQWord: specialize SetDynArrayLength<UInt64>(Index, Value);
      end;
    end;
    tkFloat:
    begin
      case tde^.FloatType of
        ftSingle: specialize SetDynArrayLength<Single>(Index, Value);
        else specialize SetDynArrayLength<Single>(Index, Value);
      end;
    end;
    tkAString: specialize SetDynArrayLength<String>(Index, Value);
    tkClass:
    begin
      OldSize := specialize GetDynArrayLength<TObject>(Index);
      for i := OldSize - 1 downto Value - 1 do
      begin
        PropArrayClass[Index, i].Free;
      end;
      specialize SetDynArrayLength<TObject>(Index, Value);
      for i := OldSize - 1 to Value - 1 do
      begin
        PropArrayClass[Index, i] := tde^.ClassType.Create;
      end;
    end;
    else begin end;
  end;
end;

function TUSerializable.GetPropArrayEnum(const Index, ArrayIndex: Integer): Int32;
  var td: PTypeData;
begin
  if not VerifyArrayProp(Index, ArrayIndex, [tkEnumeration]) then Exit(0);
  td := GetTypeData(GetArrayElementTypeInfo(Index));
  case td^.OrdType of
    otSByte: Result := specialize GetDynArrayElement<Int8>(Index, ArrayIndex);
    otSWord: Result := specialize GetDynArrayElement<Int16>(Index, ArrayIndex);
    otSLong: Result := specialize GetDynArrayElement<Int32>(Index, ArrayIndex);
    otSQWord: Result := specialize GetDynArrayElement<Int64>(Index, ArrayIndex);
    otUByte: Result := specialize GetDynArrayElement<UInt8>(Index, ArrayIndex);
    otUWord: Result := specialize GetDynArrayElement<UInt16>(Index, ArrayIndex);
    otULong: Result := specialize GetDynArrayElement<UInt32>(Index, ArrayIndex);
    otUQWord: Result := specialize GetDynArrayElement<UInt64>(Index, ArrayIndex);
    else Result := 0;
  end;
end;

procedure TUSerializable.SetPropArrayEnum(const Index, ArrayIndex: Integer; const Value: Int32);
  var td: PTypeData;
begin
  if not VerifyArrayProp(Index, ArrayIndex, [tkEnumeration]) then Exit;
  td := GetTypeData(GetArrayElementTypeInfo(Index));
  case td^.OrdType of
    otSByte: specialize SetDynArrayElement<Int8>(Index, ArrayIndex, Value);
    otSWord: specialize SetDynArrayElement<Int16>(Index, ArrayIndex, Value);
    otSLong: specialize SetDynArrayElement<Int32>(Index, ArrayIndex, Value);
    otSQWord: specialize SetDynArrayElement<Int64>(Index, ArrayIndex, Value);
    otUByte: specialize SetDynArrayElement<UInt8>(Index, ArrayIndex, Value);
    otUWord: specialize SetDynArrayElement<UInt16>(Index, ArrayIndex, Value);
    otULong: specialize SetDynArrayElement<UInt32>(Index, ArrayIndex, Value);
    otUQWord: specialize SetDynArrayElement<UInt64>(Index, ArrayIndex, Value);
  end;
end;

function TUSerializable.GetPropArrayBool(const Index, ArrayIndex: Integer): Boolean;
begin
  if not VerifyArrayProp(Index, ArrayIndex, [tkBool]) then Exit(False);
  Result := specialize GetDynArrayElement<Boolean>(Index, ArrayIndex);
end;

procedure TUSerializable.SetPropArrayBool(const Index, ArrayIndex: Integer; const Value: Boolean);
begin
  if not VerifyArrayProp(Index, ArrayIndex, [tkBool]) then Exit;
  specialize SetDynArrayElement<Boolean>(Index, ArrayIndex, Value);
end;

function TUSerializable.GetPropArrayInt8(const Index, ArrayIndex: Integer): Int8;
begin
  if not VerifyArrayProp(Index, ArrayIndex, [tkInteger, tkInt64, tkQWord]) then Exit(0);
  Result := specialize GetDynArrayElement<Int8>(Index, ArrayIndex);
end;

procedure TUSerializable.SetPropArrayInt8(const Index, ArrayIndex: Integer; const Value: Int8);  
begin
  if not VerifyArrayProp(Index, ArrayIndex, [tkInteger, tkInt64, tkQWord]) then Exit;
  specialize SetDynArrayElement<Int8>(Index, ArrayIndex, Value);
end;

function TUSerializable.GetPropArrayInt16(const Index, ArrayIndex: Integer): Int16;     
begin
  if not VerifyArrayProp(Index, ArrayIndex, [tkInteger, tkInt64, tkQWord]) then Exit(0);
  Result := specialize GetDynArrayElement<Int16>(Index, ArrayIndex);
end;

procedure TUSerializable.SetPropArrayInt16(const Index, ArrayIndex: Integer; const Value: Int16);  
begin
  if not VerifyArrayProp(Index, ArrayIndex, [tkInteger, tkInt64, tkQWord]) then Exit;
  specialize SetDynArrayElement<Int16>(Index, ArrayIndex, Value);
end;

function TUSerializable.GetPropArrayInt32(const Index, ArrayIndex: Integer): Int32;     
begin
  if not VerifyArrayProp(Index, ArrayIndex, [tkInteger, tkInt64, tkQWord]) then Exit(0);
  Result := specialize GetDynArrayElement<Int32>(Index, ArrayIndex);
end;

procedure TUSerializable.SetPropArrayInt32(const Index, ArrayIndex: Integer; const Value: Int32);  
begin
  if not VerifyArrayProp(Index, ArrayIndex, [tkInteger, tkInt64, tkQWord]) then Exit;
  specialize SetDynArrayElement<Int32>(Index, ArrayIndex, Value);
end;

function TUSerializable.GetPropArrayInt64(const Index, ArrayIndex: Integer): Int64;          
begin
  if not VerifyArrayProp(Index, ArrayIndex, [tkInteger, tkInt64, tkQWord]) then Exit(0);
  Result := specialize GetDynArrayElement<Int64>(Index, ArrayIndex);
end;

procedure TUSerializable.SetPropArrayInt64(const Index, ArrayIndex: Integer; const Value: Int64); 
begin
  if not VerifyArrayProp(Index, ArrayIndex, [tkInteger, tkInt64, tkQWord]) then Exit;
  specialize SetDynArrayElement<Int64>(Index, ArrayIndex, Value);
end;

function TUSerializable.GetPropArrayUInt8(const Index, ArrayIndex: Integer): UInt8;    
begin
  if not VerifyArrayProp(Index, ArrayIndex, [tkInteger, tkInt64, tkQWord]) then Exit(0);
  Result := specialize GetDynArrayElement<UInt8>(Index, ArrayIndex);
end;

procedure TUSerializable.SetPropArrayUInt8(const Index, ArrayIndex: Integer; const Value: UInt8);  
begin
  if not VerifyArrayProp(Index, ArrayIndex, [tkInteger, tkInt64, tkQWord]) then Exit;
  specialize SetDynArrayElement<UInt8>(Index, ArrayIndex, Value);
end;

function TUSerializable.GetPropArrayUInt16(const Index, ArrayIndex: Integer): UInt16;        
begin
  if not VerifyArrayProp(Index, ArrayIndex, [tkInteger, tkInt64, tkQWord]) then Exit(0);
  Result := specialize GetDynArrayElement<UInt16>(Index, ArrayIndex);
end;

procedure TUSerializable.SetPropArrayUInt16(const Index, ArrayIndex: Integer; const Value: UInt16);  
begin
  if not VerifyArrayProp(Index, ArrayIndex, [tkInteger, tkInt64, tkQWord]) then Exit;
  specialize SetDynArrayElement<UInt16>(Index, ArrayIndex, Value);
end;

function TUSerializable.GetPropArrayUInt32(const Index, ArrayIndex: Integer): UInt32;    
begin
  if not VerifyArrayProp(Index, ArrayIndex, [tkInteger, tkInt64, tkQWord]) then Exit(0);
  Result := specialize GetDynArrayElement<UInt32>(Index, ArrayIndex);
end;

procedure TUSerializable.SetPropArrayUInt32(const Index, ArrayIndex: Integer; const Value: UInt32); 
begin
  if not VerifyArrayProp(Index, ArrayIndex, [tkInteger, tkInt64, tkQWord]) then Exit;
  specialize SetDynArrayElement<UInt32>(Index, ArrayIndex, Value);
end;

function TUSerializable.GetPropArrayUInt64(const Index, ArrayIndex: Integer): UInt64;        
begin
  if not VerifyArrayProp(Index, ArrayIndex, [tkInteger, tkInt64, tkQWord]) then Exit(0);
  Result := specialize GetDynArrayElement<UInt64>(Index, ArrayIndex);
end;

procedure TUSerializable.SetPropArrayUInt64(const Index, ArrayIndex: Integer; const Value: UInt64); 
begin
  if not VerifyArrayProp(Index, ArrayIndex, [tkInteger, tkInt64, tkQWord]) then Exit;
  specialize SetDynArrayElement<UInt64>(Index, ArrayIndex, Value);
end;

function TUSerializable.GetPropArrayFloat(const Index, ArrayIndex: Integer): Single;
begin
  if not VerifyArrayProp(Index, ArrayIndex, [tkFloat]) then Exit(0);
  Result := specialize GetDynArrayElement<Single>(Index, ArrayIndex);
end;

procedure TUSerializable.SetPropArrayFloat(const Index, ArrayIndex: Integer; const Value: Single);   
begin
  if not VerifyArrayProp(Index, ArrayIndex, [tkFloat]) then Exit;
  specialize SetDynArrayElement<Single>(Index, ArrayIndex, Value);
end;

function TUSerializable.GetPropArrayDouble(const Index, ArrayIndex: Integer): Double;           
begin
  if not VerifyArrayProp(Index, ArrayIndex, [tkFloat]) then Exit(0);
  Result := specialize GetDynArrayElement<Double>(Index, ArrayIndex);
end;

procedure TUSerializable.SetPropArrayDouble(const Index, ArrayIndex: Integer; const Value: Double);  
begin
  if not VerifyArrayProp(Index, ArrayIndex, [tkFloat]) then Exit;
  specialize SetDynArrayElement<Double>(Index, ArrayIndex, Value);
end;

function TUSerializable.GetPropArrayString(const Index, ArrayIndex: Integer): String;         
begin
  if not VerifyArrayProp(Index, ArrayIndex, [tkAString]) then Exit('');
  Result := specialize GetDynArrayElement<String>(Index, ArrayIndex);
end;

procedure TUSerializable.SetPropArrayString(const Index, ArrayIndex: Integer; const Value: String);  
begin
  if not VerifyArrayProp(Index, ArrayIndex, [tkAString]) then Exit;
  specialize SetDynArrayElement<String>(Index, ArrayIndex, Value);
end;

function TUSerializable.GetPropArrayClass(const Index, ArrayIndex: Integer): TObject;
begin
  if not VerifyArrayProp(Index, ArrayIndex, [tkClass]) then Exit(nil);
  Result := specialize GetDynArrayElement<TObject>(Index, ArrayIndex);
end;

procedure TUSerializable.SetPropArrayClass(const Index, ArrayIndex: Integer; const Value: TObject);
begin
  if not VerifyArrayProp(Index, ArrayIndex, [tkClass]) then Exit;
  specialize SetDynArrayElement<TObject>(Index, ArrayIndex, Value);
end;

generic function TUSerializable.GetDynArrayLength<T>(const Index: Integer): Integer;
  type TArrType = array of T;
  var Ptr: Pointer;
begin
  Ptr := GetDynArrayProp(Self, _PropList[Index]);
  if not Assigned(Ptr) then Exit(0);
  Result := Length(TArrType(Ptr));
end;

generic procedure TUSerializable.SetDynArrayLength<T>(const Index: Integer; const Value: Integer);
  type TArrType = array of T;
  var Arr: TArrType;
  var Old: Pointer;
begin
  Old := GetDynArrayProp(Self, _PropList[Index]);
  if Assigned(Old) then Arr := TArrType(Old);
  if Length(Arr) = Value then Exit;
  SetLength(Arr, Value);
  SetDynArrayProp(Self, _PropList[Index], Pointer(Arr));
end;

generic function TUSerializable.GetDynArrayElement<T>(const Index, ArrayIndex: Integer): T;
  type TArrType = array of T;
begin
  Result := TArrType(GetDynArrayProp(Self, _PropList[Index]))[ArrayIndex];
end;

generic procedure TUSerializable.SetDynArrayElement<T>(const Index, ArrayIndex: Integer; const Value: T);
  type TArrType = array of T;
begin
  TArrType(GetDynArrayProp(Self, _PropList[Index]))[ArrayIndex] := Value;
end;

function TUSerializable.FindProp(const Name: String; const PropType: TTypeKinds): Integer;
  var i: Integer;
  var NameLC: String;
begin
  NameLC := LowerCase(Name);
  for i := 0 to High(_PropList) do
  if (LowerCase(_PropList[i]^.Name) = NameLC) and (_PropList[i]^.PropType^.Kind in PropType) then
  begin
    Exit(i);
  end;
  Result := -1;
end;

procedure TUSerializable.AfterConstruction;
  var td: PTypeData;
  var i: Integer;
begin
  inherited AfterConstruction;
  _TypeInfo := PTypeInfo(Self.ClassType.ClassInfo);
  _TypeData := GetTypeData(_TypeInfo);
  if (_TypeData^.PropCount > 0) then
  begin
    SetLength(_PropList, _TypeData^.PropCount);
    GetPropInfos(_TypeInfo, @_PropList[0]);
    for i := 0 to _TypeData^.PropCount - 1 do
    begin
      td := GetTypeData(_PropList[i]^.PropType);
      case _PropList[i]^.PropType^.Kind of
        tkClass:
        begin
          PropClass[i] := td^.ClassType.Create;
        end;
        tkInteger, tkInt64, tkQWord, tkBool, tkSet:
        begin
          SetOrdProp(Self, _PropList[i], 0);
        end;
        tkFloat:
        begin
          SetFloatProp(Self, _PropList[i], 0);
        end;
        else begin end;
      end;
    end;
  end;
end;

procedure TUSerializable.BeforeDestruction;
  var i: Integer;
  var td: PTypeData;
  var tie: PTypeInfo;
begin
  for i := 0 to _TypeData^.PropCount - 1 do
  begin
    if _PropList[i]^.PropType^.Kind = tkClass then
    begin
      PropClass[i].Free;
    end
    else if _PropList[i]^.PropType^.Kind = tkDynArray then
    begin
      td := GetTypeData(_PropList[i]^.PropType);
      tie := td^.ElType;
      if not Assigned(tie) then tie := td^.ElType2;
      if tie^.Kind = tkClass then
      begin
        PropArrayLength[i] := 0;
      end;
    end;
  end;
  inherited BeforeDestruction;
end;

procedure TUSerializable.SerializeTo(const Stream: TStream);
  var sh: TUStreamHelper;
  var i, sp, s, al, ai: Int64;
  var pd, pde: PTypeData;
  var pi: PTypeInfo;
  var Obj: TObject;
begin
  sh := TUStreamHelper.Create(Stream);
  try
    sh.WriteInt32(Length(_PropList));
    for i := 0 to High(_PropList) do
    begin
      sh.WriteString(_PropList[i]^.Name);
      sh.WriteUInt8(UInt8(_PropList[i]^.PropType^.Kind));
      sp := sh.Position;
      sh.WriteInt32(0);
      pd := GetTypeData(_PropList[i]^.PropType);
      case _PropList[i]^.PropType^.Kind of
        tkEnumeration:
        begin
          sh.WriteInt32(PropEnum[i]);
        end;
        tkBool:
        begin
          sh.WriteBool(PropBool[i]);
        end;
        tkInteger, tkInt64, tkQWord:
        begin
          sh.WriteUInt8(UInt8(pd^.OrdType));
          case pd^.OrdType of
            otSByte: sh.WriteInt8(PropInt8[i]);
            otSWord: sh.WriteInt16(PropInt16[i]);
            otSLong: sh.WriteInt32(PropInt32[i]);
            otSQWord: sh.WriteInt64(PropInt64[i]);
            otUByte: sh.WriteUInt8(PropUInt8[i]);
            otUWord: sh.WriteUInt16(PropUInt16[i]);
            otULong: sh.WriteUInt32(PropUInt32[i]);
            otUQWord: sh.WriteUInt64(PropUInt64[i]);
          end;
        end;
        tkFloat:
        begin
          sh.WriteUInt8(UInt8(pd^.FloatType));
          case pd^.FloatType of
            ftSingle: sh.WriteFloat(PropFloat[i]);
            else sh.WriteDouble(PropDouble[i]);
          end;
        end;
        tkAString:
        begin
          sh.WriteString(PropString[i]);
        end;
        tkClass:
        begin
          Obj := PropClass[i];
          if Assigned(Obj) and (Obj is TUSerializable) then
          begin
            sh.WriteBool(True);
            TUSerializable(Obj).SerializeTo(Stream);
          end
          else
          begin
            sh.WriteBool(False);
          end;
        end;
        tkDynArray:
        begin
          pi := pd^.ElType;
          if not Assigned(pi) then pi := pd^.ElType2;
          pde := GetTypeData(pi);
          al := PropArrayLength[i];
          sh.WriteUInt8(UInt8(pi^.Kind));
          sh.WriteInt32(pd^.elSize);
          sh.WriteInt32(al);
          if al > 0 then
          begin
            case pi^.Kind of
              tkAString:
              begin
                for ai := 0 to al - 1 do
                begin
                  sh.WriteString(PropArrayString[i, ai]);
                end;
              end;
              tkClass:
              begin
                if pde^.ClassType.InheritsFrom(TUSerializable) then
                begin
                  sh.WriteBool(True);
                  for ai := 0 to al - 1 do
                  begin
                    Obj := PropArrayClass[i, ai];
                    if Assigned(Obj) then
                    begin
                      sh.WriteBool(True);
                      TUSerializable(PropArrayClass[i, ai]).SerializeTo(Stream);
                    end
                    else
                    begin
                      sh.WriteBool(False);
                    end;
                  end;
                end
                else
                begin
                  sh.WriteBool(False);
                end;
              end
              else sh.WriteBuffer(GetArrayData(i), al * pd^.elSize);
            end;
          end;
        end;
        else begin end;
      end;
      s := sh.Position - sp;
      sh.PosPush;
      sh.Position := sp;
      sh.WriteInt32(s);
      sh.PosPop;
    end;
  finally
    sh.Free;
  end;
end;

procedure TUSerializable.SerializeTo(const FileName: String);
  var fs: TFileStream;
begin
  fs := TFileStream.Create(FileName, fmCreate);
  try
    SerializeTo(fs);
  finally
    fs.Free;
  end;
end;

procedure TUSerializable.SerializeFrom(const Stream: TStream);
  var sh: TUStreamHelper;
  var i, n, s, j, al, aes, ai: Integer;
  var Name: String;
  var Kind, ArrKind: TTypeKind;
  var OrdType: TOrdType;
  var FloatType: TFloatType;
  var pd, pde: PTypeData;
  var pi: PTypeInfo;
  var Obj: TObject;
begin
  sh := TUStreamHelper.Create(Stream);
  try
    n := sh.ReadInt32;
    for j := 0 to n - 1 do
    begin
      Name := sh.ReadString;
      Kind := TTypeKind(sh.ReadUInt8);
      s := sh.ReadInt32;
      i := FindProp(Name, [Kind]);
      if i = -1 then
      begin
        sh.Skip(s);
        Continue;
      end;
      pd := GetTypeData(_PropList[i]^.PropType);
      case Kind of
        tkEnumeration:
        begin
          PropEnum[i] := sh.ReadInt32;
        end;
        tkBool:
        begin
          PropBool[i] := sh.ReadBool;
        end;
        tkInteger, tkInt64, tkQWord:
        begin
          OrdType := TOrdType(sh.ReadUInt8);
          case OrdType of
            otSByte: PropInt8[i] := sh.ReadInt8;
            otSWord: PropInt16[i] := sh.ReadInt16;
            otSLong: PropInt32[i] := sh.ReadInt32;
            otSQWord: PropInt64[i] := sh.ReadInt64;
            otUByte: PropUInt8[i] := sh.ReadUInt8;
            otUWord: PropUInt16[i] := sh.ReadUInt16;
            otULong: PropUInt32[i] := sh.ReadUInt32;
            otUQWord: PropUInt64[i] := sh.ReadUInt64;
          end;
        end;
        tkFloat:
        begin
          FloatType := TFloatType(sh.ReadUInt8);
          case FloatType of
            ftSingle: PropFloat[i] := sh.ReadFloat;
            else PropDouble[i] := sh.ReadDouble;
          end;
        end;
        tkAString:
        begin
          PropString[i] := sh.ReadString;
        end;
        tkClass:
        begin
          if sh.ReadBool then
          begin
            Obj := PropClass[i];
            if Assigned(Obj) and (Obj is TUSerializable) then
            begin
              TUSerializable(Obj).SerializeFrom(Stream);
            end
            else
            begin
              sh.Skip(s);
            end;
          end;
        end;
        tkDynArray:
        begin
          sh.PosPush;
          pi := pd^.ElType;
          if not Assigned(pi) then pi := pd^.ElType2;
          pde := GetTypeData(pi);
          ArrKind := TTypeKind(sh.ReadUInt8);
          aes := sh.ReadInt32;
          al := sh.ReadInt32;
          if ((ArrKind = pi^.Kind) and (aes = pd^.elSize)) then
          begin
            PropArrayLength[i] := al;
            if al > 0 then
            begin
              case ArrKind of
                tkAString:
                begin
                  for ai := 0 to al - 1 do
                  begin
                    PropArrayString[i, ai] := sh.ReadString;
                  end;
                end;
                tkClass:
                begin
                  if sh.ReadBool then
                  begin
                    if pde^.ClassType.InheritsFrom(TUSerializable) then
                    begin
                      for ai := 0 to al - 1 do
                      if sh.ReadBool then
                      begin
                        TUSerializable(PropArrayClass[i, ai]).SerializeFrom(Stream);
                      end;
                    end
                    else
                    begin
                      sh.PosPop;
                      sh.Skip(s);
                      sh.PosPush;
                    end;
                  end;
                end
                else sh.ReadBuffer(GetArrayData(i), al * aes);
              end;
            end;
            sh.PosDiscard;
          end
          else
          begin
            sh.PosPop;
            sh.Skip(s);
          end;
        end;
        else begin end;
      end;
    end;
  finally
    sh.Free;
  end;
end;

procedure TUSerializable.SerializeFrom(const FileName: String);
  var fs: TFileStream;
begin
  fs := TFileStream.Create(FileName, fmOpenRead);
  try
    SerializeFrom(fs);
  finally
    fs.Free;
  end;
end;

procedure TUSerializable.Assign(const Serializable: TUSerializable);
  var i, j, n, a: Integer;
  var pi, pj: PPropInfo;
  var tedj: PTypeData;
  var tei, tej: PTypeInfo;
begin
  for i := 0 to Serializable.PropCount - 1 do
  begin
    pi := Serializable.PropInfo[i];
    j := FindProp(pi^.Name, [pi^.PropType^.Kind]);
    if j = -1 then Continue;
    pj := PropInfo[j];
    case pj^.PropType^.Kind of
      tkEnumeration, tkBool, tkInteger, tkInt64, tkQWord:
      begin
        SetOrdProp(Self, pj, GetOrdProp(Serializable, pi));
      end;
      tkFloat:
      begin
        SetFloatProp(Self, pj, GetFloatProp(Serializable, pi));
      end;
      tkAString:
      begin
        PropString[j] := Serializable.PropString[i];
      end;
      tkClass:
      begin
        if (PropClass[j] is TUSerializable)
        and (Serializable.PropClass[i] is TUSerializable) then
        begin
          TUSerializable(PropClass[j]).Assign(
            TUSerializable(Serializable.PropClass[i])
          );
        end;
      end;
      tkDynArray:
      begin
        tei := Serializable.PropArrayInfo[i];
        tej := PropArrayInfo[j];
        if tei^.Kind = tej^.Kind then
        begin
          n := Serializable.PropArrayLength[i];
          PropArrayLength[j] := n;
          for a := 0 to n - 1 do
          begin
            case tej^.Kind of
              tkEnumeration: PropArrayEnum[j, a] := Serializable.PropArrayEnum[i, a];
              tkBool: PropArrayBool[j, a] := Serializable.PropArrayBool[i, a];
              tkInteger, tkInt64, tkQWord:
              begin
                tedj := GetTypeData(tej);
                case tedj^.OrdType of
                  otSByte: PropArrayInt8[j, a] := Serializable.PropArrayInt8[i, a];
                  otSWord: PropArrayInt16[j, a] := Serializable.PropArrayInt16[i, a];
                  otSLong: PropArrayInt32[j, a] := Serializable.PropArrayInt32[i, a];
                  otSQWord: PropArrayInt64[j, a] := Serializable.PropArrayInt64[i, a];
                  otUByte: PropArrayUInt8[j, a] := Serializable.PropArrayUInt8[i, a];
                  otUWord: PropArrayUInt16[j, a] := Serializable.PropArrayUInt16[i, a];
                  otULong: PropArrayUInt32[j, a] := Serializable.PropArrayUInt32[i, a];
                  otUQWord: PropArrayUInt64[j, a] := Serializable.PropArrayUInt64[i, a];
                end;
              end;
              tkFloat:
              begin
                tedj := GetTypeData(tej);
                case tedj^.FloatType of
                  ftSingle: PropArrayFloat[j, a] := Serializable.PropArrayFloat[i, a];
                  else PropArrayDouble[j, a] := Serializable.PropArrayDouble[i, a];
                end;
              end;
              tkAString: PropArrayString[j, a] := Serializable.PropArrayString[i, a];
              tkClass:
              begin
                if (PropArrayClass[j, a] is TUSerializable)
                and (Serializable.PropArrayClass[i, a] is TUSerializable) then
                begin
                  TUSerializable(PropArrayClass[j, a]).Assign(
                    TUSerializable(Serializable.PropArrayClass[i, a])
                  );
                end;
              end;
              else begin end;
            end;
          end;
        end;
      end;
      else begin end;
    end;
  end;
end;

procedure TUSerializable.Dump(const Offset: String);
  var i, j: Integer;
  var td, tde: PTypeData;
  var tie: PTypeInfo;
  var ss: String;
  var Obj: TObject;
  var nl: Boolean;
  function FindEnumName(const Names: ShortString; const Index: Integer): ShortString;
    var i, j, n: Integer;
  begin
    i := 0;
    j := 0;
    while (i < Index) and (j < SizeOf(ShortString)) do
    begin
      n := Ord(Names[j]);
      Inc(j, n + 1);
      Inc(i);
    end;
    if j < SizeOf(ShortString) then Exit(PShortString(@Names[j])^);
    Result := '';
  end;
begin
  if Length(Offset) = 0 then
  begin
    WriteLn('RTTI Dump for ' + _TypeInfo^.Name);
  end;
  for i := 0 to High(_PropList) do
  begin
    nl := True;
    Write(Offset, _PropList[i]^.Name + ': ');
    Write(_PropList[i]^.PropType^.Kind, ' ');
    td := GetTypeData(_PropList[i]^.PropType);
    if (_PropList[i]^.PropType^.Kind in [tkInteger, tkChar, tkEnumeration, tkBool, tkWChar, tkSet, tkInt64, tkQWord]) then
    begin
      Write('OrdType = ', td^.OrdType, '; ');
      case _PropList[i]^.PropType^.Kind of
        tkEnumeration:
        begin
          if Assigned(td^.BaseTypeRef) then
          begin
            Write(Offset, 'BaseTypeRef = ', td^.BaseTypeRef^^.Name, '; ');
          end
          else
          begin
            Write(Offset);
          end;
          Write('MinValue = ', td^.MinValue, '; ');
          WriteLn('MaxValue = ', td^.MaxValue, '; ');
          Write(Offset, '  NameList = ');
          for j := 0 to td^.MaxValue - td^.MinValue do
          begin
            ss := FindEnumName(td^.NameList, j);
            Write(ss, '; ');
          end;
          Write('Value = ', FindEnumName(td^.NameList, PropEnum[i]));
        end;
        tkBool:
        begin
          Write('Value = ', PropBool[i]);
        end;
        else
        begin
          case td^.OrdType of
            otSByte: Write('Value = ', PropInt8[i]);
            otSWord: Write('Value = ', PropInt16[i]);
            otSLong: Write('Value = ', PropInt32[i]);
            otSQWord: Write('Value = ', PropInt64[i]);
            otUByte: Write('Value = ', PropUInt8[i]);
            otUWord: Write('Value = ', PropUInt16[i]);
            otULong: Write('Value = ', PropUInt32[i]);
            otUQWord: Write('Value = ', PropUInt64[i]);
          end;
        end;
      end;
    end
    else if (_PropList[i]^.PropType^.Kind in [tkFloat]) then
    begin
      Write('FloatType = ', td^.FloatType, '; ');
      Write('Value = ', PropFloat[i]);
    end
    else if (_PropList[i]^.PropType^.Kind in [tkAString]) then
    begin
      Write('CodePage = ', td^.CodePage, '; ');
      Write('Value = ', PropString[i]);
    end
    else if (_PropList[i]^.PropType^.Kind in [tkClass]) then
    begin
      Obj := PropClass[i];
      if Assigned(Obj) and (Obj is TUSerializable) then
      begin
        WriteLn(_PropList[i]^.PropType^.Name);
        TUSerializable(Obj).Dump(Offset + '  ');
        nl := False;
      end;
    end
    else if (_PropList[i]^.PropType^.Kind in [tkDynArray]) then
    begin
      Write('ElSize = ', td^.elSize);
      if (Assigned(td^.ElType)) then Write(' ElType = ', td^.ElType^.Name);
      if (Assigned(td^.ElType2)) then Write(' ElType2 = ', td^.ElType2^.Name);
      Write(' VarType = ', td^.varType);
      WriteLn(' DynUnitName = ', td^.DynUnitName);
      WriteLn(Offset, 'Length = ', PropArrayLength[i], '; Values: [');
      tie := td^.ElType;
      if not Assigned(tie) then tie := td^.ElType2;
      tde := GetTypeData(tie);
      for j := 0 to PropArrayLength[i] - 1 do
      begin
        case tie^.Kind of
          tkEnumeration:
          begin
            WriteLn(Offset, '  ', FindEnumName(tde^.NameList, PropArrayEnum[i, j]));
          end;
          tkBool:
          begin
            WriteLn(Offset, '  ', PropArrayBool[i, j]);
          end;
          tkInteger, tkInt64, tkQWord:
          begin
            case tde^.OrdType of
              otSByte: WriteLn(Offset, '  ', PropArrayInt8[i, j]);
              otSWord: WriteLn(Offset, '  ', PropArrayInt16[i, j]);
              otSLong: WriteLn(Offset, '  ', PropArrayInt32[i, j]);
              otSQWord: WriteLn(Offset, '  ', PropArrayInt64[i, j]);
              otUByte: WriteLn(Offset, '  ', PropArrayUInt8[i, j]);
              otUWord: WriteLn(Offset, '  ', PropArrayUInt16[i, j]);
              otULong: WriteLn(Offset, '  ', PropArrayUInt32[i, j]);
              otUQWord: WriteLn(Offset, '  ', PropArrayUInt64[i, j]);
            end;
          end;
          tkFloat:
          begin
            case tde^.FloatType of
              ftSingle: WriteLn(Offset, '  ', PropArrayFloat[i, j]);
              else WriteLn(Offset, '  ', PropArrayDouble[i, j]);
            end;
          end;
          tkAString:
          begin
            WriteLn(Offset, '  ', PropArrayString[i, j]);
          end;
          tkClass:
          begin
            if tde^.ClassType.InheritsFrom(TUSerializable) then
            begin
              WriteLn(Offset + '  {');
              TUSerializable(PropArrayClass[i, j]).Dump(Offset + '    ');
              WriteLn(Offset + '  }');
            end;
          end;
          else
          begin
            WriteLn(Offset, 'Unhandled property');
          end;
        end;
      end;
      Write(Offset, ']');
    end;
    if nl then WriteLn(Offset);
  end;
end;
// TUSerializable end

// TUParserToken begin
class operator TUParserToken.= (a, b: TUParserToken): Boolean;
begin
  Result := (a.Value = b.Value) and (a.TokenType = b.TokenType);
end;

class operator TUParserToken.in (a: TUParserToken; b: array of TUParserToken): Boolean;
  var i: Integer;
begin
  for i := 0 to High(b) do if a = b[i] then Exit(True);
  Result := False;
end;

class operator TUParserToken.= (a: TUParserToken; b: String): Boolean;
begin
  Result := a.Value = b;
end;

class operator TUParserToken.= (a: TUParserToken; b: array of String): Boolean;
  var i: Integer;
begin
  for i := 0 to High(b) do if a.Value = b[i] then Exit(True);
  Result := False;
end;

class operator TUParserToken.= (a: TUParserToken; b: TUTokenType): Boolean;
begin
  Result := a.TokenType = b;
end;

class operator TUParserToken.in (a: TUParserToken; b: TUTokenTypes): Boolean;
begin
  Result := a.TokenType in b;
end;

class operator TUParserToken.:= (a: TUParserToken): String;
begin
  Result := a.Value;
end;

function UParserToken(const Value: String; const TokenType: TUTokenType): TUParserToken;
begin
  Result.Value := Value;
  Result.TokenType := TokenType;
end;
// TUParserToken end

// TUParserSyntax begin
procedure TUParserSyntax.AddComment(const ACommentStart, ACommentEnd: String);
begin
  SetLength(Comment, Length(Comment) + 1);
  Comment[High(Comment)][0] := ACommentStart;
  Comment[High(Comment)][1] := ACommentEnd;
end;

procedure TUParserSyntax.AddCommentLine(const ACommentLine: String);
begin
  SetLength(CommentLine, Length(CommentLine) + 1);
  CommentLine[High(CommentLine)] := ACommentLine;
end;

procedure TUParserSyntax.AddString(const AStringStartEnd: String);
begin
  SetLength(Strings, Length(Strings) + 1);
  Strings[High(Strings)] := AStringStartEnd;
end;

procedure TUParserSyntax.AddSymbol(const ASymbol: String);
begin
  SetLength(Symbols, Length(Symbols) + 1);
  Symbols[High(Symbols)] := ASymbol;
end;

procedure TUParserSyntax.AddSymbols(const ASymbols: array of String);
  var i, n: Int32;
begin
  n := Length(Symbols);
  SetLength(Symbols, Length(Symbols) + Length(ASymbols));
  for i := 0 to High(ASymbols) do Symbols[n + i] := ASymbols[i];
end;

procedure TUParserSyntax.AddKeyword(const AKeyword: String);
begin
  SetLength(Keywords, Length(Keywords) + 1);
  Keywords[High(Keywords)] := AKeyword;
end;

procedure TUParserSyntax.AddKeywords(const AKeywords: array of String);
  var i, n: Int32;
begin
  n := Length(Keywords);
  SetLength(Keywords, Length(Keywords) + Length(AKeywords));
  for i := 0 to High(AKeywords) do Keywords[n + i] := AKeywords[i];
end;

procedure TUParserSyntax.Reset;
begin
  SetLength(Comment, 0);
  SetLength(CommentLine, 0);
  SetLength(Strings, 0);
  SetLength(Symbols, 0);
  SetLength(Keywords, 0);
  CaseSensitive := False;
end;
// TUParserSyntax end

// TUParser begin
function TUParser.GetText: String;
begin
  Result := '';
  SetLength(Result, Length(_Text));
  Move(_Text[0], Result[1], Length(_Text));
end;

function TUParser.GetComment(const Index: Int32): String;
begin
  Result := _DefaultSyntax.Comment[index][0] + _DefaultSyntax.comment[index][1];
end;

function TUParser.GetCommentCount: Int32;
begin
  Result := Length(_DefaultSyntax.Comment);
end;

function TUParser.GetCommentLine(const index: Int32): String;
begin
  Result := _DefaultSyntax.CommentLine[index];
end;

function TUParser.GetCommentLineCount: Int32;
begin
  Result := Length(_DefaultSyntax.CommentLine);
end;

function TUParser.GetKeyword(const Index: Int32): String;
begin
  Result := _DefaultSyntax.Keywords[Index];
end;

function TUParser.GetKeywordCount: Int32;
begin
  Result := Length(_DefaultSyntax.Keywords);
end;

function TUParser.GetString(const Index: Int32): String;
begin
  Result := _DefaultSyntax.Strings[Index];
end;

function TUParser.GetStringCount: Int32;
begin
  Result := Length(_DefaultSyntax.Strings);
end;

function TUParser.GetSymbol(const Index: Int32): String;
begin
  Result := _DefaultSyntax.Symbols[Index];
end;

function TUParser.GetSymbolCount: Int32;
begin
  Result := Length(_DefaultSyntax.Symbols);
end;

function TUParser.GetTextLength: Int32;
begin
  Result := Length(_Text);
end;

procedure TUParser.SetSyntax(const Value: PUParserSyntax);
begin
  if Value = nil then _Syntax := @_DefaultSyntax else _Syntax := Value;
end;

constructor TUParser.Create;
begin
  inherited Create;
  _Position := 0;
  _Text := nil;
  _DefaultSyntax.CaseSensitive := False;
  _Syntax := @_DefaultSyntax;
end;

constructor TUParser.Create(const ParseText: String; const CaseSensitive: Boolean);
begin
  inherited Create;
  _DefaultSyntax.CaseSensitive := CaseSensitive;
  _Syntax := @_DefaultSyntax;
  Parse(ParseText);
end;

destructor TUParser.Destroy;
begin
  inherited Destroy;
end;

procedure TUParser.Parse(const ParseText: String);
begin
  if Length(_Text) <> Length(ParseText) then
  SetLength(_Text, Length(ParseText));
  Move(ParseText[1], _Text[0], Length(ParseText));
  _Position := 0;
  _Line := 0;
end;

procedure TUParser.AddComment(const CommentStart, CommentEnd: String);
begin
  _DefaultSyntax.AddComment(CommentStart, CommentEnd);
end;

procedure TUParser.AddCommentLine(const CommentLine: String);
begin
  _DefaultSyntax.AddCommentLine(CommentLine);
end;

procedure TUParser.AddString(const StringStartEnd: String);
begin
  _DefaultSyntax.AddString(StringStartEnd);
end;

procedure TUParser.AddSymbol(const Symbol: String);
begin
  _DefaultSyntax.AddSymbol(Symbol);
end;

procedure TUParser.AddKeyword(const Keyword: String);
begin
  _DefaultSyntax.AddKeyword(Keyword);
end;

procedure TUParser.SkipSpaces;
begin
  while (_Position < TextLength)
  and (
    (_Text[_Position] in [#1..#32, #$EF, #$BB, #$BF])
  ) do
  begin
    if IsAtNewLine then Inc(_Line);
    Inc(_Position);
  end;
end;

procedure TUParser.StatePush;
begin
  SetLength(_States, Length(_States) + 1);
  _States[High(_States)].Line := _Line;
  _States[High(_States)].Position := _Position;
end;

procedure TUParser.StatePop;
begin
  if Length(_States) < 1 then Exit;
  StateLoad;
  StateDiscard;
end;

procedure TUParser.StateDiscard;
begin
  if Length(_States) < 1 then Exit;
  SetLength(_States, Length(_States) - 1);
end;

procedure TUParser.StateLoad;
begin
  if Length(_States) < 1 then Exit;
  _Line := _States[High(_States)].Line;
  _Position := _States[High(_States)].Position;
end;

procedure TUParser.SyntaxPush;
begin
  specialize UArrAppend<PUParserSyntax>(_SyntaxStack, Syntax);
end;

procedure TUParser.SyntaxPop;
begin
  specialize UArrPop<PUParserSyntax>(_SyntaxStack);
end;

function TUParser.Read(const Count: Int32): String;
  var c: Int32;
begin
  if Count + _Position > TextLength then
  c := TextLength - _Position
  else
  c := Count;
  Result := '';
  SetLength(Result, c);
  Move(_Text[_Position], Result[1], c);
  Inc(_Position, c);
end;

function TUParser.Read(const Pos: Int32; const Count: Int32): String;
  var c: Int32;
begin
  if Count + Pos > TextLength then
  c := TextLength - Pos
  else
  c := Count;
  if c <= 0 then
  begin
    Result := '';
    Exit;
  end;
  SetLength(Result, c);
  Move(_Text[Pos], Result[1], c);
end;

function TUParser.IsAtSymbol: Int32;
  var i, j: Int32;
  var Match: Boolean;
begin
  for i := 0 to High(_Syntax^.Symbols) do
  begin
    Match := True;
    if _Position + Length(_Syntax^.Symbols[i]) - 1 > High(_Text) then
    begin
      Match := False;
    end
    else
    begin
      for j := 0 to Length(_Syntax^.Symbols[i]) - 1 do
      if (_Syntax^.CaseSensitive and (_Text[_Position + j] <> _Syntax^.Symbols[i][j + 1]))
      or (not _Syntax^.CaseSensitive and (LowerCase(_Text[_Position + j]) <> LowerCase(_Syntax^.Symbols[i][j + 1]))) then
      begin
        Match := false;
        Break;
      end;
    end;
    if Match then
    begin
      Result := i;
      Exit;
    end;
  end;
  Result := -1;
end;

function TUParser.IsAtKeyword: Int32;
  var i, j: Int32;
  var Match: Boolean;
begin
  for i := 0 to High(_Syntax^.Keywords) do
  begin
    Match := True;
    for j := 0 to Length(_Syntax^.Keywords[i]) - 1 do
    if (_Syntax^.CaseSensitive and (_Text[_Position + j] <> _Syntax^.Keywords[i][j + 1]))
    or (not _Syntax^.CaseSensitive and (LowerCase(_Text[_Position + j]) <> LowerCase(_Syntax^.Keywords[i][j + 1]))) then
    begin
      Match := False;
      Break;
    end;
    if Match then
    begin
      Result := i;
      Exit;
    end;
  end;
  Result := -1;
end;

function TUParser.IsAtCommentLine: Int32;
  var i, j: Int32;
  var Match: Boolean;
begin
  for i := 0 to High(_Syntax^.CommentLine) do
  begin
    Match := True;
    for j := 0 to Length(_Syntax^.CommentLine[i]) - 1 do
    if (_Syntax^.CaseSensitive and (_Text[_Position + j] <> _Syntax^.CommentLine[i][j + 1]))
    or (not _Syntax^.CaseSensitive and (LowerCase(_Text[_Position + j]) <> LowerCase(_Syntax^.CommentLine[i][j + 1]))) then
    begin
      Match := False;
      Break;
    end;
    if Match then
    begin
      Result := i;
      Exit;
    end;
  end;
  Result := -1;
end;

function TUParser.IsAtCommentStart: Int32;
  var i, j: Int32;
  var Match: Boolean;
begin
  for i := 0 to High(_Syntax^.Comment) do
  begin
    Match := True;
    for j := 0 to Length(_Syntax^.Comment[i][0]) - 1 do
    if (_Syntax^.CaseSensitive and (_Text[_Position + j] <> _Syntax^.Comment[i][0][j + 1]))
    or (not _Syntax^.CaseSensitive and (LowerCase(_Text[_Position + j]) <> LowerCase(_Syntax^.Comment[i][0][j + 1]))) then
    begin
      Match := False;
      Break;
    end;
    if Match then
    begin
      Result := i;
      Exit;
    end;
  end;
  Result := -1;
end;

function TUParser.IsAtCommentEnd: Int32;
  var i, j: Int32;
  var Match: Boolean;
begin
  for i := 0 to High(_Syntax^.Comment) do
  begin
    Match := True;
    for j := 0 to Length(_Syntax^.Comment[i][1]) - 1 do
    if (_Syntax^.CaseSensitive and (_Text[_Position + j] <> _Syntax^.Comment[i][1][j + 1]))
    or (not _Syntax^.CaseSensitive and (LowerCase(_Text[_Position + j]) <> LowerCase(_Syntax^.Comment[i][1][j + 1]))) then
    begin
      Match := False;
      Break;
    end;
    if Match then
    begin
      Result := i;
      Exit;
    end;
  end;
  Result := -1;
end;

function TUParser.IsAtString: Int32;
  var i, j: Int32;
  var Match: Boolean;
begin
  for i := 0 to High(_Syntax^.Strings) do
  begin
    Match := True;
    for j := 0 to Length(_Syntax^.Strings[i]) - 1 do
    if (_Syntax^.CaseSensitive and (_Text[_Position + j] <> _Syntax^.Strings[i][j + 1]))
    or (not _Syntax^.CaseSensitive and (LowerCase(_Text[_Position + j]) <> LowerCase(_Syntax^.Strings[i][j + 1]))) then
    begin
      Match := False;
      Break;
    end;
    if Match then
    begin
      Result := i;
      Exit;
    end;
  end;
  Result := -1;
end;

function TUParser.IsAtEoF: Boolean;
begin
  Result := _Position >= TextLength;
end;

function TUParser.IsAtNewLine: Boolean;
begin
  Result := _Text[_Position] = #$A;
end;

function TUParser.NextToken: TUParserToken;
begin
  Result.Value := NextToken(Result.TokenType);
end;

function TUParser.NextToken(out TokenType: TUTokenType): String;
  var IndArr: array[0..4 * 2 - 1] of Integer;
  function FilterIndex(const Size: Integer): Boolean;
    var i: Integer;
  begin
    for i := 0 to Length(IndArr) div 2 - 1 do
    if IndArr[i * 2 + 1] < Size then
    begin
      IndArr[i * 2 + 0] := -1;
      IndArr[i * 2 + 1] := 0;
    end
    else
    begin
      Exit(False);
    end;
    Result := True;
  end;
  var IndComment: Integer absolute IndArr[0 * 2];
  var IndCommentLine: Integer absolute IndArr[1 * 2];
  var IndString: Integer absolute IndArr[2 * 2];
  var IndSymbol: Integer absolute IndArr[3 * 2];
  var i, j: Int32;
  var b: Boolean;
begin
  while True do
  begin
    Result := '';
    TokenType := tt_eof;
    SkipSpaces;
    if _Position >= TextLength then Exit;
    for i := 0 to Length(IndArr) div 2 - 1 do
    begin
      IndArr[i * 2 + 0] := -1;
      IndArr[i * 2 + 1] := 0;
    end;
    i := IsAtCommentStart;
    if i > -1 then
    begin
      j := Length(_Syntax^.Comment[i][0]);
      if FilterIndex(j) then
      begin
        IndComment := i;
        PIntegerArray(@IndComment)^[1] := j;
      end;
    end;
    i := IsAtCommentLine;
    if i > -1 then
    begin
      j := Length(_Syntax^.CommentLine[i]);
      if FilterIndex(j) then
      begin
        IndCommentLine := i;
        PIntegerArray(@IndCommentLine)^[1] := j;
      end;
    end;
    i := IsAtString;
    if i > -1 then
    begin
      j := Length(_Syntax^.Strings[i]);
      if FilterIndex(j) then
      begin
        IndString := i;
        PIntegerArray(@IndString)^[1] := j;
      end;
    end;
    i := IsAtSymbol;
    if i > -1 then
    begin
      j := Length(_Syntax^.Symbols[i]);
      if FilterIndex(j) then
      begin
        IndSymbol := i;
        PIntegerArray(@IndSymbol)^[1] := j;
      end;
    end;
    //Comment
    if IndComment > -1 then
    begin
      i := IndComment;
      Inc(_Position, Length(_Syntax^.Comment[i][0]));
      while (_Position < TextLength - Length(_Syntax^.Comment[i][1]))
      and (IsAtCommentEnd <> i) do
      begin
        Inc(_Position);
        if IsAtNewLine then Inc(_Line);
      end;
      Inc(_Position, Length(_Syntax^.Comment[i][1]));
      Continue;
    end;
    //CommentLine
    if IndCommentLine > -1 then
    begin
      i := IndCommentLine;
      Inc(_Position, Length(_Syntax^.CommentLine[i]));
      while (_Position < TextLength)
      and (_Text[_Position] <> #$D)
      and (_Text[_Position] <> #$A) do
      Inc(_Position);
      Inc(_Line);
      Continue;
    end;
    //String
    if IndString > -1 then
    begin
      i := IndString;
      TokenType := tt_string;
      Inc(_Position, Length(_Syntax^.Strings[i]));
      while (_Position <= TextLength - Length(_Syntax^.Strings[i]))
      and (IsAtString <> i) do
      begin
        Result := Result + _Text[_Position];
        Inc(_Position);
      end;
      if _Position <= TextLength - Length(_Syntax^.Strings[i]) then
      Inc(_Position, Length(_Syntax^.Strings[i]));
      Exit;
    end;
    //Symbol
    if IndSymbol > -1 then
    begin
      i := IndSymbol;
      TokenType := tt_symbol;
      Result := _Syntax^.Symbols[i];
      Inc(_Position, Length(_Syntax^.Symbols[i]));
      Exit;
    end;
    b := True;
    while b do
    begin
      Result := Result + _Text[_Position];
      Inc(_Position);
      if _Position >= Length(_Text) then b := False;
      if b and (
        (_Text[_Position] = ' ')
        or (_Text[_Position] = #$D)
        or (_Text[_Position] = #$A)
      ) then b := False;
      if b then
      begin
        b := (
          b and (IsAtSymbol = -1)
          and (IsAtCommentStart = -1)
          and (IsAtCommentLine = -1)
        );
      end;
    end;
    if Length(Result) > 0 then
    begin
      if UStrIsNumber(Result) then
      begin
        TokenType := tt_number;
        Exit;
      end;
      for i := 0 to High(_Syntax^.Keywords) do
      if LowerCase(_Syntax^.Keywords[i]) = LowerCase(Result) then
      begin
        TokenType := tt_keyword;
        Result := _Syntax^.Keywords[i];
        Exit;
      end;
      TokenType := tt_word;
    end;
    Exit;
  end;
end;

function TUParser.CheckToken: TUParserToken;
begin
  StatePush;
  try
    Result := NextToken;
  finally
    StatePop;
  end;
end;

function TUParser.OptionalToken(const Token: String): Boolean;
  var t: TUParserToken;
begin
  Result := False;
  StatePush;
  try
    t := NextToken;
    Result := t = Token;
  finally
    if Result then StateDiscard
    else StatePop;
  end;
end;
// TUParser end

// TUShortStringReader begin
procedure TUShortStringReader.Setup(const Str: ShortString);
begin
  _Ptr := @Str[0];
end;

function TUShortStringReader.ReadShortString: String;
begin
  Result := '';
  SetLength(Result, specialize Read<UInt8>());
  UMove(Result[1], _Ptr^, Length(Result));
  Inc(_Ptr, Length(Result));
end;

generic function TUShortStringReader.Read<T>: T;
begin
  UMove(Result, _Ptr^, SizeOf(Result));
  Inc(_Ptr, SizeOf(Result));
end;
// TUShortStringReader end

// TUMap begin
function TUMap.GetValue(const Index: Int32): TValue;
begin
  Result := FindValueByIndex(Index);
end;

function TUMap.GetKey(const Index: Int32): TKey;
begin
  if (Index < 0) or (Index > High(_Items)) then Exit(Default(TKey));
  Result := _Items[Index].Key;
end;

function TUMap.Add(const Key: TKey; const Value: TValue): Int32;
  var l, h, m: Int32;
begin
  l := 0;
  h := High(_Items);
  while l <= h do
  begin
    m := (l + h) shr 1;
    if _Items[m].Key < Key then
    begin
      l := m + 1;
    end
    else
    begin
      h := m - 1;
    end;
  end;
  if l > High(_Items) then
  begin
    SetLength(_Items, Length(_Items) + 1);
    _Items[l].Key := Key;
    _Items[l].Value := Value;
  end
  else if _Items[l].Key = Key then
  begin
    _Items[l].Value := Value;
  end
  else
  begin
    SetLength(_Items, Length(_Items) + 1);
    for m := High(_Items) downto l + 1 do
    begin
      _Items[m] := _Items[m - 1];
    end;
    _Items[l].Key := Key;
    _Items[l].Value := Value;
  end;
  Result := l;
end;

function TUMap.HasKey(const Key: TKey): Boolean;
begin
  Result := FindIndexByKey(Key) > -1;
end;

procedure TUMap.RemoveByKey(const Key: TKey);
  var i: Int32;
begin
  i := FindIndexByKey(Key);
  if i = -1 then Exit;
  RemoveByIndex(i);
end;

procedure TUMap.RemoveByValue(const Value: TValue);
  var i: Int32;
begin
  i := FindIndexByValue(Value);
  if i = -1 then Exit;
  RemoveByIndex(i);
end;

procedure TUMap.RemoveByIndex(const Index: Int32);
begin
  specialize UArrDelete<TItem>(_Items, Index);
end;

function TUMap.FindIndexByKey(const Key: TKey): Int32;
  var l, h, m: Int32;
begin
  l := 0;
  h := High(_Items);
  while l <= h do
  begin
    m := (l + h) shr 1;
    if _Items[m].Key = Key then Exit(m);
    if _Items[m].Key < Key then
    begin
      l := m + 1;
    end
    else
    begin
      h := m - 1;
    end;
  end;
  if (l < Length(_Items)) and (_Items[l].Key = Key) then Exit(l);
  Result := -1;
end;

function TUMap.FindIndexByValue(const Value: TValue): Int32;
  var i: Int32;
begin
  for i := 0 to High(_Items) do
  if _Items[i].Value = Value then Exit(i);
  Result := -1;
end;

function TUMap.FindValueByKey(const Key: TKey): TValue;
  var i: Int32;
begin
  i := FindIndexByKey(Key);
  if i = -1 then Exit(Default(TValue));
  Result := _Items[i].Value;
end;

function TUMap.FindValueByIndex(const Index: Int32): TValue;
begin
  if (Index < 0) or (Index > High(_Items)) then Exit(Default(TValue));
  Result := _Items[Index].Value;
end;

function TUMap.Count: Int32;
begin
  Result := Length(_Items);
end;

procedure TUMap.Clear;
begin
  _Items := nil;
end;
// TUMap end

// TUXML begin
function TUXML.TEnumerator.GetCurrent: TUXML;
begin
  if (i = -1) or (i >= n.ChildCount) then Exit(nil);
  Result := n[i];
end;

constructor TUXML.TEnumerator.Create(const Node: TUXML);
begin
  n := Node;
  i := -1;
end;

function TUXML.TEnumerator.MoveNext: Boolean;
begin
  if i < n.ChildCount then Inc(i);
  Result := i < n.ChildCount;
end;

function TUXML.ReadXML(const p: TUParser): Boolean;
  var t: TUParserToken;
  var an, av, tc: String;
  var cn: TUXML;
begin
  p.SyntaxPush;
  p.Syntax := @_SyntaxTags;
  try
    Result := False;
    t := p.NextToken;
    if t <> '<' then Exit;
    t := p.NextToken;
    if t <> tt_word then Exit;
    _Name := LowerCase(t);
    repeat
      t := p.NextToken;
      if t = tt_word then
      begin
        an := t;
        if p.NextToken <> '=' then Exit;
        t := p.NextToken;
        if not (t in [tt_word, tt_string, tt_number]) then Exit;
        av := t;
        AddAttribute(an, av);
      end
      else if t = '>' then
      begin
        tc := '';
        p.SyntaxPush;
        p.Syntax := @_SyntaxContent;
        try
          repeat
            p.StatePush;
            t := p.NextToken;
            if t = tt_eof then Exit;
            if (Length(tc) > 0) and (t = ['<', '</']) then
            begin
              cn := TUXML.Create('');
              cn.Content := tc;
              SetLength(_Children, Length(_Children) + 1);
              _Children[High(_Children)] := cn;
              tc := '';
            end;
            if t = '</' then
            begin
              p.StateDiscard;
              p.Syntax := @_SyntaxTags;
              t := p.NextToken;
              if LowerCase(t.Value) <> _Name then Exit;
              if p.NextToken <> '>' then Exit;
              Exit(True);
            end
            else if t = '<' then
            begin
              p.StatePop;
              cn := TUXML.Create('');
              if not cn.ReadXML(p) then
              begin
                FreeAndNil(cn);
                Exit;
              end;
              SetLength(_Children, Length(_Children) + 1);
              _Children[High(_Children)] := cn;
            end
            else
            begin
              p.StateDiscard;
              if Length(tc) > 0 then tc += ' ';
              tc += t.Value;
            end;
          until t = tt_eof;
        finally
          p.SyntaxPop;
        end;
      end
      else if t = '/>' then
      begin
        Exit(True);
      end
      else
      begin
        Exit;
      end;
    until t = tt_eof;
  finally
    p.SyntaxPop;
  end;
end;

function TUXML.WriteXML(const Offset: String): String;
  var i: Integer;
begin
  if IsPlainText then
  begin
    Result := Offset + _Content;
    Exit;
  end;
  Result := Offset + '<' + _Name;
  for i := 0 to High(_Attributes) do
  begin
    Result += ' ' + _Attributes[i].Name + '="' + _Attributes[i].Value + '"';
  end;
  if Length(_Children) = 0 then
  begin
    Result += '/>';
    Exit;
  end;
  Result += '>'#$D#$A;
  for i := 0 to High(_Children) do
  begin
    Result += _Children[i].WriteXML(Offset + #9) + #$D#$A;
  end;
  Result += Offset + '</' + _Name + '>';
end;

function TUXML.GetContent: String;
  var i: Integer;
begin
  Result := _Content;
  for i := 0 to High(_Children) do
  begin
    if i > 0 then Result += ' ';
    Result += _Children[i].Content;
  end;
end;

function TUXML.GetAttribute(const Index: Integer): TAttribute;
begin
  Result := _Attributes[Index];
end;

function TUXML.GetAttributeValue(const AttName: String): String;
  var a: TAttribute;
begin
  a := FindAttribute(AttName);
  if not Assigned(a) then Exit('');
  Result := a.Value;
end;

function TUXML.GetAttributeCount: Integer;
begin
  Result := Length(_Attributes);
end;

function TUXML.GetChild(const Index: Integer): TUXML;
begin
  Result := _Children[Index];
end;

function TUXML.GetChildCount: Integer;
begin
  Result := Length(_Children);
end;

function TUXML.GetChildContent(const NodeName: String): String;
  var c: TUXML;
begin
  c := FindChild(NodeName);
  if not Assigned(c) then Exit('');
  Result := c.Content;
end;

function TUXML.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

class constructor TUXML.CreateClass;
begin
  with _SyntaxTags do
  begin
    AddComment('<!--', '-->');
    AddComment('<?', '?>');
    AddString('"');
    AddSymbol('</');
    AddSymbol('/>');
    AddSymbol('<');
    AddSymbol('>');
    AddSymbol('=');
  end;
  with _SyntaxContent do
  begin
    AddComment('<!--', '-->');
    AddComment('<?', '?>');
    AddSymbol('</');
    AddSymbol('<');
  end;
end;

constructor TUXML.Create(const NodeName: String);
begin
  _Name := NodeName;
end;

destructor TUXML.Destroy;
begin
  specialize UArrClear<TAttribute>(_Attributes);
  specialize UArrClear<TUXML>(_Children);
  inherited Destroy;
end;

function TUXML.IsPlainText: Boolean;
begin
  Result := Length(_Name) = 0;
end;

procedure TUXML.AddAttribute(const AttName, AttValue: String);
  var att: TAttribute;
begin
  att := TAttribute.Create;
  att.Name := AttName;
  att.Value := AttValue;
  specialize UArrAppend<TAttribute>(_Attributes, att);
end;

function TUXML.FindAttribute(const AttName: String): TAttribute;
  var i: Integer;
begin
  for i := 0 to High(_Attributes) do
  if _Attributes[i].Name = AttName then
  begin
    Exit(_Attributes[i]);
  end;
  Result := nil;
end;

function TUXML.FindChild(const NodeName: String): TUXML;
  var i: Integer;
begin
  for i := 0 to High(_Children) do
  if _Children[i].Name = NodeName then
  begin
    Exit(_Children[i]);
  end;
  Result := nil;
end;

class function TUXML.Load(const XML: String): TUXML;
  var p: TUParser;
begin
  Result := nil;
  p := TUParser.Create(XML);
  p.Syntax := @_SyntaxTags;
  try
    Result := TUXML.Create('');
    if not Result.ReadXML(p) then FreeAndNil(Result);
    if not Assigned(Result) then WriteLn('Error, line: ', p.Line);
  finally
    p.Free;
  end;
end;

class function TUXML.Load(const Stream: TStream): TUXML;
  var s: String;
begin
  s := '';
  SetLength(s, Stream.Size);
  Stream.Read(s[1], Stream.Size);
  Result := Load(s);
end;

class function TUXML.LoadFromFile(const FileName: String): TUXML;
  var fs: TFileStream;
begin
  try
    fs := TFileStream.Create(FileName, fmOpenRead);
    Result := Load(fs);
  finally
    fs.Free;
  end;
end;

function TUXML.Save: String;
begin
  Result := '<?xml version="1.0" encoding="UTF-8"?>'#$D#$A + WriteXML;
end;

procedure TUXML.Save(const Stream: TStream);
  var s: String;
begin
  s := Save;
  Stream.Write(s[1], Length(s));
end;

procedure TUXML.SaveToFile(const FileName: String);
  var fs: TFileStream;
begin
  fs := TFileStream.Create(FileName, fmCreate);
  try
    Save(fs);
  finally
    fs.Free;
  end;
end;
// TUXML end

// TUJson begin
function TUJson.TEnumerator.GetCurrent: TUJson;
begin
  if (i = -1) or (i >= n.Count) then Exit(nil);
  Result := n.Element[i];
end;

constructor TUJson.TEnumerator.Create(const Node: TUJson);
begin
  n := Node;
  i := -1;
end;

function TUJson.TEnumerator.MoveNext: Boolean;
begin
  if i < n.Count then Inc(i);
  Result := i < n.Count;
end;

function TUJson.ReadJson(const p: TUParser): Boolean;
  var t: TUParserToken;
  var NamedNode: TNamedNode;
  var StrArr: TUStrArr;
  var Json: TUJson;
begin
  Result := False;
  t := p.NextToken;
  if t.TokenType in [tt_string, tt_word, tt_number] then
  begin
    NodeType := nt_value;
    if (t.TokenType = tt_number) and (p.CheckToken = '.') then
    begin
      _Value := t;
      _Value += p.NextToken.Value;
      t := p.NextToken;
      if t <> tt_number then
      begin
        StrArr := UStrExplode(t.Value, 'E');
        if Length(StrArr) < 2 then Exit;
        if not UStrIsNumber(StrArr[0])
        or not UStrIsNumber(StrArr[1]) then Exit;
      end;
      _Value += t.Value;
    end
    else
    begin
      _Value := t;
    end;
  end
  else if t = tt_keyword then
  begin
    NodeType := nt_object;
    _Value := LowerCase(t.Value);
  end
  else if t = '{' then
  begin
    NodeType := nt_object;
    repeat
      t := p.NextToken;
      if t = ',' then t := p.NextToken;
      if t = '}' then Break;
      if not (t in [tt_string, tt_word]) then Exit;
      NamedNode.Name := t;
      t := p.NextToken;
      if not (t = ':') then Exit;
      NamedNode.Node := TUJson.Create;
      if not NamedNode.Node.ReadJson(p) then
      begin
        FreeAndNil(NamedNode.Node);
        Exit;
      end;
      specialize UArrAppend<TNamedNode>(_Content, NamedNode);
    until t = tt_eof;
  end
  else if t = '[' then
  begin
    NodeType := nt_array;
    repeat
      t := p.CheckToken;
      if t = ',' then t := p.NextToken;
      if t = ']' then
      begin
        t := p.NextToken;
        Break;
      end;
      Json := TUJson.Create;
      if not Json.ReadJson(p) then
      begin
        FreeAndNil(Json);
        Exit;
      end;
      specialize UArrAppend<TUJson>(_Elements, Json);
    until t = tt_eof;
  end
  else
  begin
    Exit;
  end;
  Result := True;
end;

procedure TUJson.SetNodeType(const Value: TNodeType);
  var i: Int32;
begin
  if _NodeType = Value then Exit;
  if (_NodeType = nt_object) then
  begin
    for i := 0 to High(_Content) do
    begin
      FreeAndNil(_Content[i].Node);
    end;
    _Content := nil;
  end
  else if (_NodeType =  nt_array) then
  begin
    specialize UArrClear<TUJson>(_Elements);
  end;
  _NodeType := Value;
  if (_NodeType = nt_object) then _Value := '';
end;

function TUJson.GetValue: String;
  var i: Int32;
begin
  case _NodeType of
    nt_value: Result := _Value;
    nt_object:
    begin
      if Length(_Value) > 0 then
      begin
        Result := _Value;
      end
      else
      begin
        Result := '{';
        for i := 0 to High(_Content) do
        begin
          Result += '"' + _Content[i].Name + '":';
          if _Content[i].Node.IsSingleValue
          and not _Content[i].Node.IsNumber then
          begin
            Result += '"' + _Content[i].Node.Value + '"';
          end
          else
          begin
            Result += _Content[i].Node.Value;
          end;
          if i < High(_Content) then Result += ',';
        end;
        Result += '}';
      end;
    end;
    nt_array:
    begin
      Result := '[';
      for i := 0 to High(_Elements) do
      begin
        Result += _Elements[i].Value;
        if i < High(_Elements) then Result += ',';
      end;
      Result += ']';
    end;
    else Result := _Value;
  end;
end;

function TUJson.GetContent(const Key: String): TUJson;
  var i: Int32;
begin
  if _NodeType <> nt_object then Exit(nil);
  for i := 0 to High(_Content) do
  if _Content[i].Name = Key then
  begin
    Exit(_Content[i].Node);
  end;
  Result := nil;
end;

function TUJson.GetName(const Index: Int32): String;
begin
  case _NodeType of
    nt_object:
    begin
      if (Index < 0) or (Index > High(_Content)) then Exit('');
      Result := _Content[Index].Name;
    end;
    nt_array:
    begin
      if (Index < 0) or (Index > High(_Elements)) then Exit('');
      Result := IntToStr(Index);
    end;
    else Result := '';
  end;
end;

function TUJson.GetElement(const Index: Int32): TUJson;
begin
  case _NodeType of
    nt_array:
    begin
      if (Index < 0) or (Index > High(_Elements)) then Exit(nil);
      Result := _Elements[Index];
    end;
    nt_object:
    begin
      if (Index < 0) or (Index > High(_Content)) then Exit(nil);
      Result := _Content[Index].Node;
    end;
    else Result := nil;
  end;
end;

function TUJson.GetCount: Int32;
begin
  case _NodeType of
    nt_object: Result := Length(_Content);
    nt_array: Result := Length(_Elements);
    else Result := 0;
  end;
end;

function TUJson.GetIsSingleValue: Boolean;
begin
  Result := _NodeType = nt_value;
end;

function TUJson.GetIsObject: Boolean;
begin
  Result := _NodeType = nt_object;
end;

function TUJson.GetIsArray: Boolean;
begin
  Result := _NodeType = nt_array;
end;

function TUJson.GetIsNumber: Boolean;
begin
  Result := (_NodeType = nt_value) and UStrIsNumber(_Value);
end;

function TUJson.GetIsNull: Boolean;
begin
  Result := (_NodeType = nt_object) and (Length(_Value) > 0);
end;

function TUJson.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

function TUJson.FormatJson(const Offset: String): String;
  var i: Int32;
begin
  case _NodeType of
    nt_value: Result := _Value;
    nt_object:
    begin
      if Length(_Value) > 0 then
      begin
        Result := _Value;
      end
      else
      begin
        Result := #$D#$A + Offset + '{';
        for i := 0 to High(_Content) do
        begin
          Result += #$D#$A + Offset + '  "' + _Content[i].Name + '":';
          if _Content[i].Node.IsSingleValue
          and not _Content[i].Node.IsNumber then
          begin
            Result += '"' + _Content[i].Node.Value + '"';
          end
          else
          begin
            Result += _Content[i].Node.FormatJson(Offset + '  ');
          end;
          if i < High(_Content) then Result += ',';
        end;
        Result += #$D#$A + Offset + '}';
      end;
    end;
    nt_array:
    begin
      Result := #$D#$A + Offset + '[';
      for i := 0 to High(_Elements) do
      begin
        Result += _Elements[i].FormatJson(Offset + '  ');
        if i < High(_Elements) then Result += ',';
      end;
      Result += #$D#$A + Offset + ']';
    end;
    else Result := '';
  end;
end;

class constructor TUJson.CreateClass;
begin
  with _Syntax do
  begin
    AddSymbols(['{', '}', '[', ']', ':', ',', '.']);
    AddString('"');
    AddKeywords(['null', 'undefined']);
  end;
end;

constructor TUJson.Create;
begin
  _NodeType := nt_invalid;
end;

destructor TUJson.Destroy;
begin
  NodeType := nt_invalid;
  inherited Destroy;
end;

class function TUJson.Load(const Json: String): TUJson;
  var p: TUParser;
begin
  Result := nil;
  p := TUParser.Create(Json);
  p.Syntax := @_Syntax;
  try
    Result := TUJson.Create;
    if not Result.ReadJson(p) then FreeAndNil(Result);
    if not Assigned(Result) then WriteLn('Error, line: ', p.Line);
  finally
    p.Free;
  end;
end;

class function TUJson.Load(const Stream: TStream): TUJson;
  var s: String;
begin
  s := '';
  SetLength(s, Stream.Size);
  Stream.Read(s[1], Stream.Size);
  Result := Load(s);
end;

class function TUJson.LoadFromFile(const FileName: String): TUJson;
  var fs: TFileStream;
begin
  try
    fs := TFileStream.Create(FileName, fmOpenRead);
    Result := Load(fs);
  finally
    fs.Free;
  end;
end;

function TUJson.Save: String;
begin
  Result := Value;
end;

procedure TUJson.Save(const Stream: TStream);
  var s: String;
begin
  s := Save;
  Stream.Write(s[1], Length(s));
end;

procedure TUJson.SaveToFile(const FileName: String);
  var fs: TFileStream;
begin
  fs := TFileStream.Create(FileName, fmCreate);
  try
    Save(fs);
  finally
    fs.Free;
  end;
end;
// TUJson end

// Functions begin
procedure UClear(out x; const Size: UInt32);
begin
{$push}{$hints off}FillChar(x, Size, 0);{$pop}
end;

procedure UMove(out Dest; const Src; const Size: UInt32);
begin
{$push}{$hints off}Move(Src, Dest, Size);{$pop}
end;

function USignOf(const v: Int64): Int64;
begin
  if v < 0 then Result := -1 else Result := 1;
end;

function UIntToPtr(const i: PtrUInt): Pointer;
begin
  Result := nil; Result += i;
end;

function UCopyVarRec(constref src: TVarRec): TVarRec;
var
  ws: WideString;
begin
  Result := src;
  case src.VType of
    vtExtended:
    begin
      New(Result.VExtended);
      Result.VExtended^ := src.VExtended^;
    end;
    vtString:
    begin
      GetMem(Result.VString, Length(src.VString^) + 1);
      Result.VString^ := src.VString^;
    end;
    vtPChar: Result.VPChar := StrNew(src.VPChar);
    vtPWideChar:
    begin
      ws := src.VPWideChar;
      GetMem(Result.VPWideChar, (Length(ws) + 1) * SizeOf(WideChar));
      Move(PWideChar(ws)^, Result.VPWideChar^, (Length(ws) + 1) * SizeOf(WideChar));
    end;
    vtAnsiString:
    begin
      Result.VAnsiString := nil;
      AnsiString(Result.VAnsiString) := AnsiString(src.VAnsiString);
    end;
    vtCurrency:
    begin
      New(Result.VCurrency);
      Result.VCurrency^ := src.VCurrency^;
    end;
    vtVariant:
    begin
      New(Result.VVariant);
      Result.VVariant^ := src.VVariant^;
    end;
    vtInterface:
    begin
      Result.VInterface := nil;
      IInterface(Result.VInterface) := IInterface(src.VInterface);
    end;
    vtWideString:
    begin
      Result.VWideString := nil;
      WideString(Result.VWideString) := WideString(src.VWideString);
    end;
    vtInt64:
    begin
      New(Result.VInt64);
      Result.VInt64^ := src.VInt64^;
    end;
    vtUnicodeString:
    begin
      Result.VUnicodeString := nil;
      UnicodeString(Result.VUnicodeString) := UnicodeString(src.VUnicodeString);
    end;
  end;
end;

function UCopyVarRecArr(constref src: array of TVarRec): TUVarRecArr;
  var i: Integer;
begin
  Result := nil;
  SetLength(Result, Length(src));
  for i := 0 to High(src) do
  begin
    Result[i] := UCopyVarRec(src[i]);
  end;
end;

procedure UFinalizeVarRec(var vr: TVarRec);
begin
  case vr.VType of
    vtExtended:
      Dispose(vr.VExtended);
    vtString:
      Dispose(vr.VString);
    vtPChar:
      StrDispose(vr.VPChar);
    vtPWideChar:
      FreeMem(vr.VPWideChar);
    vtAnsiString:
      AnsiString(vr.VAnsiString) := '';
    vtCurrency:
      Dispose(vr.VCurrency);
    vtVariant:
      Dispose(vr.VVariant);
    vtInterface:
      IInterface(vr.VInterface) := nil;
    vtWideString:
      WideString(vr.VWideString) := '';
    vtInt64:
      Dispose(vr.VInt64);
    vtUnicodeString:
      UnicodeString(vr.VUnicodeString) := '';
  end;
  vr.VInteger := 0;
end;

procedure UFinalizeVarRecArr(var arr: array of TVarRec);
var
  i: Integer;
begin
  for i := Low(arr) to High(arr) do
  begin
    UFinalizeVarRec(arr[i]);
  end;
end;

function UIntToBool(const i: Integer): Boolean;
begin
  Result := i <> 0;
end;

function UBoolToInt(const b: Boolean): Integer;
begin
  if b then Exit(1) else Exit(0);
end;

function UBoolToStr(const b: Boolean): String;
begin
  if b then Exit('True') else Exit('False');
end;

generic function UMin<T>(const a, b: T): T;
begin
  if a < b then Exit(a) else Exit(b);
end;

function UMin(const a, b: Int8): Int8;
begin
  Result := specialize UMin<Int8>(a, b);
end;

function UMin(const a, b: Int16): Int16;
begin
  Result := specialize UMin<Int16>(a, b);
end;

function UMin(const a, b: Int32): Int32;
begin
  Result := specialize UMin<Int32>(a, b);
end;

function UMin(const a, b: Int64): Int64;
begin
  Result := specialize UMin<Int64>(a, b);
end;

function UMin(const a, b: UInt8): UInt8;
begin
  Result := specialize UMin<UInt8>(a, b);
end;

function UMin(const a, b: UInt16): UInt16;
begin
  Result := specialize UMin<UInt16>(a, b);
end;

function UMin(const a, b: UInt32): UInt32;
begin
  Result := specialize UMin<UInt32>(a, b);
end;

function UMin(const a, b: UInt64): UInt64;
begin
  Result := specialize UMin<UInt64>(a, b);
end;

function UMin(const a, b: TUFloat): TUFloat;
begin
  Result := specialize UMin<TUFloat>(a, b);
end;

function UMin(const a, b: TUDouble): TUDouble;
begin
  Result := specialize UMin<TUDouble>(a, b);
end;

function UMin(const a, b: TUVec2): TUVec2;
  var i: Int32;
begin
  for i := 0 to High(TUVec2) do Result[i] := UMin(a[i], b[i]);
end;

function UMin(const a, b: TUVec3): TUVec3;
  var i: Int32;
begin
  for i := 0 to High(TUVec3) do Result[i] := UMin(a[i], b[i]);
end;

function UMin(const a, b: TUVec4): TUVec4;
  var i: Int32;
begin
  for i := 0 to High(TUVec4) do Result[i] := UMin(a[i], b[i]);
end;

generic function UMax<T>(const a, b: T): T;
begin
  if a > b then Exit(a) else Exit(b);
end;

function UMax(const a, b: Int8): Int8;
begin
  Result := specialize UMax<Int8>(a, b);
end;

function UMax(const a, b: Int16): Int16;
begin
  Result := specialize UMax<Int16>(a, b);
end;

function UMax(const a, b: Int32): Int32; 
begin
  Result := specialize UMax<Int32>(a, b);
end;

function UMax(const a, b: Int64): Int64;
begin
  Result := specialize UMax<Int64>(a, b);
end;

function UMax(const a, b: UInt8): UInt8; 
begin
  Result := specialize UMax<UInt8>(a, b);
end;

function UMax(const a, b: UInt16): UInt16; 
begin
  Result := specialize UMax<UInt16>(a, b);
end;

function UMax(const a, b: UInt32): UInt32;   
begin
  Result := specialize UMax<UInt32>(a, b);
end;

function UMax(const a, b: UInt64): UInt64;  
begin
  Result := specialize UMax<UInt64>(a, b);
end;

function UMax(const a, b: TUFloat): TUFloat;   
begin
  Result := specialize UMax<TUFloat>(a, b);
end;

function UMax(const a, b: TUDouble): TUDouble; 
begin
  Result := specialize UMax<TUDouble>(a, b);
end;

function UMax(const a, b: TUVec2): TUVec2;
  var i: Int32;
begin
  for i := 0 to High(TUVec2) do Result[i] := UMax(a[i], b[i]);
end;

function UMax(const a, b: TUVec3): TUVec3;   
  var i: Int32;
begin
  for i := 0 to High(TUVec3) do Result[i] := UMax(a[i], b[i]);
end;

function UMax(const a, b: TUVec4): TUVec4;   
  var i: Int32;
begin
  for i := 0 to High(TUVec4) do Result[i] := UMax(a[i], b[i]);
end;

generic function UClamp<T>(const v, MinV, MaxV: T): T;
begin
  if v < MinV then Exit(MinV) else if v > MaxV then Exit(MaxV) else Exit(v);
end;

function UClamp(const v, MinV, MaxV: Int8): Int8;
begin
  Result := specialize UClamp<Int8>(v, MinV, MaxV);
end;

function UClamp(const v, MinV, MaxV: Int16): Int16;    
begin
  Result := specialize UClamp<Int16>(v, MinV, MaxV);
end;

function UClamp(const v, MinV, MaxV: Int32): Int32;   
begin
  Result := specialize UClamp<Int32>(v, MinV, MaxV);
end;

function UClamp(const v, MinV, MaxV: Int64): Int64;    
begin
  Result := specialize UClamp<Int64>(v, MinV, MaxV);
end;

function UClamp(const v, MinV, MaxV: UInt8): UInt8;  
begin
  Result := specialize UClamp<UInt8>(v, MinV, MaxV);
end;

function UClamp(const v, MinV, MaxV: UInt16): UInt16; 
begin
  Result := specialize UClamp<UInt16>(v, MinV, MaxV);
end;

function UClamp(const v, MinV, MaxV: UInt32): UInt32; 
begin
  Result := specialize UClamp<UInt32>(v, MinV, MaxV);
end;

function UClamp(const v, MinV, MaxV: UInt64): UInt64;    
begin
  Result := specialize UClamp<UInt64>(v, MinV, MaxV);
end;

function UClamp(const v, MinV, MaxV: TUFloat): TUFloat;    
begin
  Result := specialize UClamp<TUFloat>(v, MinV, MaxV);
end;

function UClamp(const v, MinV, MaxV: TUDouble): TUDouble; 
begin
  Result := specialize UClamp<TUDouble>(v, MinV, MaxV);
end;

function UClamp(const v, MinV, MaxV: TUVec2): TUVec2;
  var i: Int32;
begin
  for i := 0 to High(TUVec2) do Result[i] := UClamp(v[i], MinV[i], MaxV[i]);
end;

function UClamp(const v, MinV, MaxV: TUVec3): TUVec3; 
  var i: Int32;
begin
  for i := 0 to High(TUVec3) do Result[i] := UClamp(v[i], MinV[i], MaxV[i]);
end;

function UClamp(const v, MinV, MaxV: TUVec4): TUVec4;    
  var i: Int32;
begin
  for i := 0 to High(TUVec4) do Result[i] := UClamp(v[i], MinV[i], MaxV[i]);
end;

generic function ULerp<T>(const a, b: T; const s: TUFloat): T;
begin
  Result := a + (b - a) * s;
end;

function ULerp(const a, b: TUFloat; const s: TUFloat): TUFloat;
begin
  Result := a + (b - a) * s;
end;

function ULerp(const a, b: TUVec2; const s: TUFloat): TUVec2;
begin
  Result := a + (b - a) * s;
end;

function ULerp(const a, b: TUVec3; const s: TUFloat): TUVec3;
begin
  Result := a + (b - a) * s;
end;

function ULerp(const a, b: TUVec4; const s: TUFloat): TUVec4;
begin
  Result := a + (b - a) * s;
end;

function ULerp(const a, b: TUMat; const s: TUFloat): TUMat;
begin
  Result := a + (b - a) * s;
end;

function USmoothStep(const v, MinV, MaxV: TUFloat): TUFloat;
  var x: TUFloat absolute Result;
begin
  x := UClamp((v - MinV) / (MaxV - MinV), 0, 1);
  Result := x * x * (3 - 2 * x);
end;

generic function UBezier<T>(const f0, f1, f2, f3: T; const s: TUFloat): T;
  var s2, s3: TUFloat;
begin
  s2 := s * s;
  s3 := s2 * s;
  Result := s3 * f3 + (3 * s2 - 3 * s3) * f2 + (3 * s3 - 6 * s2 + 3 * s) * f1 + (3 * s2 - s3 - 3 * s + 1) * f0;
end;

function UBezier(const v0, v1, v2, v3: TUFloat; const s: TUFloat): TUFloat;
begin
  Result := specialize UBezier<TUFloat>(v0, v1, v2, v3, s);
end;

function UBezier(const v0, v1, v2, v3: TUDouble; const s: TUFloat): TUDouble;
begin
  Result := specialize UBezier<TUDouble>(v0, v1, v2, v3, s);
end;

function UBezier(const v0, v1, v2, v3: TUVec2; const s: TUFloat): TUVec2;
begin
  Result := specialize UBezier<TUVec2>(v0, v1, v2, v3, s);
end;

function UBezier(const v0, v1, v2, v3: TUVec3; const s: TUFloat): TUVec3;
begin
  Result := specialize UBezier<TUVec3>(v0, v1, v2, v3, s);
end;

function UBezier(const v0, v1, v2, v3: TUVec4; const s: TUFloat): TUVec4;
begin
  Result := specialize UBezier<TUVec4>(v0, v1, v2, v3, s);
end;

generic function UCatmullRom<T>(const v0, v1, v2, v3: T; const s: TUFloat): T;
  var s2, s3: TUFloat;
begin
  s2 := s * s;
  s3 := s2 * s;
  Result := 0.5 * (2 * v1 + (v2 - v0) * s + (2 * v0 - 5 * v1 + 4 * v2 - v3) * s2 + (v3 - 3 * v2 + 3 * v1 - v0) * s3);
end;

function UCatmullRom(const v0, v1, v2, v3: TUFloat; const s: TUFloat): TUFloat;
begin
  Result := specialize UCatmullRom<TUFloat>(v0, v1, v2, v3, s);
end;

function UCatmullRom(const v0, v1, v2, v3: TUDouble; const s: TUFloat): TUDouble;
begin
  Result := specialize UCatmullRom<TUDouble>(v0, v1, v2, v3, s);
end;

function UCatmullRom(const v0, v1, v2, v3: TUVec2; const s: TUFloat): TUVec2;
begin
  Result := specialize UCatmullRom<TUVec2>(v0, v1, v2, v3, s);
end;

function UCatmullRom(const v0, v1, v2, v3: TUVec3; const s: TUFloat): TUVec3;
begin
  Result := specialize UCatmullRom<TUVec3>(v0, v1, v2, v3, s);
end;

function UCatmullRom(const v0, v1, v2, v3: TUVec4; const s: TUFloat): TUVec4;
begin
  Result := specialize UCatmullRom<TUVec4>(v0, v1, v2, v3, s);
end;

generic function UEndianSwap<T>(const v: T): T;
  type TByteArr = array[0..SizeOf(T) - 1] of UInt8;
  var Src: TByteArr absolute v;
  var Dst: TByteArr absolute Result;
  var i: Int32;
begin
  for i := 0 to High(TByteArr) do
  Dst[i] := Src[High(TByteArr) - i];
end;

function UEndianSwap(const v: UInt16): UInt16;
begin
  Result := specialize UEndianSwap<UInt16>(v);
end;

function UEndianSwap(const v: UInt32): UInt32;
begin
  Result := specialize UEndianSwap<UInt32>(v);
end;

function UEndianSwap(const v: UInt64): UInt64;
begin
  Result := specialize UEndianSwap<UInt64>(v);
end;

generic procedure USwap<T>(var a: T; var b: T);
  var Temp: T;
begin
  Temp := a;
  a := b;
  b := Temp;
end;

procedure USwap(var a: Int8; var b: Int8);
begin
  specialize USwap<Int8>(a, b);
end;

procedure USwap(var a: Int16; var b: Int16); 
begin
  specialize USwap<Int16>(a, b);
end;

procedure USwap(var a: Int32; var b: Int32); 
begin
  specialize USwap<Int32>(a, b);
end;

procedure USwap(var a: Int64; var b: Int64);  
begin
  specialize USwap<Int64>(a, b);
end;

procedure USwap(var a: UInt8; var b: UInt8);   
begin
  specialize USwap<UInt8>(a, b);
end;

procedure USwap(var a: UInt16; var b: UInt16);  
begin
  specialize USwap<UInt16>(a, b);
end;

procedure USwap(var a: UInt32; var b: UInt32);
begin
  specialize USwap<UInt32>(a, b);
end;

procedure USwap(var a: UInt64; var b: UInt64);   
begin
  specialize USwap<UInt64>(a, b);
end;

procedure USwap(var a: TUFloat; var b: TUFloat); 
begin
  specialize USwap<TUFloat>(a, b);
end;

procedure USwap(var a: TUDouble; var b: TUDouble);
begin
  specialize USwap<TUDouble>(a, b);
end;

procedure USwap(var a: TUVec2; var b: TUVec2);    
begin
  specialize USwap<TUVec2>(a, b);
end;

procedure USwap(var a: TUVec3; var b: TUVec3);   
begin
  specialize USwap<TUVec3>(a, b);
end;

procedure USwap(var a: TUVec4; var b: TUVec4);  
begin
  specialize USwap<TUVec4>(a, b);
end;

procedure USwap(var a: TUMat; var b: TUMat); 
begin
  specialize USwap<TUMat>(a, b);
end;

generic function UEnumSetToStr<T>(const EnumSet: T): String;
  var i: Integer;
  var ti, eti: PTypeInfo;
  var td, etd: PTypeData;
begin
  ti := PTypeInfo(TypeInfo(EnumSet));
  case ti^.Kind of
    tkEnumeration: Result := GetEnumName(ti, PUInt64(@EnumSet)^);
    tkSet:
    begin
      td := GetTypeData(ti);
      eti := td^.CompTypeRef^;
      if not Assigned(eti) then Exit('');
      etd := GetTypeData(eti);
      Result := '[';
      for i := etd^.MinValue to etd^.MaxValue do
      begin
        if ((1 shl (i - etd^.MinValue)) and PUInt64(@EnumSet)^) > 0 then
        begin
          if Length(Result) > 1 then Result += ', ';
          Result += GetEnumName(eti, i);
        end;
      end;
      Result += ']';
    end;
    else Result := '';
  end;
end;

generic function USelect<T>(const Cond: Boolean; constref IfTrue: T; constref IfFalse: T): T;
begin
  if Cond then Exit(IfTrue) else Exit(IfFalse);
end;

function UCRC32(
  const CRC: UInt32;
  const Value: Pointer;
  const Count: UInt32
): UInt32;
  const CRC32Table: array[0..255] of UInt32 = (
    $00000000, $77073096, $ee0e612c, $990951ba, $076dc419, $706af48f, $e963a535,
    $9e6495a3, $0edb8832, $79dcb8a4, $e0d5e91e, $97d2d988, $09b64c2b, $7eb17cbd,
    $e7b82d07, $90bf1d91, $1db71064, $6ab020f2, $f3b97148, $84be41de, $1adad47d,
    $6ddde4eb, $f4d4b551, $83d385c7, $136c9856, $646ba8c0, $fd62f97a, $8a65c9ec,
    $14015c4f, $63066cd9, $fa0f3d63, $8d080df5, $3b6e20c8, $4c69105e, $d56041e4,
    $a2677172, $3c03e4d1, $4b04d447, $d20d85fd, $a50ab56b, $35b5a8fa, $42b2986c,
    $dbbbc9d6, $acbcf940, $32d86ce3, $45df5c75, $dcd60dcf, $abd13d59, $26d930ac,
    $51de003a, $c8d75180, $bfd06116, $21b4f4b5, $56b3c423, $cfba9599, $b8bda50f,
    $2802b89e, $5f058808, $c60cd9b2, $b10be924, $2f6f7c87, $58684c11, $c1611dab,
    $b6662d3d, $76dc4190, $01db7106, $98d220bc, $efd5102a, $71b18589, $06b6b51f,
    $9fbfe4a5, $e8b8d433, $7807c9a2, $0f00f934, $9609a88e, $e10e9818, $7f6a0dbb,
    $086d3d2d, $91646c97, $e6635c01, $6b6b51f4, $1c6c6162, $856530d8, $f262004e,
    $6c0695ed, $1b01a57b, $8208f4c1, $f50fc457, $65b0d9c6, $12b7e950, $8bbeb8ea,
    $fcb9887c, $62dd1ddf, $15da2d49, $8cd37cf3, $fbd44c65, $4db26158, $3ab551ce,
    $a3bc0074, $d4bb30e2, $4adfa541, $3dd895d7, $a4d1c46d, $d3d6f4fb, $4369e96a,
    $346ed9fc, $ad678846, $da60b8d0, $44042d73, $33031de5, $aa0a4c5f, $dd0d7cc9,
    $5005713c, $270241aa, $be0b1010, $c90c2086, $5768b525, $206f85b3, $b966d409,
    $ce61e49f, $5edef90e, $29d9c998, $b0d09822, $c7d7a8b4, $59b33d17, $2eb40d81,
    $b7bd5c3b, $c0ba6cad, $edb88320, $9abfb3b6, $03b6e20c, $74b1d29a, $ead54739,
    $9dd277af, $04db2615, $73dc1683, $e3630b12, $94643b84, $0d6d6a3e, $7a6a5aa8,
    $e40ecf0b, $9309ff9d, $0a00ae27, $7d079eb1, $f00f9344, $8708a3d2, $1e01f268,
    $6906c2fe, $f762575d, $806567cb, $196c3671, $6e6b06e7, $fed41b76, $89d32be0,
    $10da7a5a, $67dd4acc, $f9b9df6f, $8ebeeff9, $17b7be43, $60b08ed5, $d6d6a3e8,
    $a1d1937e, $38d8c2c4, $4fdff252, $d1bb67f1, $a6bc5767, $3fb506dd, $48b2364b,
    $d80d2bda, $af0a1b4c, $36034af6, $41047a60, $df60efc3, $a867df55, $316e8eef,
    $4669be79, $cb61b38c, $bc66831a, $256fd2a0, $5268e236, $cc0c7795, $bb0b4703,
    $220216b9, $5505262f, $c5ba3bbe, $b2bd0b28, $2bb45a92, $5cb36a04, $c2d7ffa7,
    $b5d0cf31, $2cd99e8b, $5bdeae1d, $9b64c2b0, $ec63f226, $756aa39c, $026d930a,
    $9c0906a9, $eb0e363f, $72076785, $05005713, $95bf4a82, $e2b87a14, $7bb12bae,
    $0cb61b38, $92d28e9b, $e5d5be0d, $7cdcefb7, $0bdbdf21, $86d3d2d4, $f1d4e242,
    $68ddb3f8, $1fda836e, $81be16cd, $f6b9265b, $6fb077e1, $18b74777, $88085ae6,
    $ff0f6a70, $66063bca, $11010b5c, $8f659eff, $f862ae69, $616bffd3, $166ccf45,
    $a00ae278, $d70dd2ee, $4e048354, $3903b3c2, $a7672661, $d06016f7, $4969474d,
    $3e6e77db, $aed16a4a, $d9d65adc, $40df0b66, $37d83bf0, $a9bcae53, $debb9ec5,
    $47b2cf7f, $30b5ffe9, $bdbdf21c, $cabac28a, $53b39330, $24b4a3a6, $bad03605,
    $cdd70693, $54de5729, $23d967bf, $b3667a2e, $c4614ab8, $5d681b02, $2a6f2b94,
    $b40bbe37, $c30c8ea1, $5a05df1b, $2d02ef8d
  );
  var i: Int32;
  var pb: PUInt8Arr absolute Value;
begin
  Result := CRC xor $ffffffff;
  for i := 0 to Count - 1 do
  begin
    Result := ((Result shr 8) and $00ffffff) xor CRC32Table[(Result xor pb^[i]) and $ff];
  end;
  Result := Result xor $ffffffff;
end;

function UCRC64(
  const CRC: UInt64;
  const Value: Pointer;
  const Count: UInt32
): UInt64;
  const CRC64Table: array[0..255] of UInt64 = (
    UInt64($0000000000000000), UInt64($7AD870C830358979), UInt64($F5B0E190606B12F2), UInt64($8F689158505E9B8B), UInt64($C038E5739841B68F), UInt64($BAE095BBA8743FF6),
    UInt64($358804E3F82AA47D), UInt64($4F50742BC81F2D04), UInt64($AB28ECB46814FE75), UInt64($D1F09C7C5821770C), UInt64($5E980D24087FEC87), UInt64($24407DEC384A65FE),
    UInt64($6B1009C7F05548FA), UInt64($11C8790FC060C183), UInt64($9EA0E857903E5A08), UInt64($E478989FA00BD371), UInt64($7D08FF3B88BE6F81), UInt64($07D08FF3B88BE6F8),
    UInt64($88B81EABE8D57D73), UInt64($F2606E63D8E0F40A), UInt64($BD301A4810FFD90E), UInt64($C7E86A8020CA5077), UInt64($4880FBD87094CBFC), UInt64($32588B1040A14285),
    UInt64($D620138FE0AA91F4), UInt64($ACF86347D09F188D), UInt64($2390F21F80C18306), UInt64($594882D7B0F40A7F), UInt64($1618F6FC78EB277B), UInt64($6CC0863448DEAE02),
    UInt64($E3A8176C18803589), UInt64($997067A428B5BCF0), UInt64($FA11FE77117CDF02), UInt64($80C98EBF2149567B), UInt64($0FA11FE77117CDF0), UInt64($75796F2F41224489),
    UInt64($3A291B04893D698D), UInt64($40F16BCCB908E0F4), UInt64($CF99FA94E9567B7F), UInt64($B5418A5CD963F206), UInt64($513912C379682177), UInt64($2BE1620B495DA80E),
    UInt64($A489F35319033385), UInt64($DE51839B2936BAFC), UInt64($9101F7B0E12997F8), UInt64($EBD98778D11C1E81), UInt64($64B116208142850A), UInt64($1E6966E8B1770C73),
    UInt64($8719014C99C2B083), UInt64($FDC17184A9F739FA), UInt64($72A9E0DCF9A9A271), UInt64($08719014C99C2B08), UInt64($4721E43F0183060C), UInt64($3DF994F731B68F75),
    UInt64($B29105AF61E814FE), UInt64($C849756751DD9D87), UInt64($2C31EDF8F1D64EF6), UInt64($56E99D30C1E3C78F), UInt64($D9810C6891BD5C04), UInt64($A3597CA0A188D57D),
    UInt64($EC09088B6997F879), UInt64($96D1784359A27100), UInt64($19B9E91B09FCEA8B), UInt64($636199D339C963F2), UInt64($DF7ADABD7A6E2D6F), UInt64($A5A2AA754A5BA416),
    UInt64($2ACA3B2D1A053F9D), UInt64($50124BE52A30B6E4), UInt64($1F423FCEE22F9BE0), UInt64($659A4F06D21A1299), UInt64($EAF2DE5E82448912), UInt64($902AAE96B271006B),
    UInt64($74523609127AD31A), UInt64($0E8A46C1224F5A63), UInt64($81E2D7997211C1E8), UInt64($FB3AA75142244891), UInt64($B46AD37A8A3B6595), UInt64($CEB2A3B2BA0EECEC),
    UInt64($41DA32EAEA507767), UInt64($3B024222DA65FE1E), UInt64($A2722586F2D042EE), UInt64($D8AA554EC2E5CB97), UInt64($57C2C41692BB501C), UInt64($2D1AB4DEA28ED965),
    UInt64($624AC0F56A91F461), UInt64($1892B03D5AA47D18), UInt64($97FA21650AFAE693), UInt64($ED2251AD3ACF6FEA), UInt64($095AC9329AC4BC9B), UInt64($7382B9FAAAF135E2),
    UInt64($FCEA28A2FAAFAE69), UInt64($8632586ACA9A2710), UInt64($C9622C4102850A14), UInt64($B3BA5C8932B0836D), UInt64($3CD2CDD162EE18E6), UInt64($460ABD1952DB919F),
    UInt64($256B24CA6B12F26D), UInt64($5FB354025B277B14), UInt64($D0DBC55A0B79E09F), UInt64($AA03B5923B4C69E6), UInt64($E553C1B9F35344E2), UInt64($9F8BB171C366CD9B),
    UInt64($10E3202993385610), UInt64($6A3B50E1A30DDF69), UInt64($8E43C87E03060C18), UInt64($F49BB8B633338561), UInt64($7BF329EE636D1EEA), UInt64($012B592653589793),
    UInt64($4E7B2D0D9B47BA97), UInt64($34A35DC5AB7233EE), UInt64($BBCBCC9DFB2CA865), UInt64($C113BC55CB19211C), UInt64($5863DBF1E3AC9DEC), UInt64($22BBAB39D3991495),
    UInt64($ADD33A6183C78F1E), UInt64($D70B4AA9B3F20667), UInt64($985B3E827BED2B63), UInt64($E2834E4A4BD8A21A), UInt64($6DEBDF121B863991), UInt64($1733AFDA2BB3B0E8),
    UInt64($F34B37458BB86399), UInt64($8993478DBB8DEAE0), UInt64($06FBD6D5EBD3716B), UInt64($7C23A61DDBE6F812), UInt64($3373D23613F9D516), UInt64($49ABA2FE23CC5C6F),
    UInt64($C6C333A67392C7E4), UInt64($BC1B436E43A74E9D), UInt64($95AC9329AC4BC9B5), UInt64($EF74E3E19C7E40CC), UInt64($601C72B9CC20DB47), UInt64($1AC40271FC15523E),
    UInt64($5594765A340A7F3A), UInt64($2F4C0692043FF643), UInt64($A02497CA54616DC8), UInt64($DAFCE7026454E4B1), UInt64($3E847F9DC45F37C0), UInt64($445C0F55F46ABEB9),
    UInt64($CB349E0DA4342532), UInt64($B1ECEEC59401AC4B), UInt64($FEBC9AEE5C1E814F), UInt64($8464EA266C2B0836), UInt64($0B0C7B7E3C7593BD), UInt64($71D40BB60C401AC4),
    UInt64($E8A46C1224F5A634), UInt64($927C1CDA14C02F4D), UInt64($1D148D82449EB4C6), UInt64($67CCFD4A74AB3DBF), UInt64($289C8961BCB410BB), UInt64($5244F9A98C8199C2),
    UInt64($DD2C68F1DCDF0249), UInt64($A7F41839ECEA8B30), UInt64($438C80A64CE15841), UInt64($3954F06E7CD4D138), UInt64($B63C61362C8A4AB3), UInt64($CCE411FE1CBFC3CA),
    UInt64($83B465D5D4A0EECE), UInt64($F96C151DE49567B7), UInt64($76048445B4CBFC3C), UInt64($0CDCF48D84FE7545), UInt64($6FBD6D5EBD3716B7), UInt64($15651D968D029FCE),
    UInt64($9A0D8CCEDD5C0445), UInt64($E0D5FC06ED698D3C), UInt64($AF85882D2576A038), UInt64($D55DF8E515432941), UInt64($5A3569BD451DB2CA), UInt64($20ED197575283BB3),
    UInt64($C49581EAD523E8C2), UInt64($BE4DF122E51661BB), UInt64($3125607AB548FA30), UInt64($4BFD10B2857D7349), UInt64($04AD64994D625E4D), UInt64($7E7514517D57D734),
    UInt64($F11D85092D094CBF), UInt64($8BC5F5C11D3CC5C6), UInt64($12B5926535897936), UInt64($686DE2AD05BCF04F), UInt64($E70573F555E26BC4), UInt64($9DDD033D65D7E2BD),
    UInt64($D28D7716ADC8CFB9), UInt64($A85507DE9DFD46C0), UInt64($273D9686CDA3DD4B), UInt64($5DE5E64EFD965432), UInt64($B99D7ED15D9D8743), UInt64($C3450E196DA80E3A),
    UInt64($4C2D9F413DF695B1), UInt64($36F5EF890DC31CC8), UInt64($79A59BA2C5DC31CC), UInt64($037DEB6AF5E9B8B5), UInt64($8C157A32A5B7233E), UInt64($F6CD0AFA9582AA47),
    UInt64($4AD64994D625E4DA), UInt64($300E395CE6106DA3), UInt64($BF66A804B64EF628), UInt64($C5BED8CC867B7F51), UInt64($8AEEACE74E645255), UInt64($F036DC2F7E51DB2C),
    UInt64($7F5E4D772E0F40A7), UInt64($05863DBF1E3AC9DE), UInt64($E1FEA520BE311AAF), UInt64($9B26D5E88E0493D6), UInt64($144E44B0DE5A085D), UInt64($6E963478EE6F8124),
    UInt64($21C640532670AC20), UInt64($5B1E309B16452559), UInt64($D476A1C3461BBED2), UInt64($AEAED10B762E37AB), UInt64($37DEB6AF5E9B8B5B), UInt64($4D06C6676EAE0222),
    UInt64($C26E573F3EF099A9), UInt64($B8B627F70EC510D0), UInt64($F7E653DCC6DA3DD4), UInt64($8D3E2314F6EFB4AD), UInt64($0256B24CA6B12F26), UInt64($788EC2849684A65F),
    UInt64($9CF65A1B368F752E), UInt64($E62E2AD306BAFC57), UInt64($6946BB8B56E467DC), UInt64($139ECB4366D1EEA5), UInt64($5CCEBF68AECEC3A1), UInt64($2616CFA09EFB4AD8),
    UInt64($A97E5EF8CEA5D153), UInt64($D3A62E30FE90582A), UInt64($B0C7B7E3C7593BD8), UInt64($CA1FC72BF76CB2A1), UInt64($45775673A732292A), UInt64($3FAF26BB9707A053),
    UInt64($70FF52905F188D57), UInt64($0A2722586F2D042E), UInt64($854FB3003F739FA5), UInt64($FF97C3C80F4616DC), UInt64($1BEF5B57AF4DC5AD), UInt64($61372B9F9F784CD4),
    UInt64($EE5FBAC7CF26D75F), UInt64($9487CA0FFF135E26), UInt64($DBD7BE24370C7322), UInt64($A10FCEEC0739FA5B), UInt64($2E675FB4576761D0), UInt64($54BF2F7C6752E8A9),
    UInt64($CDCF48D84FE75459), UInt64($B71738107FD2DD20), UInt64($387FA9482F8C46AB), UInt64($42A7D9801FB9CFD2), UInt64($0DF7ADABD7A6E2D6), UInt64($772FDD63E7936BAF),
    UInt64($F8474C3BB7CDF024), UInt64($829F3CF387F8795D), UInt64($66E7A46C27F3AA2C), UInt64($1C3FD4A417C62355), UInt64($935745FC4798B8DE), UInt64($E98F353477AD31A7),
    UInt64($A6DF411FBFB21CA3), UInt64($DC0731D78F8795DA), UInt64($536FA08FDFD90E51), UInt64($29B7D047EFEC8728)
  );
  var i: Int32;
  var pb: PUInt8Arr absolute Value;
begin
  Result := CRC xor UInt64($ffffffffffffffff);
  for i := 0 to Count - 1 do
  begin
    Result := ((Result shr 8) and UINt64($00ffffffffffffff)) xor CRC64Table[(Result xor pb^[i]) and $ff];
  end;
  Result := Result xor UInt64($ffffffffffffffff);
end;

procedure USinCos(const a: TUFloat; out s: TUFloat; out c: TUFloat);
begin
  s := Sin(a);
  c := Cos(a);
end;

function UCoTan(const x: TUFloat): TUFloat;
  var s, c: TUFloat;
begin
  USinCos(x, s, c);
  Result := c / s;
end;

function UArcCos(const x: TUFloat): TUFloat;
begin
  Result := UArcTan2(Sqrt((1 + x) * (1 - x)), x);
end;

function UArcTan2(const y, x: TUFloat): TUFloat;
begin
  if x = 0 then
  begin
    if y = 0 then Result := 0
    else if y > 0 then Result := UHalfPi
    else if y < 0 then Result := -UHalfPi;
  end
  else
  begin
    Result := ArcTan(y / x);
  end;
  if x < 0 then Result += UPi;
  if Result > pi then result -= UTwoPi;
end;

function UPow(const b, e: TUFloat): TUFloat;
begin
  Result := Exp(e * Ln(b));
end;

function UAddMat(const m0, m1: TUMat): TUMat;
begin
  Result[0, 0] := m0[0, 0] + m1[0, 0];
  Result[1, 0] := m0[1, 0] + m1[1, 0];
  Result[2, 0] := m0[2, 0] + m1[2, 0];
  Result[3, 0] := m0[3, 0] + m1[3, 0];
  Result[0, 1] := m0[0, 1] + m1[0, 1];
  Result[1, 1] := m0[1, 1] + m1[1, 1];
  Result[2, 1] := m0[2, 1] + m1[2, 1];
  Result[3, 1] := m0[3, 1] + m1[3, 1];
  Result[0, 2] := m0[0, 2] + m1[0, 2];
  Result[1, 2] := m0[1, 2] + m1[1, 2];
  Result[2, 2] := m0[2, 2] + m1[2, 2];
  Result[3, 2] := m0[3, 2] + m1[3, 2];
  Result[0, 3] := m0[0, 3] + m1[0, 3];
  Result[1, 3] := m0[1, 3] + m1[1, 3];
  Result[2, 3] := m0[2, 3] + m1[2, 3];
  Result[3, 3] := m0[3, 3] + m1[3, 3];
end;

function UAddMatFloat(const m: TUMat; const s: TUFloat): TUMat;
begin
  Result[0, 0] := m[0, 0] + s;
  Result[1, 0] := m[1, 0] + s;
  Result[2, 0] := m[2, 0] + s;
  Result[3, 0] := m[3, 0] + s;
  Result[0, 1] := m[0, 1] + s;
  Result[1, 1] := m[1, 1] + s;
  Result[2, 1] := m[2, 1] + s;
  Result[3, 1] := m[3, 1] + s;
  Result[0, 2] := m[0, 2] + s;
  Result[1, 2] := m[1, 2] + s;
  Result[2, 2] := m[2, 2] + s;
  Result[3, 2] := m[3, 2] + s;
  Result[0, 3] := m[0, 3] + s;
  Result[1, 3] := m[1, 3] + s;
  Result[2, 3] := m[2, 3] + s;
  Result[3, 3] := m[3, 3] + s;
end;

function USubMat(const m0, m1: TUMat): TUMat;
begin
  Result[0, 0] := m0[0, 0] - m1[0, 0];
  Result[1, 0] := m0[1, 0] - m1[1, 0];
  Result[2, 0] := m0[2, 0] - m1[2, 0];
  Result[3, 0] := m0[3, 0] - m1[3, 0];
  Result[0, 1] := m0[0, 1] - m1[0, 1];
  Result[1, 1] := m0[1, 1] - m1[1, 1];
  Result[2, 1] := m0[2, 1] - m1[2, 1];
  Result[3, 1] := m0[3, 1] - m1[3, 1];
  Result[0, 2] := m0[0, 2] - m1[0, 2];
  Result[1, 2] := m0[1, 2] - m1[1, 2];
  Result[2, 2] := m0[2, 2] - m1[2, 2];
  Result[3, 2] := m0[3, 2] - m1[3, 2];
  Result[0, 3] := m0[0, 3] - m1[0, 3];
  Result[1, 3] := m0[1, 3] - m1[1, 3];
  Result[2, 3] := m0[2, 3] - m1[2, 3];
  Result[3, 3] := m0[3, 3] - m1[3, 3];
end;

function USubMatFloat(const m: TUMat; const s: TUFloat): TUMat;
begin
  Result[0, 0] := m[0, 0] - s;
  Result[1, 0] := m[1, 0] - s;
  Result[2, 0] := m[2, 0] - s;
  Result[3, 0] := m[3, 0] - s;
  Result[0, 1] := m[0, 1] - s;
  Result[1, 1] := m[1, 1] - s;
  Result[2, 1] := m[2, 1] - s;
  Result[3, 1] := m[3, 1] - s;
  Result[0, 2] := m[0, 2] - s;
  Result[1, 2] := m[1, 2] - s;
  Result[2, 2] := m[2, 2] - s;
  Result[3, 2] := m[3, 2] - s;
  Result[0, 3] := m[0, 3] - s;
  Result[1, 3] := m[1, 3] - s;
  Result[2, 3] := m[2, 3] - s;
  Result[3, 3] := m[3, 3] - s;
end;

function UMulMat(const m0, m1: TUMat): TUMat;
begin
  Result[0, 0] := m0[0, 0] * m1[0, 0] + m0[0, 1] * m1[1, 0] + m0[0, 2] * m1[2, 0] + m0[0, 3] * m1[3, 0];
  Result[1, 0] := m0[1, 0] * m1[0, 0] + m0[1, 1] * m1[1, 0] + m0[1, 2] * m1[2, 0] + m0[1, 3] * m1[3, 0];
  Result[2, 0] := m0[2, 0] * m1[0, 0] + m0[2, 1] * m1[1, 0] + m0[2, 2] * m1[2, 0] + m0[2, 3] * m1[3, 0];
  Result[3, 0] := m0[3, 0] * m1[0, 0] + m0[3, 1] * m1[1, 0] + m0[3, 2] * m1[2, 0] + m0[3, 3] * m1[3, 0];
  Result[0, 1] := m0[0, 0] * m1[0, 1] + m0[0, 1] * m1[1, 1] + m0[0, 2] * m1[2, 1] + m0[0, 3] * m1[3, 1];
  Result[1, 1] := m0[1, 0] * m1[0, 1] + m0[1, 1] * m1[1, 1] + m0[1, 2] * m1[2, 1] + m0[1, 3] * m1[3, 1];
  Result[2, 1] := m0[2, 0] * m1[0, 1] + m0[2, 1] * m1[1, 1] + m0[2, 2] * m1[2, 1] + m0[2, 3] * m1[3, 1];
  Result[3, 1] := m0[3, 0] * m1[0, 1] + m0[3, 1] * m1[1, 1] + m0[3, 2] * m1[2, 1] + m0[3, 3] * m1[3, 1];
  Result[0, 2] := m0[0, 0] * m1[0, 2] + m0[0, 1] * m1[1, 2] + m0[0, 2] * m1[2, 2] + m0[0, 3] * m1[3, 2];
  Result[1, 2] := m0[1, 0] * m1[0, 2] + m0[1, 1] * m1[1, 2] + m0[1, 2] * m1[2, 2] + m0[1, 3] * m1[3, 2];
  Result[2, 2] := m0[2, 0] * m1[0, 2] + m0[2, 1] * m1[1, 2] + m0[2, 2] * m1[2, 2] + m0[2, 3] * m1[3, 2];
  Result[3, 2] := m0[3, 0] * m1[0, 2] + m0[3, 1] * m1[1, 2] + m0[3, 2] * m1[2, 2] + m0[3, 3] * m1[3, 2];
  Result[0, 3] := m0[0, 0] * m1[0, 3] + m0[0, 1] * m1[1, 3] + m0[0, 2] * m1[2, 3] + m0[0, 3] * m1[3, 3];
  Result[1, 3] := m0[1, 0] * m1[0, 3] + m0[1, 1] * m1[1, 3] + m0[1, 2] * m1[2, 3] + m0[1, 3] * m1[3, 3];
  Result[2, 3] := m0[2, 0] * m1[0, 3] + m0[2, 1] * m1[1, 3] + m0[2, 2] * m1[2, 3] + m0[2, 3] * m1[3, 3];
  Result[3, 3] := m0[3, 0] * m1[0, 3] + m0[3, 1] * m1[1, 3] + m0[3, 2] * m1[2, 3] + m0[3, 3] * m1[3, 3];
end;

function UMulMatFloat(const m: TUMat; const s: TUFloat): TUMat;
begin
  Result[0, 0] := m[0, 0] * s;
  Result[1, 0] := m[1, 0] * s;
  Result[2, 0] := m[2, 0] * s;
  Result[3, 0] := m[3, 0] * s;
  Result[0, 1] := m[0, 1] * s;
  Result[1, 1] := m[1, 1] * s;
  Result[2, 1] := m[2, 1] * s;
  Result[3, 1] := m[3, 1] * s;
  Result[0, 2] := m[0, 2] * s;
  Result[1, 2] := m[1, 2] * s;
  Result[2, 2] := m[2, 2] * s;
  Result[3, 2] := m[3, 2] * s;
  Result[0, 3] := m[0, 3] * s;
  Result[1, 3] := m[1, 3] * s;
  Result[2, 3] := m[2, 3] * s;
  Result[3, 3] := m[3, 3] * s;
end;

function UMulVec2Mat3x3(const v: TUVec2; const m: TUMat): TUVec2;
begin
  result := TUVec2.Make(
    v.x * m[0, 0] + v.y * m[1, 0],
    v.x * m[0, 1] + v.y * m[1, 1]
  );
end;

function UMulVec2Mat4x3(const v: TUVec2; const m: TUMat): TUVec2;
begin
  Result := TUVec2.Make(
    v.x * m[0, 0] + v.y * m[1, 0] + m[3, 0],
    v.x * m[0, 1] + v.y * m[1, 1] + m[3, 1]
  );
end;

function UMulVec2Mat4x4(const v: TUVec2; const m: TUMat): TUVec2;
  var w: TUFloat;
begin
  w := 1 / (v.x * m[0, 3] + v.y * m[1, 3] + m[3, 3]);
  Result := TUVec2.Make(
    (v.x * m[0, 0] + v.y * m[1, 0] + m[3, 0]) * w,
    (v.x * m[0, 1] + v.y * m[1, 1] + m[3, 1]) * w
  );
end;

function UMulVec3Mat3x3(const v: TUVec3; const m: TUMat): TUVec3;
begin
  Result := TUVec3.Make(
    v.x * m[0, 0] + v.y * m[1, 0] + v.z * m[2, 0],
    v.x * m[0, 1] + v.y * m[1, 1] + v.z * m[2, 1],
    v.x * m[0, 2] + v.y * m[1, 2] + v.z * m[2, 2]
  );
end;

function UMulVec3Mat4x3(const v: TUVec3; const m: TUMat): TUVec3;
begin
  Result := TUVec3.Make(
    v.x * m[0, 0] + v.y * m[1, 0] + v.z * m[2, 0] + m[3, 0],
    v.x * m[0, 1] + v.y * m[1, 1] + v.z * m[2, 1] + m[3, 1],
    v.x * m[0, 2] + v.y * m[1, 2] + v.z * m[2, 2] + m[3, 2]
  );
end;

function UMulVec3Mat4x4(const v: TUVec3; const m: TUMat): TUVec3;
  var w: TUFloat;
begin
  w := 1 / (v.x * m[0, 3] + v.y * m[1, 3] + v.z * m[2, 3] + m[3, 3]);
  Result := TUVec3.Make(
    (v.x * m[0, 0] + v.y * m[1, 0] + v.z * m[2, 0] + m[3, 0]) * w,
    (v.x * m[0, 1] + v.y * m[1, 1] + v.z * m[2, 1] + m[3, 1]) * w,
    (v.x * m[0, 2] + v.y * m[1, 2] + v.z * m[2, 2] + m[3, 2]) * w
  );
end;

function UMulVec3Quat(const v: TUVec3; const q: TUQuat): TUVec3;
  var u: TUVec3;
  var s: TUFloat;
begin
  u := TUVec3.Make(q.x, q.y, q.z);
  s := q.w;
  Result := 2 * u.Dot(v) * u + (s * s - u.Dot(u)) * v + 2 * s * u.Cross(v);
end;

function UMulVec4Mat(const v: TUVec4; const m: TUMat): TUVec4;
begin
  Result := TUVec4.Make(
    v.x * m[0, 0] + v.y * m[1, 0] + v.z * m[2, 0] + v.w * m[3, 0],
    v.x * m[0, 1] + v.y * m[1, 1] + v.z * m[2, 1] + v.w * m[3, 1],
    v.x * m[0, 2] + v.y * m[1, 2] + v.z * m[2, 2] + v.w * m[3, 2],
    v.x * m[0, 3] + v.y * m[1, 3] + v.z * m[2, 3] + v.w * m[3, 3]
  );
end;

function UTriangleNormal(const v0, v1, v2: TUVec3): TUVec3;
begin
  Result := (v1 - v0).Cross(v2 - v0).Norm;
end;

function UXc2DLineCircle(const v0, v1, c: TUVec2; const r: TUFloat; out x0, x1: TUVec2): Boolean;
  function Sgn(const x: TUFloat): TUFloat;
  begin
    if x < 0 then Exit(-1) else Exit(1);
  end;
  var u, v, s0, s1: TUVec2;
  var dr, d, dt, dr2_rcp: TUFloat;
begin
  u := v0 - c;
  v := v1 - v0;
  dr := Sqrt(Sqr(v.x) + Sqr(v.y));
  d := u.x * v.y - v.x * u.y;
  dt := Sqr(r) * Sqr(dr) - Sqr(d);
  if (dt < 0) then Exit(False);
  dr2_rcp := 1 / Sqr(dr);
  s0 := TUVec2.Make(d * v.y, -d * v.x);
  s1 := TUVec2.Make(Sgn(v.y) * v.x, Abs(v.y)) * Sqrt(dt);
  x0 := c + (s0 + s1) * dr2_rcp;
  x1 := c + (s0 - s1) * dr2_rcp;
  Result := True;
end;

operator + (const v0, v1: TUVec2): TUVec2;
begin
  Result[0] := v0[0] + v1[0];
  Result[1] := v0[1] + v1[1];
end;

operator - (const v0, v1: TUVec2): TUVec2;
begin
  Result[0] := v0[0] - v1[0];
  Result[1] := v0[1] - v1[1];
end;

operator * (const v0, v1: TUVec2): TUVec2;
begin
  Result[0] := v0[0] * v1[0];
  Result[1] := v0[1] * v1[1];
end;

operator / (const v0, v1: TUVec2): TUVec2;
begin
  Result[0] := v0[0] / v1[0];
  Result[1] := v0[1] / v1[1];
end;

operator * (const v: TUVec2; const f: TUFloat): TUVec2;
begin
  Result[0] := v[0] * f;
  Result[1] := v[1] * f;
end;

operator * (const f: TUFloat; const v: TUVec2): TUVec2;
begin
  Result[0] := v[0] * f;
  Result[1] := v[1] * f;
end;

operator - (const v: TUVec2): TUVec2;
begin
  Result[0] := -v[0];
  Result[1] := -v[1];
end;

operator + (const v0, v1: TUVec3): TUVec3;
begin
  Result[0] := v0[0] + v1[0];
  Result[1] := v0[1] + v1[1];
  Result[2] := v0[2] + v1[2];
end;

operator - (const v0, v1: TUVec3): TUVec3;
begin
  Result[0] := v0[0] - v1[0];
  Result[1] := v0[1] - v1[1];
  Result[2] := v0[2] - v1[2];
end;

operator * (const v0, v1: TUVec3): TUVec3;
begin
  Result[0] := v0[0] * v1[0];
  Result[1] := v0[1] * v1[1];
  Result[2] := v0[2] * v1[2];
end;

operator / (const v0, v1: TUVec3): TUVec3;
begin
  Result[0] := v0[0] / v1[0];
  Result[1] := v0[1] / v1[1];
  Result[2] := v0[2] / v1[2];
end;

operator * (const v: TUVec3; const f: TUFloat): TUVec3;
begin
  Result[0] := v[0] * f;
  Result[1] := v[1] * f;
  Result[2] := v[2] * f;
end;

operator * (const f: TUFloat; const v: TUVec3): TUVec3;
begin
  Result[0] := v[0] * f;
  Result[1] := v[1] * f;
  Result[2] := v[2] * f;
end;

operator - (const v: TUVec3): TUVec3;
begin
  Result[0] := -v[0];
  Result[1] := -v[1];
  Result[2] := -v[2];
end;

operator + (const v0, v1: TUVec4): TUVec4;
begin
  Result[0] := v0[0] + v1[0];
  Result[1] := v0[1] + v1[1];
  Result[2] := v0[2] + v1[2];
  Result[3] := v0[3] + v1[3];
end;

operator - (const v0, v1: TUVec4): TUVec4;
begin
  Result[0] := v0[0] - v1[0];
  Result[1] := v0[1] - v1[1];
  Result[2] := v0[2] - v1[2];
  Result[3] := v0[3] - v1[3];
end;

operator * (const v0, v1: TUVec4): TUVec4;
begin
  Result[0] := v0[0] * v1[0];
  Result[1] := v0[1] * v1[1];
  Result[2] := v0[2] * v1[2];
  Result[3] := v0[3] * v1[3];
end;

operator / (const v0, v1: TUVec4): TUVec4;
begin
  Result[0] := v0[0] / v1[0];
  Result[1] := v0[1] / v1[1];
  Result[2] := v0[2] / v1[2];
  Result[3] := v0[3] / v1[3];
end;

operator * (const v: TUVec4; const f: TUFloat): TUVec4;
begin
  Result[0] := v[0] * f;
  Result[1] := v[1] * f;
  Result[2] := v[2] * f;
  Result[3] := v[3] * f;
end;

operator * (const f: TUFloat; const v: TUVec4): TUVec4;
begin
  Result[0] := v[0] * f;
  Result[1] := v[1] * f;
  Result[2] := v[2] * f;
  Result[3] := v[3] * f;
end;

operator - (const v: TUVec4): TUVec4;
begin
  Result[0] := -v[0];
  Result[1] := -v[1];
  Result[2] := -v[2];
  Result[3] := -v[3];
end;

operator + (const m0, m1: TUMat): TUMat;
begin
  Result := UAddMat(m0, m1);
end;

operator - (const m0, m1: TUMat): TUMat;
begin
  Result := USubMat(m0, m1);
end;

operator * (const m0, m1: TUMat): TUMat;
begin
  Result := UMulMat(m0, m1);
end;

operator * (const m: TUMat; const f: TUFloat): TUMat;
begin
  Result := UMulMatFloat(m, f);
end;

operator mod (const a, b: TUDouble): TUDouble;
begin
  Result := a - b * Int(a / b);
end;

operator mod (const a, b: TUFloat): TUFloat;
begin
  Result := a - b * Int(a / b);
end;

function UStrExplode(const Str: String; const Separator: String): TUStrArr;
  var i, j: Int32;
  var CurElement: Int32;
  var PrevParamIndex: Int32;
  var b: Boolean;
begin
  Result := nil;
  if Length(Separator) < 1 then
  begin
    SetLength(Result, 1);
    Result[0] := Str;
    Exit;
  end;
  SetLength(Result, Length(Str) + 1);
  CurElement := 0;
  PrevParamIndex := 1;
  for i := 1 to Length(Str) do
  begin
    b := True;
    for j := 0 to Length(Separator) - 1 do
    begin
      if Separator[j + 1] <> Str[i + j] then
      begin
        b := False;
        Break;
      end;
    end;
    if b then
    begin
      SetLength(Result[CurElement], i - PrevParamIndex);
      Move(Str[PrevParamIndex], Result[CurElement][1], i - PrevParamIndex);
      PrevParamIndex := i + Length(Separator);
      Inc(CurElement);
    end;
  end;
  if Length(Str) >= PrevParamIndex then
  begin
    SetLength(Result[CurElement], Length(Str) - PrevParamIndex + 1);
    Move(Str[PrevParamIndex], Result[CurElement][1], Length(Str) - PrevParamIndex + 1);
    Inc(CurElement);
  end
  else
  begin
    Result[CurElement] := '';
    Inc(CurElement);
  end;
  SetLength(Result, CurElement);
end;

function UStrIsNumber(const Str: String): Boolean;
  var i, n: Integer;
begin
  if Length(Str) < 1 then Exit(False);
  if Str[1] in ['-', '+'] then n := 2 else n := 1;
  for i := n to Length(Str) do
  if not (Str[i] in ['0'..'9']) then Exit(False);
  Result := True;
end;

procedure UStrToFile(const FileName: String; const Str: String);
  var fs: TFileStream;
begin
  fs := TFileStream.Create(FileName, fmCreate);
  try
    fs.Write(Str[1], Length(Str));
  finally
    fs.Free;
  end;
end;

function UFileToStr(const FileName: String): String;
  var fs: TFileStream;
begin
  Result := '';
  if FileExists(FileName) then
  begin
    fs := TFileStream.Create(FileName, fmOpenRead);
    try
      SetLength(Result, fs.Size);
      fs.Read(Result[1], Length(Result));
    finally
      fs.Free;
    end;
  end;
end;

var LogOffset: Int32 = 0;
procedure ULog(const Text: String; const Offset: Int32);
  var Spaces: String;
begin
  if Offset < 0 then LogOffset += Offset;
  if LogOffset > 0 then
  begin
    Spaces := '';
    SetLength(Spaces, LogOffset);
    FillChar(Spaces[1], LogOffset, ' ');
    //WriteLn(LogFile, Spaces + Msg);
    WriteLn(Spaces + Text);
  end
  else
  begin
    //WriteLn(LogFile, Msg);
    WriteLn(Text);
  end;
  if Offset > 0 then LogOffset += Offset;
end;

procedure ULogOffset(const Offset: Int32);
begin
  LogOffset += Offset;
end;

generic procedure UArrSort<T>(var Arr: array of T);
  procedure SortRange(const RangeStart, RangeEnd: Integer); overload;
    var i, j: Integer;
    var tmp, pivot: T;
  begin
    if RangeEnd <= RangeStart then exit;
    i := RangeStart;
    j := RangeEnd;
    pivot := Arr[(RangeStart + RangeEnd) shr 1];
    repeat
      while (pivot > Arr[i]) do i := i + 1;
      while (Arr[j] > pivot) do j := j - 1;
      if i <= j then
      begin
        tmp := Arr[i];
        Arr[i] := Arr[j];
        Arr[j] := tmp;
        j := j - 1;
        i := i + 1;
      end;
    until i > j;
    if RangeStart < j then SortRange(RangeStart, j);
    if i < RangeEnd then SortRange(i, RangeEnd);
  end;
begin
  SortRange(Low(Arr), High(Arr));
end;

generic procedure UArrAppend<T>(var Arr: specialize TUArray<T>; const Item: T);
begin
  SetLength(Arr, Length(Arr) + 1);
  Arr[High(Arr)] := Item;
end;

generic procedure UArrAppend<T>(var Arr: specialize TUArray<T>; const Other: specialize TUArray<T>);
  var i, n: Int32;
begin
  n := Length(Arr);
  SetLength(Arr, n + Length(Other));
  for i := 0 to High(Other) do
  begin
    Arr[n + i] := Other[i];
  end;
end;

generic procedure UArrInsert<T>(var Arr: specialize TUArray<T>; const Item: T; const Position: Int32);
  var i, j: Int32;
begin
  SetLength(Arr, Length(Arr) + 1);
  if Position < 0 then i := 0
  else if Position > High(Arr) then i := High(Arr)
  else i := Position;
  for j := i to High(Arr) - 1 do
  begin
    Arr[j + 1] := Arr[j];
  end;
  Arr[i] := Item;
end;

generic procedure UArrDelete<T>(var Arr: specialize TUArray<T>; const DelStart: Int32; const DelCount: Int32);
  var i, dc: Int32;
begin
  dc := DelCount;
  if DelStart + dc > Length(Arr) then dc := Length(Arr) - DelStart;
  if (dc < 1) or (DelStart < 0) then Exit;
  for i := DelStart to High(Arr) - dc do
  begin
    Arr[i] := Arr[i + 1];
  end;
  SetLength(Arr, Length(Arr) - dc);
end;

generic procedure UArrRemove<T>(var Arr: specialize TUArray<T>; const Item: T);
  var i, j, n: Int32;
begin
  n := 0;
  for i := High(Arr) downto 0 do
  if Arr[i] = Item then
  begin
    for j := i to High(Arr) - 1 - n do
    begin
      Arr[j] := Arr[j + 1];
    end;
    Inc(n);
  end;
  SetLength(Arr, Length(Arr) - n);
end;

generic function UArrPop<T>(var Arr: specialize TUArray<T>): T;
begin
  if Length(Arr) < 1 then Exit(Default(T));
  Result := Arr[High(Arr)];
  SetLength(Arr, Length(Arr) - 1);
end;

generic function UArrFind<T>(const Arr: specialize TUArray<T>; const Item: T): Int32;
  var i: Int32;
begin
  for i := 0 to High(Arr) do
  begin
    if Arr[i] = Item then Exit(i);
  end;
  Result := -1;
end;

generic procedure UArrClear<T>(var Arr: specialize TUArray<T>);
  var i: Int32;
begin
  for i := High(Arr) downto 0 do FreeAndNil(Arr[i]);
  Arr := nil;
end;

// Functions end



end.
