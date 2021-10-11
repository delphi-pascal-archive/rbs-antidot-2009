unit adot;
(*****************************************************************************
 *
 *       RedBeeSoft AntiDOTe Library 2009 (v0.7.2) Open Source Edition
 *
 *       Copyright (c) 2007-2009 RedBeeSoft
 *       ALL RIGHTS RESERVED
 *
 *   This library is free software: you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation, either version 3 of the License, or
 *   (at your option) any later version.
 *
 *   This library is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with this library.  If not, see <http://www.gnu.org/licenses/>.
 *
 *   Visit http://www.RedBeeSoft.com for updates and downloads.
 *   COMMERCIAL VERSION AVAILABLE ON THE SITE OF THE COMPANY.
 *
 *****************************************************************************)

{$R-,Q-} // range checking / integer overflow checking
// warning edition 0.1a
{$IFDEF FPC}
  {$MODE Delphi}
  {$OBJECTCHECKS-} // object method call checking
{$ENDIF}
{$IFNDEF FPC}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_CAST OFF}
{$ENDIF}
interface

uses
  SysUtils, Windows, Classes, Math, Variants;

(*
  Type naming conventions:
    TC*** - container class (TCVector, TCSList, ...)
    TT*** - data-type class (TTInteger, TTObject, ...)
    T_*** - internal type

  Compilers compatibility:
    Delphi 7 - Delphi 2009 (http://www.codegear.com/products)
    Free Pascal Compiler 2.2.0 or higher (http://www.freepascal.org)

  Features:
    - full support for all Delphi build-in types in all containers
    - full range of containers available: vectors, maps, sets, lists and other
    - full range of set operations available
    - all containers keep data in native format
    - parameters of all methods and properties have native format
    - multifield items in all containers
    - very simple and handy interface
    - easy to learn, easy to use
    - 100% native Object Pascal code, no DLLs, no wrappers
    - compatible with Free Pascal Compiler and thus can be used on a wide range
      of platforms and operation systems
    - full sourcecode included
    - edition for commercial projects is also available!

  Hierarchy of container classes:

  TObject
  |_TCCustom
     |_ TCVector
     |   |_ TCHeap
     |   |   |_ TCPriorityQueue
     |   |_ TCBitset
     |_ TCSList
     |   |_ TCStack
     |   |_ TCQueue
     |_ TCDList
     |   |_ TCList
     |   |_ TCDeque
     |   |_ TCUnsortedMap
     |_ TCAATree
     |   |_ TCSet
     |   |_ TCMultiSet
     |   |_ TCMap
     |   |_ TCMultimap
     |_ TCTree
     |_ TCYTree
     |_ T_CJoin

  Container classes

  Sequence containers with direct-index addressing :
    TCVEctor (array)
  Sequence containers:
    TCDeque (double-ended queue)
    TCSList (singly-linked list)
    TCDList (doubly-linked list)
    TCList (TCDList)
    TCStack (LIFO stack)
    TCQueue (FIFO queue)
    TCPriority_queue (Priority queue based on heap)
    TCBitset
  Sorted associative containers:
    TCSet (Uniqie-key set)
    TCMultiset (Multiple-key set)
    TCMap (Uniqie-key map)
    TCMultimap (Multiple-key map)
  Unsorted associative containers:
    TCUnsortedMap (hash-based multiply-key map)
  Tree containers
    TCTree (N-ary tree)
    TCAATree (self-balancing binary search tree)
    TCHeap (heap)

  Data-type classes (supported data types)

  Generic integer types:
    TTInteger      TTCardinal
  Fundamental integer types:
    TTShortint     TTSmallint     TTLongint
    TTInt64        TTByte         TTWord
    TTLongword     TTUInt64
  Generic real types:
    TTReal
  Fundamental real types:
    TTReal48       TTSingle       TTDouble
    TTExtended     TTComp         TTCurrency
  String types:
    TTAnsiString   TTWideString   TTString
  Char types:
    TTChar         TTAnsiChar     TTWideChar
  Variant types:
    TTRecord       TTVariant      TTOleVariant
  Boolean types:
    TTBoolean
  Pointer types:
    TTPointer      TTList         TTObject
  Handle types:
    TTHandle       TTHIcon
  Bit types:
    TTBit
  Geometric types:
    TTPoint        TTRect         TTFPoint
    TTFRect
  Additional types:
    TTCardinal     TTDWord

  Special thanks come to:
    Pavel Nigerish (TCYTree and some ideas).
*)

{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion >= 17.0}
    {$DEFINE HiCompiler}
  {$IFEND}
{$ENDIF}

type
  TFileFormat = (ffAnsi, ffUnicode, ffUTF8, ffBinary);

const
  erHandle = 'Invalid handle';
  erCapacity = 'Invalid capacity';
  erCount  = 'Invalid count';
  erIndex  = 'Invalid index';
  erOperation = 'Invalid operation';
  erError = 'General error';

  ERRHANDLE = -1;

{$IFDEF HiCompiler}
  ffDefault = ffUnicode;
{$ELSE}
  ffDefault = ffAnsi;
{$ENDIF}

type
  {$IFDEF FPC}
  TCHandle = longint;
  {$ELSE}
  TCHandle = integer;
  {$ENDIF}
  PIntegerList = ^TIntegerList;
  TIntegerList = array[0..MaxListSize - 1] of Integer;
  PLongintList = ^TLongintList;
  TLongintList = array[0..MaxListSize - 1] of Longint;
  PLongwordList = ^TLongwordList;
  TLongwordList = array[0..MaxListSize - 1] of Longword;
  PSmallintList = ^TSmallintList;
  TSmallintList = array[0..MaxListSize - 1] of Smallint;
  PByteList = ^TByteList;
  TByteList = array[0..MaxListSize - 1] of Byte;
  PShortintList = ^TShortintList;
  TShortintList = array[0..MaxListSize - 1] of Shortint;
  PWordList = ^TWordList;
  TWordList = array[0..MaxListSize - 1] of Word;
  PCardinalList = ^TCardinalList;
  TCardinalList = array[0..MaxListSize - 1] of Cardinal;
  PInt64List = ^TInt64List;
  TInt64List = array[0..MaxListSize - 1] of Int64;
{$IFDEF HiCompiler}
  PUInt64List = ^TUInt64List;
  TUInt64List = array[0..MaxListSize - 1] of UInt64;
{$ENDIF}
  PRealList = ^TRealList;
  TRealList = array[0..MaxListSize - 1] of Real;
  PReal48List = ^TReal48List;
  TReal48List = array[0..MaxListSize - 1] of Real48;
  PSingleList = ^TSingleList;
  TSingleList = array[0..MaxListSize - 1] of Single;
  PDoubleList = ^TDoubleList;
  TDoubleList = array[0..MaxListSize - 1] of Double;
  PExtendedList = ^TExtendedList;
  TExtendedList = array[0..MaxListSize - 1] of Extended;
  PCompList = ^TCompList;
  TCompList = array[0..MaxListSize - 1] of Comp;
  PCurrencyList = ^TCurrencyList;
  TCurrencyList = array[0..MaxListSize - 1] of Currency;
  PCharList = ^TCharList;
  TCharList = array[0..MaxListSize - 1] of Char;
  PAnsiCharList = ^TAnsiCharList;
  TAnsiCharList = array[0..MaxListSize - 1] of AnsiChar;
  PWideCharList = ^TWideCharList;
  TWideCharList = array[0..MaxListSize - 1] of WideChar;

  T_HashCustom = class
  public
    function Get(var buf; Count: integer): cardinal; virtual; abstract;
  end;

  T_HashRnd = class(T_HashCustom)
  public
    function Get(var buf; Count: integer): cardinal; override;
  end;

  TCCustom = class;
  CTTCustom = class of TTCustom;
  CTT = CTTCustom;
  CType = CTT;
  TTCustom = class;

  TOnForEach = procedure(c: TTCustom; handle: TCHandle) of object;
  TOnForEachInt = function(c: TTCustom; handle: TCHandle; value: integer):integer of object;
  TOnForEachInt64 = function(c: TTCustom; handle: TCHandle; value: int64):int64 of object;
  TOnForEachDouble = function(c: TTCustom; handle: TCHandle; value: Double):Double of object;
  TOnForEachVariant = function(c: TTCustom; handle: TCHandle; value: Variant):Variant of object;
  TOnForEachString = function(c: TTCustom; handle: TCHandle; value: String):String of object;
  TOnForEachParams = procedure(c: TTCustom; handle: TCHandle; const params: array of const) of object;

  TOnForEachProc = procedure(c: TTCustom; handle: TCHandle);
  TOnForEachIntProc = function(c: TTCustom; handle: TCHandle; value: integer):integer;
  TOnForEachInt64Proc = function(c: TTCustom; handle: TCHandle; value: int64):int64;
  TOnForEachDoubleProc = function(c: TTCustom; handle: TCHandle; value: Double):Double;
  TOnForEachVariantProc = function(c: TTCustom; handle: TCHandle; value: Variant):Variant;
  TOnForEachStringProc = function(c: TTCustom; handle: TCHandle; value: String):String;
  TOnForEachParamsProc = procedure(c: TTCustom; handle: TCHandle; const params: array of const);

  TOnCompare = function(c: TTCustom; h1,h2: TCHandle):integer of object;
  TOnCompareProc = function(c: TTCustom; h1,h2: TCHandle):integer;
  TOnCompareValues = function(var n1, n2): integer of object;

  TOnDoRepeat = procedure(c: TTCustom; n: integer) of object;
  TOnDoRepeatProc = procedure(c: TTCustom; n: integer);

  TOnCalcHash = function(var buf; Count: integer; h: T_HashCustom): cardinal of object;
  TOnDeleteStorageItem = procedure(n: TCHandle) of object;

  TOnItemDeleted = procedure(handle: TCHandle) of object;
  TOnDeleteItems = array[0..0] of TOnItemDeleted;
  POnDeleteItems = ^TOnDeleteItems;

  TOnCopyItem = procedure(Src, Dst: TCHandle) of object;
  TOnCopyItems = array[0..0] of TOnCopyItem;
  POnCopyItems = ^TOnCopyItems;

  {
    Abstract container class

    Container is reponsible for calculation of hash when necessary,
    but interface object (TT*) should be in position to override default calculations
    (most of pointer types like TTString/TTObject/... do that).

    Any time when Container wants to calculate hash it MUST call OnCalcHash event
    instead of any direct calls to hash object!
  }
  TSwapProc=procedure(var a,b,temp; size: integer);
  TMoveProc=procedure(const Source; var Dest; count : Integer);
  CCCustom = class of TCCustom;
  CContainer = CCCustom;
  CStream = class of TStream;
  TFieldState = (fsNormal, fsDestroying);
  TSkipEmptyLines = (selAuto, selSkip, selLoad);
  TCCustom = class
  private
    function GetMainContainer: TCCustom;
    procedure WriteBOM(dst: TStream);
    function CheckBOM(src: TStream): boolean;
    function ReadStrA(src: TStream): ansistring;
    function ReadStrW(src: TStream): widestring;
    function ReadStrU(src: TStream): UTF8String;
    procedure StringsToStream(src: TStrings; dst: TStream);
    function SortCompareByFields(c: TTCustom; h1, h2: TCHandle): integer;
  protected
    FCount: integer;             // count of items
    FSorted: boolean;            // container is index-sorted
    FItemSize: integer;          // bytes per item (full item, including all fields!)
    FFieldSize: integer;         // size of first field (FFieldSize <= FItemSize)
    FInitZero: boolean;          // fill new items with zero
    FGrowDown: boolean;          // reduce memory allocation when number of items decrease
    FTemp: pointer;              // pointer to memory allocated for item exchange operations
    FTempLoad: pointer;          // pointer to memory allocated for loading items
    FOnCalcHash: TOnCalcHash;    // event to calculate hash (so default hash function may be overriden)
    FFields: array of TTCustom;  // any container contains set of TTCustom-fields (first one is key)
    FDelFields: POnDeleteItems;  // methods of TTCustom items from FFields which need ItemDeleted to be called
    FDelCount: integer;          // number of items in FDelFields
    FCopyFields: POnCopyItems;   // methods of TTCustom items from FFields (they implement Copy)
    FCopyCount: integer;         // number of items in FCopyFields
    FLockAddField: integer;      // AddField function does nothing when FLockAddField>0
    FFieldSeparator: string;     // separator of fields in text file
    FEOL: string;                // end-of-line marker in text file
    FTextItem: WideString;       // current line of text file (loading)
    FTextPos: integer;           // current position at FTextItem
    FField: TTCustom;            // "default" field for container
    FCompareValue: TOnCompareValues; // function coming from TT*: function(var n1, n2): integer
    FSortFields: array of TTCustom; // fields to compare for .sort
    FSortFieldsCount: integer;   // number of fields in FSortFields
    FState: TFieldState;         // current state: fsNormal or fsDestroying
    FSkipEmptyLines: TSkipEmptyLines; // how to process empty lines when loading text file
    // some optimized procedures
    FSwapItem: TSwapProc;
    FMoveItem: TMoveProc;
    FMoveField: TMoveProc;

    procedure Error(const msg: string);
    function  LoadToken:widestring;
    function  CompareHandle(handle1, handle2: TCHandle): integer;
    function  CompareIndex(index1, index2: integer): integer;
    procedure ExchangeIndex(index1, index2: integer);
    procedure ItemDeleted(handle: TCHandle);
    procedure CopyItem(src,dst: TCHandle);
    function AddField(field: TTCustom): pointer; overload; virtual;
    function AddField(AClass: CTT): pointer; overload; virtual;
    function GetField(index: integer): TTCustom; virtual;
    function GetFieldByName(const FieldName: string): TTCustom; virtual;
    function GetFieldCount:integer; virtual;
    procedure DelField(field: TTCustom); virtual;
    function IndexOfField(field: TTCustom):integer; virtual;
    procedure AddOnDelete(AOnDelete: TOnItemDeleted); virtual;
    procedure DelOnDelete(AOnDelete: TOnItemDeleted); virtual;
    procedure AddOnCopy(AOnCopy: TOnCopyItem); virtual;
    procedure DelOnCopy(AOnCopy: TOnCopyItem); virtual;
    function GetFieldSeparator:string; virtual;
    procedure SetFieldSeparator(v: string); virtual;
    procedure SetFieldSize(s: integer); virtual;
    function MoveNext(var h: TCHandle): TCHandle;
    function MovePrev(var h: TCHandle): TCHandle;
    function GetRangeLength(start, finish: TCHandle):integer; virtual;
    procedure FreeFields;

    // Basic
    function GetItemSize: integer; virtual;
    procedure SetItemSize(n: integer); virtual;
    function GetCount: integer; virtual;
    procedure SetCount(n: integer); virtual;
    function GetCapacity: integer; virtual;
    procedure SetCapacity(n: integer); virtual;
    function GetGrowDown: boolean; virtual;
    procedure SetGrowDown(n: boolean); virtual;
    function GetInitZero: boolean; virtual;
    procedure SetInitZero(n: boolean); virtual;

    // Advanced-Compare
    function GetSorted: boolean; virtual;
    procedure SetSorted(n: boolean); virtual;

  public
    constructor Create; virtual;
    // creates empty container of specified type with copy of all fields
    // does not copy data or any data-related properties (like .Sorted)
    function CloneShell(ContainerType: CContainer):pointer; overload; virtual;
    // creates EMPTY copy of container including all fields (not data)
    function CloneShell:pointer; overload; virtual;
    // creates copy of container including all fields and makes call of Assign to transfer data
    function Clone:pointer; overload; virtual;
    // creates new container of specified type and copy all fields and data
    function Clone(ContainerType: CContainer):pointer; overload; virtual;
    // copy data and other important properties from SRC container:
    // - structure of source container (number of fields) must be identical
    // - corresponding fields must be compatible
    procedure Assign(src: TCCustom); virtual;
    destructor Destroy; override;

    procedure SaveToFile(filename: string; FileFormat : TFileFormat = ffUTF8); virtual;
    procedure SaveToStream(dst: TStream; FileFormat : TFileFormat = ffUTF8); overload; virtual;
    procedure SaveToStream(dst: CStream; FileFormat : TFileFormat = ffUTF8); overload;
    procedure SaveToStringList(dst: TStrings); virtual;
    procedure LoadFromFile(filename: string; FileFormat : TFileFormat = ffUTF8); virtual;
    procedure LoadFromStream(src: TStream; FileFormat : TFileFormat = ffUTF8); virtual;
    procedure LoadFromStringList(src: TStrings); virtual;

    // Basic data manupulations
    // Get item pointer by handle
    function Get(handle: TCHandle): Pointer; virtual; abstract;
    // set field value from pointer (FieldSize bytes, NOT ItemSize)
    procedure Put(handle: TCHandle; var Item); virtual; abstract;
    // add new item to the end of storage
    function AddItem(var item): TCHandle; virtual; abstract;
    // remove item from storage by handle
    procedure Delete(handle: TCHandle); virtual; abstract;
    // insert new item before
    // -1 value for handle is possible!
    function Insert(handle: TCHandle): TCHandle; virtual; abstract;
    // insert new item after
    function InsertAfter(handle: TCHandle): TCHandle; virtual; abstract;
    // delete all items from container
    procedure Clear; virtual; abstract;
    // exchange DATA of two items
    procedure Swap(handle1, handle2: TCHandle); overload; virtual; abstract;
    // ranges MUST do not intersect
    procedure Swap(first1, last1, first2: TCHandle); overload; virtual;
    // exchange position of items when possible (TCSList/TCDList)
    // for other containers it is equal to .swap
    procedure Exchange(handle1, handle2: TCHandle); virtual;
    // move item to new position (not data!)
    procedure Move(Curhandle, Newhandle: TCHandle); virtual; abstract;
    // replace data of DST item with copy of data from SRC
    procedure Copy(Src, Dst: TCHandle); virtual; abstract;
    // allocate some more space for new items
    procedure Grow; virtual; abstract;
    // delete item next to handle
    procedure DeleteNext(handle: TCHandle); virtual;
    // Get handle from index
    function  GetHandle(index: integer): TCHandle; virtual;
    // Get index from handle
    function GetIndex(handle: TCHandle): integer; virtual;

    // Basic (navigation)
    function First: TCHandle; virtual; abstract;
    function Last: TCHandle; virtual; abstract;
    procedure Next(var n: TCHandle); virtual; abstract;
    procedure Prev(var n: TCHandle); virtual; abstract;
    function EOF(n: TCHandle): boolean; virtual;

    {
       compare-dependent operations
       sort procedures which must be implemented in descendants
    }
    // sort range of container items by default Field with default comparator
    // (can be optimized by internal implementation of swap/compare)
    procedure Sort(AFirst, ALast: TCHandle); overload; virtual;
    // sort range of container items by default Field with user-defined by-value comparator
    procedure Sort(compare: TOnCompareValues; AFirst, ALast: TCHandle); overload; virtual;
    // sort range of container items by specified Field with user-defined by-handle comparator method
    procedure Sort(field: TTCustom; compare: TOnCompare; AFirst, ALast: TCHandle); overload; virtual;
    // sort range of container items by specified Field with user-defined by-handle comparator procedure
    procedure Sort(field: TTCustom; compare: TOnCompareProc; AFirst, ALast: TCHandle); overload; virtual;

    // additional sort procedures (implemented here)
    // sort container by default Field with default comparator
    // (can be optimized by internal implementation of swap/compare)
    procedure Sort; overload; virtual;
    // sort container by default Field with user-defined by-value comparator
    procedure Sort(compare: TOnCompareValues); overload; virtual;
    // sort container by specified Field with user-defined by-handle comparator method
    procedure Sort(field: TTCustom; compare: TOnCompare); overload; virtual;
    // sort container by specified Field with user-defined by-handle comparator procedure
    procedure Sort(field: TTCustom; compare: TOnCompareProc); overload; virtual;
    // sort container by specified set of Field with default comparators procedure
    procedure Sort(const Fields: array of TTCustom); overload; virtual;
    // sort range of container items by specified set of Field with default comparators
    // internally makes call of another .Sort to sort by specified field
    // with user-defined by-handle comparator method (SortCompareByFields)
    procedure Sort(const Fields: array of TTCustom; AFirst, ALast: TCHandle); overload; virtual;

    // remove first occurrence of item
    procedure Remove(var Item); virtual;
    // remove all occurrences of item
    procedure RemoveAll(var Item); virtual;

    // Ssearch operations
    function Find(var Item): TCHandle; virtual;
    function FindFirstEqual(var Item): TCHandle; virtual;
    procedure FindNextEqual(var Item; var handle: TCHandle); virtual;
    function FindMin: TCHandle; virtual;
    function FindMax: TCHandle; virtual;

    // Hash
    function GetOnCalcHash: TOnCalcHash;
    procedure SetOnCalcHash(event: TOnCalcHash);

    // Additional
    function InsertItem(handle: TCHandle; var item): TCHandle; virtual;
    function InsertAfterItem(handle: TCHandle; var item): TCHandle; virtual;
    procedure Reverse; overload; virtual;
    procedure Reverse(AFirst, ALast: TCHandle); overload; virtual;
    procedure Rotate(shift: integer); overload; virtual;
    procedure Rotate(AFirst, ALast: TCHandle; shift: integer); overload; virtual; abstract;
    procedure RandomShuffle; overload; virtual;
    procedure RandomShuffle(AFirst, ALast: TCHandle); overload; virtual; abstract;
    procedure FirstPermutation; virtual;
    procedure LastPermutation; virtual;
    function  NextPermutation:boolean; virtual;
    function  PrevPermutation:boolean; virtual;

    function GetRootCount: integer; virtual;
    function GetFirstRoot: TCHandle; virtual;
    function GetLastRoot: TCHandle; virtual;
    function GetParent(node: TCHandle): TCHandle; virtual;
    function GetChildCount(node: TCHandle): integer; virtual;
    function GetFirstChild(node: TCHandle): TCHandle; virtual;
    function GetLastChild(node: TCHandle): TCHandle; virtual;
    function GetPrevSibling(node: TCHandle): TCHandle; virtual;
    function GetNextSibling(node: TCHandle): TCHandle; virtual;
    function AddChildItem(AParent: TCHandle; var Item): TCHandle; virtual;
    function AddRootItem(var Item): TCHandle; virtual;
    procedure ChangeParent(Node, ANewParent: TCHandle); virtual;

    // default value is internal procedure DoCalcHash
    property OnCalcHash: TOnCalcHash Read GetOnCalcHash Write SetOnCalcHash;
    property FieldSeparator: string read GetFieldSeparator write SetFieldSeparator;
    property EOL: string Read FEOL Write FEOL;
    property FieldSize: integer read FFieldSize write SetFieldSize;
    property Fields[index: integer]: TTCustom Read GetField;
    property FieldByName[const FieldName: string]: TTCustom Read GetFieldByName;
    property FieldCount: integer read GetFieldCount;
    property Count: integer Read GetCount Write SetCount;
    property Capacity: integer Read GetCapacity Write SetCapacity;
    property GrowDown: boolean Read GetGrowDown Write SetGrowDown;
    property ItemSize: integer Read GetItemSize Write SetItemSize;
    property InitZero: boolean Read GetInitZero Write SetInitZero;
    property Sorted: boolean Read GetSorted Write SetSorted;
    // Get handle from index
    property Handles[index: integer]: TCHandle Read GetHandle;
    // Get index (position) from handle
    property Indexes[handle: TCHandle]: integer read GetIndex;
    // get value's pointer from handle
    property Items[handle: TCHandle]: pointer Read Get; default;
    // get number of items in range
    property RangeLength[start, finish: TCHandle]: integer read GetRangeLength;
    // how to process empty lines when loading data from text file
    property SkipEmptyLines: TSkipEmptyLines read FSkipEmptyLines write FSkipEmptyLines;
  end;

  // linear storage of handle-accessed memory blocks
  TVectorComparator = function(a, b: integer): integer of object;
  TCVector = class(TCCustom)
  private
    procedure RemoveAllSorted(var Item);
    procedure RemoveAllUnsorted(var Item);
    function ExtCmp(a, b: integer): integer;
    function ExtCmpPRoc(a, b: integer): integer;
  protected
    FBuf:  PByteArray;          // allocated memory for items
    FCapacity: integer;         // capacity in items
    FSortCompare: TOnCompare;
    FSortCompareProc: TOnCompareProc;
    FSortField: TTCustom;
    FComparator: TVectorComparator;

    procedure SetCount(n: integer); override;
    function GetCapacity: integer; override;
    procedure SetCapacity(n: integer); override;
    function GetRangeLength(start, finish: TCHandle):integer; override;

    // universal sort:
    // - user-defined comparator
    // - universal swap (itemsize may vary)
    procedure QSort(l, r: integer);

    // universal-swap sort:
    // - internal TT* comparator
    // - universal swap (itemsize may vary)
    procedure QSortInt(l, r: integer);

    // optimized for 4-bytes-length data sort (integer/pointer/...):
    // - internal TT* comparator
    // - internal 4-bytes swap
    procedure QuickInt4(L, R: Integer);

    // result=TRUE:
    //    - Item has found
    //    - pos is position of Item
    // result=FALSE:
    //   - Item has not found
    //   - pos=position to insert Item (if we want to preserve sorted state of Container)
    function BSearch(var Item; var pos: integer): boolean;
	
    function vecAdd: TCHandle;

  public
    constructor Create; override;
    destructor Destroy; override;

    function Get(handle: TCHandle): Pointer; override;
    procedure Put(handle: TCHandle; var Item); override;
    function AddItem(var item): TCHandle; override;
    function InsertItem(handle: TCHandle; var item): TCHandle; override;
    function InsertAfterItem(handle: TCHandle; var item): TCHandle; override;
    procedure Delete(handle: TCHandle); override;
    function Insert(handle: TCHandle): TCHandle; override;
    function InsertAfter(handle: TCHandle): TCHandle; override;
    procedure Clear; override;
    procedure Swap(handle1, handle2: TCHandle); override;
    procedure Exchange(handle1, handle2: TCHandle); override;
    procedure Move(Curhandle, Newhandle: TCHandle); override;
    procedure Copy(Src, Dst: TCHandle); override;
    procedure Grow; override;
    procedure DeleteNext(handle: TCHandle); override;
    function GetHandle(index: integer): TCHandle; override;
    function GetIndex(handle: TCHandle): integer; override;
    procedure RemoveAll(var Item); override;
    procedure Rotate(AFirst, ALast: TCHandle; shift: integer); override;
    procedure Reverse(AFirst, ALast: TCHandle); override;
    procedure RandomShuffle(AFirst, ALast: TCHandle); override;
    function  NextPermutation:boolean; override;
    function  PrevPermutation:boolean; override;

    function First: TCHandle; override;
    function Last: TCHandle; override;
    procedure Next(var n: TCHandle); override;
    procedure Prev(var n: TCHandle); override;
    function EOF(n: TCHandle): boolean; override;

    function Find(var Item): TCHandle; override;
    function FindFirstEqual(var Item): TCHandle; override;
    procedure FindNextEqual(var Item; var handle: TCHandle); override;
    function FindMin: TCHandle; override;
    function FindMax: TCHandle; override;

    // sort range of container items by default Field with default comparator
    // (can be optimized by internal implementation of swap/compare)
    procedure Sort(AFirst, ALast: TCHandle); overload; override;
    // sort range of container items by default Field with user-defined by-value comparator
    procedure Sort(compare: TOnCompareValues; AFirst, ALast: TCHandle); overload; override;
    // sort range of container items by specified Field with user-defined by-handle comparator method
    procedure Sort(field: TTCustom; compare: TOnCompare; AFirst, ALast: TCHandle); overload; override;
    // sort range of container items by specified Field with user-defined by-handle comparator procedure
    procedure Sort(field: TTCustom; compare: TOnCompareProc; AFirst, ALast: TCHandle); overload; override;

    property Buf: PByteArray Read FBuf;
  end;

  P_SSListItem = ^T_SSListItem;

  T_SSListItem = packed record
    Next: P_SSListItem;
    Data: record
    end;
  end;

  TCSListComparator = function(a, b: P_SSListItem): integer of object;
  TCSList = class(TCCustom)
  private
    function AllocNewItem: P_SSListItem;
    procedure ReleaseItem(p: P_SSListItem);
    function ListGetPrev(n: P_SSListItem): P_SSListItem;
    procedure ListRemove(n: P_SSListItem);
    procedure ListInsertBefore(v, pos: P_SSListItem);
    procedure ListInsertAfter(v, pos: P_SSListItem);
    function ReallocItem(p: P_SSListItem): P_SSListItem;
    function PtrTohandle(p: P_SSListItem): TCHandle;
    function MergeSort(c: P_SSListItem): P_SSListItem;
    function ExtCmp(a, b: P_SSListItem): integer;
    function ExtCmpProc(a, b: P_SSListItem): integer;
    function IntCmp(a, b: P_SSListItem): integer;
    procedure DoSort(AFirst, ALast: P_SSListItem);
    function ListAdd: TCHandle;
    procedure ListReverse(AFirst, ALast: P_SSListItem);
    procedure ListExchange(a, b: P_SSListItem);
  protected
    FFirst: P_SSListItem;
    FLast:  P_SSListItem;
    FSortCompare: TOnCompare;
    FSortCompareProc: TOnCompareProc;
    FSortField: TTCustom;
    FComparator: TCSListComparator;
    FExtCompareValues: TOnCompareValues;

    procedure SetItemSize(n: integer); override;
    procedure SetCount(n: integer); override;

  public
    constructor Create; override;
    destructor Destroy; override;

    // compare-dependent operations
    // sort range of container items by default Field with default comparator
    // (can be optimized by internal implementation of swap/compare)
    procedure Sort(AFirst, ALast: TCHandle); overload; override;
    // sort range of container items by default Field with user-defined by-value comparator
    procedure Sort(compare: TOnCompareValues; AFirst, ALast: TCHandle); overload; override;
    // sort range of container items by specified Field with user-defined by-handle comparator method
    procedure Sort(field: TTCustom; compare: TOnCompare; AFirst, ALast: TCHandle); overload; override;
    // sort range of container items by specified Field with user-defined by-handle comparator procedure
    procedure Sort(field: TTCustom; compare: TOnCompareProc; AFirst, ALast: TCHandle); overload; override;

    function Get(handle: TCHandle): Pointer; override;
    procedure Put(handle: TCHandle; var Item); override;
    function AddItem(var item): TCHandle; override;
    procedure Delete(handle: TCHandle); override;
    function Insert(handle: TCHandle): TCHandle; override;
    function InsertAfter(handle: TCHandle): TCHandle; override;
    procedure Clear; override;
    procedure Swap(handle1, handle2: TCHandle); override;
    procedure Exchange(handle1, handle2: TCHandle); override;
    procedure Move(Curhandle, Newhandle: TCHandle); override;
    procedure Copy(Src, Dst: TCHandle); override;
    procedure Grow; override;
    procedure DeleteNext(handle: TCHandle); override;
    function GetHandle(index: integer): TCHandle; override;
    procedure Remove(var Item); override;
    procedure RemoveAll(var Item); override;
    procedure Rotate(AFirst, ALast: TCHandle; shift: integer); override;
    procedure RandomShuffle(AFirst, ALast: TCHandle); override;
    procedure Reverse(AFirst, ALast: TCHandle); override;
    function NextPermutation:boolean; override;
    function PrevPermutation:boolean; override;

    function First: TCHandle; override;
    function Last: TCHandle; override;
    procedure Next(var n: TCHandle); override;
    procedure Prev(var n: TCHandle); override;
    function EOF(n: TCHandle): boolean; override;
  end;

  P_SDListItem = ^T_SDListItem;
  T_SDListItem = packed record
    Next: P_SDListItem;
    Prev: P_SDListItem;
    Data: record
    end;
  end;

  PDList = ^TDList;
  TDList = packed record
    FFirst: P_SDListItem;
    FLast:  P_SDListItem;
  end;

  TCDListComparator = function(a, b: P_SDListItem): integer of object;
  TCDList = class(TCCustom)
  private
    procedure ListRemove(n: P_SDListItem);
    procedure ListInsertBefore(v, p: P_SDListItem);
    procedure ListInsertAfter(v, p: P_SDListItem);
    function ListAdd: TCHandle;
    procedure ListExchange(a, b: P_SDListItem);
    procedure ListReverse(AFirst, ALast: P_SDListItem);

    function AllocNewItem: P_SDListItem;
    procedure ReleaseItem(p: P_SDListItem);
    function ReallocItem(p: P_SDListItem): P_SDListItem;
    function PtrTohandle(p: P_SDListItem): integer;
    procedure DoSort(AFirst, ALast: P_SDListItem);
    function ExtCmp(a, b: P_SDListItem): integer;
    function ExtCmpProc(a, b: P_SDListItem): integer;
    function IntCmp(a, b: P_SDListItem): integer;
  protected
    FList: TDList;
    FSortCompare: TOnCompare;
    FSortCompareProc: TOnCompareProc;
    FSortField: TTCustom;
    FComparator: TCDListComparator;
    FExtCompareValues: TOnCompareValues;

    // IStorage
    procedure SetItemSize(n: integer); override;
    procedure SetCount(n: integer); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    // (compare-dependent operations)
    // sort range of container items by default Field with default comparator
    // (can be optimized by internal implementation of swap/compare)
    procedure Sort(AFirst, ALast: TCHandle); overload; override;
    // sort range of container items by default Field with user-defined by-value comparator
    procedure Sort(compare: TOnCompareValues; AFirst, ALast: TCHandle); overload; override;
    // sort range of container items by specified Field with user-defined by-handle comparator method
    procedure Sort(field: TTCustom; compare: TOnCompare; AFirst, ALast: TCHandle); overload; override;
    // sort range of container items by specified Field with user-defined by-handle comparator procedure
    procedure Sort(field: TTCustom; compare: TOnCompareProc; AFirst, ALast: TCHandle); overload; override;

    function Get(handle: TCHandle): Pointer; override;
    procedure Put(handle: TCHandle; var Item); override;
    function AddItem(var item): TCHandle; override;
    procedure Delete(handle: TCHandle); override;
    function Insert(handle: TCHandle): TCHandle; override;
    function InsertAfter(handle: TCHandle): TCHandle; override;
    procedure Clear; override;
    procedure Swap(handle1, handle2: TCHandle); override;
    procedure Exchange(handle1, handle2: TCHandle); override;
    procedure Move(Curhandle, Newhandle: TCHandle); override;
    procedure Copy(Src, Dst: TCHandle); override;
    procedure Grow; override;
    procedure DeleteNext(handle: TCHandle); override;
    function GetHandle(index: integer): TCHandle; override;
    procedure Remove(var Item); override;
    procedure RemoveAll(var Item); override;
    procedure Rotate(AFirst, ALast: TCHandle; shift: integer); override;
    procedure RandomShuffle(AFirst, ALast: TCHandle); override;
    procedure Reverse(AFirst, ALast: TCHandle); override;
    function  NextPermutation:boolean; override;
    function  PrevPermutation:boolean; override;

    function First: TCHandle; override;
    function Last: TCHandle; override;
    procedure Next(var n: TCHandle); override;
    procedure Prev(var n: TCHandle); override;
    function EOF(n: TCHandle): boolean; override;
  end;

  {
    - PTreeNode must be compatible with P_SDListItem, so we put NextSibling and
      PrevSibling in the begin.
    - @FirstChild must be compatible with PDList, so we put LastChild
      immidiately after FirstChild.
  }
  PTreeNode = ^TTreeNode;
  TTreeNode = record
    // part which is compatible with P_SDListItem
    ListItem: record end;
    NextSibling,
    PrevSibling: PTreeNode;

    // part which is compatible with PDList
    Childs: record end;
    FirstChild,
    LastChild: PTreeNode;

    // other fields and data
    Parent: PTreeNode;
    ChildCount: integer;
    TotalChildCount: integer;
    Data: record end;
  end;

 TCTree = class(TCCustom)
  private
    function AllocNewItem: PTreeNode;
    procedure ReleaseItem(p: PTreeNode);
    procedure treeToList(node: PTreeNode);
    function ExtCmp(a, b: P_SDListItem): integer;
    function ExtCmpProc(a, b: P_SDListItem): integer;
    function IntCmp(a, b: P_SDListItem): integer;
    procedure treeCopyStructure(dst: TCTree);
    function treeAddChild(AParent: PTreeNode): PTreeNode;
    procedure treeCopyData(dst: TCTree);
    procedure SaveToStreamAnsi(dst: TStream);
    function treeGetNextItemLevel(var h: PTreeNode;
      level: integer): integer;
    procedure LoadFromStreamAnsi(src: TStream; skip: boolean);
    procedure treeAddChildAtLevel(var Level: integer; NewLevel: integer;
      var node, parent: PTreeNode);
    procedure LoadFromStreamWide(src: TStream; skip: boolean);
    procedure LoadFromStreamUtf8(src: TStream; skip: boolean);
    procedure SaveToStreamWide(dst: TStream);
    procedure SaveToStreamUTF8(dst: TStream);
    procedure SaveToStreamBin(dst: TStream);
    procedure LoadFromStreamBin(src: TStream);
  protected
    FRoot: TTreeNode;
    FSortCompare: TOnCompare;
    FSortCompareProc: TOnCompareProc;
    FSortField: TTCustom;
    FExtCompareValues: TOnCompareValues;

    procedure SetCount(n: integer); override;
    function GetCapacity: integer; override;
    procedure SetCapacity(n: integer); override;

  public
    procedure Assign(src: TCCustom); override;
    procedure SaveToStream(dst: TStream; FileFormat : TFileFormat = ffUTF8); override;
    procedure LoadFromStream(src: TStream; FileFormat : TFileFormat = ffUTF8); override;

    function Get(handle: TCHandle): Pointer; override;
    procedure Put(handle: TCHandle; var Item); override;
    function AddItem(var item): TCHandle; override;
    procedure Delete(handle: TCHandle); override;
    function Insert(handle: TCHandle): TCHandle; override;
    function InsertAfter(handle: TCHandle): TCHandle; override;
    procedure Clear; override;
    procedure Swap(handle1, handle2: TCHandle); override;
    procedure Exchange(handle1, handle2: TCHandle); override;
    procedure Move(Curhandle, Newhandle: TCHandle); override;
    procedure Copy(Src, Dst: TCHandle); override;
    procedure Grow; override;
    // .GetHandle/.GetHandle operates very well for trees with high level of branching
    function  GetHandle(index: integer): TCHandle; override;
    function GetIndex(handle: TCHandle): integer; override;

    function First: TCHandle; override;
    function Last: TCHandle; override;
    procedure Next(var n: TCHandle); override;
    procedure Prev(var n: TCHandle); override;

    procedure Sort(AFirst, ALast: TCHandle); override;
    procedure Sort(compare: TOnCompareValues; AFirst, ALast: TCHandle); override;
    procedure Sort(field: TTCustom; compare: TOnCompare; AFirst, ALast: TCHandle); override;
    procedure Sort(field: TTCustom; compare: TOnCompareProc; AFirst, ALast: TCHandle); override;

    function InsertItem(handle: TCHandle; var item): TCHandle; override;
    function InsertAfterItem(handle: TCHandle; var item): TCHandle; override;
    // operation valid only if AFirst and ALast are sibling items
    procedure Reverse(AFirst, ALast: TCHandle); override;
    // operation valid only if AFirst and ALast are sibling items
    procedure Rotate(AFirst, ALast: TCHandle; shift: integer); override;
    // operation valid only if AFirst and ALast are sibling items
    procedure RandomShuffle(AFirst, ALast: TCHandle); override;

    function GetRootCount: integer; override;
    function GetFirstRoot: TCHandle; override;
    function GetLastRoot: TCHandle; override;
    function GetParent(node: TCHandle): TCHandle; override;
    function GetChildCount(node: TCHandle): integer; override;
    function GetFirstChild(node: TCHandle): TCHandle; override;
    function GetLastChild(node: TCHandle): TCHandle; override;
    function GetPrevSibling(node: TCHandle): TCHandle; override;
    function GetNextSibling(node: TCHandle): TCHandle; override;
    function AddChildItem(AParent: TCHandle; var Item): TCHandle; override;
    function AddRootItem(var Item): TCHandle; override;
    procedure ChangeParent(Node, ANewParent: TCHandle); override;

    property RootCount: integer read GetRootCount;
    property FirstRoot: TCHandle read GetFirstRoot;
    property LastRoot: TCHandle read GetLastRoot;
    property Parent[node: TCHandle]:TCHandle read GetParent;
    property ChildCount[node: TCHandle]:integer read GetChildCount;
    property FirstChild[node: TCHandle]:TCHandle read GetFirstChild;
    property LastChild[node: TCHandle]:TCHandle read GetLastChild;
    property NextSibling[node: TCHandle]:TCHandle read GetNextSibling;
    property PrevSibling[node: TCHandle]:TCHandle read GetPrevSibling;
  end;

  P_AATreeItem = ^T_AATreeItem;
  T_AATreeItem = packed record
    Parent, Left, Right: P_AATreeItem;
    Level: integer;
    Data: record
    end;
  end;

  {
    AA-tree is kind of balanced BST. It has good performance,
    comparable to Red-Black tree, but simplier implementation.
    http://en.wikipedia.org/wiki/AA_tree
  }
  TCAATree = class(TCCustom)
  private
    function AllocNewItem: P_AATreeItem; virtual;
    procedure ReleaseItem(p: P_AATreeItem); virtual;
    function PtrTohandle(p: P_AATreeItem): TCHandle;

    procedure treeSkew(var p: P_AATreeItem);
    procedure treeSplit(var p: P_AATreeItem);
    function treeAdd(p,aparent: P_AATreeItem; var dst: P_AATreeItem): Boolean;
    function treeGetHeight(p: P_AATreeItem): integer;
    function treeFullHeight: integer;
    function treeMin(p: P_AATreeItem): P_AATreeItem;
    function treeMax(p: P_AATreeItem): P_AATreeItem;
    function treeSuccessor(p: P_AATreeItem): P_AATreeItem;
    function treePredecessor(p: P_AATreeItem): P_AATreeItem;
    procedure treeClear(p: P_AATreeItem); virtual;
    function treeDelete(x: P_AATreeItem; var t: P_AATreeItem): boolean;
    function treeFind(var Item): P_AATreeItem;
    procedure treeMove(src, dst: P_AATreeItem); virtual;
    procedure treeReplace(src, dst: P_AATreeItem);

  protected
    FRoot, FBottom, FDeleted, FLast: P_AATreeItem;

    procedure SetCount(n: integer); override;

  public
    constructor Create; override;
    destructor Destroy; override;

    // Basic data manupulations
    function Get(handle: TCHandle): Pointer; override;
    procedure Put(handle: TCHandle; var Item); override;
    procedure Delete(handle: TCHandle); override;
    function Insert(handle: TCHandle): TCHandle; override;
    function InsertAfter(handle: TCHandle): TCHandle; override;
    procedure Clear; override;
    procedure Swap(handle1, handle2: TCHandle); override;
    procedure Move(Curhandle, Newhandle: TCHandle); override;
    procedure Copy(Src, Dst: TCHandle); override;
    procedure Grow; override;
    function  GetHandle(index: integer): TCHandle; override;

    // Navigation
    function First: TCHandle; override;
    function Last: TCHandle; override;
    procedure Next(var n: TCHandle); override;
    procedure Prev(var n: TCHandle); override;
    function EOF(n: TCHandle): boolean; override;

    // Advanced (compare-dependent operations)
    procedure Sort; override;
    procedure Remove(var Item); override;
    procedure RemoveAll(var Item); override;

    // Advanced (search operations)
    function Find(var Item): TCHandle; override;
    function FindFirstEqual(var Item): TCHandle; override;
    procedure FindNextEqual(var Item; var handle: TCHandle); override;
    function FindMin: TCHandle; override;
    function FindMax: TCHandle; override;

    // Advanced (additional)
    function AddItem(var item): TCHandle; override;
    function InsertItem(handle: TCHandle; var item): TCHandle; override;
    function InsertAfterItem(handle: TCHandle; var item): TCHandle; override;

    property TreeHeight: integer read treeFullHeight;
  end;

  // TCAATree can be directly used like TCSet.
  TCSet = class(TCAATree);

  // TCAATree can be directly used like TCMap.
  TCMap = class(TCAATree);

  P_MultiMapItem = ^T_MultiMapItem;
  T_MultiMapItem = packed record
    { list = FBottom for items without duplicates
      list = nil for duplicates in doubly-linked list
      otherwise it is pointer from root node to first duplicate }
    List: P_AATreeItem;
    { For duplicates we do not use fields Item.Parent and ITem.Level
      but we keep them because all items must stay compatible.
      Otherwise some operations on item (delete for example) can have an impact
      on other items.
      For duplicates Item.Left/Item.Right are pointers of doubly-linked list. }
    Item: T_AATreeItem;
  end;

  {
    Most important difference from TCMap is that for multimap it
    is not required for key values to be unique. Multimap can contain
    multiply copies of elements.
    This feature implemented like doubly-linked list of elements.
  }
  TCMultimap = class(TCAATree)
  protected
    FLockRelease: integer;

    function AllocNewItem: P_AATreeItem; override;
    procedure ReleaseItem(p: P_AATreeItem); override;
    function mmAddDuplicate(var item; src: P_AATreeItem; dst: P_MultiMapItem): TCHandle;
    function mmGetPtr(p: P_AATreeItem): P_MultiMapItem;
    procedure treeClear(p: P_AATreeItem); override;
  public
    procedure Put(handle: TCHandle; var Item); override;
    function AddItem(var item): TCHandle; override;
    procedure FindNextEqual(var Item; var handle: TCHandle); override;
    procedure Delete(handle: TCHandle); override;
    procedure RemoveAll(var Item); override;

    // Navigation
    function Last: TCHandle; override;
    procedure Next(var n: TCHandle); override;
    procedure Prev(var n: TCHandle); override;
  end;

  {
    Most important difference from TCSet is that for multiset it
    is not required for key values to be unique. Multiset can contain
    multiply copies of any element.
    This feature implemented like internal counter of instances.
  }
  TCMultiSet = class(TCMultiMap);

  P_YTreeItem = ^T_YTreeItem;
  T_YTreeItem = packed record
    Left, Right : P_YTreeItem;
    IsRed, LLeaf, RLeaf : boolean;
    Data: record end;
  end;

  {
    Very experimental implementation of container on base of Y-trees (rival of TCAATree).
  }
  TCYTree = class(TCCustom)
  private
    FFirst, FLast: P_YTreeItem;
// iterator support
    function tnext(h:P_YTreeItem):P_YTreeItem;
    function tprev(h:P_YTreeItem):P_YTreeItem;
// core functions
    function tsearch(var item):P_YTreeItem;
    function tinsert(h: P_YTreeItem; var item):P_YTreeItem;
    function tdelete(h: P_YTreeItem; var item; goleft : boolean):P_YTreeItem;
// auxilary functions
    function trotate_left(h: P_YTreeItem):P_YTreeItem;
    function trotate_right(h: P_YTreeItem):P_YTreeItem;
    function tpush_red_left(h: P_YTreeItem):P_YTreeItem;
    function tdelete_min(h: P_YTreeItem):P_YTreeItem;
    function tfix_up(h: P_YTreeItem):P_YTreeItem;
// workhorses
    function is_red_left(h:P_YTreeItem):boolean;
    function is_red_right(h:P_YTreeItem):boolean;
    function create_node(var item): P_YTreeItem;
    procedure free_node(var h:P_YTreeItem);
    procedure free_tree(var h:P_YTreeItem);
  protected
    FRoot, FInsert : P_YTreeItem;
  public
    constructor Create; override;
    destructor  Destroy; override;

    function  Find(var Item): TCHandle; override;
    function  AddItem(var Item): TCHandle; override;
    procedure Remove(var Item); override;
    procedure Clear; override;

    function Get(handle: integer): Pointer; override;
    procedure Put(handle: integer; var Item); override;

    // IStorage navigation
    function First: TCHandle; override;
    function Last: TCHandle; override;
    procedure Next(var h: TCHandle); override;
    procedure Prev(var h: TCHandle); override;
    function EOF(h: TCHandle): boolean; override;
  end;

  // read/add to head: Values[First], Insert(First, value)
  // read/add to tail: Values[Last], Add(value)
  TCDeque = class(TCDList);

  // add to tail           : Enqueue
  // read&remove from head : Dequeue
  TCQueue = class(TCSList);

  // add to head: Push
  // read&remove from head: Pop
  TCStack = class(TCSList);

  TCList = class(TCDList);

  TCHeap = class(TCVector)
  private
    procedure heapRemoveMin;
  protected
    // number of items can not increase in the heap by COUNT property, but can decrease
    procedure SetCount(n: integer); override;
    procedure SetSorted(v: boolean); override;

  public
    function AddItem(var item): TCHandle; override;
    procedure Put(handle: TCHandle; var Item); override;
    function FindMin: TCHandle; override;
    // only top (index=0) item can be deleted from the heap at moment...
    procedure Delete(handle: TCHandle); override;

    // disabled operations
    procedure Remove(var Item); override;
    function Insert(handle: TCHandle): TCHandle; override;
    function InsertItem(handle: TCHandle; var item): TCHandle; override;
    function InsertAfterItem(handle: TCHandle; var item): TCHandle; override;

    procedure Swap(handle1, handle2: TCHandle); override;
    procedure Move(Curhandle, Newhandle: TCHandle); override;
    procedure Copy(Src, Dst: TCHandle); override;
    procedure Sort; override;
  end;

  TCPriorityQueue = class(TCHeap);

  TCBitset = class(TCVector);

  (*
     Unsorted map based on hash-tables.
     Very fast add/find/delete. Overhead ~8 bytes per item.
     Perfect choise when you know maximal number of items in container.
  *)
  TUMArray = array[0..32767] of integer;
  PUMArray = ^TUMArray;
  TCUnsortedMap = class(TCCustom)
  private
  protected
    FItems: PByteArray; // array of items (FItemSize)
    FNext: PUMArray;    // next from item-index
    FFirst: integer;    // first free item
    FBuckets: PUMArray; // array of pointers to items from FItems
    FCapacity: integer; // number of items in FBuckets AND FItems
    FHash: T_HashCustom;// hash class

    // default implementaiotn of OnCalcHash
    function DoCalcHash(var buf; Count: integer; h: T_HashCustom): cardinal;
    procedure SetHash(h: T_HashCustom);
    function GetCapacity: integer; override;
    procedure SetCapacity(n: integer); override;
    procedure SetCount(n: integer); override;

  public
    constructor Create; override;
    destructor Destroy; override;
    function BucketUsed: integer;

    procedure Grow; override;
    function AddItem(var item): TCHandle; override;
    procedure Put(handle: TCHandle; var Item); override;
    function Get(handle: TCHandle): Pointer; override;
    procedure Delete(handle: TCHandle); override;
    procedure Remove(var Item); override;
    procedure RemoveAll(var Item); override;
    procedure Clear; override;

    procedure Swap(handle1, handle2: TCHandle); override;
    procedure Copy(Src, Dst: TCHandle); override;

    // Basic (navigation)
    function First: TCHandle; override;
    function Last: TCHandle; override;
    procedure Next(var n: TCHandle); override;
    procedure Prev(var n: TCHandle); override;

    function Find(var Item): TCHandle; override;
    function FindFirstEqual(var Item): TCHandle; override;
    procedure FindNextEqual(var Item; var handle: TCHandle); override;

    // disabled operations
    function Insert(handle: TCHandle): TCHandle; override;
    function InsertAfter(handle: TCHandle): TCHandle; override;
    procedure Move(Curhandle, Newhandle: TCHandle); override;
    procedure Reverse; override;
    procedure Reverse(AFirst, ALast: TCHandle); override;
    procedure Rotate(shift: integer); override;
    procedure Rotate(AFirst, ALast: TCHandle; shift: integer); override;
    procedure RandomShuffle; override;
    procedure RandomShuffle(AFirst, ALast: TCHandle); override;

    property Hash: T_HashCustom Read FHash Write SetHash;
  end;

  (*
     Container joined to another as field
  *)
  T_CJoin = class(TCCustom)
  protected
    FParent: TCCustom;
    FOffset: integer;
    FState: TFieldState;

    function GetFieldCount: integer; override;
    function AddField(field: TTCustom): pointer; overload; override;
    function GetField(index: integer): TTCustom; override;
    function GetFieldByName(const FieldName: string): TTCustom; override;
    procedure DelField(field: TTCustom); override;
    procedure AddOnDelete(AOnDelete: TOnItemDeleted); override;
    procedure DelOnDelete(AOnDelete: TOnItemDeleted); override;
    procedure AddOnCopy(AOnCopy: TOnCopyItem); override;
    procedure DelOnCopy(AOnCopy: TOnCopyItem); override;
    function IndexOfField(field: TTCustom):integer; override;
    function GetFieldSeparator:string; override;
    procedure SetFieldSeparator(v: string); override;

    // Basic
    // function GetItemSize: integer; virtual;
    procedure SetItemSize(n: integer); override;
    function GetCount: integer; override;
    procedure SetCount(n: integer); override;
    function GetCapacity: integer; override;
    procedure SetCapacity(n: integer); override;
    function GetGrowDown: boolean; override;
    procedure SetGrowDown(n: boolean); override;
    function GetInitZero: boolean; override;
    procedure SetInitZero(n: boolean); override;

    // Advanced-Compare
    //function GetCompare: TOnCompareValues; virtual;
    //procedure SetCompare(c: TOnCompareValues); virtual;
    function GetSorted: boolean; override;
    procedure SetSorted(v: boolean); override;

  public
    constructor Create; override;
    constructor CreateJoined(origin: TCCustom; Field: TTCustom);
    destructor Destroy; override;

    procedure SaveToFile(filename: string; FileFormat : TFileFormat = ffUTF8); override;
    procedure SaveToStream(dst: TStream; FileFormat : TFileFormat = ffUTF8); override;
    procedure SaveToStringList(dst: TStrings); override;
    procedure LoadFromFile(filename: string; FileFormat : TFileFormat = ffUTF8); override;
    procedure LoadFromStream(src: TStream; FileFormat : TFileFormat = ffUTF8); override;
    procedure LoadFromStringList(src: TStrings); override;

    // Basic
    function Get(handle: TCHandle): Pointer; override;
    procedure Put(handle: TCHandle; var Item); override;
    procedure Delete(handle: TCHandle); override;
    function Insert(handle: TCHandle): TCHandle; override;
    function InsertAfter(handle: TCHandle): TCHandle; override;
    procedure Clear; override;
    procedure Swap(handle1, handle2: TCHandle); override;
    procedure Move(Curhandle, Newhandle: TCHandle); override;
    procedure Copy(Src, Dst: TCHandle); override;
    procedure Grow; override;
    procedure DeleteNext(handle: TCHandle); override;
    function  GetHandle(index: integer): TCHandle; override;

    // Basic (navigation)
    function First: TCHandle; override;
    function Last: TCHandle; override;
    procedure Next(var n: TCHandle); override;
    procedure Prev(var n: TCHandle); override;
    function EOF(handle: TCHandle): boolean; override;

    // Advanced (additional)
    function AddItem(var item): TCHandle; override;
    function InsertItem(handle: TCHandle; var item): TCHandle; override;
    function InsertAfterItem(handle: TCHandle; var item): TCHandle; override;

    procedure Sort(AFirst, ALast: TCHandle); overload; override;
    procedure Sort(compare: TOnCompareValues; AFirst, ALast: TCHandle); overload; override;
    procedure Sort(field: TTCustom; compare: TOnCompare; AFirst, ALast: TCHandle); overload; override;
    procedure Sort(field: TTCustom; compare: TOnCompareProc; AFirst, ALast: TCHandle); overload; override;
  end;

  {
    Abstract class for typified interface to container
  }
  TSortMemoryRange=procedure(Buf: pointer; L, R: Integer);
  TTCustom = class
  private
    function QuoteStrW(const s: widestring): widestring;
    function QuoteStrA(const s: ansistring): ansistring;
    procedure ImportTTCustom(c: TTCustom);
    procedure ImportTCCustom(c: TCCustom);
  protected
    FContainer: TCCustom;
    FOnDeleted: TOnDeleteStorageItem;
    FSetContainerLock: integer;
    FNeedOnDelete: integer;
    FFieldSize: integer;
    FIsTextField: boolean;
    FSortMemoryRange: TSortMemoryRange; // some TT* provide effective sorters
    FName: string;
    FState: TFieldState;

    // internal
    procedure Error(const msg: string);
    function GetRangeLength(start, finish: TCHandle):integer;
    procedure SetNeedOnDelete(n: integer);
    procedure SetFieldSize(ASize: integer); virtual;
    procedure SaveItem(h: TCHandle; dst: TStream; FileFormat : TFileFormat = ffDefault); virtual;
    function LoadItem(c: TCCustom; h: TCHandle; src: TStream; FileFormat : TFileFormat = ffDefault):TCHandle; virtual;
    procedure InitContainer(c: TCCustom); virtual;
    function GetText: string; virtual;
    procedure SetText(value: string); virtual;
    function GetTextLine: string; virtual;
    procedure SetTextLine(value: string); virtual;
    function GetWideText: WideString; virtual;
    procedure SetWideText(value: WideString); virtual;
    function GetEOL:string;
    procedure SetEOL(const NewEOL: string);
    function GetPointer(h: TCHandle):pointer;
    function GetField(index: integer):TTCustom;
    function GetItemSize:integer;

    function GetCount: integer; virtual;
    procedure SetCount(n: integer); virtual;
    function GetCapacity: integer; virtual;
    procedure SetCapacity(n: integer); virtual;
    function GetGrowDown: boolean; virtual;
    procedure SetGrowDown(n: boolean); virtual;
    function GetInitZero: boolean; virtual;
    procedure SetInitZero(n: boolean); virtual;
    function GetSorted: boolean; virtual;
    procedure SetSorted(n: boolean); virtual;
    procedure setOnDeleted(n: TOnDeleteStorageItem); virtual;
    procedure setContainer(s: TCCustom); virtual;
    function GetHandle(index: integer): TCHandle;
    function GetIndex(handle: TCHandle): integer;

    procedure ItemDeleted(handle: TCHandle); virtual;
    function GetAsVariant(handle: TCHandle): variant; virtual; abstract;
    procedure SetAsVariant(handle: TCHandle; Value: variant); virtual; abstract;
    function GetHandleOf(Value: variant): TCHandle; virtual; abstract;
    function GetAsAnsiStr(handle: TCHandle): AnsiString; virtual;
    procedure SetAsAnsiStr(handle: TCHandle; Value: AnsiString); virtual;
    function GetAsWideStr(handle: TCHandle): WideString; virtual;
    procedure SetAsWideStr(handle: TCHandle; Value: WideString); virtual;
    function GetAsString(handle: TCHandle): String; virtual;
    procedure SetAsString(handle: TCHandle; Value: String); virtual;

    // operations with items of same type (for example copy integer items into another container with integers)
    // methods should be overriden by descendants
    function STCopy(Dst: TTCustom; h: TCHandle):TCHandle; overload; virtual;
    procedure STCopy(Dst: TTCustom; AFirst, ALast: TCHandle); overload; virtual;

    procedure STSet(Dst: TTCustom; h1,h2: TCHandle); overload; virtual;
    procedure STSet(Dst: TTCustom; start1,finish1, start2: TCHandle); overload; virtual;

    // internal
    property NeedOnDelete: integer read FNeedOnDelete write SetNeedOnDelete;

    // tree-related
    function GetRootCount: integer;
    function GetFirstRoot: TCHandle;
    function GetLastRoot: TCHandle;
    function GetParent(node: TCHandle): TCHandle;
    function GetChildCount(node: TCHandle): integer;
    function GetFirstChild(node: TCHandle): TCHandle;
    function GetLastChild(node: TCHandle): TCHandle;
    function GetPrevSibling(node: TCHandle): TCHandle;
    function GetNextSibling(node: TCHandle): TCHandle;

  public
    constructor Create(AContainer: TCCustom = nil); overload; virtual;
    constructor Create(AClass: CContainer); overload;
    constructor Create(AClass: CContainer; AFields: array of CTT); overload;

    destructor Destroy; override;

    // creates empty container of specified type with copy of all fields (not data)
    function CloneShell(ContainerType: CContainer):pointer; overload; virtual;
    // creates EMPTY copy of container including all fields (not data)
    function CloneShell:pointer; overload; virtual;
    // creates new container with same structure and copy of data
    function Clone(ContainerType: CContainer):pointer; overload; virtual;
    // creates copy of container and get pointer to field
    function Clone:pointer; overload; virtual;

    // copy important properties from source (not data!)
    procedure AssignShape(src: TTCustom); virtual;
    // copy important properties and data from source (AssignShape+Container.Assign)
    procedure Assign(src: TTCustom); virtual;
    function Empty:boolean;

    procedure SaveToFile(filename: string; FileFormat : TFileFormat = ffUTF8);
    procedure SaveToStream(dst: TStream; FileFormat : TFileFormat = ffUTF8); overload;
    procedure SaveToStream(dst: CStream; FileFormat : TFileFormat = ffUTF8); overload;
    procedure SaveToStringList(dst: TStrings);
    procedure LoadFromFile(filename: string; FileFormat : TFileFormat = ffUTF8);
    procedure LoadFromStream(src: TStream; FileFormat : TFileFormat = ffUTF8);
    procedure LoadFromStringList(src: TStrings);
    procedure Print(Format: TFileFormat = ffDefault; OneLine: boolean = false);
    procedure Println(Format : TFileFormat = ffDefault; OneLine: boolean = false);

    // sort container by this (calling) field with default comparator
    procedure Sort; overload;
    // sort range of container items by this (calling) field with default comparator
    procedure Sort(AFirst, ALast: TCHandle); overload;
    // sort container with user-defined by-handle comparator method
    procedure Sort(compare: TOnCompare); overload;
    // sort container with user-defined by-handle comparator procedure
    procedure Sort(compare: TOnCompareProc); overload;
    // sort range of container items with user-defined by-handle comparator method
    procedure Sort(compare: TOnCompare; AFirst, ALast: TCHandle); overload;
    // sort range of container items with user-defined by-handle comparator procedure
    procedure Sort(compare: TOnCompareProc; AFirst, ALast: TCHandle); overload;
    // sort container by several fields with default comparators
    procedure Sort(const Fields: array of TTCustom); overload;
    // sort range of container items by several fields with default comparators
    procedure Sort(const Fields: array of TTCustom; AFirst, ALast: TCHandle); overload;

    function Add(const Items: array of const): TCHandle;
    function AddRecord(const Items: array of const): TCHandle;
    function AddList(const Items: array of const): TCHandle; virtual; abstract;
    function AddTo(handle: TCHandle; dst: TTCustom): TCHandle; virtual; abstract;
    function ImportObject(obj: TObject; defvalue: TCHandle): TCHandle;

    procedure Delete(handle: TCHandle);
    procedure DeleteNext(handle: TCHandle);
    procedure Clear;
    procedure Swap(handle1, handle2: TCHandle);
    procedure Move(Curhandle, Newhandle: TCHandle);
    procedure Copy(Srchandle, Dsthandle: TCHandle);
    procedure ChangeParent(NodeHandle, ANewParent: TCHandle);

    {
       Basic set operations.
       All set-oriented methods have names in form "SetXXX". All set operations
       are stable. It means output of all operations is also ordered.
       Set operations can work not only for sets (TCSet/TCMultiset) but also
       for any ordered container:
         - Self and D must have equal set of fields.
         - Self and D must be ordered by main field (it is always true for
           TCSet/TCMultiset/TCMap/TCMultimap, but for sequence containers like
           TCVEctor/TCSList/... you may need to call .Sort)
       All set operations out data to destination container by order. It means
       that if you out result of set operation to vector then vector is ordered
       and thus ready to be used in next set operation. It may be very usefull
       for intermediate set-operations, because it would be faster to put
       result of set-operation to TCVector instead of TCSet.

       DO NOT USE set operations with containers which are unsorted by design
       (TCUnsortedMap for example).
    }
    // get subset of items which are in self but not in D (DST := SELF - D)
    procedure SetDifference(d, dst: TTCustom; start1, finish1, start2, finish2: TCHandle); overload;
    procedure SetDifference(d, dst: TTCustom); overload;
    // create new set from items which are in both sets (RESULT := SELF AND D)
    function SetDifference(d: TTCustom):pointer; overload;
    function SetDifference(d: TTCustom; start1, finish1, start2, finish2: TCHandle):pointer; overload;
    // get number of items in difference
    function SetCountDifference(d: TTCustom):integer; overload;
    function SetCountDifference(d: TTCustom; start1, finish1, start2, finish2: TCHandle):integer; overload;

    // get subset of items which are not in both sets (DST := SELF XOR D)
    procedure SetSymmetricDifference(d, dst: TTCustom; start1, finish1, start2, finish2: TCHandle); overload;
    procedure SetSymmetricDifference(d, dst: TTCustom); overload;
    function SetSymmetricDifference(d: TTCustom):pointer; overload;
    function SetSymmetricDifference(d: TTCustom; start1, finish1, start2, finish2: TCHandle):pointer; overload;
    function SetCountSymmetricDifference(d: TTCustom):integer; overload;
    function SetCountSymmetricDifference(d: TTCustom; start1, finish1, start2, finish2: TCHandle):integer; overload;

    // get items which are in self and D (DST := SELF OR D)
    procedure SetUnion(d, dst: TTCustom; start1, finish1, start2, finish2: TCHandle); overload;
    procedure SetUnion(d, dst: TTCustom); overload;
    function SetUnion(d: TTCustom):pointer; overload;
    function SetUnion(d: TTCustom; start1, finish1, start2, finish2: TCHandle):pointer; overload;
    function SetCountUnion(d: TTCustom):integer; overload;
    function SetCountUnion(d: TTCustom; start1, finish1, start2, finish2: TCHandle):integer; overload;

    // put to DST all items which are in both sets (DST := SELF AND D)
    procedure SetIntersection(d, dst: TTCustom; start1, finish1, start2, finish2: TCHandle); overload;
    procedure SetIntersection(d, dst: TTCustom); overload;
    function SetIntersection(d: TTCustom):pointer; overload;
    function SetIntersection(d: TTCustom; start1, finish1, start2, finish2: TCHandle):pointer; overload;
    function SetCountIntersection(d: TTCustom):integer; overload;
    function SetCountIntersection(d: TTCustom; start1, finish1, start2, finish2: TCHandle):integer; overload;

    // returns TRUE when contains all items from DST (D is subset of SELF)
    function SetIncludes(d: TTCustom; start1, finish1, start2, finish2: TCHandle):boolean; overload;
    function SetIncludes(d: TTCustom):boolean; overload;
    function SetIncludes(const Items: array of const):boolean; overload;

    // returns TRUE when D contains all items from container (SELF is subset of D)
    function SetSubsetOf(d: TTCustom; start1, finish1, start2, finish2: TCHandle):boolean; overload;
    function SetSubsetOf(d: TTCustom):boolean; overload;

    // returns TRUE when both SELF and D have same items (SELF equal to D)
    function SetEqual(d: TTCustom; start1, finish1, start2, finish2: TCHandle):boolean; overload;
    function SetEqual(d: TTCustom):boolean; overload;

    // navigation
    function First: TCHandle;
    function Last: TCHandle;
    procedure Next(var n: TCHandle);
    procedure Prev(var n: TCHandle);
    function GetNext(n: TCHandle): TCHandle;
    function GetPrev(n: TCHandle): TCHandle;
    function MoveNext(var n: TCHandle): TCHandle; // return N, after that N := Next(N)
    function MovePrev(var n: TCHandle): TCHandle; // return N, after that N := Prev(N)

    function FindMin: TCHandle;
    function FindMax: TCHandle;

    function EOF(handle: TCHandle): boolean;

    function AddField(field: TTCustom): pointer; overload; virtual;
    function AddField(AClass: CTT): pointer;overload; virtual;
    function AddFields(AFields: array of CTT): pointer;

    procedure DoRepeat(OnDoRepeat: TOnDoRepeat; Count: integer); overload;
    procedure DoRepeat(OnDoRepeat: TOnDoRepeatProc; Count: integer); overload;

    procedure Rotate(shift: integer); overload;
    procedure Rotate(AFirst, ALast: TCHandle; shift: integer); overload;
    procedure Reverse; overload;
    procedure Reverse(AFirst, ALast: TCHandle); overload;
    procedure RandomShuffle; overload;
    procedure RandomShuffle(AFirst, ALast: TCHandle); overload;

    // some helpers
    // enumerate container from FIRST to LAST (forward direction) and compare with ITEMS
    function  EqualToF(const items: array of const):boolean; overload;
    function  EqualToF(AFirst: TCHandle;const items: array of const):boolean; overload;
    // enumerate container from LAST to FIRST (backward direction) and compare with ITEMS
    function  EqualToB(const items: array of const):boolean; overload;
    function  EqualToB(ALast: TCHandle;const items: array of const):boolean; overload;
    // enumerates container twice - in both directions
    function  EqualTo(const items: array of const):boolean; overload;
    function  EqualTo(AFirst, ALast: TCHandle;const items: array of const):boolean; overload;

    procedure ForEach(OnForEach: TOnForEach); overload;
    procedure ForEach(OnForEach: TOnForEachProc); overload;
    procedure ForEach(OnForEach: TOnForEachParams; const params: array of const); overload;
    procedure ForEach(OnForEach: TOnForEachParamsProc; const params: array of const); overload;
    function  ForEach(OnForEach: TOnForEachInt; value: integer):integer; overload;
    function  ForEach(OnForEach: TOnForEachIntProc; value: integer):integer; overload;
    function  ForEach(OnForEach: TOnForEachInt64; value: Int64):Int64; overload;
    function  ForEach(OnForEach: TOnForEachInt64Proc; value: Int64):Int64; overload;
    function  ForEach(OnForEach: TOnForEachDouble; value: Double):Double; overload;
    function  ForEach(OnForEach: TOnForEachDoubleProc; value: Double):Double; overload;
    function  ForEach(OnForEach: TOnForEachString; value: String):String; overload;
    function  ForEach(OnForEach: TOnForEachStringProc; value: String):String; overload;
    function  ForEach(OnForEach: TOnForEachVariant; value: Variant):Variant; overload;
    function  ForEach(OnForEach: TOnForEachVariantProc; value: Variant):Variant; overload;

    procedure ForEach(OnForEach: TOnForEach; AFirst, ALast: TCHandle); overload;
    procedure ForEach(OnForEach: TOnForEachProc; AFirst, ALast: TCHandle); overload;
    procedure ForEach(OnForEach: TOnForEachParams; AFirst, ALast: TCHandle; const params: array of const); overload;
    procedure ForEach(OnForEach: TOnForEachParamsProc; AFirst, ALast: TCHandle; const params: array of const); overload;
    function  ForEach(OnForEach: TOnForEachInt; AFirst, ALast: TCHandle; value: integer):integer; overload;
    function  ForEach(OnForEach: TOnForEachIntProc; AFirst, ALast: TCHandle; value: integer):integer; overload;
    function  ForEach(OnForEach: TOnForEachInt64; AFirst, ALast: TCHandle; value: Int64):Int64; overload;
    function  ForEach(OnForEach: TOnForEachInt64Proc; AFirst, ALast: TCHandle; value: Int64):Int64; overload;
    function  ForEach(OnForEach: TOnForEachDouble; AFirst, ALast: TCHandle; value: Double):Double; overload;
    function  ForEach(OnForEach: TOnForEachDoubleProc; AFirst, ALast: TCHandle; value: Double):Double; overload;
    function  ForEach(OnForEach: TOnForEachString; AFirst, ALast: TCHandle; value: String):String; overload;
    function  ForEach(OnForEach: TOnForEachStringProc; AFirst, ALast: TCHandle; value: String):String; overload;
    function  ForEach(OnForEach: TOnForEachVariant; AFirst, ALast: TCHandle; value: Variant):Variant; overload;
    function  ForEach(OnForEach: TOnForEachVariantProc; AFirst, ALast: TCHandle; value: Variant):Variant; overload;

    procedure CopyTo(dst: TTCustom; AFirst, ALast: TCHandle); overload;
    procedure CopyTo(dst: TTCustom); overload;

    procedure FirstPermutation;
    procedure LastPermutation;
    function  NextPermutation:boolean;
    function  PrevPermutation:boolean;

    property Text: string read GetText write SetText;
    property TextLine: string read GetTextLine write SetTextLine;
    property WideText: WideString read GetWideText write SetWideText;
    property FieldSize: integer read FFieldSize write SetFieldSize;
    property Count: integer Read GetCount Write SetCount;
    property Capacity: integer Read GetCapacity Write SetCapacity;
    property GrowDown: boolean Read GetGrowDown Write SetGrowDown;
    property InitZero: boolean Read GetInitZero Write SetInitZero;
    property Sorted: boolean Read GetSorted Write SetSorted;
    property Container: TCCustom Read FContainer Write SetContainer;
    property OnDeleted: TOnDeleteStorageItem Read FOnDeleted Write SetOnDeleted;
    property Handles[index: integer]: TCHandle Read GetHandle;
    property Indexes[handle: TCHandle]: integer Read GetIndex;
    property AsVariant[handle: TCHandle]: variant Read GetAsVariant Write SetAsVariant;
    property AsAnsiStr[handle: TCHandle]: ansistring Read GetAsAnsiStr Write SetAsAnsiStr;
    property AsWideStr[handle: TCHandle]: widestring Read GetAsWideStr Write SetAsWideStr;
    property AsString[handle: TCHandle]: String Read GetAsString Write SetAsString;
    property HandleOf[Value: variant]: TCHandle Read GetHandleOf;
    property RangeLength[start, finish: TCHandle]: integer read GetRangeLength;
    property Name: string read FName write FName;
    property EndOfLine: string read GetEOL write SetEOL;
    property Pointers[handle: TCHandle]: pointer read GetPointer;
    property Fields[index: integer]: TTCustom read GetField;
    property ItemSize: integer read GetItemSize;

    // tree
    property RootCount: integer read GetRootCount;
    property FirstRoot: TCHandle read GetFirstRoot;
    property LastRoot: TCHandle read GetLastRoot;
    property Parent[node: TCHandle]:TCHandle read GetParent;
    property ChildCount[node: TCHandle]:integer read GetChildCount;
    property FirstChild[node: TCHandle]:TCHandle read GetFirstChild;
    property LastChild[node: TCHandle]:TCHandle read GetLastChild;
    property NextSibling[node: TCHandle]:TCHandle read GetNextSibling;
    property PrevSibling[node: TCHandle]:TCHandle read GetPrevSibling;
  end;
  TTCustomField = TTCustom;

  TBitType = boolean;

  TTBit = class(TTCustom)
  protected
    FBitCount: integer;

    function GetCount: integer; override;
    procedure SetCount(n: integer); override;
    function GetCapacity: integer; override;
    procedure SetCapacity(n: integer); override;

    function GetItem(n: TCHandle): TBitType;
    procedure SetItem(n: TCHandle; Value: TBitType);

    function GetAsVariant(handle: TCHandle): variant; override;
    procedure SetAsVariant(handle: TCHandle; Value: variant); override;
    function GetHandleOf(Value: variant): TCHandle; override;

  public
    constructor Create(Container: TCCustom = nil); override;

    function AddList(const Items: array of const): TCHandle; override;
    function Add(Item: TBitType): TCHandle; overload;
    function AddTo(handle: TCHandle; dst: TTCustom): TCHandle; override;

    property Items[handle: TCHandle]: TBitType Read GetItem Write SetItem; default;
  end;
  TTBitField = TTBit;

  TBooleanType = boolean;
  TOnBooleanCompare = function(a, b: TBooleanType): integer of object;
  TOnBooleanCompareProc = function(a, b: TBooleanType): integer;

  TTBoolean = class(TTCustom)
  protected
    FOnCompare: TOnBooleanCompare;
    FOnCompareProc: TOnBooleanCompareProc;

    function GetItem(n: TCHandle): TBooleanType;
    procedure SetItem(n: TCHandle; Value: TBooleanType);
    function GetAsVariant(handle: TCHandle): variant; override;
    procedure SetAsVariant(handle: TCHandle; Value: variant); override;
    function GetHandleOf(Value: variant): TCHandle; override;
    function GetAsAnsiStr(handle: TCHandle): AnsiString; override;
    procedure SetAsAnsiStr(handle: TCHandle; Value: AnsiString); override;
    function GetAsWideStr(handle: TCHandle): WideString; override;
    procedure SetAsWideStr(handle: TCHandle; Value: WideString); override;
    procedure InitContainer(c: TCCustom); override;
    procedure SetOnCompare(c: TOnBooleanCompare);
    procedure SetOnCompareProc(c: TOnBooleanCompareProc);
    function CmpInt(var a, b): integer;
    function CmpExt(var a, b): integer;
    function CmpExtProc(var a, b): integer;
    function STCopy(Dst: TTCustom; h: TCHandle):TCHandle; override;
    procedure STCopy(Dst: TTCustom; AFirst, ALast: TCHandle); override;
    procedure STSet(Dst: TTCustom; h1,h2: TCHandle); override;
    procedure STSet(Dst: TTCustom; start1,finish1, start2: TCHandle); override;

    function GetMap(key: TBooleanType): variant;
    procedure PutMap(key: TBooleanType; Value: variant);

  public
    constructor Create(Container: TCCustom = nil); override;

    function Add(Item: TBooleanType): TCHandle; overload;
    function AddList(const Items: array of const): TCHandle; override;
    function AddTo(handle: TCHandle; dst: TTCustom): TCHandle; override;
    function Insert(handle: TCHandle; Item: TBooleanType): TCHandle;
    function InsertAfter(handle: TCHandle; Item: TBooleanType): TCHandle;

    procedure Remove(Item: TBooleanType);
    procedure RemoveAll(Item: TBooleanType);

    function Find(Item: TBooleanType): TCHandle;
    function FindFirstEqual(Item: TBooleanType): TCHandle;
    procedure FindNextEqual(Item: TBooleanType; var handle: TCHandle);

    function GetMinValue: TBooleanType;    // Get min VALUE (not handle!)
    function RemoveMinValue: TBooleanType; // Get min VALUE (not handle!) & remove it from Container
    function GetMaxValue: TBooleanType;    // Get max VALUE (not handle!)
    function RemoveMaxValue: TBooleanType; // Get max VALUE (not handle!) & remove it from Container

    function AddPair(key: TBooleanType; Value: variant):TCHandle;

    // stack & deque operations
    function  Front: TBooleanType;          // examine first element
    function  Back: TBooleanType;           // examine last element
    function  Push(v: TBooleanType):TCHandle;        // insert element at front
    function  Pop: TBooleanType;            // remove first element
    function  PushBack(v: TBooleanType):TCHandle;    // insert element at back
    function  PopBack: TBooleanType;        // remove last element

    // queue operations
    function  Enqueue(v: TBooleanType):TCHandle;    // add to tail
    function  Dequeue: TBooleanType;       // read&remove from head

    procedure Sort(compare: TOnBooleanCompare); overload;
    procedure Sort(compare: TOnBooleanCompare; AFirst, ALast: TCHandle); overload;
    procedure Sort(compare: TOnBooleanCompareProc); overload;
    procedure Sort(compare: TOnBooleanCompareProc; AFirst, ALast: TCHandle); overload;

    property Map[key: TBooleanType]: variant Read GetMap Write PutMap;
    property Items[handle: TCHandle]: TBooleanType Read GetItem Write SetItem; default;
    property OnCompare: TOnBooleanCompare Read fOnCompare Write SetOnCompare;
    property OnCompareProc: TOnBooleanCompareProc Read fOnCompareProc Write SetOnCompareProc;
  end;
  TTBooleanField = TTBoolean;

  TByteType = byte;
  TOnByteCompare = function(a, b: TByteType): integer of object;
  TOnByteCompareProc = function(a, b: TByteType): integer;

  TTByte = class(TTCustom)
  protected
    FOnCompare: TOnByteCompare;
    FOnCompareProc: TOnByteCompareProc;

    function GetItem(n: TCHandle): TByteType;
    procedure SetItem(n: TCHandle; Value: TByteType);
    function GetAsVariant(handle: TCHandle): variant; override;
    procedure SetAsVariant(handle: TCHandle; Value: variant); override;
    function GetHandleOf(Value: variant): TCHandle; override;
    function GetAsAnsiStr(handle: TCHandle): AnsiString; override;
    procedure SetAsAnsiStr(handle: TCHandle; Value: AnsiString); override;
    function GetAsWideStr(handle: TCHandle): WideString; override;
    procedure SetAsWideStr(handle: TCHandle; Value: WideString); override;
    procedure InitContainer(c: TCCustom); override;
    procedure SetOnCompare(c: TOnByteCompare);
    procedure SetOnCompareProc(c: TOnByteCompareProc);
    function CmpInt(var a, b): integer;
    function CmpExt(var a, b): integer;
    function CmpExtProc(var a, b): integer;
    function STCopy(Dst: TTCustom; h: TCHandle):TCHandle; override;
    procedure STCopy(Dst: TTCustom; AFirst, ALast: TCHandle); override;
    procedure STSet(Dst: TTCustom; h1,h2: TCHandle); override;
    procedure STSet(Dst: TTCustom; start1,finish1, start2: TCHandle); override;

    function GetMap(key: TByteType): variant;
    procedure PutMap(key: TByteType; Value: variant);

  public
    constructor Create(Container: TCCustom = nil); override;

    function Add(Item: TByteType): TCHandle; overload;
    function AddList(const Items: array of const): TCHandle; override;
    function AddTo(handle: TCHandle; dst: TTCustom): TCHandle; override;
    function Insert(handle: TCHandle; Item: TByteType): TCHandle;
    function InsertAfter(handle: TCHandle; Item: TByteType): TCHandle;

    procedure Remove(Item: TByteType);
    procedure RemoveAll(Item: TByteType);

    function Find(Item: TByteType): TCHandle;
    function FindFirstEqual(Item: TByteType): TCHandle;
    procedure FindNextEqual(Item: TByteType; var handle: TCHandle);

    function GetMinValue: TByteType;    // Get min VALUE (not handle!)
    function RemoveMinValue: TByteType; // Get min VALUE (not handle!) & remove it from Container
    function GetMaxValue: TByteType;    // Get max VALUE (not handle!)
    function RemoveMaxValue: TByteType; // Get max VALUE (not handle!) & remove it from Container

    function AddPair(key: TByteType; Value: variant):TCHandle;

    // stack & deque operations
    function  Front: TByteType;          // examine first element
    function  Back: TByteType;           // examine last element
    function  Push(v: TByteType): TCHandle;        // insert element at front
    function  Pop: TByteType;            // remove first element
    function  PushBack(v: TByteType): TCHandle;    // insert element at back
    function  PopBack: TByteType;        // remove last element

    // queue operations
    function  Enqueue(v: TByteType): TCHandle;    // add to tail
    function  Dequeue: TByteType;       // read&remove from head

    procedure Sort(compare: TOnByteCompare); overload;
    procedure Sort(compare: TOnByteCompare; AFirst, ALast: TCHandle); overload;
    procedure Sort(compare: TOnByteCompareProc); overload;
    procedure Sort(compare: TOnByteCompareProc; AFirst, ALast: TCHandle); overload;

    property Map[key: TByteType]: variant Read GetMap Write PutMap;
    property Items[handle: TCHandle]: TByteType Read GetItem Write SetItem; default;
    property OnCompare: TOnByteCompare Read fOnCompare Write SetOnCompare;
    property OnCompareProc: TOnByteCompareProc Read fOnCompareProc Write SetOnCompareProc;
  end;
  TTByteField = TTByte;

  TCharType = char;
  TOnCharCompare = function(a, b: TCharType): integer of object;
  TOnCharCompareProc = function(a, b: TCharType): integer;

  TTChar = class(TTCustom)
  protected
    FOnCompare: TOnCharCompare;
    FOnComparePRoc: TOnCharCompareProc;

    function GetItem(n: TCHandle): TCharType;
    procedure SetItem(n: TCHandle; Value: TCharType);
    function GetAsVariant(handle: TCHandle): variant; override;
    procedure SetAsVariant(handle: TCHandle; Value: variant); override;
    function GetHandleOf(Value: variant): TCHandle; override;
    function GetAsAnsiStr(handle: TCHandle): AnsiString; override;
    procedure SetAsAnsiStr(handle: TCHandle; Value: AnsiString); override;
    function GetAsWideStr(handle: TCHandle): WideString; override;
    procedure SetAsWideStr(handle: TCHandle; Value: WideString); override;
    procedure InitContainer(c: TCCustom); override;
    procedure SetOnCompare(c: TOnCharCompare);
    procedure SetOnCompareProc(c: TOnCharCompareProc);
    function CmpInt(var a, b): integer;
    function CmpExt(var a, b): integer;
    function CmpExtProc(var a, b): integer;
    function STCopy(Dst: TTCustom; h: TCHandle):TCHandle; override;
    procedure STCopy(Dst: TTCustom; AFirst, ALast: TCHandle); override;
    procedure STSet(Dst: TTCustom; h1,h2: TCHandle); override;
    procedure STSet(Dst: TTCustom; start1,finish1, start2: TCHandle); override;

    function GetMap(key: TCharType): variant;
    procedure PutMap(key: TCharType; Value: variant);

  public
    constructor Create(Container: TCCustom = nil); override;

    function Add(Item: TCharType): TCHandle; overload;
    function AddList(const Items: array of const): TCHandle; override;
    function AddTo(handle: TCHandle; dst: TTCustom): TCHandle; override;
    function Insert(handle: TCHandle; Item: TCharType): TCHandle;
    function InsertAfter(handle: TCHandle; Item: TCharType): TCHandle;

    procedure Remove(Item: TCharType);
    procedure RemoveAll(Item: TCharType);

    function Find(Item: TCharType): TCHandle;
    function FindFirstEqual(Item: TCharType): TCHandle;
    procedure FindNextEqual(Item: TCharType; var handle: TCHandle);

    function GetMinValue: TCharType;    // Get min VALUE (not handle!)
    function RemoveMinValue: TCharType; // Get min VALUE (not handle!) & remove it from Container
    function GetMaxValue: TCharType;    // Get max VALUE (not handle!)
    function RemoveMaxValue: TCharType; // Get max VALUE (not handle!) & remove it from Container

    function AddPair(key: TCharType; Value: variant):TCHandle;

    // stack & deque operations
    function Front: TCharType;          // examine first element
    function Back: TCharType;           // examine last element
    function Push(v: TCharType): TCHandle;// insert element at front
    function Pop: TCharType;            // remove first element
    function PushBack(v: TCharType): TCHandle;// insert element at back
    function PopBack: TCharType;        // remove last element

    // queue operations
    function Enqueue(v: TCharType): TCHandle;    // add to tail
    function Dequeue: TCharType;       // read&remove from head

    procedure Sort(compare: TOnCharCompare); overload;
    procedure Sort(compare: TOnCharCompare; AFirst, ALast: TCHandle); overload;
    procedure Sort(compare: TOnCharCompareProc); overload;
    procedure Sort(compare: TOnCharCompareProc; AFirst, ALast: TCHandle); overload;

    property Map[key: TCharType]: variant Read GetMap Write PutMap;
    property Items[handle: TCHandle]: TCharType Read GetItem Write SetItem; default;
    property OnCompare: TOnCharCompare Read fOnCompare Write SetOnCompare;
    property OnCompareProc: TOnCharCompareProc Read fOnComparePRoc Write SetOnComparePRoc;
  end;
  TTCharField = TTChar;

  TAnsiCharType = AnsiChar;
  TOnAnsiCharCompare = function(a, b: TAnsiCharType): integer of object;
  TOnAnsiCharCompareProc = function(a, b: TAnsiCharType): integer;

  TTAnsiChar = class(TTCustom)
  protected
    FOnCompare: TOnAnsiCharCompare;
    FOnComparePRoc: TOnAnsiCharCompareProc;

    function GetItem(n: TCHandle): TAnsiCharType;
    procedure SetItem(n: TCHandle; Value: TAnsiCharType);
    function GetAsVariant(handle: TCHandle): variant; override;
    procedure SetAsVariant(handle: TCHandle; Value: variant); override;
    function GetHandleOf(Value: variant): TCHandle; override;
    function GetAsAnsiStr(handle: TCHandle): AnsiString; override;
    procedure SetAsAnsiStr(handle: TCHandle; Value: AnsiString); override;
    function GetAsWideStr(handle: TCHandle): WideString; override;
    procedure SetAsWideStr(handle: TCHandle; Value: WideString); override;
    procedure InitContainer(c: TCCustom); override;
    procedure SetOnCompare(c: TOnAnsiCharCompare);
    procedure SetOnCompareProc(c: TOnAnsiCharCompareProc);
    function CmpInt(var a, b): integer;
    function CmpExt(var a, b): integer;
    function CmpExtProc(var a, b): integer;
    function STCopy(Dst: TTCustom; h: TCHandle):TCHandle; override;
    procedure STCopy(Dst: TTCustom; AFirst, ALast: TCHandle); override;
    procedure STSet(Dst: TTCustom; h1,h2: TCHandle); override;
    procedure STSet(Dst: TTCustom; start1,finish1, start2: TCHandle); override;

    function GetMap(key: TAnsiCharType): variant;
    procedure PutMap(key: TAnsiCharType; Value: variant);

  public
    constructor Create(Container: TCCustom = nil); override;

    function Add(Item: TAnsiCharType): TCHandle; overload;
    function AddList(const Items: array of const): TCHandle; override;
    function AddTo(handle: TCHandle; dst: TTCustom): TCHandle; override;
    function Insert(handle: TCHandle; Item: TAnsiCharType): TCHandle;
    function InsertAfter(handle: TCHandle; Item: TAnsiCharType): TCHandle;

    procedure Remove(Item: TAnsiCharType);
    procedure RemoveAll(Item: TAnsiCharType);

    function Find(Item: TAnsiCharType): TCHandle;
    function FindFirstEqual(Item: TAnsiCharType): TCHandle;
    procedure FindNextEqual(Item: TAnsiCharType; var handle: TCHandle);

    function GetMinValue: TAnsiCharType;    // Get min VALUE (not handle!)
    function RemoveMinValue: TAnsiCharType; // Get min VALUE (not handle!) & remove it from Container
    function GetMaxValue: TAnsiCharType;    // Get max VALUE (not handle!)
    function RemoveMaxValue: TAnsiCharType; // Get max VALUE (not handle!) & remove it from Container

    function AddPair(key: TAnsiCharType; Value: variant):TCHandle;

    // stack & deque operations
    function Front: TAnsiCharType;          // examine first element
    function Back: TAnsiCharType;           // examine last element
    function Push(v: TAnsiCharType): TCHandle;// insert element at front
    function Pop: TAnsiCharType;            // remove first element
    function PushBack(v: TAnsiCharType): TCHandle;// insert element at back
    function PopBack: TAnsiCharType;        // remove last element

    // queue operations
    function Enqueue(v: TAnsiCharType): TCHandle;    // add to tail
    function Dequeue: TAnsiCharType;       // read&remove from head

    procedure Sort(compare: TOnAnsiCharCompare); overload;
    procedure Sort(compare: TOnAnsiCharCompare; AFirst, ALast: TCHandle); overload;
    procedure Sort(compare: TOnAnsiCharCompareProc); overload;
    procedure Sort(compare: TOnAnsiCharCompareProc; AFirst, ALast: TCHandle); overload;

    property Map[key: TAnsiCharType]: variant Read GetMap Write PutMap;
    property Items[handle: TCHandle]: TAnsiCharType Read GetItem Write SetItem; default;
    property OnCompare: TOnAnsiCharCompare Read fOnCompare Write SetOnCompare;
    property OnCompareProc: TOnAnsiCharCompareProc Read fOnComparePRoc Write SetOnComparePRoc;
  end;
  TTAnsiCharField = TTAnsiChar;

  TWideCharType = WideChar;
  TOnWideCharCompare = function(a, b: TWideCharType): integer of object;
  TOnWideCharCompareProc = function(a, b: TWideCharType): integer;

  TTWideChar = class(TTCustom)
  protected
    FOnCompare: TOnWideCharCompare;
    FOnComparePRoc: TOnWideCharCompareProc;

    function GetItem(n: TCHandle): TWideCharType;
    procedure SetItem(n: TCHandle; Value: TWideCharType);
    function GetAsVariant(handle: TCHandle): variant; override;
    procedure SetAsVariant(handle: TCHandle; Value: variant); override;
    function GetHandleOf(Value: variant): TCHandle; override;
    function GetAsAnsiStr(handle: TCHandle): AnsiString; override;
    procedure SetAsAnsiStr(handle: TCHandle; Value: AnsiString); override;
    function GetAsWideStr(handle: TCHandle): WideString; override;
    procedure SetAsWideStr(handle: TCHandle; Value: WideString); override;
    procedure InitContainer(c: TCCustom); override;
    procedure SetOnCompare(c: TOnWideCharCompare);
    procedure SetOnCompareProc(c: TOnWideCharCompareProc);
    function CmpInt(var a, b): integer;
    function CmpExt(var a, b): integer;
    function CmpExtProc(var a, b): integer;
    function STCopy(Dst: TTCustom; h: TCHandle):TCHandle; override;
    procedure STCopy(Dst: TTCustom; AFirst, ALast: TCHandle); override;
    procedure STSet(Dst: TTCustom; h1,h2: TCHandle); override;
    procedure STSet(Dst: TTCustom; start1,finish1, start2: TCHandle); override;

    function GetMap(key: TWideCharType): variant;
    procedure PutMap(key: TWideCharType; Value: variant);

  public
    constructor Create(Container: TCCustom = nil); override;

    function Add(Item: TWideCharType): TCHandle; overload;
    function AddList(const Items: array of const): TCHandle; override;
    function AddTo(handle: TCHandle; dst: TTCustom): TCHandle; override;
    function Insert(handle: TCHandle; Item: TWideCharType): TCHandle;
    function InsertAfter(handle: TCHandle; Item: TWideCharType): TCHandle;

    procedure Remove(Item: TWideCharType);
    procedure RemoveAll(Item: TWideCharType);

    function Find(Item: TWideCharType): TCHandle;
    function FindFirstEqual(Item: TWideCharType): TCHandle;
    procedure FindNextEqual(Item: TWideCharType; var handle: TCHandle);

    function GetMinValue: TWideCharType;    // Get min VALUE (not handle!)
    function RemoveMinValue: TWideCharType; // Get min VALUE (not handle!) & remove it from Container
    function GetMaxValue: TWideCharType;    // Get max VALUE (not handle!)
    function RemoveMaxValue: TWideCharType; // Get max VALUE (not handle!) & remove it from Container

    function AddPair(key: TWideCharType; Value: variant):TCHandle;

    // stack & deque operations
    function Front: TWideCharType;          // examine first element
    function Back: TWideCharType;           // examine last element
    function Push(v: TWideCharType): TCHandle;// insert element at front
    function Pop: TWideCharType;            // remove first element
    function PushBack(v: TWideCharType): TCHandle;// insert element at back
    function PopBack: TWideCharType;        // remove last element

    // queue operations
    function Enqueue(v: TWideCharType): TCHandle;    // add to tail
    function Dequeue: TWideCharType;       // read&remove from head

    procedure Sort(compare: TOnWideCharCompare); overload;
    procedure Sort(compare: TOnWideCharCompare; AFirst, ALast: TCHandle); overload;
    procedure Sort(compare: TOnWideCharCompareProc); overload;
    procedure Sort(compare: TOnWideCharCompareProc; AFirst, ALast: TCHandle); overload;

    property Map[key: TWideCharType]: variant Read GetMap Write PutMap;
    property Items[handle: TCHandle]: TWideCharType Read GetItem Write SetItem; default;
    property OnCompare: TOnWideCharCompare Read fOnCompare Write SetOnCompare;
    property OnCompareProc: TOnWideCharCompareProc Read fOnComparePRoc Write SetOnComparePRoc;
  end;
  TTWideCharField = TTWideChar;

  TDoubleType = double;
  TOnDoubleCompare = function(a, b: TDoubleType): integer of object;
  TOnDoubleCompareProc = function(a, b: TDoubleType): integer;

  TTDouble = class(TTCustom)
  protected
    FOnCompare: TOnDoubleCompare;
    FOnCompareProc: TOnDoubleCompareProc;

    function GetItem(n: TCHandle): TDoubleType;
    procedure SetItem(n: TCHandle; Value: TDoubleType);
    function GetAsVariant(handle: TCHandle): variant; override;
    procedure SetAsVariant(handle: TCHandle; Value: variant); override;
    function GetAsAnsiStr(handle: TCHandle): AnsiString; override;
    procedure SetAsAnsiStr(handle: TCHandle; Value: AnsiString); override;
    function GetAsWideStr(handle: TCHandle): WideString; override;
    procedure SetAsWideStr(handle: TCHandle; Value: WideString); override;
    function GetHandleOf(Value: variant): TCHandle; override;
    procedure InitContainer(c: TCCustom); override;
    procedure SetOnCompare(c: TOnDoubleCompare);
    procedure SetOnCompareProc(c: TOnDoubleCompareProc);
    function CmpInt(var a, b): integer;
    function CmpExt(var a, b): integer;
    function CmpExtProc(var a, b): integer;
    function STCopy(Dst: TTCustom; h: TCHandle):TCHandle; override;
    procedure STCopy(Dst: TTCustom; AFirst, ALast: TCHandle); override;
    procedure STSet(Dst: TTCustom; h1,h2: TCHandle); override;
    procedure STSet(Dst: TTCustom; start1,finish1, start2: TCHandle); override;

    function GetMap(key: TDoubleType): variant;
    procedure PutMap(key: TDoubleType; Value: variant);

  public
    constructor Create(Container: TCCustom = nil); override;

    function Add(Item: TDoubleType): TCHandle; overload;
    function AddList(const Items: array of const): TCHandle; override;
    function AddTo(handle: TCHandle; dst: TTCustom): TCHandle; override;
    function Insert(handle: TCHandle; Item: TDoubleType): TCHandle;
    function InsertAfter(handle: TCHandle; Item: TDoubleType): TCHandle;

    procedure Remove(Item: TDoubleType);
    procedure RemoveAll(Item: TDoubleType);

    function Find(Item: TDoubleType): TCHandle;
    function FindFirstEqual(Item: TDoubleType): TCHandle;
    procedure FindNextEqual(Item: TDoubleType; var handle: TCHandle);

    function GetMinValue: TDoubleType;    // Get min VALUE (not handle!)
    function RemoveMinValue: TDoubleType; // Get min VALUE (not handle!) & remove it from Container
    function GetMaxValue: TDoubleType;    // Get max VALUE (not handle!)
    function RemoveMaxValue: TDoubleType; // Get max VALUE (not handle!) & remove it from Container

    function AddPair(key: TDoubleType; Value: variant):TCHandle;

    // stack & deque operations
    function Front: TDoubleType;          // examine first element
    function Back: TDoubleType;           // examine last element
    function Push(v: TDoubleType): TCHandle;        // insert element at front
    function Pop: TDoubleType;            // remove first element
    function PushBack(v: TDoubleType): TCHandle;    // insert element at back
    function PopBack: TDoubleType;        // remove last element

    // queue operations
    function Enqueue(v: TDoubleType): TCHandle;    // add to tail
    function Dequeue: TDoubleType;       // read&remove from head

    procedure Sort(compare: TOnDoubleCompare); overload;
    procedure Sort(compare: TOnDoubleCompare; AFirst, ALast: TCHandle); overload;
    procedure Sort(compare: TOnDoubleCompareProc); overload;
    procedure Sort(compare: TOnDoubleCompareProc; AFirst, ALast: TCHandle); overload;

    property Map[key: TDoubleType]: variant Read GetMap Write PutMap;
    property Items[handle: TCHandle]: TDoubleType Read GetItem Write SetItem; default;
    property OnCompare: TOnDoubleCompare Read fOnCompare Write SetOnCompare;
    property OnCompareProc: TOnDoubleComparePRoc Read fOnCompareProc Write SetOnCompareProc;
  end;
  TTDoubleField = TTDouble;

  TSingleType = Single;
  TOnSingleCompare = function(a, b: TSingleType): integer of object;
  TOnSingleCompareProc = function(a, b: TSingleType): integer;

  TTSingle = class(TTCustom)
  protected
    FOnCompare: TOnSingleCompare;
    FOnCompareProc: TOnSingleCompareProc;

    function GetItem(n: TCHandle): TSingleType;
    procedure SetItem(n: TCHandle; Value: TSingleType);
    function GetAsVariant(handle: TCHandle): variant; override;
    procedure SetAsVariant(handle: TCHandle; Value: variant); override;
    function GetAsAnsiStr(handle: TCHandle): AnsiString; override;
    procedure SetAsAnsiStr(handle: TCHandle; Value: AnsiString); override;
    function GetAsWideStr(handle: TCHandle): WideString; override;
    procedure SetAsWideStr(handle: TCHandle; Value: WideString); override;
    function GetHandleOf(Value: variant): TCHandle; override;
    procedure InitContainer(c: TCCustom); override;
    procedure SetOnCompare(c: TOnSingleCompare);
    procedure SetOnCompareProc(c: TOnSingleCompareProc);
    function CmpInt(var a, b): integer;
    function CmpExt(var a, b): integer;
    function CmpExtProc(var a, b): integer;
    function STCopy(Dst: TTCustom; h: TCHandle):TCHandle; override;
    procedure STCopy(Dst: TTCustom; AFirst, ALast: TCHandle); override;
    procedure STSet(Dst: TTCustom; h1,h2: TCHandle); override;
    procedure STSet(Dst: TTCustom; start1,finish1, start2: TCHandle); override;

    function GetMap(key: TSingleType): variant;
    procedure PutMap(key: TSingleType; Value: variant);

  public
    constructor Create(Container: TCCustom = nil); override;

    function Add(Item: TSingleType): TCHandle; overload;
    function AddList(const Items: array of const): TCHandle; override;
    function AddTo(handle: TCHandle; dst: TTCustom): TCHandle; override;
    function Insert(handle: TCHandle; Item: TSingleType): TCHandle;
    function InsertAfter(handle: TCHandle; Item: TSingleType): TCHandle;

    procedure Remove(Item: TSingleType);
    procedure RemoveAll(Item: TSingleType);

    function Find(Item: TSingleType): TCHandle;
    function FindFirstEqual(Item: TSingleType): TCHandle;
    procedure FindNextEqual(Item: TSingleType; var handle: TCHandle);

    function GetMinValue: TSingleType;    // Get min VALUE (not handle!)
    function RemoveMinValue: TSingleType; // Get min VALUE (not handle!) & remove it from Container
    function GetMaxValue: TSingleType;    // Get max VALUE (not handle!)
    function RemoveMaxValue: TSingleType; // Get max VALUE (not handle!) & remove it from Container

    function AddPair(key: TSingleType; Value: variant):TCHandle;

    // stack & deque operations
    function Front: TSingleType;          // examine first element
    function Back: TSingleType;           // examine last element
    function Push(v: TSingleType): TCHandle;        // insert element at front
    function Pop: TSingleType;            // remove first element
    function PushBack(v: TSingleType): TCHandle;    // insert element at back
    function PopBack: TSingleType;        // remove last element

    // queue operations
    function Enqueue(v: TSingleType): TCHandle;    // add to tail
    function Dequeue: TSingleType;       // read&remove from head

    procedure Sort(compare: TOnSingleCompare); overload;
    procedure Sort(compare: TOnSingleCompare; AFirst, ALast: TCHandle); overload;
    procedure Sort(compare: TOnSingleCompareProc); overload;
    procedure Sort(compare: TOnSingleCompareProc; AFirst, ALast: TCHandle); overload;

    property Map[key: TSingleType]: variant Read GetMap Write PutMap;
    property Items[handle: TCHandle]: TSingleType Read GetItem Write SetItem; default;
    property OnCompare: TOnSingleCompare Read fOnCompare Write SetOnCompare;
    property OnCompareProc: TOnSingleComparePRoc Read fOnCompareProc Write SetOnCompareProc;
  end;
  TTSingleField = TTSingle;

  TRealType = Real;
  TOnRealCompare = function(a, b: TRealType): integer of object;
  TOnRealCompareProc = function(a, b: TRealType): integer;

  TTReal = class(TTCustom)
  protected
    FOnCompare: TOnRealCompare;
    FOnCompareProc: TOnRealCompareProc;

    function GetItem(n: TCHandle): TRealType;
    procedure SetItem(n: TCHandle; Value: TRealType);
    function GetAsVariant(handle: TCHandle): variant; override;
    procedure SetAsVariant(handle: TCHandle; Value: variant); override;
    function GetHandleOf(Value: variant): TCHandle; override;
    function GetAsAnsiStr(handle: TCHandle): AnsiString; override;
    procedure SetAsAnsiStr(handle: TCHandle; Value: AnsiString); override;
    function GetAsWideStr(handle: TCHandle): WideString; override;
    procedure SetAsWideStr(handle: TCHandle; Value: WideString); override;
    procedure InitContainer(c: TCCustom); override;
    procedure SetOnCompare(c: TOnRealCompare);
    procedure SetOnCompareProc(c: TOnRealCompareProc);
    function CmpInt(var a, b): integer;
    function CmpExt(var a, b): integer;
    function CmpExtProc(var a, b): integer;
    function STCopy(Dst: TTCustom; h: TCHandle):TCHandle; override;
    procedure STCopy(Dst: TTCustom; AFirst, ALast: TCHandle); override;
    procedure STSet(Dst: TTCustom; h1,h2: TCHandle); override;
    procedure STSet(Dst: TTCustom; start1,finish1, start2: TCHandle); override;
    function GetMap(key: TRealType): variant;
    procedure PutMap(key: TRealType; Value: variant);

  public
    constructor Create(Container: TCCustom = nil); override;

    function Add(Item: TRealType): TCHandle; overload;
    function AddList(const Items: array of const): TCHandle; override;
    function AddTo(handle: TCHandle; dst: TTCustom): TCHandle; override;
    function Insert(handle: TCHandle; Item: TRealType): TCHandle;
    function InsertAfter(handle: TCHandle; Item: TRealType): TCHandle;

    procedure Remove(Item: TRealType);
    procedure RemoveAll(Item: TRealType);

    function Find(Item: TRealType): TCHandle;
    function FindFirstEqual(Item: TRealType): TCHandle;
    procedure FindNextEqual(Item: TRealType; var handle: TCHandle);

    function GetMinValue: TRealType;    // Get min VALUE (not handle!)
    function RemoveMinValue: TRealType; // Get min VALUE (not handle!) & remove it from Container
    function GetMaxValue: TRealType;    // Get max VALUE (not handle!)
    function RemoveMaxValue: TRealType; // Get max VALUE (not handle!) & remove it from Container

    function AddPair(key: TRealType; Value: variant):TCHandle;

    // stack & deque operations
    function Front: TRealType;             // examine first element
    function Back: TRealType;              // examine last element
    function Push(v: TRealType): TCHandle; // insert element at front
    function Pop: TRealType;               // remove first element
    function PushBack(v: TRealType): TCHandle; // insert element at back
    function PopBack: TRealType;           // remove last element

    // queue operations
    function Enqueue(v: TRealType): TCHandle;    // add to tail
    function Dequeue: TRealType;       // read&remove from head

    procedure Sort(compare: TOnRealCompare); overload;
    procedure Sort(compare: TOnRealCompare; AFirst, ALast: TCHandle); overload;
    procedure Sort(compare: TOnRealCompareProc); overload;
    procedure Sort(compare: TOnRealCompareProc; AFirst, ALast: TCHandle); overload;

    property Map[key: TRealType]: variant Read GetMap Write PutMap;
    property Items[handle: TCHandle]: TRealType Read GetItem Write SetItem; default;
    property OnCompare: TOnRealCompare Read fOnCompare Write SetOnCompare;
    property OnCompareProc: TOnRealCompareProc Read fOnCompareProc Write SetOnCompareProc;
  end;
  TTRealField = TTReal;

  {$IFNDEF FPC}
  TReal48Type = Real48;
  TOnReal48Compare = function(a, b: TReal48Type): integer of object;
  TOnReal48CompareProc = function(a, b: TReal48Type): integer;

  TTReal48 = class(TTCustom)
  protected
    FOnCompare: TOnReal48Compare;
    FOnCompareProc: TOnReal48CompareProc;

    function GetItem(n: TCHandle): TReal48Type;
    procedure SetItem(n: TCHandle; Value: TReal48Type);
    function GetAsVariant(handle: TCHandle): variant; override;
    procedure SetAsVariant(handle: TCHandle; Value: variant); override;
    function GetHandleOf(Value: variant): TCHandle; override;
    function GetAsAnsiStr(handle: TCHandle): AnsiString; override;
    procedure SetAsAnsiStr(handle: TCHandle; Value: AnsiString); override;
    function GetAsWideStr(handle: TCHandle): WideString; override;
    procedure SetAsWideStr(handle: TCHandle; Value: WideString); override;
    procedure InitContainer(c: TCCustom); override;
    procedure SetOnCompare(c: TOnReal48Compare);
    procedure SetOnCompareProc(c: TOnReal48CompareProc);
    function CmpInt(var a, b): integer;
    function CmpExt(var a, b): integer;
    function CmpExtProc(var a, b): integer;
    function STCopy(Dst: TTCustom; h: TCHandle):TCHandle; override;
    procedure STCopy(Dst: TTCustom; AFirst, ALast: TCHandle); override;
    procedure STSet(Dst: TTCustom; h1,h2: TCHandle); override;
    procedure STSet(Dst: TTCustom; start1,finish1, start2: TCHandle); override;
    function GetMap(key: TReal48Type): variant;
    procedure PutMap(key: TReal48Type; Value: variant);

  public
    constructor Create(Container: TCCustom = nil); override;

    function Add(Item: TReal48Type): TCHandle; overload;
    function AddList(const Items: array of const): TCHandle; override;
    function AddTo(handle: TCHandle; dst: TTCustom): TCHandle; override;
    function Insert(handle: TCHandle; Item: TReal48Type): TCHandle;
    function InsertAfter(handle: TCHandle; Item: TReal48Type): TCHandle;

    procedure Remove(Item: TReal48Type);
    procedure RemoveAll(Item: TReal48Type);

    function Find(Item: TReal48Type): TCHandle;
    function FindFirstEqual(Item: TReal48Type): TCHandle;
    procedure FindNextEqual(Item: TReal48Type; var handle: TCHandle);

    function GetMinValue: TReal48Type;    // Get min VALUE (not handle!)
    function RemoveMinValue: TReal48Type; // Get min VALUE (not handle!) & remove it from Container
    function GetMaxValue: TReal48Type;    // Get max VALUE (not handle!)
    function RemoveMaxValue: TReal48Type; // Get max VALUE (not handle!) & remove it from Container

    function AddPair(key: TReal48Type; Value: variant):TCHandle;

    // stack & deque operations
    function Front: TReal48Type;             // examine first element
    function Back: TReal48Type;              // examine last element
    function Push(v: TReal48Type): TCHandle; // insert element at front
    function Pop: TReal48Type;               // remove first element
    function PushBack(v: TReal48Type): TCHandle; // insert element at back
    function PopBack: TReal48Type;           // remove last element

    // queue operations
    function Enqueue(v: TReal48Type): TCHandle;    // add to tail
    function Dequeue: TReal48Type;       // read&remove from head

    procedure Sort(compare: TOnReal48Compare); overload;
    procedure Sort(compare: TOnReal48Compare; AFirst, ALast: TCHandle); overload;
    procedure Sort(compare: TOnReal48CompareProc); overload;
    procedure Sort(compare: TOnReal48CompareProc; AFirst, ALast: TCHandle); overload;

    property Map[key: TReal48Type]: variant Read GetMap Write PutMap;
    property Items[handle: TCHandle]: TReal48Type Read GetItem Write SetItem; default;
    property OnCompare: TOnReal48Compare Read fOnCompare Write SetOnCompare;
    property OnCompareProc: TOnReal48CompareProc Read fOnCompareProc Write SetOnCompareProc;
  end;
  TTReal48Field = TTReal48;
  {$ENDIF}

  TCompType = Comp;
  TOnCompCompare = function(a, b: TCompType): integer of object;
  TOnCompCompareProc = function(a, b: TCompType): integer;

  TTComp = class(TTCustom)
  protected
    FOnCompare: TOnCompCompare;
    FOnCompareProc: TOnCompCompareProc;

    function GetItem(n: TCHandle): TCompType;
    procedure SetItem(n: TCHandle; Value: TCompType);
    function GetAsVariant(handle: TCHandle): variant; override;
    procedure SetAsVariant(handle: TCHandle; Value: variant); override;
    function GetHandleOf(Value: variant): TCHandle; override;
    function GetAsAnsiStr(handle: TCHandle): AnsiString; override;
    procedure SetAsAnsiStr(handle: TCHandle; Value: AnsiString); override;
    function GetAsWideStr(handle: TCHandle): WideString; override;
    procedure SetAsWideStr(handle: TCHandle; Value: WideString); override;
    procedure InitContainer(c: TCCustom); override;
    procedure SetOnCompare(c: TOnCompCompare);
    procedure SetOnCompareProc(c: TOnCompCompareProc);
    function CmpInt(var a, b): integer;
    function CmpExt(var a, b): integer;
    function CmpExtProc(var a, b): integer;
    function STCopy(Dst: TTCustom; h: TCHandle):TCHandle; override;
    procedure STCopy(Dst: TTCustom; AFirst, ALast: TCHandle); override;
    procedure STSet(Dst: TTCustom; h1,h2: TCHandle); override;
    procedure STSet(Dst: TTCustom; start1,finish1, start2: TCHandle); override;
    function GetMap(key: TCompType): variant;
    procedure PutMap(key: TCompType; Value: variant);

  public
    constructor Create(Container: TCCustom = nil); override;

    function Add(Item: TCompType): TCHandle; overload;
    function AddList(const Items: array of const): TCHandle; override;
    function AddTo(handle: TCHandle; dst: TTCustom): TCHandle; override;
    function Insert(handle: TCHandle; Item: TCompType): TCHandle;
    function InsertAfter(handle: TCHandle; Item: TCompType): TCHandle;

    procedure Remove(Item: TCompType);
    procedure RemoveAll(Item: TCompType);

    function Find(Item: TCompType): TCHandle;
    function FindFirstEqual(Item: TCompType): TCHandle;
    procedure FindNextEqual(Item: TCompType; var handle: TCHandle);

    function GetMinValue: TCompType;    // Get min VALUE (not handle!)
    function RemoveMinValue: TCompType; // Get min VALUE (not handle!) & remove it from Container
    function GetMaxValue: TCompType;    // Get max VALUE (not handle!)
    function RemoveMaxValue: TCompType; // Get max VALUE (not handle!) & remove it from Container

    function AddPair(key: TCompType; Value: variant):TCHandle;

    // stack & deque operations
    function Front: TCompType;             // examine first element
    function Back: TCompType;              // examine last element
    function Push(v: TCompType): TCHandle; // insert element at front
    function Pop: TCompType;               // remove first element
    function PushBack(v: TCompType): TCHandle; // insert element at back
    function PopBack: TCompType;           // remove last element

    // queue operations
    function Enqueue(v: TCompType): TCHandle;    // add to tail
    function Dequeue: TCompType;       // read&remove from head

    procedure Sort(compare: TOnCompCompare); overload;
    procedure Sort(compare: TOnCompCompare; AFirst, ALast: TCHandle); overload;
    procedure Sort(compare: TOnCompCompareProc); overload;
    procedure Sort(compare: TOnCompCompareProc; AFirst, ALast: TCHandle); overload;

    property Map[key: TCompType]: variant Read GetMap Write PutMap;
    property Items[handle: TCHandle]: TCompType Read GetItem Write SetItem; default;
    property OnCompare: TOnCompCompare Read fOnCompare Write SetOnCompare;
    property OnCompareProc: TOnCompCompareProc Read fOnCompareProc Write SetOnCompareProc;
  end;
  TTCompField = TTComp;

  TCurrencyType = Currency;
  TOnCurrencyCompare = function(a, b: TCurrencyType): integer of object;
  TOnCurrencyCompareProc = function(a, b: TCurrencyType): integer;

  TTCurrency = class(TTCustom)
  protected
    FOnCompare: TOnCurrencyCompare;
    FOnCompareProc: TOnCurrencyCompareProc;

    function GetItem(n: TCHandle): TCurrencyType;
    procedure SetItem(n: TCHandle; Value: TCurrencyType);
    function GetAsVariant(handle: TCHandle): variant; override;
    procedure SetAsVariant(handle: TCHandle; Value: variant); override;
    function GetHandleOf(Value: variant): TCHandle; override;
    function GetAsAnsiStr(handle: TCHandle): AnsiString; override;
    procedure SetAsAnsiStr(handle: TCHandle; Value: AnsiString); override;
    function GetAsWideStr(handle: TCHandle): WideString; override;
    procedure SetAsWideStr(handle: TCHandle; Value: WideString); override;
    procedure InitContainer(c: TCCustom); override;
    procedure SetOnCompare(c: TOnCurrencyCompare);
    procedure SetOnCompareProc(c: TOnCurrencyCompareProc);
    function CmpInt(var a, b): integer;
    function CmpExt(var a, b): integer;
    function CmpExtProc(var a, b): integer;
    function STCopy(Dst: TTCustom; h: TCHandle):TCHandle; override;
    procedure STCopy(Dst: TTCustom; AFirst, ALast: TCHandle); override;
    procedure STSet(Dst: TTCustom; h1,h2: TCHandle); override;
    procedure STSet(Dst: TTCustom; start1,finish1, start2: TCHandle); override;
    function GetMap(key: TCurrencyType): variant;
    procedure PutMap(key: TCurrencyType; Value: variant);

  public
    constructor Create(Container: TCCustom = nil); override;

    function Add(Item: TCurrencyType): TCHandle; overload;
    function AddList(const Items: array of const): TCHandle; override;
    function AddTo(handle: TCHandle; dst: TTCustom): TCHandle; override;
    function Insert(handle: TCHandle; Item: TCurrencyType): TCHandle;
    function InsertAfter(handle: TCHandle; Item: TCurrencyType): TCHandle;

    procedure Remove(Item: TCurrencyType);
    procedure RemoveAll(Item: TCurrencyType);

    function Find(Item: TCurrencyType): TCHandle;
    function FindFirstEqual(Item: TCurrencyType): TCHandle;
    procedure FindNextEqual(Item: TCurrencyType; var handle: TCHandle);

    function GetMinValue: TCurrencyType;    // Get min VALUE (not handle!)
    function RemoveMinValue: TCurrencyType; // Get min VALUE (not handle!) & remove it from Container
    function GetMaxValue: TCurrencyType;    // Get max VALUE (not handle!)
    function RemoveMaxValue: TCurrencyType; // Get max VALUE (not handle!) & remove it from Container

    function AddPair(key: TCurrencyType; Value: variant):TCHandle;

    // stack & deque operations
    function Front: TCurrencyType;             // examine first element
    function Back: TCurrencyType;              // examine last element
    function Push(v: TCurrencyType): TCHandle; // insert element at front
    function Pop: TCurrencyType;               // remove first element
    function PushBack(v: TCurrencyType): TCHandle; // insert element at back
    function PopBack: TCurrencyType;           // remove last element

    // queue operations
    function Enqueue(v: TCurrencyType): TCHandle;    // add to tail
    function Dequeue: TCurrencyType;       // read&remove from head

    procedure Sort(compare: TOnCurrencyCompare); overload;
    procedure Sort(compare: TOnCurrencyCompare; AFirst, ALast: TCHandle); overload;
    procedure Sort(compare: TOnCurrencyCompareProc); overload;
    procedure Sort(compare: TOnCurrencyCompareProc; AFirst, ALast: TCHandle); overload;

    property Map[key: TCurrencyType]: variant Read GetMap Write PutMap;
    property Items[handle: TCHandle]: TCurrencyType Read GetItem Write SetItem; default;
    property OnCompare: TOnCurrencyCompare Read fOnCompare Write SetOnCompare;
    property OnCompareProc: TOnCurrencyCompareProc Read fOnCompareProc Write SetOnCompareProc;
  end;
  TTCurrencyField = TTCurrency;

  TExtendedType = Extended;
  TOnExtendedCompare = function(a, b: TExtendedType): integer of object;
  TOnExtendedCompareProc = function(a, b: TExtendedType): integer;

  TTExtended = class(TTCustom)
  protected
    FOnCompare: TOnExtendedCompare;
    FOnCompareProc: TOnExtendedCompareProc;

    function GetItem(n: TCHandle): TExtendedType;
    procedure SetItem(n: TCHandle; Value: TExtendedType);
    function GetAsVariant(handle: TCHandle): variant; override;
    procedure SetAsVariant(handle: TCHandle; Value: variant); override;
    function GetAsAnsiStr(handle: TCHandle): AnsiString; override;
    procedure SetAsAnsiStr(handle: TCHandle; Value: AnsiString); override;
    function GetAsWideStr(handle: TCHandle): WideString; override;
    procedure SetAsWideStr(handle: TCHandle; Value: WideString); override;
    function GetHandleOf(Value: variant): TCHandle; override;
    procedure InitContainer(c: TCCustom); override;
    procedure SetOnCompare(c: TOnExtendedCompare);
    procedure SetOnCompareProc(c: TOnExtendedCompareProc);
    function CmpInt(var a, b): integer;
    function CmpExt(var a, b): integer;
    function CmpExtProc(var a, b): integer;
    function STCopy(Dst: TTCustom; h: TCHandle):TCHandle; override;
    procedure STCopy(Dst: TTCustom; AFirst, ALast: TCHandle); override;
    procedure STSet(Dst: TTCustom; h1,h2: TCHandle); override;
    procedure STSet(Dst: TTCustom; start1,finish1, start2: TCHandle); override;

    function GetMap(key: TExtendedType): variant;
    procedure PutMap(key: TExtendedType; Value: variant);

  public
    constructor Create(Container: TCCustom = nil); override;

    function Add(Item: TExtendedType): TCHandle; overload;
    function AddList(const Items: array of const): TCHandle; override;
    function AddTo(handle: TCHandle; dst: TTCustom): TCHandle; override;
    function Insert(handle: TCHandle; Item: TExtendedType): TCHandle;
    function InsertAfter(handle: TCHandle; Item: TExtendedType): TCHandle;

    procedure Remove(Item: TExtendedType);
    procedure RemoveAll(Item: TExtendedType);

    function Find(Item: TExtendedType): TCHandle;
    function FindFirstEqual(Item: TExtendedType): TCHandle;
    procedure FindNextEqual(Item: TExtendedType; var handle: TCHandle);

    function GetMinValue: TExtendedType;    // Get min VALUE (not handle!)
    function RemoveMinValue: TExtendedType; // Get min VALUE (not handle!) & remove it from Container
    function GetMaxValue: TExtendedType;    // Get max VALUE (not handle!)
    function RemoveMaxValue: TExtendedType; // Get max VALUE (not handle!) & remove it from Container

    function AddPair(key: TExtendedType; Value: variant):TCHandle;

    // stack & deque operations
    function Front: TExtendedType;          // examine first element
    function Back: TExtendedType;           // examine last element
    function Push(v: TExtendedType): TCHandle;        // insert element at front
    function Pop: TExtendedType;            // remove first element
    function PushBack(v: TExtendedType): TCHandle;    // insert element at back
    function PopBack: TExtendedType;        // remove last element

    // queue operations
    function Enqueue(v: TExtendedType): TCHandle;    // add to tail
    function Dequeue: TExtendedType;       // read&remove from head

    procedure Sort(compare: TOnExtendedCompare); overload;
    procedure Sort(compare: TOnExtendedCompare; AFirst, ALast: TCHandle); overload;
    procedure Sort(compare: TOnExtendedCompareProc); overload;
    procedure Sort(compare: TOnExtendedCompareProc; AFirst, ALast: TCHandle); overload;

    property Map[key: TExtendedType]: variant Read GetMap Write PutMap;
    property Items[handle: TCHandle]: TExtendedType Read GetItem Write SetItem; default;
    property OnCompare: TOnExtendedCompare Read fOnCompare Write SetOnCompare;
    property OnCompareProc: TOnExtendedComparePRoc Read fOnCompareProc Write SetOnCompareProc;
  end;
  TTExtendedField = TTExtended;

  TLongwordType = Longword;
  TOnLongwordCompare = function(a, b: TLongwordType): integer of object;
  TOnLongwordCompareProc = function(a, b: TLongwordType): integer;

  TTLongword = class(TTCustom)
  protected
    FOnCompare: TOnLongwordCompare;
    FOnCompareProc: TOnLongwordCompareProc;

    function GetItem(n: TCHandle): TLongwordType;
    procedure SetItem(n: TCHandle; Value: TLongwordType);
    function GetAsVariant(handle: TCHandle): variant; override;
    procedure SetAsVariant(handle: TCHandle; Value: variant); override;
    function GetHandleOf(Value: variant): TCHandle; override;
    function GetAsAnsiStr(handle: TCHandle): AnsiString; override;
    procedure SetAsAnsiStr(handle: TCHandle; Value: AnsiString); override;
    function GetAsWideStr(handle: TCHandle): WideString; override;
    procedure SetAsWideStr(handle: TCHandle; Value: WideString); override;
    procedure InitContainer(c: TCCustom); override;
    procedure SetOnCompare(c: TOnLongwordCompare);
    procedure SetOnCompareProc(c: TOnLongwordCompareProc);
    function CmpInt(var a, b): integer;
    function CmpExt(var a, b): integer;
    function CmpExtProc(var a, b): integer;
    function STCopy(Dst: TTCustom; h: TCHandle):TCHandle; override;
    procedure STCopy(Dst: TTCustom; AFirst, ALast: TCHandle); override;
    procedure STSet(Dst: TTCustom; h1,h2: TCHandle); override;
    procedure STSet(Dst: TTCustom; start1,finish1, start2: TCHandle); override;
    function GetMap(key: TLongwordType): variant;
    procedure PutMap(key: TLongwordType; Value: variant);

  public
    constructor Create(Container: TCCustom = nil); override;

    function Add(Item: TLongwordType): TCHandle; overload;
    function AddList(const Items: array of const): TCHandle; override;
    function AddTo(handle: TCHandle; dst: TTCustom): TCHandle; override;
    function Insert(handle: TCHandle; Item: TLongwordType): TCHandle;
    function InsertAfter(handle: TCHandle; Item: TLongwordType): TCHandle;

    procedure Remove(Item: TLongwordType);
    procedure RemoveAll(Item: TLongwordType);

    function Find(Item: TLongwordType): TCHandle;
    function FindFirstEqual(Item: TLongwordType): TCHandle;
    procedure FindNextEqual(Item: TLongwordType; var handle: TCHandle);

    function GetMinValue: TLongwordType;    // Get min VALUE (not handle!)
    function RemoveMinValue: TLongwordType; // Get min VALUE (not handle!) & remove it from Container
    function GetMaxValue: TLongwordType;    // Get max VALUE (not handle!)
    function RemoveMaxValue: TLongwordType; // Get max VALUE (not handle!) & remove it from Container

    function AddPair(key: TLongwordType; Value: variant):TCHandle;

    // stack & deque operations
    function Front: TLongwordType;             // examine first element
    function Back: TLongwordType;              // examine last element
    function Push(v: TLongwordType): TCHandle; // insert element at front
    function Pop: TLongwordType;               // remove first element
    function PushBack(v: TLongwordType): TCHandle; // insert element at back
    function PopBack: TLongwordType;           // remove last element

    // queue operations
    function Enqueue(v: TLongwordType): TCHandle;    // add to tail
    function Dequeue: TLongwordType;       // read&remove from head

    procedure Sort(compare: TOnLongwordCompare); overload;
    procedure Sort(compare: TOnLongwordCompare; AFirst, ALast: TCHandle); overload;
    procedure Sort(compare: TOnLongwordCompareProc); overload;
    procedure Sort(compare: TOnLongwordCompareProc; AFirst, ALast: TCHandle); overload;

    property Map[key: TLongwordType]: variant Read GetMap Write PutMap;
    property Items[handle: TCHandle]: TLongwordType Read GetItem Write SetItem; default;
    property OnCompare: TOnLongwordCompare Read fOnCompare Write SetOnCompare;
    property OnCompareProc: TOnLongwordCompareProc Read fOnCompareProc Write SetOnCompareProc;
  end;
  TTLongwordField = TTLongword;

  {$IFNDEF FPC}
  TCardinalType = TLongwordType;
  TOnCardinalCompare = TOnLongwordCompare;
  TOnCardinalCompareProc = TOnLongwordCompareProc;
  TTCardinal=TTLongword;
  {$ELSE}

  TCardinalType = Cardinal;
  TOnCardinalCompare = function(a, b: TCardinalType): integer of object;
  TOnCardinalCompareProc = function(a, b: TCardinalType): integer;

  TTCardinal = class(TTCustom)
  protected
    FOnCompare: TOnCardinalCompare;
    FOnCompareProc: TOnCardinalCompareProc;

    function GetItem(n: TCHandle): TCardinalType;
    procedure SetItem(n: TCHandle; Value: TCardinalType);
    function GetAsVariant(handle: TCHandle): variant; override;
    procedure SetAsVariant(handle: TCHandle; Value: variant); override;
    function GetHandleOf(Value: variant): TCHandle; override;
    function GetAsAnsiStr(handle: TCHandle): AnsiString; override;
    procedure SetAsAnsiStr(handle: TCHandle; Value: AnsiString); override;
    function GetAsWideStr(handle: TCHandle): WideString; override;
    procedure SetAsWideStr(handle: TCHandle; Value: WideString); override;
    procedure InitContainer(c: TCCustom); override;
    procedure SetOnCompare(c: TOnCardinalCompare);
    procedure SetOnCompareProc(c: TOnCardinalCompareProc);
    function CmpInt(var a, b): integer;
    function CmpExt(var a, b): integer;
    function CmpExtProc(var a, b): integer;
    function STCopy(Dst: TTCustom; h: TCHandle):TCHandle; override;
    procedure STCopy(Dst: TTCustom; AFirst, ALast: TCHandle); override;
    procedure STSet(Dst: TTCustom; h1,h2: TCHandle); override;
    procedure STSet(Dst: TTCustom; start1,finish1, start2: TCHandle); override;
    function GetMap(key: TCardinalType): variant;
    procedure PutMap(key: TCardinalType; Value: variant);

  public
    constructor Create(Container: TCCustom = nil); override;

    function Add(Item: TCardinalType): TCHandle; overload;
    function AddList(const Items: array of const): TCHandle; override;
    function AddTo(handle: TCHandle; dst: TTCustom): TCHandle; override;
    function Insert(handle: TCHandle; Item: TCardinalType): TCHandle;
    function InsertAfter(handle: TCHandle; Item: TCardinalType): TCHandle;

    procedure Remove(Item: TCardinalType);
    procedure RemoveAll(Item: TCardinalType);

    function Find(Item: TCardinalType): TCHandle;
    function FindFirstEqual(Item: TCardinalType): TCHandle;
    procedure FindNextEqual(Item: TCardinalType; var handle: TCHandle);

    function GetMinValue: TCardinalType;    // Get min VALUE (not handle!)
    function RemoveMinValue: TCardinalType; // Get min VALUE (not handle!) & remove it from Container
    function GetMaxValue: TCardinalType;    // Get max VALUE (not handle!)
    function RemoveMaxValue: TCardinalType; // Get max VALUE (not handle!) & remove it from Container

    function AddPair(key: TCardinalType; Value: variant):TCHandle;

    // stack & deque operations
    function Front: TCardinalType;             // examine first element
    function Back: TCardinalType;              // examine last element
    function Push(v: TCardinalType): TCHandle; // insert element at front
    function Pop: TCardinalType;               // remove first element
    function PushBack(v: TCardinalType): TCHandle; // insert element at back
    function PopBack: TCardinalType;           // remove last element

    // queue operations
    function Enqueue(v: TCardinalType): TCHandle;    // add to tail
    function Dequeue: TCardinalType;       // read&remove from head

    procedure Sort(compare: TOnCardinalCompare); overload;
    procedure Sort(compare: TOnCardinalCompare; AFirst, ALast: TCHandle); overload;
    procedure Sort(compare: TOnCardinalCompareProc); overload;
    procedure Sort(compare: TOnCardinalCompareProc; AFirst, ALast: TCHandle); overload;

    property Map[key: TCardinalType]: variant Read GetMap Write PutMap;
    property Items[handle: TCHandle]: TCardinalType Read GetItem Write SetItem; default;
    property OnCompare: TOnCardinalCompare Read fOnCompare Write SetOnCompare;
    property OnCompareProc: TOnCardinalCompareProc Read fOnCompareProc Write SetOnCompareProc;
  end;
  {$ENDIF}
  TTCardinalField = TTCardinal;

  TTDWord = class(TTLongword);
  TTDWordField = TTDWord;

  TSmallintType = Smallint;
  TOnSmallintCompare = function(a, b: TSmallintType): integer of object;
  TOnSmallintCompareProc = function(a, b: TSmallintType): integer;

  TTSmallint = class(TTCustom)
  protected
    FOnCompare: TOnSmallintCompare;
    FOnCompareProc: TOnSmallintCompareProc;

    function GetItem(n: TCHandle): TSmallintType;
    procedure SetItem(n: TCHandle; Value: TSmallintType);
    function GetAsVariant(handle: TCHandle): variant; override;
    procedure SetAsVariant(handle: TCHandle; Value: variant); override;
    function GetHandleOf(Value: variant): TCHandle; override;
    function GetAsAnsiStr(handle: TCHandle): AnsiString; override;
    procedure SetAsAnsiStr(handle: TCHandle; Value: AnsiString); override;
    function GetAsWideStr(handle: TCHandle): WideString; override;
    procedure SetAsWideStr(handle: TCHandle; Value: WideString); override;
    procedure InitContainer(c: TCCustom); override;
    procedure SetOnCompare(c: TOnSmallintCompare);
    procedure SetOnCompareProc(c: TOnSmallintCompareProc);
    function CmpInt(var a, b): integer;
    function CmpExt(var a, b): integer;
    function CmpExtProc(var a, b): integer;
    function STCopy(Dst: TTCustom; h: TCHandle):TCHandle; override;
    procedure STCopy(Dst: TTCustom; AFirst, ALast: TCHandle); override;
    procedure STSet(Dst: TTCustom; h1,h2: TCHandle); override;
    procedure STSet(Dst: TTCustom; start1,finish1, start2: TCHandle); override;
    function GetMap(key: TSmallintType): variant;
    procedure PutMap(key: TSmallintType; Value: variant);

  public
    constructor Create(Container: TCCustom = nil); override;

    function Add(Item: TSmallintType): TCHandle; overload;
    function AddList(const Items: array of const): TCHandle; override;
    function AddTo(handle: TCHandle; dst: TTCustom): TCHandle; override;
    function Insert(handle: TCHandle; Item: TSmallintType): TCHandle;
    function InsertAfter(handle: TCHandle; Item: TSmallintType): TCHandle;

    procedure Remove(Item: TSmallintType);
    procedure RemoveAll(Item: TSmallintType);

    function Find(Item: TSmallintType): TCHandle;
    function FindFirstEqual(Item: TSmallintType): TCHandle;
    procedure FindNextEqual(Item: TSmallintType; var handle: TCHandle);

    function GetMinValue: TSmallintType;    // Get min VALUE (not handle!)
    function RemoveMinValue: TSmallintType; // Get min VALUE (not handle!) & remove it from Container
    function GetMaxValue: TSmallintType;    // Get max VALUE (not handle!)
    function RemoveMaxValue: TSmallintType; // Get max VALUE (not handle!) & remove it from Container

    function AddPair(key: TSmallintType; Value: variant):TCHandle;

    // stack & deque operations
    function Front: TSmallintType;             // examine first element
    function Back: TSmallintType;              // examine last element
    function Push(v: TSmallintType): TCHandle; // insert element at front
    function Pop: TSmallintType;               // remove first element
    function PushBack(v: TSmallintType): TCHandle; // insert element at back
    function PopBack: TSmallintType;           // remove last element

    // queue operations
    function Enqueue(v: TSmallintType): TCHandle;    // add to tail
    function Dequeue: TSmallintType;       // read&remove from head

    procedure Sort(compare: TOnSmallintCompare); overload;
    procedure Sort(compare: TOnSmallintCompare; AFirst, ALast: TCHandle); overload;
    procedure Sort(compare: TOnSmallintCompareProc); overload;
    procedure Sort(compare: TOnSmallintCompareProc; AFirst, ALast: TCHandle); overload;

    property Map[key: TSmallintType]: variant Read GetMap Write PutMap;
    property Items[handle: TCHandle]: TSmallintType Read GetItem Write SetItem; default;
    property OnCompare: TOnSmallintCompare Read fOnCompare Write SetOnCompare;
    property OnCompareProc: TOnSmallintCompareProc Read fOnCompareProc Write SetOnCompareProc;
  end;
  TTSmallintField = TTSmallint;

  TShortintType = Shortint;
  TOnShortintCompare = function(a, b: TShortintType): integer of object;
  TOnShortintCompareProc = function(a, b: TShortintType): integer;

  TTShortint = class(TTCustom)
  protected
    FOnCompare: TOnShortintCompare;
    FOnCompareProc: TOnShortintCompareProc;

    function GetItem(n: TCHandle): TShortintType;
    procedure SetItem(n: TCHandle; Value: TShortintType);
    function GetAsVariant(handle: TCHandle): variant; override;
    procedure SetAsVariant(handle: TCHandle; Value: variant); override;
    function GetHandleOf(Value: variant): TCHandle; override;
    function GetAsAnsiStr(handle: TCHandle): AnsiString; override;
    procedure SetAsAnsiStr(handle: TCHandle; Value: AnsiString); override;
    function GetAsWideStr(handle: TCHandle): WideString; override;
    procedure SetAsWideStr(handle: TCHandle; Value: WideString); override;
    procedure InitContainer(c: TCCustom); override;
    procedure SetOnCompare(c: TOnShortintCompare);
    procedure SetOnCompareProc(c: TOnShortintCompareProc);
    function CmpInt(var a, b): integer;
    function CmpExt(var a, b): integer;
    function CmpExtProc(var a, b): integer;
    function STCopy(Dst: TTCustom; h: TCHandle):TCHandle; override;
    procedure STCopy(Dst: TTCustom; AFirst, ALast: TCHandle); override;
    procedure STSet(Dst: TTCustom; h1,h2: TCHandle); override;
    procedure STSet(Dst: TTCustom; start1,finish1, start2: TCHandle); override;
    function GetMap(key: TShortintType): variant;
    procedure PutMap(key: TShortintType; Value: variant);

  public
    constructor Create(Container: TCCustom = nil); override;

    function Add(Item: TShortintType): TCHandle; overload;
    function AddList(const Items: array of const): TCHandle; override;
    function AddTo(handle: TCHandle; dst: TTCustom): TCHandle; override;
    function Insert(handle: TCHandle; Item: TShortintType): TCHandle;
    function InsertAfter(handle: TCHandle; Item: TShortintType): TCHandle;

    procedure Remove(Item: TShortintType);
    procedure RemoveAll(Item: TShortintType);

    function Find(Item: TShortintType): TCHandle;
    function FindFirstEqual(Item: TShortintType): TCHandle;
    procedure FindNextEqual(Item: TShortintType; var handle: TCHandle);

    function GetMinValue: TShortintType;    // Get min VALUE (not handle!)
    function RemoveMinValue: TShortintType; // Get min VALUE (not handle!) & remove it from Container
    function GetMaxValue: TShortintType;    // Get max VALUE (not handle!)
    function RemoveMaxValue: TShortintType; // Get max VALUE (not handle!) & remove it from Container

    function AddPair(key: TShortintType; Value: variant):TCHandle;

    // stack & deque operations
    function Front: TShortintType;             // examine first element
    function Back: TShortintType;              // examine last element
    function Push(v: TShortintType): TCHandle; // insert element at front
    function Pop: TShortintType;               // remove first element
    function PushBack(v: TShortintType): TCHandle; // insert element at back
    function PopBack: TShortintType;           // remove last element

    // queue operations
    function Enqueue(v: TShortintType): TCHandle;    // add to tail
    function Dequeue: TShortintType;       // read&remove from head

    procedure Sort(compare: TOnShortintCompare); overload;
    procedure Sort(compare: TOnShortintCompare; AFirst, ALast: TCHandle); overload;
    procedure Sort(compare: TOnShortintCompareProc); overload;
    procedure Sort(compare: TOnShortintCompareProc; AFirst, ALast: TCHandle); overload;

    property Map[key: TShortintType]: variant Read GetMap Write PutMap;
    property Items[handle: TCHandle]: TShortintType Read GetItem Write SetItem; default;
    property OnCompare: TOnShortintCompare Read fOnCompare Write SetOnCompare;
    property OnCompareProc: TOnShortintCompareProc Read fOnCompareProc Write SetOnCompareProc;
  end;
  TTShortintField = TTShortint;

  TLongintType = Longint;
  TOnLongintCompare = function(a, b: TLongintType): integer of object;
  TOnLongintCompareProc = function(a, b: TLongintType): integer;

  TTLongint = class(TTCustom)
  protected
    FOnCompare: TOnLongintCompare;
    FOnCompareProc: TOnLongintCompareProc;

    function GetItem(n: TCHandle): TLongintType;
    procedure SetItem(n: TCHandle; Value: TLongintType);
    function GetAsVariant(handle: TCHandle): variant; override;
    procedure SetAsVariant(handle: TCHandle; Value: variant); override;
    function GetHandleOf(Value: variant): TCHandle; override;
    function GetAsAnsiStr(handle: TCHandle): AnsiString; override;
    procedure SetAsAnsiStr(handle: TCHandle; Value: AnsiString); override;
    function GetAsWideStr(handle: TCHandle): WideString; override;
    procedure SetAsWideStr(handle: TCHandle; Value: WideString); override;
    procedure InitContainer(c: TCCustom); override;
    procedure SetOnCompare(c: TOnLongintCompare);
    procedure SetOnCompareProc(c: TOnLongintCompareProc);
    function CmpInt(var a, b): integer;
    function CmpExt(var a, b): integer;
    function CmpExtProc(var a, b): integer;
    function STCopy(Dst: TTCustom; h: TCHandle):TCHandle; override;
    procedure STCopy(Dst: TTCustom; AFirst, ALast: TCHandle); override;
    procedure STSet(Dst: TTCustom; h1,h2: TCHandle); override;
    procedure STSet(Dst: TTCustom; start1,finish1, start2: TCHandle); override;
    function GetMap(key: TLongintType): variant;
    procedure PutMap(key: TLongintType; Value: variant);

  public
    constructor Create(Container: TCCustom = nil); override;

    function Add(Item: TLongintType): TCHandle; overload;
    function AddList(const Items: array of const): TCHandle; override;
    function AddTo(handle: TCHandle; dst: TTCustom): TCHandle; override;
    function Insert(handle: TCHandle; Item: TLongintType): TCHandle;
    function InsertAfter(handle: TCHandle; Item: TLongintType): TCHandle;

    procedure Remove(Item: TLongintType);
    procedure RemoveAll(Item: TLongintType);

    function Find(Item: TLongintType): TCHandle;
    function FindFirstEqual(Item: TLongintType): TCHandle;
    procedure FindNextEqual(Item: TLongintType; var handle: TCHandle);

    function GetMinValue: TLongintType;    // Get min VALUE (not handle!)
    function RemoveMinValue: TLongintType; // Get min VALUE (not handle!) & remove it from Container
    function GetMaxValue: TLongintType;    // Get max VALUE (not handle!)
    function RemoveMaxValue: TLongintType; // Get max VALUE (not handle!) & remove it from Container

    function AddPair(key: TLongintType; Value: variant):TCHandle;

    // stack & deque operations
    function Front: TLongintType;             // examine first element
    function Back: TLongintType;              // examine last element
    function Push(v: TLongintType): TCHandle; // insert element at front
    function Pop: TLongintType;               // remove first element
    function PushBack(v: TLongintType): TCHandle; // insert element at back
    function PopBack: TLongintType;           // remove last element

    // queue operations
    function Enqueue(v: TLongintType): TCHandle;    // add to tail
    function Dequeue: TLongintType;       // read&remove from head

    procedure Sort(compare: TOnLongintCompare); overload;
    procedure Sort(compare: TOnLongintCompare; AFirst, ALast: TCHandle); overload;
    procedure Sort(compare: TOnLongintCompareProc); overload;
    procedure Sort(compare: TOnLongintCompareProc; AFirst, ALast: TCHandle); overload;

    property Map[key: TLongintType]: variant Read GetMap Write PutMap;
    property Items[handle: TCHandle]: TLongintType Read GetItem Write SetItem; default;
    property OnCompare: TOnLongintCompare Read fOnCompare Write SetOnCompare;
    property OnCompareProc: TOnLongintCompareProc Read fOnCompareProc Write SetOnCompareProc;
  end;
  TTLongintField = TTLongint;

  {$IFNDEF FPC}
  TIntegerType = TLongintType;
  TOnIntegerCompare = TOnLongintCompare;
  TOnIntegerCompareProc = TOnLongintCompareProc;
  TTInteger = TTLongint;
  {$ELSE}
  TIntegerType = Integer;
  TOnIntegerCompare = function(a, b: TIntegerType): integer of object;
  TOnIntegerCompareProc = function(a, b: TIntegerType): integer;

  TTInteger = class(TTCustom)
  protected
    FOnCompare: TOnIntegerCompare;
    FOnCompareProc: TOnIntegerCompareProc;

    function GetItem(n: TCHandle): TIntegerType;
    procedure SetItem(n: TCHandle; Value: TIntegerType);
    function GetAsVariant(handle: TCHandle): variant; override;
    procedure SetAsVariant(handle: TCHandle; Value: variant); override;
    function GetHandleOf(Value: variant): TCHandle; override;
    function GetAsAnsiStr(handle: TCHandle): AnsiString; override;
    procedure SetAsAnsiStr(handle: TCHandle; Value: AnsiString); override;
    function GetAsWideStr(handle: TCHandle): WideString; override;
    procedure SetAsWideStr(handle: TCHandle; Value: WideString); override;
    procedure InitContainer(c: TCCustom); override;
    procedure SetOnCompare(c: TOnIntegerCompare);
    procedure SetOnCompareProc(c: TOnIntegerCompareProc);
    function CmpInt(var a, b): integer;
    function CmpExt(var a, b): integer;
    function CmpExtProc(var a, b): integer;
    function STCopy(Dst: TTCustom; h: TCHandle):TCHandle; override;
    procedure STCopy(Dst: TTCustom; AFirst, ALast: TCHandle); override;
    procedure STSet(Dst: TTCustom; h1,h2: TCHandle); override;
    procedure STSet(Dst: TTCustom; start1,finish1, start2: TCHandle); override;
    function GetMap(key: TIntegerType): variant;
    procedure PutMap(key: TIntegerType; Value: variant);

  public
    constructor Create(Container: TCCustom = nil); override;

    function Add(Item: TIntegerType): TCHandle; overload;
    function AddList(const Items: array of const): TCHandle; override;
    function AddTo(handle: TCHandle; dst: TTCustom): TCHandle; override;
    function Insert(handle: TCHandle; Item: TIntegerType): TCHandle;
    function InsertAfter(handle: TCHandle; Item: TIntegerType): TCHandle;

    procedure Remove(Item: TIntegerType);
    procedure RemoveAll(Item: TIntegerType);

    function Find(Item: TIntegerType): TCHandle;
    function FindFirstEqual(Item: TIntegerType): TCHandle;
    procedure FindNextEqual(Item: TIntegerType; var handle: TCHandle);

    function GetMinValue: TIntegerType;    // Get min VALUE (not handle!)
    function RemoveMinValue: TIntegerType; // Get min VALUE (not handle!) & remove it from Container
    function GetMaxValue: TIntegerType;    // Get max VALUE (not handle!)
    function RemoveMaxValue: TIntegerType; // Get max VALUE (not handle!) & remove it from Container

    function AddPair(key: TIntegerType; Value: variant):TCHandle;

    // stack & deque operations
    function Front: TIntegerType;             // examine first element
    function Back: TIntegerType;              // examine last element
    function Push(v: TIntegerType): TCHandle; // insert element at front
    function Pop: TIntegerType;               // remove first element
    function PushBack(v: TIntegerType): TCHandle; // insert element at back
    function PopBack: TIntegerType;           // remove last element

    // queue operations
    function Enqueue(v: TIntegerType): TCHandle;    // add to tail
    function Dequeue: TIntegerType;       // read&remove from head

    procedure Sort(compare: TOnIntegerCompare); overload;
    procedure Sort(compare: TOnIntegerCompare; AFirst, ALast: TCHandle); overload;
    procedure Sort(compare: TOnIntegerCompareProc); overload;
    procedure Sort(compare: TOnIntegerCompareProc; AFirst, ALast: TCHandle); overload;

    property Map[key: TIntegerType]: variant Read GetMap Write PutMap;
    property Items[handle: TCHandle]: TIntegerType Read GetItem Write SetItem; default;
    property OnCompare: TOnIntegerCompare Read fOnCompare Write SetOnCompare;
    property OnCompareProc: TOnIntegerCompareProc Read fOnCompareProc Write SetOnCompareProc;
  end;

  {$ENDIF}
  TTIntegerField = TTInteger;

  TTHandle = class(TTCardinal)
  protected
    procedure ItemDeleted(n: TCHandle); override;
  public
    constructor Create(Container: TCCustom = nil); override;
  end;
  TTHandleField = TTHandle;

  TTHIcon = class(TTCardinal)
  protected
    procedure ItemDeleted(n: TCHandle); override;
  public
    constructor Create(Container: TCCustom = nil); override;
  end;
  TTHIconField = TTHIcon;

  TPointType = TPoint;
  TOnPointCompare = function(a, b: TPointType): integer of object;
  TOnPointCompareProc = function(a, b: TPointType): integer;

  TTPoint = class(TTCustom)
  protected
    FOnCompare: TOnPointCompare;
    FOnCompareProc: TOnPointCompareProc;
    FComplete, FPos: integer;

    function GetItem(n: TCHandle): TPointType;
    procedure SetItem(n: TCHandle; Value: TPointType);
    function GetAsVariant(handle: TCHandle): variant; override;
    procedure SetAsVariant(handle: TCHandle; Value: variant); override;
    function GetHandleOf(Value: variant): TCHandle; override;
    function GetAsAnsiStr(handle: TCHandle): AnsiString; override;
    procedure SetAsAnsiStr(handle: TCHandle; Value: AnsiString); override;
    function GetAsWideStr(handle: TCHandle): WideString; override;
    procedure SetAsWideStr(handle: TCHandle; Value: WideString); override;
    function LoadItem(c: TCCustom; h: TCHandle; src: TStream; FileFormat : TFileFormat = ffDefault):TCHandle; override;
    procedure InitContainer(c: TCCustom); override;
    procedure SetOnCompare(c: TOnPointCompare);
    procedure SetOnCompareProc(c: TOnPointCompareProc);
    function CmpInt(var a, b): integer;
    function CmpExt(var a, b): integer;
    function CmpExtProc(var a, b): integer;
    function STCopy(Dst: TTCustom; h: TCHandle):TCHandle; override;
    procedure STCopy(Dst: TTCustom; AFirst, ALast: TCHandle); override;
    procedure STSet(Dst: TTCustom; h1,h2: TCHandle); override;
    procedure STSet(Dst: TTCustom; start1,finish1, start2: TCHandle); override;

    function GetMap(key: TPointType): variant;
    procedure PutMap(key: TPointType; Value: variant);

  public
    constructor Create(Container: TCCustom = nil); override;

    function Add(Item: TPointType): TCHandle; overload;
    function Add(x,y: integer): TCHandle; overload;
    function AddList(const AItems: array of const): TCHandle; override;
    function AddTo(handle: TCHandle; dst: TTCustom): TCHandle; override;
    function Insert(handle: TCHandle; Item: TPointType): TCHandle;
    function InsertAfter(handle: TCHandle; Item: TPointType): TCHandle;

    procedure Remove(Item: TPointType);
    procedure RemoveAll(Item: TPointType);

    function Find(Item: TPointType): TCHandle;
    function FindFirstEqual(Item: TPointType): TCHandle;
    procedure FindNextEqual(Item: TPointType; var handle: TCHandle);

    function GetMinValue: TPointType;    // Get min VALUE (not handle!)
    function RemoveMinValue: TPointType; // Get min VALUE (not handle!) & remove it from Container
    function GetMaxValue: TPointType;    // Get max VALUE (not handle!)
    function RemoveMaxValue: TPointType; // Get max VALUE (not handle!) & remove it from Container

    function AddPair(key: TPointType; Value: variant):TCHandle;

    // stack & deque operations
    function Front: TPointType;          // examine first element
    function Back: TPointType;           // examine last element
    function Push(v: TPointType): TCHandle;        // insert element at front
    function Pop: TPointType;            // remove first element
    function PushBack(v: TPointType): TCHandle;    // insert element at back
    function PopBack: TPointType;        // remove last element

    // queue operations
    function Enqueue(v: TPointType): TCHandle;    // add to tail
    function Dequeue: TPointType;       // read&remove from head

    procedure Sort(compare: TOnPointCompare); overload;
    procedure Sort(compare: TOnPointCompare; AFirst, ALast: TCHandle); overload;
    procedure Sort(compare: TOnPointCompareProc); overload;
    procedure Sort(compare: TOnPointCompareProc; AFirst, ALast: TCHandle); overload;

    property Map[key: TPointType]: variant Read GetMap Write PutMap;
    property Items[handle: TCHandle]: TPointType Read GetItem Write SetItem; default;
    property OnCompare: TOnPointCompare Read fOnCompare Write SetOnCompare;
    property OnCompareProc: TOnPointCompareProc Read fOnCompareProc Write SetOnCompareProc;
  end;
  TTPointField = TTPoint;

  TFPoint = packed record
    x,y: double;
  end;
  TFPointType = TFPoint;
  TOnFPointCompare = function(a, b: TFPointType): integer of object;
  TOnFPointCompareProc = function(a, b: TFPointType): integer;

  TTFPoint = class(TTCustom)
  protected
    FOnCompare: TOnFPointCompare;
    FOnCompareProc: TOnFPointCompareProc;
    FComplete, FPos: integer;

    function GetItem(n: TCHandle): TFPointType;
    procedure SetItem(n: TCHandle; Value: TFPointType);
    function GetAsVariant(handle: TCHandle): variant; override;
    procedure SetAsVariant(handle: TCHandle; Value: variant); override;
    function GetHandleOf(Value: variant): TCHandle; override;
    function GetAsAnsiStr(handle: TCHandle): AnsiString; override;
    procedure SetAsAnsiStr(handle: TCHandle; Value: AnsiString); override;
    function GetAsWideStr(handle: TCHandle): WideString; override;
    procedure SetAsWideStr(handle: TCHandle; Value: WideString); override;
    function LoadItem(c: TCCustom; h: TCHandle; src: TStream; FileFormat : TFileFormat = ffDefault):TCHandle; override;
    procedure InitContainer(c: TCCustom); override;
    procedure SetOnCompare(c: TOnFPointCompare);
    procedure SetOnCompareProc(c: TOnFPointCompareProc);
    function CmpInt(var a, b): integer;
    function CmpExt(var a, b): integer;
    function CmpExtProc(var a, b): integer;
    function STCopy(Dst: TTCustom; h: TCHandle):TCHandle; override;
    procedure STCopy(Dst: TTCustom; AFirst, ALast: TCHandle); override;
    procedure STSet(Dst: TTCustom; h1,h2: TCHandle); override;
    procedure STSet(Dst: TTCustom; start1,finish1, start2: TCHandle); override;

    function GetMap(key: TFPointType): variant;
    procedure PutMap(key: TFPointType; Value: variant);

  public
    constructor Create(Container: TCCustom = nil); override;

    function Add(Item: TFPointType): TCHandle; overload;
    function AddList(const AItems: array of const): TCHandle; override;
    function AddTo(handle: TCHandle; dst: TTCustom): TCHandle; override;
    function Insert(handle: TCHandle; Item: TFPointType): TCHandle;
    function InsertAfter(handle: TCHandle; Item: TFPointType): TCHandle;

    procedure Remove(Item: TFPointType);
    procedure RemoveAll(Item: TFPointType);

    function Find(Item: TFPointType): TCHandle;
    function FindFirstEqual(Item: TFPointType): TCHandle;
    procedure FindNextEqual(Item: TFPointType; var handle: TCHandle);

    function GetMinValue: TFPointType;    // Get min VALUE (not handle!)
    function RemoveMinValue: TFPointType; // Get min VALUE (not handle!) & remove it from Container
    function GetMaxValue: TFPointType;    // Get max VALUE (not handle!)
    function RemoveMaxValue: TFPointType; // Get max VALUE (not handle!) & remove it from Container

    function AddPair(key: TFPointType; Value: variant):TCHandle;

    // stack & deque operations
    function Front: TFPointType;          // examine first element
    function Back: TFPointType;           // examine last element
    function Push(v: TFPointType): TCHandle;        // insert element at front
    function Pop: TFPointType;            // remove first element
    function PushBack(v: TFPointType): TCHandle;    // insert element at back
    function PopBack: TFPointType;        // remove last element

    // queue operations
    function Enqueue(v: TFPointType): TCHandle;    // add to tail
    function Dequeue: TFPointType;       // read&remove from head

    procedure Sort(compare: TOnFPointCompare); overload;
    procedure Sort(compare: TOnFPointCompare; AFirst, ALast: TCHandle); overload;
    procedure Sort(compare: TOnFPointCompareProc); overload;
    procedure Sort(compare: TOnFPointCompareProc; AFirst, ALast: TCHandle); overload;

    property Map[key: TFPointType]: variant Read GetMap Write PutMap;
    property Items[handle: TCHandle]: TFPointType Read GetItem Write SetItem; default;
    property OnCompare: TOnFPointCompare Read fOnCompare Write SetOnCompare;
    property OnCompareProc: TOnFPointCompareProc Read fOnCompareProc Write SetOnCompareProc;
  end;
  TTFPointField = TTFPoint;

  TPointerType = pointer;
  TOnPointerCompare = function(a, b: TPointerType): integer of object;
  TOnPointerCompareProc = function(a, b: TPointerType): integer;

  TTPointer = class(TTCustom)
  protected
    FOnCompare: TOnPointerCompare;
    FOnCompareProc: TOnPointerCompareProc;

    function GetItem(n: TCHandle): TPointerType;
    procedure SetItem(n: TCHandle; Value: TPointerType);
    function GetAsVariant(handle: TCHandle): variant; override;
    procedure SetAsVariant(handle: TCHandle; Value: variant); override;
    function GetAsAnsiStr(handle: TCHandle): AnsiString; override;
    procedure SetAsAnsiStr(handle: TCHandle; Value: AnsiString); override;
    function GetAsWideStr(handle: TCHandle): WideString; override;
    procedure SetAsWideStr(handle: TCHandle; Value: WideString); override;
    function GetHandleOf(Value: variant): TCHandle; override;
    procedure InitContainer(c: TCCustom); override;
    procedure SetOnCompare(c: TOnPointerCompare);
    procedure SetOnCompareProc(c: TOnPointerCompareProc);
    function CmpInt(var a, b): integer;
    function CmpExt(var a, b): integer;
    function CmpExtProc(var a, b): integer;
    function STCopy(Dst: TTCustom; h: TCHandle):TCHandle; override;
    procedure STCopy(Dst: TTCustom; AFirst, ALast: TCHandle); override;
    procedure STSet(Dst: TTCustom; h1,h2: TCHandle); override;
    procedure STSet(Dst: TTCustom; start1,finish1, start2: TCHandle); override;

    function GetMap(key: TPointerType): variant;
    procedure PutMap(key: TPointerType; Value: variant);

  public
    constructor Create(Container: TCCustom = nil); override;

    function Add(Item: TPointerType): TCHandle; overload;
    function AddList(const Items: array of const): TCHandle; override;
    function AddTo(handle: TCHandle; dst: TTCustom): TCHandle; override;
    function Insert(handle: TCHandle; Item: TPointerType): TCHandle;
    function InsertAfter(handle: TCHandle; Item: TPointerType): TCHandle;

    procedure Remove(Item: TPointerType);

    function Find(Item: TPointerType): TCHandle;
    function FindFirstEqual(Item: TPointerType): TCHandle;
    procedure FindNextEqual(Item: TPointerType; var handle: TCHandle);

    function GetMinValue: TPointerType;    // Get min VALUE (not handle!)
    function RemoveMinValue: TPointerType; // Get min VALUE (not handle!) & remove it from Container

    function AddPair(key: TPointerType; Value: variant):TCHandle;

    // stack & deque operations
    function Front: TPointerType;          // examine first element
    function Back: TPointerType;           // examine last element
    function Push(v: TPointerType): TCHandle;        // insert element at front
    function Pop: TPointerType;            // remove first element
    function PushBack(v: TPointerType): TCHandle;    // insert element at back
    function PopBack: TPointerType;        // remove last element

    // queue operations
    function Enqueue(v: TPointerType): TCHandle;    // add to tail
    function Dequeue: TPointerType;       // read&remove from head

    procedure Sort(compare: TOnPointerCompare); overload;
    procedure Sort(compare: TOnPointerCompare; AFirst, ALast: TCHandle); overload;
    procedure Sort(compare: TOnPointerCompareProc); overload;
    procedure Sort(compare: TOnPointerCompareProc; AFirst, ALast: TCHandle); overload;

    property Map[key: TPointerType]: variant Read GetMap Write PutMap;
    property Items[handle: TCHandle]: TPointerType Read GetItem Write SetItem; default;
    property OnCompare: TOnPointerCompare Read fOnCompare Write SetOnCompare;
    property OnCompareProc: TOnPointerCompareProc Read fOnCompareProc Write SetOnCompareProc;
  end;
  TTPointerField = TTPointer;

  // replacement for TList
  TTList = class(TTPointer);

  TTObject = class(TTPointer)
  protected
    procedure ItemDeleted(n: TCHandle); override;
  public
    constructor Create(Container: TCCustom = nil); override;
  end;
  TTObjectField = TTObject;

  TOnRecordCompare = function(var a, b): integer of object;
  TOnRecordCompareProc = function(var a, b): integer;

  TTRecord = class(TTCustom)
  private
    function CmpInt1(var a, b): integer;
    function CmpInt2(var a, b): integer;
    function CmpInt4(var a, b): integer;
    function CmpInt8(var a, b): integer;
    function CmpInt(var a, b): integer;
    procedure SetCmp;
  protected
    FOnCompare: TOnRecordCompare;
    FOnCompareProc: TOnRecordCompareProc;
    FTemp: PByteArray;
    FComplete: integer;
    FPos:  integer;

    function GetItem(n: TCHandle): pointer;
    procedure SetItem(n: TCHandle; Value: pointer);
    function GetAsVariant(handle: TCHandle): variant; override;
    procedure SetAsVariant(handle: TCHandle; Value: variant); override;
    function GetAsAnsiStr(handle: TCHandle): AnsiString; override;
    procedure SetAsAnsiStr(handle: TCHandle; Value: AnsiString); override;
    function GetAsWideStr(handle: TCHandle): WideString; override;
    procedure SetAsWideStr(handle: TCHandle; Value: WideString); override;
    function GetHandleOf(Value: variant): TCHandle; override;
    procedure SetFieldSize(ASize: integer); override;
    procedure InitContainer(c: TCCustom); override;
    procedure SetOnCompare(c: TOnRecordCompare);
    procedure SetOnCompareProc(c: TOnRecordCompareProc);
    function CmpExt(var a, b): integer;
    function CmpExtProc(var a, b): integer;
    function STCopy(Dst: TTCustom; h: TCHandle):TCHandle; override;
    procedure STCopy(Dst: TTCustom; AFirst, ALast: TCHandle); override;
    procedure STSet(Dst: TTCustom; h1,h2: TCHandle); override;
    procedure STSet(Dst: TTCustom; start1,finish1, start2: TCHandle); override;

    function GetMap(var key): variant;
    procedure PutMap(var key; Value: variant);

  public
    constructor Create(AItemSize: integer; ACompare: TOnRecordCompare; Container: TCCustom = nil); overload;
    constructor Create(AItemSize: integer; Container: TCCustom = nil); overload;

    destructor Destroy; override;

    function Add(var Item): TCHandle; overload;
    function AddList(const AItems: array of const): TCHandle; override;
    function AddTo(handle: TCHandle; dst: TTCustom): TCHandle; override;
    procedure Get(handle: TCHandle; var Item);
    function Insert(handle: TCHandle; var Item): TCHandle;
    function InsertAfter(handle: TCHandle; var Item): TCHandle;

    procedure Remove(var Item);
    procedure RemoveAll(var Item);

    function Find(var Item): TCHandle;
    function FindFirstEqual(var Item): TCHandle;
    procedure FindNextEqual(var Item; var handle: TCHandle);

    procedure GetMinValue(var dst);    // Get min VALUE (not handle!)
    procedure RemoveMinValue(var dst); // Get min VALUE (not handle!) & remove it from Container
    procedure GetMaxValue(var dst);    // Get Max VALUE (not handle!)
    procedure RemoveMaxValue(var dst); // Get Max VALUE (not handle!) & remove it from Container

    function AddPair(var key; Value: variant):TCHandle;

    // stack & deque operations
    procedure Front(var dst);          // examine first element
    procedure Back(var dst);           // examine last element
    function Push(var Item): TCHandle; // insert element at front
    procedure Pop(var dst);            // remove first element
    function PushBack(var Item): TCHandle;// insert element at back
    procedure PopBack(var dst);        // remove last element

    // queue operations
    function Enqueue(var Item): TCHandle;// add to tail
    procedure Dequeue(var dst);       // read&remove from head

    procedure Sort(compare: TOnRecordCompare); overload;
    procedure Sort(compare: TOnRecordCompare; AFirst, ALast: TCHandle); overload;
    procedure Sort(compare: TOnRecordCompareProc); overload;
    procedure Sort(compare: TOnRecordCompareProc; AFirst, ALast: TCHandle); overload;

    {$IFNDEF FPC}
    property Map[var key]: variant Read GetMap Write PutMap;
    {$ENDIF}
    property Items[handle: TCHandle]: pointer Read GetItem Write SetItem; default;
    property OnCompare: TOnRecordCompare Read fOnCompare Write SetOnCompare;
    property OnCompareProc: TOnRecordCompareProc Read fOnCompareProc Write SetOnCompareProc;
    property RecordSize: integer read FFieldSize write SetFieldSize;
  end;
  TTRecordField = TTRecord;

  TRectType = TRect;
  TOnRectCompare = function(a, b: TRectType): integer of object;
  TOnRectCompareProc = function(a, b: TRectType): integer;

  TTRect = class(TTCustom)
  protected
    FOnCompare: TOnRectCompare;
    FOnCompareProc: TOnRectCompareProc;
    FComplete, FPos: integer;

    function GetItem(n: TCHandle): TRectType;
    procedure SetItem(n: TCHandle; Value: TRectType);
    function GetAsVariant(handle: TCHandle): variant; override;
    procedure SetAsVariant(handle: TCHandle; Value: variant); override;
    function GetAsAnsiStr(handle: TCHandle): AnsiString; override;
    procedure SetAsAnsiStr(handle: TCHandle; Value: AnsiString); override;
    function GetAsWideStr(handle: TCHandle): WideString; override;
    procedure SetAsWideStr(handle: TCHandle; Value: WideString); override;
    function GetHandleOf(Value: variant): TCHandle; override;
    function LoadItem(c: TCCustom; h: TCHandle; src: TStream; FileFormat : TFileFormat = ffDefault):TCHandle; override;
    procedure InitContainer(c: TCCustom); override;
    procedure SetOnCompare(c: TOnRectCompare);
    procedure SetOnCompareProc(c: TOnRectCompareProc);
    function CmpInt(var a, b): integer;
    function CmpExt(var a, b): integer;
    function CmpExtProc(var a, b): integer;
    function STCopy(Dst: TTCustom; h: TCHandle):TCHandle; override;
    procedure STCopy(Dst: TTCustom; AFirst, ALast: TCHandle); override;
    procedure STSet(Dst: TTCustom; h1,h2: TCHandle); override;
    procedure STSet(Dst: TTCustom; start1,finish1, start2: TCHandle); override;

    function GetMap(key: TRectType): variant;
    procedure PutMap(key: TRectType; Value: variant);

  public
    constructor Create(Container: TCCustom = nil); override;

    function Add(Item: TRectType): TCHandle; overload;
    function AddList(const AItems: array of const): TCHandle; override;
    function AddTo(handle: TCHandle; dst: TTCustom): TCHandle; override;
    function Insert(handle: TCHandle; Item: TRectType): TCHandle;
    function InsertAfter(handle: TCHandle; Item: TRectType): TCHandle;

    procedure Remove(Item: TRectType);
    procedure RemoveAll(Item: TRectType);

    function Find(Item: TRectType): TCHandle;
    function FindFirstEqual(Item: TRectType): TCHandle;
    procedure FindNextEqual(Item: TRectType; var handle: TCHandle);

    function GetMinValue: TRectType;    // Get min VALUE (not handle!)
    function RemoveMinValue: TRectType; // Get min VALUE (not handle!) & remove it from Container
    function GetMaxValue: TRectType;    // Get Max VALUE (not handle!)
    function RemoveMaxValue: TRectType; // Get Max VALUE (not handle!) & remove it from Container

    function AddPair(key: TRectType; Value: variant):TCHandle;

    // stack & deque operations
    function Front: TRectType;          // examine first element
    function Back: TRectType;           // examine last element
    function Push(v: TRectType): TCHandle;        // insert element at front
    function Pop: TRectType;            // remove first element
    function PushBack(v: TRectType): TCHandle;    // insert element at back
    function PopBack: TRectType;        // remove last element

    // queue operations
    function Enqueue(v: TRectType): TCHandle;    // add to tail
    function Dequeue: TRectType;       // read&remove from head

    procedure Sort(compare: TOnRectCompare); overload;
    procedure Sort(compare: TOnRectCompare; AFirst, ALast: TCHandle); overload;
    procedure Sort(compare: TOnRectCompareProc); overload;
    procedure Sort(compare: TOnRectCompareProc; AFirst, ALast: TCHandle); overload;

    property Map[key: TRectType]: variant Read GetMap Write PutMap;
    property Items[handle: TCHandle]: TRectType Read GetItem Write SetItem; default;
    property OnCompare: TOnRectCompare Read fOnCompare Write SetOnCompare;
    property OnCompareProc: TOnRectCompareProc Read fOnCompareProc Write SetOnCompareProc;
  end;
  TTRectField = TTRect;

  TFRect = packed record
    left,top,right,bottom: double;
  end;
  TFRectType = TFRect;
  TOnFRectCompare = function(a, b: TFRectType): integer of object;
  TOnFRectCompareProc = function(a, b: TFRectType): integer;

  TTFRect = class(TTCustom)
  protected
    FOnCompare: TOnFRectCompare;
    FOnCompareProc: TOnFRectCompareProc;
    FComplete, FPos: integer;

    function GetItem(n: TCHandle): TFRectType;
    procedure SetItem(n: TCHandle; Value: TFRectType);
    function GetAsVariant(handle: TCHandle): variant; override;
    procedure SetAsVariant(handle: TCHandle; Value: variant); override;
    function GetAsAnsiStr(handle: TCHandle): AnsiString; override;
    procedure SetAsAnsiStr(handle: TCHandle; Value: AnsiString); override;
    function GetAsWideStr(handle: TCHandle): WideString; override;
    procedure SetAsWideStr(handle: TCHandle; Value: WideString); override;
    function GetHandleOf(Value: variant): TCHandle; override;
    function LoadItem(c: TCCustom; h: TCHandle; src: TStream; FileFormat : TFileFormat = ffDefault):TCHandle; override;
    procedure InitContainer(c: TCCustom); override;
    procedure SetOnCompare(c: TOnFRectCompare);
    procedure SetOnCompareProc(c: TOnFRectCompareProc);
    function CmpInt(var a, b): integer;
    function CmpExt(var a, b): integer;
    function CmpExtProc(var a, b): integer;
    function STCopy(Dst: TTCustom; h: TCHandle):TCHandle; override;
    procedure STCopy(Dst: TTCustom; AFirst, ALast: TCHandle); override;
    procedure STSet(Dst: TTCustom; h1,h2: TCHandle); override;
    procedure STSet(Dst: TTCustom; start1,finish1, start2: TCHandle); override;

    function GetMap(key: TFRectType): variant;
    procedure PutMap(key: TFRectType; Value: variant);

  public
    constructor Create(Container: TCCustom = nil); override;

    function Add(Item: TFRectType): TCHandle; overload;
    function AddList(const AItems: array of const): TCHandle; override;
    function AddTo(handle: TCHandle; dst: TTCustom): TCHandle; override;
    function Insert(handle: TCHandle; Item: TFRectType): TCHandle;
    function InsertAfter(handle: TCHandle; Item: TFRectType): TCHandle;

    procedure Remove(Item: TFRectType);
    procedure RemoveAll(Item: TFRectType);

    function Find(Item: TFRectType): TCHandle;
    function FindFirstEqual(Item: TFRectType): TCHandle;
    procedure FindNextEqual(Item: TFRectType; var handle: TCHandle);

    function GetMinValue: TFRectType;    // Get min VALUE (not handle!)
    function RemoveMinValue: TFRectType; // Get min VALUE (not handle!) & remove it from Container
    function GetMaxValue: TFRectType;    // Get Max VALUE (not handle!)
    function RemoveMaxValue: TFRectType; // Get Max VALUE (not handle!) & remove it from Container

    function AddPair(key: TFRectType; Value: variant):TCHandle;

    // stack & deque operations
    function Front: TFRectType;          // examine first element
    function Back: TFRectType;           // examine last element
    function Push(v: TFRectType): TCHandle;        // insert element at front
    function Pop: TFRectType;            // remove first element
    function PushBack(v: TFRectType): TCHandle;    // insert element at back
    function PopBack: TFRectType;        // remove last element

    // queue operations
    function Enqueue(v: TFRectType): TCHandle;    // add to tail
    function Dequeue: TFRectType;       // read&remove from head

    procedure Sort(compare: TOnFRectCompare); overload;
    procedure Sort(compare: TOnFRectCompare; AFirst, ALast: TCHandle); overload;
    procedure Sort(compare: TOnFRectCompareProc); overload;
    procedure Sort(compare: TOnFRectCompareProc; AFirst, ALast: TCHandle); overload;

    property Map[key: TFRectType]: variant Read GetMap Write PutMap;
    property Items[handle: TCHandle]: TFRectType Read GetItem Write SetItem; default;
    property OnCompare: TOnFRectCompare Read fOnCompare Write SetOnCompare;
    property OnCompareProc: TOnFRectCompareProc Read fOnCompareProc Write SetOnCompareProc;
  end;
  TTFRectField = TTFRect;

  TAnsiStringType = AnsiString;
  TOnAnsiStringCompare = function(a, b: TAnsiStringType): integer of object;
  TOnAnsiStringCompareProc = function(a, b: TAnsiStringType): integer;

  TTAnsiString = class(TTCustom)
  protected
    FOnCompare: TOnAnsiStringCompare;
    FOnCompareProc: TOnAnsiStringCompareProc;
    FLexicographic: boolean;
    FCaseInsensitive: boolean;

    function GetItem(n: TCHandle): TAnsiStringType;
    procedure SetItem(n: TCHandle; Value: TAnsiStringType);
    function GetAsVariant(handle: TCHandle): variant; override;
    procedure SetAsVariant(handle: TCHandle; Value: variant); override;
    function GetHandleOf(Value: variant): TCHandle; override;
    function GetAsAnsiStr(handle: TCHandle): AnsiString; override;
    procedure SetAsAnsiStr(handle: TCHandle; Value: AnsiString); override;
    function GetAsWideStr(handle: TCHandle): WideString; override;
    procedure SetAsWideStr(handle: TCHandle; Value: WideString); override;
    procedure InitContainer(c: TCCustom); override;
    procedure SetOnCompare(c: TOnAnsiStringCompare);
    procedure SetOnCompareProc(c: TOnAnsiStringCompareProc);
    function CmpInt(var a, b): integer;
    function CmpExt(var a, b): integer;
    function CmpExtProc(var a, b): integer;
    function STCopy(Dst: TTCustom; h: TCHandle):TCHandle; override;
    procedure STCopy(Dst: TTCustom; AFirst, ALast: TCHandle); override;
    procedure STSet(Dst: TTCustom; h1,h2: TCHandle); override;
    procedure STSet(Dst: TTCustom; start1,finish1, start2: TCHandle); override;
    function GetMap(key: TAnsiStringType): variant;
    procedure PutMap(key: TAnsiStringType; Value: variant);

    // text-specific
    procedure SaveItem(h: TCHandle; dst: TStream; FileFormat : TFileFormat = ffDefault); override;
    function LoadItem(c: TCCustom; h: TCHandle; src: TStream; FileFormat : TFileFormat = ffDefault):TCHandle; override;
    procedure SetLexicographic(value: boolean);
    procedure SetCaseInsensitive(value: boolean);
    procedure DoCopyItem(Src, Dst: TCHandle);
    function  DoCalcHash(var Item; Count: integer; h: T_HashCustom): cardinal;
    procedure ItemDeleted(n: TCHandle); override;

  public
    constructor Create(Container: TCCustom = nil); override;
    procedure AssignShape(src: TTCustom); override;

    function Add(Item: TAnsiStringType): TCHandle; overload;
    function AddList(const Items: array of const): TCHandle; override;
    function AddTo(handle: TCHandle; dst: TTCustom): TCHandle; override;
    function Insert(handle: TCHandle; Item: TAnsiStringType): TCHandle;
    function InsertAfter(handle: TCHandle; Item: TAnsiStringType): TCHandle;

    procedure Remove(Item: TAnsiStringType);
    procedure RemoveAll(Item: TAnsiStringType);

    function Find(Item: TAnsiStringType): TCHandle;
    function FindFirstEqual(Item: TAnsiStringType): TCHandle;
    procedure FindNextEqual(Item: TAnsiStringType; var handle: TCHandle);

    function GetMinValue: TAnsiStringType;    // Get min VALUE (not handle!)
    function RemoveMinValue: TAnsiStringType; // Get min VALUE (not handle!) & remove it from Container
    function GetMaxValue: TAnsiStringType;    // Get max VALUE (not handle!)
    function RemoveMaxValue: TAnsiStringType; // Get max VALUE (not handle!) & remove it from Container

    function AddPair(key: TAnsiStringType; Value: variant):TCHandle;

    // stack & deque operations
    function  Front: TAnsiStringType;          // examine first element
    function  Back: TAnsiStringType;           // examine last element
    function  Push(v: TAnsiStringType): TCHandle;        // insert element at front
    function  Pop: TAnsiStringType;            // remove first element
    function  PushBack(v: TAnsiStringType): TCHandle;    // insert element at back
    function  PopBack: TAnsiStringType;        // remove last element

    // queue operations
    function  Enqueue(v: TAnsiStringType): TCHandle;    // add to tail
    function  Dequeue: TAnsiStringType;       // read&remove from head

    procedure Sort(compare: TOnAnsiStringCompare); overload;
    procedure Sort(compare: TOnAnsiStringCompare; AFirst, ALast: TCHandle); overload;
    procedure Sort(compare: TOnAnsiStringCompareProc); overload;
    procedure Sort(compare: TOnAnsiStringCompareProc; AFirst, ALast: TCHandle); overload;

    property Map[key: TAnsiStringType]: variant Read GetMap Write PutMap;
    property Items[handle: TCHandle]: TAnsiStringType Read GetItem Write SetItem; default;
    property OnCompare: TOnAnsiStringCompare Read fOnCompare Write SetOnCompare;
    property OnCompareProc: TOnAnsiStringCompareProc Read fOnCompareProc Write SetOnCompareProc;

    // text-specific
    property Lexicographic: boolean read Flexicographic write SetLexicographic;
    property CaseInsensitive: boolean read FCaseInsensitive write SetCaseInsensitive;
  end;
  TTAnsiStringField = TTAnsiString;

  TWideStringType = WideString;
  TOnWideStringCompare = function(a, b: TWideStringType): integer of object;
  TOnWideStringCompareProc = function(a, b: TWideStringType): integer;

  TTWideString = class(TTCustom)
  protected
    FOnCompare: TOnWideStringCompare;
    FOnCompareProc: TOnWideStringCompareProc;
    FLexicographic: boolean;
    FCaseInsensitive: boolean;

    function GetItem(n: TCHandle): TWideStringType;
    procedure SetItem(n: TCHandle; Value: TWideStringType);
    function GetAsVariant(handle: TCHandle): variant; override;
    procedure SetAsVariant(handle: TCHandle; Value: variant); override;
    function GetHandleOf(Value: variant): TCHandle; override;
    function GetAsAnsiStr(handle: TCHandle): AnsiString; override;
    procedure SetAsAnsiStr(handle: TCHandle; Value: AnsiString); override;
    function GetAsWideStr(handle: TCHandle): WideString; override;
    procedure SetAsWideStr(handle: TCHandle; Value: WideString); override;
    procedure InitContainer(c: TCCustom); override;
    procedure SetOnCompare(c: TOnWideStringCompare);
    procedure SetOnCompareProc(c: TOnWideStringCompareProc);
    function CmpInt(var a, b): integer;
    function CmpExt(var a, b): integer;
    function CmpExtProc(var a, b): integer;
    function STCopy(Dst: TTCustom; h: TCHandle):TCHandle; override;
    procedure STCopy(Dst: TTCustom; AFirst, ALast: TCHandle); override;
    procedure STSet(Dst: TTCustom; h1,h2: TCHandle); override;
    procedure STSet(Dst: TTCustom; start1,finish1, start2: TCHandle); override;
    function GetMap(key: TWideStringType): variant;
    procedure PutMap(key: TWideStringType; Value: variant);

    // text-specific
    procedure SaveItem(h: TCHandle; dst: TStream; FileFormat : TFileFormat = ffDefault); override;
    function LoadItem(c: TCCustom; h: TCHandle; src: TStream; FileFormat : TFileFormat = ffDefault):TCHandle; override;
    procedure SetLexicographic(value: boolean);
    procedure SetCaseInsensitive(value: boolean);
    procedure DoCopyItem(Src, Dst: TCHandle);
    function  DoCalcHash(var Item; Count: integer; h: T_HashCustom): cardinal;
    procedure ItemDeleted(n: TCHandle); override;

  public
    constructor Create(Container: TCCustom = nil); override;
    procedure AssignShape(src: TTCustom); override;

    function Add(Item: TWideStringType): TCHandle; overload;
    function AddList(const Items: array of const): TCHandle; override;
    function AddTo(handle: TCHandle; dst: TTCustom): TCHandle; override;
    function Insert(handle: TCHandle; Item: TWideStringType): TCHandle;
    function InsertAfter(handle: TCHandle; Item: TWideStringType): TCHandle;

    procedure Remove(Item: TWideStringType);
    procedure RemoveAll(Item: TWideStringType);

    function Find(Item: TWideStringType): TCHandle;
    function FindFirstEqual(Item: TWideStringType): TCHandle;
    procedure FindNextEqual(Item: TWideStringType; var handle: TCHandle);

    function GetMinValue: TWideStringType;    // Get min VALUE (not handle!)
    function RemoveMinValue: TWideStringType; // Get min VALUE (not handle!) & remove it from Container
    function GetMaxValue: TWideStringType;    // Get max VALUE (not handle!)
    function RemoveMaxValue: TWideStringType; // Get max VALUE (not handle!) & remove it from Container

    function AddPair(key: TWideStringType; Value: variant):TCHandle;

    // stack & deque operations
    function  Front: TWideStringType;          // examine first element
    function  Back: TWideStringType;           // examine last element
    function  Push(v: TWideStringType): TCHandle;        // insert element at front
    function  Pop: TWideStringType;            // remove first element
    function  PushBack(v: TWideStringType): TCHandle;    // insert element at back
    function  PopBack: TWideStringType;        // remove last element

    // queue operations
    function  Enqueue(v: TWideStringType): TCHandle;    // add to tail
    function  Dequeue: TWideStringType;       // read&remove from head

    procedure Sort(compare: TOnWideStringCompare); overload;
    procedure Sort(compare: TOnWideStringCompare; AFirst, ALast: TCHandle); overload;
    procedure Sort(compare: TOnWideStringCompareProc); overload;
    procedure Sort(compare: TOnWideStringCompareProc; AFirst, ALast: TCHandle); overload;

    property Map[key: TWideStringType]: variant Read GetMap Write PutMap;
    property Items[handle: TCHandle]: TWideStringType Read GetItem Write SetItem; default;
    property OnCompare: TOnWideStringCompare Read fOnCompare Write SetOnCompare;
    property OnCompareProc: TOnWideStringCompareProc Read fOnCompareProc Write SetOnCompareProc;

    // text-specific
    property Lexicographic: boolean read Flexicographic write SetLexicographic;
    property CaseInsensitive: boolean read FCaseInsensitive write SetCaseInsensitive;
  end;
  TTWideStringField = TTWideString;

  TStringType = String;
  TOnStringCompare = function(a, b: TStringType): integer of object;
  TOnStringCompareProc = function(a, b: TStringType): integer;

  TTString = class(TTCustom)
  protected
    FOnCompare: TOnStringCompare;
    FOnCompareProc: TOnStringCompareProc;
    FLexicographic: boolean;
    FCaseInsensitive: boolean;

    function GetItem(n: TCHandle): TStringType;
    procedure SetItem(n: TCHandle; Value: TStringType);
    function GetAsVariant(handle: TCHandle): variant; override;
    procedure SetAsVariant(handle: TCHandle; Value: variant); override;
    function GetHandleOf(Value: variant): TCHandle; override;
    function GetAsAnsiStr(handle: TCHandle): AnsiString; override;
    procedure SetAsAnsiStr(handle: TCHandle; Value: AnsiString); override;
    function GetAsWideStr(handle: TCHandle): WideString; override;
    procedure SetAsWideStr(handle: TCHandle; Value: WideString); override;
    procedure InitContainer(c: TCCustom); override;
    procedure SetOnCompare(c: TOnStringCompare);
    procedure SetOnCompareProc(c: TOnStringCompareProc);
    function CmpInt(var a, b): integer;
    function CmpExt(var a, b): integer;
    function CmpExtProc(var a, b): integer;
    function STCopy(Dst: TTCustom; h: TCHandle):TCHandle; override;
    procedure STCopy(Dst: TTCustom; AFirst, ALast: TCHandle); override;
    procedure STSet(Dst: TTCustom; h1,h2: TCHandle); override;
    procedure STSet(Dst: TTCustom; start1,finish1, start2: TCHandle); override;
    function GetMap(key: TStringType): variant;
    procedure PutMap(key: TStringType; Value: variant);

    // text-specific
    procedure SaveItem(h: TCHandle; dst: TStream; FileFormat : TFileFormat = ffDefault); override;
    function LoadItem(c: TCCustom; h: TCHandle; src: TStream; FileFormat : TFileFormat = ffDefault):TCHandle; override;
    procedure SetLexicographic(value: boolean);
    procedure SetCaseInsensitive(value: boolean);
    procedure DoCopyItem(Src, Dst: TCHandle);
    function  DoCalcHash(var Item; Count: integer; h: T_HashCustom): cardinal;
    procedure ItemDeleted(n: TCHandle); override;

  public
    constructor Create(Container: TCCustom = nil); override;
    procedure AssignShape(src: TTCustom); override;

    function Add(Item: TStringType): TCHandle; overload;
    function AddChild(AParent: TCHandle; Item: TStringType): TCHandle;
    function AddList(const Items: array of const): TCHandle; override;
    function AddTo(handle: TCHandle; dst: TTCustom): TCHandle; override;
    function Insert(handle: TCHandle; Item: TStringType): TCHandle;
    function InsertAfter(handle: TCHandle; Item: TStringType): TCHandle;

    procedure Remove(Item: TStringType);
    procedure RemoveAll(Item: TStringType);

    function Find(Item: TStringType): TCHandle;
    function FindFirstEqual(Item: TStringType): TCHandle;
    procedure FindNextEqual(Item: TStringType; var handle: TCHandle);

    function GetMinValue: TStringType;    // Get min VALUE (not handle!)
    function RemoveMinValue: TStringType; // Get min VALUE (not handle!) & remove it from Container
    function GetMaxValue: TStringType;    // Get max VALUE (not handle!)
    function RemoveMaxValue: TStringType; // Get max VALUE (not handle!) & remove it from Container

    function AddPair(key: TStringType; Value: variant):TCHandle;

    // stack & deque operations
    function  Front: TStringType;          // examine first element
    function  Back: TStringType;           // examine last element
    function  Push(v: TStringType): TCHandle;        // insert element at front
    function  Pop: TStringType;            // remove first element
    function  PushBack(v: TStringType): TCHandle;    // insert element at back
    function  PopBack: TStringType;        // remove last element

    // queue operations
    function  Enqueue(v: TStringType): TCHandle;    // add to tail
    function  Dequeue: TStringType;       // read&remove from head

    procedure Sort(compare: TOnStringCompare); overload;
    procedure Sort(compare: TOnStringCompare; AFirst, ALast: TCHandle); overload;
    procedure Sort(compare: TOnStringCompareProc); overload;
    procedure Sort(compare: TOnStringCompareProc; AFirst, ALast: TCHandle); overload;

    property Map[key: TStringType]: variant Read GetMap Write PutMap;
    property Items[handle: TCHandle]: TStringType Read GetItem Write SetItem; default;
    property OnCompare: TOnStringCompare Read fOnCompare Write SetOnCompare;
    property OnCompareProc: TOnStringCompareProc Read fOnCompareProc Write SetOnCompareProc;

    // text-specific
    property Lexicographic: boolean read Flexicographic write SetLexicographic;
    property CaseInsensitive: boolean read FCaseInsensitive write SetCaseInsensitive;
  end;
  TTStringField = TTString;

  TWordType = word;
  TOnWordCompare = function(a, b: TWordType): integer of object;
  TOnWordCompareProc = function(a, b: TWordType): integer;

  TTWord = class(TTCustom)
  protected
    FOnCompare: TOnWordCompare;
    FOnCompareProc: TOnWordCompareProc;

    function GetItem(n: TCHandle): TWordType;
    procedure SetItem(n: TCHandle; Value: TWordType);
    function GetAsVariant(handle: TCHandle): variant; override;
    procedure SetAsVariant(handle: TCHandle; Value: variant); override;
    function GetAsAnsiStr(handle: TCHandle): AnsiString; override;
    procedure SetAsAnsiStr(handle: TCHandle; Value: AnsiString); override;
    function GetAsWideStr(handle: TCHandle): WideString; override;
    procedure SetAsWideStr(handle: TCHandle; Value: WideString); override;
    function GetHandleOf(Value: variant): TCHandle; override;
    procedure InitContainer(c: TCCustom); override;
    procedure SetOnCompare(c: TOnWordCompare);
    procedure SetOnCompareProc(c: TOnWordCompareProc);
    function CmpInt(var a, b): integer;
    function CmpExt(var a, b): integer;
    function CmpExtProc(var a, b): integer;
    function STCopy(Dst: TTCustom; h: TCHandle):TCHandle; override;
    procedure STCopy(Dst: TTCustom; AFirst, ALast: TCHandle); override;
    procedure STSet(Dst: TTCustom; h1,h2: TCHandle); override;
    procedure STSet(Dst: TTCustom; start1,finish1, start2: TCHandle); override;

    function GetMap(key: TWordType): variant;
    procedure PutMap(key: TWordType; Value: variant);

  public
    constructor Create(Container: TCCustom = nil); override;

    function Add(Item: TWordType): TCHandle; overload;
    function AddList(const Items: array of const): TCHandle; override;
    function AddTo(handle: TCHandle; dst: TTCustom): TCHandle; override;
    function Insert(handle: TCHandle; Item: TWordType): TCHandle;
    function InsertAfter(handle: TCHandle; Item: TWordType): TCHandle;

    procedure Remove(Item: TWordType);
    procedure RemoveAll(Item: TWordType);

    function Find(Item: TWordType): TCHandle;
    function FindFirstEqual(Item: TWordType): TCHandle;
    procedure FindNextEqual(Item: TWordType; var handle: TCHandle);

    function GetMinValue: TWordType;    // Get min VALUE (not handle!)
    function RemoveMinValue: TWordType; // Get min VALUE (not handle!) & remove it from Container
    function GetMaxValue: TWordType;    // Get Max VALUE (not handle!)
    function RemoveMaxValue: TWordType; // Get Max VALUE (not handle!) & remove it from Container

    function AddPair(key: TWordType; Value: variant):TCHandle;

    // stack & deque operations
    function Front: TWordType;          // examine first element
    function Back: TWordType;           // examine last element
    function Push(v: TWordType): TCHandle;        // insert element at front
    function Pop: TWordType;            // remove first element
    function PushBack(v: TWordType): TCHandle;    // insert element at back
    function PopBack: TWordType;        // remove last element

    // queue operations
    function Enqueue(v: TWordType): TCHandle;    // add to tail
    function Dequeue: TWordType;       // read&remove from head

    procedure Sort(compare: TOnWordCompare); overload;
    procedure Sort(compare: TOnWordCompare; AFirst, ALast: TCHandle); overload;
    procedure Sort(compare: TOnWordCompareProc); overload;
    procedure Sort(compare: TOnWordCompareProc; AFirst, ALast: TCHandle); overload;

    property Map[key: TWordType]: variant Read GetMap Write PutMap;
    property Items[handle: TCHandle]: TWordType Read GetItem Write SetItem; default;
    property OnCompare: TOnWordCompare Read fOnCompare Write SetOnCompare;
    property OnCompareProc: TOnWordCompareProc Read fOnCompareProc Write SetOnCompareProc;
  end;
  TTWordField = TTWord;

  TInt64Type = int64;
  TOnInt64Compare = function(a, b: TInt64Type): integer of object;
  TOnInt64CompareProc = function(a, b: TInt64Type): integer;

  TTInt64 = class(TTCustom)
  protected
    FOnCompare: TOnInt64Compare;
    FOnCompareProc: TOnInt64CompareProc;

    function GetItem(n: TCHandle): TInt64Type;
    procedure SetItem(n: TCHandle; Value: TInt64Type);
    function GetAsVariant(handle: TCHandle): variant; override;
    procedure SetAsVariant(handle: TCHandle; Value: variant); override;
    function GetAsAnsiStr(handle: TCHandle): AnsiString; override;
    procedure SetAsAnsiStr(handle: TCHandle; Value: AnsiString); override;
    function GetAsWideStr(handle: TCHandle): WideString; override;
    procedure SetAsWideStr(handle: TCHandle; Value: WideString); override;
    function GetHandleOf(Value: variant): TCHandle; override;
    procedure InitContainer(c: TCCustom); override;
    procedure SetOnCompare(c: TOnInt64Compare);
    procedure SetOnCompareProc(c: TOnInt64CompareProc);
    function CmpInt(var a, b): integer;
    function CmpExt(var a, b): integer;
    function CmpExtProc(var a, b): integer;
    function STCopy(Dst: TTCustom; h: TCHandle):TCHandle; override;
    procedure STCopy(Dst: TTCustom; AFirst, ALast: TCHandle); override;
    procedure STSet(Dst: TTCustom; h1,h2: TCHandle); override;
    procedure STSet(Dst: TTCustom; start1,finish1, start2: TCHandle); override;

    function GetMap(key: TInt64Type): variant;
    procedure PutMap(key: TInt64Type; Value: variant);

  public
    constructor Create(Container: TCCustom = nil); override;

    function Add(Item: TInt64Type): TCHandle; overload;
    function AddList(const Items: array of const): TCHandle; override;
    function AddTo(handle: TCHandle; dst: TTCustom): TCHandle; override;
    function Insert(handle: TCHandle; Item: TInt64Type): TCHandle;
    function InsertAfter(handle: TCHandle; Item: TInt64Type): TCHandle;

    procedure Remove(Item: TInt64Type);
    procedure RemoveAll(Item: TInt64Type);

    function Find(Item: TInt64Type): TCHandle;
    function FindFirstEqual(Item: TInt64Type): TCHandle;
    procedure FindNextEqual(Item: TInt64Type; var handle: TCHandle);

    function GetMinValue: TInt64Type;    // Get min VALUE (not handle!)
    function RemoveMinValue: TInt64Type; // Get min VALUE (not handle!) & remove it from Container
    function GetMaxValue: TInt64Type;    // Get max VALUE (not handle!)
    function RemoveMaxValue: TInt64Type; // Get max VALUE (not handle!) & remove it from Container

    function AddPair(key: TInt64Type; Value: variant):TCHandle;

    // stack & deque operations
    function Front: TInt64Type;          // examine first element
    function Back: TInt64Type;           // examine last element
    function Push(v: TInt64Type): TCHandle;        // insert element at front
    function Pop: TInt64Type;            // remove first element
    function PushBack(v: TInt64Type): TCHandle;    // insert element at back
    function PopBack: TInt64Type;        // remove last element

    // queue operations
    function Enqueue(v: TInt64Type): TCHandle;    // add to tail
    function Dequeue: TInt64Type;       // read&remove from head

    procedure Sort(compare: TOnInt64Compare); overload;
    procedure Sort(compare: TOnInt64Compare; AFirst, ALast: TCHandle); overload;
    procedure Sort(compare: TOnInt64CompareProc); overload;
    procedure Sort(compare: TOnInt64CompareProc; AFirst, ALast: TCHandle); overload;

    property Map[key: TInt64Type]: variant Read GetMap Write PutMap;
    property Items[handle: TCHandle]: TInt64Type Read GetItem Write SetItem; default;
    property OnCompare: TOnInt64Compare Read fOnCompare Write SetOnCompare;
    property OnCompareProc: TOnInt64CompareProc Read fOnCompareProc Write SetOnCompareProc;
  end;
  TTInt64Field = TTInt64;

  {$IFDEF HiCompiler}
  TUInt64Type = UInt64;
  TOnUInt64Compare = function(a, b: TUInt64Type): integer of object;
  TOnUInt64CompareProc = function(a, b: TUInt64Type): integer;

  TTUInt64 = class(TTCustom)
  protected
    FOnCompare: TOnUInt64Compare;
    FOnCompareProc: TOnUInt64CompareProc;

    function GetItem(n: TCHandle): TUInt64Type;
    procedure SetItem(n: TCHandle; Value: TUInt64Type);
    function GetAsVariant(handle: TCHandle): variant; override;
    procedure SetAsVariant(handle: TCHandle; Value: variant); override;
    function GetAsAnsiStr(handle: TCHandle): AnsiString; override;
    procedure SetAsAnsiStr(handle: TCHandle; Value: AnsiString); override;
    function GetAsWideStr(handle: TCHandle): WideString; override;
    procedure SetAsWideStr(handle: TCHandle; Value: WideString); override;
    function GetHandleOf(Value: variant): TCHandle; override;
    procedure InitContainer(c: TCCustom); override;
    procedure SetOnCompare(c: TOnUInt64Compare);
    procedure SetOnCompareProc(c: TOnUInt64CompareProc);
    function CmpInt(var a, b): integer;
    function CmpExt(var a, b): integer;
    function CmpExtProc(var a, b): integer;
    function STCopy(Dst: TTCustom; h: TCHandle):TCHandle; override;
    procedure STCopy(Dst: TTCustom; AFirst, ALast: TCHandle); override;
    procedure STSet(Dst: TTCustom; h1,h2: TCHandle); override;
    procedure STSet(Dst: TTCustom; start1,finish1, start2: TCHandle); override;

    function GetMap(key: TUInt64Type): variant;
    procedure PutMap(key: TUInt64Type; Value: variant);

  public
    constructor Create(Container: TCCustom = nil); override;

    function Add(Item: TUInt64Type): TCHandle; overload;
    function AddList(const Items: array of const): TCHandle; override;
    function AddTo(handle: TCHandle; dst: TTCustom): TCHandle; override;
    function Insert(handle: TCHandle; Item: TUInt64Type): TCHandle;
    function InsertAfter(handle: TCHandle; Item: TUInt64Type): TCHandle;

    procedure Remove(Item: TUInt64Type);
    procedure RemoveAll(Item: TUInt64Type);

    function Find(Item: TUInt64Type): TCHandle;
    function FindFirstEqual(Item: TUInt64Type): TCHandle;
    procedure FindNextEqual(Item: TUInt64Type; var handle: TCHandle);

    function GetMinValue: TUInt64Type;    // Get min VALUE (not handle!)
    function RemoveMinValue: TUInt64Type; // Get min VALUE (not handle!) & remove it from Container
    function GetMaxValue: TUInt64Type;    // Get max VALUE (not handle!)
    function RemoveMaxValue: TUInt64Type; // Get max VALUE (not handle!) & remove it from Container

    function AddPair(key: TUInt64Type; Value: variant):TCHandle;

    // stack & deque operations
    function Front: TUInt64Type;          // examine first element
    function Back: TUInt64Type;           // examine last element
    function Push(v: TUInt64Type): TCHandle;        // insert element at front
    function Pop: TUInt64Type;            // remove first element
    function PushBack(v: TUInt64Type): TCHandle;    // insert element at back
    function PopBack: TUInt64Type;        // remove last element

    // queue operations
    function Enqueue(v: TUInt64Type): TCHandle;    // add to tail
    function Dequeue: TUInt64Type;       // read&remove from head

    procedure Sort(compare: TOnUInt64Compare); overload;
    procedure Sort(compare: TOnUInt64Compare; AFirst, ALast: TCHandle); overload;
    procedure Sort(compare: TOnUInt64CompareProc); overload;
    procedure Sort(compare: TOnUInt64CompareProc; AFirst, ALast: TCHandle); overload;

    property Map[key: TUInt64Type]: variant Read GetMap Write PutMap;
    property Items[handle: TCHandle]: TUInt64Type Read GetItem Write SetItem; default;
    property OnCompare: TOnUInt64Compare Read fOnCompare Write SetOnCompare;
    property OnCompareProc: TOnUInt64CompareProc Read fOnCompareProc Write SetOnCompareProc;
  end;
  TTUInt64Field = TTUInt64;
  {$ENDIF}

  TVariantType = variant;
  TOnVariantCompare = function(a, b: TVariantType): integer of object;
  TOnVariantCompareProc = function(a, b: TVariantType): integer;

  TTVariant = class(TTCustom)
  protected
    FOnCompare: TOnVariantCompare;
    FOnCompareProc: TOnVariantCompareProc;

    procedure ItemDeleted(n: TCHandle); override;
    function GetItem(n: TCHandle): TVariantType;
    procedure SetItem(n: TCHandle; Value: TVariantType);
    function GetAsVariant(handle: TCHandle): variant; override;
    procedure SetAsVariant(handle: TCHandle; Value: variant); override;
    function GetAsAnsiStr(handle: TCHandle): AnsiString; override;
    procedure SetAsAnsiStr(handle: TCHandle; Value: AnsiString); override;
    function GetAsWideStr(handle: TCHandle): WideString; override;
    procedure SetAsWideStr(handle: TCHandle; Value: WideString); override;
    function GetHandleOf(Value: variant): TCHandle; override;
    procedure SaveItem(h: TCHandle; dst: TStream; FileFormat : TFileFormat = ffDefault); override;
    function LoadItem(c: TCCustom; h: TCHandle; src: TStream; FileFormat : TFileFormat = ffDefault):TCHandle; override;
    procedure InitContainer(c: TCCustom); override;
    procedure SetOnCompare(c: TOnVariantCompare);
    procedure SetOnCompareProc(c: TOnVariantCompareProc);
    function CmpInt(var a, b): integer;
    function CmpExt(var a, b): integer;
    function CmpExtProc(var a, b): integer;
    function STCopy(Dst: TTCustom; h: TCHandle):TCHandle; override;
    procedure STCopy(Dst: TTCustom; AFirst, ALast: TCHandle); override;
    procedure STSet(Dst: TTCustom; h1,h2: TCHandle); override;
    procedure STSet(Dst: TTCustom; start1,finish1, start2: TCHandle); override;

    function GetMap(key: TVariantType): variant;
    procedure PutMap(key: TVariantType; Value: variant);

  public
    constructor Create(Container: TCCustom = nil); override;

    function Add(Item: TVariantType): TCHandle; overload;
    function AddList(const Items: array of const): TCHandle; override;
    function AddTo(handle: TCHandle; dst: TTCustom): TCHandle; override;
    function Insert(handle: TCHandle; Item: TVariantType): TCHandle;
    function InsertAfter(handle: TCHandle; Item: TVariantType): TCHandle;

    procedure Remove(Item: TVariantType);
    procedure RemoveAll(Item: TVariantType);

    function Find(Item: TVariantType): TCHandle;
    function FindFirstEqual(Item: TVariantType): TCHandle;
    procedure FindNextEqual(Item: TVariantType; var handle: TCHandle);

    function GetMinValue: TVariantType;    // Get min VALUE (not handle!)
    function RemoveMinValue: TVariantType; // Get min VALUE (not handle!) & remove it from Container
    function GetMaxValue: TVariantType;    // Get Max VALUE (not handle!)
    function RemoveMaxValue: TVariantType; // Get Max VALUE (not handle!) & remove it from Container

    function AddPair(key: TVariantType; Value: variant):TCHandle;

    // stack & deque operations
    function Front: TVariantType;          // examine first element
    function Back: TVariantType;           // examine last element
    function Push(v: TVariantType): TCHandle;        // insert element at front
    function Pop: TVariantType;            // remove first element
    function PushBack(v: TVariantType): TCHandle;    // insert element at back
    function PopBack: TVariantType;        // remove last element

    // queue operations
    function Enqueue(v: TVariantType): TCHandle;    // add to tail
    function Dequeue: TVariantType;       // read&remove from head

    procedure Sort(compare: TOnVariantCompare); overload;
    procedure Sort(compare: TOnVariantCompare; AFirst, ALast: TCHandle); overload;
    procedure Sort(compare: TOnVariantCompareProc); overload;
    procedure Sort(compare: TOnVariantCompareProc; AFirst, ALast: TCHandle); overload;

    property Map[key: TVariantType]: variant Read GetMap Write PutMap;
    property Items[handle: TCHandle]: TVariantType Read GetItem Write SetItem; default;
    property OnCompare: TOnVariantCompare Read fOnCompare Write SetOnCompare;
    property OnCompareProc: TOnVariantCompareProc Read fOnCompareProc Write SetOnCompareProc;
  end;
  TTVariantField = TTVariant;

  TOleVariantType = OleVariant;
  TOnOleVariantCompare = function(a, b: TOleVariantType): integer of object;
  TOnOleVariantCompareProc = function(a, b: TOleVariantType): integer;

  TTOleVariant = class(TTCustom)
  protected
    FOnCompare: TOnOleVariantCompare;
    FOnCompareProc: TOnOleVariantCompareProc;

    procedure ItemDeleted(n: TCHandle); override;
    function GetItem(n: TCHandle): TOleVariantType;
    procedure SetItem(n: TCHandle; Value: TOleVariantType);
    function GetAsVariant(handle: TCHandle): Variant; override;
    procedure SetAsVariant(handle: TCHandle; Value: Variant); override;
    function GetAsAnsiStr(handle: TCHandle): AnsiString; override;
    procedure SetAsAnsiStr(handle: TCHandle; Value: AnsiString); override;
    function GetAsWideStr(handle: TCHandle): WideString; override;
    procedure SetAsWideStr(handle: TCHandle; Value: WideString); override;
    function GetHandleOf(Value: Variant): TCHandle; override;
    procedure SaveItem(h: TCHandle; dst: TStream; FileFormat : TFileFormat = ffDefault); override;
    function LoadItem(c: TCCustom; h: TCHandle; src: TStream; FileFormat : TFileFormat = ffDefault):TCHandle; override;
    procedure InitContainer(c: TCCustom); override;
    procedure SetOnCompare(c: TOnOleVariantCompare);
    procedure SetOnCompareProc(c: TOnOleVariantCompareProc);
    function CmpInt(var a, b): integer;
    function CmpExt(var a, b): integer;
    function CmpExtProc(var a, b): integer;
    function STCopy(Dst: TTCustom; h: TCHandle):TCHandle; override;
    procedure STCopy(Dst: TTCustom; AFirst, ALast: TCHandle); override;
    procedure STSet(Dst: TTCustom; h1,h2: TCHandle); override;
    procedure STSet(Dst: TTCustom; start1,finish1, start2: TCHandle); override;

    function GetMap(key: TOleVariantType): variant;
    procedure PutMap(key: TOleVariantType; Value: variant);

  public
    constructor Create(Container: TCCustom = nil); override;

    function Add(Item: TOleVariantType): TCHandle; overload;
    function AddList(const Items: array of const): TCHandle; override;
    function AddTo(handle: TCHandle; dst: TTCustom): TCHandle; override;
    function Insert(handle: TCHandle; Item: TOleVariantType): TCHandle;
    function InsertAfter(handle: TCHandle; Item: TOleVariantType): TCHandle;

    procedure Remove(Item: TOleVariantType);
    procedure RemoveAll(Item: TOleVariantType);

    function Find(Item: TOleVariantType): TCHandle;
    function FindFirstEqual(Item: TOleVariantType): TCHandle;
    procedure FindNextEqual(Item: TOleVariantType; var handle: TCHandle);

    function GetMinValue: TOleVariantType;    // Get min VALUE (not handle!)
    function RemoveMinValue: TOleVariantType; // Get min VALUE (not handle!) & remove it from Container
    function GetMaxValue: TOleVariantType;    // Get max VALUE (not handle!)
    function RemoveMaxValue: TOleVariantType; // Get max VALUE (not handle!) & remove it from Container

    function AddPair(key: TOleVariantType; Value: variant):TCHandle;

    // stack & deque operations
    function Front: TOleVariantType;          // examine first element
    function Back: TOleVariantType;           // examine last element
    function Push(v: TOleVariantType): TCHandle;        // insert element at front
    function Pop: TOleVariantType;            // remove first element
    function PushBack(v: TOleVariantType): TCHandle;    // insert element at back
    function PopBack: TOleVariantType;        // remove last element

    // queue operations
    function Enqueue(v: TOleVariantType): TCHandle;    // add to tail
    function Dequeue: TOleVariantType;       // read&remove from head

    procedure Sort(compare: TOnOleVariantCompare); overload;
    procedure Sort(compare: TOnOleVariantCompare; AFirst, ALast: TCHandle); overload;
    procedure Sort(compare: TOnOleVariantCompareProc); overload;
    procedure Sort(compare: TOnOleVariantCompareProc; AFirst, ALast: TCHandle); overload;

    property Map[key: TOleVariantType]: variant Read GetMap Write PutMap;
    property Items[handle: TCHandle]: TOleVariantType Read GetItem Write SetItem; default;
    property OnCompare: TOnOleVariantCompare Read fOnCompare Write SetOnCompare;
    property OnCompareProc: TOnOleVariantCompareProc Read fOnCompareProc Write SetOnCompareProc;
  end;
  TTOleVariantField = TTOleVariant;

  TConsoleOutputStream = class(TStream)
  protected
    function GetSize: Int64; override;
    procedure SetSize(NewSize: Longint); override;
    procedure SetSize(const NewSize: Int64); override;
  public
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    class procedure writeln;
  end;

  TAnsiSet = set of AnsiChar;

var
  IntSymbolsA: AnsiString;
  FloatSymbolsA: AnsiString;
  IntSymbolsW: WideString;
  FloatSymbolsW: WideString;
  IntSymbols: String;
  FloatSymbols: String;

// variant -> VariantType:int + VariantValue:string
function VariantToStr(const v: variant): string;
function VariantToAnsiStr(const v: variant): Ansistring;
function VariantToWideStr(const v: variant): WideString;

// VariantType:int + VariantValue:string -> variant
function AnsiStrToVariant(v: AnsiString; vtype: integer = -1): variant;
function WideStrToVariant(v: WideString; vtype: integer = -1): variant;

function WideStringToStr(const v: WideString): string;
function AnsiStringToStr(const v: AnsiString): string;
function AnsiStringToWideStr(const v: AnsiString): WideString;
function WideStringToAnsiStr(const v: WideString): AnsiString;
function StrToSingle(const s: string): single;
function StrToDWord(const s: string): dword;
function StrToWordBool(const s: string): WordBool;
{$IFDEF HiCompiler}
function AnsiStringToUInt64(const v: AnsiString): UInt64;
function WideStringToUInt64(const v: WideString): UInt64;
{$ENDIF}

function VarAsBoolean(p: PVarRec): boolean;
function VarAsInteger(p: PVarRec): integer;
function VarAsChar(p: PVarRec): char;
function VarAsAnsiChar(p: PVarRec): AnsiChar;
function VarAsExtended(p: PVarRec): extended;
function VarAsDouble(p: PVarRec): double;
function VarAsString(p: PVarRec): string;
function VarAsPointer(p: PVarRec): pointer;
function VarAsPChar(p: PVarRec): PChar;
function VarAsObject(p: PVarRec): TObject;
function VarAsClass(p: PVarRec): TClass;
function VarAsWideChar(p: PVarRec): widechar;
function VarAsPWideChar(p: PVarRec): PWideChar;
function VarAsAnsiString(p: PVarRec): ansistring;
function VarAsCurrency(p: PVarRec): currency;
function VarAsVariant(p: PVarRec): variant;
function VarAsInterface(p: PVarRec): variant;
function VarAsWideString(p: PVarRec): WideString;
function VarAsInt64(p: PVarRec): int64;
{$IFDEF HiCompiler}
function VarAsUInt64(p: PVarRec): uint64;
function VarAsUnicodeString(p: PVarRec): UnicodeString;
{$ENDIF}

procedure WriteInteger(dst: TStream; value: integer);
procedure WriteByte(dst: TStream; value: byte);
procedure WriteString(dst: TStream; value: string);
procedure WriteAnsiString(dst: TStream; value: ansistring);
procedure WriteWideString(dst: TStream; value: widestring);
procedure WriteSingle(dst: TStream; value: single);
procedure WriteDouble(dst: TStream; value: double);
procedure WriteExtended(dst: TStream; value: extended);
procedure WriteCurrency(dst: TStream; value: currency);
procedure WriteDate(dst: TStream; value: TDateTime);
procedure WriteBoolean(dst: TStream; value: boolean);
procedure WriteInt64(dst: TStream; value: int64);
procedure WriteDWord(dst: TStream; value: dword);
procedure WriteSmallint(dst: TStream; value: smallint);
procedure WriteWord(dst: TStream; value: word);
procedure WriteVariant(dst: TStream; value: variant);
procedure WriteOleVariant(dst: TStream; value: olevariant);
procedure WriteShortInt(dst: TStream; value: shortint);

function ReadInteger(src: TStream): integer;
function ReadByte(src: TStream): byte;
function ReadString(src: TStream): string;
function ReadAnsiString(src: TStream): ansistring;
function ReadWideString(src: TStream): widestring;
function ReadSingle(src: TStream): single;
function ReadDouble(src: TStream): Double;
function ReadExtended(src: TStream): Extended;
function ReadCurrency(src: TStream): Currency;
function ReadDate(src: TStream): TDateTime;
function ReadBoolean(src: TStream): boolean;
function ReadInt64(src: TStream): Int64;
function ReadDWord(src: TStream): DWord;
function ReadSmallint(src: TStream): Smallint;
function ReadWord(src: TStream): Word;
function ReadVariant(src: TStream): variant;
function ReadOleVariant(src: TStream): olevariant;
function ReadShortInt(src: TStream): ShortInt;

procedure Exch(var a,b: integer); overload;
procedure Exch(var a,b: pointer); overload;

procedure Swap1(var a,b,temp; size: integer);
procedure Swap2(var a,b,temp; size: integer);
procedure Swap4(var a,b,temp; size: integer);
{$IFNDEF FPC}
procedure Swap3(var a,b,temp; size: integer);
procedure Swap8(var a,b,temp; size: integer);
procedure Swap12(var a,b,temp; size: integer);
procedure Swap16(var a,b,temp; size: integer);
procedure Swap20(var a,b,temp; size: integer);
procedure Swap24(var a,b,temp; size: integer);
{$ENDIF}

procedure Move1(const Source; var Dest; count : Integer);
procedure Move2(const Source; var Dest; count : Integer);
procedure Move4(const Source; var Dest; count : Integer);
{$IFNDEF FPC}
procedure Move3(const Source; var Dest; count : Integer);
procedure Move8(const Source; var Dest; count : Integer);
procedure Move12(const Source; var Dest; count : Integer);
procedure Move16(const Source; var Dest; count : Integer);
procedure Move20(const Source; var Dest; count : Integer);
procedure Move24(const Source; var Dest; count : Integer);
{$ENDIF}

function GetSwapProc(BlockSize: integer): TSwapProc;
function GetMoveProc(BlockSize: integer): TMoveProc;

function FPoint(AX,AY: double):TFPoint;
function FRect(ATop, ALeft, ARight, ABottom: double):TFRect;

type
  TStrToken = record
    Symbols: String;
    Text: String; // source text
    Pos: integer; // position to read next symbol
    Token: String; // last token
  end;

  TStrTokenW = record
    Symbols: WideString;
    Text: WideString; // source text
    Pos: integer; // position to read next symbol
    Token: WideString; // last token
  end;

  TStrTokenA = record
    Symbols: AnsiString;
    Text: AnsiString; // source text
    Pos: integer; // position to read next symbol
    Token: AnsiString; // last token
  end;

function SortString(Const s: string): string;
function SortAnsiString(Const s: AnsiString): AnsiString;
function SortWideString(Const s: WideString): WideString;

function CharInString(C: Char; const S: String): boolean;
function CharInAnsiString(C: AnsiChar; const S: AnsiString): boolean;
function CharInWideString(C: WideChar; const S: WideString): boolean;

function SetToAnsiString(Const S: TAnsiSet): AnsiString;

// ansi string token operations
procedure STokOpenA(const S: AnsiString; Symbols: TAnsiSet; var H: TStrTokenA); overload;
procedure STokOpenA(const S: AnsiString; Symbols: AnsiString; var H: TStrTokenA); overload;
procedure STokOpenA(var H: TStrTokenA); overload;
function STokReadA(var H: TStrTokenA):integer;
function STokCountA(var H: TStrTokenA):integer; overload;
function STokCountA(const S: AnsiString; Symbols: TAnsiSet):integer; overload;
function STokCountA(const S: AnsiString; Symbols: AnsiString):integer; overload;
procedure STokCloseA(var H: TStrTokenA);

function STokCountIntA(S: AnsiString): integer;
function STokCountFloatA(S: AnsiString): integer;
procedure STokOpenIntA(const S: AnsiString; var H: TStrTokenA);
procedure STokOpenFloatA(const S: AnsiString; var H: TStrTokenA);
function STokReadIntA(var H: TStrTokenA):integer;
function STokReadFloatA(var H: TStrTokenA):double;

// wide string token operations
procedure STokOpenW(const S: WideString; Symbols: TAnsiSet; var H: TStrTokenW); overload;
procedure STokOpenW(const S: WideString; Symbols: WideString; var H: TStrTokenW); overload;
procedure STokOpenW(var H: TStrTokenW); overload;
function STokReadW(var H: TStrTokenW):integer;
function STokCountW(var H: TStrTokenW):integer; overload;
function STokCountW(const S: WideString; Symbols: TAnsiSet):integer; overload;
function STokCountW(const S: WideString; Symbols: WideString):integer; overload;
procedure STokCloseW(var H: TStrTokenW);

function STokCountIntW(S: WideString): integer;
function STokCountFloatW(S: WideString): integer;
procedure STokOpenIntW(const S: WideString; var H: TStrTokenW);
procedure STokOpenFloatW(const S: WideString; var H: TStrTokenW);
function STokReadIntW(var H: TStrTokenW):integer;
function STokReadFloatW(var H: TStrTokenW):double;

// wide string token operations
procedure STokOpen(const S: String; Symbols: TAnsiSet; var H: TStrToken); overload;
procedure STokOpen(const S: String; Symbols: String; var H: TStrToken); overload;
procedure STokOpen(var H: TStrToken); overload;
function STokRead(var H: TStrToken):integer;
function STokCount(var H: TStrToken):integer; overload;
function STokCount(const S: String; Symbols: TAnsiSet):integer; overload;
function STokCount(const S: String; Symbols: String):integer; overload;
procedure STokClose(var H: TStrToken);

function STokCountInt(S: String): integer;
function STokCountFloat(S: String): integer;
procedure STokOpenInt(const S: String; var H: TStrToken);
procedure STokOpenFloat(const S: String; var H: TStrToken);
function STokReadInt(var H: TStrToken):integer;
function STokReadFloat(var H: TStrToken):double;

procedure DListRemove(n: P_SDListItem; List: PDList);
procedure DListInsertBefore(v, p: P_SDListItem; list: PDList);
procedure DListInsertAfter(v, p: P_SDListItem; list: PDList);
procedure DListAdd(v: P_SDListItem; list: PDList);
procedure DListAddSorted(v: P_SDListItem; list: PDList; Comparator: TCDListComparator);
procedure DListExchange(a, b: P_SDListItem; list: PDList); overload;
procedure DListExchange(a, b: P_SDListItem; list_a, list_b: PDList); overload;
procedure DListReverse(AFirst, ALast: P_SDListItem; list: PDList);
procedure DListMove(CurPos, NewPos: P_SDListItem; List: PDList); overload;
procedure DListMove(CurPos, NewPos: P_SDListItem; CurList, NewList: PDList); overload;
procedure DListRotate(AFirst, ALast: P_SDListItem; shift: integer; list: PDList);
procedure DListRandomShuffle(AFirst, ALast: P_SDListItem; List: PDList);
procedure DListSort(List: PDList; AFirst, ALast: P_SDListItem; Comparator: TCDListComparator);

procedure VectorLongwordReverse(Data: PLongwordList; Count: integer);
procedure VectorLongwordRotate(Data: PLongwordList; Count, Shift: integer);
procedure VectorLongwordQuickSort(Data: PLongwordList; Count: integer);
procedure VectorLongwordJumpSort(Data: PLongwordList; Count: integer);

procedure VectorIntegerReverse(Data: PIntegerList; Count: integer);
procedure VectorIntegerRotate(Data: PIntegerList; Count, Shift: integer);
procedure VectorIntegerQuickSort(Data: PIntegerList; Count: integer);
procedure VectorIntegerJumpSort(Data: PIntegerList; Count: integer);

procedure VectorWordReverse(Data: PWordList; Count: integer);
procedure VectorWordRotate(Data: PWordList; Count, Shift: integer);
procedure VectorWordQuickSort(Data: PWordList; Count: integer);
procedure VectorWordJumpSort(Data: PWordList; Count: integer);

procedure VectorSmallintReverse(Data: PSmallintList; Count: integer);
procedure VectorSmallintRotate(Data: PSmallintList; Count, Shift: integer);
procedure VectorSmallintQuickSort(Data: PSmallintList; Count: integer);
procedure VectorSmallintJumpSort(Data: PSmallintList; Count: integer);

procedure VectorByteReverse(Data: PByteList; Count: integer);
procedure VectorByteRotate(Data: PByteList; Count, Shift: integer);
procedure VectorByteQuickSort(Data: PByteList; Count: integer);
procedure VectorByteCountSort(Data: PByteList; Count: integer);

procedure VectorShortintReverse(Data: PShortintList; Count: integer);
procedure VectorShortintRotate(Data: PShortintList; Count, Shift: integer);
procedure VectorShortintQuickSort(Data: PShortintList; Count: integer);
procedure VectorShortintCountSort(Data: PShortintList; Count: integer);

procedure VectorLongintReverse(Data: PLongintList; Count: integer);
procedure VectorLongintRotate(Data: PLongintList; Count, Shift: integer);
procedure VectorLongintQuickSort(Data: PLongintList; Count: integer);
procedure VectorLongintJumpSort(Data: PLongintList; Count: integer);

procedure VectorCardinalReverse(Data: PCardinalList; Count: integer);
procedure VectorCardinalRotate(Data: PCardinalList; Count, Shift: integer);
procedure VectorCardinalQuickSort(Data: PCardinalList; Count: integer);

procedure VectorUInt64Reverse(Data: PInt64List; Count: integer);
procedure VectorUInt64Rotate(Data: PInt64List; Count, Shift: integer);
procedure VectorUInt64JumpSort(Data: {$IFDEF HiCompiler}PUInt64List{$ELSE}PInt64List{$ENDIF}; Count: integer);

procedure VectorInt64Reverse(Data: PInt64List; Count: integer);
procedure VectorInt64Rotate(Data: PInt64List; Count, Shift: integer);
procedure VectorInt64QuickSort(Data: PInt64List; Count: integer);
procedure VectorInt64JumpSort(Data: PInt64List; Count: integer);

procedure VectorRealQuickSort(Data: PRealList; Count: integer);
{$IFNDEF FPC}
procedure VectorReal48QuickSort(Data: PReal48List; Count: integer);
{$ENDIF}
procedure VectorSingleQuickSort(Data: PSingleList; Count: integer);
procedure VectorDoubleQuickSort(Data: PDoubleList; Count: integer);
procedure VectorExtendedQuickSort(Data: PExtendedList; Count: integer);
procedure VectorCompQuickSort(Data: PCompList; Count: integer);
procedure VectorCurrencyQuickSort(Data: PCurrencyList; Count: integer);

procedure VectorCharQuickSort(Data: PCharList; Count: integer);
procedure VectorAnsiCharQuickSort(Data: PAnsiCharList; Count: integer);
procedure VectorWideCharQuickSort(Data: PWideCharList; Count: integer);

implementation

constructor TCVector.Create;
begin
  inherited Create;
  InitZero := True;
end;

destructor TCVector.Destroy;
begin
  Count := 0;
  capacity := 0;
  itemsize := 0;
  FieldSize := 0;
  inherited;
end;

function TCVector.vecAdd: TCHandle;
begin
  Result := FCount;
  if Result = FCapacity then
    Grow;
  Inc(FCount);
end;

function TCVector.AddItem(var item): TCHandle;
begin
  if FSorted then
  begin
    BSearch(item, Result);
    if Result >= Count then
      Result := vecAdd
    else
      Result := Insert(handles[Result]);
    FMoveField(item, buf[result * FItemSize], FFieldSize);
  end
  else
  begin
    Result := FCount;
    if Result = FCapacity then
      Grow;
    Inc(FCount);
    FMoveField(item, buf[result * FItemSize], FFieldSize);
  end;
end;

function TCVector.InsertItem(handle: TCHandle; var item): TCHandle;
begin
  Result := Insert(handle);
  FMoveField(item, FBuf[result*FItemSize], FFieldSize);
end;

function TCVector.InsertAfterItem(handle: TCHandle; var item): TCHandle;
begin
  Result := Insert(handle + 1);
  FMoveField(item, FBuf[result*FItemSize], FFieldSize);
end;

function TCVector.Find(var Item): TCHandle;
var
  p: ^byte;
  i: integer;
begin
  if FSorted then
  begin
    if not BSearch(item, Result) then
      Result := -1;
  end
  else
  begin
    p := pointer(FBuf);
    for i := 0 to FCount-1 do
      if FCompareValue(Item, p^)<>0 then
        inc(p, FItemSize)
      else
      begin
        result := i;
        exit;
      end;
    result := -1;
  end;
end;

function TCVector.FindFirstEqual(var Item): TCHandle;
var
  p: ^byte;
begin
  result := Find(item);
  if not FSorted or (result<=0) then
    exit;
  p := @FBuf[(result-1)*FItemSize];
  while result>0 do
    if FCompareValue(Item, p^)<>0 then
      break
    else
    begin
      dec(result);
      dec(p, FItemSize);
    end;
end;

procedure TCVector.FindNextEqual(var Item; var handle: TCHandle);
var
  p: ^byte;
  i: integer;
begin
  if handle>=FCount-1 then
    handle := -1;
  if handle=-1 then
    exit;
  inc(handle);
  p := @FBuf[handle*FItemSize];
  if FSorted then
  begin
    if FCompareValue(item, p^)<>0 then
      handle := -1;
    exit;
  end;
  for i := handle to FCount-1 do
    if FCompareValue(item, p^)<>0 then
      inc(p, FItemSize)
    else
    begin
      handle := i;
      exit;
    end;
  handle := -1;
end;

function TCVector.FindMin: TCHandle;
var
  i: TCHandle;
  p,m: ^byte;
begin
  if FCount = 0 then
  begin
    Result := -1;
    exit;
  end;

  if FSorted then
  begin
    result := 0;
    exit;
  end;

  m := pointer(FBuf);
  result := 0;
  p := m;
  inc(p, FItemSize);
  for i := 1 to FCount-1 do
  begin
    if FCompareValue(p^, m^)<0 then
    begin
      m := p;
      result := i;
    end;
    inc(p, FItemSize);
  end;
end;

function TCVector.FindMax: TCHandle;
var
  i: TCHandle;
  p,m: ^byte;
begin
  if FCount = 0 then
  begin
    Result := -1;
    exit;
  end;

  if FSorted then
  begin
    result := FCount-1;
    exit;
  end;

  m := pointer(FBuf);
  result := 0;
  p := m;
  inc(p, FItemSize);
  for i := 1 to FCount-1 do
  begin
    if FCompareValue(p^, m^)>0 then
    begin
      m := p;
      result := i;
    end;
    inc(p, FItemSize);
  end;
end;

procedure TCVector.Clear;
begin
  Count := 0;
  if FGrowDown then
    Capacity := 0;
end;

procedure TCVector.Copy(Src, Dst: TCHandle);
begin
  assert((src >= 0) and (src < FCount) and (dst >= 0) and (dst < FCount), erhandle);
  if FDelCount > 0 then
    ItemDeleted(dst);
  if FCopyCount > 0 then
    CopyItem(src, dst)
  else
    FMoveItem(Buf[src * FItemSize], Buf[dst * FItemSize], FItemSize);
end;

procedure TCVector.Delete(handle: TCHandle);
begin
  assert((handle >= 0) and (handle < FCount), erhandle);
  if FDelCount > 0 then
    ItemDeleted(handle);
  Dec(FCount);
  if handle < FCount then
    System.Move(Buf[(handle + 1) * FItemSize], Buf[handle * FItemSize], (FCount - handle) * FItemSize);
  if FGrowDown then
    Capacity := FCount;
end;

procedure TCVector.DeleteNext(handle: TCHandle); // effective way to delete something for list of one-way linked items
begin
  Delete(handle + 1);
end;

procedure TCVector.RemoveAllSorted(var Item);
var i,f,l: integer;
begin
  BSearch(Item, i);
  if (i<0) or (i>FCount-1) then
    exit;
  f := i;
  l := i;
  while f>0 do
    if FCompareValue(item, get(f-1)^)=0 then
      dec(f)
    else
      break;
  while l<FCount-1 do
    if FCompareValue(item, get(l+1)^)=0 then
      inc(l)
    else
      break;
  if FDelCount > 0 then
    for i := f to l do
      ItemDeleted(i);
  System.Move(Buf[(l+1)*FItemSize], Buf[f*FItemSize], (FCount-l-1) * FItemSize);
  FCount := FCount-(l-f+1);
  if FGrowDown then
    capacity := FCount;
end;

procedure TCVector.RemoveAllUnsorted(var Item);
var i,j,f,l,dst: integer;
begin

  // skip block of items if they <>item
  i := 0;
  while i<=FCount-1 do
    if FCompareValue(item, get(i)^)<>0 then
      inc(i)
    else
      break;
  dst := i;

  while i<=FCount-1 do
  begin

    // now I it is FIRST position of equal items
    f := i;
    while i<FCount-1 do
      if FCompareValue(item, get(i+1)^)=0 then
        inc(i)
      else
        break;
    l := i;
    if FDelCount > 0 then
      for j := f to l do
        ItemDeleted(j);
    if i=FCount-1 then
      break;

    // now I it is LAST position of equal items
    inc(i);
    f := i;
    while i<FCount-1 do
      if FCompareValue(item, get(i+1)^)<>0 then
        inc(i)
      else
        break;
    l := i;

    // [f:l] -> [dst]
    if f<FCount then
    begin
      if f=l then
        FMoveItem(Buf[f*FItemSize], Buf[dst*FItemSize], FItemSize)
      else
        System.Move(Buf[f*FItemSize], Buf[dst*FItemSize], (l-f+1) * FItemSize);
      inc(dst,l-f+1);
    end;
    inc(i);
  end;
  FCount := dst;
  if FGrowDown then
    capacity := FCount;
end;

procedure TCVector.RemoveAll(var Item);
begin
  if FSorted then
    RemoveAllSorted(item)
  else
    RemoveAllUnsorted(item);
end;

procedure TCVector.Reverse(AFirst, ALast: TCHandle);
var i: integer;
begin
  if ALast<=AFirst then
    if ALast=AFirst then
      exit
    else
      Exch(AFirst, ALast);
  for i := 0 to (ALast-AFirst+1) shr 1-1 do
  begin
    FSwapItem(Buf[AFirst*FItemSize], Buf[ALast*FItemSize], ftemp^, FItemSize);
    inc(AFirst);
    dec(ALast);
  end;
end;

{
  1 2 3 4 5
  shift 2 (right)
  4 5 1 2 3
  shift -2 (left)
  1 2 3 4 5
}
procedure TCVector.Rotate(AFirst, ALast: TCHandle; Shift: integer);
var w: integer;
begin
  if ALast<=AFirst then
    if ALast=AFirst then
      exit
    else
      Exch(AFirst, ALast);
  assert((AFirst>=0) and (ALast<FCount), erHandle);
  w := ALast-AFirst+1;
  shift := -shift mod w;
  if shift<0 then
    inc(shift, w);
  if shift=0 then
    exit;
  reverse(AFirst, AFirst+Shift-1);
  reverse(AFirst+Shift, ALast);
  reverse(AFirst, ALast);
end;

procedure TCVector.RandomShuffle(AFirst, ALast: TCHandle);
var i,w: integer;
begin
  if AFirst=ALast then
    exit;
  if AFirst>ALast then
    exch(AFirst, ALast);
  assert((AFirst>=0) and (ALast>=0) and (AFirst<FCount) and (ALast<FCount));
  w := ALast-AFirst+1;
  for i := AFirst to ALast do
    Swap(i, random(w)+AFirst);
end;

function TCVector.NextPermutation:boolean;
var
  i,x,n: integer;
begin

  // find max N where A[N] < A[N+1]
  n := -1;
  for i := FCount-2 downto 0 do
    if FCompareValue(FBuf[i*FItemSize], FBuf[(i+1)*FItemSize]) < 0 then
    begin
      n := i;
      break;
    end;

  // if A[N] > A[N+1] for any N then there is no more permutations
  result := n<>-1;
  if not result then
    exit;

  // let's order range [N+1; FCoun-1]
  // now it has reverse order so just call .reverse
  reverse(n+1,FCount-1);

  // find value next to A[N] in range [N+1; FCount-1]
  // such value exists because at least original A[N+1] > A[N]
  x := -1;
  for i := N+1 to FCount-1 do
    if FCompareValue(FBuf[i*FItemSize], FBuf[N*FItemSize]) > 0 then
    begin
      x := i;
      break;
    end;

  // swap A[N] and A[X]
  swap(n,x);

  // change position of A[X] to make range [N+1; FCoun-1] ordered again
  i := x;
  while (i > n+1) and (FCompareValue(FBuf[(i-1)*FItemSize], FBuf[x*FItemSize]) > 0) do
    dec(i);
  while (i < FCount-1) and (FCompareValue(FBuf[x*FItemSize], FBuf[(i+1)*FItemSize]) > 0) do
    inc(i);
  if i<>x then
    Move(x,i);
end;

function TCVector.PrevPermutation:boolean;
var
  i,x,n: integer;
begin

  // find max N where A[N] > A[N+1]
  n := -1;
  for i := FCount-2 downto 0 do
    if FCompareValue(FBuf[i*FItemSize], FBuf[(i+1)*FItemSize]) > 0 then
    begin
      n := i;
      break;
    end;

  // if A[N] > A[N+1] for any N then there is no more permutations
  result := n<>-1;
  if not result then
    exit;

  // let's order range [N+1; FCoun-1]
  // now it has reverse order so just call .reverse
  reverse(n+1,FCount-1);

  // find value previous to A[N] in range [N+1; FCount-1]
  // such value exists because at least original A[N+1] < A[N]
  x := -1;
  for i := N+1 to FCount-1 do
    if FCompareValue(FBuf[i*FItemSize], FBuf[N*FItemSize]) < 0 then
    begin
      x := i;
      break;
    end;

  // swap A[N] and A[X]
  swap(n,x);

  // change position of A[X] to make range [N+1; FCoun-1] back ordered again
  i := x;
  while (i > n+1) and (FCompareValue(FBuf[(i-1)*FItemSize], FBuf[x*FItemSize]) < 0) do
    dec(i);
  while (i < FCount-1) and (FCompareValue(FBuf[x*FItemSize], FBuf[(i+1)*FItemSize]) < 0) do
    inc(i);
  if i<>x then
    Move(x,i);
end;

function TCVector.ExtCmp(a,b: integer):integer;
begin
  result := FSortCompare(FSortField, a,b);
end;

function TCVector.ExtCmpProc(a,b: integer):integer;
begin
  result := FSortCompareProc(FSortField, a,b);
end;

// - user-defined by-index comparator (FComparator)
// - universal swap (itemsize may vary)
procedure TCVector.QSort(l, r: integer);
var
  I, J, P: integer;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while FComparator(I, P) < 0 do
        Inc(I);
      while FComparator(J, P) > 0 do
        Dec(J);
      if I <= J then
      begin
        Swap(I, J);
        if P = I then
          P := J
        else if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QSort(L, J);
    L := I;
  until I >= R;
end;

// - internal TT* comparator
// - universal swap (itemsize may vary)
procedure TCVector.QSortInt(l, r: integer);
var
  I, J, P, FI: integer;
  PP,PI,PJ: ^byte;
  CV: TOnCompareValues;
begin
  FI := FItemSize;
  CV := FCompareValue;
  repeat
    I := L;
    PI := @Buf[i*FI];
    J := R;
    PJ := @Buf[j*FI];
    P := (L + R) shr 1;
    PP:= @Buf[P*FI];
    repeat
      while CV(PI^, PP^) < 0 do
      begin
        Inc(I);
        inc(PI, FI);
      end;
      while CV(PJ^, PP^) > 0 do
      begin
        Dec(J);
        dec(PJ, FI);
      end;
      if I <= J then
      begin
        Swap(I, J);
        if P = I then
        begin
          P := J;
          PP := PJ;
        end
        else if P = J then
        begin
          P := I;
          PP := PI;
        end;
        Inc(I);
        inc(PI, FI);
        Dec(J);
        dec(PJ, FI);
      end;
    until I > J;
    if L < J then
      QSortInt(L, J);
    L := I;
  until I >= R;
end;

// - internal TT* comparator
// - internal 4-bytes swap
procedure TCVector.QuickInt4(L, R: Integer);
var
  I, J: Integer;
  P, T: Pointer;
  S: PPointerList;
  CV: TOnCompareValues;
begin
  S := pointer(FBuf);
  CV := FCompareValue;
  repeat
    I := L;
    J := R;
    P := S^[(L + R) shr 1];
    repeat
      while CV(S^[I], P) < 0 do
        Inc(I);
      while CV(S^[J], P) > 0 do
        Dec(J);
      if I <= J then
      begin
        T := S[I];
        S[I] := S[J];
        S[J] := T;
        if P = @S[I] then
          P := @S[J]
        else
          if P = @S[J] then
            P := @S[I];
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickInt4(L, J);
    L := I;
  until I >= R;
end;

function TCVector.BSearch(var Item; var pos: integer): boolean;
var
  f, l, m: integer;
begin
  Result := FCount>0;
  if not result then
  begin
    pos := 0;
    exit;
  end;

  f := 0;
  l := FCount - 1;
  while (l - f > 1) do
  begin
    m := (f + l) div 2;
    if FCompareValue(FBuf[m*FItemSize], item) < 0 then
      f := m
    else
      l := m;
  end;

  if FCompareValue(FBuf[f*FItemSize], item) = 0 then
    pos := f
  else
  if FCompareValue(FBuf[l*FItemSize], item) = 0 then
    pos := l
  else
  begin // let's find "potential" position for such item
    if FCompareValue(item, FBuf[f*FItemSize]) <= 0 then
      pos := f
    else
    if FCompareValue(item, FBuf[l*FItemSize]) >= 0 then
      pos := l + 1
    else
      pos := l;
    Result := false;
  end;
end;

procedure TCVector.Swap(handle1, handle2: TCHandle);
begin
  assert((handle1 >= 0) and (handle1 < FCount) and (handle2 >= 0) and (handle2 < FCount), erhandle);
  FSwapItem(Buf[handle1*FItemSize], Buf[handle2*FItemSize], ftemp^, FItemSize);
end;

procedure TCVector.Exchange(handle1, handle2: TCHandle);
begin
  assert((handle1 >= 0) and (handle1 < FCount) and (handle2 >= 0) and (handle2 < FCount), erhandle);
  FSwapItem(Buf[handle1*FItemSize], Buf[handle2*FItemSize], ftemp^, FItemSize);
end;

function TCVector.Get(handle: TCHandle): Pointer;
begin
  assert((handle >= 0) and (handle < FCount),erhandle);
  Result := @Buf[handle * FItemSize];
end;

function TCVector.GetIndex(handle: TCHandle): integer;
begin
  Result := handle; // for vector it is same (index=handle)
end;

function TCVector.GetHandle(index: integer): TCHandle;
begin
  Result := index; // for vector it is same (index=handle)
end;

procedure TCVector.Grow;
var
  Delta: integer;
begin
  if FCapacity > 64 then
    Delta := FCapacity div 4
  else
    if FCapacity > 8 then
      Delta := 16
    else
      Delta := 4;
  Capacity := FCapacity + Delta;
end;

function TCVector.First: TCHandle;
begin
  if FCount > 0 then
    Result := 0
  else
    Result := -1;
end;

function TCVector.Last: TCHandle;
begin
  Result := FCount - 1;
end;

procedure TCVector.Next(var n: TCHandle);
begin
  if (n >= 0) and (n < FCount - 1) then
    inc(n)
  else
    n := -1;
end;

procedure TCVector.Prev(var n: TCHandle);
begin
  if (n > 0) then
    dec(n)
  else
    n := -1;
end;

function TCVector.EOF(n: TCHandle): boolean;
begin
  Result := n < 0;
end;

// internal-comparing sort:
//  - we use CompareValue event to compare
//  - we use internal swap when possible
procedure TCVector.Sort(AFirst, ALast: TCHandle);
begin
  if Count <= 1 then
    exit;
  if (length(FFields)=1) and assigned(FField.FSortMemoryRange) then
    FField.FSortMemoryRange(pointer(Buf), AFirst, ALast)
  else
    if (ItemSize=4) then
      QuickInt4(AFirst, ALast) // FCompareValue (from TT*) + internal 4-bytes swap
    else
      QSortInt(AFirst, ALast); // FCompareValue (from TT*) + Swap
end;

procedure TCVector.Sort(compare: TOnCompareValues; AFirst, ALast: TCHandle);
var p: TOnCompareValues;
begin
  if Count <= 1 then
    exit;
  p := FCompareValue;
  try
    FCompareValue := compare;
    if ItemSize=4 then
      QuickInt4(AFirst, ALast) // FCompareValue (user-defined) + internal 4-bytes swap
    else
      QSortInt(AFirst, ALast); // FCompareValue (user-defined) + Swap
  finally
    FCompareValue := p;
  end;
end;

// external-comparing sort:
//  - we use FComparator event to compare (come from user and most flexible one)
//  - we use external swap
procedure TCVector.Sort(field: TTCustom; compare: TOnCompare; AFirst, ALast: TCHandle);
begin
  assert((AFirst>=0) and (AFirst<FCount) and (ALast>=0) and (ALast<FCount));
  if Count<=0 then
    exit;
  FSortCompare := compare;
  FSortCompareProc := nil;
  FSortField := field;
  FComparator := ExtCmp;
  QSort(AFirst, ALast); // FComparator (ExtCmp -> user-defined) + Swap
  FComparator := nil;
end;

procedure TCVector.Sort(field: TTCustom; compare: TOnCompareProc; AFirst, ALast: TCHandle);
begin
  assert((AFirst>=0) and (AFirst<FCount) and (ALast>=0) and (ALast<FCount));
  if Count<=0 then
    exit;
  FSortCompareProc := compare;
  FSortCompare := nil;
  FSortField := field;
  FComparator := ExtCmpProc;
  QSort(AFirst, ALast);  // FComparator (ExtCmpProc -> user-defined) + Swap
  FComparator := nil;
end;

function TCVector.Insert(handle: TCHandle): TCHandle;
begin
  if (FCount = 0) and (handle = -1) then
    Result := vecAdd
  else
  begin
    Result := handle;
    assert((handle >= 0) and (handle <= FCount),erhandle);
    if FCount = FCapacity then
      Grow;
    if handle < FCount then
      System.Move(Buf[handle * FItemSize], Buf[(handle + 1) * FItemSize], (FCount - handle) * FItemSize);
    if FInitZero then
      fillchar(Buf[handle * FItemSize], FItemSize, 0);
    Inc(FCount);
  end;
end;

function TCVector.InsertAfter(handle: TCHandle): TCHandle;
begin
  Result := Insert(handle + 1);
end;

procedure TCVector.Move(CurHandle, NewHandle: TCHandle);
begin
  assert((CurHandle>=0) and (CurHandle<=FCount) and (NewHandle>=0) and (NewHandle<=FCount), erHandle);
  if CurHandle = NewHandle then
    exit;
  FMoveItem(Buf[CurHandle * FItemSize], ftemp^, FItemSize);
  if CurHandle<NewHandle then
    if NewHandle-CurHandle=1 then
      FMoveItem(Buf[(CurHandle+1) * FItemSize], Buf[CurHandle * FItemSize], FItemSize)
    else
      System.move(Buf[(CurHandle+1) * FItemSize], Buf[CurHandle * FItemSize], (NewHandle-CurHandle)*FItemSize)
  else
    if CurHandle-NewHandle=1 then
      FMoveItem(Buf[NewHandle * FItemSize], Buf[(NewHandle+1) * FItemSize], FItemSize)
    else
      System.move(Buf[NewHandle * FItemSize], Buf[(NewHandle+1) * FItemSize], (CurHandle-NewHandle)*FItemSize);
  FMoveItem(ftemp^, Buf[NewHandle * FItemSize], FItemSize);
end;

procedure TCVector.Put(handle: TCHandle; var Item);
begin
  assert((handle>=0) and (handle<=FCount), erHandle);
  FMoveField(item, buf[handle * FItemSize], FFieldSize);
end;

procedure TCVector.SetCapacity(n: integer);
var
  m: integer;
begin
  assert(n >= FCount, erCapacity);
  if n = FCapacity then
    exit;
  if FInitZero then
    m := n - FCapacity
  else
    m := 0;
  reallocmem(FBuf, n * FItemSize);
  if m > 0 then
    fillchar(buf[(n - m) * FItemSize], m * FItemSize, 0);
  FCapacity := n;
end;

function TCVector.GetRangeLength(start, finish: TCHandle):integer;
begin
  if start=-1 then
    result := 0
  else
    result := finish-start+1;
end;

function TCVector.GetCapacity: integer;
begin
  result := FCapacity;
end;

procedure TCVector.SetCount(n: integer);
var
  I, j: integer;
begin
  assert(n >= 0, erCount);
  if (n < FCount) and (FDelCount > 0) then
    for j := FCount - 1 downto n do
      ItemDeleted(j);
  i := FCount;
  FCount := n; // to prevent erCapacity from .SetCap
  if (n > FCapacity) or ((n < FCapacity) and FGrowDown) then
    Capacity := n;
  if (n > i) and FInitZero then
    FillChar(Get(i)^, (n - i) * FITemSize, 0);
end;

{ TBaseContainer }

constructor TCCustom.Create;
begin
  FFieldSeparator := ' ';
  FEOL := #13#10;
end;

function TCCustom.GetMainContainer:TCCustom;
begin
  result := self;
  while result is T_CJoin do
    result := T_CJoin(result).FParent;
end;

// creates empty container of specified type with copy of all fields
function TCCustom.CloneShell(ContainerType: CContainer):pointer;
var
  i: integer;
  t,f: TTCustom;
  m,r: TCCustom;
  c: T_CJoin;
begin
  m := GetMainContainer;
  r := ContainerType.Create;
  result := r;
  for i := low(m.FFields) to high(m.FFields) do
  begin
    t := m.Fields[i];
    if i=0 then
      CTT(t.ClassType).Create(r)
    else
    begin
      c := T_CJoin.CreateJoined(r, nil);
      f := CTT(t.ClassType).Create(c);
      c.FField :=f;
    end;
  end;
end;

// create EMPTY copy of container including all fields
function TCCustom.CloneShell:pointer;
begin
  result := CloneShell( CContainer(GetMainContainer.ClassType) );
end;

// clone container including all fields and data
function TCCustom.Clone:pointer;
begin
  result := CloneShell;
  TCCustom(result).Assign(GetMainContainer);
end;

function TCCustom.Clone(ContainerType: CContainer):pointer;
begin
  result := CloneShell(ContainerType);
  TCCustom(result).Assign(GetMainContainer);
end;

procedure TCCustom.Assign(src: TCCustom);
var
  i,j: integer;
begin
  assert(src.FieldCount=FieldCount, erOperation);
  GrowDown := src.GrowDown;
  InitZero := src.InitZero;
  clear;
  for i := low(FFields) to high(FFields) do
    Fields[i].AssignShape(src.Fields[i]);
  if src.Count>0 then
  begin
    src.FFields[0].STCopy(FFields[0], src.First, src.Last);
    for j := 1 to High(FFields) do
      src.FFields[j].STSet(FFields[j], src.First, src.Last, First);
  end;
  Sorted := src.Sorted;
end;

destructor TCCustom.Destroy;
begin
  Clear;
  FState := fsDestroying;
  // we must keep this container alive till finish deleting of fields
  FreeFields;
  reallocmem(FDelFields, 0);
  FDelCount := 0;
  SetLength(FSortFields, 0);
  FSortFieldsCount := 0;
  ItemSize := 0;
  FCopyCount := 0;
  reallocmem(FCopyFields, 0);
  inherited;
end;

procedure TCCustom.FreeFields;
var i: integer;
begin
  {
    Container MUST be empty when we call .free for fields. Because for some
    data types it is impossible to remove items correctly without corresponding
    field (TT* class). For example TTString is responsible for reallocation of
    memory allocated for strings. So if we destroy TTString then no one can
    free this memory.
  }
  if count>0 then
    clear;

  for i := high(FFields) downto low(FFields) do
    if FFields[i].FState<>fsDestroying then
      FFields[i].Destroy;
  SetLength(FFields, 0);
end;

{function TCCustom.AddItem(var item): TCHandle;
begin
  Result := Add;
  Put(Result, item);
end;}

function TCCustom.InsertItem(handle: TCHandle; var item): TCHandle;
begin
  Result := Insert(handle);
  Put(Result, item);
end;

function TCCustom.InsertAfterItem(handle: TCHandle; var item): TCHandle;
begin
  Result := InsertAfter(handle);
  Put(Result, item);
end;

procedure TCCustom.Reverse;
begin
  if FCount>0 then
    Reverse(First, Last);
end;

procedure TCCustom.Reverse(AFirst, ALast: TCHandle);
var f,l: TCHandle;
begin
  repeat
    if AFirst=ALast then
      exit;
    f := AFirst;
    next(f);
    l := ALast;
    prev(l);
    // exchange data (not positions!)
    Swap(AFirst, ALast);
    if f=ALast then
      break;
    AFirst := f;
    ALast := l;
  until false;
end;

procedure TCCustom.Rotate(shift: integer);
begin
  Rotate(First, Last, shift);
end;

procedure TCCustom.RandomShuffle;
begin
  RandomShuffle(First, Last);
end;

procedure TCCustom.FirstPermutation;
begin
  Sort;
end;

procedure TCCustom.LastPermutation;
begin
  Sort;
  Reverse;
end;

function  TCCustom.NextPermutation:boolean;
begin
  result := false;
  error(erOperation);
end;

function  TCCustom.PrevPermutation:boolean;
begin
  result := false;
  error(erOperation);
end;

function TCCustom.FindMin: TCHandle;
var
  i: TCHandle;
begin
  if Count = 0 then
  begin
    Result := -1;
    exit;
  end;
  Result := First;
  if FSorted then
    exit;
  i := Result;
  while not EOF(i) do
  begin
    if CompareHandle(i, Result) < 0 then
      Result := i;
    Next(i);
  end;
end;

function TCCustom.FindMax: TCHandle;
var
  i: TCHandle;
begin
  if FSorted then
  begin
    Result := Last;
    exit;
  end;
  Result := First;
  i := Result;
  while not EOF(i) do
  begin
    if CompareHandle(i, Result) > 0 then
      Result := i;
    Next(i);
  end;
end;

function TCCustom.Find(var Item): TCHandle;
begin
  Result := First;
  while not EOF(Result) do
    if FCompareValue(Item, items[Result]^) = 0 then
      break
    else
      Next(Result);
end;

function TCCustom.FindFirstEqual(var Item): TCHandle;
var
  i: TCHandle;
begin
  result := Find(item);
  if (result<>-1) and FSorted then
    repeat
      i := result;
      Prev(i);
      if i=-1 then
        break;
      if FCompareValue(item, items[i]^) <> 0 then
        break;
      result := i;
    until False;
end;

procedure TCCustom.FindNextEqual(var Item; var handle: TCHandle);
begin
  if handle=-1 then
    exit;
  Next(handle);
  if FSorted then
  begin
    if (handle<>-1) and (FCompareValue(item, items[handle]^)<>0) then
      handle := -1;
  end
  else
    while handle<>-1 do
      if FCompareValue(item, items[handle]^)=0 then
        break
      else
        Next(handle);
end;

procedure TCCustom.Error(const msg: string);
begin
  raise Exception.Create(msg);
end;

procedure TCCustom.DeleteNext(handle: TCHandle);
begin
  next(handle);
  if handle<>-1 then
    Delete(handle);
end;

function TCCustom.GetHandle(index: integer): TCHandle;
var i: integer;
begin
  assert((index >= 0) and (index < Count));
  result := First;
  for i := 0 to index-1 do
    Next(result);
end;

function TCCustom.GetIndex(handle: TCHandle): integer;
var p: TCHandle;
begin
  result := 0;
  p := First;
  while p<>-1 do
    if p=handle then
      exit
    else
    begin
      next(p);
      inc(result);
    end;
  result := -1;
end;

function TCCustom.EOF(n: TCHandle): boolean;
begin
  result := n=-1;
end;

procedure TCCustom.Swap(first1, last1, first2: TCHandle);
begin
  while (first1<>-1) do
    if first1<>last1 then
      Swap(MoveNext(first1), MoveNext(first2))
    else
    begin
      Swap(first1, first2);
      break;
    end;
end;

procedure TCCustom.Exchange(handle1, handle2: TCHandle);
begin
  Swap(handle1, handle2);
end;

procedure TCCustom.Sort;
begin
  if Count>0 then
    Sort(TCHandle(First), TCHandle(Last));
end;

procedure TCCustom.Sort(field: TTCustom; compare: TOnCompare);
begin
  Sort(field, compare, First, Last);
end;

procedure TCCustom.Sort(field: TTCustom; compare: TOnCompareProc);
begin
  Sort(field, compare, First, Last);
end;

procedure TCCustom.Sort(compare: TOnCompareValues);
begin
  Sort(compare, TCHandle(First), TCHandle(Last));
end;

function TCCustom.SortCompareByFields(c: TTCustom; h1,h2: TCHandle):integer;
var i: integer;
begin
  result := 0;
  for i := 0 to FSortFieldsCount-1 do
  begin
    result := FSortFields[i].Container.CompareHandle(h1,h2);
    if result<>0 then
      break;
  end;
end;

procedure TCCustom.Sort(const Fields: array of TTCustom);
begin
  Sort(Fields, TCHandle(First), TCHandle(Last));
end;

procedure TCCustom.Sort(const Fields: array of TTCustom; AFirst, ALast: TCHandle);
var i: integer;
begin
  if length(Fields)=0 then
    exit;
  FSortFieldsCount := length(Fields);
  setlength(FSortFields, FSortFieldsCount);
  for i := 0 to FSortFieldsCount-1 do
    FSortFields[i] := Fields[i];
  Sort(fields[0], SortCompareByFields, AFirst, ALast);
  setlength(FSortFields, 0);
  FSortFieldsCount := 0;
end;

procedure TCCustom.Sort(AFirst, ALast: TCHandle);
begin
  error(erOperation);
end;

procedure TCCustom.Sort(compare: TOnCompareValues; AFirst, ALast: TCHandle);
begin
  error(erOperation);
end;

procedure TCCustom.Sort(field: TTCustom; compare: TOnCompare; AFirst, ALast: TCHandle);
begin
  error(erOperation);
end;

procedure TCCustom.Sort(field: TTCustom; compare: TOnCompareProc; AFirst, ALast: TCHandle);
begin
  error(erOperation);
end;

function TCCustom.GetFieldCount: integer;
begin
  result := length(FFields);
end;

function TCCustom.AddField(field: TTCustom): pointer;
begin
  Result := field;
  if FLockAddField>0 then
    exit;
  inc(FLockAddField);
  try
    setlength(FFields, length(FFields) + 1);
    FFields[length(FFields) - 1] := field;
    if length(FFields) = 1 then
      field.Container := self;
  finally
    dec(FLockAddField);
    if (length(FFields)=1) then
      FField := FFields[0];
  end;
end;

// called from TTCustom.destroy
procedure TCCustom.DelField(field: TTCustom);
var i,j: integer;
begin
  i := IndexOfField(field);
  if i < 0 then
    exit;
  for j := i to high(FFields)-1 do
    FFields[j] := FFields[j+1];
  setlength(FFields, length(FFields) - 1);
end;

procedure TCCustom.AddOnDelete(AOnDelete: TOnItemDeleted);
begin
  inc(FDelCount);
  reallocmem(FDelFields, FDelCount*SizeOF(TOnItemDeleted));
  FDelFields[FDelCount-1] := AOnDelete;
end;

procedure TCCustom.DelOnDelete(AOnDelete: TOnItemDeleted);
var i,j: integer;
begin
  for i := 0 to FDelCount-1 do
    if CompareMem(@FDelFields[i], @AOnDelete, sizeof(TOnItemDeleted)) then
    begin
      for j := i to FDelCount-2 do
        FDelFields[j] := FDelFields[j+1];
      dec(FDelCount);
      reallocmem(FDelFields, FDelCount*SizeOF(TOnItemDeleted));
      break;
    end;
end;

procedure TCCustom.AddOnCopy(AOnCopy: TOnCopyItem);
begin
  inc(FCopyCount);
  reallocmem(FCopyFields, FCopyCount*SizeOF(TOnCopyItem));
  FCopyFields[FCopyCount-1] := AOnCopy;
end;

procedure TCCustom.DelOnCopy(AOnCopy: TOnCopyItem);
var i,j: integer;
begin
  for i := 0 to FCopyCount-1 do
    if CompareMem(@FCopyFields[i], @AOnCopy, sizeof(TOnCopyItem)) then
    begin
      for j := i to FCopyCount-2 do
        FCopyFields[j] := FCopyFields[j+1];
      dec(FCopyCount);
      reallocmem(FCopyFields, FCopyCount*SizeOF(TOnCopyItem));
      break;
    end;
end;

function TCCustom.GetFieldSeparator:string;
begin
  result := FFieldSeparator;
end;

procedure TCCustom.SetFieldSeparator(v: string);
begin
  FFieldSeparator := v;
end;

procedure TCCustom.SetFieldSize(s: integer);
begin
  FFieldSize := s;
  FMoveField := GetMoveProc(FFieldSize);
end;

function TCCustom.MoveNext(var h: TCHandle): TCHandle;
begin
  result := h;
  Next(h);
end;

function TCCustom.MovePrev(var h: TCHandle): TCHandle;
begin
  result := h;
  Prev(h);
end;

function TCCustom.GetRangeLength(start, finish: TCHandle):integer;
begin
  result := 0;
  if start=-1 then
    exit;
  inc(result);
  while start<>finish do
  begin
    inc(result);
    Next(start);
  end;
end;

function TCCustom.IndexOfField(field: TTCustom):integer;
var i: integer;
begin
  for i := low(FFields) to high(FFields) do
    if FFields[i]=field then
    begin
      result := i;
      exit;
    end;
  result := -1;
end;

function TCCustom.AddField(AClass: CTT): pointer;
begin
  result := AddField(AClass.Create(self));
end;

function TCCustom.GetField(index: integer): TTCustom;
begin
  assert( (index >= 0) and (index < length(FFields)), erIndex);
  Result := FFields[index];
end;

function TCCustom.GetFieldByName(const FieldName: string): TTCustom;
var i: integer;
begin
  for i := 0 to length(FFields)-1 do
    if SameText(FFields[i].Name, FieldName) then
    begin
      result := FFields[i];
      exit;
    end;
  result := nil;
end;

function TCCustom.GetItemSize: integer;
begin
  result := FItemSize;
end;

procedure TCCustom.SetItemSize(n: integer);
begin
  if FItemSize = n then
    exit;
  FItemSize := n;
  reallocmem(ftemp, n);
  reallocmem(FTempLoad, n);
  FSwapItem := GetSwapProc(FItemSize);
  FMoveItem := GetMoveProc(FItemSize);
end;

function TCCustom.GetCount: integer;
begin
  result := FCount;
end;

procedure TCCustom.SetCount(n: integer);
begin
  FCount:= n;
end;

function TCCustom.GetCapacity: integer;
begin
  result := FCount;
end;

procedure TCCustom.SetCapacity(n: integer);
begin
end;

function TCCustom.GetGrowDown: boolean;
begin
  result := FGrowDown;
end;

procedure TCCustom.SetGrowDown(n: boolean);
begin
  FGrowDown := n;
end;

function TCCustom.GetInitZero: boolean;
begin
  result := FInitZero;
end;

procedure TCCustom.SetInitZero(n: boolean);
begin
  FInitZero := n;
end;

procedure TCCustom.ItemDeleted(handle: TCHandle);
var i: integer;
begin
  for i := 0 to FDelCount-1 do
    FDelFields[i](handle);
end;

procedure TCCustom.CopyItem(src,dst: TCHandle);
var i: integer;
begin
  for i := 0 to FCopyCount-1 do
    FCopyFields[i](src, dst);
end;

procedure TCCustom.Remove(var Item);
var a: TCHandle;
begin
  a := Find(Item);
  if a<>-1 then
    Delete(a);
end;

procedure TCCustom.RemoveAll(var Item);
var a,b: TCHandle;
begin
  a := FindFirstEqual(item);
  while a<>-1 do
  begin
    b := a;
    FindNextEqual(item, a);
    Delete(b);
  end;
end;

function TCCustom.CompareHandle(handle1, handle2: TCHandle): integer;
begin
  Result := FCompareValue(Get(handle1)^, Get(handle2)^);
end;

function TCCustom.CompareIndex(index1, index2: integer): integer;
begin
  Result := CompareHandle(Handles[index1], Handles[index2]);
end;

procedure TCCustom.ExchangeIndex(index1, index2: integer);
begin
  Swap(Handles[index1], Handles[index2]);
end;

function TCCustom.GetSorted: boolean;
begin
  result := FSorted;
end;

procedure TCCustom.SetSorted(n: boolean);
begin
  if FSorted = n then
    exit;
  FSorted := n;
  if FSorted then
    Sort;
end;

// TCCustom - hash
function TCCustom.GetOnCalcHash: TOnCalcHash;
begin
  Result := FOnCalcHash;
end;

procedure TCCustom.SetOnCalcHash(event: TOnCalcHash);
begin
  FOnCalcHash := event;
end;

// ------------------------- TBaseContainer ------------------------------------------

function TCHeap.AddItem(var item): TCHandle;
var j: integer;
begin
  Result := FCount;
  if Result = FCapacity then
    Grow;
  Inc(FCount);

  Result := FCount;
  while Result > 1 do
  begin
    j := Result shr 1;
    if FCompareValue(FBuf[(j-1)*FItemSize], Item) <= 0 then
      break;
    FSwapItem(FBuf[(j-1)*FItemSize], FBuf[(Result-1)*FItemSize], ftemp^, FItemSize);
    Result := j;
  end;
  dec(Result);

  FMoveField(item, buf[Result*FItemSize], FFieldSize);
end;

procedure TCHeap.Put(handle: TCHandle; var Item);
begin
  error(erOperation);
end;

function TCHeap.FindMin: TCHandle;
begin
  if FCount > 0 then
    Result := 0
  else
    Result := -1;
end;

procedure TCHeap.heapRemoveMin;
var
  n, c: integer;
begin
  c := Count;
  n := 1;
  // поднмаем лучшего из сыновей в позицию N и переходим к его позиции
  while n * 2 < c do
  begin
    n := n * 2;
    if CompareHandle(handles[n], handles[n - 1]) < 0 then
      Inc(n);
    inherited Swap(handles[n - 1], handles[(n div 2) - 1]);// changed from "copy" to "Swap"
  end;
  if n * 2 <= c then
  begin
    n := n * 2;
    inherited Swap(handles[n - 1], handles[(n div 2) - 1]);
  end;

  // —ейчас у нас N - позици€ "дырки", поскольку элемент подн€лс€ вверх, а нам нужно что бы "дырка" была
  // в правом нижнем углу дерева - тогда удалив ее мы оставим дерево "плотным".
  // ѕока предок "дырки" хуже последнего, мы помещаем его (предка) в "дырку", т.о. перемеща€ ее (как место дл€ последнего) вверх.
  while n > 1 do
    if CompareHandle(handles[(n shr 1) - 1], handles[c - 1]) <= 0 then
      break
    else
    begin
      inherited Swap(handles[(n shr 1) - 1], handles[n - 1]);
      n := n div 2;
    end;
  inherited Swap(handles[c - 1], handles[n - 1]);
  inherited Delete(Last);
end;

procedure TCHeap.SetCount(n: integer);
begin
  // we can not increase the heap in such way
  assert(n <= Count, erOperation);
  inherited SetCount(n);
end;

procedure TCHeap.SetSorted(v: boolean);
begin
  if v then
    Error(erOperation);
end;

procedure TCHeap.Sort;
begin
  Error(erOperation);
end;

procedure TCHeap.Copy(Src, Dst: TCHandle);
begin
  Error(erOperation);
end;

procedure TCHeap.Delete(handle: TCHandle);
begin
  assert(handle = First, erOperation);
  heapRemoveMin;
end;

procedure TCHeap.Swap(handle1, handle2: TCHandle);
begin
  Error(erOperation);
end;

function TCHeap.Insert(handle: TCHandle): TCHandle;
begin
  Result := -1;
  Error(erOperation);
end;

function TCHeap.InsertItem(handle: TCHandle; var item): TCHandle;
begin
  Result := -1;
  Error(erOperation);
end;

function TCHeap.InsertAfterItem(handle: TCHandle; var item): TCHandle;
begin
  Result := -1;
  Error(erOperation);
end;

procedure TCHeap.Move(Curhandle, Newhandle: TCHandle);
begin
  Error(erOperation);
end;

procedure TCHeap.Remove(var Item);
begin
  Error(erOperation);
end;

// ---------------------------------------------------------------------------------------------

constructor TTCustom.Create(AContainer: TCCustom = nil);
begin
  inherited Create;
  if AContainer = nil then
    Container := TCVector.Create
  else
    Container := AContainer;
end;

function TTCustom.GetAsAnsiStr(handle: TCHandle): AnsiString;
var
  v: variant;
  i,l,h: integer;
begin
  v := GetAsVariant(handle);
  if not VarIsArray(v) then
    result := AnsiString(v)
  else
  begin
    result := '';
    l := VarArrayLowBound(v,1);
    h := VarArrayHighBound(v,1);
    if h<l then
      exit;
    result := AnsiString(IntToStr(v[l]));
    for i := l+1 to h do
      result := result + ' ' + AnsiString(IntToStr(v[i]));
  end;
end;

function GetTokenA(const S: AnsiString; var P: integer; Symbols : AnsiString): AnsiString;
var
  i,f,l: integer;
begin
  result := '';
  f := -1;
  for i := p to length(s) do
    if CharInAnsiString(s[i], Symbols) then
    begin
      f := i;
      break;
    end;
  if f<0 then
    exit;
  l := f;
  for i := f+1 to length(s) do
    if CharInAnsiString(s[i], Symbols) then
      l := i
    else
      break;
  p := l+1;
  result := copy(s,f,l-f+1);
end;

procedure STokOpenA(const S: AnsiString; Symbols: TAnsiSet; var H: TStrTokenA);
begin
  H.Symbols := SetToAnsiString(Symbols);
  H.Text := S;
  H.Pos := 1;
end;

procedure STokOpenA(const S: AnsiString; Symbols: AnsiString; var H: TStrTokenA);
begin
  H.Symbols := SortAnsiString(Symbols);
  H.Text := S;
  H.Pos := 1;
end;

procedure STokOpenA(var H: TStrTokenA);
begin
  H.Pos := 1;
end;

function STokReadA(var H: TStrTokenA):integer;
begin
  with H do
  begin
    Token := GetTokenA(Text, Pos, Symbols);
    if Token='' then
      result := -1
    else
      result := 0;
  end;
end;

function STokCountA(var H: TStrTokenA):integer;
var S: TStrTokenA;
begin
  S := H;
  STokOpenA(H);
  result := 0;
  while STokReadA(H)=0 do
    inc(result);
  H := S;
end;

function STokCountA(const S: AnsiString; Symbols: TAnsiSet):integer; overload;
var H: TStrTokenA;
begin
  STokOpenA(S, Symbols, H);
  result := 0;
  while STokReadA(H)=0 do
    inc(result);
end;

function STokCountA(const S: AnsiString; Symbols: AnsiString):integer; overload;
var H: TStrTokenA;
begin
  STokOpenA(S, Symbols, H);
  result := 0;
  while STokReadA(H)=0 do
    inc(result);
end;

procedure STokCloseA(var H: TStrTokenA);
begin
  H.Symbols := '';
  H.Text := '';
  H.Pos := 0;
  H.Token := '';
end;

function STokCountIntA(S: AnsiString): integer;
begin
  result := STokCountA(S, IntSymbolsA);
end;

function STokCountFloatA(S: AnsiString): integer;
begin
  result := STokCountA(S, FloatSymbolsA);
end;

procedure STokOpenIntA(const S: AnsiString; var H: TStrTokenA);
begin
  STokOpenA(S, IntSymbolsA, H);
end;

procedure STokOpenFloatA(const S: AnsiString; var H: TStrTokenA);
begin
  STokOpenA(S, FloatSymbolsA, H);
end;

function STokReadIntA(var H: TStrTokenA):integer;
begin
  STokReadA(H);
  result := StrToInt(String(H.Token));
end;

function STokReadFloatA(var H: TStrTokenA):double;
begin
  STokReadA(H);
  result := StrToFloat(String(H.Token));
end;

function GetTokenW(const S: WideString; var P: integer; Symbols : WideString): WideString;
var
  i,f,l: integer;
begin
  result := '';
  f := -1;
  for i := p to length(s) do
    if CharInWideString(s[i], Symbols) then
    begin
      f := i;
      break;
    end;
  if f<0 then
    exit;
  l := f;
  for i := f+1 to length(s) do
    if CharInWideString(s[i], Symbols) then
      l := i
    else
      break;
  p := l+1;
  result := copy(s,f,l-f+1);
end;

procedure STokOpenW(const S: WideString; Symbols: TAnsiSet; var H: TStrTokenW);
begin
  H.Symbols := WideString(SetToAnsiString(Symbols));
  H.Text := S;
  H.Pos := 1;
end;

procedure STokOpenW(const S: WideString; Symbols: WideString; var H: TStrTokenW);
begin
  H.Symbols := SortWideString(Symbols);
  H.Text := S;
  H.Pos := 1;
end;

procedure STokOpenW(var H: TStrTokenW); overload;
begin
  H.Pos := 1;
end;

function STokReadW(var H: TStrTokenW):integer;
begin
  with H do
  begin
    Token := GetTokenW(Text, Pos, Symbols);
    if Token='' then
      result := -1
    else
      result := 0;
  end;
end;

function STokCountW(var H: TStrTokenW):integer;
var S: TStrTokenW;
begin
  S := H;
  STokOpenW(H);
  result := 0;
  while STokReadW(H)=0 do
    inc(result);
  H := S;
end;

function STokCountW(const S: WideString; Symbols: TAnsiSet):integer; overload;
var H: TStrTokenW;
begin
  STokOpenW(S, Symbols, H);
  result := 0;
  while STokReadW(H)=0 do
    inc(result);
end;

function STokCountW(const S: WideString; Symbols: WideString):integer; overload;
var H: TStrTokenW;
begin
  STokOpenW(S, Symbols, H);
  result := 0;
  while STokReadW(H)=0 do
    inc(result);
end;

procedure STokCloseW(var H: TStrTokenW);
begin
  H.Symbols := '';
  H.Text := '';
  H.Pos := 0;
  H.Token := '';
end;

function STokCountIntW(S: WideString): integer;
begin
  result := STokCountW(S, IntSymbolsW);
end;

function STokCountFloatW(S: WideString): integer;
begin
  result := STokCountW(S, FloatSymbolsW);
end;

procedure STokOpenIntW(const S: WideString; var H: TStrTokenW);
begin
  STokOpenW(S, IntSymbolsW, H);
end;

procedure STokOpenFloatW(const S: WideString; var H: TStrTokenW);
begin
  STokOpenW(S, FloatSymbolsW, H);
end;

function STokReadIntW(var H: TStrTokenW):integer;
begin
  STokReadW(H);
  result := StrToInt(H.Token);
end;

function STokReadFloatW(var H: TStrTokenW):double;
begin
  STokReadW(H);
  result := StrToFloat(H.Token);
end;

function GetToken(const S: String; var P: integer; Symbols : String): String;
var
  i,f,l: integer;
begin
  result := '';
  f := -1;
  for i := p to length(s) do
    if CharInString(s[i], Symbols) then
    begin
      f := i;
      break;
    end;
  if f<0 then
    exit;
  l := f;
  for i := f+1 to length(s) do
    if CharInString(s[i], Symbols) then
      l := i
    else
      break;
  p := l+1;
  result := copy(s,f,l-f+1);
end;

procedure STokOpen(const S: String; Symbols: TAnsiSet; var H: TStrToken);
begin
  H.Symbols := String(SetToAnsiString(Symbols));
  H.Text := S;
  H.Pos := 1;
end;

procedure STokOpen(const S: String; Symbols: String; var H: TStrToken);
begin
  H.Symbols := SortString(Symbols);
  H.Text := S;
  H.Pos := 1;
end;

procedure STokOpen(var H: TStrToken); overload;
begin
  H.Pos := 1;
end;

function STokRead(var H: TStrToken):integer;
begin
  with H do
  begin
    Token := GetToken(Text, Pos, Symbols);
    if Token='' then
      result := -1
    else
      result := 0;
  end;
end;

function STokCount(var H: TStrToken):integer;
var S: TStrToken;
begin
  S := H;
  STokOpen(H);
  result := 0;
  while STokRead(H)=0 do
    inc(result);
  H := S;
end;

function STokCount(const S: String; Symbols: TAnsiSet):integer; overload;
var H: TStrToken;
begin
  STokOpen(S, Symbols, H);
  result := 0;
  while STokRead(H)=0 do
    inc(result);
end;

function STokCount(const S: String; Symbols: String):integer; overload;
var H: TStrToken;
begin
  STokOpen(S, Symbols, H);
  result := 0;
  while STokRead(H)=0 do
    inc(result);
end;

procedure STokClose(var H: TStrToken);
begin
  H.Symbols := '';
  H.Text := '';
  H.Pos := 0;
  H.Token := '';
end;

function STokCountInt(S: String): integer;
begin
  result := STokCount(S, IntSymbols);
end;

function STokCountFloat(S: String): integer;
begin
  result := STokCount(S, FloatSymbols);
end;

procedure STokOpenInt(const S: String; var H: TStrToken);
begin
  STokOpen(S, IntSymbols, H);
end;

procedure STokOpenFloat(const S: String; var H: TStrToken);
begin
  STokOpen(S, FloatSymbols, H);
end;

function STokReadInt(var H: TStrToken):integer;
begin
  STokRead(H);
  result := StrToInt(H.Token);
end;

function STokReadFloat(var H: TStrToken):double;
begin
  STokRead(H);
  result := StrToFloat(H.Token);
end;

procedure TTCustom.SetAsAnsiStr(handle: TCHandle; Value: AnsiString);
var
  i,n: integer;
  v: variant;
  p: TStrTokenA;
begin
  n := STokCountIntA(value);
  if n<=1 then
    SetAsVariant(handle, value)
  else
  begin
    v := VarArrayCreate([0,n-1], varInteger);
    STokOpenIntA(value, p);
    for i := 0 to n-1 do
      v[i] := STokReadIntA(p);
    SetAsVariant(handle, v);
  end;
end;

function TTCustom.GetAsWideStr(handle: TCHandle): WideString;
begin
  result := WideString(GetAsAnsiStr(handle));
end;

procedure TTCustom.SetAsWideStr(handle: TCHandle; Value: WideString);
begin
  SetAsAnsiStr(handle, AnsiString(value));
end;

function TTCustom.GetAsString(handle: TCHandle): String;
begin
  {$IFDEF HiCompiler}
  result := GetAsWideStr(handle);
  {$ELSE}
  result := GetAsAnsiStr(handle);
  {$ENDIF}
end;

procedure TTCustom.SetAsString(handle: TCHandle; Value: String);
begin
  {$IFDEF HiCompiler}
  SetAsWideStr(handle, Value);
  {$ELSE}
  SetAsAnsiStr(handle, Value);
  {$ENDIF}
end;

function TTCustom.STCopy(Dst: TTCustom; h: TCHandle):TCHandle;
begin
  result := AddTo(h, dst);
end;

procedure TTCustom.STCopy(Dst: TTCustom; AFirst, ALast: TCHandle);
begin
  AddTo(AFirst, dst);
  if AFirst<>ALast then
    repeat
      FContainer.Next(AFirst);
      AddTo(AFirst, dst);
    until AFirst=ALast;
end;

procedure TTCustom.STSet(Dst: TTCustom; h1,h2: TCHandle);
begin
  dst.AsVariant[h2] := AsVariant[h1];
end;

procedure TTCustom.STSet(Dst: TTCustom; start1,finish1, start2: TCHandle);
begin
  dst.AsVariant[start2] := AsVariant[start1];
  if start1<>finish1 then
    repeat
      FContainer.Next(start1);
      dst.FContainer.Next(start2);
      dst.AsVariant[start2] := AsVariant[start1];
    until start1=finish1;
end;

function TTCustom.QuoteStrA(const s: ansistring): ansistring;
var i: integer;
begin
  if not FIsTextField or (Container.FieldCount <=1) then
  begin
    result := s;
    exit;
  end;
  result := '"';
  for i:=1 to length(s) do
    if (s[i]='"') then
      result := result + '&q'
    else
    if (s[i]=#13) then
      result := result + '&r'
    else
    if (s[i]=#10) then
      result := result + '&n'
    else
    if (s[i]='&') then
      result := result + '&a'
    else
    result := result + s[i];
  result := result + '"';
end;

function TTCustom.QuoteStrW(const s: widestring): widestring;
var i: integer;
begin
  if not FIsTextField or (Container.FieldCount <=1) then
  begin
    result := s;
    exit;
  end;
  result := '"';
  for i:=1 to length(s) do
    if (s[i]='"') then
      result := result + '&q'
    else
    if (s[i]=#13) then
      result := result + '&r'
    else
    if (s[i]=#10) then
      result := result + '&n'
    else
    if (s[i]='&') then
      result := result + '&a'
    else
    result := result + s[i];
  result := result + '"';
end;

procedure TTCustom.SaveItem(h: TCHandle; dst: TStream; FileFormat : TFileFormat = ffDefault);
var
  s: ansistring;
  w: widestring;
  u: UTF8String;
begin
  case FileFormat of
    ffAnsi:
      begin
        s := QuoteStrA(AsAnsiStr[h]);
        if s<>'' then
          dst.Write(s[1], length(s)*sizeof(ansichar));
      end;
    ffUnicode, ffUTF8:
      begin
        w := QuoteStrW(AsWideStr[h]);
        if w<>'' then
          if FileFormat=ffUnicode then
          begin
            dst.Write(w[1], length(w)*sizeof(widechar));
          end
          else
          begin
            u := Utf8Encode(w);
            dst.Write(u[1], length(u));
          end;
      end;
    ffBinary: dst.Write(FContainer.get(h)^, FFieldSize);
    else error(erOperation);
  end;
end;

function TTCustom.LoadItem(c: TCCustom; h: TCHandle; src: TStream; FileFormat : TFileFormat = ffDefault):TCHandle;
begin
  if h=-1 then
    if FileFormat<>ffBinary then
      result := AddList([c.LoadToken])
    else
    begin
      src.Read(FContainer.FTempLoad^, FContainer.FFieldSize);
      result := FContainer.AddItem(FContainer.FTempLoad^);
    end
  else
  begin
    result := h;
    if FileFormat<>ffBinary then
      AsWideStr[h] := c.LoadToken
    else
    begin
      src.Read(FContainer.FTempLoad^, FFieldSize);
      FContainer.Put(h, FContainer.FTempLoad^);
    end;
  end;
end;

function TTCustom.Add(const Items: array of const): TCHandle;
begin
  result := AddList(items);
end;

function TTCustom.AddRecord(const Items: array of const): TCHandle;
var
  i: integer;
  m: TCCustom;
begin
  m := FContainer.GetMainContainer;
  assert(length(items)=m.FieldCount);
  result := m.FField.AddList([VarAsVariant(@Items[0])]);
  if result<>-1 then
    for i := 1 to m.FieldCount-1 do
      m.FFields[i].AsVariant[result] := VarAsVariant(@Items[i]);
end;

constructor TTCustom.Create(AClass: CContainer);
begin
  Create(AClass.Create);
end;

constructor TTCustom.Create(AClass: CContainer; AFields: array of CTT);
begin
  Create(AClass.Create);
  AddFields(AFields);
end;

destructor TTCustom.Destroy;
begin
  FState := fsDestroying;
  if assigned(FContainer) and (FContainer.FState<>fsDestroying) then
    FContainer.Destroy;
  FContainer := nil;
  inherited;
end;

function TTCustom.GetRootCount: integer;
begin
  result := FContainer.GetRootCount;
end;

function TTCustom.GetFirstRoot: TCHandle;
begin
  result := FContainer.GetFirstRoot;
end;

function TTCustom.GetLastRoot: TCHandle;
begin
  result := FContainer.GetLastRoot;
end;

function TTCustom.GetParent(node: TCHandle): TCHandle;
begin
  result := FContainer.GetParent(node);
end;

function TTCustom.GetChildCount(node: TCHandle): integer;
begin
  result := FContainer.GetChildCount(node);
end;

function TTCustom.GetFirstChild(node: TCHandle): TCHandle;
begin
  result := FContainer.GetFirstChild(node);
end;

function TTCustom.GetLastChild(node: TCHandle): TCHandle;
begin
  result := FContainer.GetLastChild(node);
end;

function TTCustom.GetPrevSibling(node: TCHandle): TCHandle;
begin
  result := FContainer.GetPrevSibling(node);
end;

function TTCustom.GetNextSibling(node: TCHandle): TCHandle;
begin
  result := FContainer.GetNextSibling(node);
end;

// creates empty container of specified type with copy of all fields (not data)
function TTCustom.CloneShell(ContainerType: CContainer):pointer;
var c: TCCustom;
begin
  c := FContainer.CloneShell(ContainerType);
  result := c.Fields[FContainer.IndexOfField(self)];
end;

// creates EMPTY copy of container including all fields (not data)
function TTCustom.CloneShell:pointer;
var c: TCCustom;
begin
  c := FContainer.CloneShell;
  result := c.Fields[FContainer.IndexOfField(self)];
end;

// creates new container with same structure and copy of data
function TTCustom.Clone(ContainerType: CContainer):pointer;
var c: TCCustom;
begin
  c := FContainer.Clone(ContainerType);
  result := c.Fields[FContainer.IndexOfField(self)];
end;

// creates copy of container and get pointer to field
function TTCustom.Clone:pointer;
var c: TCCustom;
begin
  c := FContainer.Clone;
  result := c.Fields[FContainer.IndexOfField(self)];
end;

procedure TTCustom.AssignShape(src: TTCustom);
begin
end;

procedure TTCustom.Assign(src: TTCustom);
begin
  AssignShape(src);
  FContainer.Assign(src.Container);
end;

procedure TTCustom.InitContainer(c: TCCustom);
begin
  if c=nil then exit;
  c.AddField(self);
  c.ItemSize := FieldSize;
  c.FieldSize := FieldSize;
  if FNeedOnDelete>0 then
    c.AddOnDelete(ItemDeleted);
end;

function TTCustom.GetText: string;
var f: TCHandle;
begin
  result := '';
  f := FContainer.First;
  while f<>-1 do
  begin
    if result<>'' then
      result := result + FContainer.EOL;
    result := result + AsString[f];
    FContainer.Next(f);
  end;
end;

procedure TTCustom.SetText(value: string);
var
  s: string;
  i,l: integer;
begin
  clear;
  i := 1;
  s := '';
  l := length(FContainer.EOL);
  while i<=length(value) do
    if system.copy(value,i,l)<>FContainer.EOL then
    begin
      s := s + value[i];
      inc(i);
    end
    else
    begin
      add([s]);
      s := '';
      inc(i, l);
    end;
  if s<>'' then
    add([s]);
end;

function TTCustom.GetTextLine: string;
var f: TCHandle;
begin
  result := '';
  f := FContainer.First;
  while f<>-1 do
  begin
    if result<>'' then
      result := result + FContainer.FieldSeparator;
    result := result + AsString[f];
    FContainer.Next(f);
  end;
end;

procedure TTCustom.SetTextLine(value: string);
var
  s: string;
  i,l: integer;
begin
  clear;
  i := 1;
  s := '';
  l := length(FContainer.FieldSeparator);
  while i<=length(value) do
    if system.copy(value,i,l)<>FContainer.FieldSeparator then
    begin
      s := s + value[i];
      inc(i);
    end
    else
    begin
      add([s]);
      s := '';
      inc(i, l);
    end;
  if s<>'' then
    add([s]);
end;

function TTCustom.GetWideText: WideString;
var f: TCHandle;
begin
  result := '';
  f := FContainer.First;
  while f<>-1 do
  begin
    if result<>'' then
      result := result + FContainer.EOL;
    result := result + AsWideStr[f];
    FContainer.Next(f);
  end;
end;

procedure TTCustom.SetWideText(value: WideString);
var
  s: WideString;
  i,l: integer;
begin
  clear;
  i := 1;
  s := '';
  l := length(FContainer.EOL);
  while i<=length(value) do
    if system.copy(value,i,l)<>FContainer.EOL then
    begin
      s := s + value[i];
      inc(i);
    end
    else
    begin
      add([s]);
      s := '';
      inc(i, l);
    end;
  if s<>'' then
    add([s]);
end;

function TTCustom.GetEOL:string;
begin
  result := FContainer.EOL;
end;

procedure TTCustom.SetEOL(const NewEOL: string);
begin
  FContainer.EOL := NewEOL;
end;

function TTCustom.GetPointer(h: TCHandle):pointer;
begin
  result := FContainer.Get(h);
end;

function TTCustom.GetField(index: integer):TTCustom;
begin
  result := FContainer.Fields[index];
end;

function TTCustom.GetItemSize:integer;
begin
  result := FContainer.ItemSize;
end;

procedure TTCustom.setContainer(s: TCCustom);
begin
  if FSetContainerLock>0 then
    exit;
  inc(FSetContainerLock);
  try
    assert(FContainer=nil);
    FContainer := s;
    InitContainer(FContainer);
  finally
    dec(FSetContainerLock);
  end;
end;

function TTCustom.GetHandle(index: integer): TCHandle;
begin
  Result := FContainer.Handles[index];
end;

function TTCustom.GetIndex(handle: TCHandle): integer;
begin
  Result := FContainer.Indexes[handle];
end;

function TTCustom.Empty:boolean;
begin
  result := FContainer.Count <= 0;
end;

procedure TTCustom.SaveToFile(filename: string; FileFormat : TFileFormat = ffUTF8);
begin
  Container.SaveToFile(filename, FileFormat);
end;

procedure TTCustom.SaveToStream(dst: TStream; FileFormat : TFileFormat = ffUTF8);
begin
  Container.SaveToStream(dst, FileFormat);
end;

procedure TTCustom.SaveToStream(dst: CStream; FileFormat : TFileFormat = ffUTF8);
begin
  Container.SaveToStream(dst, FileFormat);
end;

procedure TTCustom.SaveToStringList(dst: TStrings);
begin
  Container.SaveToStringList(dst);
end;

procedure TTCustom.Print(Format : TFileFormat = ffDefault; OneLine: boolean = false);
var s: string;
begin
  if OneLine then
  begin
    s := Container.EOL;
    Container.EOL := ' ';
  end;
  SaveToStream(TConsoleOutputStream, Format);
  if OneLine then
  begin
    Container.EOL := s;
  end;
end;

procedure TTCustom.Println(Format : TFileFormat = ffDefault; OneLine: boolean = false);
begin
  Print(format, OneLine);
  TConsoleOutputStream.writeln;
end;

procedure TTCustom.LoadFromFile(filename: string; FileFormat : TFileFormat = ffUTF8);
begin
  Container.LoadFromFile(filename, FileFormat);
end;

procedure TTCustom.LoadFromStream(src: TStream; FileFormat : TFileFormat = ffUTF8);
begin
  Container.LoadFromStream(src, FileFormat);
end;

procedure TTCustom.LoadFromStringList(src: TStrings);
begin
  Container.LoadFromStringList(src);
end;

function TTCustom.GetCapacity: integer;
begin
  Result := FContainer.Capacity;
end;

function TTCustom.GetCount: integer;
begin
  Result := FContainer.Count;
end;

function TTCustom.GetGrowDown: boolean;
begin
  Result := FContainer.GrowDown;
end;

function TTCustom.GetInitZero: boolean;
begin
  Result := FContainer.InitZero;
end;

procedure TTCustom.Error(const msg: string);
begin
  raise Exception.Create(msg);
end;

function TTCustom.GetRangeLength(start, finish: TCHandle):integer;
begin
  result := FContainer.RangeLength[start, finish];
end;

procedure TTCustom.ItemDeleted(handle: TCHandle);
begin
  if assigned(FOnDeleted) then
    FOnDeleted(handle);
end;

procedure TTCustom.SetCapacity(n: integer);
begin
  FContainer.Capacity := n;
end;

procedure TTCustom.SetCount(n: integer);
begin
  FContainer.Count := n;
end;

procedure TTCustom.SetGrowDown(n: boolean);
begin
  FContainer.GrowDown := n;
end;

procedure TTCustom.SetInitZero(n: boolean);
begin
  FContainer.InitZero := n;
end;

function TTCustom.GetSorted: boolean;
begin
  Result := FContainer.Sorted;
end;

procedure TTCustom.SetSorted(n: boolean);
begin
  FContainer.Sorted := n;
end;

procedure TTCustom.setOnDeleted(n: TOnDeleteStorageItem);
begin
  if assigned(FOnDeleted) then
    NeedOnDelete := NeedOnDelete-1;
  FOnDeleted := n;
  if assigned(FOnDeleted) then
    NeedOnDelete := NeedOnDelete+1;
end;

procedure TTCustom.SetNeedOnDelete(n: integer);
begin
  if (n=0) and (FNeedOnDelete>0) and assigned(FContainer) then
    Container.DelOnDelete(ItemDeleted)
  else
    if (n>0) and (FNeedOnDelete=0) and assigned(FContainer) then
      Container.AddOnDelete(ItemDeleted);
  FNeedOnDelete := n;
end;

procedure TTCustom.SetFieldSize(ASize: integer);
begin
  FFieldSize := ASize;
end;

procedure TTCustom.ImportTTCustom(c: TTCustom);
var
  n: TCHandle;
begin
  if c.count=0 then
    exit;
  if self.ClassType=c.ClassType then
    c.STCopy(self, c.First, c.Last)
  else
  begin
    n := c.First;
    while n<>-1 do
    begin
      c.AddTo(n, self);
      c.Next(n);
    end
  end;
end;

procedure TTCustom.ImportTCCustom(c: TCCustom);
var
  i,j: integer;
  n,k: TCHandle;
  src, dst: TCCustom;
  SameType: array of boolean;
begin
  if c.count=0 then
    exit;
  src := c.GetMainContainer;
  dst := FContainer.GetMainContainer;
  if (dst.FieldCount=1) and (src.FieldCount=1) then
  begin
    ImportTTCustom(Src.FField);
    exit;
  end;

  j := min(src.FieldCount, dst.FieldCount)-1;
  setlength(SameType, j+1);
  for i := 0 to j do
    SameType[i] := src.Fields[i].ClassType=dst.Fields[i].ClassType;
  n := src.First;
  while not src.EOF(n) do
  begin

    // add item (key value)
    if SameType[0] then
      k := src.FField.STCopy(dst.FField, n)
    else
      k := dst.FField.AddList([ src.FField.AsVariant[n] ]);

    // set all fields
    for i := 1 to j do
      if SameType[i] then
        src.Fields[i].STSet(dst.Fields[i], n, k);

    src.Next(n);
  end;
end;

function TTCustom.ImportObject(obj: TObject; defvalue: TCHandle): TCHandle;
var
  i: integer;
  s: TStringList;
  l: TList;
begin
  Result := defvalue;
  if obj = nil then
    exit;

  if obj is TTCustom then
  begin
    ImportTTCustom(TTCustom(obj));
    exit;
  end;

  if obj is TCCustom then
  begin
    ImportTCCustom(TCCustom(obj));
    exit;
  end;

  if obj is TStringList then
  begin
    s := TStringList(obj);
    for i := 0 to s.Count-1 do
      result := Add([s[i]]);
    exit;
  end;

  if obj is TList then
  begin
    l := TList(obj);
    for i := 0 to l.Count-1 do
      result := ImportObject(l[i], defvalue);
    exit;
  end;
end;

procedure TTCustom.Delete(handle: TCHandle);
begin
  FContainer.Delete(handle);
end;

procedure TTCustom.DeleteNext(handle: TCHandle);
begin
  FContainer.DeleteNext(handle);
end;

procedure TTCustom.Clear;
begin
  FContainer.Clear;
end;

procedure TTCustom.Copy(Srchandle, Dsthandle: TCHandle);
begin
  FContainer.Copy(srchandle, dsthandle);
end;

procedure TTCustom.ChangeParent(NodeHandle, ANewParent: TCHandle);
begin
  FContainer.ChangeParent(NodeHandle, ANewParent);
end;

procedure TTCustom.Swap(handle1, handle2: TCHandle);
begin
  FContainer.Swap(handle1, handle2);
end;

procedure TTCustom.Move(Curhandle, Newhandle: TCHandle);
begin
  FContainer.Move(curhandle, newhandle);
end;

procedure TTCustom.Sort;
begin
  if FContainer.GetMainContainer.FField=self then
    FContainer.Sort
  else
    FContainer.Sort([self]);
end;

procedure TTCustom.Sort(compare: TOnCompare);
begin
  // FPC
  FContainer.Sort(self, compare, FContainer.First, FContainer.Last);
end;

procedure TTCustom.Sort(compare: TOnCompareProc);
begin
  // FPC
  FContainer.Sort(self, compare, FContainer.First, FContainer.Last);
end;

procedure TTCustom.Sort(compare: TOnCompare; AFirst, ALast: TCHandle);
begin
  FContainer.Sort(self, compare, AFirst, ALast);
end;

procedure TTCustom.Sort(compare: TOnCompareProc; AFirst, ALast: TCHandle);
begin
  FContainer.Sort(self, compare, AFirst, ALast);
end;

procedure TTCustom.Sort(AFirst, ALast: TCHandle);
begin
  if FContainer.GetMainContainer.FField=self then
    FContainer.Sort(AFirst, ALast)
  else
    FContainer.Sort([self], AFirst, ALast);
end;

procedure TTCustom.Sort(const Fields: array of TTCustom);
begin
  FContainer.Sort(Fields);
end;

procedure TTCustom.Sort(const Fields: array of TTCustom; AFirst, ALast: TCHandle);
begin
  FContainer.Sort(Fields, AFirst, ALast);
end;

function TTCustom.First: TCHandle;
begin
  Result := FContainer.First;
end;

function TTCustom.Last: TCHandle;
begin
  Result := FContainer.Last;
end;

procedure TTCustom.Next(var n: TCHandle);
begin
  FContainer.Next(n);
end;

procedure TTCustom.Prev(var n: TCHandle);
begin
  FContainer.Prev(n);
end;

function TTCustom.GetNext(n: TCHandle): TCHandle;
begin
  FContainer.Next(n);
  result := n;
end;

function TTCustom.GetPrev(n: TCHandle): TCHandle;
begin
  FContainer.Prev(n);
  result := n;
end;

function TTCustom.MoveNext(var n: TCHandle): TCHandle; // return N, after that increment N
begin
  result := n;
  FContainer.Next(n);
end;

function TTCustom.MovePrev(var n: TCHandle): TCHandle; // return N, after that N := Prev(N)
begin
  result := n;
  FContainer.Prev(n);
end;

function TTCustom.FindMin: TCHandle;
begin
  Result := FContainer.FindMin;
end;

function TTCustom.FindMax: TCHandle;
begin
  Result := FContainer.FindMax;
end;

function TTCustom.EOF(handle: TCHandle): boolean;
begin
  Result := handle = -1;
end;

function TTCustom.AddField(field: TTCustom): pointer;
begin
  Result := Container.AddField(field);
end;

function TTCustom.AddFields(AFields: array of CTT): pointer;
var i: integer;
begin
  for i := low(AFields) to high(AFields) do
    AddField(AFields[i]);
  result := container.Fields[0];
end;

function TTCustom.AddField(AClass: CTT): pointer;
var
  j: T_CJoin;
begin
  j := T_CJoin.CreateJoined(Container, nil);
  result := AClass.Create(j);
  j.FField := result;
//  result := AddField(f);
end;

// ------------------------- TTInteger ------------------------------------------

{$IFDEF FPC}
procedure SortMemoryRangeInteger(Buf: pointer; L, R: Integer);
begin
  if R>L then
    VectorIntegerQuickSort(@PIntegerList(Buf)[L], R-L+1);
end;

constructor TTInteger.Create(Container: TCCustom = nil);
begin
  FieldSize := sizeof(TIntegerType);
  FSortMemoryRange := @SortMemoryRangeInteger;
  inherited;
end;

function TTInteger.Add(Item: TIntegerType): TCHandle;
begin
  Result := FContainer.AddItem(item);
end;

function  TTInteger.Front: TIntegerType;          // examine first element
begin
  result := items[FContainer.First];
end;

function  TTInteger.Back: TIntegerType;           // examine last element
begin
  result := items[FContainer.Last];
end;

function TTInteger.Push(v: TIntegerType):TCHandle;        // insert element at front
begin
  result := FContainer.InsertItem(FContainer.First, v);
end;

function  TTInteger.Pop: TIntegerType;            // remove first element
var
  h: TCHandle;
begin
  h := FContainer.First;
  Result := items[h];
  FContainer.Delete(h);
end;

function TTInteger.PushBack(v: TIntegerType):TCHandle;    // insert element at back
begin
  result := FContainer.AddItem(v);
end;

function  TTInteger.PopBack: TIntegerType;        // remove last element
var
  h: TCHandle;
begin
  h := FContainer.Last;
  result := items[h];
  FContainer.Delete(h);
end;

function TTInteger.Enqueue(v: TIntegerType):TCHandle;    // add to tail
begin
  result := FContainer.AddItem(v);
end;

function  TTInteger.Dequeue: TIntegerType;       // read&remove from head
var
  h: TCHandle;
begin
  h := FContainer.First;
  Result := items[h];
  FContainer.Delete(h);
end;

procedure TTInteger.Sort(compare: TOnIntegerCompare);
begin
  Sort(compare, FContainer.First, FContainer.Last);
end;

procedure TTInteger.Sort(compare: TOnIntegerCompareProc);
begin
  Sort(compare, FContainer.First, FContainer.Last);
end;

procedure TTInteger.Sort(compare: TOnIntegerCompare; AFirst, ALast: TCHandle);
begin
  FOnCompare := compare;
  FContainer.Sort(CmpExt, AFirst, ALast);
end;

procedure TTInteger.Sort(compare: TOnIntegerCompareProc; AFirst, ALast: TCHandle);
begin
  FOnCompareProc := compare;
  FContainer.Sort(CmpExtProc, AFirst, ALast);
end;

function TTInteger.AddTo(handle: TCHandle; dst: TTCustom): TCHandle;
begin
  Result := dst.Add([items[handle]]);
end;

function TTInteger.GetAsVariant(handle: TCHandle): variant;
begin
  if handle=-1 then
    Result := null
  else
    Result := items[handle];
end;

procedure TTInteger.SetAsVariant(handle: TCHandle; Value: variant);
begin
  items[handle] := Value;
end;

function TTInteger.GetAsAnsiStr(handle: TCHandle): AnsiString;
begin
  result := IntToStr(int64(items[handle]));
end;

procedure TTInteger.SetAsAnsiStr(handle: TCHandle; Value: AnsiString);
begin
  items[handle] := StrToInt64(value);
end;

function TTInteger.GetAsWideStr(handle: TCHandle): WideString;
begin
  result := IntToStr(int64(items[handle]));
end;

procedure TTInteger.SetAsWideStr(handle: TCHandle; Value: WideString);
begin
  items[handle] := StrToInt64(value);
end;

procedure TTInteger.InitContainer(c: TCCustom);
begin
  inherited;
  c.FCompareValue := CmpInt;
end;

procedure TTInteger.SetOnCompare(c: TOnIntegerCompare);
begin
  if assigned(c) then
    container.FCompareValue := CmpExt
  else
    container.FCompareValue := CmpInt;
  FOnCompareProc := nil;
  FOnCompare := c;
end;

procedure TTInteger.SetOnCompareProc(c: TOnIntegerCompareProc);
begin
  if assigned(c) then
    container.FCompareValue := CmpExtProc
  else
    container.FCompareValue := CmpInt;
  FOnCompare := nil;
  FOnCompareProc := c;
end;

function TTInteger.CmpInt(var a,b):integer;
begin
  if TIntegerType(a)<TIntegerType(b) then
    result := -1
  else
    if TIntegerType(a)=TIntegerType(b) then
      result := 0
    else
      result := 1;
end;

function TTInteger.CmpExt(var a, b): integer;
begin
  Result := FOnCompare(TIntegerType(a), TIntegerType(b));
end;

function TTInteger.CmpExtProc(var a, b): integer;
begin
  Result := FOnCompareProc(TIntegerType(a), TIntegerType(b));
end;

function TTInteger.STCopy(Dst: TTCustom; h: TCHandle):TCHandle;
begin
  result := TTInteger(dst).Add(Items[h]);
end;

procedure TTInteger.STCopy(Dst: TTCustom; AFirst, ALast: TCHandle);
begin
  TTInteger(dst).Add(Items[AFirst]);
  if AFirst<>ALast then
    repeat
      FContainer.Next(AFirst);
      TTInteger(dst).Add(Items[AFirst]);
    until AFirst=ALast;
end;

procedure TTInteger.STSet(Dst: TTCustom; h1,h2: TCHandle);
begin
  TTInteger(dst)[h2] := Items[h1];
end;

procedure TTInteger.STSet(Dst: TTCustom; start1,finish1, start2: TCHandle);
begin
  TTInteger(dst)[start2] := Items[start1];
  if start1<>finish1 then
    repeat
      FContainer.Next(start1);
      dst.FContainer.Next(start2);
      TTInteger(dst)[start2] := Items[start1];
    until start1=finish1;
end;

function TTInteger.GetMap(key: TIntegerType): variant;
begin
  Result := FContainer.Fields[1].AsVariant[Find(key)];
end;

procedure TTInteger.PutMap(key: TIntegerType; Value: variant);
var
  h: TCHandle;
begin
  h := Find(key);
  if h = -1 then
    AddPair(key, Value)
  else
    FContainer.Fields[1].AsVariant[h] := Value;
end;

function TTInteger.GetHandleOf(Value: variant): TCHandle;
begin
  Result := Find(Value);
end;

function TTInteger.AddList(const Items: array of const): TCHandle;
var
  i: integer;
  p: PVarRec;
begin
  Result := -1;
  for i := low(items) to high(items) do
  begin
    p := @items[i];
    if p.VType = vtObject then
      Result := ImportObject(p.VObject, Result)
    else
      Result := add(VarAsInt64(p));
  end;
end;

function TTInteger.Find(Item: TIntegerType): TCHandle;
begin
  Result := FContainer.Find(item);
end;

function TTInteger.FindFirstEqual(Item: TIntegerType): TCHandle;
begin
  Result := FContainer.FindFirstEqual(item);
end;

procedure TTInteger.FindNextEqual(Item: TIntegerType; var handle: TCHandle);
begin
  FContainer.FindNextEqual(item, handle);
end;

function TTInteger.GetItem(n: TCHandle): TIntegerType;
begin
  Result := TIntegerType(FContainer.Get(n)^);
end;

function TTInteger.GetMinValue: TIntegerType;
begin
  Result := TIntegerType(FContainer.Get(FContainer.FindMin)^);
end;

function TTInteger.RemoveMinValue: TIntegerType;
var
  n: TCHandle;
begin
  n := FContainer.FindMin;
  Result := TIntegerType(FContainer.Get(n)^);
  FContainer.Delete(n);
end;

function TTInteger.GetMaxValue: TIntegerType;
begin
  Result := TIntegerType(FContainer.Get(FContainer.FindMax)^);
end;

function TTInteger.RemoveMaxValue: TIntegerType;
var
  n: TCHandle;
begin
  n := FContainer.FindMax;
  Result := TIntegerType(FContainer.Get(n)^);
  FContainer.Delete(n);
end;

function TTInteger.AddPair(key: TIntegerType; Value: variant): TCHandle;
begin
  result := Add(key);
  Container.Fields[1].AsVariant[result] := Value;
end;

function TTInteger.Insert(handle: TCHandle; Item: TIntegerType): TCHandle;
begin
  Result := FContainer.InsertItem(handle, item);
end;

function TTInteger.InsertAfter(handle: TCHandle; Item: TIntegerType): TCHandle;
begin
  Result := FContainer.InsertAfterItem(handle, item);
end;

procedure TTInteger.Remove(Item: TIntegerType);
begin
  FContainer.Remove(item);
end;

procedure TTInteger.RemoveAll(Item: TIntegerType);
begin
  FContainer.RemoveAll(item);
end;

procedure TTInteger.SetItem(n: TCHandle; Value: TIntegerType);
begin
  FContainer.Put(n, Value);
end;

{$ENDIF}

{ TTDouble }

procedure SortMemoryRangeDouble(Buf: pointer; L, R: Integer);
begin
  if R>L then
    VectorDoubleQuickSort(@PDoubleList(Buf)[L], R-L+1);
end;

constructor TTDouble.Create(Container: TCCustom = nil);
begin
  FieldSize := sizeof(TDoubleType);
  FSortMemoryRange := @SortMemoryRangeDouble;
  inherited;
end;

function TTDouble.Add(Item: TDoubleType): TCHandle;
begin
  Result := FContainer.AddItem(item);
end;

function  TTDouble.Front: TDoubleType;          // examine first element
begin
  result := items[FContainer.First];
end;

function  TTDouble.Back: TDoubleType;           // examine last element
begin
  result := items[FContainer.Last];
end;

function TTDouble.Push(v: TDoubleType):TCHandle;        // insert element at front
begin
  result := FContainer.InsertItem(FContainer.First, v);
end;

function  TTDouble.Pop: TDoubleType;            // remove first element
var
  h: TCHandle;
begin
  h := FContainer.First;
  Result := items[h];
  FContainer.Delete(h);
end;

function TTDouble.PushBack(v: TDoubleType):TCHandle;    // insert element at back
begin
  result := FContainer.AddItem(v);
end;

function  TTDouble.PopBack: TDoubleType;        // remove last element
var
  h: TCHandle;
begin
  h := FContainer.Last;
  result := items[h];
  FContainer.Delete(h);
end;

function TTDouble.Enqueue(v: TDoubleType):TCHandle;    // add to tail
begin
  result := FContainer.AddItem(v);
end;

function  TTDouble.Dequeue: TDoubleType;       // read&remove from head
var
  h: TCHandle;
begin
  h := FContainer.First;
  Result := items[h];
  FContainer.Delete(h);
end;

function TTDouble.AddTo(handle: TCHandle; dst: TTCustom): TCHandle;
begin
  Result := dst.Add([items[handle]]);
end;

function TTDouble.GetAsVariant(handle: TCHandle): variant;
begin
  if handle=-1 then
    Result := null
  else
    Result := items[handle];
end;

procedure TTDouble.SetAsVariant(handle: TCHandle; Value: variant);
begin
  items[handle] := Value;
end;

function TTDouble.GetAsAnsiStr(handle: TCHandle): AnsiString;
begin
  result := AnsiString(FloatToStr(items[handle]));
end;

procedure TTDouble.SetAsAnsiStr(handle: TCHandle; Value: AnsiString);
begin
  items[handle] := StrToFloat(String(value));
end;

function TTDouble.GetAsWideStr(handle: TCHandle): WideString;
begin
  result := FloatToStr(items[handle]);
end;

procedure TTDouble.SetAsWideStr(handle: TCHandle; Value: WideString);
begin
  items[handle] := StrToFloat(value);
end;

function TTDouble.GetHandleOf(Value: variant): TCHandle;
begin
  Result := Find(Value);
end;

procedure TTDouble.InitContainer(c: TCCustom);
begin
  inherited;
  c.FCompareValue := CmpInt;
end;

function TTDouble.CmpInt(var a,b):integer;
begin
  if TDoubleType(a)<TDoubleType(b) then
    result := -1
  else
    if TDoubleType(a)=TDoubleType(b) then
      result := 0
    else
      result := 1;
end;

procedure TTDouble.SetOnCompare(c: TOnDoubleCompare);
begin
  if assigned(c) then
    container.FCompareValue := CmpExt
  else
    container.FCompareValue := CmpInt;
  FOnCompare := c;
  FOnCompareProc := nil;
end;

procedure TTDouble.SetOnCompareProc(c: TOnDoubleCompareProc);
begin
  if assigned(c) then
    container.FCompareValue := CmpExtProc
  else
    container.FCompareValue := CmpInt;
  FOnCompareProc := c;
  FOnCompare := nil;
end;

function TTDouble.CmpExt(var a, b): integer;
begin
  Result := FOnCompare(TDoubleType(a), TDoubleType(b));
end;

function TTDouble.CmpExtProc(var a, b): integer;
begin
  Result := FOnCompareProc(TDoubleType(a), TDoubleType(b));
end;

procedure TTDouble.Sort(compare: TOnDoubleCompare);
begin
  Sort(compare, FContainer.First, FContainer.Last);
end;

procedure TTDouble.Sort(compare: TOnDoubleCompareProc);
begin
  Sort(compare, FContainer.First, FContainer.Last);
end;

procedure TTDouble.Sort(compare: TOnDoubleCompare; AFirst, ALast: TCHandle);
begin
  FOnCompare := compare;
  FContainer.Sort(CmpExt, AFirst, ALast);
end;

procedure TTDouble.Sort(compare: TOnDoubleCompareProc; AFirst, ALast: TCHandle);
begin
  FOnCompareProc := compare;
  FContainer.Sort(CmpExtProc, AFirst, ALast);
end;

function TTDouble.STCopy(Dst: TTCustom; h: TCHandle):TCHandle;
begin
  result := TTDouble(dst).Add(Items[h]);
end;

procedure TTDouble.STCopy(Dst: TTCustom; AFirst, ALast: TCHandle);
begin
  TTDouble(dst).Add(Items[AFirst]);
  if AFirst<>ALast then
    repeat
      FContainer.Next(AFirst);
      TTDouble(dst).Add(Items[AFirst]);
    until AFirst=ALast;
end;

procedure TTDouble.STSet(Dst: TTCustom; h1,h2: TCHandle);
begin
  TTDouble(dst)[h2] := Items[h1];
end;

procedure TTDouble.STSet(Dst: TTCustom; start1,finish1, start2: TCHandle);
begin
  TTDouble(dst)[start2] := Items[start1];
  if start1<>finish1 then
    repeat
      FContainer.Next(start1);
      dst.FContainer.Next(start2);
      TTDouble(dst)[start2] := Items[start1];
    until start1=finish1;
end;

function TTDouble.GetMap(key: TDoubleType): variant;
begin
  Result := FContainer.Fields[1].AsVariant[Find(key)];
end;

procedure TTDouble.PutMap(key: TDoubleType; Value: variant);
var
  h: TCHandle;
begin
  h := Find(key);
  if h = -1 then
    AddPair(key, Value)
  else
    FContainer.Fields[1].AsVariant[h] := Value;
end;

function TTDouble.AddList(const Items: array of const): TCHandle;
var
  i: integer;
  p: PVarRec;
begin
  Result := -1;
  for i := low(items) to high(items) do
  begin
    p := @items[i];
    if p.VType = vtObject then
      Result := ImportObject(p.VObject, Result)
    else
      Result := add(VarAsExtended(p));
  end;
end;

function TTDouble.Find(Item: TDoubleType): TCHandle;
begin
  Result := FContainer.Find(item);
end;

function TTDouble.FindFirstEqual(Item: TDoubleType): TCHandle;
begin
  Result := FContainer.FindFirstEqual(item);
end;

procedure TTDouble.FindNextEqual(Item: TDoubleType; var handle: TCHandle);
begin
  FContainer.FindNextEqual(item, handle);
end;

function TTDouble.GetItem(n: TCHandle): TDoubleType;
begin
  Result := TDoubleType(FContainer.Get(n)^);
end;

function TTDouble.GetMaxValue: TDoubleType;
begin
  Result := TDoubleType(FContainer.Get(FContainer.FindMax)^);
end;

function TTDouble.RemoveMaxValue: TDoubleType;
var
  n: TCHandle;
begin
  n := FContainer.FindMax;
  Result := TDoubleType(FContainer.Get(n)^);
  FContainer.Delete(n);
end;

function TTDouble.AddPair(key: TDoubleType; Value: variant): TCHandle;
begin
  result := Add(key);
  Container.Fields[1].AsVariant[result] := Value;
end;

function TTDouble.GetMinValue: TDoubleType;
begin
  Result := TDoubleType(FContainer.Get(FContainer.FindMin)^);
end;

function TTDouble.RemoveMinValue: TDoubleType;
var
  n: TCHandle;
begin
  n := FContainer.FindMin;
  Result := TDoubleType(FContainer.Get(n)^);
  FContainer.Delete(n);
end;

function TTDouble.Insert(handle: TCHandle; Item: TDoubleType): TCHandle;
begin
  Result := FContainer.InsertItem(handle, item);
end;

function TTDouble.InsertAfter(handle: TCHandle; Item: TDoubleType): TCHandle;
begin
  Result := FContainer.InsertAfterItem(handle, item);
end;

procedure TTDouble.Remove(Item: TDoubleType);
begin
  FContainer.Remove(item);
end;

procedure TTDouble.RemoveAll(Item: TDoubleType);
begin
  FContainer.RemoveAll(item);
end;

procedure TTDouble.SetItem(n: TCHandle; Value: TDoubleType);
begin
  FContainer.Put(n, Value);
end;

{ TTSingle }

procedure SortMemoryRangeSingle(Buf: pointer; L, R: Integer);
begin
  if R>L then
    VectorSingleQuickSort(@PSingleList(Buf)[L], R-L+1);
end;

constructor TTSingle.Create(Container: TCCustom = nil);
begin
  FieldSize := sizeof(TSingleType);
  FSortMemoryRange := @SortMemoryRangeSingle;
  inherited;
end;

function TTSingle.Add(Item: TSingleType): TCHandle;
begin
  Result := FContainer.AddItem(item);
end;

function  TTSingle.Front: TSingleType;          // examine first element
begin
  result := items[FContainer.First];
end;

function  TTSingle.Back: TSingleType;           // examine last element
begin
  result := items[FContainer.Last];
end;

function TTSingle.Push(v: TSingleType):TCHandle;        // insert element at front
begin
  result := FContainer.InsertItem(FContainer.First, v);
end;

function  TTSingle.Pop: TSingleType;            // remove first element
var
  h: TCHandle;
begin
  h := FContainer.First;
  Result := items[h];
  FContainer.Delete(h);
end;

function TTSingle.PushBack(v: TSingleType):TCHandle;    // insert element at back
begin
  result := FContainer.AddItem(v);
end;

function  TTSingle.PopBack: TSingleType;        // remove last element
var
  h: TCHandle;
begin
  h := FContainer.Last;
  result := items[h];
  FContainer.Delete(h);
end;

function TTSingle.Enqueue(v: TSingleType):TCHandle;    // add to tail
begin
  result := FContainer.AddItem(v);
end;

function  TTSingle.Dequeue: TSingleType;       // read&remove from head
var
  h: TCHandle;
begin
  h := FContainer.First;
  Result := items[h];
  FContainer.Delete(h);
end;

function TTSingle.AddTo(handle: TCHandle; dst: TTCustom): TCHandle;
begin
  Result := dst.Add([items[handle]]);
end;

function TTSingle.GetAsVariant(handle: TCHandle): variant;
begin
  if handle=-1 then
    Result := null
  else
    Result := items[handle];
end;

procedure TTSingle.SetAsVariant(handle: TCHandle; Value: variant);
begin
  items[handle] := Value;
end;

function TTSingle.GetAsAnsiStr(handle: TCHandle): AnsiString;
begin
  result := AnsiString(FloatToStr(items[handle]));
end;

procedure TTSingle.SetAsAnsiStr(handle: TCHandle; Value: AnsiString);
begin
  items[handle] := StrToFloat(String(value));
end;

function TTSingle.GetAsWideStr(handle: TCHandle): WideString;
begin
  result := FloatToStr(items[handle]);
end;

procedure TTSingle.SetAsWideStr(handle: TCHandle; Value: WideString);
begin
  items[handle] := StrToFloat(value);
end;

function TTSingle.GetHandleOf(Value: variant): TCHandle;
begin
  Result := Find(Value);
end;

procedure TTSingle.InitContainer(c: TCCustom);
begin
  inherited;
  c.FCompareValue := CmpInt;
end;

function TTSingle.CmpInt(var a,b):integer;
begin
  if TSingleType(a)<TSingleType(b) then
    result := -1
  else
    if TSingleType(a)=TSingleType(b) then
      result := 0
    else
      result := 1;
end;

procedure TTSingle.SetOnCompare(c: TOnSingleCompare);
begin
  if assigned(c) then
    container.FCompareValue := CmpExt
  else
    container.FCompareValue := CmpInt;
  FOnCompare := c;
  FOnCompareProc := nil;
end;

procedure TTSingle.SetOnCompareProc(c: TOnSingleCompareProc);
begin
  if assigned(c) then
    container.FCompareValue := CmpExtProc
  else
    container.FCompareValue := CmpInt;
  FOnCompareProc := c;
  FOnCompare := nil;
end;

function TTSingle.CmpExt(var a, b): integer;
begin
  Result := FOnCompare(TSingleType(a), TSingleType(b));
end;

function TTSingle.CmpExtProc(var a, b): integer;
begin
  Result := FOnCompareProc(TSingleType(a), TSingleType(b));
end;

procedure TTSingle.Sort(compare: TOnSingleCompare);
begin
  Sort(compare, FContainer.First, FContainer.Last);
end;

procedure TTSingle.Sort(compare: TOnSingleCompareProc);
begin
  Sort(compare, FContainer.First, FContainer.Last);
end;

procedure TTSingle.Sort(compare: TOnSingleCompare; AFirst, ALast: TCHandle);
begin
  FOnCompare := compare;
  FContainer.Sort(CmpExt, AFirst, ALast);
end;

procedure TTSingle.Sort(compare: TOnSingleCompareProc; AFirst, ALast: TCHandle);
begin
  FOnCompareProc := compare;
  FContainer.Sort(CmpExtProc, AFirst, ALast);
end;

function TTSingle.STCopy(Dst: TTCustom; h: TCHandle):TCHandle;
begin
  result := TTSingle(dst).Add(Items[h]);
end;

procedure TTSingle.STCopy(Dst: TTCustom; AFirst, ALast: TCHandle);
begin
  TTSingle(dst).Add(Items[AFirst]);
  if AFirst<>ALast then
    repeat
      FContainer.Next(AFirst);
      TTSingle(dst).Add(Items[AFirst]);
    until AFirst=ALast;
end;

procedure TTSingle.STSet(Dst: TTCustom; h1,h2: TCHandle);
begin
  TTSingle(dst)[h2] := Items[h1];
end;

procedure TTSingle.STSet(Dst: TTCustom; start1,finish1, start2: TCHandle);
begin
  TTSingle(dst)[start2] := Items[start1];
  if start1<>finish1 then
    repeat
      FContainer.Next(start1);
      dst.FContainer.Next(start2);
      TTSingle(dst)[start2] := Items[start1];
    until start1=finish1;
end;

function TTSingle.GetMap(key: TSingleType): variant;
begin
  Result := FContainer.Fields[1].AsVariant[Find(key)];
end;

procedure TTSingle.PutMap(key: TSingleType; Value: variant);
var
  h: TCHandle;
begin
  h := Find(key);
  if h = -1 then
    AddPair(key, Value)
  else
    FContainer.Fields[1].AsVariant[h] := Value;
end;

function TTSingle.AddList(const Items: array of const): TCHandle;
var
  i: integer;
  p: PVarRec;
begin
  Result := -1;
  for i := low(items) to high(items) do
  begin
    p := @items[i];
    if p.VType = vtObject then
      Result := ImportObject(p.VObject, Result)
    else
      Result := add(VarAsExtended(p));
  end;
end;

function TTSingle.Find(Item: TSingleType): TCHandle;
begin
  Result := FContainer.Find(item);
end;

function TTSingle.FindFirstEqual(Item: TSingleType): TCHandle;
begin
  Result := FContainer.FindFirstEqual(item);
end;

procedure TTSingle.FindNextEqual(Item: TSingleType; var handle: TCHandle);
begin
  FContainer.FindNextEqual(item, handle);
end;

function TTSingle.GetItem(n: TCHandle): TSingleType;
begin
  Result := TSingleType(FContainer.Get(n)^);
end;

function TTSingle.GetMaxValue: TSingleType;
begin
  Result := TSingleType(FContainer.Get(FContainer.FindMax)^);
end;

function TTSingle.RemoveMaxValue: TSingleType;
var
  n: TCHandle;
begin
  n := FContainer.FindMax;
  Result := TSingleType(FContainer.Get(n)^);
  FContainer.Delete(n);
end;

function TTSingle.AddPair(key: TSingleType; Value: variant): TCHandle;
begin
  result := Add(key);
  Container.Fields[1].AsVariant[result] := Value;
end;

function TTSingle.GetMinValue: TSingleType;
begin
  Result := TSingleType(FContainer.Get(FContainer.FindMin)^);
end;

function TTSingle.RemoveMinValue: TSingleType;
var
  n: TCHandle;
begin
  n := FContainer.FindMin;
  Result := TSingleType(FContainer.Get(n)^);
  FContainer.Delete(n);
end;

function TTSingle.Insert(handle: TCHandle; Item: TSingleType): TCHandle;
begin
  Result := FContainer.InsertItem(handle, item);
end;

function TTSingle.InsertAfter(handle: TCHandle; Item: TSingleType): TCHandle;
begin
  Result := FContainer.InsertAfterItem(handle, item);
end;

procedure TTSingle.Remove(Item: TSingleType);
begin
  FContainer.Remove(item);
end;

procedure TTSingle.RemoveAll(Item: TSingleType);
begin
  FContainer.RemoveAll(item);
end;

procedure TTSingle.SetItem(n: TCHandle; Value: TSingleType);
begin
  FContainer.Put(n, Value);
end;

{ TTExtended }

procedure SortMemoryRangeExtended(Buf: pointer; L, R: Integer);
begin
  if R>L then
    VectorExtendedQuickSort(@PExtendedList(Buf)[L], R-L+1);
end;

constructor TTExtended.Create(Container: TCCustom = nil);
begin
  FieldSize := sizeof(TExtendedType);
  FSortMemoryRange := @SortMemoryRangeExtended;
  inherited;
end;

function TTExtended.Add(Item: TExtendedType): TCHandle;
begin
  Result := FContainer.AddItem(item);
end;

function  TTExtended.Front: TExtendedType;          // examine first element
begin
  result := items[FContainer.First];
end;

function  TTExtended.Back: TExtendedType;           // examine last element
begin
  result := items[FContainer.Last];
end;

function TTExtended.Push(v: TExtendedType):TCHandle;        // insert element at front
begin
  result := FContainer.InsertItem(FContainer.First, v);
end;

function  TTExtended.Pop: TExtendedType;            // remove first element
var
  h: TCHandle;
begin
  h := FContainer.First;
  Result := items[h];
  FContainer.Delete(h);
end;

function TTExtended.PushBack(v: TExtendedType):TCHandle;    // insert element at back
begin
  result := FContainer.AddItem(v);
end;

function  TTExtended.PopBack: TExtendedType;        // remove last element
var
  h: TCHandle;
begin
  h := FContainer.Last;
  result := items[h];
  FContainer.Delete(h);
end;

function TTExtended.Enqueue(v: TExtendedType):TCHandle;    // add to tail
begin
  result := FContainer.AddItem(v);
end;

function  TTExtended.Dequeue: TExtendedType;       // read&remove from head
var
  h: TCHandle;
begin
  h := FContainer.First;
  Result := items[h];
  FContainer.Delete(h);
end;

function TTExtended.AddTo(handle: TCHandle; dst: TTCustom): TCHandle;
begin
  Result := dst.Add([items[handle]]);
end;

function TTExtended.GetAsVariant(handle: TCHandle): variant;
begin
  if handle=-1 then
    Result := null
  else
    Result := items[handle];
end;

procedure TTExtended.SetAsVariant(handle: TCHandle; Value: variant);
begin
  items[handle] := Value;
end;

function TTExtended.GetAsAnsiStr(handle: TCHandle): AnsiString;
begin
  result := AnsiString(FloatToStr(items[handle]));
end;

procedure TTExtended.SetAsAnsiStr(handle: TCHandle; Value: AnsiString);
begin
  items[handle] := StrToFloat(String(value));
end;

function TTExtended.GetAsWideStr(handle: TCHandle): WideString;
begin
  result := FloatToStr(items[handle]);
end;

procedure TTExtended.SetAsWideStr(handle: TCHandle; Value: WideString);
begin
  items[handle] := StrToFloat(value);
end;

function TTExtended.GetHandleOf(Value: variant): TCHandle;
begin
  Result := Find(Value);
end;

procedure TTExtended.InitContainer(c: TCCustom);
begin
  inherited;
  c.FCompareValue := CmpInt;
end;

function TTExtended.CmpInt(var a,b):integer;
begin
  if TExtendedType(a)<TExtendedType(b) then
    result := -1
  else
    if TExtendedType(a)=TExtendedType(b) then
      result := 0
    else
      result := 1;
end;

procedure TTExtended.SetOnCompare(c: TOnExtendedCompare);
begin
  if assigned(c) then
    container.FCompareValue := CmpExt
  else
    container.FCompareValue := CmpInt;
  FOnCompare := c;
  FOnCompareProc := nil;
end;

procedure TTExtended.SetOnCompareProc(c: TOnExtendedCompareProc);
begin
  if assigned(c) then
    container.FCompareValue := CmpExtProc
  else
    container.FCompareValue := CmpInt;
  FOnCompareProc := c;
  FOnCompare := nil;
end;

function TTExtended.CmpExt(var a, b): integer;
begin
  Result := FOnCompare(TExtendedType(a), TExtendedType(b));
end;

function TTExtended.CmpExtProc(var a, b): integer;
begin
  Result := FOnCompareProc(TExtendedType(a), TExtendedType(b));
end;

procedure TTExtended.Sort(compare: TOnExtendedCompare);
begin
  Sort(compare, FContainer.First, FContainer.Last);
end;

procedure TTExtended.Sort(compare: TOnExtendedCompareProc);
begin
  Sort(compare, FContainer.First, FContainer.Last);
end;

procedure TTExtended.Sort(compare: TOnExtendedCompare; AFirst, ALast: TCHandle);
begin
  FOnCompare := compare;
  FContainer.Sort(CmpExt, AFirst, ALast);
end;

procedure TTExtended.Sort(compare: TOnExtendedCompareProc; AFirst, ALast: TCHandle);
begin
  FOnCompareProc := compare;
  FContainer.Sort(CmpExtProc, AFirst, ALast);
end;

function TTExtended.STCopy(Dst: TTCustom; h: TCHandle):TCHandle;
begin
  result := TTExtended(dst).Add(Items[h]);
end;

procedure TTExtended.STCopy(Dst: TTCustom; AFirst, ALast: TCHandle);
begin
  TTExtended(dst).Add(Items[AFirst]);
  if AFirst<>ALast then
    repeat
      FContainer.Next(AFirst);
      TTExtended(dst).Add(Items[AFirst]);
    until AFirst=ALast;
end;

procedure TTExtended.STSet(Dst: TTCustom; h1,h2: TCHandle);
begin
  TTExtended(dst)[h2] := Items[h1];
end;

procedure TTExtended.STSet(Dst: TTCustom; start1,finish1, start2: TCHandle);
begin
  TTExtended(dst)[start2] := Items[start1];
  if start1<>finish1 then
    repeat
      FContainer.Next(start1);
      dst.FContainer.Next(start2);
      TTExtended(dst)[start2] := Items[start1];
    until start1=finish1;
end;

function TTExtended.GetMap(key: TExtendedType): variant;
begin
  Result := FContainer.Fields[1].AsVariant[Find(key)];
end;

procedure TTExtended.PutMap(key: TExtendedType; Value: variant);
var
  h: TCHandle;
begin
  h := Find(key);
  if h = -1 then
    AddPair(key, Value)
  else
    FContainer.Fields[1].AsVariant[h] := Value;
end;

function TTExtended.AddList(const Items: array of const): TCHandle;
var
  i: integer;
  p: PVarRec;
begin
  Result := -1;
  for i := low(items) to high(items) do
  begin
    p := @items[i];
    if p.VType = vtObject then
      Result := ImportObject(p.VObject, Result)
    else
      Result := add(VarAsExtended(p));
  end;
end;

function TTExtended.Find(Item: TExtendedType): TCHandle;
begin
  Result := FContainer.Find(item);
end;

function TTExtended.FindFirstEqual(Item: TExtendedType): TCHandle;
begin
  Result := FContainer.FindFirstEqual(item);
end;

procedure TTExtended.FindNextEqual(Item: TExtendedType; var handle: TCHandle);
begin
  FContainer.FindNextEqual(item, handle);
end;

function TTExtended.GetItem(n: TCHandle): TExtendedType;
begin
  Result := TExtendedType(FContainer.Get(n)^);
end;

function TTExtended.GetMaxValue: TExtendedType;
begin
  Result := TExtendedType(FContainer.Get(FContainer.FindMax)^);
end;

function TTExtended.RemoveMaxValue: TExtendedType;
var
  n: TCHandle;
begin
  n := FContainer.FindMax;
  Result := TExtendedType(FContainer.Get(n)^);
  FContainer.Delete(n);
end;

function TTExtended.AddPair(key: TExtendedType; Value: variant): TCHandle;
begin
  result := Add(key);
  Container.Fields[1].AsVariant[result] := Value;
end;

function TTExtended.GetMinValue: TExtendedType;
begin
  Result := TExtendedType(FContainer.Get(FContainer.FindMin)^);
end;

function TTExtended.RemoveMinValue: TExtendedType;
var
  n: TCHandle;
begin
  n := FContainer.FindMin;
  Result := TExtendedType(FContainer.Get(n)^);
  FContainer.Delete(n);
end;

function TTExtended.Insert(handle: TCHandle; Item: TExtendedType): TCHandle;
begin
  Result := FContainer.InsertItem(handle, item);
end;

function TTExtended.InsertAfter(handle: TCHandle; Item: TExtendedType): TCHandle;
begin
  Result := FContainer.InsertAfterItem(handle, item);
end;

procedure TTExtended.Remove(Item: TExtendedType);
begin
  FContainer.Remove(item);
end;

procedure TTExtended.RemoveAll(Item: TExtendedType);
begin
  FContainer.RemoveAll(item);
end;

procedure TTExtended.SetItem(n: TCHandle; Value: TExtendedType);
begin
  FContainer.Put(n, Value);
end;

{ --------------------------------------- TTLongword ---------------------------}

procedure SortMemoryRangeLongword(Buf: pointer; L, R: Integer);
begin
{$IFDEF FPC}
  if R>L then
    VectorLongwordQuickSort(@PLongwordList(Buf)[L], R-L+1);
{$ELSE}
  if R>L then
    VectorLongwordJumpSort(@PLongwordList(Buf)[L], R-L+1);
{$ENDIF}
end;

constructor TTLongword.Create(Container: TCCustom = nil);
begin
  FieldSize := sizeof(TLongwordType);
  FSortMemoryRange := @SortMemoryRangeLongword;
  inherited;
end;

function TTLongword.Add(Item: TLongwordType): TCHandle;
begin
  Result := FContainer.AddItem(item);
end;

function  TTLongword.Front: TLongwordType;          // examine first element
begin
  result := items[FContainer.First];
end;

function  TTLongword.Back: TLongwordType;           // examine last element
begin
  result := items[FContainer.Last];
end;

function TTLongword.Push(v: TLongwordType):TCHandle;        // insert element at front
begin
  result := FContainer.InsertItem(FContainer.First, v);
end;

function  TTLongword.Pop: TLongwordType;            // remove first element
var
  h: TCHandle;
begin
  h := FContainer.First;
  Result := items[h];
  FContainer.Delete(h);
end;

function TTLongword.PushBack(v: TLongwordType):TCHandle;    // insert element at back
begin
  result := FContainer.AddItem(v);
end;

function  TTLongword.PopBack: TLongwordType;        // remove last element
var
  h: TCHandle;
begin
  h := FContainer.Last;
  result := items[h];
  FContainer.Delete(h);
end;

function TTLongword.Enqueue(v: TLongwordType):TCHandle;    // add to tail
begin
  result := FContainer.AddItem(v);
end;

function  TTLongword.Dequeue: TLongwordType;       // read&remove from head
var
  h: TCHandle;
begin
  h := FContainer.First;
  Result := items[h];
  FContainer.Delete(h);
end;

function TTLongword.AddTo(handle: TCHandle; dst: TTCustom): TCHandle;
begin
  Result := dst.Add([items[handle]]);
end;

function TTLongword.GetAsVariant(handle: TCHandle): variant;
begin
  if handle=-1 then
    Result := null
  else
    Result := items[handle];
end;

procedure TTLongword.SetAsVariant(handle: TCHandle; Value: variant);
begin
  items[handle] := Value;
end;

function TTLongword.GetAsAnsiStr(handle: TCHandle): AnsiString;
begin
  result := AnsiString(IntToStr(int64(items[handle])));
end;

procedure TTLongword.SetAsAnsiStr(handle: TCHandle; Value: AnsiString);
begin
  items[handle] := StrToInt64(String(value));
end;

function TTLongword.GetAsWideStr(handle: TCHandle): WideString;
begin
  result := IntToStr(int64(items[handle]));
end;

procedure TTLongword.SetAsWideStr(handle: TCHandle; Value: WideString);
begin
  items[handle] := StrToInt64(value);
end;

function TTLongword.GetHandleOf(Value: variant): TCHandle;
begin
  Result := Find(Value);
end;

function TTLongword.AddList(const Items: array of const): TCHandle;
var
  i: integer;
  p: PVarRec;
begin
  Result := -1;
  for i := low(items) to high(items) do
  begin
    p := @items[i];
    if p.VType = vtObject then
      Result := ImportObject(p.VObject, Result)
    else
      Result := add(VarAsInt64(p));
  end;
end;

function TTLongword.Find(Item: TLongwordType): TCHandle;
begin
  Result := FContainer.Find(item);
end;

function TTLongword.FindFirstEqual(Item: TLongwordType): TCHandle;
begin
  Result := FContainer.FindFirstEqual(item);
end;

procedure TTLongword.FindNextEqual(Item: TLongwordType; var handle: TCHandle);
begin
  FContainer.FindNextEqual(item, handle);
end;

function TTLongword.GetItem(n: TCHandle): TLongwordType;
begin
  Result := TLongwordType(FContainer.Get(n)^);
end;

function TTLongword.GetMinValue: TLongwordType;
begin
  Result := TLongwordType(FContainer.Get(FContainer.FindMin)^);
end;

function TTLongword.RemoveMinValue: TLongwordType;
var
  n: TCHandle;
begin
  n := FContainer.FindMin;
  Result := TLongwordType(FContainer.Get(n)^);
  FContainer.Delete(n);
end;

function TTLongword.GetMaxValue: TLongwordType;
begin
  Result := TLongwordType(FContainer.Get(FContainer.FindMax)^);
end;

function TTLongword.RemoveMaxValue: TLongwordType;
var
  n: TCHandle;
begin
  n := FContainer.FindMax;
  Result := TLongwordType(FContainer.Get(n)^);
  FContainer.Delete(n);
end;

function TTLongword.Insert(handle: TCHandle; Item: TLongwordType): TCHandle;
begin
  Result := FContainer.InsertItem(handle, item);
end;

function TTLongword.InsertAfter(handle: TCHandle; Item: TLongwordType): TCHandle;
begin
  Result := FContainer.InsertAfterItem(handle, item);
end;

procedure TTLongword.Remove(Item: TLongwordType);
begin
  FContainer.Remove(item);
end;

procedure TTLongword.RemoveAll(Item: TLongwordType);
begin
  FContainer.RemoveAll(item);
end;

procedure TTLongword.SetItem(n: TCHandle; Value: TLongwordType);
begin
  FContainer.Put(n, Value);
end;

{ --------------------------------------- TTByte ---------------------------}

procedure SortMemoryRangeByte(Buf: pointer; L, R: Integer);
begin
{$IFDEF FPC}
  if R>L then
    VectorByteQuickSort(@PByteList(Buf)[L], R-L+1);
{$ELSE}
  if R>L then
    VectorByteCountSort(@PByteList(Buf)[L], R-L+1);
{$ENDIF}
end;

constructor TTByte.Create(Container: TCCustom = nil);
begin
  FieldSize := sizeof(TByteType);
  FSortMemoryRange := @SortMemoryRangeByte;
  inherited;
end;

function TTByte.Add(Item: TByteType): TCHandle;
begin
  Result := FContainer.AddItem(item);
end;

function  TTByte.Front: TByteType;          // examine first element
begin
  result := items[FContainer.First];
end;

function  TTByte.Back: TByteType;           // examine last element
begin
  result := items[FContainer.Last];
end;

function TTByte.Push(v: TByteType):TCHandle;        // insert element at front
begin
  result := FContainer.InsertItem(FContainer.First, v);
end;

function  TTByte.Pop: TByteType;            // remove first element
var
  h: TCHandle;
begin
  h := FContainer.First;
  Result := items[h];
  FContainer.Delete(h);
end;

function TTByte.PushBack(v: TByteType):TCHandle;    // insert element at back
begin
  result := FContainer.AddItem(v);
end;

function  TTByte.PopBack: TByteType;        // remove last element
var
  h: TCHandle;
begin
  h := FContainer.Last;
  result := items[h];
  FContainer.Delete(h);
end;

function TTByte.Enqueue(v: TByteType):TCHandle;    // add to tail
begin
  result := FContainer.AddItem(v);
end;

function  TTByte.Dequeue: TByteType;       // read&remove from head
var
  h: TCHandle;
begin
  h := FContainer.First;
  Result := items[h];
  FContainer.Delete(h);
end;

function TTByte.AddTo(handle: TCHandle; dst: TTCustom): TCHandle;
begin
  Result := dst.Add([items[handle]]);
end;

function TTByte.GetAsVariant(handle: TCHandle): variant;
begin
  if handle=-1 then
    Result := null
  else
    Result := items[handle];
end;

procedure TTByte.SetAsVariant(handle: TCHandle; Value: variant);
begin
  items[handle] := Value;
end;

function TTByte.GetAsAnsiStr(handle: TCHandle): AnsiString;
begin
  result := AnsiString(inttostr(items[handle]));
end;

procedure TTByte.SetAsAnsiStr(handle: TCHandle; Value: AnsiString);
begin
  items[handle] := strtoint(String(value));
end;

function TTByte.GetAsWideStr(handle: TCHandle): WideString;
begin
  result := inttostr(items[handle]);
end;

procedure TTByte.SetAsWideStr(handle: TCHandle; Value: WideString);
begin
  items[handle] := strtoint(value);
end;

function TTByte.GetHandleOf(Value: variant): TCHandle;
begin
  Result := Find(Value);
end;

function TTByte.AddList(const Items: array of const): TCHandle;
var
  i: integer;
  p: PVarRec;
begin
  Result := -1;
  for i := low(items) to high(items) do
  begin
    p := @items[i];
    if p.VType = vtObject then
      Result := ImportObject(p.VObject, Result)
    else
      Result := add(VarAsInteger(p));
  end;
end;

function TTByte.Find(Item: TByteType): TCHandle;
begin
  Result := FContainer.Find(item);
end;

function TTByte.FindFirstEqual(Item: TByteType): TCHandle;
begin
  Result := FContainer.FindFirstEqual(item);
end;

procedure TTByte.FindNextEqual(Item: TByteType; var handle: TCHandle);
begin
  FContainer.FindNextEqual(item, handle);
end;

function TTByte.GetItem(n: TCHandle): TByteType;
begin
  Result := TByteType(FContainer.Get(n)^);
end;

function TTByte.GetMinValue: TByteType;
begin
  Result := TByteType(FContainer.Get(FContainer.FindMin)^);
end;

function TTByte.RemoveMinValue: TByteType;
var
  n: TCHandle;
begin
  n := FContainer.FindMin;
  Result := TByteType(FContainer.Get(n)^);
  FContainer.Delete(n);
end;

function TTByte.GetMaxValue: TByteType;
begin
  Result := TByteType(FContainer.Get(FContainer.FindMax)^);
end;

function TTByte.RemoveMaxValue: TByteType;
var
  n: TCHandle;
begin
  n := FContainer.FindMax;
  Result := TByteType(FContainer.Get(n)^);
  FContainer.Delete(n);
end;

function TTByte.Insert(handle: TCHandle; Item: TByteType): TCHandle;
begin
  Result := FContainer.InsertItem(handle, item);
end;

function TTByte.InsertAfter(handle: TCHandle; Item: TByteType): TCHandle;
begin
  Result := FContainer.InsertAfterItem(handle, item);
end;

procedure TTByte.Remove(Item: TByteType);
begin
  FContainer.Remove(item);
end;

procedure TTByte.RemoveAll(Item: TByteType);
begin
  FContainer.RemoveAll(item);
end;

procedure TTByte.SetItem(n: TCHandle; Value: TByteType);
begin
  FContainer.Put(n, Value);
end;

// TTChar

procedure SortMemoryRangeChar(Buf: pointer; L, R: Integer);
begin
  if R>L then
    VectorCharQuickSort(@PCharList(Buf)[L], R-L+1);
end;

constructor TTChar.Create(Container: TCCustom = nil);
begin
  FIsTextField := true;
  FieldSize := sizeof(TCharType);
  FSortMemoryRange := @SortMemoryRangeChar;
  inherited;
end;

function TTChar.Add(Item: TCharType): TCHandle;
begin
  Result := FContainer.AddItem(item);
end;

function  TTChar.Front: TCharType;          // examine first element
begin
  result := items[FContainer.First];
end;

function  TTChar.Back: TCharType;           // examine last element
begin
  result := items[FContainer.Last];
end;

function TTChar.Push(v: TCharType):TCHandle;        // insert element at front
begin
  result := FContainer.InsertItem(FContainer.First, v);
end;

function  TTChar.Pop: TCharType;            // remove first element
var
  h: TCHandle;
begin
  h := FContainer.First;
  Result := items[h];
  FContainer.Delete(h);
end;

function TTChar.PushBack(v: TCharType):TCHandle;    // insert element at back
begin
  result := FContainer.AddItem(v);
end;

function  TTChar.PopBack: TCharType;        // remove last element
var
  h: TCHandle;
begin
  h := FContainer.Last;
  result := items[h];
  FContainer.Delete(h);
end;

function TTChar.Enqueue(v: TCharType):TCHandle;    // add to tail
begin
  result := FContainer.AddItem(v);
end;

function  TTChar.Dequeue: TCharType;       // read&remove from head
var
  h: TCHandle;
begin
  h := FContainer.First;
  Result := items[h];
  FContainer.Delete(h);
end;

function TTChar.AddTo(handle: TCHandle; dst: TTCustom): TCHandle;
begin
  Result := dst.Add([items[handle]]);
end;

function TTChar.GetAsVariant(handle: TCHandle): variant;
begin
  if handle=-1 then
    Result := null
  else
    Result := items[handle];
end;

procedure TTChar.SetAsVariant(handle: TCHandle; Value: variant);
var
  s: string;
begin
  s := Value;
  if s = '' then
    items[handle] := #0
  else
    items[handle] := s[1];
end;

function TTChar.GetAsAnsiStr(handle: TCHandle): AnsiString;
begin
  result := AnsiChar(items[handle]);
end;

procedure TTChar.SetAsAnsiStr(handle: TCHandle; Value: AnsiString);
begin
  assert(length(value)=1, erOperation);
  items[handle] := string(value)[1];
end;

function TTChar.GetAsWideStr(handle: TCHandle): WideString;
begin
  result := items[handle];
end;

procedure TTChar.SetAsWideStr(handle: TCHandle; Value: WideString);
begin
  assert(length(value)=1, erOperation);
  items[handle] := string(value)[1];
end;

procedure TTChar.InitContainer(c: TCCustom);
begin
  inherited;
  c.FCompareValue := CmpInt;
end;

procedure TTChar.SetOnCompare(c: TOnCharCompare);
begin
  if assigned(c) then
    container.FCompareValue := CmpExt
  else
    container.FCompareValue := CmpInt;
  FOnCompare := c;
  FOnCompareProc := nil;
end;

procedure TTChar.SetOnCompareProc(c: TOnCharCompareProc);
begin
  if assigned(c) then
    container.FCompareValue := CmpExtProc
  else
    container.FCompareValue := CmpInt;
  FOnCompareProc := c;
  FOnCompare := nil;
end;

function TTChar.CmpInt(var a,b):integer;
begin
  if TCharType(a)<TCharType(b) then
    result := -1
  else
    if TCharType(a)=TCharType(b) then
      result := 0
    else
      result := 1;
end;

function TTChar.CmpExt(var a, b): integer;
begin
  Result := FOnCompare(TCharType(a), TCharType(b));
end;

function TTChar.CmpExtProc(var a, b): integer;
begin
  Result := FOnCompareProc(TCharType(a), TCharType(b));
end;

procedure TTChar.Sort(compare: TOnCharCompare);
begin
  Sort(compare, FContainer.First, FContainer.Last);
end;

procedure TTChar.Sort(compare: TOnCharCompareProc);
begin
  Sort(compare, FContainer.First, FContainer.Last);
end;

procedure TTChar.Sort(compare: TOnCharCompare; AFirst, ALast: TCHandle);
begin
  FOnCompare := compare;
  FContainer.Sort(CmpExt, AFirst, ALast);
end;

procedure TTChar.Sort(compare: TOnCharCompareProc; AFirst, ALast: TCHandle);
begin
  FOnCompareProc := compare;
  FContainer.Sort(CmpExtProc, AFirst, ALast);
end;

function TTChar.STCopy(Dst: TTCustom; h: TCHandle):TCHandle;
begin
  result := TTChar(dst).Add(Items[h]);
end;

procedure TTChar.STCopy(Dst: TTCustom; AFirst, ALast: TCHandle);
begin
  TTChar(dst).Add(Items[AFirst]);
  if AFirst<>ALast then
    repeat
      FContainer.Next(AFirst);
      TTChar(dst).Add(Items[AFirst]);
    until AFirst=ALast;
end;

procedure TTChar.STSet(Dst: TTCustom; h1,h2: TCHandle);
begin
  TTChar(dst)[h2] := Items[h1];
end;

procedure TTChar.STSet(Dst: TTCustom; start1,finish1, start2: TCHandle);
begin
  TTChar(dst)[start2] := Items[start1];
  if start1<>finish1 then
    repeat
      FContainer.Next(start1);
      dst.FContainer.Next(start2);
      TTChar(dst)[start2] := Items[start1];
    until start1=finish1;
end;

function TTChar.GetMap(key: TCharType): variant;
begin
  Result := FContainer.Fields[1].AsVariant[Find(key)];
end;

procedure TTChar.PutMap(key: TCharType; Value: variant);
var
  h: TCHandle;
begin
  h := Find(key);
  if h = -1 then
    AddPair(key, Value)
  else
    FContainer.Fields[1].AsVariant[h] := Value;
end;

function TTChar.GetHandleOf(Value: variant): TCHandle;
var
  s: string;
begin
  s := Value;
  if s = '' then
    Result := Find(#0)
  else
    Result := Find(s[1]);
end;

function TTChar.AddList(const Items: array of const): TCHandle;
var
  i: integer;
  p: PVarRec;
begin
  Result := -1;
  for i := low(items) to high(items) do
  begin
    p := @items[i];
    if p.VType = vtObject then
      Result := ImportObject(p.VObject, Result)
    else
      Result := add(VarAsChar(p));
  end;
end;

function TTChar.Find(Item: TCharType): TCHandle;
begin
  Result := FContainer.Find(item);
end;

function TTChar.FindFirstEqual(Item: TCharType): TCHandle;
begin
  Result := FContainer.FindFirstEqual(item);
end;

procedure TTChar.FindNextEqual(Item: TCharType; var handle: TCHandle);
begin
  FContainer.FindNextEqual(item, handle);
end;

function TTChar.GetItem(n: TCHandle): TCharType;
begin
  Result := TCharType(FContainer.Get(n)^);
end;

function TTChar.GetMinValue: TCharType;
begin
  Result := TCharType(FContainer.Get(FContainer.FindMin)^);
end;

function TTChar.RemoveMinValue: TCharType;
var
  n: TCHandle;
begin
  n := FContainer.FindMin;
  Result := TCharType(FContainer.Get(n)^);
  FContainer.Delete(n);
end;

function TTChar.GetMaxValue: TCharType;
begin
  Result := TCharType(FContainer.Get(FContainer.FindMax)^);
end;

function TTChar.RemoveMaxValue: TCharType;
var
  n: TCHandle;
begin
  n := FContainer.FindMax;
  Result := TCharType(FContainer.Get(n)^);
  FContainer.Delete(n);
end;

function TTChar.AddPair(key: TCharType; Value: variant): TCHandle;
begin
  result := Add(key);
  Container.Fields[1].AsVariant[result] := Value;
end;

function TTChar.Insert(handle: TCHandle; Item: TCharType): TCHandle;
begin
  Result := FContainer.InsertItem(handle, item);
end;

function TTChar.InsertAfter(handle: TCHandle; Item: TCharType): TCHandle;
begin
  Result := FContainer.InsertAfterItem(handle, item);
end;

procedure TTChar.Remove(Item: TCharType);
begin
  FContainer.Remove(item);
end;

procedure TTChar.RemoveAll(Item: TCharType);
begin
  FContainer.RemoveAll(item);
end;

procedure TTChar.SetItem(n: TCHandle; Value: TCharType);
begin
  FContainer.Put(n, Value);
end;

// TTAnsiChar

procedure SortMemoryRangeAnsiChar(Buf: pointer; L, R: Integer);
begin
  if R>L then
    VectorAnsiCharQuickSort(@PAnsiCharList(Buf)[L], R-L+1);
end;

constructor TTAnsiChar.Create(Container: TCCustom = nil);
begin
  FIsTextField := true;
  FieldSize := sizeof(TAnsiCharType);
  FSortMemoryRange := @SortMemoryRangeAnsiChar;
  inherited;
end;

function TTAnsiChar.Add(Item: TAnsiCharType): TCHandle;
begin
  Result := FContainer.AddItem(item);
end;

function  TTAnsiChar.Front: TAnsiCharType;          // examine first element
begin
  result := items[FContainer.First];
end;

function  TTAnsiChar.Back: TAnsiCharType;           // examine last element
begin
  result := items[FContainer.Last];
end;

function TTAnsiChar.Push(v: TAnsiCharType):TCHandle;        // insert element at front
begin
  result := FContainer.InsertItem(FContainer.First, v);
end;

function  TTAnsiChar.Pop: TAnsiCharType;            // remove first element
var
  h: TCHandle;
begin
  h := FContainer.First;
  Result := items[h];
  FContainer.Delete(h);
end;

function TTAnsiChar.PushBack(v: TAnsiCharType):TCHandle;    // insert element at back
begin
  result := FContainer.AddItem(v);
end;

function  TTAnsiChar.PopBack: TAnsiCharType;        // remove last element
var
  h: TCHandle;
begin
  h := FContainer.Last;
  result := items[h];
  FContainer.Delete(h);
end;

function TTAnsiChar.Enqueue(v: TAnsiCharType):TCHandle;    // add to tail
begin
  result := FContainer.AddItem(v);
end;

function  TTAnsiChar.Dequeue: TAnsiCharType;       // read&remove from head
var
  h: TCHandle;
begin
  h := FContainer.First;
  Result := items[h];
  FContainer.Delete(h);
end;

function TTAnsiChar.AddTo(handle: TCHandle; dst: TTCustom): TCHandle;
begin
  Result := dst.Add([items[handle]]);
end;

function TTAnsiChar.GetAsVariant(handle: TCHandle): variant;
begin
  if handle=-1 then
    Result := null
  else
    Result := items[handle];
end;

procedure TTAnsiChar.SetAsVariant(handle: TCHandle; Value: variant);
var
  s: AnsiString;
begin
  s := AnsiString(Value);
  if s = '' then
    items[handle] := #0
  else
    items[handle] := s[1];
end;

function TTAnsiChar.GetAsAnsiStr(handle: TCHandle): AnsiString;
begin
  result := items[handle];
end;

procedure TTAnsiChar.SetAsAnsiStr(handle: TCHandle; Value: AnsiString);
begin
  assert(length(value)=1, erOperation);
  items[handle] := value[1];
end;

function TTAnsiChar.GetAsWideStr(handle: TCHandle): WideString;
begin
  result := WideChar(items[handle]);
end;

procedure TTAnsiChar.SetAsWideStr(handle: TCHandle; Value: WideString);
begin
  assert(length(value)=1, erOperation);
  items[handle] := AnsiString(value)[1];
end;

procedure TTAnsiChar.InitContainer(c: TCCustom);
begin
  inherited;
  c.FCompareValue := CmpInt;
end;

procedure TTAnsiChar.SetOnCompare(c: TOnAnsiCharCompare);
begin
  if assigned(c) then
    container.FCompareValue := CmpExt
  else
    container.FCompareValue := CmpInt;
  FOnCompare := c;
  FOnCompareProc := nil;
end;

procedure TTAnsiChar.SetOnCompareProc(c: TOnAnsiCharCompareProc);
begin
  if assigned(c) then
    container.FCompareValue := CmpExtProc
  else
    container.FCompareValue := CmpInt;
  FOnCompareProc := c;
  FOnCompare := nil;
end;

function TTAnsiChar.CmpInt(var a,b):integer;
begin
  if TAnsiCharType(a)<TAnsiCharType(b) then
    result := -1
  else
    if TAnsiCharType(a)=TAnsiCharType(b) then
      result := 0
    else
      result := 1;
end;

function TTAnsiChar.CmpExt(var a, b): integer;
begin
  Result := FOnCompare(TAnsiCharType(a), TAnsiCharType(b));
end;

function TTAnsiChar.CmpExtProc(var a, b): integer;
begin
  Result := FOnCompareProc(TAnsiCharType(a), TAnsiCharType(b));
end;

procedure TTAnsiChar.Sort(compare: TOnAnsiCharCompare);
begin
  Sort(compare, FContainer.First, FContainer.Last);
end;

procedure TTAnsiChar.Sort(compare: TOnAnsiCharCompareProc);
begin
  Sort(compare, FContainer.First, FContainer.Last);
end;

procedure TTAnsiChar.Sort(compare: TOnAnsiCharCompare; AFirst, ALast: TCHandle);
begin
  FOnCompare := compare;
  FContainer.Sort(CmpExt, AFirst, ALast);
end;

procedure TTAnsiChar.Sort(compare: TOnAnsiCharCompareProc; AFirst, ALast: TCHandle);
begin
  FOnCompareProc := compare;
  FContainer.Sort(CmpExtProc, AFirst, ALast);
end;

function TTAnsiChar.STCopy(Dst: TTCustom; h: TCHandle):TCHandle;
begin
  result := TTAnsiChar(dst).Add(Items[h]);
end;

procedure TTAnsiChar.STCopy(Dst: TTCustom; AFirst, ALast: TCHandle);
begin
  TTAnsiChar(dst).Add(Items[AFirst]);
  if AFirst<>ALast then
    repeat
      FContainer.Next(AFirst);
      TTAnsiChar(dst).Add(Items[AFirst]);
    until AFirst=ALast;
end;

procedure TTAnsiChar.STSet(Dst: TTCustom; h1,h2: TCHandle);
begin
  TTAnsiChar(dst)[h2] := Items[h1];
end;

procedure TTAnsiChar.STSet(Dst: TTCustom; start1,finish1, start2: TCHandle);
begin
  TTAnsiChar(dst)[start2] := Items[start1];
  if start1<>finish1 then
    repeat
      FContainer.Next(start1);
      dst.FContainer.Next(start2);
      TTAnsiChar(dst)[start2] := Items[start1];
    until start1=finish1;
end;

function TTAnsiChar.GetMap(key: TAnsiCharType): variant;
begin
  Result := FContainer.Fields[1].AsVariant[Find(key)];
end;

procedure TTAnsiChar.PutMap(key: TAnsiCharType; Value: variant);
var
  h: TCHandle;
begin
  h := Find(key);
  if h = -1 then
    AddPair(key, Value)
  else
    FContainer.Fields[1].AsVariant[h] := Value;
end;

function TTAnsiChar.GetHandleOf(Value: variant): TCHandle;
var
  s: AnsiString;
begin
  s := AnsiString(Value);
  if s = '' then
    Result := Find(#0)
  else
    Result := Find(s[1]);
end;

function TTAnsiChar.AddList(const Items: array of const): TCHandle;
var
  i: integer;
  p: PVarRec;
begin
  Result := -1;
  for i := low(items) to high(items) do
  begin
    p := @items[i];
    if p.VType = vtObject then
      Result := ImportObject(p.VObject, Result)
    else
      Result := add(VarAsAnsiChar(p));
  end;
end;

function TTAnsiChar.Find(Item: TAnsiCharType): TCHandle;
begin
  Result := FContainer.Find(item);
end;

function TTAnsiChar.FindFirstEqual(Item: TAnsiCharType): TCHandle;
begin
  Result := FContainer.FindFirstEqual(item);
end;

procedure TTAnsiChar.FindNextEqual(Item: TAnsiCharType; var handle: TCHandle);
begin
  FContainer.FindNextEqual(item, handle);
end;

function TTAnsiChar.GetItem(n: TCHandle): TAnsiCharType;
begin
  Result := TAnsiCharType(FContainer.Get(n)^);
end;

function TTAnsiChar.GetMinValue: TAnsiCharType;
begin
  Result := TAnsiCharType(FContainer.Get(FContainer.FindMin)^);
end;

function TTAnsiChar.RemoveMinValue: TAnsiCharType;
var
  n: TCHandle;
begin
  n := FContainer.FindMin;
  Result := TAnsiCharType(FContainer.Get(n)^);
  FContainer.Delete(n);
end;

function TTAnsiChar.GetMaxValue: TAnsiCharType;
begin
  Result := TAnsiCharType(FContainer.Get(FContainer.FindMax)^);
end;

function TTAnsiChar.RemoveMaxValue: TAnsiCharType;
var
  n: TCHandle;
begin
  n := FContainer.FindMax;
  Result := TAnsiCharType(FContainer.Get(n)^);
  FContainer.Delete(n);
end;

function TTAnsiChar.AddPair(key: TAnsiCharType; Value: variant): TCHandle;
begin
  result := Add(key);
  Container.Fields[1].AsVariant[result] := Value;
end;

function TTAnsiChar.Insert(handle: TCHandle; Item: TAnsiCharType): TCHandle;
begin
  Result := FContainer.InsertItem(handle, item);
end;

function TTAnsiChar.InsertAfter(handle: TCHandle; Item: TAnsiCharType): TCHandle;
begin
  Result := FContainer.InsertAfterItem(handle, item);
end;

procedure TTAnsiChar.Remove(Item: TAnsiCharType);
begin
  FContainer.Remove(item);
end;

procedure TTAnsiChar.RemoveAll(Item: TAnsiCharType);
begin
  FContainer.RemoveAll(item);
end;

procedure TTAnsiChar.SetItem(n: TCHandle; Value: TAnsiCharType);
begin
  FContainer.Put(n, Value);
end;

// TTWideChar

procedure SortMemoryRangeWideChar(Buf: pointer; L, R: Integer);
begin
  if R>L then
    VectorWideCharQuickSort(@PWideCharList(Buf)[L], R-L+1);
end;

constructor TTWideChar.Create(Container: TCCustom = nil);
begin
  FIsTextField := true;
  FieldSize := sizeof(TWideCharType);
  FSortMemoryRange := @SortMemoryRangeWideChar;
  inherited;
end;

function TTWideChar.Add(Item: TWideCharType): TCHandle;
begin
  Result := FContainer.AddItem(item);
end;

function  TTWideChar.Front: TWideCharType;          // examine first element
begin
  result := items[FContainer.First];
end;

function  TTWideChar.Back: TWideCharType;           // examine last element
begin
  result := items[FContainer.Last];
end;

function TTWideChar.Push(v: TWideCharType):TCHandle;        // insert element at front
begin
  result := FContainer.InsertItem(FContainer.First, v);
end;

function  TTWideChar.Pop: TWideCharType;            // remove first element
var
  h: TCHandle;
begin
  h := FContainer.First;
  Result := items[h];
  FContainer.Delete(h);
end;

function TTWideChar.PushBack(v: TWideCharType):TCHandle;    // insert element at back
begin
  result := FContainer.AddItem(v);
end;

function  TTWideChar.PopBack: TWideCharType;        // remove last element
var
  h: TCHandle;
begin
  h := FContainer.Last;
  result := items[h];
  FContainer.Delete(h);
end;

function TTWideChar.Enqueue(v: TWideCharType):TCHandle;    // add to tail
begin
  result := FContainer.AddItem(v);
end;

function  TTWideChar.Dequeue: TWideCharType;       // read&remove from head
var
  h: TCHandle;
begin
  h := FContainer.First;
  Result := items[h];
  FContainer.Delete(h);
end;

function TTWideChar.AddTo(handle: TCHandle; dst: TTCustom): TCHandle;
begin
  Result := dst.Add([items[handle]]);
end;

function TTWideChar.GetAsVariant(handle: TCHandle): variant;
begin
  if handle=-1 then
    Result := null
  else
    Result := items[handle];
end;

procedure TTWideChar.SetAsVariant(handle: TCHandle; Value: variant);
var
  s: WideString;
begin
  s := Value;
  if s = '' then
    items[handle] := #0
  else
    items[handle] := s[1];
end;

function TTWideChar.GetAsAnsiStr(handle: TCHandle): AnsiString;
begin
  result := AnsiChar(items[handle]);
end;

procedure TTWideChar.SetAsAnsiStr(handle: TCHandle; Value: AnsiString);
begin
  assert(length(value)=1, erOperation);
  items[handle] := WideString(value)[1];
end;

function TTWideChar.GetAsWideStr(handle: TCHandle): WideString;
begin
  result := items[handle];
end;

procedure TTWideChar.SetAsWideStr(handle: TCHandle; Value: WideString);
begin
  assert(length(value)=1, erOperation);
  items[handle] := value[1];
end;

procedure TTWideChar.InitContainer(c: TCCustom);
begin
  inherited;
  c.FCompareValue := CmpInt;
end;

procedure TTWideChar.SetOnCompare(c: TOnWideCharCompare);
begin
  if assigned(c) then
    container.FCompareValue := CmpExt
  else
    container.FCompareValue := CmpInt;
  FOnCompare := c;
  FOnCompareProc := nil;
end;

procedure TTWideChar.SetOnCompareProc(c: TOnWideCharCompareProc);
begin
  if assigned(c) then
    container.FCompareValue := CmpExtProc
  else
    container.FCompareValue := CmpInt;
  FOnCompareProc := c;
  FOnCompare := nil;
end;

function TTWideChar.CmpInt(var a,b):integer;
begin
  if TWideCharType(a)<TWideCharType(b) then
    result := -1
  else
    if TWideCharType(a)=TWideCharType(b) then
      result := 0
    else
      result := 1;
end;

function TTWideChar.CmpExt(var a, b): integer;
begin
  Result := FOnCompare(TWideCharType(a), TWideCharType(b));
end;

function TTWideChar.CmpExtProc(var a, b): integer;
begin
  Result := FOnCompareProc(TWideCharType(a), TWideCharType(b));
end;

procedure TTWideChar.Sort(compare: TOnWideCharCompare);
begin
  Sort(compare, FContainer.First, FContainer.Last);
end;

procedure TTWideChar.Sort(compare: TOnWideCharCompareProc);
begin
  Sort(compare, FContainer.First, FContainer.Last);
end;

procedure TTWideChar.Sort(compare: TOnWideCharCompare; AFirst, ALast: TCHandle);
begin
  FOnCompare := compare;
  FContainer.Sort(CmpExt, AFirst, ALast);
end;

procedure TTWideChar.Sort(compare: TOnWideCharCompareProc; AFirst, ALast: TCHandle);
begin
  FOnCompareProc := compare;
  FContainer.Sort(CmpExtProc, AFirst, ALast);
end;

function TTWideChar.STCopy(Dst: TTCustom; h: TCHandle):TCHandle;
begin
  result := TTWideChar(dst).Add(Items[h]);
end;

procedure TTWideChar.STCopy(Dst: TTCustom; AFirst, ALast: TCHandle);
begin
  TTWideChar(dst).Add(Items[AFirst]);
  if AFirst<>ALast then
    repeat
      FContainer.Next(AFirst);
      TTWideChar(dst).Add(Items[AFirst]);
    until AFirst=ALast;
end;

procedure TTWideChar.STSet(Dst: TTCustom; h1,h2: TCHandle);
begin
  TTWideChar(dst)[h2] := Items[h1];
end;

procedure TTWideChar.STSet(Dst: TTCustom; start1,finish1, start2: TCHandle);
begin
  TTWideChar(dst)[start2] := Items[start1];
  if start1<>finish1 then
    repeat
      FContainer.Next(start1);
      dst.FContainer.Next(start2);
      TTWideChar(dst)[start2] := Items[start1];
    until start1=finish1;
end;

function TTWideChar.GetMap(key: TWideCharType): variant;
begin
  Result := FContainer.Fields[1].AsVariant[Find(key)];
end;

procedure TTWideChar.PutMap(key: TWideCharType; Value: variant);
var
  h: TCHandle;
begin
  h := Find(key);
  if h = -1 then
    AddPair(key, Value)
  else
    FContainer.Fields[1].AsVariant[h] := Value;
end;

function TTWideChar.GetHandleOf(Value: variant): TCHandle;
var
  s: WideString;
begin
  s := Value;
  if s = '' then
    Result := Find(#0)
  else
    Result := Find(s[1]);
end;

function TTWideChar.AddList(const Items: array of const): TCHandle;
var
  i: integer;
  p: PVarRec;
begin
  Result := -1;
  for i := low(items) to high(items) do
  begin
    p := @items[i];
    if p.VType = vtObject then
      Result := ImportObject(p.VObject, Result)
    else
      Result := add(VarAsWideChar(p));
  end;
end;

function TTWideChar.Find(Item: TWideCharType): TCHandle;
begin
  Result := FContainer.Find(item);
end;

function TTWideChar.FindFirstEqual(Item: TWideCharType): TCHandle;
begin
  Result := FContainer.FindFirstEqual(item);
end;

procedure TTWideChar.FindNextEqual(Item: TWideCharType; var handle: TCHandle);
begin
  FContainer.FindNextEqual(item, handle);
end;

function TTWideChar.GetItem(n: TCHandle): TWideCharType;
begin
  Result := TWideCharType(FContainer.Get(n)^);
end;

function TTWideChar.GetMinValue: TWideCharType;
begin
  Result := TWideCharType(FContainer.Get(FContainer.FindMin)^);
end;

function TTWideChar.RemoveMinValue: TWideCharType;
var
  n: TCHandle;
begin
  n := FContainer.FindMin;
  Result := TWideCharType(FContainer.Get(n)^);
  FContainer.Delete(n);
end;

function TTWideChar.GetMaxValue: TWideCharType;
begin
  Result := TWideCharType(FContainer.Get(FContainer.FindMax)^);
end;

function TTWideChar.RemoveMaxValue: TWideCharType;
var
  n: TCHandle;
begin
  n := FContainer.FindMax;
  Result := TWideCharType(FContainer.Get(n)^);
  FContainer.Delete(n);
end;

function TTWideChar.AddPair(key: TWideCharType; Value: variant): TCHandle;
begin
  result := Add(key);
  Container.Fields[1].AsVariant[result] := Value;
end;

function TTWideChar.Insert(handle: TCHandle; Item: TWideCharType): TCHandle;
begin
  Result := FContainer.InsertItem(handle, item);
end;

function TTWideChar.InsertAfter(handle: TCHandle; Item: TWideCharType): TCHandle;
begin
  Result := FContainer.InsertAfterItem(handle, item);
end;

procedure TTWideChar.Remove(Item: TWideCharType);
begin
  FContainer.Remove(item);
end;

procedure TTWideChar.RemoveAll(Item: TWideCharType);
begin
  FContainer.RemoveAll(item);
end;

procedure TTWideChar.SetItem(n: TCHandle; Value: TWideCharType);
begin
  FContainer.Put(n, Value);
end;

{ --------------------------------------- TTWord ---------------------------}

procedure SortMemoryRangeWord(Buf: pointer; L, R: Integer);
begin
{$IFDEF FPC}
  if R>L then
    VectorWordQuickSort(@PWordList(Buf)[L], R-L+1);
{$ELSE}
  if R>L then
    VectorWordJumpSort(@PWordList(Buf)[L], R-L+1);
{$ENDIF}
end;

constructor TTWord.Create(Container: TCCustom = nil);
begin
  FieldSize := sizeof(TWordType);
  FSortMemoryRange := @SortMemoryRangeWord;
  inherited;
end;

function TTWord.Add(Item: TWordType): TCHandle;
begin
  Result := FContainer.AddItem(item);
end;

function  TTWord.Front: TWordType;          // examine first element
begin
  result := items[FContainer.First];
end;

function  TTWord.Back: TWordType;           // examine last element
begin
  result := items[FContainer.Last];
end;

function TTWord.Push(v: TWordType): TCHandle;        // insert element at front
begin
  result := FContainer.InsertItem(FContainer.First, v);
end;

function  TTWord.Pop: TWordType;            // remove first element
var
  h: TCHandle;
begin
  h := FContainer.First;
  Result := items[h];
  FContainer.Delete(h);
end;

function TTWord.PushBack(v: TWordType): TCHandle;    // insert element at back
begin
  result := FContainer.AddItem(v);
end;

function  TTWord.PopBack: TWordType;        // remove last element
var
  h: TCHandle;
begin
  h := FContainer.Last;
  result := items[h];
  FContainer.Delete(h);
end;

function TTWord.Enqueue(v: TWordType): TCHandle;    // add to tail
begin
  result := FContainer.AddItem(v);
end;

function  TTWord.Dequeue: TWordType;       // read&remove from head
var
  h: TCHandle;
begin
  h := FContainer.First;
  Result := items[h];
  FContainer.Delete(h);
end;

function TTWord.AddTo(handle: TCHandle; dst: TTCustom): TCHandle;
begin
  Result := dst.Add([items[handle]]);
end;

function TTWord.AddList(const Items: array of const): TCHandle;
var
  i: integer;
  p: PVarRec;
begin
  Result := -1;
  for i := low(items) to high(items) do
  begin
    p := @items[i];
    if p.VType = vtObject then
      Result := ImportObject(p.VObject, Result)
    else
      Result := add(VarAsInteger(p));
  end;
end;

function TTWord.GetAsVariant(handle: TCHandle): variant;
begin
  if handle=-1 then
    Result := null
  else
    Result := items[handle];
end;

procedure TTWord.SetAsVariant(handle: TCHandle; Value: variant);
begin
  items[handle] := Value;
end;

function TTWord.GetAsAnsiStr(handle: TCHandle): AnsiString;
begin
  result := AnsiString(IntToStr(Items[handle]));
end;

procedure TTWord.SetAsAnsiStr(handle: TCHandle; Value: AnsiString);
begin
  Items[handle] := StrToInt(String(value));
end;

function TTWord.GetAsWideStr(handle: TCHandle): WideString;
begin
  result := WideString(AsAnsiStr[handle]);
end;

procedure TTWord.SetAsWideStr(handle: TCHandle; Value: WideString);
begin
  AsAnsiStr[handle] := AnsiString(value);
end;

function TTWord.GetHandleOf(Value: variant): TCHandle;
begin
  Result := Find(Value);
end;

function TTWord.Find(Item: TWordType): TCHandle;
begin
  Result := FContainer.Find(item);
end;

function TTWord.FindFirstEqual(Item: TWordType): TCHandle;
begin
  Result := FContainer.FindFirstEqual(item);
end;

procedure TTWord.FindNextEqual(Item: TWordType; var handle: TCHandle);
begin
  FContainer.FindNextEqual(item, handle);
end;

function TTWord.GetItem(n: TCHandle): TWordType;
begin
  Result := TWordType(FContainer.Get(n)^);
end;

function TTWord.GetMinValue: TWordType;
begin
  Result := TWordType(FContainer.Get(FContainer.FindMin)^);
end;

function TTWord.RemoveMinValue: TWordType;
var
  n: TCHandle;
begin
  n := FContainer.FindMin;
  Result := TWordType(FContainer.Get(n)^);
  FContainer.Delete(n);
end;

function TTWord.GetMaxValue: TWordType;
begin
  Result := TWordType(FContainer.Get(FContainer.FindMax)^);
end;

function TTWord.RemoveMaxValue: TWordType;
var
  n: TCHandle;
begin
  n := FContainer.FindMax;
  Result := TWordType(FContainer.Get(n)^);
  FContainer.Delete(n);
end;

function TTWord.Insert(handle: TCHandle; Item: TWordType): TCHandle;
begin
  Result := FContainer.InsertItem(handle, item);
end;

function TTWord.InsertAfter(handle: TCHandle; Item: TWordType): TCHandle;
begin
  Result := FContainer.InsertAfterItem(handle, item);
end;

procedure TTWord.Remove(Item: TWordType);
begin
  FContainer.Remove(item);
end;

procedure TTWord.RemoveAll(Item: TWordType);
begin
  FContainer.RemoveAll(item);
end;

procedure TTWord.SetItem(n: TCHandle; Value: TWordType);
begin
  FContainer.Put(n, Value);
end;

{ --------------------------------------- TTBoolean ---------------------------}

constructor TTBoolean.Create(Container: TCCustom = nil);
begin
  FieldSize := sizeof(TBooleanType);
  FSortMemoryRange := @SortMemoryRangeByte;
  inherited;
end;

function TTBoolean.Add(Item: TBooleanType): TCHandle;
begin
  Result := FContainer.AddItem(item);
end;

function TTBoolean.AddPair(key: TBooleanType; Value: variant):TCHandle;
begin
  result := Add(key);
  Container.Fields[1].AsVariant[result] := Value;
end;

function  TTBoolean.Front: TBooleanType;          // examine first element
begin
  result := items[FContainer.First];
end;

function  TTBoolean.Back: TBooleanType;           // examine last element
begin
  result := items[FContainer.Last];
end;

function TTBoolean.Push(v: TBooleanType): TCHandle;        // insert element at front
begin
  result := FContainer.InsertItem(FContainer.First, v);
end;

function  TTBoolean.Pop: TBooleanType;            // remove first element
var
  h: TCHandle;
begin
  h := FContainer.First;
  Result := items[h];
  FContainer.Delete(h);
end;

function TTBoolean.PushBack(v: TBooleanType): TCHandle;    // insert element at back
begin
  result := FContainer.AddItem(v);
end;

function  TTBoolean.PopBack: TBooleanType;        // remove last element
var
  h: TCHandle;
begin
  h := FContainer.Last;
  result := items[h];
  FContainer.Delete(h);
end;

function TTBoolean.Enqueue(v: TBooleanType): TCHandle;    // add to tail
begin
  result := FContainer.AddItem(v);
end;

function  TTBoolean.Dequeue: TBooleanType;       // read&remove from head
var
  h: TCHandle;
begin
  h := FContainer.First;
  Result := items[h];
  FContainer.Delete(h);
end;

function TTBoolean.AddList(const Items: array of const): TCHandle;
var
  i: integer;
  p: PVarRec;
begin
  Result := -1;
  for i := low(items) to high(items) do
  begin
    p := @items[i];
    if p.VType = vtObject then
      Result := ImportObject(p.VObject, Result)
    else
      Result := add(VarAsBoolean(p));
  end;
end;

function TTBoolean.AddTo(handle: TCHandle; dst: TTCustom): TCHandle;
begin
  Result := dst.Add([items[handle]]);
end;

function TTBoolean.GetAsVariant(handle: TCHandle): variant;
begin
  if handle=-1 then
    Result := null
  else
    Result := items[handle];
end;

procedure TTBoolean.SetAsVariant(handle: TCHandle; Value: variant);
begin
  items[handle] := Value;
end;

function TTBoolean.GetAsAnsiStr(handle: TCHandle): AnsiString;
begin
  if items[handle] then
    result := '1'
  else
    result := '0';
end;

procedure TTBoolean.SetAsAnsiStr(handle: TCHandle; Value: AnsiString);
begin
  items[handle] := value='1';
end;

function TTBoolean.GetAsWideStr(handle: TCHandle): WideString;
begin
  if items[handle] then
    result := '1'
  else
    result := '0';
end;

procedure TTBoolean.SetAsWideStr(handle: TCHandle; Value: WideString);
begin
  items[handle] := value='1';
end;

function TTBoolean.GetHandleOf(Value: variant): TCHandle;
begin
  Result := Find(Value);
end;

function TTBoolean.Find(Item: TBooleanType): TCHandle;
begin
  Result := FContainer.Find(item);
end;

function TTBoolean.FindFirstEqual(Item: TBooleanType): TCHandle;
begin
  Result := FContainer.FindFirstEqual(item);
end;

procedure TTBoolean.FindNextEqual(Item: TBooleanType; var handle: TCHandle);
begin
  FContainer.FindNextEqual(item, handle);
end;

function TTBoolean.GetItem(n: TCHandle): TBooleanType;
begin
  Result := TBooleanType(FContainer.Get(n)^);
end;

function TTBoolean.GetMinValue: TBooleanType;
begin
  Result := TBooleanType(FContainer.Get(FContainer.FindMin)^);
end;

function TTBoolean.RemoveMinValue: TBooleanType;
var
  n: TCHandle;
begin
  n := FContainer.FindMin;
  Result := TBooleanType(FContainer.Get(n)^);
  FContainer.Delete(n);
end;

function TTBoolean.GetMaxValue: TBooleanType;
begin
  Result := TBooleanType(FContainer.Get(FContainer.FindMax)^);
end;

function TTBoolean.RemoveMaxValue: TBooleanType;
var
  n: TCHandle;
begin
  n := FContainer.FindMax;
  Result := TBooleanType(FContainer.Get(n)^);
  FContainer.Delete(n);
end;

function TTBoolean.Insert(handle: TCHandle; Item: TBooleanType): TCHandle;
begin
  Result := FContainer.InsertItem(handle, item);
end;

function TTBoolean.InsertAfter(handle: TCHandle; Item: TBooleanType): TCHandle;
begin
  Result := FContainer.InsertAfterItem(handle, item);
end;

procedure TTBoolean.Remove(Item: TBooleanType);
begin
  FContainer.Remove(item);
end;

procedure TTBoolean.RemoveAll(Item: TBooleanType);
begin
  FContainer.RemoveAll(item);
end;

procedure TTBoolean.SetItem(n: TCHandle; Value: TBooleanType);
begin
  FContainer.Put(n, Value);
end;

{ --------------------------------------- TTRect ---------------------------}

constructor TTRect.Create(Container: TCCustom = nil);
begin
  FieldSize := sizeof(TRectType);
  inherited;
end;

function TTRect.Add(Item: TRectType): TCHandle;
begin
  Result := FContainer.AddItem(item);
end;

function  TTRect.Front: TRectType;          // examine first element
begin
  result := items[FContainer.First];
end;

function  TTRect.Back: TRectType;           // examine last element
begin
  result := items[FContainer.Last];
end;

function TTRect.Push(v: TRectType): TCHandle;        // insert element at front
begin
  result := FContainer.InsertItem(FContainer.First, v);
end;

function  TTRect.Pop: TRectType;            // remove first element
var
  h: TCHandle;
begin
  h := FContainer.First;
  Result := items[h];
  FContainer.Delete(h);
end;

function TTRect.PushBack(v: TRectType): TCHandle;    // insert element at back
begin
  result := FContainer.AddItem(v);
end;

function  TTRect.PopBack: TRectType;        // remove last element
var
  h: TCHandle;
begin
  h := FContainer.Last;
  result := items[h];
  FContainer.Delete(h);
end;

function TTRect.Enqueue(v: TRectType): TCHandle;    // add to tail
begin
  result := FContainer.AddItem(v);
end;

function  TTRect.Dequeue: TRectType;       // read&remove from head
var
  h: TCHandle;
begin
  h := FContainer.First;
  Result := items[h];
  FContainer.Delete(h);
end;

function TTRect.AddTo(handle: TCHandle; dst: TTCustom): TCHandle;
begin
  with items[handle] do
    Result := dst.Add([left, top, right, bottom]);
end;

function TTRect.AddList(const AItems: array of const): TCHandle;
var
  i, v: integer;
  p: PVarRec;
  t: PRect;
begin
  Result := -1;
  for i := low(AItems) to high(AItems) do
  begin
    p := @AItems[i];
    if p.VType = vtObject then
      Result := ImportObject(p.VObject, Result)
    else
    begin
      v := VarAsInteger(p);
      if FComplete = 0 then
        FPos := add(rect(0, 0, 0, 0));
      t := FContainer.Get(FPos);
      case FComplete of
        0: t.Left := v;
        1: t.Top  := v;
        2: t.Right := v;
        3: t.Bottom := v;
      end;
      Inc(FComplete);
      if FComplete > 3 then
        FComplete := 0;
      Result := FPos;
    end;
  end;
end;

function TTRect.GetAsVariant(handle: TCHandle): variant;
begin
  if handle=-1 then
    Result := null
  else
  begin
    result := VarArrayCreate([0,3], varInteger);
    with items[handle] do
    begin
      result[0] := left;
      result[1] := top;
      result[2] := right;
      result[3] := bottom;
    end;
  end;
end;

procedure TTRect.SetAsVariant(handle: TCHandle; Value: variant);
var p: TRect;
begin
  p.left := value[0];
  p.top := value[1];
  p.right := value[2];
  p.bottom := value[3];
  items[handle] := p;
end;

function TTRect.GetAsAnsiStr(handle: TCHandle): AnsiString;
begin
  with items[handle] do
    result := AnsiString(inttostr(left)+' '+
      inttostr(top)+' '+inttostr(right)+' '+inttostr(bottom));
end;

// examples: "(1;1 80 25)", "2 2 16 16", ...
procedure TTRect.SetAsAnsiStr(handle: TCHandle; Value: AnsiString);
var
  r: TRect;
  H: TStrTokenA;
begin
  with r do
  begin
    STokOpenIntA(Value, H);
    left := STokReadIntA(H);
    top := STokReadIntA(H);
    right := STokReadIntA(H);
    bottom := STokReadIntA(H);
  end;
  Items[handle] := r;
end;

function TTRect.GetAsWideStr(handle: TCHandle): WideString;
begin
  result := WideString(AsAnsiStr[handle]);
end;

procedure TTRect.SetAsWideStr(handle: TCHandle; Value: WideString);
begin
  AsAnsiStr[handle] := AnsiString(value);
end;

function TTRect.LoadItem(c: TCCustom; h: TCHandle; src: TStream; FileFormat : TFileFormat = ffDefault):TCHandle;
var r: TRectType;
begin
  if FileFormat=ffBinary then
    src.Read(r, sizeof(r))
  else
    with r do
    begin
      left := StrToInt(c.LoadToken);
      top := StrToInt(c.LoadToken);
      right := StrToInt(c.LoadToken);
      bottom := StrToInt(c.LoadToken);
    end;
  if h=-1 then
    result := Add(r)
  else
  begin
    result := h;
    items[h] := r;
  end;
end;

procedure TTRect.InitContainer(c: TCCustom);
begin
  inherited;
  c.FCompareValue := CmpInt;
end;

function TTRect.CmpInt(var a,b):integer;
begin
  with TRectType(a) do
  if Left<TRectType(b).Left then
    result := -1
  else
  if Left>TRectType(b).Left then
    result := 1
  else
    if Top<TRectType(b).Top then
      result := -1
    else
    if Top>TRectType(b).Top then
      result := 1
    else
      if Right<TRectType(b).Right then
        result := -1
      else
      if Right>TRectType(b).Right then
        result := 1
      else
        if Bottom<TRectType(b).Bottom then
          result := -1
        else
        if Bottom>TRectType(b).Bottom then
          result := 1
        else
          result := 0;
end;

procedure TTRect.SetOnCompare(c: TOnRectCompare);
begin
  if assigned(c) then
    container.FCompareValue := CmpExt
  else
    container.FCompareValue := CmpInt;
  FOnCompare := c;
  FOnCompareProc := nil;
end;

procedure TTRect.SetOnCompareProc(c: TOnRectCompareProc);
begin
  if assigned(c) then
    container.FCompareValue := CmpExtProc
  else
    container.FCompareValue := CmpInt;
  FOnCompareProc := c;
  FOnCompare := nil;
end;

function TTRect.CmpExt(var a, b): integer;
begin
  Result := FOnCompare(TRectType(a), TRectType(b));
end;

function TTRect.CmpExtProc(var a, b): integer;
begin
  Result := FOnCompareProc(TRectType(a), TRectType(b));
end;

procedure TTRect.Sort(compare: TOnRectCompare);
begin
  Sort(compare, FContainer.First, FContainer.Last);
end;

procedure TTRect.Sort(compare: TOnRectCompareProc);
begin
  Sort(compare, FContainer.First, FContainer.Last);
end;

procedure TTRect.Sort(compare: TOnRectCompare; AFirst, ALast: TCHandle);
begin
  FOnCompare := compare;
  FContainer.Sort(CmpExt, AFirst, ALast);
end;

procedure TTRect.Sort(compare: TOnRectCompareProc; AFirst, ALast: TCHandle);
begin
  FOnCompareProc := compare;
  FContainer.Sort(CmpExtProc, AFirst, ALast);
end;

function TTRect.STCopy(Dst: TTCustom; h: TCHandle):TCHandle;
begin
  result := TTRect(dst).Add(Items[h]);
end;

procedure TTRect.STCopy(Dst: TTCustom; AFirst, ALast: TCHandle);
begin
  TTRect(dst).Add(Items[AFirst]);
  if AFirst<>ALast then
    repeat
      FContainer.Next(AFirst);
      TTRect(dst).Add(Items[AFirst]);
    until AFirst=ALast;
end;

procedure TTRect.STSet(Dst: TTCustom; h1,h2: TCHandle);
begin
  TTRect(dst)[h2] := Items[h1];
end;

procedure TTRect.STSet(Dst: TTCustom; start1,finish1, start2: TCHandle);
begin
  TTRect(dst)[start2] := Items[start1];
  if start1<>finish1 then
    repeat
      FContainer.Next(start1);
      dst.FContainer.Next(start2);
      TTRect(dst)[start2] := Items[start1];
    until start1=finish1;
end;

function TTRect.GetMap(key: TRectType): variant;
begin
  Result := FContainer.Fields[1].AsVariant[Find(key)];
end;

procedure TTRect.PutMap(key: TRectType; Value: variant);
var
  h: TCHandle;
begin
  h := Find(key);
  if h = -1 then
    AddPair(key, Value)
  else
    FContainer.Fields[1].AsVariant[h] := Value;
end;

function TTRect.GetHandleOf(Value: variant): TCHandle;
var
  p: array of byte;
  r: TRect;
begin
  p := Value;
  if length(p) <> FieldSize then
    raise Exception.Create('')
  else
  begin
    system.Move(p[0], r, FieldSize);
    Result := Find(r);
  end;
end;

function TTRect.Find(Item: TRectType): TCHandle;
begin
  Result := FContainer.Find(item);
end;

function TTRect.FindFirstEqual(Item: TRectType): TCHandle;
begin
  Result := FContainer.FindFirstEqual(item);
end;

procedure TTRect.FindNextEqual(Item: TRectType; var handle: TCHandle);
begin
  FContainer.FindNextEqual(item, handle);
end;

function TTRect.GetItem(n: TCHandle): TRectType;
begin
  Result := TRectType(FContainer.Get(n)^);
end;

function TTRect.GetMinValue: TRectType;
begin
  Result := TRectType(FContainer.Get(FContainer.FindMin)^);
end;

function TTRect.RemoveMinValue: TRectType;
var
  n: TCHandle;
begin
  n := FContainer.FindMin;
  Result := TRectType(FContainer.Get(n)^);
  FContainer.Delete(n);
end;

function TTRect.GetMaxValue: TRectType;
begin
  Result := TRectType(FContainer.Get(FContainer.FindMax)^);
end;

function TTRect.RemoveMaxValue: TRectType;
var
  n: TCHandle;
begin
  n := FContainer.FindMax;
  Result := TRectType(FContainer.Get(n)^);
  FContainer.Delete(n);
end;

function TTRect.AddPair(key: TRectType; Value: variant): TCHandle;
begin
  result := Add(key);
  Container.Fields[1].AsVariant[result] := Value;
end;

function TTRect.Insert(handle: TCHandle; Item: TRectType): TCHandle;
begin
  Result := FContainer.InsertItem(handle, item);
end;

function TTRect.InsertAfter(handle: TCHandle; Item: TRectType): TCHandle;
begin
  Result := FContainer.InsertAfterItem(handle, item);
end;

procedure TTRect.Remove(Item: TRectType);
begin
  FContainer.Remove(item);
end;

procedure TTRect.RemoveAll(Item: TRectType);
begin
  FContainer.RemoveAll(item);
end;

procedure TTRect.SetItem(n: TCHandle; Value: TRectType);
begin
  FContainer.Put(n, Value);
end;

{ --------------------------------------- TTFRect ---------------------------}

constructor TTFRect.Create(Container: TCCustom = nil);
begin
  FieldSize := sizeof(TFRectType);
  inherited;
end;

function TTFRect.Add(Item: TFRectType): TCHandle;
begin
  Result := FContainer.AddItem(item);
end;

function  TTFRect.Front: TFRectType;          // examine first element
begin
  result := items[FContainer.First];
end;

function  TTFRect.Back: TFRectType;           // examine last element
begin
  result := items[FContainer.Last];
end;

function TTFRect.Push(v: TFRectType): TCHandle;        // insert element at front
begin
  result := FContainer.InsertItem(FContainer.First, v);
end;

function  TTFRect.Pop: TFRectType;            // remove first element
var
  h: TCHandle;
begin
  h := FContainer.First;
  Result := items[h];
  FContainer.Delete(h);
end;

function TTFRect.PushBack(v: TFRectType): TCHandle;    // insert element at back
begin
  result := FContainer.AddItem(v);
end;

function  TTFRect.PopBack: TFRectType;        // remove last element
var
  h: TCHandle;
begin
  h := FContainer.Last;
  result := items[h];
  FContainer.Delete(h);
end;

function TTFRect.Enqueue(v: TFRectType): TCHandle;    // add to tail
begin
  result := FContainer.AddItem(v);
end;

function  TTFRect.Dequeue: TFRectType;       // read&remove from head
var
  h: TCHandle;
begin
  h := FContainer.First;
  Result := items[h];
  FContainer.Delete(h);
end;

function TTFRect.AddTo(handle: TCHandle; dst: TTCustom): TCHandle;
begin
  with items[handle] do
    Result := dst.Add([left, top, right, bottom]);
end;

function TTFRect.AddList(const AItems: array of const): TCHandle;
var
  i: integer;
  v: double;
  p: PVarRec;
  t: ^TFRect;
begin
  Result := -1;
  for i := low(AItems) to high(AItems) do
  begin
    p := @AItems[i];
    if p.VType = vtObject then
      Result := ImportObject(p.VObject, Result)
    else
    begin
      v := VarAsDouble(p);
      if FComplete = 0 then
        FPos := add(FRect(0, 0, 0, 0));
      t := FContainer.Get(FPos);
      case FComplete of
        0: t.Left := v;
        1: t.Top  := v;
        2: t.Right := v;
        3: t.Bottom := v;
      end;
      Inc(FComplete);
      if FComplete > 3 then
        FComplete := 0;
      Result := FPos;
    end;
  end;
end;

function TTFRect.GetAsVariant(handle: TCHandle): variant;
begin
  if handle=-1 then
    Result := null
  else
  begin
    result := VarArrayCreate([0,3], varDouble);
    with items[handle] do
    begin
      result[0] := left;
      result[1] := top;
      result[2] := right;
      result[3] := bottom;
    end;
  end;
end;

procedure TTFRect.SetAsVariant(handle: TCHandle; Value: variant);
var p: TFRect;
begin
  p.left := value[0];
  p.top := value[1];
  p.right := value[2];
  p.bottom := value[3];
  items[handle] := p;
end;

function TTFRect.GetAsAnsiStr(handle: TCHandle): AnsiString;
begin
  with items[handle] do
    result := AnsiString(FloatToStr(left)+' '+
      FloatToStr(top)+' '+FloatToStr(right)+' '+FloatToStr(bottom));
end;

// examples: "(1;1 80 25)", "2 2 16 16", ...
procedure TTFRect.SetAsAnsiStr(handle: TCHandle; Value: AnsiString);
var
  r: TFRect;
  H: TStrTokenA;
begin
  with r do
  begin
    STokOpenFloatA(Value, H);
    left := STokReadFloatA(H);
    top := STokReadFloatA(H);
    right := STokReadFloatA(H);
    bottom := STokReadFloatA(H);
  end;
  Items[handle] := r;
end;

function TTFRect.GetAsWideStr(handle: TCHandle): WideString;
begin
  result := WideString(AsAnsiStr[handle]);
end;

procedure TTFRect.SetAsWideStr(handle: TCHandle; Value: WideString);
begin
  AsAnsiStr[handle] := AnsiString(value);
end;

function TTFRect.LoadItem(c: TCCustom; h: TCHandle; src: TStream; FileFormat : TFileFormat = ffDefault):TCHandle;
var r: TFRectType;
begin
  if FileFormat=ffBinary then
    src.Read(r, sizeof(r))
  else
    with r do
    begin
      left := StrToFloat(c.LoadToken);
      top := StrToFloat(c.LoadToken);
      right := StrToFloat(c.LoadToken);
      bottom := StrToFloat(c.LoadToken);
    end;
  if h=-1 then
    result := Add(r)
  else
  begin
    result := h;
    items[h] := r;
  end;
end;

procedure TTFRect.InitContainer(c: TCCustom);
begin
  inherited;
  c.FCompareValue := CmpInt;
end;

function TTFRect.CmpInt(var a,b):integer;
begin
  with TFRectType(a) do
    if left<TFRectType(b).left then result := -1 else
      if left>TFRectType(b).left then result := 1 else
        if top<TFRectType(b).top then result := -1 else
          if top>TFRectType(b).top then result := 1 else
            if right<TFRectType(b).right then result := -1 else
              if right>TFRectType(b).right then result := 1 else
                if bottom<TFRectType(b).bottom then result := -1 else
                  if bottom>TFRectType(b).bottom then result := 1 else
                    result := 0;
end;

procedure TTFRect.SetOnCompare(c: TOnFRectCompare);
begin
  if assigned(c) then
    container.FCompareValue := CmpExt
  else
    container.FCompareValue := CmpInt;
  FOnCompare := c;
  FOnCompareProc := nil;
end;

procedure TTFRect.SetOnCompareProc(c: TOnFRectCompareProc);
begin
  if assigned(c) then
    container.FCompareValue := CmpExtProc
  else
    container.FCompareValue := CmpInt;
  FOnCompareProc := c;
  FOnCompare := nil;
end;

function TTFRect.CmpExt(var a, b): integer;
begin
  Result := FOnCompare(TFRectType(a), TFRectType(b));
end;

function TTFRect.CmpExtProc(var a, b): integer;
begin
  Result := FOnCompareProc(TFRectType(a), TFRectType(b));
end;

procedure TTFRect.Sort(compare: TOnFRectCompare);
begin
  Sort(compare, FContainer.First, FContainer.Last);
end;

procedure TTFRect.Sort(compare: TOnFRectCompareProc);
begin
  Sort(compare, FContainer.First, FContainer.Last);
end;

procedure TTFRect.Sort(compare: TOnFRectCompare; AFirst, ALast: TCHandle);
begin
  FOnCompare := compare;
  FContainer.Sort(CmpExt, AFirst, ALast);
end;

procedure TTFRect.Sort(compare: TOnFRectCompareProc; AFirst, ALast: TCHandle);
begin
  FOnCompareProc := compare;
  FContainer.Sort(CmpExtProc, AFirst, ALast);
end;

function TTFRect.STCopy(Dst: TTCustom; h: TCHandle):TCHandle;
begin
  result := TTFRect(dst).Add(Items[h]);
end;

procedure TTFRect.STCopy(Dst: TTCustom; AFirst, ALast: TCHandle);
begin
  TTFRect(dst).Add(Items[AFirst]);
  if AFirst<>ALast then
    repeat
      FContainer.Next(AFirst);
      TTFRect(dst).Add(Items[AFirst]);
    until AFirst=ALast;
end;

procedure TTFRect.STSet(Dst: TTCustom; h1,h2: TCHandle);
begin
  TTFRect(dst)[h2] := Items[h1];
end;

procedure TTFRect.STSet(Dst: TTCustom; start1,finish1, start2: TCHandle);
begin
  TTFRect(dst)[start2] := Items[start1];
  if start1<>finish1 then
    repeat
      FContainer.Next(start1);
      dst.FContainer.Next(start2);
      TTFRect(dst)[start2] := Items[start1];
    until start1=finish1;
end;

function TTFRect.GetMap(key: TFRectType): variant;
begin
  Result := FContainer.Fields[1].AsVariant[Find(key)];
end;

procedure TTFRect.PutMap(key: TFRectType; Value: variant);
var
  h: TCHandle;
begin
  h := Find(key);
  if h = -1 then
    AddPair(key, Value)
  else
    FContainer.Fields[1].AsVariant[h] := Value;
end;

function TTFRect.GetHandleOf(Value: variant): TCHandle;
var
  p: array of byte;
  r: TFRect;
begin
  p := Value;
  if length(p) <> FieldSize then
    raise Exception.Create('')
  else
  begin
    system.Move(p[0], r, FieldSize);
    Result := Find(r);
  end;
end;

function TTFRect.Find(Item: TFRectType): TCHandle;
begin
  Result := FContainer.Find(item);
end;

function TTFRect.FindFirstEqual(Item: TFRectType): TCHandle;
begin
  Result := FContainer.FindFirstEqual(item);
end;

procedure TTFRect.FindNextEqual(Item: TFRectType; var handle: TCHandle);
begin
  FContainer.FindNextEqual(item, handle);
end;

function TTFRect.GetItem(n: TCHandle): TFRectType;
begin
  Result := TFRectType(FContainer.Get(n)^);
end;

function TTFRect.GetMinValue: TFRectType;
begin
  Result := TFRectType(FContainer.Get(FContainer.FindMin)^);
end;

function TTFRect.RemoveMinValue: TFRectType;
var
  n: TCHandle;
begin
  n := FContainer.FindMin;
  Result := TFRectType(FContainer.Get(n)^);
  FContainer.Delete(n);
end;

function TTFRect.GetMaxValue: TFRectType;
begin
  Result := TFRectType(FContainer.Get(FContainer.FindMax)^);
end;

function TTFRect.RemoveMaxValue: TFRectType;
var
  n: TCHandle;
begin
  n := FContainer.FindMax;
  Result := TFRectType(FContainer.Get(n)^);
  FContainer.Delete(n);
end;

function TTFRect.AddPair(key: TFRectType; Value: variant): TCHandle;
begin
  result := Add(key);
  Container.Fields[1].AsVariant[result] := Value;
end;

function TTFRect.Insert(handle: TCHandle; Item: TFRectType): TCHandle;
begin
  Result := FContainer.InsertItem(handle, item);
end;

function TTFRect.InsertAfter(handle: TCHandle; Item: TFRectType): TCHandle;
begin
  Result := FContainer.InsertAfterItem(handle, item);
end;

procedure TTFRect.Remove(Item: TFRectType);
begin
  FContainer.Remove(item);
end;

procedure TTFRect.RemoveAll(Item: TFRectType);
begin
  FContainer.RemoveAll(item);
end;

procedure TTFRect.SetItem(n: TCHandle; Value: TFRectType);
begin
  FContainer.Put(n, Value);
end;

{ --------------------------------------- TTPoint ---------------------------}

constructor TTPoint.Create(Container: TCCustom = nil);
begin
  FieldSize := sizeof(TPointType);
  inherited;
end;

function TTPoint.GetAsVariant(handle: TCHandle): variant;
begin
  if handle=-1 then
    Result := null
  else
  begin
    result := VarArrayCreate([0,1], varInteger);
    with items[handle] do
    begin
      result[0] := x;
      result[1] := y;
    end;
  end;
end;

procedure TTPoint.SetAsVariant(handle: TCHandle; Value: variant);
var p: TPoint;
begin
  p.x := value[0];
  p.y := value[1];
  items[handle] := p;
end;

function TTPoint.GetHandleOf(Value: variant): TCHandle;
var p: TPoint;
begin
  p.x := value[0];
  p.y := value[1];
  result := Find(p);
end;

function TTPoint.GetAsAnsiStr(handle: TCHandle): AnsiString;
begin
  with items[handle] do
    result := AnsiString(inttostr(x)+' '+inttostr(y));
end;

// examples:
// (80; 25)
// +22 -16
procedure TTPoint.SetAsAnsiStr(handle: TCHandle; Value: AnsiString);
var
  r: TPoint;
  p: TStrTokenA;
begin
  with r do
  begin
    STokOpenIntA(Value, p);
    x := STokReadIntA(p);
    y := STokReadIntA(p);
  end;
  Items[handle] := r;
end;

function TTPoint.GetAsWideStr(handle: TCHandle): WideString;
begin
  result := WideString(AsAnsiStr[handle]);
end;

procedure TTPoint.SetAsWideStr(handle: TCHandle; Value: WideString);
begin
  AsAnsiStr[handle] := AnsiString(value);
end;

function TTPoint.LoadItem(c: TCCustom; h: TCHandle; src: TStream; FileFormat : TFileFormat = ffDefault):TCHandle;
var r: TPointType;
begin
  if FileFormat=ffBinary then
    src.Read(r, sizeof(r))
  else
    with r do
    begin
      x := StrToInt(c.LoadToken);
      y := StrToInt(c.LoadToken);
    end;
  if h=-1 then
    result := Add(r)
  else
  begin
    result := h;
    items[h] := r;
  end;
end;

procedure TTPoint.InitContainer(c: TCCustom);
begin
  inherited;
  c.FCompareValue := CmpInt;
end;

function TTPoint.CmpInt(var a,b):integer;
begin
  with TPointType(a) do
    if X<TPointType(b).X then
      result := -1
    else
    if X>TPointType(b).X then
      result := 1
    else
      if Y<TPointType(b).Y then
        result := -1
      else
      if Y>TPointType(b).Y then
        result := 1
      else
        result := 0;
end;

procedure TTPoint.SetOnCompare(c: TOnPointCompare);
begin
  if assigned(c) then
    container.FCompareValue := CmpExt
  else
    container.FCompareValue := CmpInt;
  FOnCompare := c;
  FOnCompareProc := nil;
end;

procedure TTPoint.SetOnCompareProc(c: TOnPointCompareProc);
begin
  if assigned(c) then
    container.FCompareValue := CmpExtProc
  else
    container.FCompareValue := CmpInt;
  FOnCompareProc := c;
  FOnCompare := nil;
end;

function TTPoint.CmpExt(var a, b): integer;
begin
  Result := FOnCompare(TPointType(a), TPointType(b));
end;

function TTPoint.CmpExtProc(var a, b): integer;
begin
  Result := FOnCompareProc(TPointType(a), TPointType(b));
end;

procedure TTPoint.Sort(compare: TOnPointCompare);
begin
  Sort(compare, FContainer.First, FContainer.Last);
end;

procedure TTPoint.Sort(compare: TOnPointCompareProc);
begin
  Sort(compare, FContainer.First, FContainer.Last);
end;

procedure TTPoint.Sort(compare: TOnPointCompare; AFirst, ALast: TCHandle);
begin
  FOnCompare := compare;
  FContainer.Sort(CmpExt, AFirst, ALast);
end;

procedure TTPoint.Sort(compare: TOnPointCompareProc; AFirst, ALast: TCHandle);
begin
  FOnCompareProc := compare;
  FContainer.Sort(CmpExtProc, AFirst, ALast);
end;

function TTPoint.STCopy(Dst: TTCustom; h: TCHandle):TCHandle;
begin
  result := TTPoint(dst).Add(Items[h]);
end;

procedure TTPoint.STCopy(Dst: TTCustom; AFirst, ALast: TCHandle);
begin
  TTPoint(dst).Add(Items[AFirst]);
  if AFirst<>ALast then
    repeat
      FContainer.Next(AFirst);
      TTPoint(dst).Add(Items[AFirst]);
    until AFirst=ALast;
end;

procedure TTPoint.STSet(Dst: TTCustom; h1,h2: TCHandle);
begin
  TTPoint(dst)[h2] := Items[h1];
end;

procedure TTPoint.STSet(Dst: TTCustom; start1,finish1, start2: TCHandle);
begin
  TTPoint(dst)[start2] := Items[start1];
  if start1<>finish1 then
    repeat
      FContainer.Next(start1);
      dst.FContainer.Next(start2);
      TTPoint(dst)[start2] := Items[start1];
    until start1=finish1;
end;

function TTPoint.GetMap(key: TPointType): variant;
begin
  Result := FContainer.Fields[1].AsVariant[Find(key)];
end;

procedure TTPoint.PutMap(key: TPointType; Value: variant);
var
  h: TCHandle;
begin
  h := Find(key);
  if h = -1 then
    AddPair(key, Value)
  else
    FContainer.Fields[1].AsVariant[h] := Value;
end;

function TTPoint.Add(Item: TPointType): TCHandle;
begin
  Result := FContainer.AddItem(item);
end;

function TTPoint.Add(x,y: integer): TCHandle;
var p: TPointType;
begin
  p.X := x;
  p.Y := y;
  Result := FContainer.AddItem(p);
end;

function  TTPoint.Front: TPointType;          // examine first element
begin
  result := items[FContainer.First];
end;

function  TTPoint.Back: TPointType;           // examine last element
begin
  result := items[FContainer.Last];
end;

function TTPoint.Push(v: TPointType): TCHandle;        // insert element at front
begin
  result := FContainer.InsertItem(FContainer.First, v);
end;

function  TTPoint.Pop: TPointType;            // remove first element
var
  h: TCHandle;
begin
  h := FContainer.First;
  Result := items[h];
  FContainer.Delete(h);
end;

function TTPoint.PushBack(v: TPointType): TCHandle;    // insert element at back
begin
  result := FContainer.AddItem(v);
end;

function  TTPoint.PopBack: TPointType;        // remove last element
var
  h: TCHandle;
begin
  h := FContainer.Last;
  result := items[h];
  FContainer.Delete(h);
end;

function TTPoint.Enqueue(v: TPointType): TCHandle;    // add to tail
begin
  result := FContainer.AddItem(v);
end;

function  TTPoint.Dequeue: TPointType;       // read&remove from head
var
  h: TCHandle;
begin
  h := FContainer.First;
  Result := items[h];
  FContainer.Delete(h);
end;

function TTPoint.AddTo(handle: TCHandle; dst: TTCustom): TCHandle;
begin
  with items[handle] do
    Result := dst.Add([x, y]);
end;

function TTPoint.AddList(const AItems: array of const): TCHandle;
var
  i: integer;
  p: PVarRec;
  t: PPoint;
begin
  Result := -1;
  for i := low(aitems) to high(aitems) do
  begin
    p := @aitems[i];
    if p.VType = vtObject then
      Result := ImportObject(p.VObject, Result)
    else
    if FComplete = 0 then
    begin
      FComplete := 1;
      FPos := add(point(VarAsInteger(p), 0));
      Result := FPos;
    end
    else
    begin
      t := FContainer.Get(FPos);
      t.Y := VarAsInteger(p);
      FComplete := 0;
      Result := FPos;
    end;
  end;
end;

function TTPoint.Find(Item: TPointType): TCHandle;
begin
  Result := FContainer.Find(item);
end;

function TTPoint.FindFirstEqual(Item: TPointType): TCHandle;
begin
  Result := FContainer.FindFirstEqual(item);
end;

procedure TTPoint.FindNextEqual(Item: TPointType; var handle: TCHandle);
begin
  FContainer.FindNextEqual(item, handle);
end;

function TTPoint.GetItem(n: TCHandle): TPointType;
begin
  Result := TPointType(FContainer.Get(n)^);
end;

function TTPoint.GetMinValue: TPointType;
begin
  Result := TPointType(FContainer.Get(FContainer.FindMin)^);
end;

function TTPoint.RemoveMinValue: TPointType;
var
  n: TCHandle;
begin
  n := FContainer.FindMin;
  Result := TPointType(FContainer.Get(n)^);
  FContainer.Delete(n);
end;

function TTPoint.GetMaxValue: TPointType;
begin
  Result := TPointType(FContainer.Get(FContainer.FindMax)^);
end;

function TTPoint.RemoveMaxValue: TPointType;
var
  n: TCHandle;
begin
  n := FContainer.FindMax;
  Result := TPointType(FContainer.Get(n)^);
  FContainer.Delete(n);
end;

function TTPoint.AddPair(key: TPointType; Value: variant): TCHandle;
begin
  result := Add(key);
  Container.Fields[1].AsVariant[result] := Value;
end;

function TTPoint.Insert(handle: TCHandle; Item: TPointType): TCHandle;
begin
  result := FContainer.InsertItem(handle, item);
end;

function TTPoint.InsertAfter(handle: TCHandle; Item: TPointType): TCHandle;
begin
  result := FContainer.InsertAfterItem(handle, item);
end;

procedure TTPoint.Remove(Item: TPointType);
begin
  FContainer.Remove(item);
end;

procedure TTPoint.RemoveAll(Item: TPointType);
begin
  FContainer.RemoveAll(item);
end;

procedure TTPoint.SetItem(n: TCHandle; Value: TPointType);
begin
  FContainer.Put(n, Value);
end;

{ --------------------------------------- TTFPoint ---------------------------}

function FPoint(AX,AY: double):TFPoint;
begin
  with result do
  begin
    x := ax;
    y := ay;
  end;
end;

function FRect(ATop, ALeft, ARight, ABottom: double):TFRect;
begin
  with result do
  begin
    left := aleft;
    top := atop;
    right := aright;
    bottom := abottom;
  end;
end;

constructor TTFPoint.Create(Container: TCCustom = nil);
begin
  FieldSize := sizeof(TFPointType);
  inherited;
end;

function TTFPoint.GetAsVariant(handle: TCHandle): variant;
begin
  if handle=-1 then
    Result := null
  else
  begin
    result := VarArrayCreate([0,1], varDouble);
    with items[handle] do
    begin
      result[0] := x;
      result[1] := y;
    end;
  end;
end;

procedure TTFPoint.SetAsVariant(handle: TCHandle; Value: variant);
var p: TFPoint;
begin
  p.x := value[0];
  p.y := value[1];
  items[handle] := p;
end;

function TTFPoint.GetHandleOf(Value: variant): TCHandle;
var p: TFPoint;
begin
  p.x := value[0];
  p.y := value[1];
  result := Find(p);
end;

function TTFPoint.GetAsAnsiStr(handle: TCHandle): AnsiString;
begin
  with items[handle] do
    result := AnsiString(FloatToStr(x)+' '+FloatToStr(y));
end;

// examples: "(80, 25)", "22 16", ...
procedure TTFPoint.SetAsAnsiStr(handle: TCHandle; Value: AnsiString);
var
  r: TFPoint;
  H: TStrTokenA;
begin
  with r do
  begin
    STokOpenFloatA(Value, H);
    x := STokReadFloatA(H);
    y := STokReadFloatA(H);
  end;
  Items[handle] := r;
end;

function TTFPoint.GetAsWideStr(handle: TCHandle): WideString;
begin
  result := WideString(AsAnsiStr[handle]);
end;

procedure TTFPoint.SetAsWideStr(handle: TCHandle; Value: WideString);
begin
  AsAnsiStr[handle] := AnsiString(value);
end;

function TTFPoint.LoadItem(c: TCCustom; h: TCHandle; src: TStream; FileFormat : TFileFormat = ffDefault):TCHandle;
var r: TFPointType;
begin
  if FileFormat=ffBinary then
    src.Read(r, sizeof(r))
  else
    with r do
    begin
      x := StrToFloat(c.LoadToken);
      y := StrToFloat(c.LoadToken);
    end;
  if h=-1 then
    result := Add(r)
  else
  begin
    result := h;
    items[h] := r;
  end;
end;

procedure TTFPoint.InitContainer(c: TCCustom);
begin
  inherited;
  c.FCompareValue := CmpInt;
end;

function CompareDouble(var a,b: double): integer;
begin
  if a<b then
    result := -1
  else
    if a>b then
      result := 1
    else
      result := 0;
end;

function TTFPoint.CmpInt(var a,b):integer;
begin
  with TFPointType(a) do
    if X<TFPointType(b).X then
      result := -1
    else
      if X>TFPointType(b).X then
        result := 1
      else
        if Y<TFPointType(b).Y then
          result := -1
        else
          if Y>TFPointType(b).Y then
            result := 1
          else
            result := 0;
end;

procedure TTFPoint.SetOnCompare(c: TOnFPointCompare);
begin
  if assigned(c) then
    container.FCompareValue := CmpExt
  else
    container.FCompareValue := CmpInt;
  FOnCompare := c;
  FOnCompareProc := nil;
end;

procedure TTFPoint.SetOnCompareProc(c: TOnFPointCompareProc);
begin
  if assigned(c) then
    container.FCompareValue := CmpExtProc
  else
    container.FCompareValue := CmpInt;
  FOnCompareProc := c;
  FOnCompare := nil;
end;

function TTFPoint.CmpExt(var a, b): integer;
begin
  Result := FOnCompare(TFPointType(a), TFPointType(b));
end;

function TTFPoint.CmpExtProc(var a, b): integer;
begin
  Result := FOnCompareProc(TFPointType(a), TFPointType(b));
end;

procedure TTFPoint.Sort(compare: TOnFPointCompare);
begin
  Sort(compare, FContainer.First, FContainer.Last);
end;

procedure TTFPoint.Sort(compare: TOnFPointCompareProc);
begin
  Sort(compare, FContainer.First, FContainer.Last);
end;

procedure TTFPoint.Sort(compare: TOnFPointCompare; AFirst, ALast: TCHandle);
begin
  FOnCompare := compare;
  FContainer.Sort(CmpExt, AFirst, ALast);
end;

procedure TTFPoint.Sort(compare: TOnFPointCompareProc; AFirst, ALast: TCHandle);
begin
  FOnCompareProc := compare;
  FContainer.Sort(CmpExtProc, AFirst, ALast);
end;

function TTFPoint.STCopy(Dst: TTCustom; h: TCHandle):TCHandle;
begin
  result := TTFPoint(dst).Add(Items[h]);
end;

procedure TTFPoint.STCopy(Dst: TTCustom; AFirst, ALast: TCHandle);
begin
  TTFPoint(dst).Add(Items[AFirst]);
  if AFirst<>ALast then
    repeat
      FContainer.Next(AFirst);
      TTFPoint(dst).Add(Items[AFirst]);
    until AFirst=ALast;
end;

procedure TTFPoint.STSet(Dst: TTCustom; h1,h2: TCHandle);
begin
  TTFPoint(dst)[h2] := Items[h1];
end;

procedure TTFPoint.STSet(Dst: TTCustom; start1,finish1, start2: TCHandle);
begin
  TTFPoint(dst)[start2] := Items[start1];
  if start1<>finish1 then
    repeat
      FContainer.Next(start1);
      dst.FContainer.Next(start2);
      TTFPoint(dst)[start2] := Items[start1];
    until start1=finish1;
end;

function TTFPoint.GetMap(key: TFPointType): variant;
begin
  Result := FContainer.Fields[1].AsVariant[Find(key)];
end;

procedure TTFPoint.PutMap(key: TFPointType; Value: variant);
var
  h: TCHandle;
begin
  h := Find(key);
  if h = -1 then
    AddPair(key, Value)
  else
    FContainer.Fields[1].AsVariant[h] := Value;
end;

function TTFPoint.Add(Item: TFPointType): TCHandle;
begin
  Result := FContainer.AddItem(item);
end;

function  TTFPoint.Front: TFPointType;          // examine first element
begin
  result := items[FContainer.First];
end;

function  TTFPoint.Back: TFPointType;           // examine last element
begin
  result := items[FContainer.Last];
end;

function TTFPoint.Push(v: TFPointType): TCHandle;        // insert element at front
begin
  result := FContainer.InsertItem(FContainer.First, v);
end;

function  TTFPoint.Pop: TFPointType;            // remove first element
var
  h: TCHandle;
begin
  h := FContainer.First;
  Result := items[h];
  FContainer.Delete(h);
end;

function TTFPoint.PushBack(v: TFPointType): TCHandle;    // insert element at back
begin
  result := FContainer.AddItem(v);
end;

function  TTFPoint.PopBack: TFPointType;        // remove last element
var
  h: TCHandle;
begin
  h := FContainer.Last;
  result := items[h];
  FContainer.Delete(h);
end;

function TTFPoint.Enqueue(v: TFPointType): TCHandle;    // add to tail
begin
  result := FContainer.AddItem(v);
end;

function  TTFPoint.Dequeue: TFPointType;       // read&remove from head
var
  h: TCHandle;
begin
  h := FContainer.First;
  Result := items[h];
  FContainer.Delete(h);
end;

function TTFPoint.AddTo(handle: TCHandle; dst: TTCustom): TCHandle;
begin
  with items[handle] do
    Result := dst.Add([x, y]);
end;

function TTFPoint.AddList(const AItems: array of const): TCHandle;
var
  i: integer;
  p: PVarRec;
  t: ^TFPoint;
begin
  Result := -1;
  for i := low(aitems) to high(aitems) do
  begin
    p := @aitems[i];
    if p.VType = vtObject then
      Result := ImportObject(p.VObject, Result)
    else
    if FComplete = 0 then
    begin
      FComplete := 1;
      FPos := add(FPoint(VarAsDouble(p), 0));
      Result := FPos;
    end
    else
    begin
      t := FContainer.Get(FPos);
      t.Y := VarAsDouble(p);
      FComplete := 0;
      Result := FPos;
    end;
  end;
end;

function TTFPoint.Find(Item: TFPointType): TCHandle;
begin
  Result := FContainer.Find(item);
end;

function TTFPoint.FindFirstEqual(Item: TFPointType): TCHandle;
begin
  Result := FContainer.FindFirstEqual(item);
end;

procedure TTFPoint.FindNextEqual(Item: TFPointType; var handle: TCHandle);
begin
  FContainer.FindNextEqual(item, handle);
end;

function TTFPoint.GetItem(n: TCHandle): TFPointType;
begin
  Result := TFPointType(FContainer.Get(n)^);
end;

function TTFPoint.GetMinValue: TFPointType;
begin
  Result := TFPointType(FContainer.Get(FContainer.FindMin)^);
end;

function TTFPoint.RemoveMinValue: TFPointType;
var
  n: TCHandle;
begin
  n := FContainer.FindMin;
  Result := TFPointType(FContainer.Get(n)^);
  FContainer.Delete(n);
end;

function TTFPoint.GetMaxValue: TFPointType;
begin
  Result := TFPointType(FContainer.Get(FContainer.FindMax)^);
end;

function TTFPoint.RemoveMaxValue: TFPointType;
var
  n: TCHandle;
begin
  n := FContainer.FindMax;
  Result := TFPointType(FContainer.Get(n)^);
  FContainer.Delete(n);
end;

function TTFPoint.AddPair(key: TFPointType; Value: variant): TCHandle;
begin
  result := Add(key);
  Container.Fields[1].AsVariant[result] := Value;
end;

function TTFPoint.Insert(handle: TCHandle; Item: TFPointType): TCHandle;
begin
  result := FContainer.InsertItem(handle, item);
end;

function TTFPoint.InsertAfter(handle: TCHandle; Item: TFPointType): TCHandle;
begin
  result := FContainer.InsertAfterItem(handle, item);
end;

procedure TTFPoint.Remove(Item: TFPointType);
begin
  FContainer.Remove(item);
end;

procedure TTFPoint.RemoveAll(Item: TFPointType);
begin
  FContainer.RemoveAll(item);
end;

procedure TTFPoint.SetItem(n: TCHandle; Value: TFPointType);
begin
  FContainer.Put(n, Value);
end;

{ --------------------------------------- TTPointer ---------------------------}

procedure SortMemoryRangePointer(Buf: pointer; L, R: Integer);
begin
  {$IFDEF FPC}
  if R>L then
    VectorIntegerQuickSort(@PIntegerList(Buf)[L], R-L+1);
  {$ELSE}
  if R>L then
    VectorIntegerJumpSort(@PIntegerList(Buf)[L], R-L+1);
  {$ENDIF}
end;

constructor TTPointer.Create(Container: TCCustom = nil);
begin
  FieldSize := sizeof(TPointerType);
  FSortMemoryRange := @SortMemoryRangePointer;
  inherited;
end;

function TTPointer.Add(Item: TPointerType): TCHandle;
begin
  Result := FContainer.AddItem(item);
end;

function  TTPointer.Front: TPointerType;          // examine first element
begin
  result := items[FContainer.First];
end;

function  TTPointer.Back: TPointerType;           // examine last element
begin
  result := items[FContainer.Last];
end;

function TTPointer.Push(v: TPointerType): TCHandle;        // insert element at front
begin
  result := FContainer.InsertItem(FContainer.First, v);
end;

function  TTPointer.Pop: TPointerType;            // remove first element
var
  h: TCHandle;
begin
  h := FContainer.First;
  Result := items[h];
  FContainer.Delete(h);
end;

function TTPointer.PushBack(v: TPointerType): TCHandle;    // insert element at back
begin
  result := FContainer.AddItem(v);
end;

function  TTPointer.PopBack: TPointerType;        // remove last element
var
  h: TCHandle;
begin
  h := FContainer.Last;
  result := items[h];
  FContainer.Delete(h);
end;

function TTPointer.Enqueue(v: TPointerType): TCHandle;    // add to tail
begin
  result := FContainer.AddItem(v);
end;

function  TTPointer.Dequeue: TPointerType;       // read&remove from head
var
  h: TCHandle;
begin
  h := FContainer.First;
  Result := items[h];
  FContainer.Delete(h);
end;

function TTPointer.AddTo(handle: TCHandle; dst: TTCustom): TCHandle;
begin
  Result := dst.Add([items[handle]]);
end;

function TTPointer.GetAsVariant(handle: TCHandle): variant;
begin
  if handle=-1 then
    Result := null
  else
    Result := integer(items[handle]);
end;

procedure TTPointer.SetAsVariant(handle: TCHandle; Value: variant);
begin
  items[handle] := pointer(integer(Value));
end;

function TTPointer.GetAsAnsiStr(handle: TCHandle): AnsiString;
begin
  setlength(result, sizeof(pointer)*2);
  BinToHex(Items[handle], PAnsiChar(result), sizeof(pointer));
end;

procedure TTPointer.SetAsAnsiStr(handle: TCHandle; Value: AnsiString);
var p: pointer;
begin
  HexToBin(PAnsiChar(value), @p, sizeof(pointer));
  items[handle] := p;
end;

function TTPointer.GetAsWideStr(handle: TCHandle): WideString;
begin
  result := WideString(AsAnsiStr[handle]);
end;

procedure TTPointer.SetAsWideStr(handle: TCHandle; Value: WideString);
begin
  AsAnsiStr[handle] := AnsiString(Value);
end;

function TTPointer.GetHandleOf(Value: variant): TCHandle;
begin
  Result := Find(pointer(integer(Value)));
end;

function TTPointer.AddList(const Items: array of const): TCHandle;
var
  i: integer;
  p: PVarRec;
begin
  Result := -1;
  for i := low(items) to high(items) do
  begin
    p := @items[i];
    if p.VType = vtObject then
      Result := ImportObject(p.VObject, Result)
    else
      Result := add(VarAsPointer(p));
  end;
end;

function TTPointer.Find(Item: TPointerType): TCHandle;
begin
  Result := FContainer.Find(item);
end;

function TTPointer.FindFirstEqual(Item: TPointerType): TCHandle;
begin
  Result := FContainer.FindFirstEqual(item);
end;

procedure TTPointer.FindNextEqual(Item: TPointerType; var handle: TCHandle);
begin
  FContainer.FindNextEqual(item, handle);
end;

function TTPointer.GetItem(n: TCHandle): TPointerType;
begin
  Result := TPointerType(FContainer.Get(n)^);
end;

function TTPointer.GetMinValue: TPointerType;
begin
  Result := TPointerType(FContainer.Get(FContainer.FindMin)^);
end;

function TTPointer.RemoveMinValue: TPointerType;
var
  n: TCHandle;
begin
  n := FContainer.FindMin;
  Result := TPointerType(FContainer.Get(n)^);
  FContainer.Delete(n);
end;

function TTPointer.Insert(handle: TCHandle; Item: TPointerType): TCHandle;
begin
  result := FContainer.InsertItem(handle, item);
end;

function TTPointer.InsertAfter(handle: TCHandle; Item: TPointerType): TCHandle;
begin
  result := FContainer.InsertAfterItem(handle, item);
end;

procedure TTPointer.Remove(Item: TPointerType);
begin
  FContainer.Remove(item);
end;

procedure TTPointer.SetItem(n: TCHandle; Value: TPointerType);
begin
  FContainer.Put(n, Value);
end;

{ --------------------------------------- TTRecord ---------------------------}

constructor TTRecord.Create(AItemSize: integer; ACompare: TOnRecordCompare; Container: TCCustom = nil);
begin
  FieldSize  := AItemSize;
  reallocmem(FTemp, AItemSize);
  inherited Create(Container);
  OnCompare := ACompare;
end;

constructor TTRecord.Create(AItemSize: integer; Container: TCCustom = nil);
begin
  FieldSize := AItemSize;
  reallocmem(FTemp, AItemSize);
  inherited Create(Container);
end;

{
  Sometimes TTRecord does not have FieldSize when constructing because user set
  ItemSize later. We must change itemsize of container item and move all
  following fields in such case.
}
procedure TTRecord.SetFieldSize(ASize: integer);
var
  i,j: integer;
  m: TCCustom;
begin
  assert(FFieldSize=0, erOperation);
  inherited SetFieldSize(ASize);
  reallocmem(FTemp, FFieldSize);
  m := FContainer.GetMainContainer;
  FContainer.FieldSize := ASize;
  // TC_Join will invoke Parent.ItemSize := Parent.ItemSize + ASize
  FContainer.ItemSize := ASize;
  j := high(integer);
  for i := 0 to length(m.FFields)-1 do
    if m.FFields[i]=self then
      j := i
    else
      if i>j then
        with m.FFields[i].FContainer as T_CJoin do
          FOffset := FOffset+ASize;
  if not assigned(FOnCompare) and not assigned(FOnCompareProc) then
    SetCmp;
end;

destructor TTRecord.Destroy;
begin
  reallocmem(FTemp, 0);
  inherited Destroy;
end;

function TTRecord.Add(var Item): TCHandle;
begin
  Result := FContainer.AddItem(item);
end;

procedure TTRecord.Front(var dst);          // examine first element
begin
  with FContainer do
    FMoveField(Items[First]^, dst, FieldSize);
end;

procedure TTRecord.Back(var dst);           // examine last element
begin
  with FContainer do
    FMoveField(Items[Last]^, dst, FieldSize);
end;

function TTRecord.Push(var Item): TCHandle;          // insert element at front
begin
  result := FContainer.InsertItem(FContainer.First, Item);
end;

procedure TTRecord.Pop(var dst);            // remove first element
var
  h: TCHandle;
begin
  with FContainer do
  begin
    h := First;
    FMoveField(Items[h]^, dst, FieldSize);
    Delete(h);
  end;
end;

function TTRecord.PushBack(var Item): TCHandle;      // insert element at back
begin
  result := FContainer.AddItem(Item);
end;

procedure TTRecord.PopBack(var dst);        // remove last element
var
  h: TCHandle;
begin
  with FContainer do
  begin
    h := Last;
    FMoveField(Items[h]^, dst, FieldSize);
    Delete(h);
  end;
end;

function TTRecord.Enqueue(var Item): TCHandle;      // add to tail
begin
  result := FContainer.AddItem(Item);
end;

procedure TTRecord.Dequeue(var dst);       // read&remove from head
var
  h: TCHandle;
begin
  with FContainer do
  begin
    h := First;
    FMoveField(Items[h]^, dst, FieldSize);
    Delete(h);
  end;
end;

function TTRecord.AddTo(handle: TCHandle; dst: TTCustom): TCHandle;
var
  p: ^byte;
  i: integer;
begin
  Result := -1;
  p := items[handle];
  for i := 0 to FieldSize - 1 do
  begin
    Result := dst.Add([p^]);
    Inc(p);
  end;
end;

function TTRecord.AddList(const AItems: array of const): TCHandle;
var
  i: integer;
  p: PVarRec;
  t: PByte;
begin
  Result := -1;
  for i := low(AItems) to high(AItems) do
  begin
    p := @AItems[i];
    if p.VType = vtObject then
      Result := ImportObject(p.VObject, Result)
    else
    begin
      if FComplete = 0 then
        FPos := add(FTemp^);
      t := @PByteArray(FContainer.Get(FPos))[FComplete];
      t^ := VarAsInteger(p);
      Inc(FComplete);
      if FComplete >= FieldSize then
        FComplete := 0;
      Result := FPos;
    end;
  end;
end;

function TTRecord.GetAsVariant(handle: TCHandle): variant;
var
  i: integer;
  p: PByteArray;
begin
  if handle=-1 then
    Result := null
  else
  begin
    result := VarArrayCreate([0,FieldSize-1], varInteger);
    p := items[handle];
    for i := 0 to FieldSize-1 do
      result[i] := p[i];
  end;
end;

procedure TTRecord.SetAsVariant(handle: TCHandle; Value: variant);
var
  i: integer;
  p: PByteArray;
begin
  p := items[handle];
  for i := 0 to FieldSize-1 do
    p[i] := value[i];
  container.put(handle, p^);
end;

function TTRecord.GetAsAnsiStr(handle: TCHandle): AnsiString;
begin
  setlength(result, RecordSize*2);
  BinToHex(Items[handle], PAnsiChar(result), RecordSize);
end;

procedure TTRecord.SetAsAnsiStr(handle: TCHandle; Value: AnsiString);
begin
  HexToBin(PAnsiChar(value), pointer(FTemp), RecordSize);
  Items[handle] := FTemp;
end;

function TTRecord.GetAsWideStr(handle: TCHandle): WideString;
begin
  result := WideString(AsAnsiStr[handle]);
end;

procedure TTRecord.SetAsWideStr(handle: TCHandle; Value: WideString);
begin
  AsAnsiStr[handle] := AnsiString(value);
end;

function TTRecord.GetHandleOf(Value: variant): TCHandle;
var
  r: array of byte;
begin
  r := Value;
  if length(r) <> FieldSize then
    raise Exception.Create('')
  else
    Result := Find(r[0]);
end;

procedure TTRecord.Get(handle: TCHandle; var Item);
begin
  with FContainer do
    FMoveField(Items[handle]^, Item, FieldSize);
end;

function TTRecord.Find(var Item): TCHandle;
begin
  Result := FContainer.Find(item);
end;

function TTRecord.FindFirstEqual(var Item): TCHandle;
begin
  Result := FContainer.FindFirstEqual(item);
end;

procedure TTRecord.FindNextEqual(var Item; var handle: TCHandle);
begin
  FContainer.FindNextEqual(item, handle);
end;

function TTRecord.GetItem(n: TCHandle): pointer;
begin
  Result := FContainer[n];
end;

procedure TTRecord.GetMinValue(var dst);
begin
  with FContainer do
    FMoveField(Items[FindMin]^, dst, FieldSize);
end;

procedure TTRecord.RemoveMinValue(var dst);
var
  i: TCHandle;
begin
  with FContainer do
  begin
    i := FindMin;
    FMoveField(Items[i]^, dst, FieldSize);
    Delete(i);
  end;
end;

procedure TTRecord.GetMaxValue(var dst);
begin
  with FContainer do
    FMoveField(Items[FindMax]^, dst, FieldSize);
end;

procedure TTRecord.RemoveMaxValue(var dst);
var
  i: TCHandle;
begin
  with FContainer do
  begin
    i := FindMax;
    FMoveField(Items[i]^, dst, FieldSize);
    Delete(i);
  end;
end;

function TTRecord.Insert(handle: TCHandle; var Item): TCHandle;
begin
  Result := FContainer.InsertItem(handle, item);
end;

function TTRecord.InsertAfter(handle: TCHandle; var Item): TCHandle;
begin
  Result := FContainer.InsertAfterItem(handle, item);
end;

procedure TTRecord.Remove(var Item);
begin
  FContainer.Remove(item);
end;

procedure TTRecord.RemoveAll(var Item);
begin
  FContainer.RemoveAll(item);
end;

procedure TTRecord.SetItem(n: TCHandle; Value: pointer);
begin
  FContainer.Put(n, Value^);
end;

{ TTBit }

constructor TTBit.Create(Container: TCCustom = nil);
begin
  FieldSize := sizeof(integer);
  inherited;
end;

function TTBit.Add(Item: TBitType): TCHandle;
var
  b: integer;
begin
  if FBitCount mod 32 = 0 then
  begin
    b := 0;
    FContainer.AddItem(b);
  end;
  Result := FBitCount;
  Inc(FBitCount);
  items[Result] := item;
end;

function TTBit.AddList(const Items: array of const): TCHandle;
var
  i: integer;
  p: PVarRec;
begin
  Result := -1;
  for i := low(items) to high(items) do
  begin
    p := @items[i];
    if p.VType = vtObject then
      Result := ImportObject(p.VObject, Result)
    else
      Result := add(VarAsBoolean(p));
  end;
end;

function TTBit.GetAsVariant(handle: TCHandle): variant;
begin
  if handle=-1 then
    Result := null
  else
    Result := items[handle];
end;

procedure TTBit.SetAsVariant(handle: TCHandle; Value: variant);
begin
  items[handle] := Value;
end;

function TTBit.GetHandleOf(Value: variant): TCHandle;
begin
  raise Exception.Create('');
  Result := -1;
end;

function TTBit.AddTo(handle: TCHandle; dst: TTCustom): TCHandle;
begin
  Result := dst.Add([items[handle]]);
end;

function TTBit.GetCapacity: integer;
begin
  Result := FContainer.Capacity * 32;
end;

function TTBit.GetCount: integer;
begin
  // let's return number of items instead of byte number (we pack 8 boolean values as bits inside of byte)
  Result := FBitCount;
end;

procedure TTBit.SetCapacity(n: integer);
begin
  FContainer.Capacity := (n + 31) div 32;
end;

procedure TTBit.SetCount(n: integer);
var
  j: integer;
begin
  FContainer.Count := (n + 31) div 32;
  if (n < FBitCount) and assigned(FOnDeleted) then
    for j := FBitCount - 1 downto n do
      ItemDeleted(j);
  FBitCount := n;
end;

function TTBit.GetItem(n: TCHandle): TBitType;
var
  p: ^dword;
begin
  p := FContainer.Get(n div 32);
  Result := p^ and (1 shl (n mod 32)) <> 0;
end;

procedure TTBit.SetItem(n: TCHandle; Value: TBitType);
var
  p: ^dword;
begin
  p := FContainer.Get(n div 32);
  if Value then
    p^ := p^ or (1 shl (n mod 32))
  else
    p^ := p^ and not byte(1 shl (n mod 32));
end;

function T_HashRnd.Get(var buf; count: integer): cardinal;
var
  c: ^cardinal;
  b: ^byte;
  i: integer;
  n: cardinal;
begin
  c := @buf;
  result := 0;
  n := 0;
  for i := 0 to count div 4-1 do
  begin
    n := n*1664525 + 1013904223;
    result := result xor n + c^;
    inc(c);
  end;
  b := pointer(c);
  for i := 0 to count mod 4-1 do
  begin
    n := n*1664525 + 1013904223;
    result := result xor n + b^;
    inc(b);
  end;
end;

{ TTHandle }

constructor TTHandle.Create(Container: TCCustom = nil);
begin
  inc(FNeedOnDelete);
  inherited;
end;

procedure TTHandle.ItemDeleted(n: TCHandle);
var
  h: DWord;
begin
  h := Items[n];
  if h <> 0 then
    CloseHandle(h);
  inherited;
end;

{ TTHIcon }

constructor TTHIcon.Create(Container: TCCustom = nil);
begin
  inc(FNeedOnDelete);
  inherited;
end;

procedure TTHIcon.ItemDeleted(n: TCHandle);
var
  h: DWord;
begin
  h := Items[n];
  if h <> 0 then
    DestroyIcon(h);
  inherited;
end;

{ TTObject }

constructor TTObject.Create(Container: TCCustom = nil);
begin
  inc(FNeedOnDelete);
  inherited;
end;

procedure TTObject.ItemDeleted(n: TCHandle);
begin
  TObject(Items[n]).Free;
  inherited;
end;

function PShortString2Str(p: PShortString): string;
begin
  if p = nil then
    Result := ''
  else
    Result := String(p^);
end;

function PAnsiChar2Str(p: PAnsiChar): string;
begin
  if p = nil then
    Result := ''
  else
    Result := String(p);
end;

function PWideChar2Str(p: PWideChar): string;
begin
  if p = nil then
    Result := ''
  else
    Result := p;
end;

// useful to convert AnsiChar -> Char, ...
function StrToChar(s: string): char;
begin
  if s = '' then
    Result := #0
  else
    Result := s[1];
end;

function StrToBoolean(s: string): boolean;
begin
  Result := (s = '1') or sametext(s, 'true');
end;

function StrToWideChar(s: WideString): widechar;
begin
  if s = '' then
    Result := #0
  else
    Result := s[1];
end;

function VarAsInteger(p: PVarRec): integer;
begin
  Result := VarAsInt64(p);
end;

function VarAsBoolean(p: PVarRec): boolean;
begin
  case p.VType of
    vtInteger: Result := p.VInteger <> 0;
    vtBoolean: Result := p.VBoolean;
    vtChar: Result  := p.VChar = '1';
    vtExtended: Result := p.VExtended^ <> 0;
    vtString: Result := StrToBoolean(PShortString2Str(p.VString));
    vtPointer: Result := p.VPointer <> nil;
    vtPChar: Result := StrToBoolean(PAnsiChar2Str(p.VPChar));
    vtObject: Result := p.VObject <> nil;
    vtClass: Result := p.VClass <> nil;
    vtWideChar: Result := p.VWideChar = '1';
    vtPWideChar: Result := StrToBoolean(PWideChar2Str(p.VPWideChar));
    vtAnsiString: Result := StrToBoolean(String(AnsiString(p.VAnsiString)));
    vtCurrency: Result := p.VCurrency^ <> 0;
    vtVariant: Result := p.VVariant^;
    vtInterface: Result := p.VInterface <> nil;
    vtWideString: Result := StrToBoolean(WideString(p.VWideString));
    vtInt64: Result := p.VInt64^ <> 0;
{$IFDEF HiCompiler}
    vtUnicodeString: result := StrToBoolean(UnicodeString(p.VUnicodeString));
{$ENDIF}
    else
      raise Exception.Create('');
  end;
end;

function VarAsChar(p: PVarRec): char;
var
  s: string;
begin
  s := VarAsString(p);
  if s = '' then
    Result := #0
  else
    Result := s[1];
end;

function VarAsAnsiChar(p: PVarRec): AnsiChar;
var
  s: AnsiString;
begin
  s := VarAsAnsiString(p);
  if s = '' then
    Result := #0
  else
    Result := s[1];
end;

function VarAsExtended(p: PVarRec): extended;
begin
  case p.VType of
    vtInteger: Result := p.VInteger;
    vtBoolean: if p.VBoolean then
        Result := 1
      else
        Result := 0;
    vtChar: Result := byte(p.VChar);
    vtExtended: Result := p.vExtended^;
    vtString: Result := StrToFloat(PShortString2Str(p.VString));
    vtPointer: Result := integer(p.VPointer);
    vtPChar: Result  := StrToFloat(PAnsiChar2Str(p.VPChar));
    vtObject: Result := integer(p.VObject);
    vtClass: Result  := integer(p.VClass);
    vtWideChar: Result := integer(p.VWideChar);
    vtPWideChar: Result := StrToFloat(PWideChar2Str(p.VPWideChar));
    vtAnsiString: Result := StrToFloat(String(AnsiString(p.VAnsiString)));
    vtCurrency: Result := p.VCurrency^;
    vtVariant: Result := p.VVariant^;
    vtInterface: raise Exception.Create('');
    vtWideString: Result := StrToFloat(WideString(p.VWideString));
    vtInt64: Result := p.VInt64^;
{$IFDEF HiCompiler}
    vtUnicodeString: result := StrToFloat(UnicodeString(p.VUnicodeString));
{$ENDIF}
    else
      raise Exception.Create('');
  end;
end;

function VarAsDouble(p: PVarRec): double;
begin
  case p.VType of
    vtInteger: Result := p.VInteger;
    vtBoolean: if p.VBoolean then
        Result := 1
      else
        Result := 0;
    vtChar: Result := byte(p.VChar);
    vtExtended: Result := p.vExtended^;
    vtString: Result := StrToFloat(PShortString2Str(p.VString));
    vtPointer: Result := integer(p.VPointer);
    vtPChar: Result  := StrToFloat(PAnsiChar2Str(p.VPChar));
    vtObject: Result := integer(p.VObject);
    vtClass: Result  := integer(p.VClass);
    vtWideChar: Result := integer(p.VWideChar);
    vtPWideChar: Result := StrToFloat(PWideChar2Str(p.VPWideChar));
    vtAnsiString: Result := StrToFloat(String(AnsiString(p.VAnsiString)));
    vtCurrency: Result := p.VCurrency^;
    vtVariant: Result := p.VVariant^;
    vtInterface: raise Exception.Create('');
    vtWideString: Result := StrToFloat(WideString(p.VWideString));
    vtInt64: Result := p.VInt64^;
{$IFDEF HiCompiler}
    vtUnicodeString: result := StrToFloat(UnicodeString(p.VUnicodeString));
{$ENDIF}
    else
      raise Exception.Create('');
  end;
end;

function VarAsString(p: PVarRec): string;
begin
  case p.VType of
    vtInteger: Result := IntToStr(p.VInteger);
    vtBoolean: if p.VBoolean then
        Result := '1'
      else
        Result := '0';
    vtChar: Result := Char(p.VChar);
    vtExtended: Result := FloatToStr(p.vExtended^);
    vtString: Result := PShortString2Str(p.VString);
    vtPointer: Result := IntToStr(integer(p.VPointer));
    vtPChar: Result  := PAnsiChar2Str(p.VPChar);
    vtObject: Result := IntToStr(integer(p.VObject));
    vtClass: Result  := IntToStr(integer(p.VClass));
    vtWideChar: Result := p.VWideChar;
    vtPWideChar: Result := PWideChar2Str(p.VPWideChar);
    vtAnsiString: Result := String(AnsiString(p.VAnsiString));
    vtCurrency: Result := FloatToStr(p.VCurrency^);
    vtVariant: Result := p.VVariant^;
    vtInterface: raise Exception.Create('');
    vtWideString: Result := WideString(p.VWideString);
    vtInt64: Result := IntToStr(p.VInt64^);
{$IFDEF HiCompiler}
    vtUnicodeString: result := UnicodeString(p.VUnicodeString);
{$ENDIF}
    else
      raise Exception.Create('');
  end;
end;

function VarAsPointer(p: PVarRec): pointer;
begin
  case p.VType of
    vtInteger: Result := pointer(p.VInteger);
    vtBoolean: raise Exception.Create('');
    vtChar: raise Exception.Create('');
    vtExtended: raise Exception.Create('');
    vtString: Result := Pointer(StrToInt(PShortString2Str(p.VString)));
    vtPointer: Result := p.VPointer;
    vtPChar: Result  := pointer(StrToInt(PAnsiChar2Str(p.VPChar)));
    vtObject: Result := p.VObject;
    vtClass: Result  := p.VClass;
    vtWideChar: raise Exception.Create('');
    vtPWideChar: Result  := pointer(StrToInt(PWideChar2Str(p.VPWideChar)));
    vtAnsiString: Result := pointer(StrToInt(String(AnsiString(p.VAnsiString))));
    vtCurrency: raise Exception.Create('');
    vtVariant: Result := p.VVariant;
    vtInterface: raise Exception.Create('');
    vtWideString: Result := pointer(StrToInt(WideString(p.VWideString)));
    vtInt64: Result := pointer(integer(p.VInt64^));
{$IFDEF HiCompiler}
    vtUnicodeString: result := pointer(StrToInt(UnicodeString(p.VUnicodeString)));
{$ENDIF}
    else
      raise Exception.Create('');
  end;
end;

function VarAsPChar(p: PVarRec): PChar;
begin
  Result := VarAsPointer(p);
end;

function VarAsObject(p: PVarRec): TObject;
begin
  Result := VarAsPointer(p);
end;

function VarAsClass(p: PVarRec): TClass;
begin
  Result := TClass(VarAsPointer(p));
end;

function VarAsWideChar(p: PVarRec): widechar;
begin
  case p.VType of
    vtInteger: Result := widechar(p.VInteger);
    vtBoolean: if p.VBoolean then
        Result := '1'
      else
        Result := '0';
    vtChar: Result := WideString(p.VChar)[1];
    vtExtended: raise Exception.Create('');
    vtString: Result := StrToWideChar(PShortString2Str(p.VString));
    vtPointer: raise Exception.Create('');
    vtPChar: Result := StrToWideChar(PAnsiChar2Str(p.VPChar));
    vtObject: raise Exception.Create('');
    vtClass: raise Exception.Create('');
    vtWideChar: Result := p.VWideChar;
    vtPWideChar: Result := StrToWideChar(PWideChar2Str(p.VPWideChar));
    vtAnsiString: Result := StrToWideChar(String(AnsiString(p.VAnsiString)));
    vtCurrency: Result := widechar(round(p.VCurrency^));
    vtVariant: Result := StrToWideChar(p.VVariant^);
    vtInterface: raise Exception.Create('');
    vtWideString: Result := StrToWideChar(WideString(p.VWideString));
    vtInt64: Result := widechar(p.VInt64^);
{$IFDEF HiCompiler}
    vtUnicodeString: result := StrToWideChar(UnicodeString(p.VUnicodeString));
{$ENDIF}
    else
      raise Exception.Create('');
  end;
end;

function VarAsPWideChar(p: PVarRec): PWideChar;
begin
  Result := VarAsPointer(p);
end;

function VarAsAnsiString(p: PVarRec): AnsiString;
begin
  Result := AnsiString(VarAsString(p));
end;

function VarAsCurrency(p: PVarRec): currency;
begin
  if p.VType = vtCurrency then
    Result := p.vCurrency^
  else
    Result := VarAsExtended(p);
end;

function VarAsVariant(p: PVarRec): variant;
begin
  case p.VType of
    vtInteger: Result := p.VInteger;
    vtBoolean: Result := p.VBoolean;
    vtChar: Result  := p.VChar;
    vtExtended: Result := p.vExtended^;
    vtString: Result := p.VString^;
    vtPointer: Result := integer(p.VPointer);
    vtPChar: Result := PAnsiChar2Str(p.VPChar);
    vtObject: Result := integer(p.VObject);
    vtClass: Result := integer(p.VClass);
    vtWideChar: Result := p.VWideChar;
    vtPWideChar: Result := PWideChar2Str(p.VPWideChar);
    vtAnsiString: Result := ansistring(p.VAnsiString);
    vtCurrency: Result := p.VCurrency^;
    vtVariant: Result := p.VVariant^;
    vtInterface: Result := IInterface(p.VInterface);
    vtWideString: Result := WideString(p.VWideString);
    vtInt64: Result := p.VInt64^;
{$IFDEF HiCompiler}
    vtUnicodeString: result := UnicodeString(p.VUnicodeString);
{$ENDIF}
    else
      raise Exception.Create('');
  end;
end;

function VarAsInterface(p: PVarRec): variant;
begin
  if p.VType = vtInterface then
    Result := IInterface(p.VInterface)
  else
    raise Exception.Create('');
end;

function VarAsWideString(p: PVarRec): WideString;
begin
  case p.VType of
    vtInteger: Result := IntToStr(p.VInteger);
    vtBoolean: if p.VBoolean then
        Result := '1'
      else
        Result := '0';
    vtChar: Result := WideChar(p.VChar);
    vtExtended: Result := FloatToStr(p.vExtended^);
    vtString: Result := PShortString2Str(p.VString);
    vtPointer: Result := IntToStr(integer(p.VPointer));
    vtPChar: Result  := PAnsiChar2Str(p.VPChar);
    vtObject: Result := IntToStr(integer(p.VObject));
    vtClass: Result  := IntToStr(integer(p.VClass));
    vtWideChar: Result := p.VWideChar;
    vtPWideChar: Result := p.VPWideChar;
    vtAnsiString: Result := WideString(AnsiString(p.VAnsiString));
    vtCurrency: Result := FloatToStr(p.VCurrency^);
    vtVariant: Result := p.VVariant^;
    vtInterface: raise Exception.Create('');
    vtWideString: Result := WideString(p.VWideString);
    vtInt64: Result := IntToStr(p.VInt64^);
{$IFDEF HiCompiler}
    vtUnicodeString: result := UnicodeString(p.VUnicodeString);
{$ENDIF}
    else
      raise Exception.Create('');
  end;
end;

function VarAsInt64(p: PVarRec): int64;
begin
  case p.VType of
    vtInteger: Result := p.VInteger;
    vtBoolean: Result := byte(p.VBoolean);
    vtChar: Result  := byte(p.VChar);
    vtExtended: Result := round(p.VExtended^);
    vtString: Result := StrToInt64(PShortString2Str(p.VString));
    vtPointer: Result := integer(p.VPointer);
    vtPChar: Result := StrToInt64(PAnsiChar2Str(p.VPChar));
    vtObject: Result := integer(p.VObject);
    vtClass: Result := integer(p.VClass);
    vtWideChar: Result := integer(p.VWideChar);
    vtPWideChar: Result := StrToInt64(PWideChar2Str(p.VPWideChar));
    vtAnsiString: Result := StrToInt64(String(AnsiString(p.VAnsiString)));
    vtCurrency: Result := round(p.VCurrency^);
    vtVariant: Result := p.VVariant^;
    vtInterface: raise Exception.Create('');
    vtWideString: Result := StrToInt64(WideString(p.VWideString));
    vtInt64: Result := p.VInt64^;
{$IFDEF HiCompiler}
    vtUnicodeString: result := StrToInt64(UnicodeString(p.VUnicodeString));
{$ENDIF}
    else
      raise Exception.Create('');
  end;
end;

{$IFDEF HiCompiler}
function VarAsUInt64(p: PVarRec): uint64;
begin
  case p.VType of
    vtInteger: Result := p.VInteger;
    vtBoolean: Result := byte(p.VBoolean);
    vtChar: Result  := byte(p.VChar);
    vtExtended: Result := round(p.VExtended^);
    vtString: Result := StrToInt64(PShortString2Str(p.VString));
    vtPointer: Result := integer(p.VPointer);
    vtPChar: Result := StrToInt64(PAnsiChar2Str(p.VPChar));
    vtObject: Result := integer(p.VObject);
    vtClass: Result := integer(p.VClass);
    vtWideChar: Result := integer(p.VWideChar);
    vtPWideChar: Result := StrToInt64(PWideChar2Str(p.VPWideChar));
    vtAnsiString: Result := StrToInt64(String(AnsiString(p.VAnsiString)));
    vtCurrency: Result := round(p.VCurrency^);
    vtVariant: Result := p.VVariant^;
    vtInterface: raise Exception.Create('');
    vtWideString: Result := StrToInt64(WideString(p.VWideString));
    vtInt64: Result := uint64(p.VInt64^);
    vtUnicodeString: result := StrToInt64(UnicodeString(p.VUnicodeString));
    else
      raise Exception.Create('');
  end;
end;
{$ENDIF}

{$IFDEF HiCompiler}
function VarAsUnicodeString(p: PVarRec): UnicodeString;
begin
  if p.VType=vtUnicodeString then
    result := UnicodeString(p.VUnicodeString)
  else
    result := VarAsWideString(p);
end;
{$ENDIF}

{ TCSList }

function TCSList.ListAdd: TCHandle;
var
  v: P_SSListItem;
begin
  v := AllocNewItem;
  Result := PtrTohandle(v);
  if FLast <> nil then
    ListInsertAfter(v, FLast)
  else
  begin
    FFirst := v;
    FLast  := v;
  end;
  Inc(FCount);
end;

function TCSList.AddItem(var item): TCHandle;
var
  v, pos: P_SSListItem;
begin
  // add item
  v := AllocNewItem;
  Result := PtrTohandle(v);

  if FSorted then
  begin
    if FFirst=nil then
    begin
      FFirst := v;
      FLast := v;
    end
    else
    begin
      pos := FFirst;
      if FCompareValue(item, pos.data)<=0 then
        ListInsertBefore(v, pos)
      else
      begin
        while pos.next<>nil do
          if FCompareValue(item, pos.Next.Data) <= 0 then
            break
          else
            pos := pos.Next;
        ListInsertAfter(v, pos);
      end;
    end;
  end
  else

    if FLast <> nil then
      ListInsertAfter(v, FLast)
    else
    begin
      FFirst := v;
      FLast  := v;
    end;

  Inc(FCount);
  // move data
  FMoveField(item, v.data, FFieldSize);
end;

function TCSList.AllocNewItem: P_SSListItem;
begin
  Result := AllocMem(sizeof(T_SSListItem) + FItemSize);
end;

procedure TCSList.Clear;
begin
  while FFirst <> nil do
  begin
    FLast  := FFirst;
    FFirst := FFirst.Next;
    if FDelCount>0 then
      ItemDeleted(PtrToHandle(FLast));
    ReleaseItem(FLast);
  end;
  FFirst := nil;
  FLast  := nil;
  FCount := 0;
end;

procedure TCSList.Copy(Src, Dst: TCHandle);
begin
  if FDelCount>0 then
    ItemDeleted(dst);
  if FCopyCount > 0 then
    CopyItem(src, dst)
  else
    FMoveItem(P_SSListItem(src).Data, P_SSListItem(dst).Data, FITemSize);
end;

constructor TCSList.Create;
begin
  inherited Create;
  InitZero := True;
end;

procedure TCSList.Delete(handle: TCHandle);
var
  p: P_SSListItem;
begin
  if FDelCount>0 then
    ItemDeleted(handle);
  p := P_SSListItem(handle);
  ListRemove(p);
  ReleaseITem(p);
  Dec(FCount);
end;

procedure TCSList.DeleteNext(handle: TCHandle);
// effective way to delete something for list of one-way linked items
var
  p, q: P_SSListItem;
begin
  p := P_SSListItem(handle);
  if p.Next = nil then
    exit;
  q := p.Next;
  if FDelCount>0 then
    ItemDeleted(PtrTohandle(q));
  p.Next := q.Next;
  if q = FLast then
    FLast := p;
  releaseItem(q);
  Dec(FCount);
end;

procedure TCSList.Remove(var Item);
var
  p: P_SSListItem;
begin
  p := FFirst;
  if p<>nil then
    if FCompareValue(item, p.data)=0 then
      Delete(PtrToHandle(p))
    else
      while p.Next<>nil do
        if FCompareValue(item, p.next.data)<>0 then
          p := p.Next
        else
        begin
          DeleteNext(PtrToHandle(p));
          break;
        end;
end;

procedure TCSList.RemoveAll(var Item);
var
  p: P_SSListItem;
begin
  while FFirst<>nil do
    if FCompareValue(Item, FFirst.data)=0 then
      Delete(PtrToHandle(FFirst))
    else
      break;
  if FFirst=nil then
    exit;
  p := FFirst;
  while p.Next<>nil do
    if FCompareValue(item, p.next.data)=0 then
      DeleteNext(PtrToHandle(p))
    else
      p := p.Next;
end;

procedure TCSList.Rotate(AFirst, ALast: TCHandle; shift: integer);
var
  PrevFirst, PrevP, p: P_SSListItem;
  i,w: integer;
begin
  assert((AFirst<>-1) and (ALast<>-1), erOperation);

  if AFirst=ALast then
    exit;
  // find W and make SHIFT positive and less than W
  w := 1;
  p := P_SSListItem(AFirst);
  while (p<>P_SSListItem(ALast)) and (p<>nil) do
  begin
    p := p.Next;
    inc(w);
  end;
  if p=nil then
  begin
    Rotate(ALast, AFirst, shift);
    exit;
  end;
  shift := shift mod w;
  if shift=0 then
    exit;
  if shift<0 then
    inc(shift, w);

  // number of left shifts = w-shift
  PrevFirst := ListGetPrev(P_SSListItem(AFirst));
  PrevP := P_SSListItem(AFirst);
  for i := 0 to w-shift-2 do
    PrevP := PrevP.Next;
  p := PrevP.Next;

  // prev(AFirst) -> P-ALast -> AFirst-prev(p) -> next(ALast)
  if PrevFirst=nil then
    FFirst := p
  else
    PrevFirst.Next := p;
  PrevP.Next := P_SSListItem(ALast).Next;
  P_SSListItem(ALast).Next := P_SSListItem(AFirst);
  if FLast=P_SSListItem(ALast) then
    FLast := PrevP;
end;

procedure TCSList.RandomShuffle(AFirst, ALast: TCHandle);
var
  f,l,p: P_SSListItem;
  i,w: integer;
  n: TCHandle;
  h: TTInteger;
begin
  assert((AFirst<>-1) and (ALast<>-1));
  if AFirst=ALast then
    exit;
  p := P_SSListItem(AFirst);
  w := 1;
  while (p<>nil) and (p<>P_SSListItem(ALast)) do
  begin
    inc(w);
    p := p.next;
  end;
  if p=nil then
  begin
    RandomShuffle(ALast, AFirst);
    exit;
  end;
  if w<=1 then
    exit;
  h := TTInteger.create;
  h.Capacity := w;
  n := AFirst;
  for i := 0 to w-1 do
    h.add(MoveNext(n));
  h.RandomShuffle;

  f := ListGetPrev(P_SSListItem(AFirst));
  l := P_SSListItem(ALast).Next;
  if f=nil then
    FFirst := P_SSListItem(h[0]);
  if l=nil then
    FLast := P_SSListItem(h[h.count-1]);
  for i := 0 to h.Count-1 do
  begin
    p := P_SSListItem(h[i]);
    if f<>nil then
      f.Next := p;
    f := p;
  end;
  f.Next := l;

  h.free;
end;

procedure TCSList.ListReverse(AFirst, ALast: P_SSListItem);
var
  list, temp, prev, first, next: P_SSListItem;
begin

  // prev -> A->B->...->C -> next
  prev := ListGetPrev(AFirst);
  first:= AFirst;
  next := ALast.Next;
  list := nil;
  repeat
    temp := AFirst;
    if AFirst=ALast then
      AFirst := nil
    else
      AFirst := AFirst.Next;
    temp.next := list;
    list := temp;
  until AFirst=nil;

  // prev -> C->...->B->A -> next
  if prev=nil then
    FFirst := list
  else
    prev.Next := list;

  // "first" has moved to the end of reverse range
  first.Next := next;
  if next=nil then
    FLast := first;
end;

procedure TCSList.Reverse(AFirst, ALast: TCHandle);
begin
  ListReverse(P_SSListItem(AFirst), P_SSListItem(ALast));
end;

function TCSList.NextPermutation:boolean;
var
  i,x,n: P_SSListItem;
begin
  if FCount<2 then
  begin
    result := false;
    exit;
  end;

  // find max N where A[N] < A[N+1]
  n := nil;
  i := FFirst;
  while i.Next<>nil do
  begin
    if FCompareValue(i.data, i.next.data) < 0 then
      n := i;
    i := i.Next;
  end;

  // if A[N] > A[N+1] for any N then there is no more permutations
  result := n<>nil;
  if not result then
    exit;

  // let's order range [N+1; FCoun-1]
  // range has reverse order so just call .reverse to make it ordered
  if n.Next<>nil then
    ListReverse(n.Next, FLast);

  // find min X (N<X<=FCount-1) where A[X] > A[N]
  // thus we find value next to A[N] in range [N+1; FCount-1]
  // such value exists because at least original A[N+1] > A[N]
  x := N;
  while FCompareValue(x.next.data, n.data) <= 0 do
    x := x.Next;
  i := x;
  x := x.Next;
  i.Next := x.Next;
  if FLast=x then
    FLast := i;

  // now [X] is item next to [N]
  // and [X] have removed from the structure

  // move X instead of N and remove N from the structure
  if FFirst=n then
    FFirst := x
  else
    ListGetPrev(n).Next := x;
  x.Next := n.Next;

  // find position for N in range [X..FLast] to make range ordered again
  while x.next<>nil do
    if FCompareValue(n.data, x.next.data) > 0 then
      x := x.Next
    else
      break;
  n.Next := x.Next;
  x.Next := n;
  if FLast=x then
    FLast := n;

end;

function TCSList.PrevPermutation:boolean;
var
  i,x,n: P_SSListItem;
begin
  if FCount<2 then
  begin
    result := false;
    exit;
  end;

  // find max N where A[N] > A[N+1]
  n := nil;
  i := FFirst;
  while i.Next<>nil do
  begin
    if FCompareValue(i.data, i.next.data) > 0 then
      n := i;
    i := i.Next;
  end;

  // if A[N] < A[N+1] for any N then there is no more permutations
  result := n<>nil;
  if not result then
    exit;

  // let's make range [N+1; FCoun-1] reverse-ordered
  // range has direct order so just call .reverse to make it reverse-ordered
  if n.Next<>nil then
    ListReverse(n.Next, FLast);

  // find min X (N<X<=FCount-1) where A[X] < A[N]
  // thus we find value prev to A[N] in range [N+1; FCount-1]
  // such value exists because at least original A[N+1] < A[N]
  x := N;
  while FCompareValue(x.next.data, n.data) >= 0 do
    x := x.Next;
  i := x;
  x := x.Next;
  i.Next := x.Next;
  if FLast=x then
    FLast := i;

  // now [X] is item next to [N]
  // and [X] have removed from the structure

  // move X instead of N and remove N from the structure
  if FFirst=n then
    FFirst := x
  else
    ListGetPrev(n).Next := x;
  x.Next := n.Next;

  // find position for N in range [X..FLast] to make range reverse-ordered again
  while x.next<>nil do
    if FCompareValue(n.data, x.next.data) < 0 then
      x := x.Next
    else
      break;
  n.Next := x.Next;
  x.Next := n;
  if FLast=x then
    FLast := n;

end;

procedure TCSList.ListExchange(a, b: P_SSListItem);
begin
  if FFirst=a then
    FFirst := b
  else
  begin
    ListGetPrev(a).Next := b;
    if FFirst=b then
      FFirst := a
    else
      ListGetPrev(b).Next := a;
  end;

  if FLast=a then
    FLast := b
  else
    if FLast=b then
      FLast := a;

  Exch(pointer(a.Next), pointer(b.Next));
end;

procedure TCSList.Exchange(handle1, handle2: TCHandle);
begin
  assert((handle1<>-1) and (handle2<>-1));
  ListExchange(P_SSListItem(handle1), P_SSListItem(handle2));
end;

function TCSList.MergeSort(c: P_SSListItem):P_SSListItem;
var
  l,r,n: P_SSListItem;
begin
  result := c;
  if c=nil then
    exit;
  if c.next=nil then
    exit;

  // split C->L+R
  l := nil;
  r := nil;
  repeat
    n := c;
    c := c.Next;
    n.Next := l;
    l := n;
    if c=nil then
      break;
    n := c;
    c := c.Next;
    n.Next := r;
    r := n;
  until c=nil;

  // sort L&R
  l := MergeSort(l);
  r := MergeSort(r);

  // merge L+R->Result
  if l=nil then
    result := r
  else
  if r=nil then
    result := l
  else
  begin

    // result=Head, c=Tail
    if FComparator(l, r) <= 0 then // L < R
    begin
      result := l;
      l := l.Next;
    end else begin
      result := r;
      r := r.Next;
    end;
    c := result;

    // L&R -> Tail (c)
    while (l<>nil) and (r<>nil) do
      if FComparator(l, r) <= 0 then // L < R
      begin
        c.Next := l;
        c := l;
        l := l.Next;
      end else begin
        c.Next := r;
        c := r;
        r := r.Next;
      end;

    // put remain items to tail
    if l=nil then
      c.Next := r
    else
      c.Next := l;
  end;
end;

function TCSList.IntCmp(a,b: P_SSListItem):integer;
begin
  result := FCompareValue(a.data, b.data);
end;

function TCSList.ExtCmp(a,b: P_SSListItem):integer;
begin
  result := FSortCompare(FSortField, PtrToHandle(a),PtrToHandle(b));
end;

function TCSList.ExtCmpProc(a,b: P_SSListItem):integer;
begin
  result := FSortCompareProc(FSortField, PtrToHandle(a),PtrToHandle(b));
end;

procedure TCSList.DoSort(AFirst, ALast: P_SSListItem);
var f,l,n: P_SSListItem;
begin
  n := ALast.Next;
  ALast.Next := nil;
  f := MergeSort(AFirst);
  l := f;
  while l.Next<>nil do
    l := l.Next;
  if ALast=FLast then
    FLast := l;
  l.Next := n;
  if AFirst=FFirst then
    FFirst := f
  else
    ListGetPrev(AFirst).Next := f;
end;

procedure TCSList.Sort(AFirst, ALast: TCHandle);
begin
  FComparator := IntCmp;
  DoSort(P_SSListItem(AFirst), P_SSListItem(ALast));
end;

procedure TCSList.Sort(compare: TOnCompareValues; AFirst, ALast: TCHandle);
var p: TOnCompareValues;
begin
  p := FCompareValue;
  try
    FCompareValue := compare;
    FComparator := IntCmp;
    DoSort(P_SSListItem(AFirst), P_SSListItem(ALast));
  finally
    FCompareValue := p;
  end;
end;

procedure TCSList.Sort(field: TTCustom; compare: TOnCompare; AFirst, ALast: TCHandle);
begin
  if Count<=0 then
    exit;
  FSortField := field;
  FSortCompare := compare;
  FComparator := ExtCmp;
  DoSort(P_SSListItem(AFirst), P_SSListItem(ALast));
end;

procedure TCSList.Sort(field: TTCustom; compare: TOnCompareProc; AFirst, ALast: TCHandle);
begin
  if Count<=0 then
    exit;
  FSortField := field;
  FSortCompareProc := compare;
  FComparator := ExtCmpProc;
  DoSort(P_SSListItem(AFirst), P_SSListItem(ALast));
end;

destructor TCSList.Destroy;
begin
  Clear;
  itemsize := 0;
  inherited;
end;

procedure TCSList.Swap(handle1, handle2: TCHandle);
begin
  FSwapItem(P_SSListItem(handle1).Data, P_SSListItem(handle2).Data, ftemp^, FItemSize);
end;

function TCSList.Get(handle: TCHandle): Pointer;
begin
  assert(handle<>-1,erhandle);
  Result := @P_SSListItem(handle).Data;
end;

function TCSList.GetHandle(index: integer): TCHandle;
var
  p: P_SSListItem;
begin
  if (index < 0) or (index > FCount - 1) then
    Result := -1
  else
  begin
    p := FFirst;
    while index > 0 do
    begin
      Dec(index);
      p := p.Next;
    end;
    Result := PtrTohandle(p);
  end;
end;

procedure TCSList.Grow;
begin
end;

// TCBasic navigation
function TCSList.PtrTohandle(p: P_SSListItem): TCHandle;
begin
  if p = nil then
    Result := -1
  else
    Result := TCHandle(p);
end;

function TCSList.First: TCHandle;
begin
  Result := PtrTohandle(FFirst);
end;

function TCSList.Last: TCHandle;
begin
  Result := PtrTohandle(FLast);
end;

procedure TCSList.Next(var n: TCHandle);
begin
  if EOF(n) then
    n := -1
  else
    n := PtrTohandle(P_SSListItem(n).Next);
end;

procedure TCSList.Prev(var n: TCHandle);
begin
  if EOF(n) then
    n := -1
  else
    n := PtrTohandle(ListGetPrev(P_SSListItem(n)));
end;

function TCSList.EOF(n: TCHandle): boolean;
begin
  Result := n = -1;
end;

function TCSList.Insert(handle: TCHandle): TCHandle;
var
  pos, v: P_SSListItem;
begin
  if (FFirst = nil) and (handle = -1) then
    Result := ListAdd
  else
  begin
    pos := P_SSListItem(handle);
    v := AllocNewItem;
    ListInsertBefore(v, pos);
    Inc(FCount);
    Result := PtrTohandle(v);
  end;
end;

function TCSList.InsertAfter(handle: TCHandle): TCHandle;
var
  pos, v: P_SSListItem;
begin
  if (FFirst = nil) and (handle = -1) then
    Result := ListAdd
  else
  begin
    pos := P_SSListItem(handle);
    v := AllocNewItem;
    ListInsertAfter(v, pos);
    Inc(FCount);
    Result := PtrTohandle(v);
  end;
end;

function TCSList.ListGetPrev(n: P_SSListItem): P_SSListItem;
var
  r: P_SSListItem;
begin
  r := FFirst;
  while r <> nil do
    if r.Next = n then
      break
    else
      r := r.Next;
  Result := r;
end;

procedure TCSList.ListInsertAfter(v, pos: P_SSListItem);
begin
  v.Next := pos.Next;
  pos.Next := v;
  if pos = FLast then
    FLast := v;
end;

procedure TCSList.ListInsertBefore(v, pos: P_SSListItem);
begin
  v.Next := pos;
  if pos = FFirst then
    FFirst := v
  else
    ListGetPrev(pos).Next := v;
end;

procedure TCSList.ListRemove(n: P_SSListItem);
var
  c: P_SSListItem;
begin
  if n = FFirst then
  begin
    FFirst := n.Next;
    if n = FLast then
      FLast := FFirst; // actually possible only when N is the only item in Container
  end
  else
  begin
    c := ListGetPrev(n);
    c.Next := n.Next;
    if n = FLast then
      FLast := c;
  end;
end;

procedure TCSList.Move(Curhandle, Newhandle: TCHandle);
var
  c, n: P_SSListItem;
begin
  if Curhandle=Newhandle then
    exit;
  c := P_SSListItem(Curhandle);
  n := P_SSListItem(Newhandle);
  while (c<>nil) and (c<>n) do
    c := c.Next;
  if c=nil then // move item backward
  begin
    c := P_SSListItem(Curhandle);
    ListRemove(c);
    ListInsertBefore(c, n);
  end
  else
  begin
    c := P_SSListItem(Curhandle);
    ListRemove(c);
    ListInsertAfter(c, n);
  end;
end;

procedure TCSList.Put(handle: TCHandle; var Item);
begin
  FMoveField(item, P_SSListItem(handle).Data, FFieldSize);
end;

function TCSList.ReallocItem(p: P_SSListItem): P_SSListItem;
begin
  Result := p;
  reallocmem(Result, sizeof(T_SSListItem) + FItemSize);
end;

procedure TCSList.ReleaseItem(p: P_SSListItem);
begin
  reallocmem(p, 0);
end;

procedure TCSList.SetCount(n: integer);
var
  c, d: P_SSListItem;
  i: integer;
begin
  if n > FCount then
    for i := 0 to n - FCount - 1 do
      ListAdd
  else
  if n = 0 then
    Clear
  else
  begin // 0 < N < FCount
    c := P_SSListItem(handles[n - 1]); // last item we should leave
    FLast := c;
    c := c.Next;
    FLast.Next := nil;
    FCount := n;
    while c <> nil do
    begin
      d := c;
      c := c.Next;
      if FDelCount>0 then
        ItemDeleted(PtrTohandle(d));
      ReleaseItem(d);
    end;
  end;
end;

procedure TCSList.SetItemSize(n: integer);
begin
  inherited;
  if FFirst = nil then
    exit;
  FFirst := ReallocItem(FFirst);
  FLast  := FFirst;
  while FLast.Next <> nil do
  begin
    FLast.Next := ReallocItem(FLast.Next);
    FLast := FLast.Next;
  end;
end;

{ TCDList }

constructor TCDList.Create;
begin
  inherited Create;
  InitZero := True;
end;

destructor TCDList.Destroy;
begin
  Clear;
  itemsize := 0;
  inherited;
end;

function DListMerge(c: P_SDListItem; Comparator: TCDListComparator):P_SDListItem;
var
  l,r,n: P_SDListItem;
begin
  result := c;
  if c=nil then
    exit;
  if c.next=nil then
    exit;

  // treeSplit C->L+R
  l := nil;
  r := nil;
  repeat
    n := c;
    c := c.Next;
    n.Next := l;
    l := n;
    if c=nil then
      break;
    n := c;
    c := c.Next;
    n.Next := r;
    r := n;
  until c=nil;

  // sort L&R
  l := DListMerge(l, Comparator);
  r := DListMerge(r, Comparator);

  // merge L+R->Result
  if l=nil then
    result := r
  else
  if r=nil then
    result := l
  else
  begin

    // result=Head, c=Tail
    if Comparator(l, r) <= 0 then // L < R
    begin
      result := l;
      l := l.Next;
    end else begin
      result := r;
      r := r.Next;
    end;
    c := result;

    // L&R -> Tail (c)
    while (l<>nil) and (r<>nil) do
      if Comparator(l, r) <= 0 then // L < R
      begin
        c.Next := l;
        c := l;
        l := l.Next;
      end else begin
        c.Next := r;
        c := r;
        r := r.Next;
      end;

    // put remain items to tail
    if l=nil then
      c.Next := r
    else
      c.Next := l;
  end;
end;

procedure DListSort(List: PDList; AFirst, ALast: P_SDListItem; Comparator: TCDListComparator);
var f,l,n: P_SDListItem;
begin
  n := ALast.Next;
  ALast.Next := nil;
  f := DListMerge(AFirst, Comparator);
  l := f;
  while l.Next<>nil do
    l := l.Next;
  if ALast=List.FLast then
    List.FLast := l;
  l.Next := n;
  if AFirst=List.FFirst then
    List.FFirst := f
  else
    AFirst.Prev.Next := f;

  // restore backward links
  // this part is the only difference from TCSList.DoSort
  f := nil;
  n := List.FFirst;
  while n<>nil do
  begin
    n.Prev := f;
    f := n;
    n := n.Next;
  end;
end;

procedure TCDList.DoSort(AFirst, ALast: P_SDListItem);
begin
  DListSort(@FList, AFirst, ALast, FComparator);
end;

function TCDList.IntCmp(a, b: P_SDListItem): integer;
begin
  result := FCompareValue(a.data, b.data);
end;

function TCDList.ExtCmp(a, b: P_SDListItem): integer;
begin
  result := FSortCompare(FSortField, PtrToHandle(a),PtrToHandle(b));
end;

function TCDList.ExtCmpProc(a, b: P_SDListItem): integer;
begin
  result := FSortCompareProc(FSortField, PtrToHandle(a),PtrToHandle(b));
end;

// sort range of container items by default Field with default comparator
// (can be optimized by internal implementation of swap/compare)
procedure TCDList.Sort(AFirst, ALast: TCHandle);
begin
  FComparator := IntCmp;
  DoSort(P_SDListItem(AFirst), P_SDListItem(ALast));
end;

// sort range of container items by default Field with user-defined by-value comparator
procedure TCDList.Sort(compare: TOnCompareValues; AFirst, ALast: TCHandle);
var p: TOnCompareValues;
begin
  p := FCompareValue;
  try
    FCompareValue := compare;
    FComparator := IntCmp;
    DoSort(P_SDListItem(AFirst), P_SDListItem(ALast));
  finally
    FCompareValue := p;
  end;
end;

// sort range of container items by specified Field with user-defined by-handle comparator method
procedure TCDList.Sort(field: TTCustom; compare: TOnCompare; AFirst, ALast: TCHandle);
begin
  if Count<=0 then
    exit;
  FSortField := field;
  FSortCompare := compare;
  FComparator := ExtCmp;
  DoSort(P_SDListItem(AFirst), P_SDListItem(ALast));
end;

// sort range of container items by specified Field with user-defined by-handle comparator procedure
procedure TCDList.Sort(field: TTCustom; compare: TOnCompareProc; AFirst, ALast: TCHandle);
begin
  if Count<=0 then
    exit;
  FSortField := field;
  FSortCompareProc := compare;
  FComparator := ExtCmpProc;
  DoSort(P_SDListItem(AFirst), P_SDListItem(ALast));
end;

procedure DListAdd(v: P_SDListItem; list: PDList);
begin
  with list^do
    if FLast <> nil then
      DListInsertAfter(v, FLast, list)
    else
    begin
      FFirst := v;
      FLast  := v;
    end;
end;

function TCDList.ListAdd: TCHandle;
begin
  result := TCHandle(AllocNewItem);
  DListAdd(P_SDListItem(result), @FList);
  Inc(FCount);
end;

procedure DListAddSorted(v: P_SDListItem; list: PDList; Comparator: TCDListComparator);
var
  p: P_SDListItem;
begin
  with list^do
    if FFirst=nil then
    begin
      FFirst := v;
      FLast := v;
    end
    else
    begin
      p := FFirst;
      if Comparator(v, p)<=0 then
        DListInsertBefore(v, p, list)
      else
      begin
        while p.next<>nil do
          if Comparator(v, p.Next) <= 0 then
            break
          else
            p := p.Next;
        DListInsertAfter(v, p, list);
      end;
    end;
end;

function TCDList.AddItem(var item): TCHandle;
var
  pos, v: P_SDListItem;
begin
  // add item
  v := AllocNewItem;
  Result := TCHandle(v);

  if FSorted then
  begin
    if FList.FFirst=nil then
    begin
      FList.FFirst := v;
      FList.FLast := v;
    end
    else
    begin
      pos := FList.FFirst;
      if FCompareValue(item, pos.data)<=0 then
        ListInsertBefore(v, pos)
      else
      begin
        while pos.next<>nil do
          if FCompareValue(item, pos.Next.Data) <= 0 then
            break
          else
            pos := pos.Next;
        ListInsertAfter(v, pos);
      end;
    end;
  end
  else

    if FList.FLast <> nil then
      ListInsertAfter(v, FList.FLast)
    else
    begin
      FList.FFirst := v;
      FList.FLast  := v;
    end;

  Inc(FCount);
  // move data
  FMoveField(item, v.data, FFieldSize);
end;

function TCDList.AllocNewItem: P_SDListItem;
begin
  Result := AllocMem(sizeof(T_SDListItem) + FItemSize);
end;

procedure TCDList.Clear;
begin
  while FList.FFirst <> nil do
  begin
    FList.FLast  := FList.FFirst;
    FList.FFirst := FList.FFirst.Next;
    ReleaseItem(FList.FLast);
  end;
  FList.FFirst := nil;
  FList.FLast  := nil;
  FCount := 0;
end;

procedure TCDList.Copy(Src, Dst: TCHandle);
begin
  if FDelCount>0 then
    ItemDeleted(dst);
  if FCopyCount > 0 then
    CopyItem(src, dst)
  else
    FMoveItem(P_SDListItem(src).Data, P_SDListItem(dst).Data, FITemSize);
end;

procedure TCDList.Remove(var Item);
var p: P_SDListItem;
begin
  p := FList.FFirst;
  while p<>nil do
    if FCompareValue(Item, p.data)<>0 then
      p := p.Next
    else
    begin
      Delete(PtrToHandle(p));
      exit;
    end;
end;

procedure TCDList.RemoveAll(var Item);
var p,q: P_SDListItem;
begin
  p := FList.FFirst;
  while p<>nil do
  begin
    q := p;
    p := p.Next;
    if FCompareValue(Item, q.data)=0 then
      Delete(PtrToHandle(q));
  end;
end;

procedure DListRandomShuffle(AFirst, ALast: P_SDListItem; List: PDList);
var
  f,l,p,n: P_SDListItem;
  i,w: integer;
  h: TTPointer;
begin
  if AFirst=ALast then
    exit;
  p := AFirst;
  w := 1;
  while (p<>nil) and (p<>ALast) do
  begin
    inc(w);
    p := p.next;
  end;
  if p=nil then
  begin
    DListRandomShuffle(ALast, AFirst, List);
    exit;
  end;
  if w<=1 then
    exit;
  h := TTPointer.create;
  h.Capacity := w;
  n := AFirst;
  for i := 0 to w-1 do
  begin
    h.add(n);
    n := n.Next;
  end;
  h.RandomShuffle;

  f := AFirst.Prev;
  l := ALast.Next;
  with List^ do
  begin
    if f=nil then
      FFirst := h[0];
    if l=nil then
      FLast := h[h.count-1];
  end;
  for i := 0 to h.Count-1 do
  begin
    p := h[i];
    if f<>nil then
      f.Next := p;
    p.Prev := f;
    f := p;
  end;
  f.Next := l;
  if l<>nil then
    l.Prev := f;

  h.free;
end;

procedure TCDList.RandomShuffle(AFirst, ALast: TCHandle);
begin
  assert((AFirst<>-1) and (ALast<>-1));
  DListRandomShuffle(P_SDListItem(AFirst), P_SDListItem(ALast), @FList);
end;

procedure DListReverse(AFirst, ALast: P_SDListItem; list: PDList);
var
  q, temp, prev, first, next: P_SDListItem;
begin

  // prev -> A->B->...->C -> next
  prev := AFirst.Prev;
  first:= AFirst;
  next := ALast.Next;
  q := nil;
  repeat
    temp := AFirst;
    if AFirst=ALast then
      AFirst := nil
    else
      AFirst := AFirst.Next;
    temp.next := q;
    q := temp;
  until AFirst=nil;

  with list^do
  begin
    // prev -> C->...->B->A -> next
    if prev=nil then
      FFirst := q
    else
      prev.Next := q;

    // "first" has moved to the end of reverse range
    first.Next := next;
    if next=nil then
      FLast := first;
  end;

  // restore backward links
  ALast.Prev := prev;
  while ALast.next<>nil do
  begin
    ALast.Next.Prev := ALast;
    if ALast=first then
      break;
    ALast := ALast.Next;
  end;
end;

procedure TCDList.ListReverse(AFirst, ALast: P_SDListItem);
begin
  DListReverse(AFirst, ALast, @FList);
end;

procedure TCDList.Reverse(AFirst, ALast: TCHandle);
begin
  ListReverse(P_SDListItem(AFirst), P_SDListItem(ALast));
end;

function TCDList.NextPermutation:boolean;
var
  i,x,n: P_SDListItem;
begin
  if FCount<2 then
  begin
    result := false;
    exit;
  end;

  with FList do
  begin
    // find max N where A[N] < A[N+1]
    n := nil;
    i := FFirst;
    while i.Next<>nil do
    begin
      if FCompareValue(i.data, i.next.data) < 0 then
        n := i;
      i := i.Next;
    end;

    // if A[N] > A[N+1] for any N then there is no more permutations
    result := n<>nil;
    if not result then
      exit;

    // let's order range [N+1; FCoun-1]
    // range has reverse order so just call .reverse to make it ordered
    if n.Next<>nil then
      ListReverse(n.Next, FLast);

    // find min X (N<X<=FCount-1) where A[X] > A[N]
    // thus we find value next to A[N] in range [N+1; FCount-1]
    // such value exists because at least original A[N+1] > A[N]
    x := N;
    while FCompareValue(x.next.data, n.data) <= 0 do
      x := x.Next;
    i := x;
    x := x.Next;
    i.Next := x.Next;
    if FLast=x then
      FLast := i;

    // now [X] is item next to [N]
    // and [X] have removed from the structure

    // move X instead of N and remove N from the structure
    if FFirst=n then
      FFirst := x
    else
      n.prev.Next := x;
    x.Next := n.Next;

    // find position for N in range [X..FLast] to make range ordered again
    while x.next<>nil do
      if FCompareValue(n.data, x.next.data) > 0 then
        x := x.Next
      else
        break;
    n.Next := x.Next;
    x.Next := n;
    if FLast=x then
      FLast := n;

    // correct backward links
    x := FFirst;
    x.prev := nil;
    while x.Next<>nil do
    begin
      x.Next.Prev := x;
      x := x.Next;
    end;
  end;
end;

function TCDList.PrevPermutation:boolean;
var
  i,x,n: P_SDListItem;
begin
  if FCount<2 then
  begin
    result := false;
    exit;
  end;

  with FList do
  begin
    // find max N where A[N] > A[N+1]
    n := nil;
    i := FFirst;
    while i.Next<>nil do
    begin
      if FCompareValue(i.data, i.next.data) > 0 then
        n := i;
      i := i.Next;
    end;

    // if A[N] < A[N+1] for any N then there is no more permutations
    result := n<>nil;
    if not result then
      exit;

    // let's make range [N+1; FCoun-1] reverse-ordered
    // range has direct order so we just call .reverse to make it reverse-ordered
    if n.Next<>nil then
      ListReverse(n.Next, FLast);

    // find min X (N<X<=FCount-1) where A[X] < A[N]
    // thus we find value prev to A[N] in range [N+1; FCount-1]
    // such value exists because at least original A[N+1] < A[N]
    x := N;
    while FCompareValue(x.next.data, n.data) >= 0 do
      x := x.Next;
    i := x;
    x := x.Next;
    i.Next := x.Next;
    if FLast=x then
      FLast := i;

    // now [X] is item next to [N]
    // and [X] have removed from the structure

    // move X instead of N and remove N from the structure
    if FFirst=n then
      FFirst := x
    else
      n.prev.Next := x;
    x.Next := n.Next;

    // find position for N in range [X..FLast] to make range reverse-ordered again
    while x.next<>nil do
      if FCompareValue(n.data, x.next.data) < 0 then
        x := x.Next
      else
        break;
    n.Next := x.Next;
    x.Next := n;
    if FLast=x then
      FLast := n;

    // correct backward links
    x := FFirst;
    while x.Next<>nil do
    begin
      x.Next.Prev := x;
      x := x.Next;
    end;
  end;
end;

procedure DListRotate(AFirst, ALast: P_SDListItem; shift: integer; list: PDList);
var
  p, pprev, pfirst, nlast: P_SDListItem;
  i,w: integer;
begin
  if AFirst=ALast then
    exit;
  // find W and make SHIFT positive and less than W
  w := 1;
  p := AFirst;
  while (p<>ALast) and (p<>nil) do
  begin
    p := p.Next;
    inc(w);
  end;
  if p=nil then
  begin
    DListRotate(ALast, AFirst, shift, List);
    exit;
  end;
  shift := shift mod w;
  if shift=0 then
    exit;
  if shift<0 then
    inc(shift, w);

  // number of left shifts = w-shift
  p := AFirst;
  for i := 0 to w-shift-1 do
    p := p.Next;

  // prev(AFirst) <-> P-ALast <-> AFirst-prev(p) <-> next(ALast)
  pprev := p.prev;
  pfirst := AFirst.prev;
  nlast := ALast.next;

  with List^do
  begin
    if pfirst=nil then
      FFirst := p
    else
      pfirst.Next := p;
    pprev.Next := ALast.Next;
    ALast.Next := AFirst;
    if FLast=ALast then
      FLast := pprev;
  end;

  p.Prev := pfirst;
  AFirst.prev := ALast;
  if nlast<>nil then
    nlast.Prev := pprev;
end;

procedure TCDList.Rotate(AFirst, ALast: TCHandle; shift: integer);
begin
  assert((AFirst<>-1) and (ALast<>-1), erOperation);
  DListRotate(P_SDListItem(AFirst), P_SDListItem(ALast), shift, @FList);
end;

procedure TCDList.Delete(handle: TCHandle);
var
  p: P_SDListItem;
begin
  if FDelCount>0 then
    ItemDeleted(handle);
  p := P_SDListItem(handle);
  ListRemove(p);
  ReleaseITem(p);
  Dec(FCount);
end;

procedure TCDList.DeleteNext(handle: TCHandle);
var
  p, q: P_SDListItem;
begin
  p := P_SDListItem(handle);
  if p.Next = nil then
    exit;
  q := p.Next;                   // item to delete
  if FDelCount>0 then
    ItemDeleted(PtrTohandle(q));
  p.Next := q.Next;
  if p.Next <> nil then
    p.Next.Prev := p;
  if q = FList.FLast then
    FList.FLast := p;
  releaseItem(q);
  Dec(FCount);
end;

function TCDList.EOF(n: TCHandle): boolean;
begin
  Result := n = -1;
end;

procedure TCDList.Swap(handle1, handle2: TCHandle);
begin
  FSwapItem(P_SDListItem(handle1).Data, P_SDListItem(handle2).Data, ftemp^, FItemSize);
end;

procedure DListExchange(a, b: P_SDListItem; list: PDList);
begin
  Exch(pointer(a.Prev), pointer(b.Prev));
  Exch(pointer(a.Next), pointer(b.Next));
  with list^ do
  begin
    if a.Prev=nil then
      FFirst := a
    else
      a.Prev.Next := a;
    if a.Next=nil then
      FLast := a
    else
      a.Next.Prev := a;
    if b.Prev=nil then
      FFirst := b
    else
      b.Prev.Next := b;
    if b.Next=nil then
      FLast := b
    else
      b.Next.Prev := b;
  end;
end;

procedure DListExchange(a, b: P_SDListItem; list_a, list_b: PDList);
begin
  Exch(pointer(a.Prev), pointer(b.Prev));
  Exch(pointer(a.Next), pointer(b.Next));
  if a.Prev=nil then
    list_b.FFirst := a
  else
    a.Prev.Next := a;
  if a.Next=nil then
    list_b.FLast := a
  else
    a.Next.Prev := a;
  if b.Prev=nil then
    list_a.FFirst := b
  else
    b.Prev.Next := b;
  if b.Next=nil then
    list_a.FLast := b
  else
    b.Next.Prev := b;
end;

procedure TCDList.ListExchange(a, b: P_SDListItem);
begin
  DListExchange(a,b, @FList);
end;

procedure TCDList.Exchange(handle1, handle2: TCHandle);
begin
  assert((handle1<>-1) and (handle2<>-1));
  ListExchange(P_SDListItem(handle1), P_SDListItem(handle2));
end;

function TCDList.First: TCHandle;
begin
  Result := PtrTohandle(FList.FFirst);
end;

function TCDList.Get(handle: TCHandle): Pointer;
begin
  assert(handle<>-1,erhandle);
  Result := @P_SDListItem(handle).Data;
end;

function TCDList.GetHandle(index: integer): TCHandle;
var
  p: P_SDListItem;
begin
  if (index < 0) or (index > FCount - 1) then
    Result := -1
  else
  begin
    p := FList.FFirst;
    while index > 0 do
    begin
      Dec(index);
      p := p.Next;
    end;
    Result := PtrTohandle(p);
  end;
end;

procedure TCDList.Grow;
begin
end;

function TCDList.Insert(handle: TCHandle): TCHandle;
var
  v: P_SDListItem;
begin
  if (FList.FFirst = nil) and (handle = -1) then
    Result := ListAdd
  else
  begin
    v := AllocNewItem;
    Inc(FCount);
    Result := PtrTohandle(v);
    ListInsertBefore(v, P_SDListItem(handle));
  end;
end;

function TCDList.InsertAfter(handle: TCHandle): TCHandle;
var
  v: P_SDListItem;
begin
  if (FList.FFirst = nil) and (handle = -1) then
    Result := ListAdd
  else
  begin
    v := AllocNewItem;
    Inc(FCount);
    Result := PtrTohandle(v);
    ListInsertAfter(v, P_SDListItem(handle));
  end;
end;

function TCDList.Last: TCHandle;
begin
  Result := PtrTohandle(FList.FLast);
end;

 //    v
 //     \
 //  p-> . <-p.next
procedure DListInsertAfter(v, p: P_SDListItem; list: PDList);
begin
  with list^do
    if FLast = p then
      FLast := v;
  if p.Next <> nil then
    p.Next.Prev := v;
  v.Next := p.Next;
  p.Next := v;
  v.Prev := p;
end;

procedure TCDList.ListInsertAfter(v, p: P_SDListItem);
begin
  DListInsertAfter(v,p, @FList);
end;

 //         v
 //          \
 //  p.Prev-> . <-p
procedure DListInsertBefore(v, p: P_SDListItem; list: PDList);
begin
  with list^do
    if FFirst = p then
      FFirst := v;
  if p.Prev <> nil then
    p.Prev.Next := v;
  v.Prev := p.Prev;
  p.Prev := v;
  v.Next := p;
end;

procedure TCDList.ListInsertBefore(v, p: P_SDListItem);
begin
  DListInsertBefore(v,p, @FList);
end;

// n.Prev <-> n <-> n.next
procedure DListRemove(n: P_SDListItem; List: PDList);
begin
  if n.Prev <> nil then
    n.Prev.Next := n.Next;
  if n.Next <> nil then
    n.Next.Prev := n.Prev;
  with List^do
  begin
    if n = FFirst then
      FFirst := n.Next;
    if n = FLast then
      FLast := n.Prev;
  end;
end;

procedure TCDList.ListRemove(n: P_SDListItem);
begin
  DListRemove(n, @FList);
end;

procedure DListMove(CurPos, NewPos: P_SDListItem; List: PDList);
var
  c: P_SDListItem;
begin
  c := CurPos;
  while (c<>nil) and (c<>NewPos) do
    c := c.Next;
  DListRemove(CurPos, List);
  if c=nil then // move item backward
    DListInsertBefore(CurPos, NewPos, List)
  else
    DListInsertAfter(CurPos, NewPos, List);
end;

procedure DListMove(CurPos, NewPos: P_SDListItem; CurList, NewList: PDList);
begin
  if CurList=NewList then
    DListMove(CurPos, NewPos, CurList)
  else
  begin
    DListRemove(CurPos, CurList);
    DListInsertBefore(CurPos, NewPos, NewList);
  end;
end;

procedure TCDList.Move(Curhandle, Newhandle: TCHandle);
begin
  DListMove(P_SDListItem(Curhandle),P_SDListItem(Newhandle), @FList);
end;

procedure TCDList.Next(var n: TCHandle);
begin
  if EOF(n) then
    n := -1
  else
    n := PtrTohandle(P_SDListItem(n).Next);
end;

procedure TCDList.Prev(var n: TCHandle);
begin
  if EOF(n) then
    n := -1
  else
    n := PtrTohandle(P_SDListItem(n).Prev);
end;

function TCDList.PtrTohandle(p: P_SDListItem): TCHandle;
begin
  if p = nil then
    Result := -1
  else
    Result := TCHandle(p);
end;

procedure TCDList.Put(handle: TCHandle; var Item);
begin
  FMoveField(item, P_SDListItem(handle).Data, FFieldSize);
end;

function TCDList.ReallocItem(p: P_SDListItem): P_SDListItem;
begin
  Result := p;
  reallocmem(Result, sizeof(T_SDListItem) + FItemSize);
end;

procedure TCDList.ReleaseItem(p: P_SDListItem);
begin
  reallocmem(p, 0);
end;

procedure TCDList.SetCount(n: integer);
var
  c, d: P_SDListItem;
  i: integer;
begin
  if n > FCount then
    for i := 0 to n - FCount - 1 do
      ListAdd
  else
  if n = 0 then
    Clear
  else
  begin // 0 < N < FCount
    c := P_SDListItem(handles[n - 1]); // last item we should leave
    FList.FLast := c;
    c := c.Next;
    FList.FLast.Next := nil;
    FCount := n;
    while c <> nil do
    begin
      d := c;
      c := c.Next;
      if FDelCount>0 then
        ItemDeleted(PtrTohandle(d));
      ReleaseItem(d);
    end;
  end;
end;

procedure TCDList.SetItemSize(n: integer);
begin
  inherited;
  with FList do
  begin
    if FFirst = nil then
      exit;
    FFirst := ReallocItem(FFirst);
    FLast  := FFirst;
    while FLast.Next <> nil do
    begin
      FLast.Next := ReallocItem(FLast.Next);
      FLast.Next.Prev := FLast;
      FLast := FLast.Next;
    end;
  end;
end;

{ --------------------------------------- T_CJoin ---------------------------}

constructor T_CJoin.Create;
begin
  Error(erOperation);
end;

constructor T_CJoin.CreateJoined(origin: TCCustom; Field: TTCustom);
begin
  inherited Create;
  FParent := origin;
  FField := field;
end;

destructor T_CJoin.Destroy;
begin
  FState := fsDestroying;
  inherited;
  if FParent<>nil then
    if FParent.FState<>fsDEstroying then
      FParent.Free;
  FParent := nil;
end;

function T_CJoin.AddItem(var item): TCHandle;
begin
  result := -1;
  error(erOperation);
//  result := FParent.Add;
//  put(result, item);
end;

procedure T_CJoin.Clear;
begin
  FParent.Clear;
end;

procedure T_CJoin.Copy(Src, Dst: TCHandle);
begin
  FParent.Copy(src, dst);
end;

procedure T_CJoin.Delete(handle: TCHandle);
begin
  FParent.Delete(handle);
end;

procedure T_CJoin.DeleteNext(handle: TCHandle);
begin
  FParent.DeleteNext(handle);
end;

function T_CJoin.EOF(handle: TCHandle): boolean;
begin
  result := FParent.EOF(handle);
end;

procedure T_CJoin.Swap(handle1, handle2: TCHandle);
begin
  FParent.Swap(handle1, handle2);
end;

function T_CJoin.First: TCHandle;
begin
  result := FParent.First;
end;

function T_CJoin.Get(handle: TCHandle): Pointer;
begin
  Result := @PByteArray(FParent[handle])[FOffset];
end;

function T_CJoin.GetCapacity: integer;
begin
  result := FParent.Capacity;
end;

function T_CJoin.GetCount: integer;
begin
  result := FParent.Count;
end;

function T_CJoin.GetGrowDown: boolean;
begin
  result := FParent.GrowDown;
end;

function T_CJoin.GetHandle(index: integer): TCHandle;
begin
  result := FParent.GetHandle(index);
end;

function T_CJoin.GetInitZero: boolean;
begin
  result := FParent.GetInitZero;
end;

function T_CJoin.GetSorted: boolean;
begin
  result := FParent.GetSorted;
end;

procedure T_CJoin.Grow;
begin
  FParent.Grow;
end;

function T_CJoin.Insert(handle: TCHandle): TCHandle;
begin
  result := FParent.Insert(handle);
end;

function T_CJoin.InsertAfter(handle: TCHandle): TCHandle;
begin
  result := FParent.InsertAfter(handle);
end;

function T_CJoin.InsertAfterItem(handle: TCHandle; var item): TCHandle;
begin
  result := FParent.InsertAfter(handle);
  put(result, item);
end;

function T_CJoin.InsertItem(handle: TCHandle; var item): TCHandle;
begin
  result := FParent.Insert(handle);
  put(result, item);
end;

procedure T_CJoin.Sort(AFirst, ALast: TCHandle);
begin
  FParent.sort(AFirst, ALast);
end;

procedure T_CJoin.Sort(compare: TOnCompareValues; AFirst, ALast: TCHandle);
begin
  FParent.sort(compare, AFirst, ALast);
end;

procedure T_CJoin.Sort(field: TTCustom; compare: TOnCompare; AFirst, ALast: TCHandle);
begin
  FParent.sort(field, compare, AFirst, ALast);
end;

procedure T_CJoin.Sort(field: TTCustom; compare: TOnCompareProc; AFirst, ALast: TCHandle);
begin
  FParent.sort(field, compare, AFirst, ALast);
end;

function T_CJoin.Last: TCHandle;
begin
  result := FParent.Last;
end;

procedure T_CJoin.Move(Curhandle, Newhandle: TCHandle);
begin
  FParent.Move(Curhandle, Newhandle);
end;

procedure T_CJoin.Next(var n: TCHandle);
begin
  FParent.Next(n);
end;

procedure T_CJoin.Prev(var n: TCHandle);
begin
  FParent.Prev(n);
end;

procedure T_CJoin.Put(handle: TCHandle; var Item);
begin
  FMoveField(Item, PByteArray(FParent[handle])[FOffset], FFieldSize);
end;

procedure T_CJoin.SetCapacity(n: integer);
begin
  FParent.SetCapacity(n);
end;

procedure T_CJoin.SetCount(n: integer);
begin
  FParent.SetCount(n);
end;

procedure T_CJoin.SetGrowDown(n: boolean);
begin
  FParent.SetGrowDown(n);
end;

procedure T_CJoin.SetInitZero(n: boolean);
begin
  FParent.SetInitZero(n);
end;

procedure T_CJoin.SetItemSize(n: integer);
begin
  inherited SetItemSize(n);
  {
  N=0 is also important!
  for example TTRecord MUST get valid FOffset
  even when constructing with ItemSize=0
  (correct ItemSize come later)
  }
  //assert(FItemSize=0, erOperation); // can be called once!
  if n<FieldSize then
    exit;
  FieldSize := n;
  if FOffset=0 then
    FOffset := FParent.ItemSize;
  FParent.ItemSize := FParent.ItemSize + FItemSize;
end;

procedure T_CJoin.SetSorted(v: boolean);
begin
  FParent.SetSorted(v);
end;

function T_CJoin.GetFieldCount: integer;
begin
  result := FParent.FieldCount;
end;

function T_CJoin.AddField(field: TTCustom): pointer;
begin
  result := FParent.AddField(field);
end;

procedure T_CJoin.DelField(field: TTCustom);
begin
  FParent.DelField(field);
end;

function T_CJoin.GetField(index: integer): TTCustom;
begin
  result := FParent.GetField(index);
end;

function T_CJoin.GetFieldByName(const FieldName: string): TTCustom;
begin
  result := FParent.GetFieldByName(FieldName);
end;

procedure T_CJoin.AddOnDelete(AOnDelete: TOnItemDeleted);
begin
  FPArent.AddOnDelete(AOnDelete);
end;

procedure T_CJoin.DelOnDelete(AOnDelete: TOnItemDeleted);
begin
  FPArent.DelOnDelete(AOnDelete);
end;

procedure T_CJoin.AddOnCopy(AOnCopy: TOnCopyItem);
begin
  FPArent.AddOnCopy(AOnCopy);
end;

procedure T_CJoin.DelOnCopy(AOnCopy: TOnCopyItem);
begin
  FParent.DelOnCopy(AOnCopy);
end;

function T_CJoin.IndexOfField(field: TTCustom): integer;
begin
  result := FParent.IndexOfField(field);
end;

function T_CJoin.GetFieldSeparator:string;
begin
  result := FParent.FieldSeparator;
end;

procedure T_CJoin.SetFieldSeparator(v: string);
begin
  FParent.FieldSeparator := v;
end;

procedure TCCustom.SaveToFile(filename: string; FileFormat : TFileFormat = ffUTF8);
var s: TFileStream;
begin
  s := TFileStream.Create(filename, fmCreate or fmOpenWrite);
  try
    SaveToStream(s, FileFormat);
  finally
    s.Free;
  end;
end;

procedure TCCustom.WriteBOM(dst: TStream);
const bom: array[0..2] of byte = ($ef, $bb, $bf);
begin
  dst.Write(bom, sizeof(bom));
end;

function TCCustom.CheckBOM(src: TStream):boolean;
const
  bom: array[0..2] of byte = ($ef, $bb, $bf);
var
  buf: array[0..2] of byte;
  pos: int64;
begin
  pos := src.Position;
  try
    src.read(buf, sizeof(buf));
    result := CompareMem(@bom, @buf, 3);
  except
    result := false;
  end;
  if not result then
    src.Position := pos;
end;

procedure TCCustom.LoadFromFile(filename: string; FileFormat : TFileFormat = ffUTF8);
var s: TFileStream;
begin
  s := TFileStream.Create(filename, fmOpenRead);
  try
    LoadFromStream(s, FileFormat);
  finally
    s.Free;
  end;
end;

procedure TCCustom.SaveToStream(dst: CStream; FileFormat : TFileFormat = ffUTF8);
var d: TStream;
begin
  d := dst.create;
  try
    SaveToStream(d, FileFormat);
  finally
    d.free;
  end;
end;

{
  Some rules for storing of container to text file:
    - every record (one item of container) is single line in text file
    - string-fields are double-quoted but only when record contains more than one field

  Presentation of different types:

  - Single-field record
      string/char     - just string, no quotes
      int(s)/float(s) - numbers separated with space symbol
      record          - hex-string of record data
  - Multi-field record
      Same rules but string/char in quotes (either single or doubble quote)

  Encoding of some special symbols:
    &q - '"'
    &r - #13
    &n - #10
    &a - '&'
}
procedure TCCustom.SaveToStream(dst: TStream; FileFormat : TFileFormat = ffUTF8);
var
  i: integer;
  h: TCHandle;
  b: boolean;
  eola, fsa: ansistring;
  eolw, fsw: widestring;
  eolu, fsu: utf8string;
begin
  assert(length(FFields)>0, erOperation);
  if FileFormat = ffUTF8 then
    WriteBOM(dst);
  eola := AnsiString(EOL);
  eolw := EOL;
  eolu := utf8encode(eolw);
  fsa := AnsiString(FieldSeparator);
  fsw := FieldSeparator;
  fsu := utf8encode(fsw);
  b := false;
  h := First;
  while not EOF(h) do
  begin

    // start new line
    if b and (EOL<>'') and (FileFormat<>ffBinary) then
      case FileFormat of
        ffAnsi : dst.Write(eola[1], length(eola)*sizeof(eola[1]));
        ffUnicode : dst.Write(eolw[1], length(eolw)*sizeof(eolw[1]));
        ffUTF8 : dst.Write(eolu[1], length(eolu)*sizeof(eolu[1]));
      end;
    b := true;

    // save fields one by one
    for i := 0 to high(FFields) do
    begin

      // start new field
      if (i>0) and (FieldSeparator<>'') and (FileFormat<>ffBinary) then
        case FileFormat of
          ffAnsi : dst.Write(fsa[1], length(fsa)*sizeof(fsa[1]));
          ffUnicode : dst.Write(fsw[1], length(fsw)*sizeof(fsw[1]));
          ffUTF8 : dst.Write(fsu[1], length(fsu)*sizeof(fsu[1]));
        end;

      // store field
      FFields[i].SaveItem(h, dst, FileFormat);
    end;

    Next(h);
  end;
end;

type
  {
    TStream implementation to capture output of SaveToStream and store to TStringList.
  }
  TStringListStream = class(TStream)
  public
    dst: TStrings;
    eolsize: integer;
    eolbuf: pointer;

    destructor Destroy; override;
    constructor create(dst: TStrings; eol: widestring);
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

constructor TStringListStream.create(dst: TStrings; eol: widestring);
begin
  inherited create;
  self.dst := dst;
  eolsize := length(eol)*sizeof(eol[1]);
  eolbuf := allocmem(eolsize);
  if eol<>'' then
    move(eol[1], eolbuf^, eolsize);
end;

destructor TStringListStream.destroy;
begin
  reallocmem(eolbuf, 0);
  inherited;
end;

// write support
function TStringListStream.Write(const Buffer; Count: Longint): Longint;
var
  s: widestring;
  n: integer;
begin
  result := count;
  if count<=0 then
    exit;
  if (count=eolsize) then
    if comparemem(@buffer, eolbuf, eolsize) then
    begin
      dst.Add('');
      exit;
    end;
  if dst.Count=0 then
    dst.add('');
  n := count div sizeof(widechar);
  if n=0 then
    exit;
  setlength(s, n);
  move(buffer, s[1], n*sizeof(widechar));
  dst[dst.count-1] := dst[dst.count-1] + s;
end;

function TStringListStream.Read(var Buffer; Count: Longint): Longint;
begin
  {$IFDEF FPC} result:=0; {$ENDIF}
  raise exception.create(erOperation);
end;

procedure TCCustom.SaveToStringList(dst: TStrings);
var
  s: TStringListStream;
begin
  dst.add('');
  s := TStringListStream.create(dst, eol);
  try
    SaveToStream(s, ffUnicode);
  finally
    s.free;
  end;
  dst.add('');
end;

procedure TCCustom.StringsToStream(src: TStrings; dst: TStream);
var
  i: integer;
  t,e: widestring;
begin
  e := EOL;
  for i := 0 to src.count-1 do
  begin
    t := src[i];
    if t<>'' then
      dst.write(t[1], length(t)*sizeof(t[1]));
    if (e<>'') and (i<src.count-1) then
      dst.write(e[1], length(e)*sizeof(e[1]));
  end;
end;

function TCCustom.GetRootCount: integer;
begin
  result := FCount;
end;

function TCCustom.GetFirstRoot: TCHandle;
begin
  result := First;
end;

function TCCustom.GetLastRoot: TCHandle;
begin
  result := Last;
end;

function TCCustom.GetParent(node: TCHandle): TCHandle;
begin
  result := -1;
end;

function TCCustom.GetChildCount(node: TCHandle): integer;
begin
  result := 0;
end;

function TCCustom.GetFirstChild(node: TCHandle): TCHandle;
begin
  result := -1;
end;

function TCCustom.GetLastChild(node: TCHandle): TCHandle;
begin
  result := -1;
end;

function TCCustom.GetPrevSibling(node: TCHandle): TCHandle;
begin
  Prev(node);
  result := node;
end;

function TCCustom.GetNextSibling(node: TCHandle): TCHandle;
begin
  Next(node);
  result := node;
end;

function TCCustom.AddChildItem(AParent: TCHandle; var Item): TCHandle;
begin
  assert(AParent=-1);
  result := AddItem(Item);
end;

function TCCustom.AddRootItem(var Item): TCHandle;
begin
  result := AddItem(Item);
end;

procedure TCCustom.ChangeParent(Node, ANewParent: TCHandle);
begin
  error(erOperation);
end;

procedure TCCustom.LoadFromStringList(src: TStrings);
var
  s: TMemoryStream;
begin
  s := TMemoryStream.create;
  try
    StringsToStream(src, s);
    s.Position := 0;
    LoadFromStream(s, ffUnicode);
  finally
    s.free;
  end;
end;

function IsTailW(var tail, str: widestring): boolean;
var i,j: integer;
begin
  if length(tail)>length(str) then
    result := false
  else
  begin
    j := length(str)-length(tail);
    for i := 1 to length(tail) do
      if tail[i]<>str[j+i] then
      begin
        result := false;
        exit;
      end;
    result := true;
  end;
end;

function IsTailA(var tail, str: ansistring): boolean;
var i,j: integer;
begin
  if length(tail)>length(str) then
    result := false
  else
  begin
    j := length(str)-length(tail);
    for i := 1 to length(tail) do
      if tail[i]<>str[j+i] then
      begin
        result := false;
        exit;
      end;
    result := true;
  end;
end;

function IsTailU(var tail, str: utf8string): boolean;
var i,j: integer;
begin
  if length(tail)>length(str) then
    result := false
  else
  begin
    j := length(str)-length(tail);
    for i := 1 to length(tail) do
      if tail[i]<>str[j+i] then
      begin
        result := false;
        exit;
      end;
    result := true;
  end;
end;

function TCCustom.ReadStrA(src: TStream): ansistring;
var
  c: ansichar;
  e: ansistring;
begin
  result := '';
  e := AnsiString(EOL);
  repeat
    if src.Position>=src.Size then
      exit;
    src.Read(c, sizeof(c));
    result := result + c;
  until IsTailA(e, result);
  setlength(result, length(result)-length(e))
end;

function TCCustom.ReadStrW(src: TStream): widestring;
var
  c: widechar;
  e: widestring;
begin
  result := '';
  e := EOL;
  repeat
    if src.Position>=src.Size then
      exit;
    src.Read(c, sizeof(c));
    result := result + c;
  until IsTailW(e, result);
  setlength(result, length(result)-length(e))
end;

function TCCustom.ReadStrU(src: TStream): UTF8String;
var
  e: UTF8String;
  l: integer;
begin
  l := 0;
  result := '';
  e := UTF8String(EOL);
  repeat
    if src.Position>=src.Size then
      exit;
    inc(l);
    setlength(result, l);
    src.Read(result[l], sizeof(result[l]));
  until IsTailU(e, result);
  setlength(result, length(result)-length(e))
end;

{
  text presentation form: "<text>"
  special symbols encoding:
  \q = "
  \r = #13
  \n = #10
  \s = \
}
function TCCustom.LoadToken:widestring;
type
  TState = (stEmpty, stInNum, stInText, stBackslash);
var
  state: TState;
  len: integer;
  c: widechar;
  numsymbols: widestring;
begin
  if (FieldCount=1) and (FField.FIsTextField) then
  begin
    result := FTextItem;
    exit;
  end;
  result := '';
  len := length(FTextItem);
  state := stEmpty;
  numsymbols := '-+0123456789' + decimalseparator;
  while FTextPos<=len do
  begin
    c := FTextItem[FTextPos];
    inc(FTextPos);
    case state of
      stEmpty:
        if c='"' then
          state := stInText
        else
          if pos(c, numsymbols) > 0 then
          begin
            result := c;
            state := stInNum;
          end;
      stInNum:
        if pos(c, numsymbols) > 0 then
          result := result + c
        else
        begin
          dec(FTextPos);
          break;
        end;
      stInText:
        if c='&' then
          state := stBackslash
        else
          if c='"' then
            break
          else
            result := result + c;
      stBackslash:
        begin
          if (c='q') or (c='Q') then
            result := result + '"'
          else
          if (c='r') or (c='R') then
            result := result + #13
          else
          if (c='n') or (c='N') then
            result := result + #10
          else
          if (c='a') or (c='A') then
            result := result + '&'
          else
            error(erOperation);
          state := stInText;
        end;
    end;
  end;
end;

procedure TCCustom.LoadFromStream(src: TStream; FileFormat : TFileFormat = ffUTF8);
var
  i: integer;
  h: TCHandle;
  u: UTF8String;
  SkipEmpty: boolean;
begin
  assert(length(FFields)>0, erOperation);
  Clear;
  if FileFormat = ffUTF8 then
    CheckBOM(src);
  if FSkipEmptyLines=selAuto then
    SkipEmpty := (FieldCount>1) or not FField.FIsTextField
  else
    SkipEmpty := FSkipEmptyLines=selSkip;
  while src.Position<src.Size do
  begin
    if FileFormat = ffBinary then
    begin
      h := FFields[0].LoadItem(self, -1, src, FileFormat);
      for i := 1 to high(FFields) do
        FFields[i].LoadItem(self, h, src, FileFormat)
    end
    else
    begin

      // read string and convert it to WideString
      case FileFormat of
        ffAnsi: FTextItem := WideString(ReadStrA(src));
        ffUnicode: FTextItem := ReadStrW(src);
        ffUTF8:
          begin
            u := ReadStrU(src);
            {$IFDEF HiCompiler}
            FTextItem := UTF8ToWideString(u);
            {$ELSE}
            FTextItem := Utf8Decode(u);
            {$ENDIF}
          end;
        else raise exception.create(erOperation);
      end;

      if SkipEmpty and (trim(FTextItem)='') then
        continue;

      // extract fields from string (LoadItem should use .LoadToken)
      FTextPos := 1;
      h := FFields[0].LoadItem(self, -1, src, FileFormat);
      for i := 1 to high(FFields) do
        FFields[i].LoadItem(self, h, src, FileFormat);
    end;
  end;
end;

procedure T_CJoin.SaveToFile(filename: string; FileFormat: TFileFormat);
begin
  FParent.SaveToFile(filename, FileFormat);
end;

procedure T_CJoin.SaveToStream(dst: TStream; FileFormat: TFileFormat);
begin
  FParent.SaveToStream(dst, FileFormat);
end;

procedure T_CJoin.SaveToStringList(dst: TStrings);
begin
  FParent.SaveToStringList(dst);
end;

procedure T_CJoin.LoadFromFile(filename: string; FileFormat : TFileFormat = ffUTF8);
begin
  FParent.LoadFromFile(filename, FileFormat);
end;

procedure T_CJoin.LoadFromStream(src: TStream; FileFormat : TFileFormat = ffUTF8);
begin
  FParent.LoadFromStream(src, FileFormat);
end;

procedure T_CJoin.LoadFromStringList(src: TStrings);
begin
  FParent.LoadFromStringList(src);
end;

{ TCYTree }

function TCYTree.AddItem(var Item): TCHandle;
begin
  if froot=nil then begin
    froot:= create_node(item);
    ffirst:=froot;
    flast:=froot;
  end else froot:=tinsert(froot, item);
  froot.isred:=false;
  result:=integer(finsert);
end;

procedure TCYTree.Clear;
begin
  if assigned(froot) then free_tree(froot);
  froot:=nil;
  fcount:=0;
end;

constructor TCYTree.Create;
begin
  inherited;
  froot:=nil;
  fcount:=0;
  ffirst:=nil;
  flast:=nil;
end;

function TCYTree.create_node(var item): P_YTreeItem;
begin
  result := AllocMem(sizeof(T_YTreeItem) + FItemSize);
  FMoveField(item, result.data, fFieldSize); // COPY
  result.IsRed:=true;
  result.lleaf:=true;
  result.rleaf:=true;
  inc(fcount);
  finsert:=result;
end;

procedure TCYTree.Remove(var Item);
begin
  if FCount = 0 then exit;
  FRoot:=tdelete(FRoot, item ,true);
  if assigned(FRoot) then FRoot.isred:=false;
end;

destructor TCYTree.Destroy;
begin
  clear;
  inherited;
end;

function TCYTree.EOF(h: TCHandle): boolean;
begin
  result := h <> ERRHANDLE;
end;

function TCYTree.Find(var Item): TCHandle;
begin
  result:=integer(tsearch(item));
  if result = 0 then result:= ERRHANDLE;
end;

function TCYTree.First: TCHandle;
begin
  if FFirst = nil then result := ERRHANDLE else result:=TCHandle(FFirst);
end;

procedure TCYTree.free_node(var h: P_YTreeItem);
begin
  reallocmem(h,0);
  dec(fcount);
end;

procedure TCYTree.free_tree(var h: P_YTreeItem);
begin
  if not h.LLeaf then free_tree(h.Left);
  if not h.RLeaf then free_tree(h.Right);
  free_node(h);
end;

function TCYTree.Get(handle: integer): Pointer;
begin
  if handle = ERRHANDLE then result:=nil
  else result:=@P_YTreeItem(handle).data;
end;

function TCYTree.is_red_left(h: P_YTreeItem): boolean;
begin
  if (h=nil) or h.lleaf then result:=false else result:=h.left.isred;
end;

function TCYTree.is_red_right(h: P_YTreeItem): boolean;
begin
  if (h=nil) or h.rleaf then result:=false else result:=h.right.isred;
end;

function TCYTree.Last: TCHandle;
begin
  if FLast = nil then result := ERRHANDLE else result:=TCHandle(FLast);
end;

procedure TCYTree.Next(var h: TCHandle);
var x: P_YTreeItem;
begin
  x:=tnext(P_YTreeItem(h));
  if x = nil then h := ERRHANDLE else h:=TCHandle(x);
end;

procedure TCYTree.Prev(var h: TCHandle);
var x: P_YTreeItem;
begin
  x:=tprev(P_YTreeItem(h));
  if x = nil then h := ERRHANDLE else h:=TCHandle(x);
end;

procedure TCYTree.Put(handle: integer; var Item);
begin
  if handle <> ERRHANDLE then
    FMoveField(item, P_YTreeItem(handle).data, fFieldSize);
end;

function TCYTree.tdelete(h: P_YTreeItem; var item;goleft: boolean): P_YTreeItem;
var cmp : integer;
    min : P_YTreeItem;
begin
  result:=nil;
  if h=nil then exit;
  cmp := FCompareValue(item,h.data);
  if (cmp < 0) then begin
    if h.lleaf then exit; // if h.left = nil -> we trying to delete unexistent node
    h:=tpush_red_left(h);
    h.left := tdelete(h.left, item, true);
  end else begin
    if is_red_left(h) then begin
      h := trotate_right(h);
      cmp := FCompareValue(item,h.data);
    end;
    if h.rleaf then begin  // we are going to delete right leaf
      if cmp=0 then begin
        if goleft then begin
          result:=h.left;
          if result = nil then ffirst:=h.right;
          if h.right <> nil then h.right.lleaf:=true;
        end else begin
          result:=h.right;
          if result = nil then flast:=h.left;
          if h.right <> nil then h.left.rleaf:=true;
        end;
        free_node(h);
      end;
      exit;
    end;
    // here h.right is non-nil (and left as well)
    if not is_red_right(h) and not is_red_left(h.right) then begin
      h.isred := not h.isred;
      h.left.isred := not h.left.isred;
      h.right.isred := true;
      if is_red_left(h.left) then begin
        h := trotate_right(h);
        h.isred := not h.isred;
        h.left.isred := not h.left.isred;
        h.right.isred := not h.right.isred;
      end;
      cmp := FCompareValue(item,h.data);
    end;
    if cmp=0 then begin
      min := h.right;
      while not min.lleaf do min:=min.left;
//      h.value := min.value;
//      h.key := min.key;
      FMoveItem(min.data, h.data, fItemSize); // COPY
      if min=h.right then begin
        h.right:=min.right;
        h.rleaf:=true;
        if flast = min then
          flast:=h;
        free_node(min);
      end else h.right := tdelete_min(h.right);
    end
    else h.right := tdelete(h.right, item, false);
  end;

  result:= tfix_up(h);
end;

function TCYTree.tdelete_min(h: P_YTreeItem): P_YTreeItem;
begin
  if h.lleaf then begin
    if h.left = nil then ffirst:=tnext(h);
    result:=h.left;
    h.right.lleaf:=true;
    free_node(h);
    exit;
  end; // here we have h.left
  h:=tpush_red_left(h);
  h.left := tdelete_min(h.left);
  result:= tfix_up(h);
end;

function TCYTree.tfix_up(h: P_YTreeItem): P_YTreeItem;
begin
  if is_red_right(h) then h := trotate_left(h);
  if is_red_left(h) and is_red_left(h.left) then h := trotate_right(h);
  if is_red_left(h) and is_red_right(h) then begin
    h.isred:=true;
    h.left.isred:=false;
    h.right.isred:=false;
  end;
  result:= h;
end;

function TCYTree.tinsert(h: P_YTreeItem; var item): P_YTreeItem;
var cmp : integer;
begin
  cmp := FCompareValue(item,h.data);
  if cmp = 0 then begin
  // here you can implement multimap =) for usual tree we use overwriting
  // h.value :=value;
    FMoveField(item, h.data, fFieldSize); // COPY
    finsert:=h;
  end
  else if cmp < 0 then begin  // insert new node at left
    if h.lleaf then begin // insert here
      h.lleaf:=false;
      Result := create_node(item);
      result.left:=h.left;
      result.right:=h;
      h.left:=result;
      if result.left = nil then ffirst:=result;
    end else h.left:= tinsert(h.left, item) // pass to next
  end else begin              // insert new node at right
    if h.rleaf then begin // insert here
      h.rleaf:=false;
      Result := create_node(item);
      result.right:=h.right;
      result.left:=h;
      h.right:=result;
      if result.right = nil then flast:=result;
    end else h.right:= tinsert(h.right, item);
  end;
  result:=tfix_up(h);
end;

function TCYTree.tnext(h: P_YTreeItem): P_YTreeItem;
begin
  result:=h.right;
  if h.rleaf then exit;
  while not result.lleaf do result:=result.left;
end;

function TCYTree.tprev(h: P_YTreeItem): P_YTreeItem;
begin
  result:=h.left;
  if h.lleaf then exit;
  while not result.rleaf do result:=result.right;
end;

function TCYTree.tpush_red_left(h: P_YTreeItem): P_YTreeItem;
begin
  result:=h;
  if is_red_left(h) then exit;
  if is_red_left(h.left) then exit;

  h.isred := not h.isred;
  h.left.isred := true;
  h.right.isred := not h.right.isred;

  if is_red_left(h.right) then begin
    h.right := trotate_right(h.right);
    h := trotate_left(h);
    h.isred := not h.isred;
    h.left.isred := not h.left.isred;
    h.right.isred := not h.right.isred;
  end;
  result:=h;
end;

function TCYTree.trotate_left(h: P_YTreeItem): P_YTreeItem;
begin
  result:= h.right;
  h.right:= result.left;
  if result.lleaf then begin
    h.rleaf:=true;
    h.right:=result;
  end;
  result.left:= h;
  result.lleaf:=false;
  result.isred:= h.isred;
  h.isred:= true;
end;

function TCYTree.trotate_right(h: P_YTreeItem): P_YTreeItem;
begin
  result := h.left;
  h.left:= result.right;
  if result.rleaf then begin
    h.lleaf:=true;
    h.left:=result;
  end;
  result.right:= h;
  result.rleaf:=false;
  result.isred := h.isred;
  h.isred := true;
end;

function TCYTree.tsearch(var item): P_YTreeItem;
var cmp : integer;
begin
  result := froot;
  while (result<>nil) do begin
    cmp := FCompareValue(item, result.data);
    if (cmp = 0) then exit;
    if (cmp < 0) then if result.lleaf then break else result := result.left
    else              if result.rleaf then break else result := result.right;
  end;
  result:=nil;
end;

{ TCAATree }

constructor TCAATree.Create;
begin
  inherited;
  FBottom := AllocNewItem;
  dec(FCount);
  FBottom.Level := 0;
  FBottom.Left := FBottom;
  FBottom.Right := FBottom;
  FDeleted := FBottom;
  FRoot := FBottom;
  FLast := FBottom;
end;

destructor TCAATree.Destroy;
begin
  clear;
  ReleaseItem(FBottom);
  inc(FCount);
  FBottom := nil;
  FDeleted := nil;
  FRoot := nil;
  inherited;
end;

procedure TCAATree.SetCount(n: integer);
begin
  assert(n=0, erOperation);
  if n=0 then
    clear;
end;

function TCAATree.AllocNewItem: P_AATreeItem;
begin
  Result := AllocMem(sizeof(T_AATreeItem) + FItemSize);
  inc(FCount);
end;

procedure TCAATree.ReleaseItem(p: P_AATreeItem);
begin
  reallocmem(p, 0);
  dec(FCount);
end;

procedure TCAATree.treeClear(p: P_AATreeItem);
begin
  if (p=nil) or (p=FBottom) then
    exit;
  treeClear(p.Left);
  treeClear(p.Right);
  if FDelCount>0 then
    ItemDeleted(PtrToHandle(p));
  releaseItem(p);
end;

function TCAATree.PtrTohandle(p: P_AATreeItem): TCHandle;
begin
  if (p = nil) or (p=FBottom) then
    Result := -1
  else
    Result := TCHandle(p);
end;

function TCAATree.treeGetHeight(p: P_AATreeItem): integer;
begin
  if p=FBottom then
    result := 0
  else
    result := max(treeGetHeight(p.Left), treeGetHeight(p.Right)) + 1;
end;

function TCAATree.treeFullHeight: integer;
begin
  result := treeGetHeight(FRoot);
end;

function TCAATree.treeMin(p: P_AATreeItem): P_AATreeItem;
begin
  if (p=nil) or (p=FBottom) then
    result := nil
  else
  begin
    result := p;
    while result.Left <> FBottom do
      result := result.Left;
  end;
end;

function TCAATree.treeMax(p: P_AATreeItem): P_AATreeItem;
begin
  if (p=nil) or (p=FBottom) then
    result := nil
  else
  begin
    result := p;
    while result.Right <> FBottom do
      result := result.Right;
  end;
end;

function TCAATree.treeFind(var Item): P_AATreeItem;
var n: integer;
begin
  result := FRoot;
  while result<>FBottom do
  begin
    n := FCompareValue(item, result.Data);
    if n<0 then
      result := result.Left
    else
      if n>0 then
        result := result.Right
      else
        exit;
  end;
end;

function TCAATree.treeSuccessor(p: P_AATreeItem): P_AATreeItem;
begin
  if (p=nil) or (p=FBottom) then
    result := nil
  else
    if p.Right <> FBottom then
      result := treeMin(p.Right)
    else
    begin
      result := p.Parent;
      while (result<>FBottom) and (p=result.Right) do
      begin
        p := result;
        result := result.Parent;
      end;
    end;
end;

function TCAATree.treePredecessor(p: P_AATreeItem): P_AATreeItem;
begin
  if (p=nil) or (p=FBottom) then
    result := nil
  else
    if p.Left <> FBottom then
      result := treeMax(p.Left)
    else
    begin
      result := p.Parent;
      while (result<>FBottom) and (p=result.Left) do
      begin
        p := result;
        result := result.Parent;
      end;
    end;
end;

function TCAATree.treeAdd(p,aparent: P_AATreeItem; var dst: P_AATreeItem): Boolean;
var r: integer;
begin
  if dst = FBottom then
    with p^ do
    begin
      Parent := AParent;
      Left := FBottom;
      Right := FBottom;
      Level := 1;
      dst := p;
      result := true;
      exit;
    end;
  r := FCompareValue(p.data, dst.Data);
  if r>0 then
    result := treeAdd(p, dst, dst.right)
  else
  if r<0 then
    result := treeAdd(p, dst, dst.left)
  else
    result := false;
  if not result then
    exit;
  treeSkew(dst);
  treeSplit(dst);
end;

{
  Src: 1(p)   Dst:  2(p)
       / \          / \
     2(t) X        Y  1(t)
     / \              / \
    Y   Z            Z   X
}
procedure TCAATree.treeSkew(var p: P_AATreeItem);
var
  t: P_AATreeItem;
begin
  if (p.Left.Level = p.Level) then
  begin

    // change Right&Left links
    t := p;
    p := p.left;
    t.left := p.right;
    p.right := t;

    // change Parent links
    p.Parent := t.Parent;
    t.Parent := p;
    t.Left.Parent := t;
  end;
end;

{
  Src: 1(p)     Dst:  2(p)
       / \            /  \
     X  2(t)        1(t)  Z
        /  \        /  \
       Y    Z      X    Y
}
procedure TCAATree.treeSplit(var p: P_AATreeItem);
var
  t: P_AATreeItem;
begin
  if (p.Right.Right.Level = p.Level) then
  begin

    // change Right&Left links
    t := p;
    p := p.Right;
    t.Right := p.Left;
    p.Left := t;
    inc(p.Level);

    // change Parent links
    p.Parent := t.Parent;
    t.Parent := p;
    t.Right.Parent := t;
  end;
end;

procedure TCAATree.treeMove(src, dst: P_AATreeItem);
begin
  FMoveItem(src.Data, dst.Data, FItemSize);
end;

function TCAATree.treeDelete(x: P_AATreeItem; var t: P_AATreeItem):boolean;
begin
  result := false;
  if (t=nil) or (t=FBottom) then
    exit;

  // search down the tree and set pointers last and deleted
  Flast := t;
  if FCompareValue(x.data, t.Data) < 0 then
    result := treeDelete(x, t.Left)
  else
  begin
    FDeleted := t;
    result := treeDelete(x, t.Right);
  end;

  // At the bottom of the tree we remove the element (if it is present)
  if (t = FLast) and (FDeleted <> FBottom) and (FCompareValue(x.data, FDeleted.Data)=0) then
  begin
    if FDelCount>0 then
      ItemDeleted(PtrToHandle(FDeleted));
    // we copy key-field only
    // it is necessary to rebalance tree and move
    // FDeleted into right position (position for FLast)
    if FLast<>FDeleted then
      FMoveField(FLast.Data, FDeleted.Data, FFieldSize);
    t.Right.Parent := t.Parent;
    t := t.Right;
    result := true;
  end
  else
    // On the way back, we rebalance
    if (t.Left.Level < t.Level-1) or (t.Right.Level < t.Level-1) then
    begin
      dec(t.Level);
      if t.Right.Level > t.Level then
        t.right.level := t.level;
      treeSkew(t);
      treeSkew(t.right);
      treeSkew(t.right.right);
      treeSplit(t);
      treeSplit(t.right);
    end;
end;

function TCAATree.EOF(n: TCHandle): boolean;
begin
  Result := n = -1;
end;

function TCAATree.Get(handle: TCHandle): Pointer;
begin
  assert(handle<>-1,erhandle);
  Result := @P_AATreeItem(handle).Data;
end;

procedure TCAATree.Put(handle: TCHandle; var Item);
var n: integer;
begin
  assert(handle<>-1,erhandle);

  // lock ItemDeleted & delete item from structure
  n := FDelCount;
  FDelCount := 0;
  // ----
  if not treeDelete(P_AATreeItem(handle), FRoot) then
    error(erError);
  // content of FDeleted has cleaned & replaced with key from FLast
  // now we move FLast into position of FDeleted
  if FLast<>FDeleted then
  begin
    treeMove(FLast, FDeleted);
    treeReplace(FLast, FDeleted);
  end;
  // ItemDeleted for deleted item has called from treeDelete
  // so we must not call it again
  assert(handle=TCHandle(FDeleted));
  FDeleted := FBottom;
  FLast := FBottom;
  // ----
  FDelCount := n;

  // fill item with new key and put to structure
  FMoveField(item, P_AATreeItem(handle).Data, FFieldSize);
  // new key can conflict with another key!
  if not treeAdd(P_AATreeItem(handle), FBottom, FRoot) then
  begin
    if FDelCount>0 then
      ItemDeleted(handle);
    ReleaseItem(P_AATreeItem(handle));
  end;
end;

function TCAATree.AddItem(var item): TCHandle;
var p: P_AATreeItem;
begin
  p := AllocNewItem;
  FMoveField(item, p.Data, FFieldSize);
  if treeAdd(p, FBottom, FRoot) then
    result := PtrToHandle(p)
  else
  begin
    ReleaseItem(p);
    result := -1;
  end;
end;

procedure TCAATree.Clear;
begin
  if FCount=0 then
    exit;
  treeClear(FRoot);
  FBottom.Level := 0;
  FBottom.Left := FBottom;
  FBottom.Right := FBottom;
  FDeleted := FBottom;
  FRoot := FBottom;
end;

// replace position of DST item in the tree with SRC item
procedure TCAATree.treeReplace(src,dst: P_AATreeItem);
begin
  src.Parent := dst.Parent;
  src.Left := dst.Left;
  src.Right := dst.Right;
  src.Level := dst.Level;

  // root item has "parent=FBottom"
  // but FBottom.left/right MUST refer to FBottom
  if src.Parent<>FBottom then
    if src.Parent.Left=dst then
      src.Parent.Left := src
    else
      src.Parent.Right := src;
  src.Left.Parent := src;
  src.Right.Parent := src;

  if FRoot=dst then
    FRoot := src;
end;

procedure TCAATree.Delete(handle: TCHandle);
begin
  if not treeDelete(P_AATreeItem(handle), FRoot) then
    exit;
  // content of FDeleted has cleaned & replaced with key from FLast
  // now we move FLast into position of FDeleted
  if FLast<>FDeleted then
  begin
    treeMove(FLast, FDeleted);
    treeReplace(FLast, FDeleted);
  end;
  // ItemDeleted for deleted item has called from treeDelete
  // so we must not call it again
  ReleaseItem(FDeleted);
  FDeleted := FBottom;
  FLast := FBottom;
end;

function TCAATree.First: TCHandle;
begin
  result := PtrTohandle( treeMin(FRoot) );
end;

function TCAATree.Last: TCHandle;
begin
  result := PtrTohandle( treeMax(FRoot) );
end;

procedure TCAATree.Next(var n: TCHandle);
begin
  n := PtrToHandle( treeSuccessor(P_AATreeItem(n)) );
end;

procedure TCAATree.Prev(var n: TCHandle);
begin
  n := PtrToHandle( treePredecessor(P_AATreeItem(n)) );
end;

function TCAATree.FindMin: TCHandle;
begin
  result := PtrToHandle( treeMin(FRoot) );
end;

function TCAATree.FindMax: TCHandle;
begin
  result := PtrToHandle( treeMax(FRoot) );
end;

function TCAATree.Find(var Item): TCHandle;
begin
  result := PtrToHandle( treeFind(item) );
end;

function TCAATree.FindFirstEqual(var Item): TCHandle;
begin
  result := PtrToHandle( treeFind(item) );
end;

procedure TCAATree.FindNextEqual(var Item; var handle: TCHandle);
begin
  handle := -1;
end;

procedure TCAATree.Remove(var Item);
var n: TCHandle;
begin
  n := Find(Item);
  if n<>-1 then
    Delete(n);
end;

procedure TCAATree.RemoveAll(var Item);
begin
  // pure TCAATree container does not contain duplicates
  Remove(Item);
end;

function TCAATree.GetHandle(index: integer): TCHandle;
begin
  result := First;
  while (index>0) and (result<>-1) do
  begin
    dec(index);
    Next(result);
  end;
end;

function TCAATree.InsertItem(handle: TCHandle; var item): TCHandle;
begin
  error(erOperation);
  result := -1;
end;

function TCAATree.InsertAfterItem(handle: TCHandle; var item): TCHandle;
begin
  error(erOperation);
  result := -1;
end;

procedure TCAATree.Sort;
begin
  // BST is always sorted
end;

procedure TCAATree.Copy(Src, Dst: TCHandle);
begin
  error(erOperation);
end;

procedure TCAATree.Swap(handle1, handle2: TCHandle);
begin
  error(erOperation);
end;

procedure TCAATree.Grow;
begin
end;

function TCAATree.Insert(handle: TCHandle): TCHandle;
begin
  error(erOperation);
  result := -1;
end;

function TCAATree.InsertAfter(handle: TCHandle): TCHandle;
begin
  error(erOperation);
  result := -1;
end;

procedure TCAATree.Move(Curhandle, Newhandle: TCHandle);
begin
  error(erOperation);
end;

{ TCMultimap }

function TCMultimap.AllocNewItem: P_AATreeItem;
var p: P_MultiMapItem;
begin
  p := allocmem(sizeof(T_MultiMapItem)+FItemSize);
  p.List := FBottom;
  result := @p.Item;
  inc(FCount);
end;

procedure TCMultimap.ReleaseItem(p: P_AATreeItem);
begin
  if FLockRelease>0 then
    exit;
  freemem(mmGetPtr(P));
  dec(FCount);
end;

function TCMultimap.mmGetPtr(p: P_AATreeItem): P_MultiMapItem;
var t: ^pointer;
begin
  t := pointer(p);
  dec(t);
  result := pointer(t);
end;

procedure TCMultimap.treeClear(p: P_AATreeItem);
var
  m,d: P_AATreeItem;
begin
  if (p=nil) or (p=FBottom) then
    exit;
  treeClear(p.Left);
  treeClear(p.Right);
  m := mmGetPtr(p).List;
  if m<>FBottom then
    repeat
      d := m;
      m := m.Right;
      if FDelCount>0 then
        ItemDeleted(PtrToHandle(d));
      releaseItem(d);
    until m=nil;
  if FDelCount>0 then
    ItemDeleted(PtrToHandle(p));
  releaseItem(p);
end;

function TCMultimap.mmAddDuplicate(var item; src: P_AATreeItem; dst: P_MultiMapItem): TCHandle;
begin
  FMoveField(item, src.Data, FFieldSize);
  if dst.List=FBottom then
    src.Right := nil
  else
    src.Right := dst.List;
  if src.Right<>nil then
    src.Right.Left := src;
  mmGetPtr(src).List := nil;
  dst.List := src;
  src.Left := @dst.Item;
  // dst.List = src, src.List = nil, so this part is ok
  result := PtrToHandle(src);
end;

function TCMultimap.AddItem(var item): TCHandle;
begin
  result := Find(item);
  if result=-1 then
    result := inherited AddItem(item)
  else
    result := mmAddDuplicate(item, AllocNewItem, mmGetPtr(P_AATreeItem(result)) );
end;

procedure TCMultimap.Put(handle: TCHandle; var Item);
var
  r: TCHandle;
  n: integer;
begin
  assert((handle<>-1) and (P_AATreeItem(handle)<>FBottom));

  // lock .ReleaseItem and delete item from structure
  inc(FLockRelease);
  n := FDelCount;
  FDelCount := 0;
  Delete(handle);
  FDelCount := n;
  dec(FLockRelease);

  // put item to structure with new Key
  r := Find(item);
  if r<>-1 then
    mmAddDuplicate(item, pointer(handle), mmGetPtr(P_AATreeItem(r)) )
  else
  begin
    FMoveField(item, P_AATreeItem(handle).Data, FFieldSize);
    with mmGetPtr(P_AATreeItem(handle))^ do
    begin
      List := FBottom;
      Item.Left := nil;
      Item.Right := nil;
    end;
    if not treeAdd(P_AATreeItem(handle), FBottom, FRoot) then
      error(erError);
  end;
end;

procedure TCMultimap.Delete(handle: TCHandle);
var
  p,l,r: P_MultiMapItem;
begin
  if (handle=-1) or (P_AATreeItem(handle)=FBottom) then
    exit;
  p := mmGetPtr(P_AATreeItem(handle));

  // item without duplicates (regular AA-tree item)
  if p.List=FBottom then
  begin
    inherited delete(handle);
    exit;
  end;

  if FDelCount>0 then
    ItemDeleted(handle);

  // item in list of duplicates
  if p.List=nil then
  begin
    l := mmGetPtr(p.Item.Left);
    if p.Item.Right <> nil  then
      p.Item.Right.Left := @l.Item;

    // left item it is also item in list of duplicates
    if l.List=nil then
      l.Item.Right := p.Item.Right
    else
      // left item it is first duplicate
      // if it is last item in chain, then we do not have duplicates anymore
      if p.Item.Right=nil then
        l.List := FBottom
      else
        // otherwise we set parent.list to next duplicate
        l.List := p.Item.Right;
  end
  else
  begin
    // we here only when it is first item in list (not empty) of duplicates
    r := mmGetPtr(p.list);
    // right item is last one
    if r.Item.Right=nil then
      r.List := FBottom
    else
      r.List := r.Item.Right;
    treeReplace(@r.item, @p.Item);
  end;

  ReleaseItem(@p.Item);
end;

procedure TCMultimap.RemoveAll(var Item);
var
  t,q: P_AATreeItem;
  p: P_MultiMapItem;
begin
  t := treeFind(item);
  if t=FBottom then
    exit;
  p := mmGetPtr(t);
  if p.List<>FBottom then
  begin
    t := p.List;
    while t<>nil do
    begin
      q := t;
      t := t.Right;
      if FDelCount>0 then
        ItemDeleted(PtrToHandle(q));
      ReleaseItem(q);
    end;
  end;
  p.List := FBottom;
  inherited delete(PtrToHandle(@p.Item));
end;

procedure TCMultimap.FindNextEqual(var Item; var handle: TCHandle);
var p: P_MultiMapItem;
begin
  if (P_AATreeItem(handle)=FBottom) then
    handle := -1;
  if (handle=-1) then
    exit;
  p := mmGetPtr(P_AATreeItem(handle));

  // it is item from list of duplicates - return next item in list
  // it is first item in list of duplicates - return first duplicate
  // otherwise item does not have duplicates - return -1
  if p.List=nil then
    handle := PtrToHandle( p.Item.Right )
  else
    if p.List<>FBottom then
      handle := PtrToHandle(p.List)
    else
      handle := -1;
end;

function TCMultimap.Last: TCHandle;
var
  p: P_MultiMapItem;
  m: P_AATreeItem;
begin
  result := inherited Last;
  if result=-1 then
    exit;
  p := mmGetPtr(P_AATreeItem(result));
  if p.List<>FBottom then
  begin
    m := p.List;
    while m.Right<>nil do
      m := m.Right;
    result := PtrToHandle(m);
  end;
end;

procedure TCMultimap.Next(var n: TCHandle);
var
  p: P_MultiMapItem;
begin
  if (n=-1) or (P_AATreeItem(n)=FBottom) then
  begin
    n := -1;
    exit;
  end;
  p := mmGetPtr(P_AATreeItem(n));

  // item without duplicates (regular item of AA-tree)
  if p.List=FBottom then
    inherited Next(n)
  else

    // first item in list (not empty) of dulicates
    if p.List<>nil then
      n := PtrToHandle(p.List)
    else

      // item in list of duplicates
      if p.Item.Right<>nil then
        n := PtrToHandle(p.Item.Right)
      else
      begin
        repeat
          p := mmGetPtr(p.item.left);
        until p.List<>nil;
        n := PtrToHandle(@p.Item);
        inherited Next(n);
      end;
end;

procedure TCMultimap.Prev(var n: TCHandle);
var
  p: P_MultiMapItem;
  m: P_AATreeItem;
begin
  if (n=-1) or (P_AATreeItem(n)=FBottom) then
  begin
    n := -1;
    exit;
  end;

  p := mmGetPtr(P_AATreeItem(n));

  // item in list of duplicates
  if (p.List=nil) then
    n := PtrToHandle(p.Item.Left)
  else

  // item without duplicates (regular item of AA-tree)
  // or first item in list of duplicates
  begin
    inherited Prev(n);
    if n=-1 then
      exit;
    p := mmGetPtr(P_AATreeItem(n));
    if p.List=FBottom then
      n := PtrToHandle(@p.Item)
    else
    begin
      m := p.List;
      while m.Right<>nil do
        m := m.Right;
      n := PtrToHandle(m);
    end;
  end;
end;

{ TTUInt64 }

{$IFDEF HiCompiler}

procedure SortMemoryRangeUInt64(Buf: pointer; L, R: Integer);
begin
{$IFDEF FPC}
  if R>L then
    VectorUInt64QuickSort(@PUInt64List(Buf)[L], R-L+1);
{$ELSE}
  if R>L then
    VectorUInt64JumpSort(@PUInt64List(Buf)[L], R-L+1);
{$ENDIF}
end;

constructor TTUInt64.Create(Container: TCCustom);
begin
  FieldSize := sizeof(TUInt64Type);
  FSortMemoryRange := @SortMemoryRangeUInt64;
  inherited;
end;

function TTUInt64.Add(Item: TUInt64Type): TCHandle;
begin
  Result := FContainer.AddItem(item);
end;

function  TTUInt64.Front: TUInt64Type;          // examine first element
begin
  result := items[FContainer.First];
end;

function  TTUInt64.Back: TUInt64Type;           // examine last element
begin
  result := items[FContainer.Last];
end;

function TTUInt64.Push(v: TUInt64Type): TCHandle;        // insert element at front
begin
  result := FContainer.InsertItem(FContainer.First, v);
end;

function  TTUInt64.Pop: TUInt64Type;            // remove first element
var
  h: TCHandle;
begin
  h := FContainer.First;
  Result := items[h];
  FContainer.Delete(h);
end;

function TTUInt64.PushBack(v: TUInt64Type): TCHandle;    // insert element at back
begin
  result := FContainer.AddItem(v);
end;

function  TTUInt64.PopBack: TUInt64Type;        // remove last element
var
  h: TCHandle;
begin
  h := FContainer.Last;
  result := items[h];
  FContainer.Delete(h);
end;

function TTUInt64.Enqueue(v: TUInt64Type): TCHandle;    // add to tail
begin
  result := FContainer.AddItem(v);
end;

function  TTUInt64.Dequeue: TUInt64Type;       // read&remove from head
var
  h: TCHandle;
begin
  h := FContainer.First;
  Result := items[h];
  FContainer.Delete(h);
end;

function TTUInt64.AddList(const Items: array of const): TCHandle;
var
  i: integer;
  p: PVarRec;
begin
  Result := -1;
  for i := low(items) to high(items) do
  begin
    p := @items[i];
    if p.VType = vtObject then
      Result := ImportObject(p.VObject, Result)
    else
      Result := add(VarAsUInt64(p));
  end;
end;

function TTUInt64.AddTo(handle: TCHandle; dst: TTCustom): TCHandle;
begin
  Result := dst.Add([items[handle]]);
end;

function TTUInt64.Find(Item: TUInt64Type): TCHandle;
begin
  Result := FContainer.Find(item);
end;

function TTUInt64.FindFirstEqual(Item: TUInt64Type): TCHandle;
begin
  Result := FContainer.FindFirstEqual(item);
end;

procedure TTUInt64.FindNextEqual(Item: TUInt64Type; var handle: TCHandle);
begin
  FContainer.FindNextEqual(item, handle);
end;

function TTUInt64.GetAsVariant(handle: TCHandle): variant;
begin
  if handle=-1 then
    Result := null
  else
    Result := items[handle];
end;

function TTUInt64.GetHandleOf(Value: variant): TCHandle;
begin
  Result := Find(Value);
end;

function TTUInt64.GetItem(n: TCHandle): TUInt64Type;
begin
  Result := TUInt64Type(FContainer.Get(n)^);
end;

function TTUInt64.GetMinValue: TUInt64Type;
begin
  Result := TUInt64Type(FContainer.Get(FContainer.FindMin)^);
end;

function TTUInt64.GetMaxValue: TUInt64Type;
begin
  Result := TUInt64Type(FContainer.Get(FContainer.FindMax)^);
end;

function TTUInt64.Insert(handle: TCHandle; Item: TUInt64Type): TCHandle;
begin
  Result := FContainer.InsertItem(handle, item);
end;

function TTUInt64.InsertAfter(handle: TCHandle; Item: TUInt64Type): TCHandle;
begin
  Result := FContainer.InsertAfterItem(handle, item);
end;

procedure TTUInt64.Remove(Item: TUInt64Type);
begin
  FContainer.Remove(item);
end;

procedure TTUInt64.RemoveAll(Item: TUInt64Type);
begin
  FContainer.RemoveAll(item);
end;

function TTUInt64.RemoveMinValue: TUInt64Type;
var
  n: TCHandle;
begin
  n := FContainer.FindMin;
  Result := TUInt64Type(FContainer.Get(n)^);
  FContainer.Delete(n);
end;

function TTUInt64.RemoveMaxValue: TUInt64Type;
var
  n: TCHandle;
begin
  n := FContainer.FindMax;
  Result := TUInt64Type(FContainer.Get(n)^);
  FContainer.Delete(n);
end;

procedure TTUInt64.SetAsVariant(handle: TCHandle; Value: variant);
begin
  items[handle] := Value;
end;

function TTUInt64.GetAsAnsiStr(handle: TCHandle): AnsiString;
begin
  result := AnsiString(IntToStr(items[handle]));
end;

procedure TTUInt64.SetAsAnsiStr(handle: TCHandle; Value: AnsiString);
begin
  items[handle] := StrToInt64(String(Value));
end;

function TTUInt64.GetAsWideStr(handle: TCHandle): WideString;
begin
  result := IntToStr(items[handle]);
end;

procedure TTUInt64.SetAsWideStr(handle: TCHandle; Value: WideString);
begin
  items[handle] := StrToInt64(Value);
end;

procedure TTUInt64.SetItem(n: TCHandle; Value: TUInt64Type);
begin
  FContainer.Put(n, Value);
end;

function TTUInt64.AddPair(key: TUInt64Type; Value: variant): TCHandle;
begin
  result := Add(key);
  Container.Fields[1].AsVariant[result] := Value;
end;

procedure TTUInt64.InitContainer(c: TCCustom);
begin
  inherited;
  c.FCompareValue := CmpInt;
end;

function TTUInt64.CmpInt(var a,b):integer;
begin
  if TUInt64Type(a)<TUInt64Type(b) then
    result := -1
  else
    if TUInt64Type(a)=TUInt64Type(b) then
      result := 0
    else
      result := 1;
end;

procedure TTUInt64.SetOnCompare(c: TOnUInt64Compare);
begin
  if assigned(c) then
    container.FCompareValue := CmpExt
  else
    container.FCompareValue := CmpInt;
  FOnCompare := c;
  FOnCompareProc := nil;
end;

procedure TTUInt64.SetOnCompareProc(c: TOnUInt64CompareProc);
begin
  if assigned(c) then
    container.FCompareValue := CmpExtProc
  else
    container.FCompareValue := CmpInt;
  FOnCompareProc := c;
  FOnCompare := nil;
end;

function TTUInt64.CmpExt(var a, b): integer;
begin
  Result := FOnCompare(TUInt64Type(a), TUInt64Type(b));
end;

function TTUInt64.CmpExtProc(var a, b): integer;
begin
  Result := FOnCompareProc(TUInt64Type(a), TUInt64Type(b));
end;

procedure TTUInt64.Sort(compare: TOnUInt64Compare);
begin
  Sort(compare, FContainer.First, FContainer.Last);
end;

procedure TTUInt64.Sort(compare: TOnUInt64CompareProc);
begin
  Sort(compare, FContainer.First, FContainer.Last);
end;

procedure TTUInt64.Sort(compare: TOnUInt64Compare; AFirst, ALast: TCHandle);
begin
  FOnCompare := compare;
  FContainer.Sort(CmpExt, AFirst, ALast);
end;

procedure TTUInt64.Sort(compare: TOnUInt64CompareProc; AFirst, ALast: TCHandle);
begin
  FOnCompareProc := compare;
  FContainer.Sort(CmpExtProc, AFirst, ALast);
end;

function TTUInt64.STCopy(Dst: TTCustom; h: TCHandle):TCHandle;
begin
  result := TTUInt64(dst).Add(Items[h]);
end;

procedure TTUInt64.STCopy(Dst: TTCustom; AFirst, ALast: TCHandle);
begin
  TTUInt64(dst).Add(Items[AFirst]);
  if AFirst<>ALast then
    repeat
      FContainer.Next(AFirst);
      TTUInt64(dst).Add(Items[AFirst]);
    until AFirst=ALast;
end;

procedure TTUInt64.STSet(Dst: TTCustom; h1,h2: TCHandle);
begin
  TTUInt64(dst)[h2] := Items[h1];
end;

procedure TTUInt64.STSet(Dst: TTCustom; start1,finish1, start2: TCHandle);
begin
  TTUInt64(dst)[start2] := Items[start1];
  if start1<>finish1 then
    repeat
      FContainer.Next(start1);
      dst.FContainer.Next(start2);
      TTUInt64(dst)[start2] := Items[start1];
    until start1=finish1;
end;

function TTUInt64.GetMap(key: TUInt64Type): variant;
begin
  Result := FContainer.Fields[1].AsVariant[Find(key)];
end;

procedure TTUInt64.PutMap(key: TUInt64Type; Value: variant);
var
  h: TCHandle;
begin
  h := Find(key);
  if h = -1 then
    AddPair(key, Value)
  else
    FContainer.Fields[1].AsVariant[h] := Value;
end;

{$ENDIF}

// TTInt64
procedure SortMemoryRangeInt64(Buf: pointer; L, R: Integer);
begin
{$IFDEF FPC}
  if R>L then
    VectorInt64QuickSort(@PInt64List(Buf)[L], R-L+1);
{$ELSE}
  if R>L then
    VectorInt64JumpSort(@PInt64List(Buf)[L], R-L+1);
{$ENDIF}
end;

constructor TTInt64.Create(Container: TCCustom);
begin
  FieldSize := sizeof(TInt64Type);
  FSortMemoryRange := @SortMemoryRangeInt64;
  inherited;
end;

function TTInt64.Add(Item: TInt64Type): TCHandle;
begin
  Result := FContainer.AddItem(item);
end;

function  TTInt64.Front: TInt64Type;          // examine first element
begin
  result := items[FContainer.First];
end;

function  TTInt64.Back: TInt64Type;           // examine last element
begin
  result := items[FContainer.Last];
end;

function TTInt64.Push(v: TInt64Type): TCHandle;        // insert element at front
begin
  result := FContainer.InsertItem(FContainer.First, v);
end;

function  TTInt64.Pop: TInt64Type;            // remove first element
var
  h: TCHandle;
begin
  h := FContainer.First;
  Result := items[h];
  FContainer.Delete(h);
end;

function TTInt64.PushBack(v: TInt64Type): TCHandle;    // insert element at back
begin
  result := FContainer.AddItem(v);
end;

function  TTInt64.PopBack: TInt64Type;        // remove last element
var
  h: TCHandle;
begin
  h := FContainer.Last;
  result := items[h];
  FContainer.Delete(h);
end;

function TTInt64.Enqueue(v: TInt64Type): TCHandle;    // add to tail
begin
  result := FContainer.AddItem(v);
end;

function  TTInt64.Dequeue: TInt64Type;       // read&remove from head
var
  h: TCHandle;
begin
  h := FContainer.First;
  Result := items[h];
  FContainer.Delete(h);
end;

function TTInt64.AddList(const Items: array of const): TCHandle;
var
  i: integer;
  p: PVarRec;
begin
  Result := -1;
  for i := low(items) to high(items) do
  begin
    p := @items[i];
    if p.VType = vtObject then
      Result := ImportObject(p.VObject, Result)
    else
      Result := add(VarAsInt64(p));
  end;
end;

function TTInt64.AddTo(handle: TCHandle; dst: TTCustom): TCHandle;
begin
  Result := dst.Add([items[handle]]);
end;

function TTInt64.Find(Item: TInt64Type): TCHandle;
begin
  Result := FContainer.Find(item);
end;

function TTInt64.FindFirstEqual(Item: TInt64Type): TCHandle;
begin
  Result := FContainer.FindFirstEqual(item);
end;

procedure TTInt64.FindNextEqual(Item: TInt64Type; var handle: TCHandle);
begin
  FContainer.FindNextEqual(item, handle);
end;

function TTInt64.GetAsVariant(handle: TCHandle): variant;
begin
  if handle=-1 then
    Result := null
  else
    Result := items[handle];
end;

function TTInt64.GetHandleOf(Value: variant): TCHandle;
begin
  Result := Find(Value);
end;

function TTInt64.GetItem(n: TCHandle): TInt64Type;
begin
  Result := TInt64Type(FContainer.Get(n)^);
end;

function TTInt64.GetMinValue: TInt64Type;
begin
  Result := TInt64Type(FContainer.Get(FContainer.FindMin)^);
end;

function TTInt64.GetMaxValue: TInt64Type;
begin
  Result := TInt64Type(FContainer.Get(FContainer.FindMax)^);
end;

function TTInt64.Insert(handle: TCHandle; Item: TInt64Type): TCHandle;
begin
  Result := FContainer.InsertItem(handle, item);
end;

function TTInt64.InsertAfter(handle: TCHandle; Item: TInt64Type): TCHandle;
begin
  Result := FContainer.InsertAfterItem(handle, item);
end;

procedure TTInt64.Remove(Item: TInt64Type);
begin
  FContainer.Remove(item);
end;

procedure TTInt64.RemoveAll(Item: TInt64Type);
begin
  FContainer.RemoveAll(item);
end;

function TTInt64.RemoveMinValue: TInt64Type;
var
  n: TCHandle;
begin
  n := FContainer.FindMin;
  Result := TInt64Type(FContainer.Get(n)^);
  FContainer.Delete(n);
end;

function TTInt64.RemoveMaxValue: TInt64Type;
var
  n: TCHandle;
begin
  n := FContainer.FindMax;
  Result := TInt64Type(FContainer.Get(n)^);
  FContainer.Delete(n);
end;

procedure TTInt64.SetAsVariant(handle: TCHandle; Value: variant);
begin
  items[handle] := Value;
end;

function TTInt64.GetAsAnsiStr(handle: TCHandle): AnsiString;
begin
  result := AnsiString(IntToStr(items[handle]));
end;

procedure TTInt64.SetAsAnsiStr(handle: TCHandle; Value: AnsiString);
begin
  items[handle] := StrToInt64(String(Value));
end;

function TTInt64.GetAsWideStr(handle: TCHandle): WideString;
begin
  result := IntToStr(items[handle]);
end;

procedure TTInt64.SetAsWideStr(handle: TCHandle; Value: WideString);
begin
  items[handle] := StrToInt64(Value);
end;

procedure TTInt64.SetItem(n: TCHandle; Value: TInt64Type);
begin
  FContainer.Put(n, Value);
end;

function TTInt64.AddPair(key: TInt64Type; Value: variant): TCHandle;
begin
  result := Add(key);
  Container.Fields[1].AsVariant[result] := Value;
end;

procedure TTInt64.InitContainer(c: TCCustom);
begin
  inherited;
  c.FCompareValue := CmpInt;
end;

function TTInt64.CmpInt(var a,b):integer;
begin
  if TInt64Type(a)<TInt64Type(b) then
    result := -1
  else
    if TInt64Type(a)=TInt64Type(b) then
      result := 0
    else
      result := 1;
end;

procedure TTInt64.SetOnCompare(c: TOnInt64Compare);
begin
  if assigned(c) then
    container.FCompareValue := CmpExt
  else
    container.FCompareValue := CmpInt;
  FOnCompare := c;
  FOnCompareProc := nil;
end;

procedure TTInt64.SetOnCompareProc(c: TOnInt64CompareProc);
begin
  if assigned(c) then
    container.FCompareValue := CmpExtProc
  else
    container.FCompareValue := CmpInt;
  FOnCompareProc := c;
  FOnCompare := nil;
end;

function TTInt64.CmpExt(var a, b): integer;
begin
  Result := FOnCompare(TInt64Type(a), TInt64Type(b));
end;

function TTInt64.CmpExtProc(var a, b): integer;
begin
  Result := FOnCompareProc(TInt64Type(a), TInt64Type(b));
end;

procedure TTInt64.Sort(compare: TOnInt64Compare);
begin
  Sort(compare, FContainer.First, FContainer.Last);
end;

procedure TTInt64.Sort(compare: TOnInt64CompareProc);
begin
  Sort(compare, FContainer.First, FContainer.Last);
end;

procedure TTInt64.Sort(compare: TOnInt64Compare; AFirst, ALast: TCHandle);
begin
  FOnCompare := compare;
  FContainer.Sort(CmpExt, AFirst, ALast);
end;

procedure TTInt64.Sort(compare: TOnInt64CompareProc; AFirst, ALast: TCHandle);
begin
  FOnCompareProc := compare;
  FContainer.Sort(CmpExtProc, AFirst, ALast);
end;

function TTInt64.STCopy(Dst: TTCustom; h: TCHandle):TCHandle;
begin
  result := TTInt64(dst).Add(Items[h]);
end;

procedure TTInt64.STCopy(Dst: TTCustom; AFirst, ALast: TCHandle);
begin
  TTInt64(dst).Add(Items[AFirst]);
  if AFirst<>ALast then
    repeat
      FContainer.Next(AFirst);
      TTInt64(dst).Add(Items[AFirst]);
    until AFirst=ALast;
end;

procedure TTInt64.STSet(Dst: TTCustom; h1,h2: TCHandle);
begin
  TTInt64(dst)[h2] := Items[h1];
end;

procedure TTInt64.STSet(Dst: TTCustom; start1,finish1, start2: TCHandle);
begin
  TTInt64(dst)[start2] := Items[start1];
  if start1<>finish1 then
    repeat
      FContainer.Next(start1);
      dst.FContainer.Next(start2);
      TTInt64(dst)[start2] := Items[start1];
    until start1=finish1;
end;

function TTInt64.GetMap(key: TInt64Type): variant;
begin
  Result := FContainer.Fields[1].AsVariant[Find(key)];
end;

procedure TTInt64.PutMap(key: TInt64Type; Value: variant);
var
  h: TCHandle;
begin
  h := Find(key);
  if h = -1 then
    AddPair(key, Value)
  else
    FContainer.Fields[1].AsVariant[h] := Value;
end;

{ TTVariant }

function TTVariant.Add(Item: TVariantType): TCHandle;
begin
  Result := FContainer.AddItem(item);
  if result<>-1 then
  {$IFDEF FPC}
    FillChar(Item, sizeof(ITem), 0);
  {$ELSE}
  asm
     xor eax,eax
     lea edx,Item
     mov [edx],eax
     mov [edx+4],eax
     mov [edx+8],eax
     mov [edx+12],eax
  end;
  {$ENDIF}
end;

function  TTVariant.Front: TVariantType;          // examine first element
begin
  result := items[FContainer.First];
end;

function  TTVariant.Back: TVariantType;           // examine last element
begin
  result := items[FContainer.Last];
end;

function TTVariant.Push(v: TVariantType): TCHandle;        // insert element at front
begin
  result := Insert(FContainer.First, v);
end;

function  TTVariant.Pop: TVariantType;            // remove first element
var
  h: TCHandle;
begin
  h := FContainer.First;
  Result := items[h];
  FContainer.Delete(h);
end;

function TTVariant.PushBack(v: TVariantType): TCHandle;    // insert element at back
begin
  result := Add(v);
end;

function  TTVariant.PopBack: TVariantType;        // remove last element
var
  h: TCHandle;
begin
  h := FContainer.Last;
  result := items[h];
  FContainer.Delete(h);
end;

function TTVariant.Enqueue(v: TVariantType): TCHandle;    // add to tail
begin
  result := Add(v);
end;

function  TTVariant.Dequeue: TVariantType;       // read&remove from head
var
  h: TCHandle;
begin
  h := FContainer.First;
  Result := items[h];
  FContainer.Delete(h);
end;

function TTVariant.AddList(const Items: array of const): TCHandle;
var
  i: integer;
  p: PVarRec;
begin
  Result := -1;
  for i := low(items) to high(items) do
  begin
    p := @items[i];
    if p.VType = vtObject then
      Result := ImportObject(p.VObject, Result)
    else
      Result := add(VarAsVariant(p));
  end;
end;

function TTVariant.AddTo(handle: TCHandle; dst: TTCustom): TCHandle;
begin
  Result := dst.Add([items[handle]]);
end;

procedure SortMemoryRangeVariant(Buf: pointer; L, R: Integer);
type
  PVariantArray = ^TVariantArray;
  TVariantArray = array[0..32767] of Variant;
var
  I, J, P: Integer;
  T: Variant;
begin
  if R<=L then exit;
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while PVariantArray(buf)[I]<PVariantArray(buf)[P] do
        Inc(I);
      while PVariantArray(buf)[J]>PVariantArray(buf)[P] do
        Dec(J);
      if I <= J then
      begin
        T := PVariantArray(buf)[I];
        PVariantArray(buf)[I] := PVariantArray(buf)[J];
        PVariantArray(buf)[J] := T;
        if P = I then
          P := J
        else if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      SortMemoryRangeVariant(Buf, L, J);
    L := I;
  until I >= R;
end;

constructor TTVariant.Create(Container: TCCustom);
begin
  FieldSize := sizeof(TVariantType);
  inc(FNeedOnDelete);
  FSortMemoryRange := @SortMemoryRangeVariant;
  inherited;
end;

function TTVariant.Find(Item: TVariantType): TCHandle;
begin
  Result := FContainer.Find(item);
end;

function TTVariant.FindFirstEqual(Item: TVariantType): TCHandle;
begin
  Result := FContainer.FindFirstEqual(item);
end;

procedure TTVariant.FindNextEqual(Item: TVariantType; var handle: TCHandle);
begin
  FContainer.FindNextEqual(item, handle);
end;

function TTVariant.GetAsVariant(handle: TCHandle): variant;
begin
  if handle=-1 then
    Result := null
  else
    Result := items[handle];
end;

procedure TTVariant.SaveItem(h: TCHandle; dst: TStream; FileFormat : TFileFormat = ffDefault);
begin
  if FileFormat<>ffBinary then
    inherited
  else
    WriteVariant(dst, items[h]);
end;

function TTVariant.LoadItem(c: TCCustom; h: TCHandle; src: TStream; FileFormat : TFileFormat = ffDefault):TCHandle;
var
  t: integer;
  v: TVariantType;
begin
  if FileFormat=ffBinary then
    v := ReadVariant(src)
  else
  begin
    t := StrToInt(c.LoadToken);
    v := WideStrToVariant(c.LoadToken, t);
  end;
  if h=-1 then
    result := Add(v)
  else
  begin
    result := h;
    items[h] := v;
  end;
end;

function TTVariant.GetHandleOf(Value: variant): TCHandle;
begin
  Result := Find(Value);
end;

function TTVariant.GetItem(n: TCHandle): TVariantType;
begin
  Result := TVariantType(FContainer.Get(n)^);
end;

function TTVariant.GetMinValue: TVariantType;
begin
  Result := TVariantType(FContainer.Get(FContainer.FindMin)^);
end;

function TTVariant.GetMaxValue: TVariantType;
begin
  Result := TVariantType(FContainer.Get(FContainer.FindMax)^);
end;

function TTVariant.Insert(handle: TCHandle; Item: TVariantType): TCHandle;
begin
  Result := FContainer.InsertItem(handle, item);
end;

function TTVariant.InsertAfter(handle: TCHandle; Item: TVariantType): TCHandle;
begin
  Result := FContainer.InsertAfterItem(handle, item);
end;

procedure TTVariant.ItemDeleted(n: TCHandle);
var
  p: ^TVariantType;
begin
  p := FContainer[n];
  p^ := null;
  inherited;
end;

procedure TTVariant.Remove(Item: TVariantType);
begin
  FContainer.Remove(item);
end;

procedure TTVariant.RemoveAll(Item: TVariantType);
begin
  FContainer.RemoveAll(item);
end;

function TTVariant.RemoveMinValue: TVariantType;
var
  n: TCHandle;
begin
  n := FContainer.FindMin;
  Result := TVariantType(FContainer.Get(n)^);
  FContainer.Delete(n);
end;

function TTVariant.RemoveMaxValue: TVariantType;
var
  n: TCHandle;
begin
  n := FContainer.FindMax;
  Result := TVariantType(FContainer.Get(n)^);
  FContainer.Delete(n);
end;

procedure TTVariant.SetAsVariant(handle: TCHandle; Value: variant);
begin
  items[handle] := Value;
end;

function TTVariant.GetAsAnsiStr(handle: TCHandle): AnsiString;
begin
  result := VariantToAnsiStr(Items[handle]);
end;

procedure TTVariant.SetAsAnsiStr(handle: TCHandle; Value: AnsiString);
begin
  Items[handle] := AnsiStrToVariant(value);
end;

function TTVariant.GetAsWideStr(handle: TCHandle): WideString;
begin
  result := VariantToWideStr(Items[handle]);
end;

procedure TTVariant.SetAsWideStr(handle: TCHandle; Value: WideString);
begin
  Items[handle] := WideStrToVariant(value);
end;

procedure TTVariant.SetItem(n: TCHandle; Value: TVariantType);
begin
  FContainer.Put(n, Value);
end;

function TTVariant.AddPair(key: TVariantType; Value: variant): TCHandle;
begin
  result := Add(key);
  Container.Fields[1].AsVariant[result] := Value;
end;

procedure TTVariant.InitContainer(c: TCCustom);
begin
  inherited;
  c.FCompareValue := CmpInt;
end;

function TTVariant.CmpInt(var a,b):integer;
begin
  if TVariantType(a) < TVariantType(b) then
    result := -1
  else
    if TVariantType(a) = TVariantType(b) then
      result := 0
    else
      result := 1;
end;

procedure TTVariant.SetOnCompare(c: TOnVariantCompare);
begin
  if assigned(c) then
    container.FCompareValue := CmpExt
  else
    container.FCompareValue := CmpInt;
  FOnCompare := c;
  FOnCompareProc := nil;
end;

procedure TTVariant.SetOnCompareProc(c: TOnVariantCompareProc);
begin
  if assigned(c) then
    container.FCompareValue := CmpExtProc
  else
    container.FCompareValue := CmpInt;
  FOnCompareProc := c;
  FOnCompare := nil;
end;

function TTVariant.CmpExt(var a, b): integer;
begin
  Result := FOnCompare(TVariantType(a), TVariantType(b));
end;

function TTVariant.CmpExtProc(var a, b): integer;
begin
  Result := FOnCompareProc(TVariantType(a), TVariantType(b));
end;

procedure TTVariant.Sort(compare: TOnVariantCompare);
begin
  Sort(compare, FContainer.First, FContainer.Last);
end;

procedure TTVariant.Sort(compare: TOnVariantCompareProc);
begin
  Sort(compare, FContainer.First, FContainer.Last);
end;

procedure TTVariant.Sort(compare: TOnVariantCompare; AFirst, ALast: TCHandle);
begin
  FOnCompare := compare;
  FContainer.Sort(CmpExt, AFirst, ALast);
end;

procedure TTVariant.Sort(compare: TOnVariantCompareProc; AFirst, ALast: TCHandle);
begin
  FOnCompareProc := compare;
  FContainer.Sort(CmpExtProc, AFirst, ALast);
end;

function TTVariant.STCopy(Dst: TTCustom; h: TCHandle):TCHandle;
begin
  result := TTVariant(dst).Add(Items[h]);
end;

procedure TTVariant.STCopy(Dst: TTCustom; AFirst, ALast: TCHandle);
begin
  TTVariant(dst).Add(Items[AFirst]);
  if AFirst<>ALast then
    repeat
      FContainer.Next(AFirst);
      TTVariant(dst).Add(Items[AFirst]);
    until AFirst=ALast;
end;

procedure TTVariant.STSet(Dst: TTCustom; h1,h2: TCHandle);
begin
  TTVariant(dst)[h2] := Items[h1];
end;

procedure TTVariant.STSet(Dst: TTCustom; start1,finish1, start2: TCHandle);
begin
  TTVariant(dst)[start2] := Items[start1];
  if start1<>finish1 then
    repeat
      FContainer.Next(start1);
      dst.FContainer.Next(start2);
      TTVariant(dst)[start2] := Items[start1];
    until start1=finish1;
end;

function TTVariant.GetMap(key: TVariantType): variant;
begin
  Result := FContainer.Fields[1].AsVariant[Find(key)];
end;

procedure TTVariant.PutMap(key: TVariantType; Value: variant);
var
  h: TCHandle;
begin
  h := Find(key);
  if h = -1 then
    AddPair(key, Value)
  else
    FContainer.Fields[1].AsVariant[h] := Value;
end;

{ TTOleVariant }

procedure SortMemoryRangeOleVariant(Buf: pointer; L, R: Integer);
type
  POleVariantArray = ^TOleVariantArray;
  TOleVariantArray = array[0..32767] of OleVariant;
var
  I, J, P: Integer;
  T: OleVariant;
begin
  if R<=L then exit;
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while POleVariantArray(buf)[I]<POleVariantArray(buf)[P] do
        Inc(I);
      while POleVariantArray(buf)[J]>POleVariantArray(buf)[P] do
        Dec(J);
      if I <= J then
      begin
        T := POleVariantArray(buf)[I];
        POleVariantArray(buf)[I] := POleVariantArray(buf)[J];
        POleVariantArray(buf)[J] := T;
        if P = I then
          P := J
        else if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      SortMemoryRangeOleVariant(Buf, L, J);
    L := I;
  until I >= R;
end;

constructor TTOleVariant.Create(Container: TCCustom);
begin
  FieldSize := sizeof(TOleVariantType);
  inc(FNeedOnDelete);
  FSortMemoryRange := @SortMemoryRangeOleVariant;
  inherited;
end;

function TTOleVariant.Add(Item: TOleVariantType): TCHandle;
begin
  Result := FContainer.AddItem(item);
  if result<>-1 then
  {$IFDEF FPC}
    FillChar(Item, sizeof(ITem), 0);
  {$ELSE}
  asm
     xor eax,eax
     lea edx,Item
     mov [edx],eax
     mov [edx+4],eax
     mov [edx+8],eax
     mov [edx+12],eax
  end;
  {$ENDIF}
end;

function  TTOleVariant.Front: TOleVariantType;          // examine first element
begin
  result := items[FContainer.First];
end;

function  TTOleVariant.Back: TOleVariantType;           // examine last element
begin
  result := items[FContainer.Last];
end;

function TTOleVariant.Push(v: TOleVariantType): TCHandle;        // insert element at front
begin
  result := Insert(FContainer.First, v);
end;

function  TTOleVariant.Pop: TOleVariantType;            // remove first element
var
  h: TCHandle;
begin
  h := FContainer.First;
  Result := items[h];
  FContainer.Delete(h);
end;

function TTOleVariant.PushBack(v: TOleVariantType): TCHandle;    // insert element at back
begin
  result := Add(v);
end;

function  TTOleVariant.PopBack: TOleVariantType;        // remove last element
var
  h: TCHandle;
begin
  h := FContainer.Last;
  result := items[h];
  FContainer.Delete(h);
end;

function TTOleVariant.Enqueue(v: TOleVariantType): TCHandle;    // add to tail
begin
  result := Add(v);
end;

function  TTOleVariant.Dequeue: TOleVariantType;       // read&remove from head
var
  h: TCHandle;
begin
  h := FContainer.First;
  Result := items[h];
  FContainer.Delete(h);
end;

function TTOleVariant.AddList(const Items: array of const): TCHandle;
var
  i: integer;
  p: PVarRec;
begin
  Result := -1;
  for i := low(items) to high(items) do
  begin
    p := @items[i];
    if p.VType = vtObject then
      Result := ImportObject(p.VObject, Result)
    else
      Result := add(VarAsVariant(p));
  end;
end;

function TTOleVariant.AddTo(handle: TCHandle; dst: TTCustom): TCHandle;
begin
  Result := dst.Add([items[handle]]);
end;

function TTOleVariant.Find(Item: TOleVariantType): TCHandle;
begin
  Result := FContainer.Find(item);
end;

function TTOleVariant.FindFirstEqual(Item: TOleVariantType): TCHandle;
begin
  Result := FContainer.FindFirstEqual(item);
end;

procedure TTOleVariant.FindNextEqual(Item: TOleVariantType; var handle: TCHandle);
begin
  FContainer.FindNextEqual(item, handle);
end;

function TTOleVariant.GetAsVariant(handle: TCHandle): Variant;
begin
  if handle=-1 then
    Result := null
  else
    Result := items[handle];
end;

procedure TTOleVariant.SaveItem(h: TCHandle; dst: TStream; FileFormat : TFileFormat = ffDefault);
begin
  if FileFormat<>ffBinary then
    inherited
  else
    WriteOleVariant(dst, items[h]);
end;

function TTOleVariant.LoadItem(c: TCCustom; h: TCHandle; src: TStream; FileFormat : TFileFormat = ffDefault):TCHandle;
var
  t: integer;
  v: TOleVariantType;
begin
  if FileFormat=ffBinary then
    v := ReadOleVariant(src)
  else
  begin
    t := StrToInt(c.LoadToken);
    v := WideStrToVariant(c.LoadToken, t);
  end;
  if h=-1 then
    result := Add(v)
  else
  begin
    result := h;
    items[h] := v;
  end;
end;

function TTOleVariant.GetHandleOf(Value: Variant): TCHandle;
begin
  Result := Find(Value);
end;

function TTOleVariant.GetItem(n: TCHandle): TOleVariantType;
begin
  Result := TOleVariantType(FContainer.Get(n)^);
end;

function TTOleVariant.GetMinValue: TOleVariantType;
begin
  Result := TOleVariantType(FContainer.Get(FContainer.FindMin)^);
end;

function TTOleVariant.GetMaxValue: TOleVariantType;
begin
  Result := TOleVariantType(FContainer.Get(FContainer.FindMax)^);
end;

function TTOleVariant.Insert(handle: TCHandle; Item: TOleVariantType): TCHandle;
begin
  Result := FContainer.InsertItem(handle, item);
end;

function TTOleVariant.InsertAfter(handle: TCHandle; Item: TOleVariantType): TCHandle;
begin
  Result := FContainer.InsertAfterItem(handle, item);
end;

procedure TTOleVariant.ItemDeleted(n: TCHandle);
var
  p: ^TOleVariantType;
begin
  p := FContainer[n];
  p^ := null;
  inherited;
end;

procedure TTOleVariant.Remove(Item: TOleVariantType);
begin
  FContainer.Remove(item);
end;

procedure TTOleVariant.RemoveAll(Item: TOleVariantType);
begin
  FContainer.RemoveAll(item);
end;

function TTOleVariant.RemoveMinValue: TOleVariantType;
var
  n: TCHandle;
begin
  n := FContainer.FindMin;
  Result := TOleVariantType(FContainer.Get(n)^);
  FContainer.Delete(n);
end;

function TTOleVariant.RemoveMaxValue: TOleVariantType;
var
  n: TCHandle;
begin
  n := FContainer.FindMax;
  Result := TOleVariantType(FContainer.Get(n)^);
  FContainer.Delete(n);
end;

procedure TTOleVariant.SetAsVariant(handle: TCHandle; Value: Variant);
begin
  items[handle] := Value;
end;

function TTOleVariant.GetAsAnsiStr(handle: TCHandle): AnsiString;
begin
  result := VariantToAnsiStr(Items[handle]);
end;

procedure TTOleVariant.SetAsAnsiStr(handle: TCHandle; Value: AnsiString);
begin
  Items[handle] := AnsiStrToVariant(value);
end;

function TTOleVariant.GetAsWideStr(handle: TCHandle): WideString;
begin
  result := VariantToWideStr(Items[handle]);
end;

procedure TTOleVariant.SetAsWideStr(handle: TCHandle; Value: WideString);
begin
  Items[handle] := WideStrToVariant(value);
end;

procedure TTOleVariant.SetItem(n: TCHandle; Value: TOleVariantType);
begin
  FContainer.Put(n, Value);
end;

function TTByte.AddPair(key: TByteType; Value: variant): TCHandle;
begin
  result := Add(key);
  Container.Fields[1].AsVariant[result] := Value;
end;

function TTLongword.AddPair(key: TLongwordType; Value: variant): TCHandle;
begin
  result := Add(key);
  Container.Fields[1].AsVariant[result] := Value;
end;

function TTPointer.AddPair(key: TPointerType; Value: variant): TCHandle;
begin
  result := Add(key);
  Container.Fields[1].AsVariant[result] := Value;
end;

function TTRecord.AddPair(var key; Value: variant): TCHandle;
begin
  result := Add(key);
  Container.Fields[1].AsVariant[result] := Value;
end;

function TTWord.AddPair(key: TWordType; Value: variant): TCHandle;
begin
  result := Add(key);
  Container.Fields[1].AsVariant[result] := Value;
end;

function TTOleVariant.AddPair(key: TOleVariantType; Value: variant): TCHandle;
begin
  result := Add(key);
  Container.Fields[1].AsVariant[result] := Value;
end;

procedure TTBoolean.InitContainer(c: TCCustom);
begin
  inherited;
  c.FCompareValue := CmpInt;
end;

function TTBoolean.CmpInt(var a,b):integer;
begin
  if TBooleanType(a) then
    if TBooleanType(b) then
      result := 0
    else
      result := 1
  else
    if TBooleanType(b) then
      result := -1
    else
      result := 0;
end;

procedure TTBoolean.SetOnCompare(c: TOnBooleanCompare);
begin
  if assigned(c) then
    container.FCompareValue := CmpExt
  else
    container.FCompareValue := CmpInt;
  FOnCompare := c;
  FOnCompareProc := nil;
end;

procedure TTBoolean.SetOnCompareProc(c: TOnBooleanCompareProc);
begin
  if assigned(c) then
    container.FCompareValue := CmpExtProc
  else
    container.FCompareValue := CmpInt;
  FOnCompareProc := c;
  FOnCompare := nil;
end;

function TTBoolean.CmpExt(var a, b): integer;
begin
  Result := FOnCompare(TBooleanType(a), TBooleanType(b));
end;

function TTBoolean.CmpExtProc(var a, b): integer;
begin
  Result := FOnCompareProc(TBooleanType(a), TBooleanType(b));
end;

procedure TTBoolean.Sort(compare: TOnBooleanCompare);
begin
  Sort(compare, FContainer.First, FContainer.Last);
end;

procedure TTBoolean.Sort(compare: TOnBooleanCompareProc);
begin
  Sort(compare, FContainer.First, FContainer.Last);
end;

procedure TTBoolean.Sort(compare: TOnBooleanCompare; AFirst, ALast: TCHandle);
begin
  FOnCompare := compare;
  FContainer.Sort(CmpExt, AFirst, ALast);
end;

procedure TTBoolean.Sort(compare: TOnBooleanCompareProc; AFirst, ALast: TCHandle);
begin
  FOnCompareProc := compare;
  FContainer.Sort(CmpExtProc, AFirst, ALast);
end;

function TTBoolean.STCopy(Dst: TTCustom; h: TCHandle):TCHandle;
begin
  result := TTBoolean(dst).Add(Items[h]);
end;

procedure TTBoolean.STCopy(Dst: TTCustom; AFirst, ALast: TCHandle);
begin
  TTBoolean(dst).Add(Items[AFirst]);
  if AFirst<>ALast then
    repeat
      FContainer.Next(AFirst);
      TTBoolean(dst).Add(Items[AFirst]);
    until AFirst=ALast;
end;

procedure TTBoolean.STSet(Dst: TTCustom; h1,h2: TCHandle);
begin
  TTBoolean(dst)[h2] := Items[h1];
end;

procedure TTBoolean.STSet(Dst: TTCustom; start1,finish1, start2: TCHandle);
begin
  TTBoolean(dst)[start2] := Items[start1];
  if start1<>finish1 then
    repeat
      FContainer.Next(start1);
      dst.FContainer.Next(start2);
      TTBoolean(dst)[start2] := Items[start1];
    until start1=finish1;
end;

function TTBoolean.GetMap(key: TBooleanType): variant;
begin
  Result := FContainer.Fields[1].AsVariant[Find(key)];
end;

procedure TTBoolean.PutMap(key: TBooleanType; Value: variant);
var
  h: TCHandle;
begin
  h := Find(key);
  if h = -1 then
    AddPair(key, Value)
  else
    FContainer.Fields[1].AsVariant[h] := Value;
end;

procedure TTByte.InitContainer(c: TCCustom);
begin
  inherited;
  c.FCompareValue := CmpInt;
end;

function TTByte.CmpInt(var a,b):integer;
{$IFDEF FPC}
begin
  result := integer(byte(a))-integer(byte(b));
end;
{$ELSE}
asm
   movzx   eax, byte ptr [a]
   movzx   edx, byte ptr [b]
   sub     eax, edx
end;
{$ENDIF}

procedure TTByte.SetOnCompare(c: TOnByteCompare);
begin
  if assigned(c) then
    container.FCompareValue := CmpExt
  else
    container.FCompareValue := CmpInt;
  FOnCompare := c;
  FOnCompareProc := nil;
end;

procedure TTByte.SetOnCompareProc(c: TOnByteCompareProc);
begin
  if assigned(c) then
    container.FCompareValue := CmpExtProc
  else
    container.FCompareValue := CmpInt;
  FOnCompareProc := c;
  FOnCompare := nil;
end;

function TTByte.CmpExt(var a, b): integer;
begin
  Result := FOnCompare(TByteType(a), TByteType(b));
end;

function TTByte.CmpExtProc(var a, b): integer;
begin
  Result := FOnCompareProc(TByteType(a), TByteType(b));
end;

procedure TTByte.Sort(compare: TOnByteCompare);
begin
  Sort(compare, FContainer.First, FContainer.Last);
end;

procedure TTByte.Sort(compare: TOnByteCompareProc);
begin
  Sort(compare, FContainer.First, FContainer.Last);
end;

procedure TTByte.Sort(compare: TOnByteCompare; AFirst, ALast: TCHandle);
begin
  FOnCompare := compare;
  FContainer.Sort(CmpExt, AFirst, ALast);
end;

procedure TTByte.Sort(compare: TOnByteCompareProc; AFirst, ALast: TCHandle);
begin
  FOnCompareProc := compare;
  FContainer.Sort(CmpExtProc, AFirst, ALast);
end;

function TTByte.STCopy(Dst: TTCustom; h: TCHandle):TCHandle;
begin
  result := TTByte(dst).Add(Items[h]);
end;

procedure TTByte.STCopy(Dst: TTCustom; AFirst, ALast: TCHandle);
begin
  TTByte(dst).Add(Items[AFirst]);
  if AFirst<>ALast then
    repeat
      FContainer.Next(AFirst);
      TTByte(dst).Add(Items[AFirst]);
    until AFirst=ALast;
end;

procedure TTByte.STSet(Dst: TTCustom; h1,h2: TCHandle);
begin
  TTByte(dst)[h2] := Items[h1];
end;

procedure TTByte.STSet(Dst: TTCustom; start1,finish1, start2: TCHandle);
begin
  TTByte(dst)[start2] := Items[start1];
  if start1<>finish1 then
    repeat
      FContainer.Next(start1);
      dst.FContainer.Next(start2);
      TTByte(dst)[start2] := Items[start1];
    until start1=finish1;
end;

function TTByte.GetMap(key: TByteType): variant;
begin
  Result := FContainer.Fields[1].AsVariant[Find(key)];
end;

procedure TTByte.PutMap(key: TByteType; Value: variant);
var
  h: TCHandle;
begin
  h := Find(key);
  if h = -1 then
    AddPair(key, Value)
  else
    FContainer.Fields[1].AsVariant[h] := Value;
end;

procedure TTLongword.InitContainer(c: TCCustom);
begin
  inherited;
  c.FCompareValue := CmpInt;
end;

function TTLongword.CmpInt(var a,b):integer;
begin
  if TLongwordType(a)<TLongwordType(b) then
    result := -1
  else
    if TLongwordType(a)=TLongwordType(b) then
      result := 0
    else
      result := 1;
end;

procedure TTLongword.SetOnCompare(c: TOnLongwordCompare);
begin
  if assigned(c) then
    container.FCompareValue := CmpExt
  else
    container.FCompareValue := CmpInt;
  FOnCompareProc := nil;
  FOnCompare := c;
end;

procedure TTLongword.SetOnCompareProc(c: TOnLongwordCompareProc);
begin
  if assigned(c) then
    container.FCompareValue := CmpExtProc
  else
    container.FCompareValue := CmpInt;
  FOnCompare := nil;
  FOnCompareProc := c;
end;

function TTLongword.CmpExt(var a, b): integer;
begin
  Result := FOnCompare(TLongwordType(a), TLongwordType(b));
end;

function TTLongword.CmpExtProc(var a, b): integer;
begin
  Result := FOnCompareProc(TLongwordType(a), TLongwordType(b));
end;

procedure TTLongword.Sort(compare: TOnLongwordCompare);
begin
  Sort(compare, FContainer.First, FContainer.Last);
end;

procedure TTLongword.Sort(compare: TOnLongwordCompareProc);
begin
  Sort(compare, FContainer.First, FContainer.Last);
end;

procedure TTLongword.Sort(compare: TOnLongwordCompare; AFirst, ALast: TCHandle);
begin
  FOnCompare := compare;
  FContainer.Sort(CmpExt, AFirst, ALast);
end;

procedure TTLongword.Sort(compare: TOnLongwordCompareProc; AFirst, ALast: TCHandle);
begin
  FOnCompareProc := compare;
  FContainer.Sort(CmpExtProc, AFirst, ALast);
end;

function TTLongword.STCopy(Dst: TTCustom; h: TCHandle):TCHandle;
begin
  result := TTLongword(dst).Add(Items[h]);
end;

procedure TTLongword.STCopy(Dst: TTCustom; AFirst, ALast: TCHandle);
begin
  TTLongword(dst).Add(Items[AFirst]);
  if AFirst<>ALast then
    repeat
      FContainer.Next(AFirst);
      TTLongword(dst).Add(Items[AFirst]);
    until AFirst=ALast;
end;

procedure TTLongword.STSet(Dst: TTCustom; h1,h2: TCHandle);
begin
  TTLongword(dst)[h2] := Items[h1];
end;

procedure TTLongword.STSet(Dst: TTCustom; start1,finish1, start2: TCHandle);
begin
  TTLongword(dst)[start2] := Items[start1];
  if start1<>finish1 then
    repeat
      FContainer.Next(start1);
      dst.FContainer.Next(start2);
      TTLongword(dst)[start2] := Items[start1];
    until start1=finish1;
end;

function TTLongword.GetMap(key: TLongwordType): variant;
begin
  Result := FContainer.Fields[1].AsVariant[Find(key)];
end;

procedure TTLongword.PutMap(key: TLongwordType; Value: variant);
var
  h: TCHandle;
begin
  h := Find(key);
  if h = -1 then
    AddPair(key, Value)
  else
    FContainer.Fields[1].AsVariant[h] := Value;
end;

procedure TTPointer.InitContainer(c: TCCustom);
begin
  inherited;
  c.FCompareValue := CmpInt;
end;

function TTPointer.CmpInt(var a,b):integer;
begin
  if integer(a)<integer(b) then
    result := -1
  else
    if integer(a)=integer(b) then
      result := 0
    else
      result := 1;
end;

procedure TTPointer.SetOnCompare(c: TOnPointerCompare);
begin
  if assigned(c) then
    container.FCompareValue := CmpExt
  else
    container.FCompareValue := CmpInt;
  FOnCompare := c;
  FOnCompareProc := nil;
end;

procedure TTPointer.SetOnCompareProc(c: TOnPointerCompareProc);
begin
  if assigned(c) then
    container.FCompareValue := CmpExtProc
  else
    container.FCompareValue := CmpInt;
  FOnCompareProc := c;
  FOnCompare := nil;
end;

function TTPointer.CmpExt(var a, b): integer;
begin
  Result := FOnCompare(TPointerType(a), TPointerType(b));
end;

function TTPointer.CmpExtProc(var a, b): integer;
begin
  Result := FOnCompareProc(TPointerType(a), TPointerType(b));
end;

procedure TTPointer.Sort(compare: TOnPointerCompare);
begin
  Sort(compare, FContainer.First, FContainer.Last);
end;

procedure TTPointer.Sort(compare: TOnPointerCompareProc);
begin
  Sort(compare, FContainer.First, FContainer.Last);
end;

procedure TTPointer.Sort(compare: TOnPointerCompare; AFirst, ALast: TCHandle);
begin
  FOnCompare := compare;
  FContainer.Sort(CmpExt, AFirst, ALast);
end;

procedure TTPointer.Sort(compare: TOnPointerCompareProc; AFirst, ALast: TCHandle);
begin
  FOnCompareProc := compare;
  FContainer.Sort(CmpExtProc, AFirst, ALast);
end;

function TTPointer.STCopy(Dst: TTCustom; h: TCHandle):TCHandle;
begin
  result := TTPointer(dst).Add(Items[h]);
end;

procedure TTPointer.STCopy(Dst: TTCustom; AFirst, ALast: TCHandle);
begin
  TTPointer(dst).Add(Items[AFirst]);
  if AFirst<>ALast then
    repeat
      FContainer.Next(AFirst);
      TTPointer(dst).Add(Items[AFirst]);
    until AFirst=ALast;
end;

procedure TTPointer.STSet(Dst: TTCustom; h1,h2: TCHandle);
begin
  TTPointer(dst)[h2] := Items[h1];
end;

procedure TTPointer.STSet(Dst: TTCustom; start1,finish1, start2: TCHandle);
begin
  TTPointer(dst)[start2] := Items[start1];
  if start1<>finish1 then
    repeat
      FContainer.Next(start1);
      dst.FContainer.Next(start2);
      TTPointer(dst)[start2] := Items[start1];
    until start1=finish1;
end;

function TTPointer.GetMap(key: TPointerType): variant;
begin
  Result := FContainer.Fields[1].AsVariant[Find(key)];
end;

procedure TTPointer.PutMap(key: TPointerType; Value: variant);
var
  h: TCHandle;
begin
  h := Find(key);
  if h = -1 then
    AddPair(key, Value)
  else
    FContainer.Fields[1].AsVariant[h] := Value;
end;

procedure TTRecord.InitContainer(c: TCCustom);
begin
  inherited;
  SetCmp;
end;

procedure TTRecord.SetCmp;
begin
  if FieldSize=1 then Container.FCompareValue := CmpInt1 else
    if FieldSize=2 then Container.FCompareValue := CmpInt2 else
      if FieldSize=4 then Container.FCompareValue := CmpInt4 else
        if FieldSize=8 then Container.FCompareValue := CmpInt8 else
          Container.FCompareValue := CmpInt;
end;

function CmpIntN(var a,b; c: integer):integer;
var
  p11, p12: ^longword;
  p21, p22: ^byte;
  i: integer;
begin
  p11 := @a;
  p12 := @b;
  for i := 0 to c div 4-1 do
  begin
    if p11^<p12^ then
    begin
      result := -1;
      exit;
    end
    else
      if p11^<>p12^ then
      begin
        result := 1;
        exit;
      end;
    inc(p11);
    inc(p12);
  end;

  p21 := pointer(p11);
  p22 := pointer(p12);
  for i := 0 to c mod 4-1 do
  begin
    if p21^<p22^ then
    begin
      result := -1;
      exit;
    end
    else
      if p21^<>p22^ then
      begin
        result := 1;
        exit;
      end;
    inc(p21);
    inc(p22);
  end;

  result := 0;
end;

function TTRecord.CmpInt(var a,b):integer;
begin
  result := CmpIntN(a,b,FieldSize);
end;

function TTRecord.CmpInt1(var a,b):integer;
{$IFDEF FPC}
begin
  result := integer(byte(a))-integer(byte(b));
end;
{$ELSE}
asm
   movzx   eax, byte ptr [a]
   movzx   edx, byte ptr [b]
   sub     eax, edx
end;
{$ENDIF}

function TTRecord.CmpInt2(var a,b):integer;
{$IFDEF FPC}
begin
  result := integer(word(a))-integer(word(b));
end;
{$ELSE}
asm
   movzx   eax, word ptr [a]
   movzx   edx, word ptr [b]
   sub     eax, edx
end;
{$ENDIF}

function TTRecord.CmpInt4(var a,b):integer;
begin
  if longword(a)<longword(b) then
    result := -1
  else
    if longword(a)=longword(b) then
      result := 0
    else
      result := 1;
end;

function TTRecord.CmpInt8(var a,b):integer;
var c,d: ^longword;
begin
  if longword(a)<longword(b) then
    result := -1
  else
    if longword(a)>longword(b) then
      result := 1
    else
    begin
      c := @a;
      d := @b;
      inc(c);
      inc(d);
      if c^<d^ then
        result := -1
      else
        if c^=d^ then
          result := 0
        else
          result := 1;
    end;
end;

procedure TTRecord.SetOnCompare(c: TOnRecordCompare);
begin
  if assigned(c) then
    container.FCompareValue := CmpExt
  else
    SetCmp;
  FOnCompare := c;
  FOnCompareProc := nil;
end;

procedure TTRecord.SetOnCompareProc(c: TOnRecordCompareProc);
begin
  if assigned(c) then
    container.FCompareValue := CmpExtProc
  else
    SetCmp;
  FOnCompareProc := c;
  FOnCompare := nil;
end;

function TTRecord.CmpExt(var a, b): integer;
begin
  Result := FOnCompare(a, b);
end;

function TTRecord.CmpExtProc(var a, b): integer;
begin
  Result := FOnCompareProc(a, b);
end;

procedure TTRecord.Sort(compare: TOnRecordCompare);
begin
  Sort(compare, FContainer.First, FContainer.Last);
end;

procedure TTRecord.Sort(compare: TOnRecordCompareProc);
begin
  Sort(compare, FContainer.First, FContainer.Last);
end;

procedure TTRecord.Sort(compare: TOnRecordCompare; AFirst, ALast: TCHandle);
begin
  FOnCompare := compare;
  FContainer.Sort(CmpExt, AFirst, ALast);
end;

procedure TTRecord.Sort(compare: TOnRecordCompareProc; AFirst, ALast: TCHandle);
begin
  FOnCompareProc := compare;
  FContainer.Sort(CmpExtProc, AFirst, ALast);
end;

function TTRecord.STCopy(Dst: TTCustom; h: TCHandle):TCHandle;
begin
  result := TTRecord(dst).Add(Items[h]^);
end;

procedure TTRecord.STCopy(Dst: TTCustom; AFirst, ALast: TCHandle);
begin
  TTRecord(dst).Add(Items[AFirst]^);
  if AFirst<>ALast then
    repeat
      FContainer.Next(AFirst);
      TTRecord(dst).Add(Items[AFirst]^);
    until AFirst=ALast;
end;

procedure TTRecord.STSet(Dst: TTCustom; h1,h2: TCHandle);
begin
  TTRecord(dst)[h2] := Items[h1];
end;

procedure TTRecord.STSet(Dst: TTCustom; start1,finish1, start2: TCHandle);
begin
  TTRecord(dst)[start2] := Items[start1];
  if start1<>finish1 then
    repeat
      FContainer.Next(start1);
      dst.FContainer.Next(start2);
      TTRecord(dst)[start2] := Items[start1];
    until start1=finish1;
end;

function TTRecord.GetMap(var key): variant;
begin
  Result := FContainer.Fields[1].AsVariant[Find(key)];
end;

procedure TTRecord.PutMap(var key; Value: variant);
var
  h: TCHandle;
begin
  h := Find(key);
  if h = -1 then
    AddPair(key, Value)
  else
    FContainer.Fields[1].AsVariant[h] := Value;
end;

procedure TTWord.InitContainer(c: TCCustom);
begin
  inherited;
  c.FCompareValue := CmpInt;
end;

function TTWord.CmpInt(var a,b):integer;
{$IFDEF FPC}
begin
  result := integer(word(a))-integer(word(b));
end;
{$ELSE}
asm
   movzx   eax, word ptr [a]
   movzx   edx, word ptr [b]
   sub     eax, edx
end;
{$ENDIF}

procedure TTWord.SetOnCompare(c: TOnWordCompare);
begin
  if assigned(c) then
    container.FCompareValue := CmpExt
  else
    container.FCompareValue := CmpInt;
  FOnCompare := c;
  FOnCompareProc := nil;
end;

procedure TTWord.SetOnCompareProc(c: TOnWordCompareProc);
begin
  if assigned(c) then
    container.FCompareValue := CmpExtProc
  else
    container.FCompareValue := CmpInt;
  FOnCompareProc := c;
  FOnCompare := nil;
end;

function TTWord.CmpExt(var a, b): integer;
begin
  Result := FOnCompare(TWordType(a), TWordType(b));
end;

function TTWord.CmpExtProc(var a, b): integer;
begin
  Result := FOnCompareProc(TWordType(a), TWordType(b));
end;

procedure TTWord.Sort(compare: TOnWordCompare);
begin
  Sort(compare, FContainer.First, FContainer.Last);
end;

procedure TTWord.Sort(compare: TOnWordCompareProc);
begin
  Sort(compare, FContainer.First, FContainer.Last);
end;

procedure TTWord.Sort(compare: TOnWordCompare; AFirst, ALast: TCHandle);
begin
  FOnCompare := compare;
  FContainer.Sort(CmpExt, AFirst, ALast);
end;

procedure TTWord.Sort(compare: TOnWordCompareProc; AFirst, ALast: TCHandle);
begin
  FOnCompareProc := compare;
  FContainer.Sort(CmpExtProc, AFirst, ALast);
end;

function TTWord.STCopy(Dst: TTCustom; h: TCHandle):TCHandle;
begin
  result := TTWord(dst).Add(Items[h]);
end;

procedure TTWord.STCopy(Dst: TTCustom; AFirst, ALast: TCHandle);
begin
  TTWord(dst).Add(Items[AFirst]);
  if AFirst<>ALast then
    repeat
      FContainer.Next(AFirst);
      TTWord(dst).Add(Items[AFirst]);
    until AFirst=ALast;
end;

procedure TTWord.STSet(Dst: TTCustom; h1,h2: TCHandle);
begin
  TTWord(dst)[h2] := Items[h1];
end;

procedure TTWord.STSet(Dst: TTCustom; start1,finish1, start2: TCHandle);
begin
  TTWord(dst)[start2] := Items[start1];
  if start1<>finish1 then
    repeat
      FContainer.Next(start1);
      dst.FContainer.Next(start2);
      TTWord(dst)[start2] := Items[start1];
    until start1=finish1;
end;

function TTWord.GetMap(key: TWordType): variant;
begin
  Result := FContainer.Fields[1].AsVariant[Find(key)];
end;

procedure TTWord.PutMap(key: TWordType; Value: variant);
var
  h: TCHandle;
begin
  h := Find(key);
  if h = -1 then
    AddPair(key, Value)
  else
    FContainer.Fields[1].AsVariant[h] := Value;
end;

procedure TTOleVariant.InitContainer(c: TCCustom);
begin
  inherited;
  c.FCompareValue := CmpInt;
end;

function TTOleVariant.CmpInt(var a,b):integer;
begin
  if TOleVariantType(a) < TOleVariantType(b) then
    result := -1
  else
    if TOleVariantType(a) = TOleVariantType(b) then
      result := 0
    else
      result := 1;
end;

procedure TTOleVariant.SetOnCompare(c: TOnOleVariantCompare);
begin
  if assigned(c) then
    container.FCompareValue := CmpExt
  else
    container.FCompareValue := CmpInt;
  FOnCompare := c;
  FOnCompareProc := nil;
end;

procedure TTOleVariant.SetOnCompareProc(c: TOnOleVariantCompareProc);
begin
  if assigned(c) then
    container.FCompareValue := CmpExtProc
  else
    container.FCompareValue := CmpInt;
  FOnCompareProc := c;
  FOnCompare := nil;
end;

function TTOleVariant.CmpExt(var a, b): integer;
begin
  Result := FOnCompare(TOleVariantType(a), TOleVariantType(b));
end;

function TTOleVariant.CmpExtProc(var a, b): integer;
begin
  Result := FOnCompareProc(TOleVariantType(a), TOleVariantType(b));
end;

procedure TTOleVariant.Sort(compare: TOnOleVariantCompare);
begin
  Sort(compare, FContainer.First, FContainer.Last);
end;

procedure TTOleVariant.Sort(compare: TOnOleVariantCompareProc);
begin
  Sort(compare, FContainer.First, FContainer.Last);
end;

procedure TTOleVariant.Sort(compare: TOnOleVariantCompare; AFirst, ALast: TCHandle);
begin
  FOnCompare := compare;
  FContainer.Sort(CmpExt, AFirst, ALast);
end;

procedure TTOleVariant.Sort(compare: TOnOleVariantCompareProc; AFirst, ALast: TCHandle);
begin
  FOnCompareProc := compare;
  FContainer.Sort(CmpExtProc, AFirst, ALast);
end;

function TTOleVariant.STCopy(Dst: TTCustom; h: TCHandle):TCHandle;
begin
  result := TTOleVariant(dst).Add(Items[h]);
end;

procedure TTOleVariant.STCopy(Dst: TTCustom; AFirst, ALast: TCHandle);
begin
  TTOleVariant(dst).Add(Items[AFirst]);
  if AFirst<>ALast then
    repeat
      FContainer.Next(AFirst);
      TTOleVariant(dst).Add(Items[AFirst]);
    until AFirst=ALast;
end;

procedure TTOleVariant.STSet(Dst: TTCustom; h1,h2: TCHandle);
begin
  TTOleVariant(dst)[h2] := Items[h1];
end;

procedure TTOleVariant.STSet(Dst: TTCustom; start1,finish1, start2: TCHandle);
begin
  TTOleVariant(dst)[start2] := Items[start1];
  if start1<>finish1 then
    repeat
      FContainer.Next(start1);
      dst.FContainer.Next(start2);
      TTOleVariant(dst)[start2] := Items[start1];
    until start1=finish1;
end;

function TTOleVariant.GetMap(key: TOleVariantType): variant;
begin
  Result := FContainer.Fields[1].AsVariant[Find(key)];
end;

procedure TTOleVariant.PutMap(key: TOleVariantType; Value: variant);
var
  h: TCHandle;
begin
  h := Find(key);
  if h = -1 then
    AddPair(key, Value)
  else
    FContainer.Fields[1].AsVariant[h] := Value;
end;

procedure WriteInteger(dst: TStream; value: integer);
begin
  dst.Write(value, sizeof(value));
end;

procedure WriteLongint(dst: TStream; value: longint);
begin
  dst.Write(value, sizeof(value));
end;

procedure WriteByte(dst: TStream; value: byte);
begin
  dst.Write(value, sizeof(value));
end;

procedure WriteString(dst: TStream; value: string);
begin
  WriteLongint(dst, length(value));
  if value<>'' then
    dst.Write(value[1], length(value)*sizeof(value[1]));
end;

procedure WriteAnsiString(dst: TStream; value: ansistring);
begin
  WriteLongint(dst, length(value));
  if value<>'' then
    dst.Write(value[1], length(value)*sizeof(value[1]));
end;

procedure WriteWideString(dst: TStream; value: widestring);
begin
  WriteLongint(dst, length(value));
  if value<>'' then
    dst.Write(value[1], length(value)*sizeof(value[1]));
end;

procedure WriteSingle(dst: TStream; value: single);
begin
  dst.Write(value, sizeof(value));
end;

procedure WriteDouble(dst: TStream; value: double);
begin
  dst.Write(value, sizeof(value));
end;

procedure WriteExtended(dst: TStream; value: extended);
begin
  dst.Write(value, sizeof(value));
end;

procedure WriteCurrency(dst: TStream; value: currency);
begin
  dst.Write(value, sizeof(value));
end;

procedure WriteDate(dst: TStream; value: TDateTime);
begin
  dst.Write(value, sizeof(value));
end;

procedure WriteBoolean(dst: TStream; value: boolean);
begin
  dst.Write(value, sizeof(value));
end;

procedure WriteInt64(dst: TStream; value: int64);
begin
  dst.Write(value, sizeof(value));
end;

procedure WriteDWord(dst: TStream; value: dword);
begin
  dst.Write(value, sizeof(value));
end;

procedure WriteSmallint(dst: TStream; value: smallint);
begin
  dst.Write(value, sizeof(value));
end;

procedure WriteShortInt(dst: TStream; value: shortint);
begin
  dst.Write(value, sizeof(value));
end;

procedure WriteWord(dst: TStream; value: word);
begin
  dst.Write(value, sizeof(value));
end;

procedure WriteVariant(dst: TStream; value: variant);
var
  vt: TVarType;
  i,j,k,dim: integer;
begin
  vt := VarType(Value);
  dst.Write(vt, sizeof(vt));
  case vt and varTypeMask of
    varEmpty, varNull:;
    varOleStr:
      WriteWideString(dst, Value);
    varString:
      WriteString(dst, Value);
    varByte:
      WriteByte(dst, value);
    varShortInt:
      WriteShortInt(dst, value);
    varWord:
      WriteWord(dst, value);
    varSmallInt:
      WriteSmallInt(dst, value);
    varInteger:
      WriteInteger(dst, Value);
    varSingle:
      WriteSingle(dst, Value);
    varDouble:
      WriteDouble(dst, Value);
    varCurrency:
      WriteCurrency(dst, Value);
    varDate:
      WriteDate(dst, Value);
    varBoolean:
      WriteBoolean(dst, Value);
    varLongWord:
      WriteDWord(dst, value);
    varInt64:
      WriteInt64(dst, value);
    varArray:
      begin
        dim := VarArrayDimCount(value);
        assert(dim <= 3);
        WriteInteger(dst, dim);
        for i := 0 to dim-1 do
        begin
          WriteInteger(dst, VarArrayLowBound(value, i));
          WriteInteger(dst, VarArrayHighBound(value, i));
        end;
        case dim of
          1:
            for i := VarArrayLowBound(value, 1) to VarArrayHighBound(value, 1) do
              WriteVariant(dst, value[i]);
          2:
            for i := VarArrayLowBound(value, 1) to VarArrayHighBound(value, 1) do
              for j := VarArrayLowBound(value, 2) to VarArrayHighBound(value, 2) do
                WriteVariant(dst, value[i,j]);
          3:
            for i := VarArrayLowBound(value, 1) to VarArrayHighBound(value, 1) do
              for j := VarArrayLowBound(value, 2) to VarArrayHighBound(value, 2) do
                for k := VarArrayLowBound(value, 3) to VarArrayHighBound(value, 3) do
                  WriteVariant(dst, value[i,j,k]);
        end;
      end;
    else raise exception.create(erOperation);
  end;
end;

procedure WriteOleVariant(dst: TStream; value: olevariant);
begin
  WriteVariant(dst, value);
end;

function ReadInteger(src: TStream): integer;
begin
  src.Read(result, sizeof(result));
end;

function ReadLongint(src: TStream): longint;
begin
  src.Read(result, sizeof(result));
end;

function ReadByte(src: TStream): byte;
begin
  src.Read(result, sizeof(result));
end;

function ReadSingle(src: TStream): single;
begin
  src.Read(result, sizeof(result));
end;

function ReadDouble(src: TStream): Double;
begin
  src.Read(result, sizeof(result));
end;

function ReadExtended(src: TStream): Extended;
begin
  src.Read(result, sizeof(result));
end;

function ReadCurrency(src: TStream): Currency;
begin
  src.Read(result, sizeof(result));
end;

function ReadDWord(src: TStream): DWord;
begin
  src.Read(result, sizeof(result));
end;

function ReadInt64(src: TStream): Int64;
begin
  src.Read(result, sizeof(result));
end;

function ReadDate(src: TStream): TDateTime;
begin
  src.Read(result, sizeof(result));
end;

function ReadWord(src: TStream): Word;
begin
  src.Read(result, sizeof(result));
end;

function ReadBoolean(src: TStream): Boolean;
begin
  src.Read(result, sizeof(result));
end;

function ReadSmallint(src: TStream): Smallint;
begin
  src.Read(result, sizeof(result));
end;

function ReadShortInt(src: TStream): ShortInt;
begin
  src.Read(result, sizeof(result));
end;

function ReadString(src: TStream): string;
var n: longint;
begin
  n := ReadLongint(src);
  assert(n>=0);
  setlength(result, n);
  src.Read(result[1], n*sizeof(result[1]));
end;

function ReadWideString(src: TStream): widestring;
var n: longint;
begin
  n := ReadLongint(src);
  assert(n>=0);
  setlength(result, n);
  src.Read(result[1], n*sizeof(result[1]));
end;

function ReadAnsiString(src: TStream): ansistring;
var n: longint;
begin
  n := ReadLongint(src);
  assert(n>=0);
  setlength(result, n);
  src.Read(result[1], n*sizeof(result[1]));
end;

function ReadVariant(src: TStream): variant;
var
  vt: TVarType;
  i,j,k,dim: integer;
  bounds: array of integer;
begin
  src.Read(vt, sizeof(vt));
  case vt and varTypeMask of
    varEmpty: result := Unassigned;
    varNull: result := Null;
    varOleStr:
      result := ReadWideString(src);
    varString:
      result := ReadString(src);
    varByte:
      result := ReadByte(src);
    varShortInt:
      result := ReadShortInt(src);
    varWord:
      result := ReadWord(src);
    varSmallInt:
      result := ReadSmallInt(src);
    varInteger:
      result := ReadInteger(src);
    varSingle:
      result := ReadSingle(src);
    varDouble:
      result := ReadDouble(src);
    varCurrency:
      result := ReadCurrency(src);
    varDate:
      result := ReadDate(src);
    varBoolean:
      result := ReadBoolean(src);
    varLongWord:
      result := ReadDWord(src);
    varInt64:
      result := ReadInt64(src);
    varArray:
      begin
        dim := ReadInteger(src);
        assert(dim <= 3);
        setlength(bounds, dim*2);
        for i := 0 to dim-1 do
        begin
          bounds[i*2] := ReadInteger(src);
          bounds[i*2+1] := ReadInteger(src);
        end;
        result := VarArrayCreate(bounds, varVariant);
        case dim of
          1:
            for i := VarArrayLowBound(result, 1) to VarArrayHighBound(result, 1) do
              result[i] := ReadVariant(src);
          2:
            for i := VarArrayLowBound(result, 1) to VarArrayHighBound(result, 1) do
              for j := VarArrayLowBound(result, 2) to VarArrayHighBound(result, 2) do
                result[i,j] := ReadVariant(src);
          3:
            for i := VarArrayLowBound(result, 1) to VarArrayHighBound(result, 1) do
              for j := VarArrayLowBound(result, 2) to VarArrayHighBound(result, 2) do
                for k := VarArrayLowBound(result, 3) to VarArrayHighBound(result, 3) do
                  result[i,j,k] := ReadVariant(src);
        end;
      end;
    else raise exception.create(erOperation);
  end;
end;

function ReadOleVariant(src: TStream): olevariant;
begin
  result := ReadVariant(src);
end;

procedure TTCustom.ForEach(OnForEach: TOnForEach);
var h,hh: TCHandle;
begin
  h := FContainer.First;
  while h<>-1 do
  begin
    hh := h;
    FContainer.Next(h);
    OnForEach(self, hh);
  end;
end;

procedure TTCustom.ForEach(OnForEach: TOnForEachProc);
var h,hh: TCHandle;
begin
  h := FContainer.First;
  while h<>-1 do
  begin
    hh := h;
    FContainer.Next(h);
    OnForEach(self, hh);
  end;
end;

procedure TTCustom.ForEach(OnForEach: TOnForEachParams; const params: array of const);
var h,hh: TCHandle;
begin
  h := FContainer.First;
  while h<>-1 do
  begin
    hh := h;
    FContainer.Next(h);
    OnForEach(self, hh, params);
  end;
end;

procedure TTCustom.ForEach(OnForEach: TOnForEachParamsProc; const params: array of const);
var h,hh: TCHandle;
begin
  h := FContainer.First;
  while h<>-1 do
  begin
    hh := h;
    FContainer.Next(h);
    OnForEach(self, hh, params);
  end;
end;

function  TTCustom.ForEach(OnForEach: TOnForEachInt; value: integer):integer;
var h,hh: TCHandle;
begin
  h := FContainer.First;
  while h<>-1 do
  begin
    hh := h;
    FContainer.Next(h);
    value := OnForEach(self, hh, value);
  end;
  result := value;
end;

function  TTCustom.ForEach(OnForEach: TOnForEachIntProc; value: integer):integer;
var h,hh: TCHandle;
begin
  h := FContainer.First;
  while h<>-1 do
  begin
    hh := h;
    FContainer.Next(h);
    value := OnForEach(self, hh, value);
  end;
  result := value;
end;

function  TTCustom.ForEach(OnForEach: TOnForEachInt64; value: Int64):Int64;
var h,hh: TCHandle;
begin
  h := FContainer.First;
  while h<>-1 do
  begin
    hh := h;
    FContainer.Next(h);
    value := OnForEach(self, hh, value);
  end;
  result := value;
end;

function  TTCustom.ForEach(OnForEach: TOnForEachInt64Proc; value: Int64):Int64;
var h,hh: TCHandle;
begin
  h := FContainer.First;
  while h<>-1 do
  begin
    hh := h;
    FContainer.Next(h);
    value := OnForEach(self, hh, value);
  end;
  result := value;
end;

function  TTCustom.ForEach(OnForEach: TOnForEachDouble; value: Double):Double;
var h,hh: TCHandle;
begin
  h := FContainer.First;
  while h<>-1 do
  begin
    hh := h;
    FContainer.Next(h);
    value := OnForEach(self, hh, value);
  end;
  result := value;
end;

function  TTCustom.ForEach(OnForEach: TOnForEachDoubleProc; value: Double):Double;
var h,hh: TCHandle;
begin
  h := FContainer.First;
  while h<>-1 do
  begin
    hh := h;
    FContainer.Next(h);
    value := OnForEach(self, hh, value);
  end;
  result := value;
end;

function  TTCustom.ForEach(OnForEach: TOnForEachVariant; value: Variant):Variant;
var h,hh: TCHandle;
begin
  h := FContainer.First;
  while h<>-1 do
  begin
    hh := h;
    FContainer.Next(h);
    value := OnForEach(self, hh, value);
  end;
  result := value;
end;

function  TTCustom.ForEach(OnForEach: TOnForEachVariantProc; value: Variant):Variant;
var h,hh: TCHandle;
begin
  h := FContainer.First;
  while h<>-1 do
  begin
    hh := h;
    FContainer.Next(h);
    value := OnForEach(self, hh, value);
  end;
  result := value;
end;

function  TTCustom.ForEach(OnForEach: TOnForEachString; value: String):String;
var h,hh: TCHandle;
begin
  h := FContainer.First;
  while h<>-1 do
  begin
    hh := h;
    FContainer.Next(h);
    value := OnForEach(self, hh, value);
  end;
  result := value;
end;

function  TTCustom.ForEach(OnForEach: TOnForEachStringProc; value: String):String;
var h,hh: TCHandle;
begin
  h := FContainer.First;
  while h<>-1 do
  begin
    hh := h;
    FContainer.Next(h);
    value := OnForEach(self, hh, value);
  end;
  result := value;
end;

function TTCustom.ForEach(OnForEach: TOnForEachInt; AFirst,
  ALast: TCHandle; value: integer): integer;
begin
  while AFirst<>-1 do
  begin
    value := OnForEach(self, AFirst, value);
    if AFirst=ALast then
      break;
    FContainer.Next(AFirst);
  end;
  result := value;
end;

function TTCustom.ForEach(OnForEach: TOnForEachIntProc; AFirst,
  ALast: TCHandle; value: integer): integer;
begin
  while AFirst<>-1 do
  begin
    value := OnForEach(self, AFirst, value);
    if AFirst=ALast then
      break;
    FContainer.Next(AFirst);
  end;
  result := value;
end;

function TTCustom.ForEach(OnForEach: TOnForEachInt64; AFirst,
  ALast: TCHandle; value: Int64): Int64;
begin
  while AFirst<>-1 do
  begin
    value := OnForEach(self, AFirst, value);
    if AFirst=ALast then
      break;
    FContainer.Next(AFirst);
  end;
  result := value;
end;

procedure TTCustom.ForEach(OnForEach: TOnForEachParamsProc; AFirst,
  ALast: TCHandle; const params: array of const);
begin
  while AFirst<>-1 do
  begin
    OnForEach(self, AFirst, params);
    if AFirst=ALast then
      break;
    FContainer.Next(AFirst);
  end;
end;

procedure TTCustom.ForEach(OnForEach: TOnForEach; AFirst, ALast: TCHandle);
begin
  while AFirst<>-1 do
  begin
    OnForEach(self, AFirst);
    if AFirst=ALast then
      break;
    FContainer.Next(AFirst);
  end;
end;

procedure TTCustom.ForEach(OnForEach: TOnForEachProc; AFirst,
  ALast: TCHandle);
begin
  while AFirst<>-1 do
  begin
    OnForEach(self, AFirst);
    if AFirst=ALast then
      break;
    FContainer.Next(AFirst);
  end;
end;

procedure TTCustom.ForEach(OnForEach: TOnForEachParams; AFirst,
  ALast: TCHandle; const params: array of const);
begin
  while AFirst<>-1 do
  begin
    OnForEach(self, AFirst, params);
    if AFirst=ALast then
      break;
    FContainer.Next(AFirst);
  end;
end;

function TTCustom.ForEach(OnForEach: TOnForEachStringProc; AFirst,
  ALast: TCHandle; value: String): String;
begin
  while AFirst<>-1 do
  begin
    OnForEach(self, AFirst, value);
    if AFirst=ALast then
      break;
    FContainer.Next(AFirst);
  end;  
  result:=value;
end;

function TTCustom.ForEach(OnForEach: TOnForEachVariant; AFirst,
  ALast: TCHandle; value: Variant): Variant;
begin
  while AFirst<>-1 do
  begin
    OnForEach(self, AFirst, value);
    if AFirst=ALast then
      break;
    FContainer.Next(AFirst);
  end;
  result:=value;
end;

function TTCustom.ForEach(OnForEach: TOnForEachVariantProc; AFirst,
  ALast: TCHandle; value: Variant): Variant;
begin
  while AFirst<>-1 do
  begin
    OnForEach(self, AFirst, value);
    if AFirst=ALast then
      break;
    FContainer.Next(AFirst);
  end;
  result:=value;
end;

function TTCustom.ForEach(OnForEach: TOnForEachString; AFirst,
  ALast: TCHandle; value: String): String;
begin
  while AFirst<>-1 do
  begin
    OnForEach(self, AFirst, value);
    if AFirst=ALast then
      break;
    FContainer.Next(AFirst);
  end;
  result:=value;
end;

function TTCustom.ForEach(OnForEach: TOnForEachInt64Proc; AFirst,
  ALast: TCHandle; value: Int64): Int64;
begin
  while AFirst<>-1 do
  begin
    value := OnForEach(self, AFirst, value);
    if AFirst=ALast then
      break;
    FContainer.Next(AFirst);
  end;
  result := value;
end;

function TTCustom.ForEach(OnForEach: TOnForEachDouble; AFirst,
  ALast: TCHandle; value: Double): Double;
begin
  while AFirst<>-1 do
  begin
    value := OnForEach(self, AFirst, value);
    if AFirst=ALast then
      break;
    FContainer.Next(AFirst);
  end;
  result := value;
end;

function TTCustom.ForEach(OnForEach: TOnForEachDoubleProc; AFirst,
  ALast: TCHandle; value: Double): Double;
begin
  while AFirst<>-1 do
  begin
    value := OnForEach(self, AFirst, value);
    if AFirst=ALast then
      break;
    FContainer.Next(AFirst);
  end;
  result := value;
end;

procedure TTCustom.CopyTo(dst: TTCustom);
begin
  CopyTo(dst, FContainer.First, FContainer.Last);
end;

procedure TTCustom.CopyTo(dst: TTCustom; AFirst, ALast: TCHandle);
begin
  if AFirst=-1 then
    exit;
  if ClassType=dst.ClassType then
    STCopy(dst, AFirst, ALast)
  else
    repeat
      AddTo(AFirst, dst);
      if AFirst=ALast then
        break;
      FContainer.Next(AFirst);
    until false;
end;

procedure TTCustom.FirstPermutation;
begin
  FContainer.FirstPermutation;
end;

procedure TTCustom.LastPermutation;
begin
  FContainer.LastPermutation;
end;

function  TTCustom.NextPermutation:boolean;
begin
  result := FContainer.NextPermutation;
end;

function  TTCustom.PrevPermutation:boolean;
begin
  result := FContainer.PrevPermutation;
end;

procedure TTCustom.SetDifference(d, dst: TTCustom; start1, finish1, start2, finish2: TCHandle);
var
  cmp: TOnCompareValues;
  n: integer;
  f1: TCHandle;
begin
  assert(d is self.ClassType);
  cmp := Container.FCompareValue;
  f1 := finish1;
  if finish1<>-1 then
    next(finish1);
  if finish2<>-1 then
    d.next(finish2);

  while (start1<>finish1) and (start2<>finish2) do
  begin
    n := cmp(FContainer.get(start1)^, d.FContainer.get(start2)^);
    if n<0 then
    begin
      STCopy(dst, start1);
      Next(start1)
    end
    else
      if n>0 then
        d.Next(start2)
      else
      begin
        Next(start1);
        d.Next(start2)
      end;
  end;

  if (start1<>finish1) then
    STCopy(dst, start1, f1);
end;

procedure TTCustom.SetDifference(d, dst: TTCustom);
begin
  SetDifference(d,dst, FContainer.First, FContainer.Last, d.First, d.Last);
end;

function TTCustom.SetDifference(d: TTCustom):pointer;
begin
  result := CloneShell(TCVEctor);
  SetDifference(d, TTCustom(result));
end;

function TTCustom.SetDifference(d: TTCustom; start1, finish1, start2, finish2: TCHandle):pointer;
begin
  result := CloneShell(TCVEctor);
  SetDifference(d, TTCustom(result), start1, finish1, start2, finish2);
end;

function TTCustom.SetCountDifference(d: TTCustom):integer;
begin
  result := SetCountDifference(d, FContainer.First, FContainer.Last, d.FContainer.First, d.FContainer.Last);
end;

function TTCustom.SetCountDifference(d: TTCustom; start1, finish1, start2, finish2: TCHandle):integer;
var
  cmp: TOnCompareValues;
  n: integer;
  f1: TCHandle;
begin
  assert(d is self.ClassType);
  cmp := Container.FCompareValue;
  result := 0;
  f1 := finish1;
  if finish1<>-1 then
    next(finish1);
  if finish2<>-1 then
    d.next(finish2);

  while (start1<>finish1) and (start2<>finish2) do
  begin
    n := cmp(FContainer.get(start1)^, d.FContainer.get(start2)^);
    if n<0 then
    begin
      inc(result);
      Next(start1)
    end
    else
      if n>0 then
        d.Next(start2)
      else
      begin
        Next(start1);
        d.Next(start2)
      end;
  end;

  if (start1<>finish1) then
    inc(result, RangeLength[start1, f1]);
end;

procedure TTCustom.SetSymmetricDifference(d, dst: TTCustom; start1, finish1, start2, finish2: TCHandle);
var
  cmp: TOnCompareValues;
  n: integer;
  f1,f2: TCHandle;
begin
  assert(d is self.ClassType);
  cmp := Container.FCompareValue;
  f1 := finish1;
  f2 := finish2;
  if finish1<>-1 then
    next(finish1);
  if finish2<>-1 then
    d.next(finish2);

  while (start1<>finish1) and (start2<>finish2) do
  begin
    n := cmp(FContainer.get(start1)^, d.FContainer.get(start2)^);
    if n<0 then
    begin
      STCopy(dst, start1);
      Next(start1)
    end
    else
      if n>0 then
      begin
        d.STCopy(dst, start2);
        d.Next(start2)
      end
      else
      begin
        Next(start1);
        d.Next(start2)
      end;
  end;

  if (start1<>finish1) then
    STCopy(dst, start1, f1)
  else
    if (start2<>finish2) then
      d.STCopy(dst, start2, f2);
end;

procedure TTCustom.SetSymmetricDifference(d, dst: TTCustom);
begin
  SetSymmetricDifference(d,dst, FContainer.First, FContainer.Last, d.First, d.Last);
end;

function TTCustom.SetSymmetricDifference(d: TTCustom):pointer;
begin
  result := CloneShell(TCVEctor);
  SetSymmetricDifference(d, TTCustom(result));
end;

function TTCustom.SetSymmetricDifference(d: TTCustom; start1, finish1, start2, finish2: TCHandle):pointer;
begin
  result := CloneShell(TCVEctor);
  SetSymmetricDifference(d, TTCustom(result), start1, finish1, start2, finish2);
end;

function TTCustom.SetCountSymmetricDifference(d: TTCustom):integer;
begin
  result := SetCountSymmetricDifference(d, FContainer.First, FContainer.Last, d.First, d.Last);
end;

function TTCustom.SetCountSymmetricDifference(d: TTCustom; start1, finish1, start2, finish2: TCHandle):integer;
var
  cmp: TOnCompareValues;
  n: integer;
  f1,f2: TCHandle;
begin
  assert(d is self.ClassType);
  cmp := Container.FCompareValue;
  result := 0;
  f1 := finish1;
  f2 := finish2;
  if finish1<>-1 then
    next(finish1);
  if finish2<>-1 then
    d.next(finish2);

  while (start1<>finish1) and (start2<>finish2) do
  begin
    n := cmp(FContainer.get(start1)^, d.FContainer.get(start2)^);
    if n<0 then
    begin
      inc(result);
      Next(start1)
    end
    else
      if n>0 then
      begin
        inc(result);
        d.Next(start2)
      end
      else
      begin
        Next(start1);
        d.Next(start2)
      end;
  end;

  if (start1<>finish1) then
    inc(result, RangeLength[start1, f1])
  else
    if (start2<>finish2) then
      inc(result, d.RangeLength[start2, f2]);
end;

procedure TTCustom.SetUnion(d, dst: TTCustom; start1, finish1, start2, finish2: TCHandle);
var
  cmp: TOnCompareValues;
  n: integer;
  f1,f2: TCHandle;
begin
  assert(d is self.ClassType);
  cmp := Container.FCompareValue;
  f1 := finish1;
  f2 := finish2;
  if finish1<>-1 then
    next(finish1);
  if finish2<>-1 then
    d.next(finish2);

  while (start1<>finish1) and (start2<>finish2) do
  begin
    n := cmp(FContainer.get(start1)^, d.FContainer.get(start2)^);
    if n<0 then
    begin
      STCopy(dst, start1);
      Next(start1)
    end
    else
      if n>0 then
      begin
        d.STCopy(dst, start2);
        d.Next(start2)
      end
      else
      begin
        STCopy(dst, start1);
        Next(start1);
        d.Next(start2)
      end;
  end;

  if (start1<>finish1) then
    STCopy(dst, start1, f1)
  else
    if (start2<>finish2) then
      d.STCopy(dst, start2, f2);
end;

procedure TTCustom.SetUnion(d, dst: TTCustom);
begin
  SetUnion(d,dst, FContainer.First, FContainer.Last, d.First, d.Last);
end;

function TTCustom.SetUnion(d: TTCustom):pointer;
begin
  result := CloneShell(TCVEctor);
  SetUnion(d, TTCustom(result));
end;

function TTCustom.SetUnion(d: TTCustom; start1, finish1, start2, finish2: TCHandle):pointer;
begin
  result := CloneShell(TCVEctor);
  SetUnion(d, TTCustom(result), start1, finish1, start2, finish2);
end;

function TTCustom.SetCountUnion(d: TTCustom):integer;
begin
  result := SetCountUnion(d, FContainer.First, FContainer.Last, d.First, d.Last);
end;

function TTCustom.SetCountUnion(d: TTCustom; start1, finish1, start2, finish2: TCHandle):integer;
var
  cmp: TOnCompareValues;
  n: integer;
  f1,f2: TCHandle;
begin
  assert(d is self.ClassType);
  cmp := Container.FCompareValue;
  result := 0;
  f1 := finish1;
  f2 := finish2;
  if finish1<>-1 then
    next(finish1);
  if finish2<>-1 then
    d.next(finish2);

  while (start1<>finish1) and (start2<>finish2) do
  begin
    n := cmp(FContainer.get(start1)^, d.FContainer.get(start2)^);
    if n<0 then
    begin
      inc(result);
      Next(start1)
    end
    else
      if n>0 then
      begin
        inc(result);
        d.Next(start2)
      end
      else
      begin
        inc(result);
        Next(start1);
        d.Next(start2)
      end;
  end;

  if (start1<>finish1) then
    inc(result, RangeLength[start1, f1])
  else
    if (start2<>finish2) then
      inc(result, d.RangeLength[start2, f2]);
end;

procedure TTCustom.SetIntersection(d, dst: TTCustom; start1, finish1, start2, finish2: TCHandle);
var
  cmp: TOnCompareValues;
  n: integer;
begin
  assert(d is self.ClassType);
  cmp := Container.FCompareValue;
  if (start1=-1) or (start2=-1) then
    exit;
  if finish1<>-1 then
    next(finish1);
  if finish2<>-1 then
    d.next(finish2);

  while (start1<>finish1) and (start2<>finish2) do
  begin
    n := cmp(FContainer.get(start1)^, d.FContainer.get(start2)^);
    if n < 0 then
      Next(start1)
    else
      if n > 0 then
        d.Next(start2)
      else
      begin
        STCopy(dst, start1);
        Next(start1);
        d.Next(start2);
      end;
  end;
end;

procedure TTCustom.SetIntersection(d, dst: TTCustom);
begin
  SetIntersection(d,dst, FContainer.First, FContainer.Last, d.First, d.Last);
end;

function TTCustom.SetIntersection(d: TTCustom):pointer;
begin
  result := CloneShell(TCVector);
  SetIntersection(d, TTCustom(result));
end;

function TTCustom.SetIntersection(d: TTCustom; start1, finish1, start2, finish2: TCHandle):pointer;
begin
  result := CloneShell(TCVector);
  SetIntersection(d, TTCustom(result), start1, finish1, start2, finish2);
end;

function TTCustom.SetCountIntersection(d: TTCustom):integer;
begin
  result := SetCountIntersection(d, FContainer.First, FContainer.Last, d.First, d.Last);
end;

function TTCustom.SetCountIntersection(d: TTCustom; start1, finish1, start2, finish2: TCHandle):integer;
var
  cmp: TOnCompareValues;
  n: integer;
begin
  assert(d is self.ClassType);
  cmp := Container.FCompareValue;
  result := 0;
  if (start1=-1) or (start2=-1) then
    exit;
  if finish1<>-1 then
    next(finish1);
  if finish2<>-1 then
    d.next(finish2);

  while (start1<>finish1) and (start2<>finish2) do
  begin
    n := cmp(FContainer.get(start1)^, d.FContainer.get(start2)^);
    if n < 0 then
      Next(start1)
    else
      if n > 0 then
        d.Next(start2)
      else
      begin
        inc(result);
        Next(start1);
        d.Next(start2);
      end;
  end;
end;

{
  TRUE when SELF includes all items from D (D is subset)
}
function TTCustom.SetIncludes(d: TTCustom; start1, finish1, start2, finish2: TCHandle):boolean;
begin
  result := d.SetSubsetOf(self, start2, finish2, start1, finish1);
end;

function TTCustom.SetIncludes(d: TTCustom):boolean;
begin
  result := d.SetSubsetOf(self);
end;

function TTCustom.SetIncludes(const Items: array of const):boolean;
var d: TTCustom;
begin
  d := CTT(ClassType).Create(CContainer(FContainer.ClassType));
  try
    d.Add(Items);
    result := SetIncludes(d);
  finally
    d.free;
  end;
end;

{
  TRUE when D includes all items from SELF (self is subset of D)
}
function TTCustom.SetSubsetOf(d: TTCustom; start1, finish1, start2, finish2: TCHandle):boolean;
begin
  result := SetCountDifference(d, start1, finish1, start2, finish2) = 0;
end;

function TTCustom.SetSubsetOf(d: TTCustom):boolean;
begin
  result := SetSubsetOf(d, FContainer.First, FContainer.Last, d.FContainer.First, d.FContainer.Last);
end;

function TTCustom.SetEqual(d: TTCustom; start1, finish1, start2, finish2: TCHandle):boolean;
var
  cmp: TOnCompareValues;
begin
  assert(d is self.ClassType);
  cmp := Container.FCompareValue;
  result := (start1=-1) and (start2=-1);
  if result or ((start1=-1)<>(start2=-1)) then
    exit;
  repeat
    if cmp(FContainer.get(start1)^, d.FContainer.get(start2)^)<>0 then
      exit;
    if (start1=finish1)<>(start2=finish2) then
      exit;
    if (start1=finish1) then
      break;
    Next(start1);
    d.Next(start2);
  until false;
  result := true;
end;

function TTCustom.SetEqual(d: TTCustom):boolean;
begin
  result := SetEqual(d, FContainer.First, FContainer.Last, d.FContainer.First, d.FContainer.Last);
end;

procedure TTCustom.DoRepeat(OnDoRepeat: TOnDoRepeat; Count: integer);
var i: integer;
begin
  for i := 0 to Count-1 do
    OnDoRepeat(self, i);
end;

procedure TTCustom.DoRepeat(OnDoRepeat: TOnDoRepeatProc; Count: integer);
var i: integer;
begin
  for i := 0 to Count-1 do
    OnDoRepeat(self, i);
end;

procedure TTCustom.Rotate(shift: integer);
begin
  FContainer.Rotate(shift);
end;

procedure TTCustom.Rotate(AFirst, ALast: TCHandle; shift: integer);
begin
  FContainer.Rotate(AFirst, ALast, shift);
end;

procedure TTCustom.Reverse;
begin
  FContainer.Reverse;
end;

procedure TTCustom.Reverse(AFirst, ALast: TCHandle);
begin
  FContainer.Reverse(AFirst, ALast);
end;

procedure TTCustom.RandomShuffle;
begin
  FContainer.RandomShuffle;
end;

procedure TTCustom.RandomShuffle(AFirst, ALast: TCHandle);
begin
  FContainer.RandomShuffle(AFirst, ALast);
end;

function TTCustom.EqualToF(const items: array of const):boolean;
begin
  result := EqualToF(First, items);
end;

function TTCustom.EqualToF(AFirst: TCHandle; const items: array of const):boolean;
var
  i: integer;
begin
  for i := low(items) to high(items) do
    if AsVariant[MoveNext(AFirst)]<>VarAsVariant(@items[i]) then
    begin
      result := false;
      exit;
    end;
  Result := true;
end;

function  TTCustom.EqualToB(const items: array of const):boolean;
begin
  result := EqualToB(Last, items);
end;

function  TTCustom.EqualToB(ALast: TCHandle;const items: array of const):boolean;
var
  i: integer;
begin
  for i := high(items) downto low(items) do
    if AsVariant[MovePrev(ALast)]<>VarAsVariant(@items[i]) then
    begin
      result := false;
      exit;
    end;
  Result := true;
end;

function  TTCustom.EqualTo(const items: array of const):boolean;
begin
  result := (length(items)=count) and EqualToF(items) and EqualToB(items);
end;

function  TTCustom.EqualTo(AFirst, ALast: TCHandle;const items: array of const):boolean;
begin
  result := EqualToF(AFirst, items) and EqualToB(ALast, items);
end;

procedure Exch(var a,b: integer); // (EAX, EDX) + ECX
{$IFDEF FPC}
var n: integer;
begin
  n := a; a := b; b := n;
end;
{$ELSE}
asm
   mov ecx, [eax]
   xchg ecx,[edx]
   mov [eax],ecx
end;
{$ENDIF}

procedure Exch(var a,b: pointer);
{$IFDEF FPC}
var n: pointer;
begin
  n := a; a := b; b := n;
end;
{$ELSE}
asm
   mov ecx, [eax]
   xchg ecx,[edx]
   mov [eax],ecx
end;
{$ENDIF}

procedure SwapN(var a,b,temp; size: integer);
var
  i: integer;
  l: longint;
  c: byte;
  l1,l2: ^longint;
  b1,b2: ^byte;
begin
  l1 := @a;
  l2 := @b;
  for i := 0 to size div 4-1 do
  begin
    l:=l1^; l1^:=l2^; l2^:=l;
    inc(l1);
    inc(l2);
  end;
  b1 := pointer(l1);
  b2 := pointer(l2);
  for i := 0 to size mod 4-1 do
  begin
    c:=b1^; b1^:=b2^; b2^:=c;
    inc(b1);
    inc(b2);
  end;
end;

// eax, edx, ecx, [ebp+$08]
procedure Swap1(var a,b,temp; size: integer);
{$IFDEF FPC}
var n: byte;
begin
  n := byte(a); byte(a) := byte(b); byte(b) := n;
end;
{$ELSE}
asm
   mov cl, [eax]
   xchg cl,[edx]
   mov [eax],cl
end;
{$ENDIF}

procedure Swap2(var a,b,temp; size: integer);
{$IFDEF FPC}
var n: word;
begin
  n := word(a); word(a) := word(b); word(b) := n;
end;
{$ELSE}
asm
   mov cx, [eax]
   xchg cx,[edx]
   mov [eax],cx
end;
{$ENDIF}

procedure Swap4(var a,b,temp; size: integer);
{$IFDEF FPC}
var n: longint;
begin
  n := longint(a); longint(a) := longint(b); longint(b) := n;
end;
{$ELSE}
asm
   mov ecx, [eax]
   xchg ecx,[edx]
   mov [eax],ecx
end;
{$ENDIF}

{$IFNDEF FPC}
procedure Swap3(var a,b,temp; size: integer);
asm
   mov cx, [eax]
   xchg cx,[edx]
   mov [eax],cx
   mov cl, [eax+2]
   xchg cl,[edx+2]
   mov [eax+2],cl
end;

procedure Swap8(var a,b,temp; size: integer);
asm
   mov ecx, [eax]
   xchg ecx,[edx]
   mov [eax],ecx
   mov ecx, [eax+4]
   xchg ecx,[edx+4]
   mov [eax+4],ecx
end;

procedure Swap12(var a,b,temp; size: integer);
asm
   mov ecx, [eax]
   xchg ecx,[edx]
   mov [eax],ecx
   mov ecx, [eax+4]
   xchg ecx,[edx+4]
   mov [eax+4],ecx
   mov ecx, [eax+8]
   xchg ecx,[edx+8]
   mov [eax+8],ecx
end;

procedure Swap16(var a,b,temp; size: integer);
asm
   mov ecx, [eax]
   xchg ecx,[edx]
   mov [eax],ecx
   mov ecx, [eax+4]
   xchg ecx,[edx+4]
   mov [eax+4],ecx
   mov ecx, [eax+8]
   xchg ecx,[edx+8]
   mov [eax+8],ecx
   mov ecx, [eax+12]
   xchg ecx,[edx+12]
   mov [eax+12],ecx
end;

procedure Swap20(var a,b,temp; size: integer);
asm
   mov ecx, [eax]
   xchg ecx,[edx]
   mov [eax],ecx
   mov ecx, [eax+4]
   xchg ecx,[edx+4]
   mov [eax+4],ecx
   mov ecx, [eax+8]
   xchg ecx,[edx+8]
   mov [eax+8],ecx
   mov ecx, [eax+12]
   xchg ecx,[edx+12]
   mov [eax+12],ecx
   mov ecx, [eax+16]
   xchg ecx,[edx+16]
   mov [eax+16],ecx
end;

procedure Swap24(var a,b,temp; size: integer);
asm
   mov ecx, [eax]
   xchg ecx,[edx]
   mov [eax],ecx
   mov ecx, [eax+4]
   xchg ecx,[edx+4]
   mov [eax+4],ecx
   mov ecx, [eax+8]
   xchg ecx,[edx+8]
   mov [eax+8],ecx
   mov ecx, [eax+12]
   xchg ecx,[edx+12]
   mov [eax+12],ecx
   mov ecx, [eax+16]
   xchg ecx,[edx+16]
   mov [eax+16],ecx
   mov ecx, [eax+20]
   xchg ecx,[edx+20]
   mov [eax+20],ecx
end;
{$ENDIF}

procedure Move1(const Source; var Dest; count : Integer);
{$IFDEF FPC}
begin
  byte(Dest):=byte(Source);
end;
{$ELSE}
// eax, edx, ecx
asm
   mov al, byte ptr [source]
   mov byte ptr [dest],al
end;
{$ENDIF}

procedure Move2(const Source; var Dest; count : Integer);
{$IFDEF FPC}
begin
  word(Dest):=word(Source);
end;
{$ELSE}
asm
   mov ax, word ptr [source]
   mov word ptr [dest],ax
end;
{$ENDIF}

procedure Move4(const Source; var Dest; count : Integer);
{$IFDEF FPC}
begin
  longword(Dest):=longword(Source);
end;
{$ELSE}
asm
   mov eax, [source]
   mov [dest],eax
end;
{$ENDIF}

{$IFNDEF FPC}
procedure Move3(const Source; var Dest; count : Integer);
asm
   mov cx, word ptr [source]
   mov word ptr [dest],cx
   mov cl, byte ptr [source+2]
   mov byte ptr [dest+2],cl
end;

procedure Move8(const Source; var Dest; count : Integer);
asm
   mov ecx, [source]
   mov [dest],ecx
   mov ecx, [source+4]
   mov [dest+4],ecx
end;

procedure Move12(const Source; var Dest; count : Integer);
asm
   mov ecx, [source]
   mov [dest],ecx
   mov ecx, [source+4]
   mov [dest+4],ecx
   mov ecx, [source+8]
   mov [dest+8],ecx
end;

procedure Move16(const Source; var Dest; count : Integer);
asm
   mov ecx, [source]
   mov [dest],ecx
   mov ecx, [source+4]
   mov [dest+4],ecx
   mov ecx, [source+8]
   mov [dest+8],ecx
   mov ecx, [source+12]
   mov [dest+12],ecx
end;

procedure Move20(const Source; var Dest; count : Integer);
asm
   mov ecx, [source]
   mov [dest],ecx
   mov ecx, [source+4]
   mov [dest+4],ecx
   mov ecx, [source+8]
   mov [dest+8],ecx
   mov ecx, [source+12]
   mov [dest+12],ecx
   mov ecx, [source+16]
   mov [dest+16],ecx
end;

procedure Move24(const Source; var Dest; count : Integer);
asm
   mov ecx, [source]
   mov [dest],ecx
   mov ecx, [source+4]
   mov [dest+4],ecx
   mov ecx, [source+8]
   mov [dest+8],ecx
   mov ecx, [source+12]
   mov [dest+12],ecx
   mov ecx, [source+16]
   mov [dest+16],ecx
   mov ecx, [source+20]
   mov [dest+20],ecx
end;
{$ENDIF}

function GetSwapProc(BlockSize: integer): TSwapProc;
const
{$IFDEF FPC}
  s: array[0..2] of integer = (1,2,4);
  p: array[0..2] of TSwapProc = (Swap1, Swap2, Swap4);
{$ELSE}
  s: array[0..8] of integer = (1,2,3,4,8,12,16,20,24);
  p: array[0..8] of TSwapProc = (Swap1, Swap2, Swap3, Swap4, Swap8, Swap12, Swap16, Swap20, Swap24);
{$ENDIF}
var
  i: integer;
begin
  for i := low(s) to high(s) do
    if s[i]=BlockSize then
    begin
      result := p[i];
      exit;
    end;
  result := SwapN;
end;

function GetMoveProc(BlockSize: integer): TMoveProc;
const
{$IFDEF FPC}
  s: array[0..2] of integer = (1,2,4);
  p: array[0..2] of TMoveProc = (Move1, Move2, Move4);
{$ELSE}
  s: array[0..8] of integer = (1,2,3,4,8,12,16,20,24);
  p: array[0..8] of TMoveProc = (Move1, Move2, move3, Move4, Move8, Move12, Move16, Move20, Move24);
{$ENDIF}
var
  i: integer;
begin
  for i := low(s) to high(s) do
    if s[i]=BlockSize then
    begin
      result := p[i];
      exit;
    end;
  result := system.move;
end;

{ TCUnsortedMap }

function TCUnsortedMap.AddItem(var item): TCHandle;
var
  h: integer;
begin
  if FCount>=FCapacity then
    Grow;
  h := FOnCalcHash(Item, FFieldSize, Hash) mod dword(FCapacity);
  result := FFirst;
  FFirst := FNext[FFirst];
  FNext[result] := FBuckets[h];
  FBuckets[h] := result;
  FMoveField(item, FItems[result*FItemSize], FFieldSize);
  inc(FCount);
end;

function TCUnsortedMap.BucketUsed:integer;
var i: integer;
begin
  result := 0;
  for i := 0 to FCapacity-1 do
    if FBuckets[i]<>-1 then
      inc(result);
end;

procedure TCUnsortedMap.Put(handle: TCHandle; var Item);
var
  h: TCHandle;
  n: integer;
begin
  if FItemSize<>FFieldSize then
    FMoveItem(FItems[handle*FItemSize], FTemp^, FItemSize);

  // we just move item into new position, so we should disable
  // cleaning of fields
  n := FDelCount;
  FDelCount := 0;
  Delete(handle);
  FDelCount := n;

  h := AddItem(item);
  if FItemSize<>FFieldSize then
  begin
    FMoveItem(FTemp^, FItems[h*FItemSize], FItemSize);
    FMoveField(Item, FItems[h*FItemSize], FFieldSize);
  end;
end;

function TCUnsortedMap.Get(handle: TCHandle): Pointer;
begin
  assert(handle>=0);
  result := @FItems[handle*FItemSize];
end;

procedure TCUnsortedMap.Delete(handle: TCHandle);
var
  i,j: integer;
begin
  j := FOnCalcHash(FItems[handle*FItemSize], FFieldSize, Hash) mod dword(FCapacity);
  i := FBuckets[j];
  if i=-1 then
    exit;
  dec(FCount);
  if i=handle then
  begin
    if FDelCount > 0 then
      ItemDeleted(i);
    FBuckets[j] := FNext[i];
    FNext[i] := FFirst;
    FFirst := i;
    exit;
  end;
  j := FNext[i];
  while j<>handle do
  begin
    if j=-1 then
      exit;
    i := j;
    j := FNext[j];
  end;
  if FDelCount > 0 then
    ItemDeleted(j);
  FNext[i] := FNext[j];
  FNext[j] := FFirst;
  FFirst := j;
end;

procedure TCUnsortedMap.Remove(var Item);
var
  i,j: integer;
begin
  j := FOnCalcHash(Item, FFieldSize, Hash) mod dword(FCapacity);
  i := FBuckets[j];
  if i=-1 then
    exit;
  if FCompareValue(Item, FItems[i*FItemSize])=0 then
  begin
    if FDelCount > 0 then
      ItemDeleted(i);
    FBuckets[j] := FNext[i];
    FNext[i] := FFirst;
    FFirst := i;
    dec(FCount);
    exit;
  end;

  j := FNext[i];
  while j<>-1 do
  begin
    if FCompareValue(Item, FItems[j*FItemSize])=0 then
    begin
      if FDelCount > 0 then
        ItemDeleted(j);
      FNext[i] := FNext[j];
      FNext[j] := FFirst;
      FFirst := j;
      break;
    end;
    i := j;
    j := FNext[j];
  end;
end;

procedure TCUnsortedMap.RemoveAll(var Item);
var
  i,j,n: integer;
begin
  j := FOnCalcHash(Item, FFieldSize, Hash) mod dword(FCapacity);
  i := FBuckets[j];
  if i=-1 then
    exit;
  while FCompareValue(Item, FItems[i*FItemSize])=0 do
  begin
    n := FNext[i];
    // remove item from bucket
    FBuckets[j] := n;
    // remove item from container
    FNext[i] := FFirst;
    FFirst := i;
    i := n;
    dec(FCount);
    if i=-1 then
      exit;
  end;

  j := i;
  i := FNext[i];
  while i<>-1 do
    if FCompareValue(Item, FItems[i*FItemSize])=0 then
    begin
      n := FNext[i];
      // remove item from bucket
      FNext[j] := n;
      // remove item from container
      FNext[i] := FFirst;
      FFirst := i;
      i := n;
      dec(FCount);
    end
    else
    begin
      j := i;
      i := FNext[i];
    end;
end;

procedure TCUnsortedMap.SetCapacity(n: integer);
var
  pp,p,list,i: integer;
begin
  assert(n>=FCount);
  if (n=FCount) or (n<FCapacity) then
    exit;

  // reallocate memory, add new items to list of free
  Reallocmem(FBuckets, n*SizeOf(integer));
  Reallocmem(FNext, n*SizeOf(integer));
  Reallocmem(FItems, n*FItemSize);
  fillchar(FItems[FCapacity*FItemSize], FItemSize*(n-FCapacity), 0);
  for i := FCapacity to n-1 do
  begin
    FBuckets[i] := -1;
    FNext[i] := FFirst;
    FFirst := i;
  end;

  // move all items from hash table to temp list
  List := -1;
  for i := 0 to FCapacity-1 do
  begin
    p := FBuckets[i];
    while p<>-1 do
    begin
      pp := p;
      p := FNext[p];
      FNext[pp] := List;
      List := pp;
    end;
  end;
  fillchar(FBuckets^, SizeOf(pointer)*n, $FF);
  FCapacity := n;

  // rehash
  while List<>-1 do
  begin
    p := List;
    List := FNext[List];
    i := FOnCalcHash(FItems[p*FItemSize], FFieldSize, Hash) mod dword(FCapacity);
    FNext[p] := FBuckets[i];
    FBuckets[i] := p;
  end;

end;

procedure TCUnsortedMap.Grow;
begin
  if Capacity < 16 then
    Capacity := 16
  else
    if Capacity < 16384 then
      Capacity := Capacity*4
    else
      Capacity := Capacity*2;
end;

constructor TCUnsortedMap.Create;
begin
  inherited Create;
  OnCalcHash := DoCalcHash; // default implementation - just use of Hash field
  hash := T_HashRnd.Create; // default hash alg - very fast and good enough
  FFirst := -1;
end;

destructor TCUnsortedMap.Destroy;
begin
  Clear;
  Reallocmem(FBuckets, 0);
  Reallocmem(FItems, 0);
  Reallocmem(FNext, 0);
  FCapacity := 0;
  FCount := 0;
  FReeAndNil(FHash);
  inherited;
end;

function TCUnsortedMap.DoCalcHash(var buf; Count: integer; h: T_HashCustom): cardinal;
begin
  Result := h.Get(buf, Count);
end;

procedure TCUnsortedMap.SetHash(h: T_HashCustom);
begin
  FreeAndNil(FHash);
  FHash := h;
end;

function TCUnsortedMap.GetCapacity: integer;
begin
  result := FCapacity;
end;

procedure TCUnsortedMap.SetCount(n: integer);
begin
  if n=0 then
    clear
  else
    while n>FCount do
      Delete(First);
end;

procedure TCUnsortedMap.Clear;
var i,j: integer;
begin

  // call ItemDeleted for every item in collection
  if FDelCount > 0 then
    for i := 0 to FCapacity-1 do
    begin
      j := FBuckets[i];
      while j>=0 do
      begin
        ItemDeleted(j);
        j := FNext[j];
      end;
      FBuckets[i] := -1;
    end;

  // put everything to list of free items
  FFirst := -1;
  for i := 0 to FCapacity-1 do
  begin
    FBuckets[i] := -1;
    FNext[i] := FFirst;
    FFirst := i;
  end;
  FCount := 0;
end;

procedure TCUnsortedMap.Swap(handle1, handle2: TCHandle);
begin
  error(erOperation);
//  FSwapItem(FItems[handle1*FItemSize], FItems[handle2*FItemSize], FTemp, FItemSize);
end;

procedure TCUnsortedMap.Copy(Src, Dst: TCHandle);
begin
  error(erOperation);
{  if FDelCount > 0 then
    ItemDeleted(dst);
  if FCopyCount > 0 then
    CopyItem(src, dst)
  else
    FMoveItem(FItems[src*FItemSize], FItems[dst*FItemSize], FItemSize);}
end;

function TCUnsortedMap.First: TCHandle;
var i: integer;
begin
  for i := 0 to FCapacity-1 do
    if FBuckets[i]>=0 then
    begin
      result := FBuckets[i];
      exit;
    end;
  result := -1;
end;

function TCUnsortedMap.Last: TCHandle;
var i: integer;
begin
  for i := FCapacity-1 downto 0 do
    if FBuckets[i]>=0 then
    begin
      result := FBuckets[i];
      while FNext[result]<>-1 do
        result := FNext[result];
      exit;
    end;
  result := -1;
end;

procedure TCUnsortedMap.Next(var n: TCHandle);
var
  i,j: integer;
  r: TCHandle;
begin
  r := n;
  n := FNext[r];
  if n>=0 then
    exit;
  j := FOnCalcHash(FItems[r*FItemSize], FFieldSize, Hash) mod dword(FCapacity);
  for i := j+1 to FCapacity-1 do
    if FBuckets[i]>=0 then
    begin
      n := FBuckets[i];
      exit;
    end;
  n := -1;
end;

procedure TCUnsortedMap.Prev(var n: TCHandle);
var i,j: integer;
begin
  j := FOnCalcHash(FItems[n*FItemSize], FFieldSize, Hash) mod dword(FCapacity);
  i := FBuckets[j];

  if i=n then
  begin
    for i := j-1 downto 0 do
      if FBuckets[i]>=0 then
      begin
        n := FBuckets[i];
        while FNext[n]<>-1 do
          n := FNext[n];
        exit;
      end;
    n := -1;
    exit;
  end;

  repeat
    n := i;
    i := FNext[i];
  until i=n;
end;

function TCUnsortedMap.Find(var Item): TCHandle;
begin
  result := FindFirstEqual(Item);
end;

function TCUnsortedMap.FindFirstEqual(var Item): TCHandle;
begin
  if FCount=0 then
  begin
    result := -1;
    exit;
  end;
  result := FOnCalcHash(Item, FFieldSize, Hash) mod dword(FCapacity);
  result := FBuckets[result];
  while result<>-1 do
    if FCompareValue(Item, FItems[result*FItemSize])=0 then
      exit
    else
      result := FNext[result];
end;

procedure TCUnsortedMap.FindNextEqual(var Item; var handle: TCHandle);
begin
  handle := FNext[handle];
  while handle<>-1 do
    if FCompareValue(Item, FItems[handle*FItemSize])=0 then
      exit
    else
      handle := FNext[handle];
end;

function TCUnsortedMap.Insert(handle: TCHandle): TCHandle;
begin
  result := -1;
  error(erOperation);
end;

function TCUnsortedMap.InsertAfter(handle: TCHandle): TCHandle;
begin
  result := -1;
  error(erOperation);
end;

procedure TCUnsortedMap.Move(Curhandle, Newhandle: TCHandle);
begin
  error(erOperation);
end;

procedure TCUnsortedMap.RandomShuffle;
begin
  error(erOperation);
end;

procedure TCUnsortedMap.RandomShuffle(AFirst, ALast: TCHandle);
begin
  error(erOperation);
end;

procedure TCUnsortedMap.Reverse(AFirst, ALast: TCHandle);
begin
  error(erOperation);
end;

procedure TCUnsortedMap.Reverse;
begin
  error(erOperation);
end;

procedure TCUnsortedMap.Rotate(AFirst, ALast: TCHandle; shift: integer);
begin
  error(erOperation);
end;

procedure TCUnsortedMap.Rotate(shift: integer);
begin
  error(erOperation);
end;

{  TTAnsiString }

constructor TTAnsiString.Create(Container: TCCustom = nil);
begin
  inc(FNeedOnDelete);
  FieldSize := sizeof(TAnsiStringType);
  // delphi-like behaviour by default
  FLexicographic := true;
  FCaseInsensitive := true;
  FIsTextField := true;
  inherited;
end;

function TTAnsiString.Add(Item: TAnsiStringType): TCHandle;
begin
  Result := FContainer.AddItem(item);
  if result<>-1 then
    pointer(Item) := nil;
end;

function  TTAnsiString.Front: TAnsiStringType;          // examine first element
begin
  result := items[FContainer.First];
end;

function  TTAnsiString.Back: TAnsiStringType;           // examine last element
begin
  result := items[FContainer.Last];
end;

function TTAnsiString.Push(v: TAnsiStringType):TCHandle;        // insert element at front
begin
  result := FContainer.InsertItem(FContainer.First, v);
  pointer(v) := nil;
end;

function  TTAnsiString.Pop: TAnsiStringType;            // remove first element
var
  h: TCHandle;
begin
  h := FContainer.First;
  Result := items[h];
  FContainer.Delete(h);
end;

function TTAnsiString.PushBack(v: TAnsiStringType):TCHandle;    // insert element at back
begin
  result := FContainer.AddItem(v);
  pointer(v) := nil;
end;

function  TTAnsiString.PopBack: TAnsiStringType;        // remove last element
var
  h: TCHandle;
begin
  h := FContainer.Last;
  result := items[h];
  FContainer.Delete(h);
end;

function TTAnsiString.Enqueue(v: TAnsiStringType):TCHandle;    // add to tail
begin
  result := FContainer.AddItem(v);
  pointer(v) := nil;
end;

function  TTAnsiString.Dequeue: TAnsiStringType;       // read&remove from head
var
  h: TCHandle;
begin
  h := FContainer.First;
  Result := items[h];
  FContainer.Delete(h);
end;

function TTAnsiString.AddTo(handle: TCHandle; dst: TTCustom): TCHandle;
begin
  Result := dst.Add([items[handle]]);
end;

function TTAnsiString.GetAsVariant(handle: TCHandle): variant;
begin
  if handle=-1 then
    Result := null
  else
    Result := items[handle];
end;

procedure TTAnsiString.SetAsVariant(handle: TCHandle; Value: variant);
begin
  items[handle] := AnsiString(Value);
end;

function TTAnsiString.GetAsAnsiStr(handle: TCHandle): AnsiString;
begin
  result := items[handle];
end;

procedure TTAnsiString.SetAsAnsiStr(handle: TCHandle; Value: AnsiString);
begin
  items[handle] := value;
end;

function TTAnsiString.GetAsWideStr(handle: TCHandle): WideString;
begin
  result := WideString(items[handle]);
end;

procedure TTAnsiString.SetAsWideStr(handle: TCHandle; Value: WideString);
begin
  items[handle] := AnsiString(value);
end;

function TTAnsiString.GetHandleOf(Value: variant): TCHandle;
begin
  Result := Find(AnsiString(Value));
end;

function TTAnsiString.AddList(const Items: array of const): TCHandle;
var
  i: integer;
  p: PVarRec;
begin
  Result := -1;
  for i := low(items) to high(items) do
  begin
    p := @items[i];
    if p.VType = vtObject then
      Result := ImportObject(p.VObject, Result)
    else
      Result := add(VarAsAnsiString(p));
  end;
end;

function TTAnsiString.Find(Item: TAnsiStringType): TCHandle;
begin
  Result := FContainer.Find(item);
end;

function TTAnsiString.FindFirstEqual(Item: TAnsiStringType): TCHandle;
begin
  Result := FContainer.FindFirstEqual(item);
end;

procedure TTAnsiString.FindNextEqual(Item: TAnsiStringType; var handle: TCHandle);
begin
  FContainer.FindNextEqual(item, handle);
end;

function TTAnsiString.GetItem(n: TCHandle): TAnsiStringType;
begin
  Result := TAnsiStringType(FContainer.Get(n)^);
end;

function TTAnsiString.GetMinValue: TAnsiStringType;
begin
  Result := TAnsiStringType(FContainer.Get(FContainer.FindMin)^);
end;

function TTAnsiString.RemoveMinValue: TAnsiStringType;
var
  n: TCHandle;
begin
  n := FContainer.FindMin;
  Result := TAnsiStringType(FContainer.Get(n)^);
  FContainer.Delete(n);
end;

function TTAnsiString.GetMaxValue: TAnsiStringType;
begin
  Result := TAnsiStringType(FContainer.Get(FContainer.FindMax)^);
end;

function TTAnsiString.RemoveMaxValue: TAnsiStringType;
var
  n: TCHandle;
begin
  n := FContainer.FindMax;
  Result := TAnsiStringType(FContainer.Get(n)^);
  FContainer.Delete(n);
end;

function TTAnsiString.Insert(handle: TCHandle; Item: TAnsiStringType): TCHandle;
begin
  Result := FContainer.InsertItem(handle, item);
  pointer(Item) := nil;
end;

function TTAnsiString.InsertAfter(handle: TCHandle; Item: TAnsiStringType): TCHandle;
begin
  Result := FContainer.InsertAfterItem(handle, item);
  pointer(Item) := nil;
end;

procedure TTAnsiString.Remove(Item: TAnsiStringType);
begin
  FContainer.Remove(item);
end;

procedure TTAnsiString.RemoveAll(Item: TAnsiStringType);
begin
  FContainer.RemoveAll(item);
end;

procedure TTAnsiString.SetItem(n: TCHandle; Value: TAnsiStringType);
var p: ^TAnsiStringType;
begin
  p := FContainer.Get(n);
  if p<>nil then
    p^ := '';
  FContainer.Put(n, Value);
  pointer(Value) := nil;
end;

procedure TTAnsiString.InitContainer(c: TCCustom);
begin
  inherited;
  if c<>nil then
  begin
    c.AddOnCopy(DoCopyItem);
    c.InitZero := True;         // otherwise any string-operation on invalid pointer will raise exception!
    c.OnCalcHash := DoCalcHash;
    c.FCompareValue := CmpInt;
  end;
end;

// Delphi functions for string comparison are lexicographical
// but lexicographical comparison differ from regular only
// when length(A)<>length(B)
function TTAnsiString.CmpInt(var a,b):integer;
begin
  if not FLexicographic and (length(AnsiString(a)) <> length(AnsiString(b))) then
    result := length(AnsiString(a))-length(AnsiString(b))
  else
    if FCaseInsensitive then
      result := AnsiCompareText(String(AnsiString(a)), String(AnsiString(b)))
    else
      result := AnsiCompareStr(String(AnsiString(a)), String(AnsiString(b)));
end;

procedure TTAnsiString.SetOnCompare(c: TOnAnsiStringCompare);
begin
  if assigned(c) then
    container.FCompareValue := CmpExt
  else
    container.FCompareValue := CmpInt;
  FOnCompare := c;
  FOnCompareProc := nil;
end;

procedure TTAnsiString.SetOnCompareProc(c: TOnAnsiStringCompareProc);
begin
  if assigned(c) then
    container.FCompareValue := CmpExtProc
  else
    container.FCompareValue := CmpInt;
  FOnCompareProc := c;
  FOnCompare := nil;
end;

function TTAnsiString.CmpExt(var a, b): integer;
begin
  Result := FOnCompare(TAnsiStringType(a), TAnsiStringType(b));
end;

function TTAnsiString.CmpExtProc(var a, b): integer;
begin
  Result := FOnCompareProc(TAnsiStringType(a), TAnsiStringType(b));
end;

procedure TTAnsiString.Sort(compare: TOnAnsiStringCompare);
begin
  Sort(compare, FContainer.First, FContainer.Last);
end;

procedure TTAnsiString.Sort(compare: TOnAnsiStringCompareProc);
begin
  Sort(compare, FContainer.First, FContainer.Last);
end;

procedure TTAnsiString.Sort(compare: TOnAnsiStringCompare; AFirst, ALast: TCHandle);
begin
  FOnCompare := compare;
  FContainer.Sort(CmpExt, AFirst, ALast);
end;

procedure TTAnsiString.Sort(compare: TOnAnsiStringCompareProc; AFirst, ALast: TCHandle);
begin
  FOnCompareProc := compare;
  FContainer.Sort(CmpExtProc, AFirst, ALast);
end;

function TTAnsiString.STCopy(Dst: TTCustom; h: TCHandle):TCHandle;
begin
  result := TTAnsiString(dst).Add(Items[h]);
end;

procedure TTAnsiString.STCopy(Dst: TTCustom; AFirst, ALast: TCHandle);
begin
  TTAnsiString(dst).Add(Items[AFirst]);
  if AFirst<>ALast then
    repeat
      FContainer.Next(AFirst);
      TTAnsiString(dst).Add(Items[AFirst]);
    until AFirst=ALast;
end;

procedure TTAnsiString.STSet(Dst: TTCustom; h1,h2: TCHandle);
begin
  TTAnsiString(dst)[h2] := Items[h1];
end;

procedure TTAnsiString.STSet(Dst: TTCustom; start1,finish1, start2: TCHandle);
begin
  TTAnsiString(dst)[start2] := Items[start1];
  if start1<>finish1 then
    repeat
      FContainer.Next(start1);
      dst.FContainer.Next(start2);
      TTAnsiString(dst)[start2] := Items[start1];
    until start1=finish1;
end;

function TTAnsiString.GetMap(key: TAnsiStringType): variant;
begin
  Result := FContainer.Fields[1].AsVariant[Find(key)];
end;

procedure TTAnsiString.PutMap(key: TAnsiStringType; Value: variant);
var
  h: TCHandle;
begin
  h := Find(key);
  if h = -1 then
    AddPair(key, Value)
  else
    FContainer.Fields[1].AsVariant[h] := Value;
end;

function TTAnsiString.AddPair(key: TAnsiStringType; Value: variant): TCHandle;
begin
  result := Add(key);
  Container.Fields[1].AsVariant[result] := Value;
end;

procedure TTAnsiString.AssignShape(src: TTCustom);
begin
  if src is TTAnsiString then
    with src as TTAnsiString do
    begin
      self.Lexicographic := Lexicographic;
      self.CaseInsensitive := CaseInsensitive;
    end;
end;

procedure TTAnsiString.DoCopyItem(Src, Dst: TCHandle);
var
  s,d: ^TAnsiStringType;
begin
  s := FContainer[src];
  d := FContainer[dst];
  d^ := s^;
end;

function  TTAnsiString.DoCalcHash(var Item; Count: integer; h: T_HashCustom): cardinal;
begin
  if TAnsiStringType(Item) = '' then
    Result := 0
  else
    Result := h.Get(TAnsiStringType(Item)[1], length(TAnsiStringType(Item))*SizeOf(AnsiChar));
end;

procedure TTAnsiString.ItemDeleted(n: TCHandle);
begin
  TAnsiStringType(FContainer[n]^) := '';
  inherited;
end;

procedure TTAnsiString.SetCaseInsensitive(value: boolean);
begin
  FCaseInsensitive := value;
end;

procedure TTAnsiString.SetLexicographic(value: boolean);
begin
  FLexicographic := value;
end;

procedure TTAnsiString.SaveItem(h: TCHandle; dst: TStream; FileFormat : TFileFormat = ffDefault);
begin
  if FileFormat<>ffBinary then
    inherited
  else
    WriteAnsiString(dst, items[h]);
end;

function TTAnsiString.LoadItem(c: TCCustom; h: TCHandle; src: TStream; FileFormat : TFileFormat = ffDefault):TCHandle;
var v: TAnsiStringType;
begin
  if FileFormat=ffBinary then
    v := ReadAnsiString(src)
  else
    v := AnsiString(c.LoadToken);
  if h=-1 then
    result := Add(v)
  else
  begin
    result := h;
    items[h] := v;
  end;
end;

{  TTWideString }

constructor TTWideString.Create(Container: TCCustom = nil);
begin
  inc(FNeedOnDelete);
  FieldSize := sizeof(TWideStringType);
  // delphi-like behaviour by default
  FLexicographic := true;
  FCaseInsensitive := true;
  FIsTextField := true;
  inherited;
end;

function TTWideString.Add(Item: TWideStringType): TCHandle;
begin
  Result := FContainer.AddItem(item);
  if result<>-1 then
    pointer(Item) := nil;
end;

function  TTWideString.Front: TWideStringType;          // examine first element
begin
  result := items[FContainer.First];
end;

function  TTWideString.Back: TWideStringType;           // examine last element
begin
  result := items[FContainer.Last];
end;

function TTWideString.Push(v: TWideStringType):TCHandle;        // insert element at front
begin
  result := FContainer.InsertItem(FContainer.First, v);
  pointer(v) := nil;
end;

function  TTWideString.Pop: TWideStringType;            // remove first element
var
  h: TCHandle;
begin
  h := FContainer.First;
  Result := items[h];
  FContainer.Delete(h);
end;

function TTWideString.PushBack(v: TWideStringType):TCHandle;    // insert element at back
begin
  result := FContainer.AddItem(v);
  pointer(v) := nil;
end;

function  TTWideString.PopBack: TWideStringType;        // remove last element
var
  h: TCHandle;
begin
  h := FContainer.Last;
  result := items[h];
  FContainer.Delete(h);
end;

function TTWideString.Enqueue(v: TWideStringType):TCHandle;    // add to tail
begin
  result := FContainer.AddItem(v);
  pointer(v) := nil;
end;

function  TTWideString.Dequeue: TWideStringType;       // read&remove from head
var
  h: TCHandle;
begin
  h := FContainer.First;
  Result := items[h];
  FContainer.Delete(h);
end;

function TTWideString.AddTo(handle: TCHandle; dst: TTCustom): TCHandle;
begin
  Result := dst.Add([items[handle]]);
end;

function TTWideString.GetAsVariant(handle: TCHandle): variant;
begin
  if handle=-1 then
    Result := null
  else
    Result := items[handle];
end;

procedure TTWideString.SetAsVariant(handle: TCHandle; Value: variant);
begin
  items[handle] := Value;
end;

function TTWideString.GetAsAnsiStr(handle: TCHandle): AnsiString;
begin
  result := AnsiString(items[handle]);
end;

procedure TTWideString.SetAsAnsiStr(handle: TCHandle; Value: AnsiString);
begin
  items[handle] := WideString(value);
end;

function TTWideString.GetAsWideStr(handle: TCHandle): WideString;
begin
  result := items[handle];
end;

procedure TTWideString.SetAsWideStr(handle: TCHandle; Value: WideString);
begin
  items[handle] := value;
end;

function TTWideString.GetHandleOf(Value: variant): TCHandle;
begin
  Result := Find(Value);
end;

function TTWideString.AddList(const Items: array of const): TCHandle;
var
  i: integer;
  p: PVarRec;
begin
  Result := -1;
  for i := low(items) to high(items) do
  begin
    p := @items[i];
    if p.VType = vtObject then
      Result := ImportObject(p.VObject, Result)
    else
      Result := add(VarAsWideString(p));
  end;
end;

function TTWideString.Find(Item: TWideStringType): TCHandle;
begin
  Result := FContainer.Find(item);
end;

function TTWideString.FindFirstEqual(Item: TWideStringType): TCHandle;
begin
  Result := FContainer.FindFirstEqual(item);
end;

procedure TTWideString.FindNextEqual(Item: TWideStringType; var handle: TCHandle);
begin
  FContainer.FindNextEqual(item, handle);
end;

function TTWideString.GetItem(n: TCHandle): TWideStringType;
begin
  Result := TWideStringType(FContainer.Get(n)^);
end;

function TTWideString.GetMinValue: TWideStringType;
begin
  Result := TWideStringType(FContainer.Get(FContainer.FindMin)^);
end;

function TTWideString.RemoveMinValue: TWideStringType;
var
  n: TCHandle;
begin
  n := FContainer.FindMin;
  Result := TWideStringType(FContainer.Get(n)^);
  FContainer.Delete(n);
end;

function TTWideString.GetMaxValue: TWideStringType;
begin
  Result := TWideStringType(FContainer.Get(FContainer.FindMax)^);
end;

function TTWideString.RemoveMaxValue: TWideStringType;
var
  n: TCHandle;
begin
  n := FContainer.FindMax;
  Result := TWideStringType(FContainer.Get(n)^);
  FContainer.Delete(n);
end;

function TTWideString.Insert(handle: TCHandle; Item: TWideStringType): TCHandle;
begin
  Result := FContainer.InsertItem(handle, item);
  pointer(Item) := nil;
end;

function TTWideString.InsertAfter(handle: TCHandle; Item: TWideStringType): TCHandle;
begin
  Result := FContainer.InsertAfterItem(handle, item);
  pointer(Item) := nil;
end;

procedure TTWideString.Remove(Item: TWideStringType);
begin
  FContainer.Remove(item);
end;

procedure TTWideString.RemoveAll(Item: TWideStringType);
begin
  FContainer.RemoveAll(item);
end;

procedure TTWideString.SetItem(n: TCHandle; Value: TWideStringType);
var p: ^TWideStringType;
begin
  p := FContainer.Get(n);
  if p<>nil then
    p^ := '';
  FContainer.Put(n, Value);
  pointer(Value) := nil;
end;

procedure TTWideString.InitContainer(c: TCCustom);
begin
  inherited;
  if c<>nil then
  begin
    c.AddOnCopy(DoCopyItem);
    c.InitZero := True;         // otherwise any string-operation on invalid pointer will raise exception!
    c.OnCalcHash := DoCalcHash;
    c.FCompareValue := CmpInt;
  end;
end;

// Delphi functions for string comparison are lexicographical
// but lexicographical comparison differ from regular only
// when length(A)<>length(B)
function TTWideString.CmpInt(var a,b):integer;
begin
  if not FLexicographic and (length(WideString(a)) <> length(WideString(b))) then
    result := length(WideString(a))-length(WideString(b))
  else
    if FCaseInsensitive then
      result := WideCompareText(WideString(a), WideString(b))
    else
      result := WideCompareStr(WideString(a), WideString(b));
end;

procedure TTWideString.SetOnCompare(c: TOnWideStringCompare);
begin
  if assigned(c) then
    container.FCompareValue := CmpExt
  else
    container.FCompareValue := CmpInt;
  FOnCompare := c;
  FOnCompareProc := nil;
end;

procedure TTWideString.SetOnCompareProc(c: TOnWideStringCompareProc);
begin
  if assigned(c) then
    container.FCompareValue := CmpExtProc
  else
    container.FCompareValue := CmpInt;
  FOnCompareProc := c;
  FOnCompare := nil;
end;

function TTWideString.CmpExt(var a, b): integer;
begin
  Result := FOnCompare(TWideStringType(a), TWideStringType(b));
end;

function TTWideString.CmpExtProc(var a, b): integer;
begin
  Result := FOnCompareProc(TWideStringType(a), TWideStringType(b));
end;

procedure TTWideString.Sort(compare: TOnWideStringCompare);
begin
  Sort(compare, FContainer.First, FContainer.Last);
end;

procedure TTWideString.Sort(compare: TOnWideStringCompareProc);
begin
  Sort(compare, FContainer.First, FContainer.Last);
end;

procedure TTWideString.Sort(compare: TOnWideStringCompare; AFirst, ALast: TCHandle);
begin
  FOnCompare := compare;
  FContainer.Sort(CmpExt, AFirst, ALast);
end;

procedure TTWideString.Sort(compare: TOnWideStringCompareProc; AFirst, ALast: TCHandle);
begin
  FOnCompareProc := compare;
  FContainer.Sort(CmpExtProc, AFirst, ALast);
end;

function TTWideString.STCopy(Dst: TTCustom; h: TCHandle):TCHandle;
begin
  result := TTWideString(dst).Add(Items[h]);
end;

procedure TTWideString.STCopy(Dst: TTCustom; AFirst, ALast: TCHandle);
begin
  TTWideString(dst).Add(Items[AFirst]);
  if AFirst<>ALast then
    repeat
      FContainer.Next(AFirst);
      TTWideString(dst).Add(Items[AFirst]);
    until AFirst=ALast;
end;

procedure TTWideString.STSet(Dst: TTCustom; h1,h2: TCHandle);
begin
  TTWideString(dst)[h2] := Items[h1];
end;

procedure TTWideString.STSet(Dst: TTCustom; start1,finish1, start2: TCHandle);
begin
  TTWideString(dst)[start2] := Items[start1];
  if start1<>finish1 then
    repeat
      FContainer.Next(start1);
      dst.FContainer.Next(start2);
      TTWideString(dst)[start2] := Items[start1];
    until start1=finish1;
end;

function TTWideString.GetMap(key: TWideStringType): variant;
begin
  Result := FContainer.Fields[1].AsVariant[Find(key)];
end;

procedure TTWideString.PutMap(key: TWideStringType; Value: variant);
var
  h: TCHandle;
begin
  h := Find(key);
  if h = -1 then
    AddPair(key, Value)
  else
    FContainer.Fields[1].AsVariant[h] := Value;
end;

function TTWideString.AddPair(key: TWideStringType; Value: variant): TCHandle;
begin
  result := Add(key);
  Container.Fields[1].AsVariant[result] := Value;
end;

procedure TTWideString.ItemDeleted(n: TCHandle);
begin
  TWideStringType(FContainer[n]^) := '';
  inherited;
end;

procedure TTWideString.SetCaseInsensitive(value: boolean);
begin
  FCaseInsensitive := value;
end;

procedure TTWideString.SetLexicographic(value: boolean);
begin
  FLexicographic := value;
end;

procedure TTWideString.SaveItem(h: TCHandle; dst: TStream; FileFormat : TFileFormat = ffDefault);
begin
  if FileFormat<>ffBinary then
    inherited
  else
    WriteWideString(dst, items[h]);
end;

function TTWideString.LoadItem(c: TCCustom; h: TCHandle; src: TStream; FileFormat : TFileFormat = ffDefault):TCHandle;
var v: TWideStringType;
begin
  if FileFormat=ffBinary then
    v := ReadWideString(src)
  else
    v := c.LoadToken;
  if h=-1 then
    result := Add(v)
  else
  begin
    result := h;
    items[h] := v;
  end;
end;

procedure TTWideString.AssignShape(src: TTCustom);
begin
  if src is TTWideString then
    with src as TTWideString do
    begin
      self.Lexicographic := Lexicographic;
      self.CaseInsensitive := CaseInsensitive;
    end;
end;

function TTWideString.DoCalcHash(var Item; Count: integer; h: T_HashCustom): cardinal;
begin
  if TWideStringType(Item) = '' then
    Result := 0
  else
    Result := h.Get(TWideStringType(Item)[1], length(TWideStringType(Item))*SizeOf(WideChar));
end;

procedure TTWideString.DoCopyItem(Src, Dst: TCHandle);
var
  s,d: ^TWideStringType;
begin
  s := FContainer[src];
  d := FContainer[dst];
  d^ := s^;
end;

{  TTString }

constructor TTString.Create(Container: TCCustom = nil);
begin
  inc(FNeedOnDelete);
  FieldSize := sizeof(TStringType);
  // delphi-like behaviour by default
  FLexicographic := true;
  FCaseInsensitive := true;
  FIsTextField := true;
  inherited;
end;

function TTString.Add(Item: TStringType): TCHandle;
begin
  Result := FContainer.AddItem(item);
  if result<>-1 then
    pointer(Item) := nil;
end;

function TTString.AddChild(AParent: TCHandle; Item: TStringType): TCHandle;
begin
  Result := FContainer.AddChildItem(AParent, item);
  if result<>-1 then
    pointer(Item) := nil;
end;

function  TTString.Front: TStringType;          // examine first element
begin
  result := items[FContainer.First];
end;

function  TTString.Back: TStringType;           // examine last element
begin
  result := items[FContainer.Last];
end;

function TTString.Push(v: TStringType):TCHandle;        // insert element at front
begin
  result := FContainer.InsertItem(FContainer.First, v);
  pointer(v) := nil;
end;

function  TTString.Pop: TStringType;            // remove first element
var
  h: TCHandle;
begin
  h := FContainer.First;
  Result := items[h];
  FContainer.Delete(h);
end;

function TTString.PushBack(v: TStringType):TCHandle;    // insert element at back
begin
  result := FContainer.AddItem(v);
  pointer(v) := nil;
end;

function  TTString.PopBack: TStringType;        // remove last element
var
  h: TCHandle;
begin
  h := FContainer.Last;
  result := items[h];
  FContainer.Delete(h);
end;

function TTString.Enqueue(v: TStringType):TCHandle;    // add to tail
begin
  result := FContainer.AddItem(v);
  pointer(v) := nil;
end;

function  TTString.Dequeue: TStringType;       // read&remove from head
var
  h: TCHandle;
begin
  h := FContainer.First;
  Result := items[h];
  FContainer.Delete(h);
end;

function TTString.AddTo(handle: TCHandle; dst: TTCustom): TCHandle;
begin
  Result := dst.Add([items[handle]]);
end;

function TTString.GetAsVariant(handle: TCHandle): variant;
begin
  if handle=-1 then
    Result := null
  else
    Result := items[handle];
end;

procedure TTString.SetAsVariant(handle: TCHandle; Value: variant);
begin
  items[handle] := Value;
end;

function TTString.GetAsAnsiStr(handle: TCHandle): AnsiString;
begin
  result := AnsiString(items[handle]);
end;

procedure TTString.SetAsAnsiStr(handle: TCHandle; Value: AnsiString);
begin
  items[handle] := String(value);
end;

function TTString.GetAsWideStr(handle: TCHandle): WideString;
begin
  result := items[handle];
end;

procedure TTString.SetAsWideStr(handle: TCHandle; Value: WideString);
begin
  items[handle] := value;
end;

function TTString.GetHandleOf(Value: variant): TCHandle;
begin
  Result := Find(Value);
end;

function TTString.AddList(const Items: array of const): TCHandle;
var
  i: integer;
  p: PVarRec;
begin
  Result := -1;
  for i := low(items) to high(items) do
  begin
    p := @items[i];
    if p.VType = vtObject then
      Result := ImportObject(p.VObject, Result)
    else
      Result := add(VarAsString(p));
  end;
end;

function TTString.Find(Item: TStringType): TCHandle;
begin
  Result := FContainer.Find(item);
end;

function TTString.FindFirstEqual(Item: TStringType): TCHandle;
begin
  Result := FContainer.FindFirstEqual(item);
end;

procedure TTString.FindNextEqual(Item: TStringType; var handle: TCHandle);
begin
  FContainer.FindNextEqual(item, handle);
end;

function TTString.GetItem(n: TCHandle): TStringType;
var p: ^TStringType;
begin
  p := FContainer.Get(n);
  Result := p^;
end;

function TTString.GetMinValue: TStringType;
begin
  Result := TStringType(FContainer.Get(FContainer.FindMin)^);
end;

function TTString.RemoveMinValue: TStringType;
var
  n: TCHandle;
begin
  n := FContainer.FindMin;
  Result := TStringType(FContainer.Get(n)^);
  FContainer.Delete(n);
end;

function TTString.GetMaxValue: TStringType;
begin
  Result := TStringType(FContainer.Get(FContainer.FindMax)^);
end;

function TTString.RemoveMaxValue: TStringType;
var
  n: TCHandle;
begin
  n := FContainer.FindMax;
  Result := TStringType(FContainer.Get(n)^);
  FContainer.Delete(n);
end;

function TTString.Insert(handle: TCHandle; Item: TStringType): TCHandle;
begin
  Result := FContainer.InsertItem(handle, item);
  pointer(Item) := nil;
end;

function TTString.InsertAfter(handle: TCHandle; Item: TStringType): TCHandle;
begin
  Result := FContainer.InsertAfterItem(handle, item);
  pointer(Item) := nil;
end;

procedure TTString.Remove(Item: TStringType);
begin
  FContainer.Remove(item);
end;

procedure TTString.RemoveAll(Item: TStringType);
begin
  FContainer.RemoveAll(item);
end;

procedure TTString.SetItem(n: TCHandle; Value: TStringType);
var p: ^TStringType;
begin
  p := FContainer.Get(n);
  if p<>nil then
    p^ := '';
  FContainer.Put(n, Value);
  pointer(Value) := nil;
end;

procedure TTString.InitContainer(c: TCCustom);
begin
  inherited;
  if c<>nil then
  begin
    c.AddOnCopy(DoCopyItem);
    c.InitZero := True;         // otherwise any string-operation on invalid pointer will raise exception!
    c.OnCalcHash := DoCalcHash;
    c.FCompareValue := CmpInt;
  end;
end;

// Delphi functions for string comparison are lexicographical
// but lexicographical comparison differ from regular only
// when length(A)<>length(B)
function TTString.CmpInt(var a,b):integer;
begin
  if not FLexicographic and (length(String(a)) <> length(String(b))) then
    result := length(String(a))-length(String(b))
  else
    if FCaseInsensitive then
      result := AnsiCompareText(String(a), String(b))
    else
      result := AnsiCompareStr(String(a), String(b));
end;

procedure TTString.SetOnCompare(c: TOnStringCompare);
begin
  if assigned(c) then
    container.FCompareValue := CmpExt
  else
    container.FCompareValue := CmpInt;
  FOnCompare := c;
  FOnCompareProc := nil;
end;

procedure TTString.SetOnCompareProc(c: TOnStringCompareProc);
begin
  if assigned(c) then
    container.FCompareValue := CmpExtProc
  else
    container.FCompareValue := CmpInt;
  FOnCompareProc := c;
  FOnCompare := nil;
end;

function TTString.CmpExt(var a, b): integer;
begin
  Result := FOnCompare(TStringType(a), TStringType(b));
end;

function TTString.CmpExtProc(var a, b): integer;
begin
  Result := FOnCompareProc(TStringType(a), TStringType(b));
end;

procedure TTString.Sort(compare: TOnStringCompare);
begin
  Sort(compare, FContainer.First, FContainer.Last);
end;

procedure TTString.Sort(compare: TOnStringCompareProc);
begin
  Sort(compare, FContainer.First, FContainer.Last);
end;

procedure TTString.Sort(compare: TOnStringCompare; AFirst, ALast: TCHandle);
begin
  FOnCompare := compare;
  FContainer.Sort(CmpExt, AFirst, ALast);
end;

procedure TTString.Sort(compare: TOnStringCompareProc; AFirst, ALast: TCHandle);
begin
  FOnCompareProc := compare;
  FContainer.Sort(CmpExtProc, AFirst, ALast);
end;

function TTString.STCopy(Dst: TTCustom; h: TCHandle):TCHandle;
begin
  result := TTString(dst).Add(Items[h]);
end;

procedure TTString.STCopy(Dst: TTCustom; AFirst, ALast: TCHandle);
begin
  TTString(dst).Add(Items[AFirst]);
  if AFirst<>ALast then
    repeat
      FContainer.Next(AFirst);
      TTString(dst).Add(Items[AFirst]);
    until AFirst=ALast;
end;

procedure TTString.STSet(Dst: TTCustom; h1,h2: TCHandle);
begin
  TTString(dst)[h2] := Items[h1];
end;

procedure TTString.STSet(Dst: TTCustom; start1,finish1, start2: TCHandle);
begin
  TTString(dst)[start2] := Items[start1];
  if start1<>finish1 then
    repeat
      FContainer.Next(start1);
      dst.FContainer.Next(start2);
      TTString(dst)[start2] := Items[start1];
    until start1=finish1;
end;

function TTString.GetMap(key: TStringType): variant;
begin
  Result := FContainer.Fields[1].AsVariant[Find(key)];
end;

procedure TTString.PutMap(key: TStringType; Value: variant);
var
  h: TCHandle;
begin
  h := Find(key);
  if h = -1 then
    AddPair(key, Value)
  else
    FContainer.Fields[1].AsVariant[h] := Value;
end;

function TTString.AddPair(key: TStringType; Value: variant): TCHandle;
begin
  result := Add(key);
  Container.Fields[1].AsVariant[result] := Value;
end;

procedure TTString.AssignShape(src: TTCustom);
begin
  if src is TTString then
    with src as TTString do
    begin
      self.Lexicographic := Lexicographic;
      self.CaseInsensitive := CaseInsensitive;
    end;
end;

procedure TTString.DoCopyItem(Src, Dst: TCHandle);
var
  s,d: ^TStringType;
begin
  s := FContainer[src];
  d := FContainer[dst];
  d^ := s^;
end;

function  TTString.DoCalcHash(var Item; Count: integer; h: T_HashCustom): cardinal;
begin
  if TStringType(Item) = '' then
    Result := 0
  else
    Result := h.Get(TStringType(Item)[1], length(TStringType(Item))*SizeOf(Char));
end;

procedure TTString.ItemDeleted(n: TCHandle);
begin
  TStringType(FContainer[n]^) := '';
  inherited;
end;

procedure TTString.SetCaseInsensitive(value: boolean);
begin
  FCaseInsensitive := value;
end;

procedure TTString.SetLexicographic(value: boolean);
begin
  FLexicographic := value;
end;

procedure TTString.SaveItem(h: TCHandle; dst: TStream; FileFormat : TFileFormat = ffDefault);
begin
  if FileFormat<>ffBinary then
    inherited
  else
    WriteString(dst, items[h]);
end;

function TTString.LoadItem(c: TCCustom; h: TCHandle; src: TStream; FileFormat : TFileFormat = ffDefault):TCHandle;
var v: TStringType;
begin
  if FileFormat=ffBinary then
    v := ReadString(src)
  else
    v := c.LoadToken;
  if h=-1 then
    result := Add(v)
  else
  begin
    result := h;
    items[h] := v;
  end;
end;

function VariantToStr(const v: variant): string;
begin
  // must be splitted into 2 commands
  // otherwise result of IntToStr may be converted to variant and
  // concatenation became math summary (found under D7)!
  result := v;
  result := IntToStr(VarType(V) and varTypeMask) + ' ' + result;
end;

function VariantToAnsiStr(const v: variant): AnsiString;
begin
  // must be splitted into 2 commands
  // otherwise result of IntToStr may be converted to variant and
  // concatenation became math summary (found under D7)!
  result := AnsiString(v);
  result := AnsiString(IntToStr(VarType(V) and varTypeMask)) + ' ' + result;
end;

function VariantToWideStr(const v: variant): WideString;
begin
  // must be splitted into 2 commands
  // otherwise result of IntToStr may be converted to variant and
  // concatenation became math summary (found under D7)!
  result := v;
  result := IntToStr(VarType(V) and varTypeMask) + ' ' + result;
end;

function StrToSingle(const s: string): single;
begin
  result := StrToFloat(s);
end;

function StrToDWord(const s: string): dword;
begin
  result := StrToInt64(s);
end;

function StrToWordBool(const s: string): WordBool;
begin
  result := StrToBool(s);
end;

function WideStringToStr(const v: WideString): string;
begin
  result := v;
end;

function AnsiStringToStr(const v: AnsiString): string;
begin
  result := String(v);
end;

function AnsiStringToWideStr(const v: AnsiString): WideString;
begin
  result := WideString(v);
end;

function WideStringToAnsiStr(const v: WideString): AnsiString;
begin
  result := AnsiString(v);
end;

{$IFDEF HiCompiler}
function AnsiStringToUInt64(const v: AnsiString): UInt64;
var i: integer;
begin
  result := 0;
  for i := 1 to length(v) do
    if (v[i]>='0') and (v[i]<='9') then
      result := result*10+byte(v[i])
    else
      if (v[i]<>'+') or (i<>1) then
        raise exception.Create(format('"%s" is not UInt64 value', [v]));
end;

function WideStringToUInt64(const v: WideString): UInt64;
var i: integer;
begin
  result := 0;
  for i := 1 to length(v) do
    case v[i] of
      '0': result := result*10;
      '1': result := result*10+1;
      '2': result := result*10+2;
      '3': result := result*10+3;
      '4': result := result*10+4;
      '5': result := result*10+5;
      '6': result := result*10+6;
      '7': result := result*10+7;
      '8': result := result*10+8;
      '9': result := result*10+9;
      else
        if (v[i]<>'+') or (i<>1) then
          raise exception.Create(format('"%s" is not UInt64 value', [v]));
    end;
end;
{$ENDIF}

function AnsiStrToVariant(v: AnsiString; vtype: integer = -1): variant;
var
  i,p: integer;
begin
  if vtype=-1 then
  begin
    v := AnsiString(Trim(String(v)));
    p := 0;
    for i := 1 to length(v) do
      if v[i]=' ' then
      begin
        p := i;
        break;
      end;

    // v does not contain type of variant
    if p<=0 then
    begin
      result := v;
      exit;
    end;

    vtype := StrToInt(system.copy(string(v),1,p-1));
    v := system.copy(v,p+1,high(integer));
  end;

  case vtype of
    varEmpty:   result := Unassigned;                //The variant is Unassigned.
    varNull:	result := Null;                      //The variant is Null.
    varSmallint:result := SmallInt(StrToInt(String(v)));     //16-bit signed integer (type Smallint in Delphi, short in C++ ).
    varInteger:	result := StrToInt(String(v));               //32-bit signed integer (type Integer in Delphi, int in C++).
    varSingle:	result := StrToSingle(String(v));            //Single-precision floating-point v (type Single in Delphi, float in C++).
    varDouble:	result := StrToFloat(String(v));             //Double-precision floating-point v (type double).
    varCurrency:result := StrToCurr(String(v));              //Currency floating-point v (type Currency).
    varDate:	result := StrToDateTime(String(v));          //Date and time v (type TDateTime).

    varOleStr:	result := StringToOleStr(v)^;        //Reference to a dynamically allocated UNICODE string.
    varDispatch:result := Null;                      //Reference to an Automation object (an IDispatch interface pointer).
    varError:	result := StrToInt(String(v));               //Operating system error code.
    varBoolean:	result := StrToWordBool(String(v));          //16-bit boolean (type WordBool).
    varVariant:	result := Null;                      //A variant.
    varUnknown:	result := Null;                      //Reference to an unknown object (an IInterface or IUnknown interface pointer).
    varShortInt:result := ShortInt(StrToInt(String(v)));     //8-bit signed integer (type ShortInt in Delphi or signed char in C++)
    varByte:   	result := Byte(StrToInt(String(v)));         //A Byte
    varWord:	result := Word(StrToInt(String(v)));         //unsigned 16-bit v (Word)

    varLongWord:result := StrToDWord(String(v));             //unsigned 32-bit v (type LongWord in Delphi or unsigned long in C++)
    varInt64:	result := StrToInt64(String(v));             //64-bit signed integer (Int64 in Delphi or __int64 in C++)
    varStrArg:	result := AnsiStringToWideStr(v);    //COM-compatible string.
    varString:	result := AnsiStringToStr(v);        //Reference to a dynamically allocated string (not COM compatible).

{$IFDEF HiCompiler}
    varUInt64: result := AnsiStringToUInt64(v); // vt_ui8 (21)
    varUString: result := UnicodeString(v); // Unicode string (258), not OLE compatible
{$ENDIF}

    // varAny      = $0101; // Corba any (257), not OLE compatible
    // varArray    = $2000;
    // varByRef    = $4000;
    else raise exception.create(erOperation);
  end;
end;

function WideStrToVariant(v: WideString; vtype: integer = -1): variant;
var
  i,p: integer;
begin
  if vtype=-1 then
  begin
    v := Trim(v);
    p := 0;
    for i := 1 to length(v) do
      if v[i]=' ' then
      begin
        p := i;
        break;
      end;

    // v does not contain type of variant
    if p<=0 then
    begin
      result := v;
      exit;
    end;

    vtype := StrToInt(system.copy(v,1,p-1));
    v := system.copy(v,p+1,high(integer));
  end;

  case vtype of
    varEmpty:   result := Unassigned;                //The variant is Unassigned.
    varNull:	result := Null;                      //The variant is Null.
    varSmallint:result := SmallInt(StrToInt(v));     //16-bit signed integer (type Smallint in Delphi, short in C++ ).
    varInteger:	result := StrToInt(v);               //32-bit signed integer (type Integer in Delphi, int in C++).
    varSingle:	result := StrToSingle(v);            //Single-precision floating-point v (type Single in Delphi, float in C++).
    varDouble:	result := StrToFloat(v);             //Double-precision floating-point v (type double).
    varCurrency:result := StrToCurr(v);              //Currency floating-point v (type Currency).
    varDate:	result := StrToDateTime(v);          //Date and time v (type TDateTime).

    varOleStr:	result := v;                         //Reference to a dynamically allocated UNICODE string.
    varDispatch:result := Null;                      //Reference to an Automation object (an IDispatch interface pointer).
    varError:	result := StrToInt(v);               //Operating system error code.
    varBoolean:	result := StrToWordBool(v);          //16-bit boolean (type WordBool).
    varVariant:	result := Null;                      //A variant.
    varUnknown:	result := Null;                      //Reference to an unknown object (an IInterface or IUnknown interface pointer).
    varShortInt:result := ShortInt(StrToInt(v));     //8-bit signed integer (type ShortInt in Delphi or signed char in C++)
    varByte:   	result := Byte(StrToInt(v));         //A Byte
    varWord:	result := Word(StrToInt(v));         //unsigned 16-bit v (Word)

    varLongWord:result := StrToDWord(v);             //unsigned 32-bit v (type LongWord in Delphi or unsigned long in C++)
    varInt64:	result := StrToInt64(v);             //64-bit signed integer (Int64 in Delphi or __int64 in C++)
    varStrArg:	result := v;                         //COM-compatible string.
    varString:	result := WideStringToStr(v);        //Reference to a dynamically allocated string (not COM compatible).

{$IFDEF HiCompiler}
    varUInt64: result := WideStringToUInt64(v); // vt_ui8 (21)
    varUString: result := UnicodeString(v); // Unicode string (258), not OLE compatible
{$ENDIF}

    // varAny      = $0101; // Corba any (257), not OLE compatible
    // varArray    = $2000;
    // varByRef    = $4000;
    else raise exception.create(erOperation);
  end;
end;

{ TConsoleOutputStream }

function TConsoleOutputStream.Seek(Offset: Integer; Origin: Word): Longint;
begin
  {$IFDEF FPC} result:=0; {$ENDIF} 
  raise exception.create(erOperation);
end;

function TConsoleOutputStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  {$IFDEF FPC} result:=0; {$ENDIF} 
  raise exception.create(erOperation);
end;

procedure TConsoleOutputStream.SetSize(NewSize: Integer);
begin
  raise exception.create(erOperation);
end;

procedure TConsoleOutputStream.SetSize(const NewSize: Int64);
begin
  raise exception.create(erOperation);
end;

function TConsoleOutputStream.GetSize: Int64;
begin
  {$IFDEF FPC} result:=0; {$ENDIF} 
  raise exception.create(erOperation);
end;

function TConsoleOutputStream.Read(var Buffer; Count: Integer): Longint;
begin
  {$IFDEF FPC} result:=0; {$ENDIF} 
  raise exception.create(erOperation);
end;

function TConsoleOutputStream.Write(const Buffer; Count: Integer): Longint;
var
  c: ^char;
  i: integer;
begin
  result := count;
  c := @buffer;
  for i := 0 to Count div sizeof(char)-1 do
  begin
    system.write(c^);
    inc(c);
  end;
end;

class procedure TConsoleOutputStream.writeln;
begin
  system.writeln;
end;

{ TCTree }

function TCTree.treeAddChild(AParent: PTreeNode): PTreeNode;
begin
  assert(AParent<>nil);
  result := AllocNewItem;
  result.Parent := AParent;
  DListAdd(@result.ListItem, @AParent.Childs);
  Inc(FCount);
  Inc(AParent.ChildCount);
  repeat
    Inc(AParent.TotalChildCount);
    AParent := AParent.Parent;
  until AParent=nil;
end;

// copy structure only (creates empty items)!
procedure TCTree.treeCopyStructure(dst: TCTree);
var
  s,d: PTreeNode;
begin
  s := @FRoot;
  d := @dst.FRoot;
  while true do
    if s.FirstChild<>nil then
    begin
      s := s.FirstChild;
      d := dst.treeAddChild(d);
    end
    else
      begin
        while (s<>nil) and (s.NextSibling=nil) do
        begin
          s := s.Parent;
          d := d.Parent;
        end;
        if s=nil then
          break;
        s := s.NextSibling;
        d := dst.treeAddChild(d.Parent);
      end;
end;

// copy data (destination tree must have same structure)!
procedure TCTree.treeCopyData(dst: TCTree);
var
  i: integer;
  f,l,d: TCHandle;
begin
  f := First;
  l := Last;
  d := dst.First;
  for i := 0 to High(FFields) do
    FFields[i].STSet(dst.FFields[i], f, l, d);
end;

procedure TCTree.Assign(src: TCCustom);
var
  i: integer;
begin
  assert(src.FieldCount=FieldCount, erOperation);
  if not (src is ClassType) then
  begin
    inherited;
    exit;
  end;
  GrowDown := src.GrowDown;
  InitZero := src.InitZero;
  clear;
  for i := low(FFields) to high(FFields) do
    Fields[i].AssignShape(src.Fields[i]);
  TCTree(src).treeCopyStructure(self);
  TCTree(src).treeCopyData(self);
  FSorted := src.Sorted;
end;

procedure TCTree.SetCount(n: integer);
begin
  error(erOperation);
end;

function TCTree.GetCapacity: integer;
begin
  result := FCount;
end;

procedure TCTree.SetCapacity(n: integer);
begin
end;

procedure SaveAsUTF8(Const S: WideString; Dst: TStream);
var
  u: utf8string;
begin
  u := utf8encode(s);
  dst.Write(u[1], length(u)*sizeof(u[1]));
end;

procedure SaveAsWide(Const S: WideString; Dst: TStream);
begin
  dst.Write(s[1], length(s)*sizeof(s[1]));
end;

procedure SaveAsAnsi(Const S: AnsiString; Dst: TStream);
begin
  dst.Write(s[1], length(s)*sizeof(s[1]));
end;

procedure TCTree.SaveToStream(dst: TStream; FileFormat : TFileFormat = ffUTF8);
begin
  assert(length(FFields)>0, erOperation);
  if FileFormat = ffUTF8 then
    WriteBOM(dst);
  if FCount=0 then
    exit;
  case FileFormat of
    ffAnsi: SaveToStreamAnsi(dst);
    ffUnicode: SaveToStreamWide(dst);
    ffUTF8: SaveToStreamUTF8(dst);
    ffBinary: SaveToStreamBin(dst);
    else error(erOperation);
  end;
end;

procedure TCTree.SaveToStreamAnsi(dst: TStream);
var
  i: integer;
  h: PTreeNode;
  b: boolean;
  head, e, fs: AnsiString;
begin
  e := AnsiString(EOL);
  fs := AnsiString(FieldSeparator);
  head := '';
  b := false;
  h := PTreeNode(FirstRoot);
  repeat

    // end of line
    if b and (e<>'') then
      SaveAsAnsi(e, dst);
    b := true;

    // head (tree level)
    if head<>'' then
      SaveAsAnsi(Head, dst);

    // fields
    for i := 0 to high(FFields) do
    begin
      if (i>0) and (fs<>'') then
        SaveAsAnsi(fs, dst);
      FFields[i].SaveItem(TCHandle(h), dst, ffAnsi);
    end;

    // next item/level
    head := AnsiString(StringOfChar(' ', treeGetNextItemLevel(h, length(head)) ));
  until h=@FRoot;
end;

procedure TCTree.SaveToStreamWide(dst: TStream);
var
  i: integer;
  h: PTreeNode;
  b: boolean;
  head, e, fs: WideString;
begin
  e := EOL;
  fs := FieldSeparator;
  head := '';
  b := false;
  h := PTreeNode(FirstRoot);
  repeat

    // end of line
    if b and (e<>'') then
      SaveAsWide(e, dst);
    b := true;

    // head (tree level)
    if head<>'' then
      SaveAsWide(head, dst);

    // fields
    for i := 0 to high(FFields) do
    begin
      if (i>0) and (fs<>'') then
        SaveAsWide(fs, dst);
      FFields[i].SaveItem(TCHandle(h), dst, ffUnicode);
    end;

    // next item/level
    head := StringOfChar(' ', treeGetNextItemLevel(h, length(head)) );
  until h=@FRoot;
end;

procedure TCTree.SaveToStreamUTF8(dst: TStream);
var
  i: integer;
  h: PTreeNode;
  b: boolean;
  head, e, fs: WideString;
begin
  e := EOL;
  fs := FieldSeparator;
  head := '';
  b := false;
  h := PTreeNode(FirstRoot);
  repeat

    // end of line
    if b and (e<>'') then
      SaveAsUTF8(e, dst);
    b := true;

    // head (tree level)
    if head<>'' then
      SaveAsUTF8(head, dst);

    // fields
    for i := 0 to high(FFields) do
    begin
      if (i>0) and (fs<>'') then
        SaveAsUTF8(fs, dst);
      FFields[i].SaveItem(TCHandle(h), dst, ffUTF8);
    end;

    // next item/level
    head := StringOfChar(' ', treeGetNextItemLevel(h, length(head)) );
  until h=@FRoot;
end;

procedure TCTree.SaveToStreamBin(dst: TStream);
var
  i: integer;
  l: longint;
  h: PTreeNode;
begin
  h := PTreeNode(FirstRoot);
  l := 1;
  repeat
    dst.Write(l, sizeof(l));
    for i := 0 to high(FFields) do
      FFields[i].SaveItem(TCHandle(h), dst, ffBinary);
    l := treeGetNextItemLevel(h, l);
  until h=@FRoot;
end;

function TCTree.treeGetNextItemLevel(var h: PTreeNode; level: integer):integer;
begin
  result := level;
  if h.FirstChild<>nil then
  begin
    h := h.FirstChild;
    inc(result);
  end
  else
  begin
    while (h.NextSibling=nil) do
    begin
      h := h.Parent;
      dec(result);
      if h=@FRoot then
        exit;
    end;
    h := h.NextSibling;
  end;
end;

procedure TCTree.LoadFromStream(src: TStream; FileFormat : TFileFormat = ffUTF8);
var
  SkipEmpty: boolean;
begin
  assert(length(FFields)>0, erOperation);
  Clear;
  if FileFormat = ffUTF8 then
    CheckBOM(src);
  if FSkipEmptyLines=selAuto then
    SkipEmpty := (FieldCount>1) or not FField.FIsTextField
  else
    SkipEmpty := FSkipEmptyLines=selSkip;
  case FileFormat of
    ffAnsi: LoadFromStreamAnsi(src, SkipEmpty);
    ffUnicode: LoadFromStreamWide(src, SkipEmpty);
    ffUTF8: LoadFromStreamUTF8(src, SkipEmpty);
    ffBinary: LoadFromStreamBin(src);
    else error(erOperation);
  end;
end;

// 'A'->1, ' A'1->2, ...
function FindStrLevelA(var s: AnsiString): integer;
var i: integer;
begin
  result := 1;
  for i := 1 to length(s) do
    if s[i]<>' ' then
    begin
      result := i;
      s := copy(s,i,high(integer));
      exit;
    end;
end;

function FindStrLevelW(var s: WideString): integer;
var i: integer;
begin
  result := 1;
  for i := 1 to length(s) do
    if s[i]<>' ' then
    begin
      result := i;
      s := copy(s,i,high(integer));
      exit;
    end;
end;

procedure TCTree.treeAddChildAtLevel(var Level: integer; NewLevel: integer; var node, parent: PTreeNode);
var i: integer;
begin
  assert((NewLevel-Level<=1) and (NewLevel>0) and ((node<>nil) or (NewLevel=1)));
  if NewLevel>Level then
    Parent := Node
  else
    for i := 0 to Level-NewLevel-1 do
      Parent := Parent.Parent;
  Level := NewLevel;
  Node := treeAddChild(Parent);
end;

procedure TCTree.LoadFromStreamAnsi(src: TStream; skip: boolean);
var
  size: int64;
  i,l: integer;
  p,n: PTreeNode;
  s: AnsiString;
begin
  p := @FRoot;
  n := nil;
  l := 1;
  size := src.size;
  while src.Position<Size do
  begin
    s := ReadStrA(src);
    if Skip and (trim(string(s))='') then
      continue;
    i := FindStrLevelA(s);
    FTextItem := WideString(s);
    FTextPos := 1;
    treeAddChildAtLevel(l, i, n, p);
    for i := 0 to high(FFields) do
      FFields[i].LoadItem(self, TCHandle(n), src, ffAnsi);
  end;
end;

procedure TCTree.LoadFromStreamWide(src: TStream; skip: boolean);
var
  size: int64;
  i,l: integer;
  p,n: PTreeNode;
  s: WideString;
begin
  p := @FRoot;
  n := nil;
  l := 1;
  size := src.size;
  while src.Position<Size do
  begin
    s := ReadStrW(src);
    if Skip and (trim(s)='') then
      continue;
    i := FindStrLevelW(s);
    FTextItem := s;
    FTextPos := 1;
    treeAddChildAtLevel(l, i, n, p);
    for i := 0 to high(FFields) do
      FFields[i].LoadItem(self, TCHandle(n), src, ffUnicode);
  end;
end;

procedure TCTree.LoadFromStreamUtf8(src: TStream; skip: boolean);
var
  size: int64;
  i,l: integer;
  p,n: PTreeNode;
  s: WideString;
  u: UTF8String;
begin
  p := @FRoot;
  n := nil;
  l := 1;
  size := src.size;
  while src.Position<Size do
  begin
    u := ReadStrU(src);
    {$IFDEF HiCompiler}
    s := UTF8ToWideString(u);
    {$ELSE}
    s := Utf8Decode(u);
    {$ENDIF}
    if Skip and (trim(s)='') then
      continue;
    i := FindStrLevelW(s);
    FTextItem := s;
    FTextPos := 1;
    treeAddChildAtLevel(l, i, n, p);
    for i := 0 to high(FFields) do
      FFields[i].LoadItem(self, TCHandle(n), src, ffUTF8);
  end;
end;

procedure TCTree.LoadFromStreamBin(src: TStream);
var
  size: int64;
  i,l: longint;
  p,n: PTreeNode;
begin
  p := @FRoot;
  n := nil;
  l := 1;
  size := src.size;
  while src.Position<Size do
  begin
    src.read(i, sizeof(i));
    treeAddChildAtLevel(l, i, n, p);
    for i := 0 to high(FFields) do
      FFields[i].LoadItem(self, TCHandle(n), src, ffBinary);
  end;
end;

function TCTree.Get(handle: TCHandle): Pointer;
begin
  assert((handle<>-1) and (pointer(handle)<>nil) and (pointer(handle)<>@FRoot));
  result := @PTreeNode(handle).data;
end;

procedure TCTree.Put(handle: TCHandle; var Item);
begin
  assert((handle<>-1) and (pointer(handle)<>nil));
  FMoveField(item, PTreeNode(handle).data, FFieldSize);
end;

function TCTree.AllocNewItem: PTreeNode;
begin
  Result := AllocMem(sizeof(TTreeNode) + FItemSize);
end;

procedure TCTree.ReleaseItem(p: PTreeNode);
begin
  freemem(p);
end;

function TCTree.AddItem(var item): TCHandle;
begin
  result := TCHandle(AllocNewItem);
  with PTreeNode(result)^ do
  begin
    Parent := @FRoot;
    FMoveField(item, data, FFieldSize);
  end;
  if FSorted then
    DListAddSorted(@PTreeNode(result).ListItem, @FRoot.Childs, IntCmp)
  else
    DListAdd(@PTreeNode(result).ListItem, @FRoot.Childs);
  Inc(FCount);
  Inc(FRoot.TotalChildCount);
  Inc(FRoot.ChildCount);
end;

function TCTree.AddChildItem(AParent: TCHandle; var Item): TCHandle;
var v: PTreeNode;
begin
  assert(pointer(AParent)<>nil);
  if AParent=-1 then
    AParent := TCHandle(@FRoot);
  v := AllocNewItem;
  result := TCHandle(v);
  with v^ do
  begin
    Parent := pointer(AParent);
    FMoveField(item, data, FFieldSize);
  end;
  if FSorted then
    DListAddSorted(@v.ListItem, @PTreeNode(AParent).Childs, IntCmp)
  else
    DListAdd(@v.ListItem, @PTreeNode(AParent).Childs);
  Inc(FCount);
  Inc(PTreeNode(AParent).ChildCount);
  repeat
    v := v.Parent;
    Inc(v.TotalChildCount);
  until v=@FRoot;
end;

function TCTree.AddRootItem(var Item): TCHandle;
begin
  result := AddChildItem(TCHandle(@FRoot), Item);
end;

procedure TCTree.ChangeParent(Node, ANewParent: TCHandle);
var
  p: PTreeNode;
  c: integer;
begin
  assert((pointer(Node)<>nil) and (Node<>-1) and (PTreeNode(Node)<>@FRoot));
  assert((pointer(ANewParent)<>nil) and (ANewParent<>-1));
  assert(not FSorted);

  // move item to new list of childs
  p := PTreeNode(Node).Parent;
  DListRemove(pointer(Node), @p.Childs);
  DListAdd(pointer(Node), @PTreeNode(ANewParent).Childs);

  // correct .Parent
  PTreeNode(Node).Parent := PTreeNode(ANewParent);

  // correct .ChildCount/.TotalChildCount
  if p=pointer(ANewParent) then
    exit;

  // for old parent
  c := PTreeNode(Node).TotalChildCount+1;
  dec(p.ChildCount);
  dec(p.TotalChildCount, c);
  while p<>@FRoot do
  begin
    p := p.Parent;
    dec(p.TotalChildCount, c);
  end;

  // for new parent
  p := PTreeNode(ANewParent);
  inc(p.ChildCount);
  inc(p.TotalChildCount, c);
  while p<>@FRoot do
  begin
    p := p.Parent;
    inc(p.TotalChildCount, c);
  end;
end;

procedure TCTree.Delete(handle: TCHandle);
var
  p,n: PTreeNode;
  i: integer;
begin
  assert((handle<>-1) and (pointer(handle)<>nil) and (pointer(handle)<>@FRoot));

  // remove item with childs form parent's list
  n := pointer(handle);
  p := n.Parent;
  DListRemove(@n.ListItem, @p.Childs);
  n.PrevSibling := nil;
  n.NextSibling := nil;

  // correct .TotalCount for all parents and .FCount
  dec(p.ChildCount);
  i := n.TotalChildCount+1;
  repeat
    dec(p.TotalChildCount, i);
    p := p.Parent;
  until p=nil;
  dec(FCount, i);

  // relese item with all childs
  treeToList(n);
  repeat
    p := n;
    n := n.NextSibling;
    if FDelCount>0 then
      ItemDeleted(TCHandle(p));
    ReleaseItem(p);
  until n=nil;
end;

function TCTree.Insert(handle: TCHandle): TCHandle;
var v: PTreeNode;
begin
  assert((handle<>-1) and (pointer(handle)<>nil) and (pointer(handle)<>@FRoot));
  v := AllocNewItem;
  v.Parent := PTreeNode(handle).Parent;
  DListInsertBefore(pointer(v), pointer(handle), @v.parent.childs);
  result := TCHandle(v);
  Inc(FCount);
  Inc(v.Parent.ChildCount);
  repeat
    v := v.Parent;
    inc(v.TotalChildCount);
  until v=@FRoot;
end;

function TCTree.InsertAfter(handle: TCHandle): TCHandle;
var v: PTreeNode;
begin
  assert((handle<>-1) and (pointer(handle)<>nil) and (pointer(handle)<>@FRoot));
  v := AllocNewItem;
  v.Parent := PTreeNode(handle).Parent;
  DListInsertAfter(pointer(v), pointer(handle), @v.parent.childs);
  result := TCHandle(v);
  Inc(FCount);
  Inc(v.Parent.ChildCount);
  repeat
    v := v.Parent;
    inc(v.TotalChildCount);
  until v=@FRoot;
end;

// convert tree to list
procedure TCTree.treeToList(node: PTreeNode);
begin
  assert((node<>nil) and (TCHandle(node)<>-1));
  repeat
    if node.FirstChild<>nil then
    begin
      node.FirstChild.PrevSibling := node;
      node.LastChild.NextSibling := node.NextSibling;
      if node.NextSibling<>nil then
        node.NextSibling.PrevSibling := node.LastChild;
      node.NextSibling := node.FirstChild;
    end;
    node.TotalChildCount := 0;
    node.ChildCount := 0;
    node.FirstChild := nil;
    node.LastChild := nil;
    node := node.NextSibling;
  until node=nil;
end;

procedure TCTree.Clear;
var p,d: PTreeNode;
begin
  treeToList(@FRoot);
  p := FRoot.NextSibling;
  while p<>nil do
  begin
    d := p;
    p := p.NextSibling;
    if FDelCount>0 then
      ItemDeleted(TCHandle(d));
    ReleaseItem(d);
  end;
  FCount := 0;
  fillchar(FRoot, sizeof(FRoot), 0);
end;

procedure TCTree.Swap(handle1, handle2: TCHandle);
begin
  assert((pointer(handle1)<>nil) and (handle1<>-1) and (PTreeNode(handle1)<>@FRoot));
  assert((pointer(handle2)<>nil) and (handle2<>-1) and (PTreeNode(handle2)<>@FRoot));
  assert(not FSorted);
  FSwapItem(PTreeNode(handle1).Data, PTreeNode(handle2).Data, FTemp^, FItemSize);
end;

procedure TCTree.Exchange(handle1, handle2: TCHandle);
var
  c: integer;
  p: PTreeNode;
begin
  assert((pointer(handle1)<>nil) and (handle1<>-1) and (PTreeNode(handle1)<>@FRoot));
  assert((pointer(handle2)<>nil) and (handle2<>-1) and (PTreeNode(handle2)<>@FRoot));
  assert(not FSorted);
  DListExchange(pointer(handle1), pointer(handle2),
    @PTreeNode(handle1).parent.childs, @PTreeNode(handle2).parent.childs);

  if PTreeNode(handle1).Parent=PTreeNode(handle2).Parent then
    exit;

  // correct .TotalChildCount
  // .ChildCount does't change
  c := PTreeNode(handle2).TotalChildCount-PTreeNode(handle1).TotalChildCount;
  p := PTreeNode(handle1).Parent;
  repeat
    inc(p.TotalChildCount, c);
    p := p.parent;
  until p=@FRoot;
  c := PTreeNode(handle1).TotalChildCount-PTreeNode(handle2).TotalChildCount;
  p := PTreeNode(handle2).Parent;
  repeat
    inc(p.TotalChildCount, c);
    p := p.parent;
  until p=@FRoot;

  // correct .parent
  exch(pointer(PTreeNode(handle1).parent), pointer(PTreeNode(handle2).parent));
end;

procedure TCTree.Move(CurHandle, NewHandle: TCHandle);
var
  p: PTreeNode;
  c: integer;
begin
  assert((pointer(CurHandle)<>nil) and (CurHandle<>-1) and (PTreeNode(CurHandle)<>@FRoot));
  assert((pointer(NewHandle)<>nil) and (NewHandle<>-1) and (PTreeNode(NewHandle)<>@FRoot));
  assert(not FSorted);
  DListMove(pointer(CurHandle), pointer(NewHandle),
    @PTreeNode(CurHandle).Parent.Childs, @PTreeNode(NewHandle).Parent.Childs);

  if PTreeNode(CurHandle).Parent=PTreeNode(NewHandle).Parent then
    exit;

  // correct .TotalChildCount
  p := PTreeNode(CurHandle);
  dec(p.Parent.ChildCount);
  c := p.TotalChildCount+1;
  repeat
    p := p.Parent;
    dec(p.TotalChildCount, c);
  until p=@FRoot;
  p := PTreeNode(NewHandle);
  inc(p.Parent.ChildCount);
  repeat
    p := p.Parent;
    inc(p.TotalChildCount, c);
  until p=@FRoot;

  // correct .Parent
  PTreeNode(CurHandle).Parent := PTreeNode(NewHandle).Parent;
end;

procedure TCTree.Copy(Src, Dst: TCHandle);
begin
  assert((not FSorted) and (src<>-1) and (dst<>-1) and (pointer(src)<>nil) and (pointer(dst)<>nil));
  if FDelCount>0 then
    ItemDeleted(dst);
  if FCopyCount > 0 then
    CopyItem(src, dst)
  else
    FMoveItem(PTreeNode(src).Data, PTreeNode(dst).Data, FItemSize);
end;

procedure TCTree.Grow;
begin
end;

{
  Example of nodes numeration (C=TotalChildCount):
  FRoot (C=10)
    |_0 (C=7)
    | |_1 (C=0)
    | |_2 (C=0)
    | |_3 (C=2)
    | | |_4 (C=0)
    | | |_5 (C=0)
    | |_6 (C=0)
    | |_7 (C=0)
    |_8 (C=1)
      |_9 (C=0)
}
function TCTree.GetHandle(index: integer): TCHandle;
var
  f: integer;
  n: PTreeNode;
begin
  assert((index>=0) and (index<FCount));
  n := @FRoot;
  f := -1;
  repeat
    if f=index then
    begin
      result := TCHandle(n);
      exit;
    end;

    // find child with range where Index is
    inc(f);
    n := n.FirstChild;
    repeat
      if f+n.TotalChildCount>=index then
        break;
      inc(f, n.TotalChildCount+1);
      n := n.NextSibling;
    until false;
  until false;
end;

function TCTree.GetIndex(handle: TCHandle): integer;
var n: PTreeNode;
begin
  assert((handle<>-1) and (pointer(handle)<>nil) and (pointer(handle)<>@FRoot));
  result := -1;
  n := pointer(handle);
  while n<>@FRoot do
  begin
    while n.PrevSibling<>nil do
    begin
      n := n.PrevSibling;
      inc(result, n.TotalChildCount+1);
    end;
    n := n.Parent;
    inc(result);
  end;
end;

function TCTree.First: TCHandle;
begin
  result := TCHandle(FRoot.FirstChild);
  if PTreeNode(result)=nil then
    result := -1;
end;

function TCTree.Last: TCHandle;
begin
  result := TCHandle(FRoot.LastChild);
  if PTreeNode(result)=nil then
    result := -1
  else
    while PTreeNode(result).LastChild<>nil do
      result := TCHandle(PTreeNode(result).LastChild);
end;

procedure TCTree.Next(var n: TCHandle);
var p: PTreeNode;
begin
  assert((pointer(n)<>nil) and (pointer(n)<>@FRoot));
  if n=-1 then
    exit;
  p := PTreeNode(n);
  n := -1;
  if p.FirstChild<>nil then
    n := TCHandle(p.FirstChild)
  else
  begin
    while (p.NextSibling=nil) do
    begin
      p := p.Parent;
      if p=@FRoot then
        exit;
    end;
    n := TCHandle(p.NextSibling);
  end;
end;

procedure TCTree.Prev(var n: TCHandle);
var p: PTreeNode;
begin
  assert((pointer(n)<>nil) and (pointer(n)<>@FRoot));
  if n=-1 then
    exit;
  p := PTreeNode(n);
  n := -1;
  if p.PrevSibling=nil then
    if p.Parent=@FRoot then
      exit
    else
      n := TCHandle(p.Parent)
  else
  begin
    p := p.PrevSibling;
    while p.LastChild<>nil do
      p := p.LastChild;
    n := TCHandle(p);
  end;
end;

function TCTree.IntCmp(a, b: P_SDListItem): integer;
begin
  result := FCompareValue(PTreeNode(a).data, PTreeNode(b).data);
end;

// sort range of container items by default Field with default comparator
// (can be optimized by internal implementation of swap/compare)
procedure TCTree.Sort(AFirst, ALast: TCHandle);
begin
  if FCount<=0 then
    exit;
  assert((AFirst<>-1) and (pointer(AFirst)<>nil) and (ALast<>-1) and
    (pointer(ALast)<>nil) and (PTreeNode(AFirst).Parent=PTreeNode(ALast).Parent));
  DListSort(@PTreeNode(AFirst).Parent.Childs, @PTreeNode(AFirst).ListItem,
    @PTreeNode(ALast).ListItem, IntCmp);
end;

// sort range of container items by default Field with user-defined by-value comparator
procedure TCTree.Sort(compare: TOnCompareValues; AFirst, ALast: TCHandle);
var p: TOnCompareValues;
begin
  if FCount<=0 then
    exit;
  assert((AFirst<>-1) and (pointer(AFirst)<>nil) and (ALast<>-1) and
    (pointer(ALast)<>nil) and (PTreeNode(AFirst).Parent=PTreeNode(ALast).Parent));
  p := FCompareValue;
  try
    FCompareValue := compare;
    DListSort(@PTreeNode(AFirst).Parent.Childs, @PTreeNode(AFirst).ListItem,
      @PTreeNode(ALast).ListItem, IntCmp);
  finally
    FCompareValue := p;
  end;
end;

function TCTree.ExtCmp(a, b: P_SDListItem): integer;
begin
  result := FSortCompare(FSortField, TCHandle(a),TCHandle(b));
end;

// sort range of container items by specified Field with user-defined by-handle comparator method
procedure TCTree.Sort(field: TTCustom; compare: TOnCompare; AFirst, ALast: TCHandle);
begin
  if FCount<=0 then
    exit;
  assert((AFirst<>-1) and (pointer(AFirst)<>nil) and (ALast<>-1) and
    (pointer(ALast)<>nil) and (PTreeNode(AFirst).Parent=PTreeNode(ALast).Parent));
  FSortField := field;
  FSortCompare := compare;
  DListSort(@PTreeNode(AFirst).Parent.Childs, @PTreeNode(AFirst).ListItem,
    @PTreeNode(ALast).ListItem, ExtCmp);
end;

function TCTree.ExtCmpProc(a, b: P_SDListItem): integer;
begin
  result := FSortCompareProc(FSortField, TCHandle(a),TCHandle(b));
end;

// sort range of container items by specified Field with user-defined by-handle comparator procedure
procedure TCTree.Sort(field: TTCustom; compare: TOnCompareProc; AFirst, ALast: TCHandle);
begin
  if Count<=0 then
    exit;
  FSortField := field;
  FSortCompareProc := compare;
  DListSort(@PTreeNode(AFirst).Parent.Childs, @PTreeNode(AFirst).ListItem,
    @PTreeNode(ALast).ListItem, ExtCmpProc);
end;

function TCTree.InsertItem(handle: TCHandle; var item): TCHandle;
var v: PTreeNode;
begin
  assert((handle<>-1) and (pointer(handle)<>nil) and (pointer(handle)<>@FRoot));
  v := AllocNewItem;
  v.Parent := PTreeNode(handle).Parent;
  DListInsertBefore(pointer(v), pointer(handle), @v.parent.childs);
  FMoveField(item, v.data, FFieldSize);
  result := TCHandle(v);
  Inc(FCount);
  Inc(v.Parent.ChildCount);
  repeat
    v := v.Parent;
    inc(v.TotalChildCount);
  until v=@FRoot;
end;

function TCTree.InsertAfterItem(handle: TCHandle; var item): TCHandle;
var v: PTreeNode;
begin
  assert((handle<>-1) and (pointer(handle)<>nil) and (pointer(handle)<>@FRoot));
  assert(not FSorted);
  v := AllocNewItem;
  v.Parent := PTreeNode(handle).Parent;
  DListInsertAfter(pointer(v), pointer(handle), @v.parent.childs);
  FMoveField(item, v.data, FFieldSize);
  result := TCHandle(v);
  Inc(FCount);
  Inc(v.Parent.ChildCount);
  repeat
    v := v.Parent;
    inc(v.TotalChildCount);
  until v=@FRoot;
end;

procedure TCTree.Reverse(AFirst, ALast: TCHandle);
begin
  assert((AFirst<>-1) and (pointer(AFirst)<>nil) and (pointer(AFirst)<>@FRoot));
  assert((ALast<>-1) and (pointer(ALast)<>nil) and (pointer(ALast)<>@FRoot));
  assert(PTreeNode(AFirst).Parent=PTreeNode(ALast).Parent);
  assert(not FSorted);
  DListReverse(pointer(AFirst), pointer(ALast), @PTreeNode(AFirst).Parent.Childs);
end;

procedure TCTree.Rotate(AFirst, ALast: TCHandle; shift: integer);
begin
  assert((AFirst<>-1) and (pointer(AFirst)<>nil) and (pointer(AFirst)<>@FRoot));
  assert((ALast<>-1) and (pointer(ALast)<>nil) and (pointer(ALast)<>@FRoot));
  assert(PTreeNode(AFirst).Parent=PTreeNode(ALast).Parent);
  assert(not FSorted);
  DListRotate(pointer(AFirst), pointer(ALast), shift, @PTreeNode(AFirst).Parent.Childs);
end;

procedure TCTree.RandomShuffle(AFirst, ALast: TCHandle);
begin
  assert((AFirst<>-1) and (pointer(AFirst)<>nil) and (pointer(AFirst)<>@FRoot));
  assert((ALast<>-1) and (pointer(ALast)<>nil) and (pointer(ALast)<>@FRoot));
  assert(PTreeNode(AFirst).Parent=PTreeNode(ALast).Parent);
  assert(not FSorted);
  DListRandomShuffle(pointer(AFirst), pointer(ALast), @PTreeNode(AFirst).Parent.Childs);
end;

function TCTree.GetRootCount: integer;
begin
  result := FRoot.ChildCount;
end;

function TCTree.GetFirstRoot: TCHandle;
begin
  result := TCHandle(FRoot.FirstChild);
  if pointer(result)=nil then
    result := -1;
end;

function TCTree.GetLastRoot: TCHandle;
begin
  result := TCHandle(FRoot.LastChild);
  if pointer(result)=nil then
    result := -1;
end;

function TCTree.GetParent(node: TCHandle): TCHandle;
begin
  assert((pointer(node)<>nil) and (pointer(node)<>@FRoot));
  result := TCHandle(PTreeNode(node).Parent);
  if pointer(result)=@FRoot then
    result := -1;
end;

function TCTree.GetChildCount(node: TCHandle): integer;
begin
  result := PTreeNode(node).ChildCount;
end;

function TCTree.GetFirstChild(node: TCHandle): TCHandle;
begin
  assert((pointer(node)<>nil) and (pointer(node)<>@FRoot));
  result := TCHandle(PTreeNode(node).FirstChild);
  if pointer(result)=nil then
    result := -1;
end;

function TCTree.GetLastChild(node: TCHandle): TCHandle;
begin
  assert((pointer(node)<>nil) and (pointer(node)<>@FRoot));
  result := TCHandle(PTreeNode(node).LastChild);
  if pointer(result)=nil then
    result := -1;
end;

function TCTree.GetPrevSibling(node: TCHandle): TCHandle;
begin
  assert((pointer(node)<>nil) and (pointer(node)<>@FRoot));
  result := TCHandle(PTreeNode(node).PrevSibling);
  if pointer(result)=nil then
    result := -1;
end;

function TCTree.GetNextSibling(node: TCHandle): TCHandle;
begin
  assert((pointer(node)<>nil) and (pointer(node)<>@FRoot));
  result := TCHandle(PTreeNode(node).NextSibling);
  if pointer(result)=nil then
    result := -1;
end;

procedure VectorLongwordReverse(Data: PLongwordList; Count: integer);
var
  i: integer;
  j: TLongwordType;
  src,dst: ^TLongwordType;
begin
  src := pointer(Data);
  dst := @Data[Count-1];
  for i := 0 to Count shr 1-1 do
  begin
    j := src^;
    src^ := dst^;
    dst^ := j;
    inc(src);
    dec(dst);
  end;
end;

procedure VectorLongwordRotate(Data: PLongwordList; Count, Shift: integer);
begin
  Shift := -Shift mod Count;
  if Shift<0 then
    inc(Shift, Count);
  if Shift=0 then
    exit;
  VectorLongwordReverse(Data, Shift);
  VectorLongwordReverse(@Data[Shift], Count-Shift);
  VectorLongwordReverse(Data, Count);
end;

procedure VectorSmallintReverse(Data: PSmallintList; Count: integer);
var
  i: integer;
  j: TSmallintType;
  src,dst: ^TSmallintType;
begin
  src := pointer(Data);
  dst := @Data[Count-1];
  for i := 0 to Count shr 1-1 do
  begin
    j := src^;
    src^ := dst^;
    dst^ := j;
    inc(src);
    dec(dst);
  end;
end;

procedure VectorSmallintRotate(Data: PSmallintList; Count, Shift: integer);
begin
  Shift := -Shift mod Count;
  if Shift<0 then
    inc(Shift, Count);
  if Shift=0 then
    exit;
  VectorSmallintReverse(Data, Shift);
  VectorSmallintReverse(@Data[Shift], Count-Shift);
  VectorSmallintReverse(Data, Count);
end;

procedure VectorWordReverse(Data: PWordList; Count: integer);
begin
  VectorSmallintReverse(pointer(Data), Count);
end;

procedure VectorWordRotate(Data: PWordList; Count, Shift: integer);
begin
  VectorSmallintRotate(pointer(Data), Count, Shift);
end;

procedure VectorByteReverse(Data: PByteList; Count: integer);
var
  i: integer;
  j: TByteType;
  src,dst: ^TByteType;
begin
  src := pointer(Data);
  dst := @Data[Count-1];
  for i := 0 to Count shr 1-1 do
  begin
    j := src^;
    src^ := dst^;
    dst^ := j;
    inc(src);
    dec(dst);
  end;
end;

procedure VectorByteRotate(Data: PByteList; Count, Shift: integer);
begin
  Shift := -Shift mod Count;
  if Shift<0 then
    inc(Shift, Count);
  if Shift=0 then
    exit;
  VectorByteReverse(Data, Shift);
  VectorByteReverse(@Data[Shift], Count-Shift);
  VectorByteReverse(Data, Count);
end;

procedure VectorShortintReverse(Data: PShortintList; Count: integer);
begin
  VectorByteReverse(pointer(Data), Count);
end;

procedure VectorShortintRotate(Data: PShortintList; Count, Shift: integer);
begin
  VectorByteRotate(pointer(Data), Count, Shift);
end;

procedure VectorByteQuickSort(Data: PByteList; Count: integer);
var
  I, J: ^TByteType;
  P, T: TByteType;
begin
  if Count<=1 then
    exit;
  dec(Count); // Count -> right bound
  if Count<=75 then
  begin
    I := @Data[1];
    for T := 0 to Count-1 do
    begin
      P := I^;
      J := I;
      dec(J);
      while (Longword(J)>=Longword(data)) and (J^>P) do
      begin
        PByteList(J)[1] := J^;
        dec(J);
      end;
      PByteList(J)[1] := P;
      inc(I);
    end;
  end
  else
    repeat
      I := pointer(data);
      J := @data[Count];
      P := data[Count shr 1];
      repeat
        while I^<P do
          Inc(I);
        while J^>P do
          Dec(J);
        if Longword(I) <= Longword(J) then
        begin
          T := I^;
          I^ := J^;
          J^ := T;
          Inc(I);
          Dec(J);
        end;
      until Longword(I) > Longword(J);
      if Longword(J)>Longword(data) then
        VectorByteQuickSort(data, (Longword(J)-Longword(data)) div sizeof(TByteType)+1);
      dec(Count, (Longword(I)-Longword(data)) div sizeof(TByteType));
      data := pointer(I);
    until Count=0;
end;

procedure VectorByteCountSort(Data: PByteList; Count: integer);
var
  c: array[TByteType] of integer;
  i,j: integer;
begin
  if Count<=64 then
  begin
    VectorByteQuickSort(Data, Count);
    exit;
  end;
  Fillchar(c, sizeof(c), 0);
  for i := 0 to Count-1 do
    inc(c[Data[i]]);
  j := 0;
  for i := 0 to 255 do
    if c[i]>0 then
    begin
      FillChar(Data[j], c[i], i);
      inc(j, c[i]);
    end;
end;

procedure VectorShortintQuickSort(Data: PShortintList; Count: integer);
var
  I, J: ^TShortintType;
  P, T: TShortintType;
begin
  if Count<=1 then
    exit;
  dec(Count); // Count -> right bound
  if Count<=75 then
  begin
    I := @Data[1];
    for T := 0 to Count-1 do
    begin
      P := I^;
      J := I;
      dec(J);
      while (Longword(J)>=Longword(data)) and (J^>P) do
      begin
        PShortintList(J)[1] := J^;
        dec(J);
      end;
      PShortintList(J)[1] := P;
      inc(I);
    end;
  end
  else
    repeat
      I := pointer(data);
      J := @data[Count];
      P := data[Count shr 1];
      repeat
        while I^<P do
          Inc(I);
        while J^>P do
          Dec(J);
        if Longword(I) <= Longword(J) then
        begin
          T := I^;
          I^ := J^;
          J^ := T;
          Inc(I);
          Dec(J);
        end;
      until Longword(I) > Longword(J);
      if Longword(J)>Longword(data) then
        VectorShortintQuickSort(data, (Longword(J)-Longword(data)) div sizeof(TShortintType)+1);
      dec(Count, (Longword(I)-Longword(data)) div sizeof(TShortintType));
      data := pointer(I);
    until Count=0;
end;

procedure VectorShortintCountSort(Data: PShortintList; Count: integer);
var
  L,R,M: integer;
begin
  if Count<=0 then
    exit;
  VectorByteCountSort(pointer(data), Count);
  L := 0;
  R := Count-1;
  if Data[R]>=0 then
    exit;
  while R-L>1 do
  begin
    M := (R+L) shr 1;
    if Data[M]<0 then
      R := M
    else
      L := M;
  end;
  if Data[L]<0 then
    M := L
  else
    M := R;
  VectorShortintRotate(Data, Count, -M);
end;

procedure VectorLongintQuickSort(Data: PLongintList; Count: integer);
var
  I, J: ^TLongintType;
  P, T: TLongintType;
begin
  if Count<=1 then
    exit;
  dec(Count); // Count -> right bound
  if Count<=75 then
  begin
    I := @Data[1];
    for T := 0 to Count-1 do
    begin
      P := I^;
      J := I;
      dec(J);
      while (Longword(J)>=Longword(data)) and (J^>P) do
      begin
        PLongintList(J)[1] := J^;
        dec(J);
      end;
      PLongintList(J)[1] := P;
      inc(I);
    end;
  end
  else
    repeat
      I := pointer(data);
      J := @data[Count];
      P := data[Count shr 1];
      repeat
        while I^<P do
          Inc(I);
        while J^>P do
          Dec(J);
        if Longword(I) <= Longword(J) then
        begin
          T := I^;
          I^ := J^;
          J^ := T;
          Inc(I);
          Dec(J);
        end;
      until Longword(I) > Longword(J);
      if Longword(J)>Longword(data) then
        VectorLongintQuickSort(data, (Longword(J)-Longword(data)) div sizeof(TLongintType)+1);
      dec(Count, (Longword(I)-Longword(data)) div sizeof(TLongintType));
      data := pointer(I);
    until Count=0;
end;

procedure VectorLongintReverse(Data: PLongintList; Count: integer);
var
  i: integer;
  j: TLongintType;
  src,dst: ^TLongintType;
begin
  src := pointer(Data);
  dst := @Data[Count-1];
  for i := 0 to Count shr 1-1 do
  begin
    j := src^;
    src^ := dst^;
    dst^ := j;
    inc(src);
    dec(dst);
  end;
end;

procedure VectorLongintRotate(Data: PLongintList; Count, Shift: integer);
begin
  Shift := -Shift mod Count;
  if Shift<0 then
    inc(Shift, Count);
  if Shift=0 then
    exit;
  VectorLongintReverse(Data, Shift);
  VectorLongintReverse(@Data[Shift], Count-Shift);
  VectorLongintReverse(Data, Count);
end;

// JumpSort algorythm (developed by Andrei Galatyn)
// 1-byte-for-step implementation for Longint type
procedure VectorLongintJumpSort(Data: PLongintList; Count: integer);
var
  L,R,M: integer;
begin
  if Count<=0 then
    exit;
  VectorLongwordJumpSort(pointer(data), Count);
  L := 0;
  R := Count-1;
  if Data[R]>=0 then
    exit;
  while R-L>1 do
  begin
    M := (R+L) shr 1;
    if Data[M]<0 then
      R := M
    else
      L := M;
  end;
  if Data[L]<0 then
    M := L
  else
    M := R;
  VectorLongintRotate(Data, Count, -M);
end;

procedure VectorIntegerReverse(Data: PIntegerList; Count: integer);
var
  i: integer;
  j: TIntegerType;
  src,dst: ^TIntegerType;
begin
  src := pointer(Data);
  dst := @Data[Count-1];
  for i := 0 to Count shr 1-1 do
  begin
    j := src^;
    src^ := dst^;
    dst^ := j;
    inc(src);
    dec(dst);
  end;
end;

procedure VectorIntegerRotate(Data: PIntegerList; Count, Shift: integer);
begin
  Shift := -Shift mod Count;
  if Shift<0 then
    inc(Shift, Count);
  if Shift=0 then
    exit;
  VectorIntegerReverse(Data, Shift);
  VectorIntegerReverse(@Data[Shift], Count-Shift);
  VectorIntegerReverse(Data, Count);
end;

procedure VectorLongwordQuickSort(Data: PLongwordList; Count: integer);
var
  I, J: ^TLongwordType;
  P, T: TLongwordType;
begin
  if Count<=1 then
    exit;
  dec(Count); // Count -> right bound
  if Count<=75 then
  begin
    I := @Data[1];
    for T := 0 to Count-1 do
    begin
      P := I^;
      J := I;
      dec(J);
      while (Longword(J)>=Longword(data)) and (J^>P) do
      begin
        PLongwordList(J)[1] := J^;
        dec(J);
      end;
      PLongwordList(J)[1] := P;
      inc(I);
    end;
  end
  else
    repeat
      I := pointer(data);
      J := @data[Count];
      P := data[Count shr 1];
      repeat
        while I^<P do
          Inc(I);
        while J^>P do
          Dec(J);
        if Longword(I) <= Longword(J) then
        begin
          T := I^;
          I^ := J^;
          J^ := T;
          Inc(I);
          Dec(J);
        end;
      until Longword(I) > Longword(J);
      if Longword(J)>Longword(data) then
        VectorLongwordQuickSort(data, (Longword(J)-Longword(data)) div sizeof(TLongwordType)+1);
      dec(Count, (Longword(I)-Longword(data)) div sizeof(TLongwordType));
      data := pointer(I);
    until Count=0;
end;

procedure VectorWordQuickSort(Data: PWordList; Count: integer);
var
  I, J: ^TWordType;
  P, T: TWordType;
begin
  if Count<=1 then
    exit;
  dec(Count); // Count -> right bound
  if Count<=75 then
  begin
    I := @Data[1];
    for T := 0 to Count-1 do
    begin
      P := I^;
      J := I;
      dec(J);
      while (Longword(J)>=Longword(data)) and (J^>P) do
      begin
        PWordList(J)[1] := J^;
        dec(J);
      end;
      PWordList(J)[1] := P;
      inc(I);
    end;
  end
  else
    repeat
      I := pointer(data);
      J := @data[Count];
      P := data[Count shr 1];
      repeat
        while I^<P do
          Inc(I);
        while J^>P do
          Dec(J);
        if Longword(I) <= Longword(J) then
        begin
          T := I^;
          I^ := J^;
          J^ := T;
          Inc(I);
          Dec(J);
        end;
      until Longword(I) > Longword(J);
      if Longword(J)>Longword(data) then
        VectorWordQuickSort(data, (Longword(J)-Longword(data)) div sizeof(TWordType)+1);
      dec(Count, (Longword(I)-Longword(data)) div sizeof(TWordType));
      data := pointer(I);
    until Count=0;
end;

procedure VectorIntegerQuickSort(Data: PIntegerList; Count: integer);
var
  I, J: ^TIntegerType;
  P, T: TIntegerType;
begin
  if Count<=1 then
    exit;
  dec(Count); // Count -> right bound
  if Count<=75 then
  begin
    I := @Data[1];
    for T := 0 to Count-1 do
    begin
      P := I^;
      J := I;
      dec(J);
      while (Longword(J)>=Longword(data)) and (J^>P) do
      begin
        PIntegerList(J)[1] := J^;
        dec(J);
      end;
      PIntegerList(J)[1] := P;
      inc(I);
    end;
  end
  else
    repeat
      I := pointer(data);
      J := @data[Count];
      P := data[Count shr 1];
      repeat
        while I^<P do
          Inc(I);
        while J^>P do
          Dec(J);
        if Longword(I) <= Longword(J) then
        begin
          T := I^;
          I^ := J^;
          J^ := T;
          Inc(I);
          Dec(J);
        end;
      until Longword(I) > Longword(J);
      if Longword(J)>Longword(data) then
        VectorIntegerQuickSort(data, (Longword(J)-Longword(data)) div sizeof(TIntegerType)+1);
      dec(Count, (Longword(I)-Longword(data)) div sizeof(TIntegerType));
      data := pointer(I);
    until Count=0;
end;

procedure VectorLongwordJumpSort4(data: PLongwordList; Count: integer);
var
  L,R: array[byte] of integer;
  I,Pos: integer;
  Value,Part,NewValue: TLongwordType;
begin
  fillchar(L, sizeof(L), 0);
  for I := 0 to Count-1 do
    inc(L[data[I] and $FF]);
  Pos := 0;
  for I := low(L) to high(L) do
  begin
    Value := Pos+L[I];
    L[I] := Pos;
    Pos := Value;
  end;
  R := L;
  Pos := 0;
  while Pos<Count do
  begin
    Value := data[Pos];
    Part := Value and $FF;
    if (Pos>=L[Part]) and (Pos<R[Part]) then
    begin
      Pos := R[Part];
      continue;
    end;
    I := Pos;
    repeat
      Pos := R[Part];
      inc(R[Part]);
      if Pos=I then
      begin
        data[Pos] := value;
        inc(Pos);
        break;
      end;
      NewValue := data[Pos];
      data[Pos] := Value;
      Value := NewValue;
      Part := Value and $FF;
    until false;
  end;
end;

procedure VectorLongwordJumpSort3(data: PLongwordList; Count: integer);
var
  L,R: array[byte] of integer;
  I,Pos: integer;
  Value,Part,NewValue: TLongwordType;
begin
  fillchar(L, sizeof(L), 0);
  for I := 0 to Count-1 do
    inc(L[(data[I] shr 8) and $FF]);
  Pos := 0;
  for I := low(L) to high(L) do
  begin
    Value := Pos+L[I];
    L[I] := Pos;
    Pos := Value;
  end;
  R := L;
  Pos := 0;
  while Pos<Count do
  begin
    Value := data[Pos];
    Part := (Value shr 8) and $FF;
    if (Pos>=L[Part]) and (Pos<R[Part]) then
    begin
      Pos := R[Part];
      continue;
    end;
    I := Pos;
    repeat
      Pos := R[Part];
      inc(R[Part]);
      if Pos=I then
      begin
        data[Pos] := value;
        inc(Pos);
        break;
      end;
      NewValue := data[Pos];
      data[Pos] := Value;
      Value := NewValue;
      Part := (Value shr 8) and $FF;
    until false;
  end;
  for I := low(L) to high(L) do
    if R[I]-L[I]>1 then
      if R[I]-L[I]>64 then
        VectorLongwordJumpSort4(@data[L[I]], R[I]-L[I])
      else
        VectorLongwordQuickSort(@data[L[I]], R[I]-L[I]);
end;

procedure VectorLongwordJumpSort2(data: PLongwordList; Count: integer);
var
  L,R: array[byte] of integer;
  I,Pos: integer;
  Value,Part,NewValue: TLongwordType;
begin
  fillchar(L, sizeof(L), 0);
  for I := 0 to Count-1 do
    inc(L[(data[I] shr 16) and $FF]);
  Pos := 0;
  for I := low(L) to high(L) do
  begin
    Value := Pos+L[I];
    L[I] := Pos;
    Pos := Value;
  end;
  R := L;
  Pos := 0;
  while Pos<Count do
  begin
    Value := data[Pos];
    Part := (Value shr 16) and $FF;
    if (Pos>=L[Part]) and (Pos<R[Part]) then
    begin
      Pos := R[Part];
      continue;
    end;
    I := Pos;
    repeat
      Pos := R[Part];
      inc(R[Part]);
      if Pos=I then
      begin
        data[Pos] := value;
        inc(Pos);
        break;
      end;
      NewValue := data[Pos];
      data[Pos] := Value;
      Value := NewValue;
      Part := (Value shr 16) and $FF;
    until false;
  end;
  for I := low(L) to high(L) do
    if R[I]-L[I]>1 then
      if R[I]-L[I]>64 then
        VectorLongwordJumpSort3(@data[L[I]], R[I]-L[I])
      else
        VectorLongwordQuickSort(@data[L[I]], R[I]-L[I]);
end;

// JumpSort algorythm (developed by Andrei Galatyn)
// 1-byte-for-step implementation for Longword type
procedure VectorLongwordJumpSort(Data: PLongwordList; Count: integer);
var
  L,R: array[byte] of integer;
  I,Pos: integer;
  Value,Part,NewValue: TLongwordType;
begin
  if Count<=0 then
    exit;
  fillchar(L, sizeof(L), 0);
  for I := 0 to Count-1 do
    inc(L[data[I] shr 24]);
  Pos := 0;
  for I := low(L) to high(L) do
  begin
    Value := Pos+L[I];
    L[I] := Pos;
    Pos := Value;
  end;
  R := L;
  Pos := 0;
  while Pos<Count do
  begin
    Value := data[Pos];
    Part := Value shr 24;
    if (Pos>=L[Part]) and (Pos<R[Part]) then
    begin
      Pos := R[Part];
      continue;
    end;
    I := Pos;
    repeat
      Pos := R[Part];
      inc(R[Part]);
      if Pos=I then
      begin
        data[Pos] := value;
        inc(Pos);
        break;
      end;
      NewValue := data[Pos];
      data[Pos] := Value;
      Value := NewValue;
      Part := Value shr 24;
    until false;
  end;
  for I := low(L) to high(L) do
    if R[I]-L[I]>1 then
      if R[I]-L[I]>64 then
        VectorLongwordJumpSort2(@data[L[I]], R[I]-L[I])
      else
        VectorLongwordQuickSort(@data[L[I]], R[I]-L[I]);
end;

procedure VectorWordJumpSort2(data: PWordList; Count: integer);
var
  L,R: array[byte] of integer;
  I,Pos: integer;
  Value,Part,NewValue: TLongwordType;
begin
  fillchar(L, sizeof(L), 0);
  for I := 0 to Count-1 do
    inc(L[data[I] and $FF]);
  Pos := 0;
  for I := low(L) to high(L) do
  begin
    Value := Pos+L[I];
    L[I] := Pos;
    Pos := Value;
  end;
  R := L;
  Pos := 0;
  while Pos<Count do
  begin
    Value := data[Pos];
    Part := Value and $FF;
    if (Pos>=L[Part]) and (Pos<R[Part]) then
    begin
      Pos := R[Part];
      continue;
    end;
    I := Pos;
    repeat
      Pos := R[Part];
      inc(R[Part]);
      if Pos=I then
      begin
        data[Pos] := value;
        inc(Pos);
        break;
      end;
      NewValue := data[Pos];
      data[Pos] := Value;
      Value := NewValue;
      Part := Value and $FF;
    until false;
  end;
end;

// JumpSort algorythm (developed by Andrei Galatyn)
// 1-byte-for-step implementation for Word type
procedure VectorWordJumpSort(Data: PWordList; Count: integer);
var
  L,R: array[byte] of integer;
  I,Pos: integer;
  Value,Part,NewValue: longword;
begin
  if Count<=0 then
    exit;
  fillchar(L, sizeof(L), 0);
  for I := 0 to Count-1 do
    inc(L[data[I] shr 8]);
  Pos := 0;
  for I := low(L) to high(L) do
  begin
    Value := Pos+L[I];
    L[I] := Pos;
    Pos := Value;
  end;
  R := L;
  Pos := 0;
  while Pos<Count do
  begin
    Value := data[Pos];
    Part := Value shr 8;
    if (Pos>=L[Part]) and (Pos<R[Part]) then
    begin
      Pos := R[Part];
      continue;
    end;
    I := Pos;
    repeat
      Pos := R[Part];
      inc(R[Part]);
      if Pos=I then
      begin
        data[Pos] := value;
        inc(Pos);
        break;
      end;
      NewValue := data[Pos];
      data[Pos] := Value;
      Value := NewValue;
      Part := Value shr 8;
    until false;
  end;
  for I := low(L) to high(L) do
    if R[I]-L[I]>1 then
      if R[I]-L[I]>64 then
        VectorWordJumpSort2(@data[L[I]], R[I]-L[I])
      else
        VectorWordQuickSort(@data[L[I]], R[I]-L[I]);
end;

// JumpSort algorythm (developed by Andrei Galatyn)
// 1-byte-for-step implementation for 32-bit integer type
procedure VectorIntegerJumpSort(Data: PIntegerList; Count: integer);
var
  L,R,M: integer;
begin
  if Count<=0 then
    exit;
  VectorLongwordJumpSort(pointer(data), Count);
  L := 0;
  R := Count-1;
  if Data[R]>=0 then
    exit;
  while R-L>1 do
  begin
    M := (R+L) shr 1;
    if Data[M]<0 then
      R := M
    else
      L := M;
  end;
  if Data[L]<0 then
    M := L
  else
    M := R;
  VectorIntegerRotate(Data, Count, -M);
end;

procedure VectorSmallintQuickSort(Data: PSmallintList; Count: integer);
var
  I, J: ^TSmallintType;
  P, T: TSmallintType;
begin
  if Count<=1 then
    exit;
  dec(Count); // Count -> right bound
  if Count<=75 then
  begin
    I := @Data[1];
    for T := 0 to Count-1 do
    begin
      P := I^;
      J := I;
      dec(J);
      while (Longword(J)>=Longword(data)) and (J^>P) do
      begin
        PSmallintList(J)[1] := J^;
        dec(J);
      end;
      PSmallintList(J)[1] := P;
      inc(I);
    end;
  end
  else
    repeat
      I := pointer(data);
      J := @data[Count];
      P := data[Count shr 1];
      repeat
        while I^<P do
          Inc(I);
        while J^>P do
          Dec(J);
        if Longword(I) <= Longword(J) then
        begin
          T := I^;
          I^ := J^;
          J^ := T;
          Inc(I);
          Dec(J);
        end;
      until Longword(I) > Longword(J);
      if Longword(J)>Longword(data) then
        VectorSmallintQuickSort(data, (Longword(J)-Longword(data)) div sizeof(TSmallintType)+1);
      dec(Count, (Longword(I)-Longword(data)) div sizeof(TSmallintType));
      data := pointer(I);
    until Count=0;
end;

procedure VectorCardinalReverse(Data: PCardinalList; Count: integer);
var
  i: integer;
  j: TCardinalType;
  src,dst: ^TCardinalType;
begin
  src := pointer(Data);
  dst := @Data[Count-1];
  for i := 0 to Count shr 1-1 do
  begin
    j := src^;
    src^ := dst^;
    dst^ := j;
    inc(src);
    dec(dst);
  end;
end;

procedure VectorCardinalRotate(Data: PCardinalList; Count, Shift: integer);
begin
  Shift := -Shift mod Count;
  if Shift<0 then
    inc(Shift, Count);
  if Shift=0 then
    exit;
  VectorCardinalReverse(Data, Shift);
  VectorCardinalReverse(@Data[Shift], Count-Shift);
  VectorCardinalReverse(Data, Count);
end;

procedure VectorCardinalQuickSort(Data: PCardinalList; Count: integer);
var
  I, J: ^TCardinalType;
  P, T: TCardinalType;
begin
  if Count<=1 then
    exit;
  dec(Count); // Count -> right bound
  if Count<=75 then
  begin
    I := @Data[1];
    for T := 0 to Count-1 do
    begin
      P := I^;
      J := I;
      dec(J);
      while (Longword(J)>=Longword(data)) and (J^>P) do
      begin
        PCardinalList(J)[1] := J^;
        dec(J);
      end;
      PCardinalList(J)[1] := P;
      inc(I);
    end;
  end
  else
    repeat
      I := pointer(data);
      J := @data[Count];
      P := data[Count shr 1];
      repeat
        while I^<P do
          Inc(I);
        while J^>P do
          Dec(J);
        if Longword(I) <= Longword(J) then
        begin
          T := I^;
          I^ := J^;
          J^ := T;
          Inc(I);
          Dec(J);
        end;
      until Longword(I) > Longword(J);
      if Longword(J)>Longword(data) then
        VectorCardinalQuickSort(data, (Longword(J)-Longword(data)) div sizeof(TCardinalType)+1);
      dec(Count, (Longword(I)-Longword(data)) div sizeof(TCardinalType));
      data := pointer(I);
    until Count=0;
end;

procedure VectorInt64Reverse(Data: PInt64List; Count: integer);
var
  i: integer;
  j: TInt64Type;
  src,dst: ^TInt64Type;
begin
  src := pointer(Data);
  dst := @Data[Count-1];
  for i := 0 to Count shr 1-1 do
  begin
    j := src^;
    src^ := dst^;
    dst^ := j;
    inc(src);
    dec(dst);
  end;
end;

procedure VectorInt64Rotate(Data: PInt64List; Count, Shift: integer);
begin
  Shift := -Shift mod Count;
  if Shift<0 then
    inc(Shift, Count);
  if Shift=0 then
    exit;
  VectorInt64Reverse(Data, Shift);
  VectorInt64Reverse(@Data[Shift], Count-Shift);
  VectorInt64Reverse(Data, Count);
end;

procedure VectorUInt64Reverse(Data: PInt64List; Count: integer);
begin
  VectorInt64Reverse(Data, Count);
end;

procedure VectorUInt64Rotate(Data: PInt64List; Count, Shift: integer);
begin
  VectorUInt64Rotate(Data, Count, Shift);
end;

procedure VectorInt64QuickSort(Data: PInt64List; Count: integer);
var
  I, J: ^TInt64Type;
  P, T: TInt64Type;
  N: integer;
begin
  if Count<=1 then
    exit;
  dec(Count); // Count -> right bound
  if Count<=75 then
  begin
    I := @Data[1];
    for N := 0 to Count-1 do
    begin
      P := I^;
      J := I;
      dec(J);
      while (Longword(J)>=Longword(data)) and (J^>P) do
      begin
        PInt64List(J)[1] := J^;
        dec(J);
      end;
      PInt64List(J)[1] := P;
      inc(I);
    end;
  end
  else
    repeat
      I := pointer(data);
      J := @data[Count];
      P := data[Count shr 1];
      repeat
        while I^<P do
          Inc(I);
        while J^>P do
          Dec(J);
        if Longword(I) <= Longword(J) then
        begin
          T := I^;
          I^ := J^;
          J^ := T;
          Inc(I);
          Dec(J);
        end;
      until Longword(I) > Longword(J);
      if Longword(J)>Longword(data) then
        VectorInt64QuickSort(data, (Longword(J)-Longword(data)) div sizeof(TInt64Type)+1);
      dec(Count, (Longword(I)-Longword(data)) div sizeof(TInt64Type));
      data := pointer(I);
    until Count=0;
end;

{$IFDEF HiCompiler}

procedure VectorUInt64InsSort(Data: PUInt64List; Count: integer);
var
  I,J: ^TUInt64Type;
  N: integer;
  P: TUInt64Type;
begin
  dec(Count); // Count -> right bound
  I := @Data[1];
  for N := 0 to Count-1 do
  begin
    P := I^;
    J := I;
    dec(J);
    while (Longword(J)>=Longword(data)) and (J^>P) do
    begin
      PInt64List(J)[1] := J^;
      dec(J);
    end;
    PInt64List(J)[1] := P;
    inc(I);
  end;
end;

{$ELSE}

function CmpUInt64(var a,b: TInt64Type):integer;
var pa,pb: ^TLongwordType;
begin
  pa := @a;
  pb := @b;
  inc(pa);
  inc(pb);
  if pa^<pb^ then
    result := -1
  else
    if pa^>pb^ then
      result := 1
    else
    begin
      dec(pa);
      dec(pb);
      if pa^<pb^ then
        result := -1
      else
        if pa^=pb^ then
          result := 0
        else
          result := 1;
    end;
end;

procedure VectorUInt64InsSort(Data: PInt64List; Count: integer);
var
  I,J: ^TInt64Type;
  N: integer;
  P: TInt64Type;
begin
  dec(Count); // Count -> right bound
  I := @Data[1];
  for N := 0 to Count-1 do
  begin
    P := I^;
    J := I;
    dec(J);
    while (Longword(J)>=Longword(data)) and (CmpUInt64(J^, P)>0) do
    begin
      PInt64List(J)[1] := J^;
      dec(J);
    end;
    PInt64List(J)[1] := P;
    inc(I);
  end;
end;

{$ENDIF}

// JumpSort algorythm (developed by Andrei Galatyn)
// 1-byte-for-step implementation for UInt64 type
procedure VectorUInt64JumpSort_int(Data: {$IFDEF HiCompiler}PUInt64List{$ELSE}PInt64List{$ENDIF}; Count, Shift: integer);
var
  L,R: array[byte] of integer;
  I,Pos: integer;
  Value,Part,NewValue: {$IFDEF HiCompiler}TUInt64Type{$ELSE}TInt64Type{$ENDIF};
begin
  if Count<=1 then
    exit;
  if Count<=50 then
  begin
    VectorUInt64InsSort(Data, Count);
    exit;
  end;
  fillchar(L, sizeof(L), 0);
  for I := 0 to Count-1 do
    inc(L[(data[I] shr Shift) and $FF]);
  Pos := 0;
  for I := low(L) to high(L) do
  begin
    Value := Pos+L[I];
    L[I] := Pos;
    Pos := Value;
  end;
  R := L;
  Pos := 0;
  while Pos<Count do
  begin
    Value := data[Pos];
    Part := (Value shr Shift) and $FF;
    if (Pos>=L[Part]) and (Pos<R[Part]) then
    begin
      Pos := R[Part];
      continue;
    end;
    I := Pos;
    repeat
      Pos := R[Part];
      inc(R[Part]);
      if Pos=I then
      begin
        data[Pos] := value;
        inc(Pos);
        break;
      end;
      NewValue := data[Pos];
      data[Pos] := Value;
      Value := NewValue;
      Part := (Value shr Shift) and $FF;
    until false;
  end;
  if Shift>0 then
    for I := low(L) to high(L) do
      if R[I]-L[I]>1 then
        VectorUInt64JumpSort_int(@data[L[I]], R[I]-L[I], Shift-8);
end;

procedure VectorUInt64JumpSort(Data: {$IFDEF HiCompiler}PUInt64List{$ELSE}PInt64List{$ENDIF}; Count: integer);
begin
  VectorUInt64JumpSort_int(Data, Count, 56);
end;

procedure VectorInt64JumpSort(Data: PInt64List; Count: integer);
var
  L,R,M: integer;
begin
  if Count<=0 then
    exit;
  VectorUInt64JumpSort(pointer(data), Count);
  L := 0;
  R := Count-1;
  if Data[R]>=0 then
    exit;
  while R-L>1 do
  begin
    M := (R+L) shr 1;
    if Data[M]<0 then
      R := M
    else
      L := M;
  end;
  if Data[L]<0 then
    M := L
  else
    M := R;
  VectorInt64Rotate(Data, Count, -M);
end;

// JumpSort algorythm (developed by Andrei Galatyn)
// 1-byte-for-step implementation for smallint type
procedure VectorSmallintJumpSort(Data: PSmallintList; Count: integer);
var
  L,R,M: integer;
begin
  if Count<=0 then
    exit;
  VectorWordJumpSort(pointer(data), Count);
  L := 0;
  R := Count-1;
  if Data[R]>=0 then
    exit;
  while R-L>1 do
  begin
    M := (R+L) shr 1;
    if Data[M]<0 then
      R := M
    else
      L := M;
  end;
  if Data[L]<0 then
    M := L
  else
    M := R;
  VectorSmallintRotate(Data, Count, -M);
end;

// ------------------------------ TTSmallint -------------------------

procedure SortMemoryRangeSmallint(Buf: pointer; L, R: Integer);
begin
{$IFDEF FPC}
  if R>L then
    VectorSmallintQuickSort(@PSmallintList(Buf)[L], R-L+1);
{$ELSE}
  if R>L then
    VectorSmallintJumpSort(@PSmallintList(Buf)[L], R-L+1);
{$ENDIF}
end;

constructor TTSmallint.Create(Container: TCCustom = nil);
begin
  FieldSize := sizeof(TSmallintType);
  FSortMemoryRange := @SortMemoryRangeSmallint;
  inherited;
end;

function TTSmallint.Add(Item: TSmallintType): TCHandle;
begin
  Result := FContainer.AddItem(item);
end;

function  TTSmallint.Front: TSmallintType;          // examine first element
begin
  result := items[FContainer.First];
end;

function  TTSmallint.Back: TSmallintType;           // examine last element
begin
  result := items[FContainer.Last];
end;

function TTSmallint.Push(v: TSmallintType):TCHandle;        // insert element at front
begin
  result := FContainer.InsertItem(FContainer.First, v);
end;

function  TTSmallint.Pop: TSmallintType;            // remove first element
var
  h: TCHandle;
begin
  h := FContainer.First;
  Result := items[h];
  FContainer.Delete(h);
end;

function TTSmallint.PushBack(v: TSmallintType):TCHandle;    // insert element at back
begin
  result := FContainer.AddItem(v);
end;

function  TTSmallint.PopBack: TSmallintType;        // remove last element
var
  h: TCHandle;
begin
  h := FContainer.Last;
  result := items[h];
  FContainer.Delete(h);
end;

function TTSmallint.Enqueue(v: TSmallintType):TCHandle;    // add to tail
begin
  result := FContainer.AddItem(v);
end;

function  TTSmallint.Dequeue: TSmallintType;       // read&remove from head
var
  h: TCHandle;
begin
  h := FContainer.First;
  Result := items[h];
  FContainer.Delete(h);
end;

procedure TTSmallint.Sort(compare: TOnSmallintCompare);
begin
  Sort(compare, FContainer.First, FContainer.Last);
end;

procedure TTSmallint.Sort(compare: TOnSmallintCompareProc);
begin
  Sort(compare, FContainer.First, FContainer.Last);
end;

procedure TTSmallint.Sort(compare: TOnSmallintCompare; AFirst, ALast: TCHandle);
begin
  FOnCompare := compare;
  FContainer.Sort(CmpExt, AFirst, ALast);
end;

procedure TTSmallint.Sort(compare: TOnSmallintCompareProc; AFirst, ALast: TCHandle);
begin
  FOnCompareProc := compare;
  FContainer.Sort(CmpExtProc, AFirst, ALast);
end;

function TTSmallint.AddTo(handle: TCHandle; dst: TTCustom): TCHandle;
begin
  Result := dst.Add([items[handle]]);
end;

function TTSmallint.GetAsVariant(handle: TCHandle): variant;
begin
  if handle=-1 then
    Result := null
  else
    Result := items[handle];
end;

procedure TTSmallint.SetAsVariant(handle: TCHandle; Value: variant);
begin
  items[handle] := Value;
end;

function TTSmallint.GetAsAnsiStr(handle: TCHandle): AnsiString;
begin
  result := AnsiString(IntToStr(int64(items[handle])));
end;

procedure TTSmallint.SetAsAnsiStr(handle: TCHandle; Value: AnsiString);
begin
  items[handle] := StrToInt64(String(value));
end;

function TTSmallint.GetAsWideStr(handle: TCHandle): WideString;
begin
  result := IntToStr(int64(items[handle]));
end;

procedure TTSmallint.SetAsWideStr(handle: TCHandle; Value: WideString);
begin
  items[handle] := StrToInt64(value);
end;

procedure TTSmallint.InitContainer(c: TCCustom);
begin
  inherited;
  c.FCompareValue := CmpInt;
end;

procedure TTSmallint.SetOnCompare(c: TOnSmallintCompare);
begin
  if assigned(c) then
    container.FCompareValue := CmpExt
  else
    container.FCompareValue := CmpInt;
  FOnCompareProc := nil;
  FOnCompare := c;
end;

procedure TTSmallint.SetOnCompareProc(c: TOnSmallintCompareProc);
begin
  if assigned(c) then
    container.FCompareValue := CmpExtProc
  else
    container.FCompareValue := CmpInt;
  FOnCompare := nil;
  FOnCompareProc := c;
end;

function TTSmallint.CmpInt(var a,b):integer;
begin
  if TSmallintType(a)<TSmallintType(b) then
    result := -1
  else
    if TSmallintType(a)=TSmallintType(b) then
      result := 0
    else
      result := 1;
end;

function TTSmallint.CmpExt(var a, b): integer;
begin
  Result := FOnCompare(TSmallintType(a), TSmallintType(b));
end;

function TTSmallint.CmpExtProc(var a, b): integer;
begin
  Result := FOnCompareProc(TSmallintType(a), TSmallintType(b));
end;

function TTSmallint.STCopy(Dst: TTCustom; h: TCHandle):TCHandle;
begin
  result := TTSmallint(dst).Add(Items[h]);
end;

procedure TTSmallint.STCopy(Dst: TTCustom; AFirst, ALast: TCHandle);
begin
  TTSmallint(dst).Add(Items[AFirst]);
  if AFirst<>ALast then
    repeat
      FContainer.Next(AFirst);
      TTSmallint(dst).Add(Items[AFirst]);
    until AFirst=ALast;
end;

procedure TTSmallint.STSet(Dst: TTCustom; h1,h2: TCHandle);
begin
  TTSmallint(dst)[h2] := Items[h1];
end;

procedure TTSmallint.STSet(Dst: TTCustom; start1,finish1, start2: TCHandle);
begin
  TTSmallint(dst)[start2] := Items[start1];
  if start1<>finish1 then
    repeat
      FContainer.Next(start1);
      dst.FContainer.Next(start2);
      TTSmallint(dst)[start2] := Items[start1];
    until start1=finish1;
end;

function TTSmallint.GetMap(key: TSmallintType): variant;
begin
  Result := FContainer.Fields[1].AsVariant[Find(key)];
end;

procedure TTSmallint.PutMap(key: TSmallintType; Value: variant);
var
  h: TCHandle;
begin
  h := Find(key);
  if h = -1 then
    AddPair(key, Value)
  else
    FContainer.Fields[1].AsVariant[h] := Value;
end;

function TTSmallint.GetHandleOf(Value: variant): TCHandle;
begin
  Result := Find(Value);
end;

function TTSmallint.AddList(const Items: array of const): TCHandle;
var
  i: integer;
  p: PVarRec;
begin
  Result := -1;
  for i := low(items) to high(items) do
  begin
    p := @items[i];
    if p.VType = vtObject then
      Result := ImportObject(p.VObject, Result)
    else
      Result := add(VarAsInt64(p));
  end;
end;

function TTSmallint.Find(Item: TSmallintType): TCHandle;
begin
  Result := FContainer.Find(item);
end;

function TTSmallint.FindFirstEqual(Item: TSmallintType): TCHandle;
begin
  Result := FContainer.FindFirstEqual(item);
end;

procedure TTSmallint.FindNextEqual(Item: TSmallintType; var handle: TCHandle);
begin
  FContainer.FindNextEqual(item, handle);
end;

function TTSmallint.GetItem(n: TCHandle): TSmallintType;
begin
  Result := TSmallintType(FContainer.Get(n)^);
end;

function TTSmallint.GetMinValue: TSmallintType;
begin
  Result := TSmallintType(FContainer.Get(FContainer.FindMin)^);
end;

function TTSmallint.RemoveMinValue: TSmallintType;
var
  n: TCHandle;
begin
  n := FContainer.FindMin;
  Result := TSmallintType(FContainer.Get(n)^);
  FContainer.Delete(n);
end;

function TTSmallint.GetMaxValue: TSmallintType;
begin
  Result := TSmallintType(FContainer.Get(FContainer.FindMax)^);
end;

function TTSmallint.RemoveMaxValue: TSmallintType;
var
  n: TCHandle;
begin
  n := FContainer.FindMax;
  Result := TSmallintType(FContainer.Get(n)^);
  FContainer.Delete(n);
end;

function TTSmallint.AddPair(key: TSmallintType; Value: variant): TCHandle;
begin
  result := Add(key);
  Container.Fields[1].AsVariant[result] := Value;
end;

function TTSmallint.Insert(handle: TCHandle; Item: TSmallintType): TCHandle;
begin
  Result := FContainer.InsertItem(handle, item);
end;

function TTSmallint.InsertAfter(handle: TCHandle; Item: TSmallintType): TCHandle;
begin
  Result := FContainer.InsertAfterItem(handle, item);
end;

procedure TTSmallint.Remove(Item: TSmallintType);
begin
  FContainer.Remove(item);
end;

procedure TTSmallint.RemoveAll(Item: TSmallintType);
begin
  FContainer.RemoveAll(item);
end;

procedure TTSmallint.SetItem(n: TCHandle; Value: TSmallintType);
begin
  FContainer.Put(n, Value);
end;

// ------------------------------ TTShortint -------------------------

procedure SortMemoryRangeShortint(Buf: pointer; L, R: Integer);
begin
{$IFDEF FPC}
  if R>L then
    VectorShortintQuickSort(@PShortintList(Buf)[L], R-L+1);
{$ELSE}
  if R>L then
    VectorShortintCountSort(@PShortintList(Buf)[L], R-L+1);
{$ENDIF}
end;

constructor TTShortint.Create(Container: TCCustom = nil);
begin
  FieldSize := sizeof(TShortintType);
  FSortMemoryRange := @SortMemoryRangeShortint;
  inherited;
end;

function TTShortint.Add(Item: TShortintType): TCHandle;
begin
  Result := FContainer.AddItem(item);
end;

function  TTShortint.Front: TShortintType;          // examine first element
begin
  result := items[FContainer.First];
end;

function  TTShortint.Back: TShortintType;           // examine last element
begin
  result := items[FContainer.Last];
end;

function TTShortint.Push(v: TShortintType):TCHandle;        // insert element at front
begin
  result := FContainer.InsertItem(FContainer.First, v);
end;

function  TTShortint.Pop: TShortintType;            // remove first element
var
  h: TCHandle;
begin
  h := FContainer.First;
  Result := items[h];
  FContainer.Delete(h);
end;

function TTShortint.PushBack(v: TShortintType):TCHandle;    // insert element at back
begin
  result := FContainer.AddItem(v);
end;

function  TTShortint.PopBack: TShortintType;        // remove last element
var
  h: TCHandle;
begin
  h := FContainer.Last;
  result := items[h];
  FContainer.Delete(h);
end;

function TTShortint.Enqueue(v: TShortintType):TCHandle;    // add to tail
begin
  result := FContainer.AddItem(v);
end;

function  TTShortint.Dequeue: TShortintType;       // read&remove from head
var
  h: TCHandle;
begin
  h := FContainer.First;
  Result := items[h];
  FContainer.Delete(h);
end;

procedure TTShortint.Sort(compare: TOnShortintCompare);
begin
  Sort(compare, FContainer.First, FContainer.Last);
end;

procedure TTShortint.Sort(compare: TOnShortintCompareProc);
begin
  Sort(compare, FContainer.First, FContainer.Last);
end;

procedure TTShortint.Sort(compare: TOnShortintCompare; AFirst, ALast: TCHandle);
begin
  FOnCompare := compare;
  FContainer.Sort(CmpExt, AFirst, ALast);
end;

procedure TTShortint.Sort(compare: TOnShortintCompareProc; AFirst, ALast: TCHandle);
begin
  FOnCompareProc := compare;
  FContainer.Sort(CmpExtProc, AFirst, ALast);
end;

function TTShortint.AddTo(handle: TCHandle; dst: TTCustom): TCHandle;
begin
  Result := dst.Add([items[handle]]);
end;

function TTShortint.GetAsVariant(handle: TCHandle): variant;
begin
  if handle=-1 then
    Result := null
  else
    Result := items[handle];
end;

procedure TTShortint.SetAsVariant(handle: TCHandle; Value: variant);
begin
  items[handle] := Value;
end;

function TTShortint.GetAsAnsiStr(handle: TCHandle): AnsiString;
begin
  result := AnsiString(IntToStr(int64(items[handle])));
end;

procedure TTShortint.SetAsAnsiStr(handle: TCHandle; Value: AnsiString);
begin
  items[handle] := StrToInt64(String(value));
end;

function TTShortint.GetAsWideStr(handle: TCHandle): WideString;
begin
  result := IntToStr(int64(items[handle]));
end;

procedure TTShortint.SetAsWideStr(handle: TCHandle; Value: WideString);
begin
  items[handle] := StrToInt64(value);
end;

procedure TTShortint.InitContainer(c: TCCustom);
begin
  inherited;
  c.FCompareValue := CmpInt;
end;

procedure TTShortint.SetOnCompare(c: TOnShortintCompare);
begin
  if assigned(c) then
    container.FCompareValue := CmpExt
  else
    container.FCompareValue := CmpInt;
  FOnCompareProc := nil;
  FOnCompare := c;
end;

procedure TTShortint.SetOnCompareProc(c: TOnShortintCompareProc);
begin
  if assigned(c) then
    container.FCompareValue := CmpExtProc
  else
    container.FCompareValue := CmpInt;
  FOnCompare := nil;
  FOnCompareProc := c;
end;

function TTShortint.CmpInt(var a,b):integer;
begin
  if TShortintType(a)<TShortintType(b) then
    result := -1
  else
    if TShortintType(a)=TShortintType(b) then
      result := 0
    else
      result := 1;
end;

function TTShortint.CmpExt(var a, b): integer;
begin
  Result := FOnCompare(TShortintType(a), TShortintType(b));
end;

function TTShortint.CmpExtProc(var a, b): integer;
begin
  Result := FOnCompareProc(TShortintType(a), TShortintType(b));
end;

function TTShortint.STCopy(Dst: TTCustom; h: TCHandle):TCHandle;
begin
  result := TTShortint(dst).Add(Items[h]);
end;

procedure TTShortint.STCopy(Dst: TTCustom; AFirst, ALast: TCHandle);
begin
  TTShortint(dst).Add(Items[AFirst]);
  if AFirst<>ALast then
    repeat
      FContainer.Next(AFirst);
      TTShortint(dst).Add(Items[AFirst]);
    until AFirst=ALast;
end;

procedure TTShortint.STSet(Dst: TTCustom; h1,h2: TCHandle);
begin
  TTShortint(dst)[h2] := Items[h1];
end;

procedure TTShortint.STSet(Dst: TTCustom; start1,finish1, start2: TCHandle);
begin
  TTShortint(dst)[start2] := Items[start1];
  if start1<>finish1 then
    repeat
      FContainer.Next(start1);
      dst.FContainer.Next(start2);
      TTShortint(dst)[start2] := Items[start1];
    until start1=finish1;
end;

function TTShortint.GetMap(key: TShortintType): variant;
begin
  Result := FContainer.Fields[1].AsVariant[Find(key)];
end;

procedure TTShortint.PutMap(key: TShortintType; Value: variant);
var
  h: TCHandle;
begin
  h := Find(key);
  if h = -1 then
    AddPair(key, Value)
  else
    FContainer.Fields[1].AsVariant[h] := Value;
end;

function TTShortint.GetHandleOf(Value: variant): TCHandle;
begin
  Result := Find(Value);
end;

function TTShortint.AddList(const Items: array of const): TCHandle;
var
  i: integer;
  p: PVarRec;
begin
  Result := -1;
  for i := low(items) to high(items) do
  begin
    p := @items[i];
    if p.VType = vtObject then
      Result := ImportObject(p.VObject, Result)
    else
      Result := add(VarAsInt64(p));
  end;
end;

function TTShortint.Find(Item: TShortintType): TCHandle;
begin
  Result := FContainer.Find(item);
end;

function TTShortint.FindFirstEqual(Item: TShortintType): TCHandle;
begin
  Result := FContainer.FindFirstEqual(item);
end;

procedure TTShortint.FindNextEqual(Item: TShortintType; var handle: TCHandle);
begin
  FContainer.FindNextEqual(item, handle);
end;

function TTShortint.GetItem(n: TCHandle): TShortintType;
begin
  Result := TShortintType(FContainer.Get(n)^);
end;

function TTShortint.GetMinValue: TShortintType;
begin
  Result := TShortintType(FContainer.Get(FContainer.FindMin)^);
end;

function TTShortint.RemoveMinValue: TShortintType;
var
  n: TCHandle;
begin
  n := FContainer.FindMin;
  Result := TShortintType(FContainer.Get(n)^);
  FContainer.Delete(n);
end;

function TTShortint.GetMaxValue: TShortintType;
begin
  Result := TShortintType(FContainer.Get(FContainer.FindMax)^);
end;

function TTShortint.RemoveMaxValue: TShortintType;
var
  n: TCHandle;
begin
  n := FContainer.FindMax;
  Result := TShortintType(FContainer.Get(n)^);
  FContainer.Delete(n);
end;

function TTShortint.AddPair(key: TShortintType; Value: variant): TCHandle;
begin
  result := Add(key);
  Container.Fields[1].AsVariant[result] := Value;
end;

function TTShortint.Insert(handle: TCHandle; Item: TShortintType): TCHandle;
begin
  Result := FContainer.InsertItem(handle, item);
end;

function TTShortint.InsertAfter(handle: TCHandle; Item: TShortintType): TCHandle;
begin
  Result := FContainer.InsertAfterItem(handle, item);
end;

procedure TTShortint.Remove(Item: TShortintType);
begin
  FContainer.Remove(item);
end;

procedure TTShortint.RemoveAll(Item: TShortintType);
begin
  FContainer.RemoveAll(item);
end;

procedure TTShortint.SetItem(n: TCHandle; Value: TShortintType);
begin
  FContainer.Put(n, Value);
end;

// ------------------------------ TTLongint -------------------------

procedure SortMemoryRangeLongint(Buf: pointer; L, R: Integer);
begin
{$IFDEF FPC}
  if R>L then
    VectorLongintQuickSort(@PLongintList(Buf)[L], R-L+1);
{$ELSE}
  if R>L then
    VectorLongintJumpSort(@PLongintList(Buf)[L], R-L+1);
{$ENDIF}
end;

constructor TTLongint.Create(Container: TCCustom = nil);
begin
  FieldSize := sizeof(TLongintType);
  FSortMemoryRange := @SortMemoryRangeLongint;
  inherited;
end;

function TTLongint.Add(Item: TLongintType): TCHandle;
begin
  Result := FContainer.AddItem(item);
end;

function  TTLongint.Front: TLongintType;          // examine first element
begin
  result := items[FContainer.First];
end;

function  TTLongint.Back: TLongintType;           // examine last element
begin
  result := items[FContainer.Last];
end;

function TTLongint.Push(v: TLongintType):TCHandle;        // insert element at front
begin
  result := FContainer.InsertItem(FContainer.First, v);
end;

function  TTLongint.Pop: TLongintType;            // remove first element
var
  h: TCHandle;
begin
  h := FContainer.First;
  Result := items[h];
  FContainer.Delete(h);
end;

function TTLongint.PushBack(v: TLongintType):TCHandle;    // insert element at back
begin
  result := FContainer.AddItem(v);
end;

function  TTLongint.PopBack: TLongintType;        // remove last element
var
  h: TCHandle;
begin
  h := FContainer.Last;
  result := items[h];
  FContainer.Delete(h);
end;

function TTLongint.Enqueue(v: TLongintType):TCHandle;    // add to tail
begin
  result := FContainer.AddItem(v);
end;

function  TTLongint.Dequeue: TLongintType;       // read&remove from head
var
  h: TCHandle;
begin
  h := FContainer.First;
  Result := items[h];
  FContainer.Delete(h);
end;

procedure TTLongint.Sort(compare: TOnLongintCompare);
begin
  Sort(compare, FContainer.First, FContainer.Last);
end;

procedure TTLongint.Sort(compare: TOnLongintCompareProc);
begin
  Sort(compare, FContainer.First, FContainer.Last);
end;

procedure TTLongint.Sort(compare: TOnLongintCompare; AFirst, ALast: TCHandle);
begin
  FOnCompare := compare;
  FContainer.Sort(CmpExt, AFirst, ALast);
end;

procedure TTLongint.Sort(compare: TOnLongintCompareProc; AFirst, ALast: TCHandle);
begin
  FOnCompareProc := compare;
  FContainer.Sort(CmpExtProc, AFirst, ALast);
end;

function TTLongint.AddTo(handle: TCHandle; dst: TTCustom): TCHandle;
begin
  Result := dst.Add([items[handle]]);
end;

function TTLongint.GetAsVariant(handle: TCHandle): variant;
begin
  if handle=-1 then
    Result := null
  else
    Result := items[handle];
end;

procedure TTLongint.SetAsVariant(handle: TCHandle; Value: variant);
begin
  items[handle] := Value;
end;

function TTLongint.GetAsAnsiStr(handle: TCHandle): AnsiString;
begin
  result := AnsiString(IntToStr(int64(items[handle])));
end;

procedure TTLongint.SetAsAnsiStr(handle: TCHandle; Value: AnsiString);
begin
  items[handle] := StrToInt64(String(value));
end;

function TTLongint.GetAsWideStr(handle: TCHandle): WideString;
begin
  result := IntToStr(int64(items[handle]));
end;

procedure TTLongint.SetAsWideStr(handle: TCHandle; Value: WideString);
begin
  items[handle] := StrToInt64(value);
end;

procedure TTLongint.InitContainer(c: TCCustom);
begin
  inherited;
  c.FCompareValue := CmpInt;
end;

procedure TTLongint.SetOnCompare(c: TOnLongintCompare);
begin
  if assigned(c) then
    container.FCompareValue := CmpExt
  else
    container.FCompareValue := CmpInt;
  FOnCompareProc := nil;
  FOnCompare := c;
end;

procedure TTLongint.SetOnCompareProc(c: TOnLongintCompareProc);
begin
  if assigned(c) then
    container.FCompareValue := CmpExtProc
  else
    container.FCompareValue := CmpInt;
  FOnCompare := nil;
  FOnCompareProc := c;
end;

function TTLongint.CmpInt(var a,b):integer;
begin
  if TLongintType(a)<TLongintType(b) then
    result := -1
  else
    if TLongintType(a)=TLongintType(b) then
      result := 0
    else
      result := 1;
end;

function TTLongint.CmpExt(var a, b): integer;
begin
  Result := FOnCompare(TLongintType(a), TLongintType(b));
end;

function TTLongint.CmpExtProc(var a, b): integer;
begin
  Result := FOnCompareProc(TLongintType(a), TLongintType(b));
end;

function TTLongint.STCopy(Dst: TTCustom; h: TCHandle):TCHandle;
begin
  result := TTLongint(dst).Add(Items[h]);
end;

procedure TTLongint.STCopy(Dst: TTCustom; AFirst, ALast: TCHandle);
begin
  TTLongint(dst).Add(Items[AFirst]);
  if AFirst<>ALast then
    repeat
      FContainer.Next(AFirst);
      TTLongint(dst).Add(Items[AFirst]);
    until AFirst=ALast;
end;

procedure TTLongint.STSet(Dst: TTCustom; h1,h2: TCHandle);
begin
  TTLongint(dst)[h2] := Items[h1];
end;

procedure TTLongint.STSet(Dst: TTCustom; start1,finish1, start2: TCHandle);
begin
  TTLongint(dst)[start2] := Items[start1];
  if start1<>finish1 then
    repeat
      FContainer.Next(start1);
      dst.FContainer.Next(start2);
      TTLongint(dst)[start2] := Items[start1];
    until start1=finish1;
end;

function TTLongint.GetMap(key: TLongintType): variant;
begin
  Result := FContainer.Fields[1].AsVariant[Find(key)];
end;

procedure TTLongint.PutMap(key: TLongintType; Value: variant);
var
  h: TCHandle;
begin
  h := Find(key);
  if h = -1 then
    AddPair(key, Value)
  else
    FContainer.Fields[1].AsVariant[h] := Value;
end;

function TTLongint.GetHandleOf(Value: variant): TCHandle;
begin
  Result := Find(Value);
end;

function TTLongint.AddList(const Items: array of const): TCHandle;
var
  i: integer;
  p: PVarRec;
begin
  Result := -1;
  for i := low(items) to high(items) do
  begin
    p := @items[i];
    if p.VType = vtObject then
      Result := ImportObject(p.VObject, Result)
    else
      Result := add(VarAsInt64(p));
  end;
end;

function TTLongint.Find(Item: TLongintType): TCHandle;
begin
  Result := FContainer.Find(item);
end;

function TTLongint.FindFirstEqual(Item: TLongintType): TCHandle;
begin
  Result := FContainer.FindFirstEqual(item);
end;

procedure TTLongint.FindNextEqual(Item: TLongintType; var handle: TCHandle);
begin
  FContainer.FindNextEqual(item, handle);
end;

function TTLongint.GetItem(n: TCHandle): TLongintType;
begin
  Result := TLongintType(FContainer.Get(n)^);
end;

function TTLongint.GetMinValue: TLongintType;
begin
  Result := TLongintType(FContainer.Get(FContainer.FindMin)^);
end;

function TTLongint.RemoveMinValue: TLongintType;
var
  n: TCHandle;
begin
  n := FContainer.FindMin;
  Result := TLongintType(FContainer.Get(n)^);
  FContainer.Delete(n);
end;

function TTLongint.GetMaxValue: TLongintType;
begin
  Result := TLongintType(FContainer.Get(FContainer.FindMax)^);
end;

function TTLongint.RemoveMaxValue: TLongintType;
var
  n: TCHandle;
begin
  n := FContainer.FindMax;
  Result := TLongintType(FContainer.Get(n)^);
  FContainer.Delete(n);
end;

function TTLongint.AddPair(key: TLongintType; Value: variant): TCHandle;
begin
  result := Add(key);
  Container.Fields[1].AsVariant[result] := Value;
end;

function TTLongint.Insert(handle: TCHandle; Item: TLongintType): TCHandle;
begin
  Result := FContainer.InsertItem(handle, item);
end;

function TTLongint.InsertAfter(handle: TCHandle; Item: TLongintType): TCHandle;
begin
  Result := FContainer.InsertAfterItem(handle, item);
end;

procedure TTLongint.Remove(Item: TLongintType);
begin
  FContainer.Remove(item);
end;

procedure TTLongint.RemoveAll(Item: TLongintType);
begin
  FContainer.RemoveAll(item);
end;

procedure TTLongint.SetItem(n: TCHandle; Value: TLongintType);
begin
  FContainer.Put(n, Value);
end;

// ------------------------------ TTCardinal -------------------------

{$IFDEF FPC}
procedure SortMemoryRangeCardinal(Buf: pointer; L, R: Integer);
begin
  if R>L then
    VectorCardinalQuickSort(@PCardinalList(Buf)[L], R-L+1);
end;

constructor TTCardinal.Create(Container: TCCustom = nil);
begin
  FieldSize := sizeof(TCardinalType);
  FSortMemoryRange := @SortMemoryRangeCardinal;
  inherited;
end;

function TTCardinal.Add(Item: TCardinalType): TCHandle;
begin
  Result := FContainer.AddItem(item);
end;

function  TTCardinal.Front: TCardinalType;          // examine first element
begin
  result := items[FContainer.First];
end;

function  TTCardinal.Back: TCardinalType;           // examine last element
begin
  result := items[FContainer.Last];
end;

function TTCardinal.Push(v: TCardinalType):TCHandle;        // insert element at front
begin
  result := FContainer.InsertItem(FContainer.First, v);
end;

function  TTCardinal.Pop: TCardinalType;            // remove first element
var
  h: TCHandle;
begin
  h := FContainer.First;
  Result := items[h];
  FContainer.Delete(h);
end;

function TTCardinal.PushBack(v: TCardinalType):TCHandle;    // insert element at back
begin
  result := FContainer.AddItem(v);
end;

function  TTCardinal.PopBack: TCardinalType;        // remove last element
var
  h: TCHandle;
begin
  h := FContainer.Last;
  result := items[h];
  FContainer.Delete(h);
end;

function TTCardinal.Enqueue(v: TCardinalType):TCHandle;    // add to tail
begin
  result := FContainer.AddItem(v);
end;

function  TTCardinal.Dequeue: TCardinalType;       // read&remove from head
var
  h: TCHandle;
begin
  h := FContainer.First;
  Result := items[h];
  FContainer.Delete(h);
end;

procedure TTCardinal.Sort(compare: TOnCardinalCompare);
begin
  Sort(compare, FContainer.First, FContainer.Last);
end;

procedure TTCardinal.Sort(compare: TOnCardinalCompareProc);
begin
  Sort(compare, FContainer.First, FContainer.Last);
end;

procedure TTCardinal.Sort(compare: TOnCardinalCompare; AFirst, ALast: TCHandle);
begin
  FOnCompare := compare;
  FContainer.Sort(CmpExt, AFirst, ALast);
end;

procedure TTCardinal.Sort(compare: TOnCardinalCompareProc; AFirst, ALast: TCHandle);
begin
  FOnCompareProc := compare;
  FContainer.Sort(CmpExtProc, AFirst, ALast);
end;

function TTCardinal.AddTo(handle: TCHandle; dst: TTCustom): TCHandle;
begin
  Result := dst.Add([items[handle]]);
end;

function TTCardinal.GetAsVariant(handle: TCHandle): variant;
begin
  if handle=-1 then
    Result := null
  else
    Result := items[handle];
end;

procedure TTCardinal.SetAsVariant(handle: TCHandle; Value: variant);
begin
  items[handle] := Value;
end;

function TTCardinal.GetAsAnsiStr(handle: TCHandle): AnsiString;
begin
  result := IntToStr(int64(items[handle]));
end;

procedure TTCardinal.SetAsAnsiStr(handle: TCHandle; Value: AnsiString);
begin
  items[handle] := StrToInt64(value);
end;

function TTCardinal.GetAsWideStr(handle: TCHandle): WideString;
begin
  result := IntToStr(int64(items[handle]));
end;

procedure TTCardinal.SetAsWideStr(handle: TCHandle; Value: WideString);
begin
  items[handle] := StrToInt64(value);
end;

procedure TTCardinal.InitContainer(c: TCCustom);
begin
  inherited;
  c.FCompareValue := CmpInt;
end;

procedure TTCardinal.SetOnCompare(c: TOnCardinalCompare);
begin
  if assigned(c) then
    container.FCompareValue := CmpExt
  else
    container.FCompareValue := CmpInt;
  FOnCompareProc := nil;
  FOnCompare := c;
end;

procedure TTCardinal.SetOnCompareProc(c: TOnCardinalCompareProc);
begin
  if assigned(c) then
    container.FCompareValue := CmpExtProc
  else
    container.FCompareValue := CmpInt;
  FOnCompare := nil;
  FOnCompareProc := c;
end;

function TTCardinal.CmpInt(var a,b):integer;
begin
  if TCardinalType(a)<TCardinalType(b) then
    result := -1
  else
    if TCardinalType(a)=TCardinalType(b) then
      result := 0
    else
      result := 1;
end;

function TTCardinal.CmpExt(var a, b): integer;
begin
  Result := FOnCompare(TCardinalType(a), TCardinalType(b));
end;

function TTCardinal.CmpExtProc(var a, b): integer;
begin
  Result := FOnCompareProc(TCardinalType(a), TCardinalType(b));
end;

function TTCardinal.STCopy(Dst: TTCustom; h: TCHandle):TCHandle;
begin
  result := TTCardinal(dst).Add(Items[h]);
end;

procedure TTCardinal.STCopy(Dst: TTCustom; AFirst, ALast: TCHandle);
begin
  TTCardinal(dst).Add(Items[AFirst]);
  if AFirst<>ALast then
    repeat
      FContainer.Next(AFirst);
      TTCardinal(dst).Add(Items[AFirst]);
    until AFirst=ALast;
end;

procedure TTCardinal.STSet(Dst: TTCustom; h1,h2: TCHandle);
begin
  TTCardinal(dst)[h2] := Items[h1];
end;

procedure TTCardinal.STSet(Dst: TTCustom; start1,finish1, start2: TCHandle);
begin
  TTCardinal(dst)[start2] := Items[start1];
  if start1<>finish1 then
    repeat
      FContainer.Next(start1);
      dst.FContainer.Next(start2);
      TTCardinal(dst)[start2] := Items[start1];
    until start1=finish1;
end;

function TTCardinal.GetMap(key: TCardinalType): variant;
begin
  Result := FContainer.Fields[1].AsVariant[Find(key)];
end;

procedure TTCardinal.PutMap(key: TCardinalType; Value: variant);
var
  h: TCHandle;
begin
  h := Find(key);
  if h = -1 then
    AddPair(key, Value)
  else
    FContainer.Fields[1].AsVariant[h] := Value;
end;

function TTCardinal.GetHandleOf(Value: variant): TCHandle;
begin
  Result := Find(Value);
end;

function TTCardinal.AddList(const Items: array of const): TCHandle;
var
  i: integer;
  p: PVarRec;
begin
  Result := -1;
  for i := low(items) to high(items) do
  begin
    p := @items[i];
    if p.VType = vtObject then
      Result := ImportObject(p.VObject, Result)
    else
      Result := add(VarAsInt64(p));
  end;
end;

function TTCardinal.Find(Item: TCardinalType): TCHandle;
begin
  Result := FContainer.Find(item);
end;

function TTCardinal.FindFirstEqual(Item: TCardinalType): TCHandle;
begin
  Result := FContainer.FindFirstEqual(item);
end;

procedure TTCardinal.FindNextEqual(Item: TCardinalType; var handle: TCHandle);
begin
  FContainer.FindNextEqual(item, handle);
end;

function TTCardinal.GetItem(n: TCHandle): TCardinalType;
begin
  Result := TCardinalType(FContainer.Get(n)^);
end;

function TTCardinal.GetMinValue: TCardinalType;
begin
  Result := TCardinalType(FContainer.Get(FContainer.FindMin)^);
end;

function TTCardinal.RemoveMinValue: TCardinalType;
var
  n: TCHandle;
begin
  n := FContainer.FindMin;
  Result := TCardinalType(FContainer.Get(n)^);
  FContainer.Delete(n);
end;

function TTCardinal.GetMaxValue: TCardinalType;
begin
  Result := TCardinalType(FContainer.Get(FContainer.FindMax)^);
end;

function TTCardinal.RemoveMaxValue: TCardinalType;
var
  n: TCHandle;
begin
  n := FContainer.FindMax;
  Result := TCardinalType(FContainer.Get(n)^);
  FContainer.Delete(n);
end;

function TTCardinal.AddPair(key: TCardinalType; Value: variant): TCHandle;
begin
  result := Add(key);
  Container.Fields[1].AsVariant[result] := Value;
end;

function TTCardinal.Insert(handle: TCHandle; Item: TCardinalType): TCHandle;
begin
  Result := FContainer.InsertItem(handle, item);
end;

function TTCardinal.InsertAfter(handle: TCHandle; Item: TCardinalType): TCHandle;
begin
  Result := FContainer.InsertAfterItem(handle, item);
end;

procedure TTCardinal.Remove(Item: TCardinalType);
begin
  FContainer.Remove(item);
end;

procedure TTCardinal.RemoveAll(Item: TCardinalType);
begin
  FContainer.RemoveAll(item);
end;

procedure TTCardinal.SetItem(n: TCHandle; Value: TCardinalType);
begin
  FContainer.Put(n, Value);
end;
{$ENDIF}

procedure VectorRealQuickSort(Data: PRealList; Count: integer);
var
  I, J: ^TRealType;
  P, T: TRealType;
  N: integer;
begin
  if Count<=1 then
    exit;
  dec(Count); // Count -> right bound
  if Count<=75 then
  begin
    I := @Data[1];
    for N := 0 to Count-1 do
    begin
      P := I^;
      J := I;
      dec(J);
      while (Longword(J)>=Longword(data)) and (J^>P) do
      begin
        PRealList(J)[1] := J^;
        dec(J);
      end;
      PRealList(J)[1] := P;
      inc(I);
    end;
  end
  else
    repeat
      I := pointer(data);
      J := @data[Count];
      P := data[Count shr 1];
      repeat
        while I^<P do
          Inc(I);
        while J^>P do
          Dec(J);
        if Longword(I) <= Longword(J) then
        begin
          T := I^;
          I^ := J^;
          J^ := T;
          Inc(I);
          Dec(J);
        end;
      until Longword(I) > Longword(J);
      if Longword(J)>Longword(data) then
        VectorRealQuickSort(data, (Longword(J)-Longword(data)) div sizeof(TRealType)+1);
      dec(Count, (Longword(I)-Longword(data)) div sizeof(TRealType));
      data := pointer(I);
    until Count=0;
end;

{$IFNDEF FPC}
procedure VectorReal48QuickSort(Data: PReal48List; Count: integer);
var
  I, J: ^TReal48Type;
  P, T: TReal48Type;
  N: integer;
begin
  if Count<=1 then
    exit;
  dec(Count); // Count -> right bound
  if Count<=75 then
  begin
    I := @Data[1];
    for N := 0 to Count-1 do
    begin
      P := I^;
      J := I;
      dec(J);
      while (Longword(J)>=Longword(data)) and (J^>P) do
      begin
        PReal48List(J)[1] := J^;
        dec(J);
      end;
      PReal48List(J)[1] := P;
      inc(I);
    end;
  end
  else
    repeat
      I := pointer(data);
      J := @data[Count];
      P := data[Count shr 1];
      repeat
        while I^<P do
          Inc(I);
        while J^>P do
          Dec(J);
        if Longword(I) <= Longword(J) then
        begin
          T := I^;
          I^ := J^;
          J^ := T;
          Inc(I);
          Dec(J);
        end;
      until Longword(I) > Longword(J);
      if Longword(J)>Longword(data) then
        VectorReal48QuickSort(data, (Longword(J)-Longword(data)) div sizeof(TReal48Type)+1);
      dec(Count, (Longword(I)-Longword(data)) div sizeof(TReal48Type));
      data := pointer(I);
    until Count=0;
end;
{$ENDIF}

procedure VectorSingleQuickSort(Data: PSingleList; Count: integer);
var
  I, J: ^TSingleType;
  P, T: TSingleType;
  N: integer;
begin
  if Count<=1 then
    exit;
  dec(Count); // Count -> right bound
  if Count<=75 then
  begin
    I := @Data[1];
    for N := 0 to Count-1 do
    begin
      P := I^;
      J := I;
      dec(J);
      while (Longword(J)>=Longword(data)) and (J^>P) do
      begin
        PSingleList(J)[1] := J^;
        dec(J);
      end;
      PSingleList(J)[1] := P;
      inc(I);
    end;
  end
  else
    repeat
      I := pointer(data);
      J := @data[Count];
      P := data[Count shr 1];
      repeat
        while I^<P do
          Inc(I);
        while J^>P do
          Dec(J);
        if Longword(I) <= Longword(J) then
        begin
          T := I^;
          I^ := J^;
          J^ := T;
          Inc(I);
          Dec(J);
        end;
      until Longword(I) > Longword(J);
      if Longword(J)>Longword(data) then
        VectorSingleQuickSort(data, (Longword(J)-Longword(data)) div sizeof(TSingleType)+1);
      dec(Count, (Longword(I)-Longword(data)) div sizeof(TSingleType));
      data := pointer(I);
    until Count=0;
end;

procedure VectorDoubleQuickSort(Data: PDoubleList; Count: integer);
var
  I, J: ^TDoubleType;
  P, T: TDoubleType;
  N: integer;
begin
  if Count<=1 then
    exit;
  dec(Count); // Count -> right bound
  if Count<=75 then
  begin
    I := @Data[1];
    for N := 0 to Count-1 do
    begin
      P := I^;
      J := I;
      dec(J);
      while (Longword(J)>=Longword(data)) and (J^>P) do
      begin
        PDoubleList(J)[1] := J^;
        dec(J);
      end;
      PDoubleList(J)[1] := P;
      inc(I);
    end;
  end
  else
    repeat
      I := pointer(data);
      J := @data[Count];
      P := data[Count shr 1];
      repeat
        while I^<P do
          Inc(I);
        while J^>P do
          Dec(J);
        if Longword(I) <= Longword(J) then
        begin
          T := I^;
          I^ := J^;
          J^ := T;
          Inc(I);
          Dec(J);
        end;
      until Longword(I) > Longword(J);
      if Longword(J)>Longword(data) then
        VectorDoubleQuickSort(data, (Longword(J)-Longword(data)) div sizeof(TDoubleType)+1);
      dec(Count, (Longword(I)-Longword(data)) div sizeof(TDoubleType));
      data := pointer(I);
    until Count=0;
end;

procedure VectorExtendedQuickSort(Data: PExtendedList; Count: integer);
var
  I, J: ^TExtendedType;
  P, T: TExtendedType;
  N: integer;
begin
  if Count<=1 then
    exit;
  dec(Count); // Count -> right bound
  if Count<=75 then
  begin
    I := @Data[1];
    for N := 0 to Count-1 do
    begin
      P := I^;
      J := I;
      dec(J);
      while (Longword(J)>=Longword(data)) and (J^>P) do
      begin
        PExtendedList(J)[1] := J^;
        dec(J);
      end;
      PExtendedList(J)[1] := P;
      inc(I);
    end;
  end
  else
    repeat
      I := pointer(data);
      J := @data[Count];
      P := data[Count shr 1];
      repeat
        while I^<P do
          Inc(I);
        while J^>P do
          Dec(J);
        if Longword(I) <= Longword(J) then
        begin
          T := I^;
          I^ := J^;
          J^ := T;
          Inc(I);
          Dec(J);
        end;
      until Longword(I) > Longword(J);
      if Longword(J)>Longword(data) then
        VectorExtendedQuickSort(data, (Longword(J)-Longword(data)) div sizeof(TExtendedType)+1);
      dec(Count, (Longword(I)-Longword(data)) div sizeof(TExtendedType));
      data := pointer(I);
    until Count=0;
end;

procedure VectorCompQuickSort(Data: PCompList; Count: integer);
var
  I, J: ^TCompType;
  P, T: TCompType;
  N: integer;
begin
  if Count<=1 then
    exit;
  dec(Count); // Count -> right bound
  if Count<=75 then
  begin
    I := @Data[1];
    for N := 0 to Count-1 do
    begin
      P := I^;
      J := I;
      dec(J);
      while (Longword(J)>=Longword(data)) and (J^>P) do
      begin
        PCompList(J)[1] := J^;
        dec(J);
      end;
      PCompList(J)[1] := P;
      inc(I);
    end;
  end
  else
    repeat
      I := pointer(data);
      J := @data[Count];
      P := data[Count shr 1];
      repeat
        while I^<P do
          Inc(I);
        while J^>P do
          Dec(J);
        if Longword(I) <= Longword(J) then
        begin
          T := I^;
          I^ := J^;
          J^ := T;
          Inc(I);
          Dec(J);
        end;
      until Longword(I) > Longword(J);
      if Longword(J)>Longword(data) then
        VectorCompQuickSort(data, (Longword(J)-Longword(data)) div sizeof(TCompType)+1);
      dec(Count, (Longword(I)-Longword(data)) div sizeof(TCompType));
      data := pointer(I);
    until Count=0;
end;

procedure VectorCurrencyQuickSort(Data: PCurrencyList; Count: integer);
var
  I, J: ^TCurrencyType;
  P, T: TCurrencyType;
  N: integer;
begin
  if Count<=1 then
    exit;
  dec(Count); // Count -> right bound
  if Count<=75 then
  begin
    I := @Data[1];
    for N := 0 to Count-1 do
    begin
      P := I^;
      J := I;
      dec(J);
      while (Longword(J)>=Longword(data)) and (J^>P) do
      begin
        PCurrencyList(J)[1] := J^;
        dec(J);
      end;
      PCurrencyList(J)[1] := P;
      inc(I);
    end;
  end
  else
    repeat
      I := pointer(data);
      J := @data[Count];
      P := data[Count shr 1];
      repeat
        while I^<P do
          Inc(I);
        while J^>P do
          Dec(J);
        if Longword(I) <= Longword(J) then
        begin
          T := I^;
          I^ := J^;
          J^ := T;
          Inc(I);
          Dec(J);
        end;
      until Longword(I) > Longword(J);
      if Longword(J)>Longword(data) then
        VectorCurrencyQuickSort(data, (Longword(J)-Longword(data)) div sizeof(TCurrencyType)+1);
      dec(Count, (Longword(I)-Longword(data)) div sizeof(TCurrencyType));
      data := pointer(I);
    until Count=0;
end;

procedure VectorCharQuickSort(Data: PCharList; Count: integer);
var
  I, J: ^TCharType;
  P, T: TCharType;
  N: integer;
begin
  if Count<=1 then
    exit;
  dec(Count); // Count -> right bound
  if Count<=75 then
  begin
    I := @Data[1];
    for N := 0 to Count-1 do
    begin
      P := I^;
      J := I;
      dec(J);
      while (Longword(J)>=Longword(data)) and (J^>P) do
      begin
        PCharList(J)[1] := J^;
        dec(J);
      end;
      PCharList(J)[1] := P;
      inc(I);
    end;
  end
  else
    repeat
      I := pointer(data);
      J := @data[Count];
      P := data[Count shr 1];
      repeat
        while I^<P do
          Inc(I);
        while J^>P do
          Dec(J);
        if Longword(I) <= Longword(J) then
        begin
          T := I^;
          I^ := J^;
          J^ := T;
          Inc(I);
          Dec(J);
        end;
      until Longword(I) > Longword(J);
      if Longword(J)>Longword(data) then
        VectorCharQuickSort(data, (Longword(J)-Longword(data)) div sizeof(TCharType)+1);
      dec(Count, (Longword(I)-Longword(data)) div sizeof(TCharType));
      data := pointer(I);
    until Count=0;
end;

procedure VectorAnsiCharQuickSort(Data: PAnsiCharList; Count: integer);
var
  I, J: ^TAnsiCharType;
  P, T: TAnsiCharType;
  N: integer;
begin
  if Count<=1 then
    exit;
  dec(Count); // Count -> right bound
  if Count<=75 then
  begin
    I := @Data[1];
    for N := 0 to Count-1 do
    begin
      P := I^;
      J := I;
      dec(J);
      while (Longword(J)>=Longword(data)) and (J^>P) do
      begin
        PAnsiCharList(J)[1] := J^;
        dec(J);
      end;
      PAnsiCharList(J)[1] := P;
      inc(I);
    end;
  end
  else
    repeat
      I := pointer(data);
      J := @data[Count];
      P := data[Count shr 1];
      repeat
        while I^<P do
          Inc(I);
        while J^>P do
          Dec(J);
        if Longword(I) <= Longword(J) then
        begin
          T := I^;
          I^ := J^;
          J^ := T;
          Inc(I);
          Dec(J);
        end;
      until Longword(I) > Longword(J);
      if Longword(J)>Longword(data) then
        VectorAnsiCharQuickSort(data, (Longword(J)-Longword(data)) div sizeof(TAnsiCharType)+1);
      dec(Count, (Longword(I)-Longword(data)) div sizeof(TAnsiCharType));
      data := pointer(I);
    until Count=0;
end;

procedure VectorWideCharQuickSort(Data: PWideCharList; Count: integer);
var
  I, J: ^TWideCharType;
  P, T: TWideCharType;
  N: integer;
begin
  if Count<=1 then
    exit;
  dec(Count); // Count -> right bound
  if Count<=75 then
  begin
    I := @Data[1];
    for N := 0 to Count-1 do
    begin
      P := I^;
      J := I;
      dec(J);
      while (Longword(J)>=Longword(data)) and (J^>P) do
      begin
        PWideCharList(J)[1] := J^;
        dec(J);
      end;
      PWideCharList(J)[1] := P;
      inc(I);
    end;
  end
  else
    repeat
      I := pointer(data);
      J := @data[Count];
      P := data[Count shr 1];
      repeat
        while I^<P do
          Inc(I);
        while J^>P do
          Dec(J);
        if Longword(I) <= Longword(J) then
        begin
          T := I^;
          I^ := J^;
          J^ := T;
          Inc(I);
          Dec(J);
        end;
      until Longword(I) > Longword(J);
      if Longword(J)>Longword(data) then
        VectorWideCharQuickSort(data, (Longword(J)-Longword(data)) div sizeof(TWideCharType)+1);
      dec(Count, (Longword(I)-Longword(data)) div sizeof(TWideCharType));
      data := pointer(I);
    until Count=0;
end;

// TTReal

procedure SortMemoryRangeReal(Buf: pointer; L, R: Integer);
begin
  if R>L then
    VectorRealQuickSort(@PRealList(Buf)[L], R-L+1);
end;

constructor TTReal.Create(Container: TCCustom = nil);
begin
  FieldSize := sizeof(TRealType);
  FSortMemoryRange := @SortMemoryRangeReal;
  inherited;
end;

function TTReal.Add(Item: TRealType): TCHandle;
begin
  Result := FContainer.AddItem(item);
end;

function  TTReal.Front: TRealType;          // examine first element
begin
  result := items[FContainer.First];
end;

function  TTReal.Back: TRealType;           // examine last element
begin
  result := items[FContainer.Last];
end;

function TTReal.Push(v: TRealType):TCHandle;        // insert element at front
begin
  result := FContainer.InsertItem(FContainer.First, v);
end;

function  TTReal.Pop: TRealType;            // remove first element
var
  h: TCHandle;
begin
  h := FContainer.First;
  Result := items[h];
  FContainer.Delete(h);
end;

function TTReal.PushBack(v: TRealType):TCHandle;    // insert element at back
begin
  result := FContainer.AddItem(v);
end;

function  TTReal.PopBack: TRealType;        // remove last element
var
  h: TCHandle;
begin
  h := FContainer.Last;
  result := items[h];
  FContainer.Delete(h);
end;

function TTReal.Enqueue(v: TRealType):TCHandle;    // add to tail
begin
  result := FContainer.AddItem(v);
end;

function  TTReal.Dequeue: TRealType;       // read&remove from head
var
  h: TCHandle;
begin
  h := FContainer.First;
  Result := items[h];
  FContainer.Delete(h);
end;

function TTReal.AddTo(handle: TCHandle; dst: TTCustom): TCHandle;
begin
  Result := dst.Add([items[handle]]);
end;

function TTReal.GetAsVariant(handle: TCHandle): variant;
begin
  if handle=-1 then
    Result := null
  else
    Result := items[handle];
end;

procedure TTReal.SetAsVariant(handle: TCHandle; Value: variant);
begin
  items[handle] := Value;
end;

function TTReal.GetAsAnsiStr(handle: TCHandle): AnsiString;
begin
  result := AnsiString(FloatToStr(items[handle]));
end;

procedure TTReal.SetAsAnsiStr(handle: TCHandle; Value: AnsiString);
begin
  items[handle] := StrToFloat(String(value));
end;

function TTReal.GetAsWideStr(handle: TCHandle): WideString;
begin
  result := FloatToStr(items[handle]);
end;

procedure TTReal.SetAsWideStr(handle: TCHandle; Value: WideString);
begin
  items[handle] := StrToFloat(value);
end;

function TTReal.GetHandleOf(Value: variant): TCHandle;
begin
  Result := Find(Value);
end;

procedure TTReal.InitContainer(c: TCCustom);
begin
  inherited;
  c.FCompareValue := CmpInt;
end;

function TTReal.CmpInt(var a,b):integer;
begin
  if TRealType(a)<TRealType(b) then
    result := -1
  else
    if TRealType(a)=TRealType(b) then
      result := 0
    else
      result := 1;
end;

procedure TTReal.SetOnCompare(c: TOnRealCompare);
begin
  if assigned(c) then
    container.FCompareValue := CmpExt
  else
    container.FCompareValue := CmpInt;
  FOnCompare := c;
  FOnCompareProc := nil;
end;

procedure TTReal.SetOnCompareProc(c: TOnRealCompareProc);
begin
  if assigned(c) then
    container.FCompareValue := CmpExtProc
  else
    container.FCompareValue := CmpInt;
  FOnCompareProc := c;
  FOnCompare := nil;
end;

function TTReal.CmpExt(var a, b): integer;
begin
  Result := FOnCompare(TRealType(a), TRealType(b));
end;

function TTReal.CmpExtProc(var a, b): integer;
begin
  Result := FOnCompareProc(TRealType(a), TRealType(b));
end;

procedure TTReal.Sort(compare: TOnRealCompare);
begin
  Sort(compare, FContainer.First, FContainer.Last);
end;

procedure TTReal.Sort(compare: TOnRealCompareProc);
begin
  Sort(compare, FContainer.First, FContainer.Last);
end;

procedure TTReal.Sort(compare: TOnRealCompare; AFirst, ALast: TCHandle);
begin
  FOnCompare := compare;
  FContainer.Sort(CmpExt, AFirst, ALast);
end;

procedure TTReal.Sort(compare: TOnRealCompareProc; AFirst, ALast: TCHandle);
begin
  FOnCompareProc := compare;
  FContainer.Sort(CmpExtProc, AFirst, ALast);
end;

function TTReal.STCopy(Dst: TTCustom; h: TCHandle):TCHandle;
begin
  result := TTReal(dst).Add(Items[h]);
end;

procedure TTReal.STCopy(Dst: TTCustom; AFirst, ALast: TCHandle);
begin
  TTReal(dst).Add(Items[AFirst]);
  if AFirst<>ALast then
    repeat
      FContainer.Next(AFirst);
      TTReal(dst).Add(Items[AFirst]);
    until AFirst=ALast;
end;

procedure TTReal.STSet(Dst: TTCustom; h1,h2: TCHandle);
begin
  TTReal(dst)[h2] := Items[h1];
end;

procedure TTReal.STSet(Dst: TTCustom; start1,finish1, start2: TCHandle);
begin
  TTReal(dst)[start2] := Items[start1];
  if start1<>finish1 then
    repeat
      FContainer.Next(start1);
      dst.FContainer.Next(start2);
      TTReal(dst)[start2] := Items[start1];
    until start1=finish1;
end;

function TTReal.GetMap(key: TRealType): variant;
begin
  Result := FContainer.Fields[1].AsVariant[Find(key)];
end;

procedure TTReal.PutMap(key: TRealType; Value: variant);
var
  h: TCHandle;
begin
  h := Find(key);
  if h = -1 then
    AddPair(key, Value)
  else
    FContainer.Fields[1].AsVariant[h] := Value;
end;

function TTReal.AddList(const Items: array of const): TCHandle;
var
  i: integer;
  p: PVarRec;
begin
  Result := -1;
  for i := low(items) to high(items) do
  begin
    p := @items[i];
    if p.VType = vtObject then
      Result := ImportObject(p.VObject, Result)
    else
      Result := add(VarAsDouble(p));
  end;
end;

function TTReal.Find(Item: TRealType): TCHandle;
begin
  Result := FContainer.Find(item);
end;

function TTReal.FindFirstEqual(Item: TRealType): TCHandle;
begin
  Result := FContainer.FindFirstEqual(item);
end;

procedure TTReal.FindNextEqual(Item: TRealType; var handle: TCHandle);
begin
  FContainer.FindNextEqual(item, handle);
end;

function TTReal.GetItem(n: TCHandle): TRealType;
begin
  Result := TRealType(FContainer.Get(n)^);
end;

function TTReal.GetMaxValue: TRealType;
begin
  Result := TRealType(FContainer.Get(FContainer.FindMax)^);
end;

function TTReal.RemoveMaxValue: TRealType;
var
  n: TCHandle;
begin
  n := FContainer.FindMax;
  Result := TRealType(FContainer.Get(n)^);
  FContainer.Delete(n);
end;

function TTReal.AddPair(key: TRealType; Value: variant): TCHandle;
begin
  result := Add(key);
  Container.Fields[1].AsVariant[result] := Value;
end;

function TTReal.GetMinValue: TRealType;
begin
  Result := TRealType(FContainer.Get(FContainer.FindMin)^);
end;

function TTReal.RemoveMinValue: TRealType;
var
  n: TCHandle;
begin
  n := FContainer.FindMin;
  Result := TRealType(FContainer.Get(n)^);
  FContainer.Delete(n);
end;

function TTReal.Insert(handle: TCHandle; Item: TRealType): TCHandle;
begin
  Result := FContainer.InsertItem(handle, item);
end;

function TTReal.InsertAfter(handle: TCHandle; Item: TRealType): TCHandle;
begin
  Result := FContainer.InsertAfterItem(handle, item);
end;

procedure TTReal.Remove(Item: TRealType);
begin
  FContainer.Remove(item);
end;

procedure TTReal.RemoveAll(Item: TRealType);
begin
  FContainer.RemoveAll(item);
end;

procedure TTReal.SetItem(n: TCHandle; Value: TRealType);
begin
  FContainer.Put(n, Value);
end;

// TTReal48
{$IFNDEF FPC}
procedure SortMemoryRangeReal48(Buf: pointer; L, R: Integer);
begin
  if R>L then
    VectorReal48QuickSort(@PReal48List(Buf)[L], R-L+1);
end;

constructor TTReal48.Create(Container: TCCustom = nil);
begin
  FieldSize := sizeof(TReal48Type);
  FSortMemoryRange := @SortMemoryRangeReal48;
  inherited;
end;

function TTReal48.Add(Item: TReal48Type): TCHandle;
begin
  Result := FContainer.AddItem(item);
end;

function  TTReal48.Front: TReal48Type;          // examine first element
begin
  result := items[FContainer.First];
end;

function  TTReal48.Back: TReal48Type;           // examine last element
begin
  result := items[FContainer.Last];
end;

function TTReal48.Push(v: TReal48Type):TCHandle;        // insert element at front
begin
  result := FContainer.InsertItem(FContainer.First, v);
end;

function  TTReal48.Pop: TReal48Type;            // remove first element
var
  h: TCHandle;
begin
  h := FContainer.First;
  Result := items[h];
  FContainer.Delete(h);
end;

function TTReal48.PushBack(v: TReal48Type):TCHandle;    // insert element at back
begin
  result := FContainer.AddItem(v);
end;

function  TTReal48.PopBack: TReal48Type;        // remove last element
var
  h: TCHandle;
begin
  h := FContainer.Last;
  result := items[h];
  FContainer.Delete(h);
end;

function TTReal48.Enqueue(v: TReal48Type):TCHandle;    // add to tail
begin
  result := FContainer.AddItem(v);
end;

function  TTReal48.Dequeue: TReal48Type;       // read&remove from head
var
  h: TCHandle;
begin
  h := FContainer.First;
  Result := items[h];
  FContainer.Delete(h);
end;

function TTReal48.AddTo(handle: TCHandle; dst: TTCustom): TCHandle;
begin
  Result := dst.Add([items[handle]]);
end;

function TTReal48.GetAsVariant(handle: TCHandle): variant;
begin
  if handle=-1 then
    Result := null
  else
    Result := items[handle];
end;

procedure TTReal48.SetAsVariant(handle: TCHandle; Value: variant);
begin
  items[handle] := Value;
end;

function TTReal48.GetAsAnsiStr(handle: TCHandle): AnsiString;
begin
  result := AnsiString(FloatToStr(items[handle]));
end;

procedure TTReal48.SetAsAnsiStr(handle: TCHandle; Value: AnsiString);
begin
  items[handle] := StrToFloat(String(value));
end;

function TTReal48.GetAsWideStr(handle: TCHandle): WideString;
begin
  result := FloatToStr(items[handle]);
end;

procedure TTReal48.SetAsWideStr(handle: TCHandle; Value: WideString);
begin
  items[handle] := StrToFloat(value);
end;

function TTReal48.GetHandleOf(Value: variant): TCHandle;
begin
  Result := Find(Value);
end;

procedure TTReal48.InitContainer(c: TCCustom);
begin
  inherited;
  c.FCompareValue := CmpInt;
end;

function TTReal48.CmpInt(var a,b):integer;
begin
  if TReal48Type(a)<TReal48Type(b) then
    result := -1
  else
    if TReal48Type(a)=TReal48Type(b) then
      result := 0
    else
      result := 1;
end;

procedure TTReal48.SetOnCompare(c: TOnReal48Compare);
begin
  if assigned(c) then
    container.FCompareValue := CmpExt
  else
    container.FCompareValue := CmpInt;
  FOnCompare := c;
  FOnCompareProc := nil;
end;

procedure TTReal48.SetOnCompareProc(c: TOnReal48CompareProc);
begin
  if assigned(c) then
    container.FCompareValue := CmpExtProc
  else
    container.FCompareValue := CmpInt;
  FOnCompareProc := c;
  FOnCompare := nil;
end;

function TTReal48.CmpExt(var a, b): integer;
begin
  Result := FOnCompare(TReal48Type(a), TReal48Type(b));
end;

function TTReal48.CmpExtProc(var a, b): integer;
begin
  Result := FOnCompareProc(TReal48Type(a), TReal48Type(b));
end;

procedure TTReal48.Sort(compare: TOnReal48Compare);
begin
  Sort(compare, FContainer.First, FContainer.Last);
end;

procedure TTReal48.Sort(compare: TOnReal48CompareProc);
begin
  Sort(compare, FContainer.First, FContainer.Last);
end;

procedure TTReal48.Sort(compare: TOnReal48Compare; AFirst, ALast: TCHandle);
begin
  FOnCompare := compare;
  FContainer.Sort(CmpExt, AFirst, ALast);
end;

procedure TTReal48.Sort(compare: TOnReal48CompareProc; AFirst, ALast: TCHandle);
begin
  FOnCompareProc := compare;
  FContainer.Sort(CmpExtProc, AFirst, ALast);
end;

function TTReal48.STCopy(Dst: TTCustom; h: TCHandle):TCHandle;
begin
  result := TTReal48(dst).Add(Items[h]);
end;

procedure TTReal48.STCopy(Dst: TTCustom; AFirst, ALast: TCHandle);
begin
  TTReal48(dst).Add(Items[AFirst]);
  if AFirst<>ALast then
    repeat
      FContainer.Next(AFirst);
      TTReal48(dst).Add(Items[AFirst]);
    until AFirst=ALast;
end;

procedure TTReal48.STSet(Dst: TTCustom; h1,h2: TCHandle);
begin
  TTReal48(dst)[h2] := Items[h1];
end;

procedure TTReal48.STSet(Dst: TTCustom; start1,finish1, start2: TCHandle);
begin
  TTReal48(dst)[start2] := Items[start1];
  if start1<>finish1 then
    repeat
      FContainer.Next(start1);
      dst.FContainer.Next(start2);
      TTReal48(dst)[start2] := Items[start1];
    until start1=finish1;
end;

function TTReal48.GetMap(key: TReal48Type): variant;
begin
  Result := FContainer.Fields[1].AsVariant[Find(key)];
end;

procedure TTReal48.PutMap(key: TReal48Type; Value: variant);
var
  h: TCHandle;
begin
  h := Find(key);
  if h = -1 then
    AddPair(key, Value)
  else
    FContainer.Fields[1].AsVariant[h] := Value;
end;

function TTReal48.AddList(const Items: array of const): TCHandle;
var
  i: integer;
  p: PVarRec;
begin
  Result := -1;
  for i := low(items) to high(items) do
  begin
    p := @items[i];
    if p.VType = vtObject then
      Result := ImportObject(p.VObject, Result)
    else
      Result := add(VarAsDouble(p));
  end;
end;

function TTReal48.Find(Item: TReal48Type): TCHandle;
begin
  Result := FContainer.Find(item);
end;

function TTReal48.FindFirstEqual(Item: TReal48Type): TCHandle;
begin
  Result := FContainer.FindFirstEqual(item);
end;

procedure TTReal48.FindNextEqual(Item: TReal48Type; var handle: TCHandle);
begin
  FContainer.FindNextEqual(item, handle);
end;

function TTReal48.GetItem(n: TCHandle): TReal48Type;
begin
  Result := TReal48Type(FContainer.Get(n)^);
end;

function TTReal48.GetMaxValue: TReal48Type;
begin
  Result := TReal48Type(FContainer.Get(FContainer.FindMax)^);
end;

function TTReal48.RemoveMaxValue: TReal48Type;
var
  n: TCHandle;
begin
  n := FContainer.FindMax;
  Result := TReal48Type(FContainer.Get(n)^);
  FContainer.Delete(n);
end;

function TTReal48.AddPair(key: TReal48Type; Value: variant): TCHandle;
begin
  result := Add(key);
  Container.Fields[1].AsVariant[result] := Value;
end;

function TTReal48.GetMinValue: TReal48Type;
begin
  Result := TReal48Type(FContainer.Get(FContainer.FindMin)^);
end;

function TTReal48.RemoveMinValue: TReal48Type;
var
  n: TCHandle;
begin
  n := FContainer.FindMin;
  Result := TReal48Type(FContainer.Get(n)^);
  FContainer.Delete(n);
end;

function TTReal48.Insert(handle: TCHandle; Item: TReal48Type): TCHandle;
begin
  Result := FContainer.InsertItem(handle, item);
end;

function TTReal48.InsertAfter(handle: TCHandle; Item: TReal48Type): TCHandle;
begin
  Result := FContainer.InsertAfterItem(handle, item);
end;

procedure TTReal48.Remove(Item: TReal48Type);
begin
  FContainer.Remove(item);
end;

procedure TTReal48.RemoveAll(Item: TReal48Type);
begin
  FContainer.RemoveAll(item);
end;

procedure TTReal48.SetItem(n: TCHandle; Value: TReal48Type);
begin
  FContainer.Put(n, Value);
end;
{$ENDIF}

// TTComp

procedure SortMemoryRangeComp(Buf: pointer; L, R: Integer);
begin
  if R>L then
    VectorCompQuickSort(@PCompList(Buf)[L], R-L+1);
end;

constructor TTComp.Create(Container: TCCustom = nil);
begin
  FieldSize := sizeof(TCompType);
  FSortMemoryRange := @SortMemoryRangeComp;
  inherited;
end;

function TTComp.Add(Item: TCompType): TCHandle;
begin
  Result := FContainer.AddItem(item);
end;

function  TTComp.Front: TCompType;          // examine first element
begin
  result := items[FContainer.First];
end;

function  TTComp.Back: TCompType;           // examine last element
begin
  result := items[FContainer.Last];
end;

function TTComp.Push(v: TCompType):TCHandle;        // insert element at front
begin
  result := FContainer.InsertItem(FContainer.First, v);
end;

function  TTComp.Pop: TCompType;            // remove first element
var
  h: TCHandle;
begin
  h := FContainer.First;
  Result := items[h];
  FContainer.Delete(h);
end;

function TTComp.PushBack(v: TCompType):TCHandle;    // insert element at back
begin
  result := FContainer.AddItem(v);
end;

function  TTComp.PopBack: TCompType;        // remove last element
var
  h: TCHandle;
begin
  h := FContainer.Last;
  result := items[h];
  FContainer.Delete(h);
end;

function TTComp.Enqueue(v: TCompType):TCHandle;    // add to tail
begin
  result := FContainer.AddItem(v);
end;

function  TTComp.Dequeue: TCompType;       // read&remove from head
var
  h: TCHandle;
begin
  h := FContainer.First;
  Result := items[h];
  FContainer.Delete(h);
end;

function TTComp.AddTo(handle: TCHandle; dst: TTCustom): TCHandle;
begin
  Result := dst.Add([items[handle]]);
end;

function TTComp.GetAsVariant(handle: TCHandle): variant;
begin
  if handle=-1 then
    Result := null
  else
    Result := items[handle];
end;

procedure TTComp.SetAsVariant(handle: TCHandle; Value: variant);
begin
  items[handle] := Value;
end;

function TTComp.GetAsAnsiStr(handle: TCHandle): AnsiString;
begin
  result := AnsiString(FloatToStr(items[handle]));
end;

procedure TTComp.SetAsAnsiStr(handle: TCHandle; Value: AnsiString);
begin
  items[handle] := StrToInt64(String(value));
end;

function TTComp.GetAsWideStr(handle: TCHandle): WideString;
begin
  result := FloatToStr(items[handle]);
end;

procedure TTComp.SetAsWideStr(handle: TCHandle; Value: WideString);
begin
  items[handle] := StrToInt64(value);
end;

function TTComp.GetHandleOf(Value: variant): TCHandle;
begin
  Result := Find(Value);
end;

procedure TTComp.InitContainer(c: TCCustom);
begin
  inherited;
  c.FCompareValue := CmpInt;
end;

function TTComp.CmpInt(var a,b):integer;
begin
  if TCompType(a)<TCompType(b) then
    result := -1
  else
    if TCompType(a)=TCompType(b) then
      result := 0
    else
      result := 1;
end;

procedure TTComp.SetOnCompare(c: TOnCompCompare);
begin
  if assigned(c) then
    container.FCompareValue := CmpExt
  else
    container.FCompareValue := CmpInt;
  FOnCompare := c;
  FOnCompareProc := nil;
end;

procedure TTComp.SetOnCompareProc(c: TOnCompCompareProc);
begin
  if assigned(c) then
    container.FCompareValue := CmpExtProc
  else
    container.FCompareValue := CmpInt;
  FOnCompareProc := c;
  FOnCompare := nil;
end;

function TTComp.CmpExt(var a, b): integer;
begin
  Result := FOnCompare(TCompType(a), TCompType(b));
end;

function TTComp.CmpExtProc(var a, b): integer;
begin
  Result := FOnCompareProc(TCompType(a), TCompType(b));
end;

procedure TTComp.Sort(compare: TOnCompCompare);
begin
  Sort(compare, FContainer.First, FContainer.Last);
end;

procedure TTComp.Sort(compare: TOnCompCompareProc);
begin
  Sort(compare, FContainer.First, FContainer.Last);
end;

procedure TTComp.Sort(compare: TOnCompCompare; AFirst, ALast: TCHandle);
begin
  FOnCompare := compare;
  FContainer.Sort(CmpExt, AFirst, ALast);
end;

procedure TTComp.Sort(compare: TOnCompCompareProc; AFirst, ALast: TCHandle);
begin
  FOnCompareProc := compare;
  FContainer.Sort(CmpExtProc, AFirst, ALast);
end;

function TTComp.STCopy(Dst: TTCustom; h: TCHandle):TCHandle;
begin
  result := TTComp(dst).Add(Items[h]);
end;

procedure TTComp.STCopy(Dst: TTCustom; AFirst, ALast: TCHandle);
begin
  TTComp(dst).Add(Items[AFirst]);
  if AFirst<>ALast then
    repeat
      FContainer.Next(AFirst);
      TTComp(dst).Add(Items[AFirst]);
    until AFirst=ALast;
end;

procedure TTComp.STSet(Dst: TTCustom; h1,h2: TCHandle);
begin
  TTComp(dst)[h2] := Items[h1];
end;

procedure TTComp.STSet(Dst: TTCustom; start1,finish1, start2: TCHandle);
begin
  TTComp(dst)[start2] := Items[start1];
  if start1<>finish1 then
    repeat
      FContainer.Next(start1);
      dst.FContainer.Next(start2);
      TTComp(dst)[start2] := Items[start1];
    until start1=finish1;
end;

function TTComp.GetMap(key: TCompType): variant;
begin
  Result := FContainer.Fields[1].AsVariant[Find(key)];
end;

procedure TTComp.PutMap(key: TCompType; Value: variant);
var
  h: TCHandle;
begin
  h := Find(key);
  if h = -1 then
    AddPair(key, Value)
  else
    FContainer.Fields[1].AsVariant[h] := Value;
end;

function TTComp.AddList(const Items: array of const): TCHandle;
var
  i: integer;
  p: PVarRec;
begin
  Result := -1;
  for i := low(items) to high(items) do
  begin
    p := @items[i];
    if p.VType = vtObject then
      Result := ImportObject(p.VObject, Result)
    else
      Result := add(VarAsInt64(p));
  end;
end;

function TTComp.Find(Item: TCompType): TCHandle;
begin
  Result := FContainer.Find(item);
end;

function TTComp.FindFirstEqual(Item: TCompType): TCHandle;
begin
  Result := FContainer.FindFirstEqual(item);
end;

procedure TTComp.FindNextEqual(Item: TCompType; var handle: TCHandle);
begin
  FContainer.FindNextEqual(item, handle);
end;

function TTComp.GetItem(n: TCHandle): TCompType;
begin
  Result := TCompType(FContainer.Get(n)^);
end;

function TTComp.GetMaxValue: TCompType;
begin
  Result := TCompType(FContainer.Get(FContainer.FindMax)^);
end;

function TTComp.RemoveMaxValue: TCompType;
var
  n: TCHandle;
begin
  n := FContainer.FindMax;
  Result := TCompType(FContainer.Get(n)^);
  FContainer.Delete(n);
end;

function TTComp.AddPair(key: TCompType; Value: variant): TCHandle;
begin
  result := Add(key);
  Container.Fields[1].AsVariant[result] := Value;
end;

function TTComp.GetMinValue: TCompType;
begin
  Result := TCompType(FContainer.Get(FContainer.FindMin)^);
end;

function TTComp.RemoveMinValue: TCompType;
var
  n: TCHandle;
begin
  n := FContainer.FindMin;
  Result := TCompType(FContainer.Get(n)^);
  FContainer.Delete(n);
end;

function TTComp.Insert(handle: TCHandle; Item: TCompType): TCHandle;
begin
  Result := FContainer.InsertItem(handle, item);
end;

function TTComp.InsertAfter(handle: TCHandle; Item: TCompType): TCHandle;
begin
  Result := FContainer.InsertAfterItem(handle, item);
end;

procedure TTComp.Remove(Item: TCompType);
begin
  FContainer.Remove(item);
end;

procedure TTComp.RemoveAll(Item: TCompType);
begin
  FContainer.RemoveAll(item);
end;

procedure TTComp.SetItem(n: TCHandle; Value: TCompType);
begin
  FContainer.Put(n, Value);
end;

// TTCurrency

procedure SortMemoryRangeCurrency(Buf: pointer; L, R: Integer);
begin
  if R>L then
    VectorCurrencyQuickSort(@PCurrencyList(Buf)[L], R-L+1);
end;

constructor TTCurrency.Create(Container: TCCustom = nil);
begin
  FieldSize := sizeof(TCurrencyType);
  FSortMemoryRange := @SortMemoryRangeCurrency;
  inherited;
end;

function TTCurrency.Add(Item: TCurrencyType): TCHandle;
begin
  Result := FContainer.AddItem(item);
end;

function  TTCurrency.Front: TCurrencyType;          // examine first element
begin
  result := items[FContainer.First];
end;

function  TTCurrency.Back: TCurrencyType;           // examine last element
begin
  result := items[FContainer.Last];
end;

function TTCurrency.Push(v: TCurrencyType):TCHandle;        // insert element at front
begin
  result := FContainer.InsertItem(FContainer.First, v);
end;

function  TTCurrency.Pop: TCurrencyType;            // remove first element
var
  h: TCHandle;
begin
  h := FContainer.First;
  Result := items[h];
  FContainer.Delete(h);
end;

function TTCurrency.PushBack(v: TCurrencyType):TCHandle;    // insert element at back
begin
  result := FContainer.AddItem(v);
end;

function  TTCurrency.PopBack: TCurrencyType;        // remove last element
var
  h: TCHandle;
begin
  h := FContainer.Last;
  result := items[h];
  FContainer.Delete(h);
end;

function TTCurrency.Enqueue(v: TCurrencyType):TCHandle;    // add to tail
begin
  result := FContainer.AddItem(v);
end;

function  TTCurrency.Dequeue: TCurrencyType;       // read&remove from head
var
  h: TCHandle;
begin
  h := FContainer.First;
  Result := items[h];
  FContainer.Delete(h);
end;

function TTCurrency.AddTo(handle: TCHandle; dst: TTCustom): TCHandle;
begin
  Result := dst.Add([items[handle]]);
end;

function TTCurrency.GetAsVariant(handle: TCHandle): variant;
begin
  if handle=-1 then
    Result := null
  else
    Result := items[handle];
end;

procedure TTCurrency.SetAsVariant(handle: TCHandle; Value: variant);
begin
  items[handle] := Value;
end;

function TTCurrency.GetAsAnsiStr(handle: TCHandle): AnsiString;
begin
  result := AnsiString(FloatToStr(items[handle]));
end;

procedure TTCurrency.SetAsAnsiStr(handle: TCHandle; Value: AnsiString);
begin
  items[handle] := StrToFloat(String(value));
end;

function TTCurrency.GetAsWideStr(handle: TCHandle): WideString;
begin
  result := FloatToStr(items[handle]);
end;

procedure TTCurrency.SetAsWideStr(handle: TCHandle; Value: WideString);
begin
  items[handle] := StrToFloat(value);
end;

function TTCurrency.GetHandleOf(Value: variant): TCHandle;
begin
  Result := Find(Value);
end;

procedure TTCurrency.InitContainer(c: TCCustom);
begin
  inherited;
  c.FCompareValue := CmpInt;
end;

function TTCurrency.CmpInt(var a,b):integer;
begin
  if TCurrencyType(a)<TCurrencyType(b) then
    result := -1
  else
    if TCurrencyType(a)=TCurrencyType(b) then
      result := 0
    else
      result := 1;
end;

procedure TTCurrency.SetOnCompare(c: TOnCurrencyCompare);
begin
  if assigned(c) then
    container.FCompareValue := CmpExt
  else
    container.FCompareValue := CmpInt;
  FOnCompare := c;
  FOnCompareProc := nil;
end;

procedure TTCurrency.SetOnCompareProc(c: TOnCurrencyCompareProc);
begin
  if assigned(c) then
    container.FCompareValue := CmpExtProc
  else
    container.FCompareValue := CmpInt;
  FOnCompareProc := c;
  FOnCompare := nil;
end;

function TTCurrency.CmpExt(var a, b): integer;
begin
  Result := FOnCompare(TCurrencyType(a), TCurrencyType(b));
end;

function TTCurrency.CmpExtProc(var a, b): integer;
begin
  Result := FOnCompareProc(TCurrencyType(a), TCurrencyType(b));
end;

procedure TTCurrency.Sort(compare: TOnCurrencyCompare);
begin
  Sort(compare, FContainer.First, FContainer.Last);
end;

procedure TTCurrency.Sort(compare: TOnCurrencyCompareProc);
begin
  Sort(compare, FContainer.First, FContainer.Last);
end;

procedure TTCurrency.Sort(compare: TOnCurrencyCompare; AFirst, ALast: TCHandle);
begin
  FOnCompare := compare;
  FContainer.Sort(CmpExt, AFirst, ALast);
end;

procedure TTCurrency.Sort(compare: TOnCurrencyCompareProc; AFirst, ALast: TCHandle);
begin
  FOnCompareProc := compare;
  FContainer.Sort(CmpExtProc, AFirst, ALast);
end;

function TTCurrency.STCopy(Dst: TTCustom; h: TCHandle):TCHandle;
begin
  result := TTCurrency(dst).Add(Items[h]);
end;

procedure TTCurrency.STCopy(Dst: TTCustom; AFirst, ALast: TCHandle);
begin
  TTCurrency(dst).Add(Items[AFirst]);
  if AFirst<>ALast then
    repeat
      FContainer.Next(AFirst);
      TTCurrency(dst).Add(Items[AFirst]);
    until AFirst=ALast;
end;

procedure TTCurrency.STSet(Dst: TTCustom; h1,h2: TCHandle);
begin
  TTCurrency(dst)[h2] := Items[h1];
end;

procedure TTCurrency.STSet(Dst: TTCustom; start1,finish1, start2: TCHandle);
begin
  TTCurrency(dst)[start2] := Items[start1];
  if start1<>finish1 then
    repeat
      FContainer.Next(start1);
      dst.FContainer.Next(start2);
      TTCurrency(dst)[start2] := Items[start1];
    until start1=finish1;
end;

function TTCurrency.GetMap(key: TCurrencyType): variant;
begin
  Result := FContainer.Fields[1].AsVariant[Find(key)];
end;

procedure TTCurrency.PutMap(key: TCurrencyType; Value: variant);
var
  h: TCHandle;
begin
  h := Find(key);
  if h = -1 then
    AddPair(key, Value)
  else
    FContainer.Fields[1].AsVariant[h] := Value;
end;

function TTCurrency.AddList(const Items: array of const): TCHandle;
var
  i: integer;
  p: PVarRec;
begin
  Result := -1;
  for i := low(items) to high(items) do
  begin
    p := @items[i];
    if p.VType = vtObject then
      Result := ImportObject(p.VObject, Result)
    else
      Result := add(VarAsDouble(p));
  end;
end;

function TTCurrency.Find(Item: TCurrencyType): TCHandle;
begin
  Result := FContainer.Find(item);
end;

function TTCurrency.FindFirstEqual(Item: TCurrencyType): TCHandle;
begin
  Result := FContainer.FindFirstEqual(item);
end;

procedure TTCurrency.FindNextEqual(Item: TCurrencyType; var handle: TCHandle);
begin
  FContainer.FindNextEqual(item, handle);
end;

function TTCurrency.GetItem(n: TCHandle): TCurrencyType;
begin
  Result := TCurrencyType(FContainer.Get(n)^);
end;

function TTCurrency.GetMaxValue: TCurrencyType;
begin
  Result := TCurrencyType(FContainer.Get(FContainer.FindMax)^);
end;

function TTCurrency.RemoveMaxValue: TCurrencyType;
var
  n: TCHandle;
begin
  n := FContainer.FindMax;
  Result := TCurrencyType(FContainer.Get(n)^);
  FContainer.Delete(n);
end;

function TTCurrency.AddPair(key: TCurrencyType; Value: variant): TCHandle;
begin
  result := Add(key);
  Container.Fields[1].AsVariant[result] := Value;
end;

function TTCurrency.GetMinValue: TCurrencyType;
begin
  Result := TCurrencyType(FContainer.Get(FContainer.FindMin)^);
end;

function TTCurrency.RemoveMinValue: TCurrencyType;
var
  n: TCHandle;
begin
  n := FContainer.FindMin;
  Result := TCurrencyType(FContainer.Get(n)^);
  FContainer.Delete(n);
end;

function TTCurrency.Insert(handle: TCHandle; Item: TCurrencyType): TCHandle;
begin
  Result := FContainer.InsertItem(handle, item);
end;

function TTCurrency.InsertAfter(handle: TCHandle; Item: TCurrencyType): TCHandle;
begin
  Result := FContainer.InsertAfterItem(handle, item);
end;

procedure TTCurrency.Remove(Item: TCurrencyType);
begin
  FContainer.Remove(item);
end;

procedure TTCurrency.RemoveAll(Item: TCurrencyType);
begin
  FContainer.RemoveAll(item);
end;

procedure TTCurrency.SetItem(n: TCHandle; Value: TCurrencyType);
begin
  FContainer.Put(n, Value);
end;

function SortString(Const s: string): string;
begin
  result := s;
  if result<>'' then
    VectorCharQuickSort(@result[1], length(result));
end;

function SortAnsiString(Const s: AnsiString): AnsiString;
begin
  result := s;
  if result<>'' then
    VectorAnsiCharQuickSort(@result[1], length(result));
end;

function SortWideString(Const s: WideString): WideString;
begin
  result := s;
  if result<>'' then
    VectorWideCharQuickSort(@result[1], length(result));
end;

function CharInString(C: Char; const S: String): boolean;
var f,l,m: integer;
begin
  result := s<>'';
  if not result then
    exit;
  f := 1;
  l := length(s);
  while (l-f>1) do
  begin
    m := (f+l) shr 1;
    if C<=S[m] then
      l := m
    else
      f := m;  
  end;
  result := (s[f]=c) or (s[l]=c);
end;

function CharInAnsiString(C: AnsiChar; const S: AnsiString): boolean;
var f,l,m: integer;
begin
  result := s<>'';
  if not result then
    exit;
  f := 1;
  l := length(s);
  while (l-f>1) do
  begin
    m := (f+l) shr 1;
    if C<=S[m] then
      l := m
    else
      f := m;
  end;
  result := (s[f]=c) or (s[l]=c);
end;

function CharInWideString(C: WideChar; const S: WideString): boolean;
var f,l,m: integer;
begin
  result := s<>'';
  if not result then
    exit;
  f := 1;
  l := length(s);
  while (l-f>1) do
  begin
    m := (f+l) shr 1;
    if C<=S[m] then
      l := m
    else
      f := m;
  end;
  result := (s[f]=c) or (s[l]=c);
end;

function SetToAnsiString(Const S: TAnsiSet): AnsiString;
var i: AnsiChar;
begin
  result := '';
  for i := low(i) to high(i) do
    if i in S then
      result := result + i;
end;

initialization
  IntSymbols := SortString('+-0123456789');
  FloatSymbols := SortString('+-0123456789eE'+DecimalSeparator);
  IntSymbolsA := SortAnsiString('+-0123456789');
  FloatSymbolsA := SortAnsiString('+-0123456789eE'+AnsiString(DecimalSeparator));
  IntSymbolsW := SortWideString('+-0123456789');
  FloatSymbolsW := SortWideString('+-0123456789eE'+DecimalSeparator);

end.
