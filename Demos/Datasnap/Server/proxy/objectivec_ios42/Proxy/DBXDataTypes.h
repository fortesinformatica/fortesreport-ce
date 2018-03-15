//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

#import <Foundation/Foundation.h>
 /**
 * Those are the types supported by the Datasnap server.

 */
typedef enum  {
	// / <summary>Data types supported by DBX.</summary>
	
	// /<summary></summary>
	UnknownType = 0,	
	// /<summary>8 bit Ansi String</summary>
	AnsiStringType = 1,
	// /<summary>32 bit Date</summary>
	DateType = 2,
	// /<summary>Blob with a subtype</summary>
	BlobType = 3,
	// /<summary>16 big Boolean</summary>
	BooleanType = 4,
	// /<summary>16 bit signed integer</summary>
	Int16Type = 5,
	// /<summary>32 bit signed integer</summary>
	Int32Type = 6,
	// /<summary>64 bit floating point</summary>
	DoubleType = 7,
	// /<summary>TBcd decimal from the FMTBcd unit</summary>
	BcdType = 8, /* { BCD } */
	// /<summary>Fixed length byte array</summary>
	BytesType = 9,
	// /<summary>32 bit Time</summary>
	TimeType = 10,
	// /<summary>TDateTime
	// / Internally managed as a <c>TDBXDataTypes.TimeStampType</c>
	// /</summary>
	DateTimeType = 11, /* { Time-stamp (64 bit) } */
	// /<summary>Unsigned 16 bit integer</summary>
	UInt16Type = 12,
	// /<summary>Unsigned 32 bit integer</summary>
	UInt32Type = 13,
	// ///<summary></summary>
	// FLOATIEEE = 14; { 80-bit IEEE float }
	// /<summary>Variable length byte array with maximum length of 64
	// kilobytes</summary>
	VarBytesType = 15,
	// ///<summary></summary>
	// LOCKINFO = 16; { Look for LOCKINFO typedef }
	// /<summary>Oracle cursor type</summary>
	CursorType = 17,
	// /<summary>64 bit integer</summary>
	Int64Type = 18,
	// /<summary>unsigned 64 bit integer</summary>
	UInt64Type = 19,
	// /<summary>Abstract data type</summary>
	AdtType = 20,
	// /<summary>Array data type</summary>
	ArrayType = 21,
	// /<summary>Reference data type</summary>
	RefType = 22,
	// /<summary>Nested table data type</summary>
	TableType = 23,
	// /<summary>TSQLTimeStamp in the SqlTimSt unit</summary>
	TimeStampType = 24,
	// /<summary>Delphi Currency data type in System unit.
	// / Internally managed as a <c>TDBXDataTypes.BCDType</c>
	// /</summary>
	CurrencyType = 25,
	// /<summary>UCS2 unicode string</summary>
	WideStringType = 26,
	
	// /<summary>32 bit floating point</summary>
	SingleType = 27,
	
	// /<summary>8 bit signed integer</summary>
	Int8Type = 28,
	// /<summary>8 bit unsigned integer</summary>
	UInt8Type = 29,
	// /<summary>Object serialization</summary>
	ObjectType = 30,
	// /<summary>Character array</summary>
	CharArrayType = 31,
	// /<summary>Time Interval</summary>
	IntervalType = 32,
	// /<summary>BinaryBlobType equivalent to <c>BlobType</c> with a
	// /<c>BinarySubType</c> sub type/
	// /</summary>
	BinaryBlobType = 33,
	// /<summary>
	// / DBXConnection type for DataSnap server methods that receive or set the
	// server side
	// / connection.
	// /</summary>
	DBXConnectionType = 34,
	// /<summary>
	// / Variant out or return parameter. Not supported as a TDBXReader column.
	// /</summary>
	VariantType = 35,
	TimeStampOffsetType = 36,
	// /<summary>DBX type for a JSON value
	// /</summary>
	JsonValueType = 37,
	// /<summary>
	// /DBX type for a callback argument
	// /</summary>
	CallbackType = 38,
	// /<summary>Maximum number of base types excluding sub types that
	// / are supported by TDataSet type system.
	// /</summary>
	MaxBaseTypes = 39,
	
} DBXDataTypes;


