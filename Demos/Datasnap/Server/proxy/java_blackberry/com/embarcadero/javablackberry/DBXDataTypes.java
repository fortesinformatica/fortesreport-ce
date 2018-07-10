//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

package com.embarcadero.javablackberry;

/**
 * Represents the DBXDelphi types wrapped.
 * 
 */

public class DBXDataTypes {
	// / <summary>Data types supported by DBX.</summary>

	// /<summary></summary>
	static public final int UnknownType = 0;
	// /<summary>8 bit Ansi String</summary>
	static public final int AnsiStringType = 1;
	// /<summary>32 bit Date</summary>
	static public final int DateType = 2;
	// /<summary>Blob with a subtype</summary>
	static public final int BlobType = 3;
	// /<summary>16 big Boolean</summary>
	static public final int BooleanType = 4;
	// /<summary>16 bit signed integer</summary>
	static public final int Int16Type = 5;
	// /<summary>32 bit signed integer</summary>
	static public final int Int32Type = 6;
	// /<summary>64 bit floating point</summary>
	static public final int DoubleType = 7;
	// /<summary>TBcd decimal from the FMTBcd unit</summary>
	static public final int BcdType = 8; /* { BCD } */
	// /<summary>Fixed length byte array</summary>
	static public final int BytesType = 9;
	// /<summary>32 bit Time</summary>
	static public final int TimeType = 10;
	// /<summary>TDateTime
	// / Internally managed as a <c>TDBXDataTypes.TimeStampType</c>
	// /</summary>
	static public final int DateTimeType = 11; /* { Time-stamp (64 bit) } */
	// /<summary>Unsigned 16 bit integer</summary>
	static public final int UInt16Type = 12;
	// /<summary>Unsigned 32 bit integer</summary>
	static public final int UInt32Type = 13;
	// /<summary>Variable length byte array with maximum length of 64
	// kilobytes</summary>
	static public final int VarBytesType = 15;
	// /<summary>Oracle cursor type</summary>
	static public final int CursorType = 17;
	// /<summary>64 bit integer</summary>
	static public final int Int64Type = 18;
	// /<summary>unsigned 64 bit integer</summary>
	static public final int UInt64Type = 19;
	// /<summary>Abstract data type</summary>
	static public final int AdtType = 20;
	// /<summary>Array data type</summary>
	static public final int ArrayType = 21;
	// /<summary>Reference data type</summary>
	static public final int RefType = 22;
	// /<summary>Nested table data type</summary>
	static public final int TableType = 23;
	// /<summary>TSQLTimeStamp in the SqlTimSt unit</summary>
	static public final int TimeStampType = 24;
	// /<summary>Delphi Currency data type in System unit.
	// / Internally managed as a <c>TDBXDataTypes.BCDType</c>
	// /</summary>
	static public final int CurrencyType = 25;
	// /<summary>UCS2 unicode string</summary>
	static public final int WideStringType = 26;

	// /<summary>32 bit floating point</summary>
	static public final int SingleType = 27;

	// /<summary>8 bit signed integer</summary>
	static public final int Int8Type = 28;
	// /<summary>8 bit unsigned integer</summary>
	static public final int UInt8Type = 29;
	// /<summary>Object serialization</summary>
	static public final int ObjectType = 30;
	// /<summary>Character array</summary>
	static public final int CharArrayType = 31;
	// /<summary>Time Interval</summary>
	static public final int IntervalType = 32;
	// /<summary>BinaryBlobType equivalent to <c>BlobType</c> with a
	// /<c>BinarySubType</c> sub type/
	// /</summary>
	static public final int BinaryBlobType = 33;
	// /<summary>
	// / DBXConnection type for DataSnap server methods that receive or set the
	// server side
	// / connection.
	// /</summary>
	static public final int DBXConnectionType = 34;
	// /<summary>
	// / Variant out or return parameter. Not supported as a TDBXReader column.
	// /</summary>
	static public final int VariantType = 35;
	static public final int TimeStampOffsetType = 36;
	// /<summary>DBX type for a JSON value
	// /</summary>
	static public final int JsonValueType = 37;
	// /<summary>
	// /DBX type for a callback argument
	// /</summary>
	static public final int CallbackType = 38;
	// /<summary>Maximum number of base types excluding sub types that
	// / are supported by TDataSet type system.
	// /</summary>
	static public final int MaxBaseTypes = 39;

}