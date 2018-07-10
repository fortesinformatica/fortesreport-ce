//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

namespace Embarcadero.Datasnap.WindowsPhone7
{
    /**
     *    Data types supported by DBX.
     */
    class DBXDataTypes
    {
	 
	    public const int UnknownType = 0;
	    
        /**
        *8 bit Ansi String
        */
	    public const int AnsiStringType = 1;
	    
        /**
        *32 bit Date
        */
	    public const int DateType = 2;
	    
        /**
        *Blob with a subtype
        */
	    public const int BlobType = 3;
	    
        /**
        *Boolean
        */
	    public const int BooleanType = 4;
	    
        /**
        *16 bit signed integer
        */
	    public const int Int16Type = 5;
	    
        /**
         *32 bit signed integer
         */
	    public const int Int32Type = 6;
	    
        /**
         *64 bit floating point
         */
	    public const int DoubleType = 7;
	    
        /**
         *TBcd decimal
        */
	    public const int BcdType = 8; /* { BCD } */
	    
        /**
        *Fixed length byte array
        */
	    public const int BytesType = 9;
	    
        /**
         *32 bit Time
         */         
	    public const int TimeType = 10;
	    
        /**
         *TDateTime
	    */
	    public const int DateTimeType = 11;
	    
        /**
        *Unsigned 16 bit integer
        */
	    public const int UInt16Type = 12;
	    
        /**
        *Unsigned 32 bit integer
        */
	    public const int UInt32Type = 13;
	    
	    /**
	     * Variable length byte array with maximum length of 64 kilobytes
	     */         	    
	    public const int VarBytesType = 15;
	    
	    /**
	     *Oracle cursor type
	     */
	    public const int CursorType = 17;
	    
        /**
         *64 bit integer
         */
	    public const int Int64Type = 18;
	    
        /**
         * unsigned 64 bit integer
         */
	    public const int UInt64Type = 19;
	    
        /**
         *Abstract data type
         */
	    public const int AdtType = 20;
	    
        /**
         *Array data type
         */
	    public const int ArrayType = 21;
	    
        /**
         *Reference data type
         */
	    public const int RefType = 22;
	    
        /**
        * Nested table data type
        */
	    public const int TableType = 23;
	    
        /**
        *TSQLTimeStamp in the SqlTimSt unit
        */
	    public const int TimeStampType = 24;
	    /**
	     *Delphi Currency data type in System unit.
	     */
	    public const int CurrencyType = 25;
	    
        /**
        *UCS2 unicode string
        */
	    public const int WideStringType = 26;

	    /**
	     *32 bit floating point
	     */
	    public const int SingleType = 27;

	    /**
	     *8 bit signed integer
	     */
	    public const int Int8Type = 28;
	    
        /**
         *8 bit unsigned integer
         */
        public const int UInt8Type = 29;
	    
        /**
         *Object serialization
         */
	    public const int ObjectType = 30;
	    
        /**
         *Character array
         */
	    public const int CharArrayType = 31;
	    
        /**
         *Time Interval
         */
	    public const int IntervalType = 32;
	    
        /**
         *BinaryBlobType equivalent to <c>BlobType</c> with a
	    *<c>BinarySubType</c> sub type
	    */
	    public const int BinaryBlobType = 33;
	    
        /**
	    *DBXConnection type for DataSnap server methods that receive or set the
	    *server side
	    *connection.
	    */
	    public const int DBXConnectionType = 34;
	    /**
	    *Variant out or return parameter. Not supported as a TDBXReader column.
	    */
	    public const int VariantType = 35;
	    public const int TimeStampOffsetType = 36;
	    
        /**
         *DBX type for a JSON value
	    */
	    public const int JsonValueType = 37;
	    
        /**
	    *DBX type for a callback argument
	    */
	    public const int CallbackType = 38;
	    
        /**
        *Maximum number of base types excluding sub types that
	    *are supported by TDataSet type system.
	    */
        public const int MaxBaseTypes = 39;
    }
}
