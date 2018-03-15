//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

using System;

namespace Embarcadero.Datasnap.WindowsPhone7
{

    /**
     * This class extends DBXValue and overrides the setter and getter methods of
     * DBXValue to allows to set and get specified types
     */
    public class DBXWritableValue : DBXValue {

        /**
        * It clears internal structure with defaults. 
        */
	    
        public override void Clear() {    
		    booleanValue = false;
		    INT32Value = 0;
		    UINT32Value = 0;
		    INT64Value = 0;
		    UINT64Value = 0;
		    stringValue = "";
		    singleValue = 0;
		    doubleValue = 0;
		    dateTimeValue = DateTime.MinValue;
		    streamValue = null;
		    objectValue = null;
		    jsonValueValue = null;
		    bcdValue = 0;
		    DBXValueValue = null;
		    TimeStampValue = DateTime.MinValue;
	    }

        public override string ToString()
        {
            return base.ToString();
        }

        /**
        * Return as DBXDataTypes
        */   
	    public override int GetAsTDBXDate()  {
		    checkCurrentDBXType(DBXDataTypes.DateType);
		    return this.INT32Value;
	    }

       /**
        * Set as DBXDataTypes
        */
        public override void SetAsTDBXDate(int Value)
        {
		    this.INT32Value = Value;
		    setDBXType(DBXDataTypes.DateType);
	    }

        /**
        * Set as JsonValueType
        */
        public override void SetAsJSONValue(TJSONValue Value)
        {
		    jsonValueValue = Value;
		    setDBXType(DBXDataTypes.JsonValueType);
	    }

        /**
        * Return as JsonValueType
        */  
        public override TJSONValue GetAsJSONValue()
        {
		    checkCurrentDBXType(DBXDataTypes.JsonValueType);
		    return jsonValueValue;
	    }

        /**
        * Set as BinaryBlobType
        */
        public override void SetAsStream(TStream Value)
        {
		    streamValue = Value;
		    setDBXType(DBXDataTypes.BinaryBlobType);
	    }

        /**
        * Return as BinaryBlobType
        */  
        public override TStream GetAsStream()
        {
		    checkCurrentDBXType(DBXDataTypes.BinaryBlobType);
		    return streamValue;
	    }

        /**
        * Return as UInt64Type
        */
        public override long GetAsUInt64()
        {
		    checkCurrentDBXType(DBXDataTypes.UInt64Type);
		    return UINT64Value;
	    }

       /**
        * Return as UInt32Type
        */
        public override long GetAsUInt32()
        {
		    checkCurrentDBXType(DBXDataTypes.UInt32Type);
		    return UINT32Value;
	    }

        /**
        * Set as TableType
        */
        public override void SetAsTable(TableType Value)
        {
		    objectValue = (Object) Value;
		    setDBXType(DBXDataTypes.TableType);		
	    }
        
        /**
        * Return as TableType
        */
        public override Object GetAsTable()
        {
		    checkCurrentDBXType(DBXDataTypes.TableType);
		    return objectValue;
	    }

	    public static Double CurrencyRound(double value) {
		    Double bd = Math.Round(value, DBXDefaultFormatter.CURRENCYDECIMALPLACE);
		    return bd;
	    }

	    public DBXWritableValue() : base()
        {
	    }

        protected override void throwInvalidValue()
        {
		    throw new DBXException("Invalid value for param");
	    }

        /**
        * Set as String
        */
	    public override void SetAsAnsiString(String value) {
		    SetAsString(value);
	    }

        /**
        * Return as WideStringType
        */
        public override String GetAsAnsiString()
        {
		    checkCurrentDBXType(DBXDataTypes.WideStringType);
		    return stringValue;
	    }

        /**
        * Set as BooleanType
        */
        public override void SetAsBoolean(bool Value)
        {
		    this.booleanValue = Value;
		    setDBXType(DBXDataTypes.BooleanType);
	    }

        /**
        * Set as UInt8Type
        */
        public override void SetAsUInt8(int Value)
        {
		    if (Value >= 0 && Value <= 255) {
			    this.INT32Value = Value;
			    setDBXType(DBXDataTypes.UInt8Type);
		    } else
			    throwInvalidValue();
	    }

        /**
        * Set as Int8Type
        */
        public override void SetAsInt8(int Value)
        {
		    if (Value >= -127 && Value <= 128) {
			    this.INT32Value = Value;
			    setDBXType(DBXDataTypes.Int8Type);
		    } else
			    throwInvalidValue();
	    }

        /**
        * Set as UInt16Type
        */
        public override void SetAsUInt16(int Value)
        {
		    if (Value >= 0 && Value <= 65535) {
			    this.INT32Value = Value;
			    setDBXType(DBXDataTypes.UInt16Type);
		    } else
			    throwInvalidValue();
	    }

        /**
        * Set as Int16Type
        */
        public override void SetAsInt16(int Value)
        {
		    if (Value >= -32768 && Value <= 32767) {
			    this.INT32Value = Value;
			    setDBXType(DBXDataTypes.Int16Type);
		    } else
			    throwInvalidValue();
	    }

        /**
        * Set as Int32Type
        */
        public override void SetAsInt32(int Value)
        {
		    if (Value >= -2147483648 && Value <= 2147483647) {
			    this.INT32Value = Value;
			    setDBXType(DBXDataTypes.Int32Type);
		    } else
			    throwInvalidValue();

	    }

        /**
        * Set as Int64Type
        */
        public override void SetAsInt64(long Value)
        {
		    if (Value >= -9223372036854775808L && Value <= 9223372036854775807L) {
			    this.INT64Value = Value;
			    setDBXType(DBXDataTypes.Int64Type);
		    } else
			    throwInvalidValue();

	    }

        /**
        * Set as WideStringType
        */
        public override void SetAsString(String Value)
        {
		    this.stringValue = Value;
		    setDBXType(DBXDataTypes.WideStringType);
	    }

        /**
        * Set as SingleType
        */
        public override void SetAsSingle(float Value)
        {
		    this.singleValue = Value;
		    setDBXType(DBXDataTypes.SingleType);
	    }

        /**
        * Set as DoubleType
        */
        public override void SetAsDouble(double Value)
        {
		    this.doubleValue = Value;
		    setDBXType(DBXDataTypes.DoubleType);
	    }

	    /**
        * Set as DateType
        */
	    public override void SetAsDate(DateTime Value) {
		    this.dateTimeValue = Value;
		    setDBXType(DBXDataTypes.DateType);
	    }

        /**
        * Set as TimeType
        */
        public override void SetAsTime(DateTime Value)
        {
		    this.dateTimeValue = Value;
		    setDBXType(DBXDataTypes.TimeType);
	    }

        /**
        * Set as DateTimeType
        */
        public override void SetAsDateTime(DateTime Value)
        {
		    this.dateTimeValue = Value;
		    setDBXType(DBXDataTypes.DateTimeType);
	    }

        /**
        * Set as TimeStampType
        */
        public override void SetAsTimeStamp(DateTime Value)
        {
		    this.TimeStampValue = Value;
		    setDBXType(DBXDataTypes.TimeStampType);
	    }

        /**
        * Set as BcdType
        */
        public override void SetAsBcd(double Value)
        {
		    this.bcdValue = Value;
		    setDBXType(DBXDataTypes.BcdType);
	    }

        /**
        * Set as CurrencyType
        */
        public override void SetAsCurrency(double Value)
        {
		    this.doubleValue = Value;
		    setDBXType(DBXDataTypes.CurrencyType);
	    }

        /**
        * Set as UInt32Type
        */
	    public void SetAsUInt32(long Value)  {
		    if (Value >= 0 && Value <= 4294967295L) {
			    this.UINT32Value = Value;
			    setDBXType(DBXDataTypes.UInt32Type);
		    } else
			    throwInvalidValue();

	    }

        /**
        * Set as UInt64Type
        */
        public override void SetAsUInt64(long Value)
        {
		    this.UINT64Value = Value;
		    setDBXType(DBXDataTypes.UInt64Type);
	    }

        protected override void checkCurrentDBXType(int value)
        {
		    if (value != CurrentDBXType)
			    throw new DBXException("Incorrect type in DBXValue");
	    }

	    /**
        * Return as BooleanType
        */
        public override bool GetAsBoolean()
        {
		    checkCurrentDBXType(DBXDataTypes.BooleanType);
		    return this.booleanValue;
	    }

       /**
        * Return as UInt8Type
        */
        public override int GetAsUInt8()
        {
		    checkCurrentDBXType(DBXDataTypes.UInt8Type);
		    return this.INT32Value;
	    }

        /**
        * Return as Int8Type
        */
        public override int GetAsInt8()
        {
		    checkCurrentDBXType(DBXDataTypes.Int8Type);
		    return this.INT32Value;
	    }

        /**
        * Return as UInt16Type
        */
        public override int GetAsUInt16()
        {
		    checkCurrentDBXType(DBXDataTypes.UInt16Type);
		    return this.INT32Value;
	    }

        /**
        * Return as Int16Type
        */
        public override int GetAsInt16()
        {
		    checkCurrentDBXType(DBXDataTypes.Int16Type);
		    return this.INT32Value;
	    }

        /**
        * Return as Int32Type
        */
        public override int GetAsInt32()
        {
		    checkCurrentDBXType(DBXDataTypes.Int32Type);
		    return this.INT32Value;
	    }

        /**
        * Return as Int64Type
        */        
        public override long GetAsInt64()
        {
		    checkCurrentDBXType(DBXDataTypes.Int64Type);
		    return this.INT64Value;
	    }

        /**
        * Return as WideStringType
        */  
	    public override String GetAsString()  {
		    checkCurrentDBXType(DBXDataTypes.WideStringType);
		    return this.stringValue;
	    }

        /**
        * Return as SingleType
        */ 
        public override float GetAsSingle()
        {
		    checkCurrentDBXType(DBXDataTypes.SingleType);
		    return this.singleValue;
	    }

        /**
        * Return as DoubleType
        */ 
        public override double GetAsDouble()
        {
		    setDBXType(DBXDataTypes.DoubleType);
		    return this.doubleValue;
	    }

        /**
        * Return as DateType
        */ 
        public override DateTime GetAsDate()
        {
		    checkCurrentDBXType(DBXDataTypes.DateType);
		    return this.dateTimeValue;
	    }

        /**
        * Return as TimeType
        */ 
        public override DateTime GetAsTime()
        {
		    checkCurrentDBXType(DBXDataTypes.TimeType);
		    return this.dateTimeValue;
	    }

        /**
        * Return as DateTimeType
        */
        public override DateTime GetAsDateTime()
        {
		    checkCurrentDBXType(DBXDataTypes.DateTimeType);
		    return this.dateTimeValue;
	    }

        /**
        * Return as TimeStampType
        */
        public override DateTime GetAsTimeStamp()
        {
		    checkCurrentDBXType(DBXDataTypes.TimeStampType);
		    return this.TimeStampValue;
	    }

        /**
        * Return as CurrencyType
        */
        public override double GetAsCurrency()
        {
		    checkCurrentDBXType(DBXDataTypes.CurrencyType);
		    return this.doubleValue;
	    }

        /**
        * Return as BcdType
        */
        public override double GetAsBcd()
        {
		    checkCurrentDBXType(DBXDataTypes.BcdType);
		    return this.bcdValue;
	    }

        /**
        * Return as TimeType
        */
        public override int GetAsTDBXTime()
        {
		    checkCurrentDBXType(DBXDataTypes.TimeType);
		    return this.INT32Value;
	    }

        /**
        * Set as TimeType
        */
        public override void SetAsTDBXTime(int Value)
        {
		    this.INT32Value = Value;
		    setDBXType(DBXDataTypes.TimeType);
	    }

        /**
        * Set as BlobType
        */
        public override void SetAsBlob(TStream value)
        {
		    this.objectValue = value;
		    setDBXType(DBXDataTypes.BlobType);
	    }

        /**
        * Return as BlobType
        */
        public override TStream GetAsBlob()
        {
		    checkCurrentDBXType(DBXDataTypes.BlobType);
		    return (TStream) this.objectValue;
	    }

	    // ********** DBXValueType ******************************
	    public override void SetAsDBXValue(DBXValue Value)  {
		    setDBXType(Value.getDBXType());
		    isSimpleValueType = true;
		    DBXValueValue = Value;
	    }

        /**
        * Return as DBXValueValue
        */
	    public override DBXValue GetAsDBXValue()  {
		    if (!isSimpleValueType)
			    throw new DBXException("Invalid DBX type");
		    return this.DBXValueValue;
	    }

	    /**
	     * Set null a DBXValue based on its TypeName
	     */
	    public override void SetTDBXNull(String TypeName)  {
		    if (TypeName.Equals("TDBXStringValue")) {
			    TDBXStringValue v = new TDBXStringValue();
			    v.setNull();
			    SetAsDBXValue(v);
		    } else if (TypeName.Equals("TDBXAnsiCharsValue")) {
			    TDBXAnsiCharsValue v = new TDBXAnsiCharsValue();
			    v.setNull();
			    SetAsDBXValue(v);
		    } else if (TypeName.Equals("TDBXAnsiStringValue")) {
			    TDBXAnsiStringValue v = new TDBXAnsiStringValue();
			    v.setNull();
			    SetAsDBXValue(v);
		    } else if (TypeName.Equals("TDBXSingleValue")) {
			    TDBXSingleValue v = new TDBXSingleValue();
			    v.setNull();
			    SetAsDBXValue(v);
		    } else if (TypeName.Equals("TDBXWideStringValue")) {
			    TDBXWideStringValue v = new TDBXWideStringValue();
			    v.setNull();
			    SetAsDBXValue(v);
		    } else if (TypeName.Equals("TDBXDateValue")) {
			    TDBXDateValue v = new TDBXDateValue();
			    v.setNull();
			    SetAsDBXValue(v);
		    } else if (TypeName.Equals("TDBXTimeValue")) {
			    TDBXTimeValue v = new TDBXTimeValue();
			    v.setNull();
			    SetAsDBXValue(v);
		    } else if (TypeName.Equals("TDBXBooleanValue")) {
			    TDBXBooleanValue v = new TDBXBooleanValue();
			    v.setNull();
			    SetAsDBXValue(v);
		    } else if (TypeName.Equals("TDBXDoubleValue")) {
			    TDBXDoubleValue v = new TDBXDoubleValue();
			    v.setNull();
			    SetAsDBXValue(v);
		    } else if (TypeName.Equals("TDBXInt64Value")) {
			    TDBXInt64Value v = new TDBXInt64Value();
			    v.setNull();
			    SetAsDBXValue(v);
		    } else if (TypeName.Equals("TDBXInt32Value")) {
			    TDBXInt32Value v = new TDBXInt32Value();
			    v.setNull();
			    SetAsDBXValue(v);
		    } else if (TypeName.Equals("TDBXInt16Value")) {
			    TDBXInt16Value v = new TDBXInt16Value();
			    v.setNull();
			    SetAsDBXValue(v);
		    } else if (TypeName.Equals("TDBXInt8Value")) {
			    TDBXInt8Value v = new TDBXInt8Value();
			    v.setNull();
			    SetAsDBXValue(v);
            }
            else if (TypeName.Equals("TDBXStreamValue"))
            {
                TDBXStreamValue v = new TDBXStreamValue();
                v.setNull();
                SetAsDBXValue(v);
            }
            else if (TypeName.Equals("TDBXReaderValue"))
            {
                TDBXReaderValue v = new TDBXReaderValue();
                v.setNull();
                SetAsDBXValue(v);
            }

	    }

    }




}
