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
     * Abstract class that can hold all supported simple (String, int, double, etc)
     * and complex types (TDBXReader, TParams, etc)
     * 
     */
 
    public abstract class DBXValue {
	    protected int CurrentDBXType = DBXDataTypes.UnknownType;
	    protected bool isSimpleValueType = false;

	    /* VALUES HOLDER */
        protected bool booleanValue;
        protected int INT32Value;
        protected long UINT32Value;
        protected long INT64Value;
        protected long UINT64Value;
        protected String stringValue;
        protected float singleValue;
        protected double doubleValue;
        protected DateTime dateTimeValue;
        protected TStream streamValue;
        protected Object objectValue;
        protected TJSONValue jsonValueValue;
        protected double bcdValue;
        protected DBXValue DBXValueValue;
        protected DateTime TimeStampValue;	
	
	    /**
	     * Adds in a JArray DBXValue and then use it in the body of the request 
	     */
	    public void appendTo(TJSONArray json) {
		    try {
			    if (containsASimpleValueType()) {
				    GetAsDBXValue().appendTo(json);
				    return;
			    }

			    switch (CurrentDBXType) {
			    case DBXDataTypes.Int8Type: {
				    json.add(new TJSONNumber(GetAsInt8()));
				    break;
			    }
			    case DBXDataTypes.Int16Type: {
				    json.add(new TJSONNumber(GetAsInt16()));
				    break;
			    }
			    case DBXDataTypes.Int32Type: {
				    json.add(new TJSONNumber(GetAsInt32()));
				    break;
			    }
			    case DBXDataTypes.Int64Type: {
				    json.add(new TJSONNumber(GetAsInt64()));
				    break;
			    }
			    case DBXDataTypes.UInt8Type: {
				    json.add(new TJSONNumber(GetAsUInt8()));
				    break;
			    }
			    case DBXDataTypes.UInt16Type: {
				    json.add(new TJSONNumber(GetAsUInt16()));
				    break;
			    }
			    case DBXDataTypes.UInt32Type: {
				    json.add(new TJSONNumber(GetAsUInt32()));
				    break;
			    }
			    case DBXDataTypes.UInt64Type: {
				    json.add(new TJSONNumber(GetAsUInt64()));
				    break;
			    }
			    case DBXDataTypes.AnsiStringType:
			    case DBXDataTypes.WideStringType: {
				    json.add(new TJSONString(GetAsString()));
				    break;
			    }
			    case DBXDataTypes.DateTimeType: {
				    json.add(new TJSONString(DBXDefaultFormatter.getInstance().DateTimeToString(
						    dateTimeValue)));
				    break;
			    }
			    case DBXDataTypes.TimeStampType: {
				    json.add(new TJSONString(DBXDefaultFormatter.getInstance().DateTimeToString(
						    TimeStampValue)));
				    break;
			    }
			    case DBXDataTypes.DateType: {
				    json.add(new TJSONString(DBXDefaultFormatter.getInstance().TDBXDateToString(
						    GetAsTDBXDate())));
				    break;
			    }
			    case DBXDataTypes.TimeType: {
				    json.add(new TJSONString(DBXDefaultFormatter.getInstance().TDBXTimeToString(
						    GetAsTDBXTime())));
				    break;
			    }
			    case DBXDataTypes.JsonValueType: {
				    Object o = GetAsJSONValue().getInternalObject();
				    json.add(o);
				    break;
			    }
			    case DBXDataTypes.TableType: {
				    try {
					    json.add(((JSONSerializable) objectValue).asJSONObject());
				    } catch (Exception e) {
					    throw new DBXException(e.Message);
				    }
				    break;
			    }
			    case DBXDataTypes.CurrencyType: {
				    try {
					    json.add(new TJSONNumber(GetAsCurrency()));
				    } catch (Exception e) {
					    throw new DBXException(e.Message);
				    }
				    break;
			    }
			    case DBXDataTypes.DoubleType: {
				    try {
					    json.add(new TJSONNumber(GetAsDouble()));
				    } catch (Exception e) {
					    throw new DBXException(e.Message);
				    }
				    break;
			    }
			    case DBXDataTypes.SingleType: {
				    try {
					    json.add(new TJSONNumber(GetAsSingle()));
				    } catch (Exception e) {
					    throw new DBXException(e.Message);
				    }
				    break;
			    }
			    case DBXDataTypes.BinaryBlobType: {
				    json.add(StreamToJson());
				    break;
			    }
			    default:
				    throw new DBXException("Cannot convert this type to string");
			    }
		    } catch (DBXException) {
			    return;
		    }
	    }
	
	    /**
    	 * 
    	 * @return a JArray with the ByteArray of Stream
    	 */
	    protected TJSONArray StreamToJson() 
        {
		    checkCurrentDBXType(DBXDataTypes.BinaryBlobType);

		    try {
			    byte[] b = DBXTools.streamToByteArray(streamValue);
			    TJSONArray jsArr = new TJSONArray();
			    for (int i = 0; i < b.Length; i++)
				    jsArr.add(new TJSONNumber(Convert.ToInt32(b[i])));
			    return jsArr;
		    } catch (Exception e) {
			    throw new DBXException(e.Message);
		    }
	    }

        public virtual int GetAsTDBXDate()
        {
		    throw new DBXException("Invalid Type Access");
	    }

        public virtual void SetAsTDBXDate(int Value) 
        {
		    throw new DBXException("Invalid Type Access");
	    }

        public virtual void SetAsJSONValue(TJSONValue Value) 
        {
		    throw new DBXException("Invalid Type Access");
	    }

	    // ********** DBXValueType ******************************
	    // Setter
        public virtual void SetAsDBXValue(DBXValue Value) 
        {
		    throw new DBXException("Invalid Type Access");
	    }

	    // Getter
        public virtual DBXValue GetAsDBXValue()
        {
		    throw new DBXException("Invalid Type Access");
	    }
	
	
	    protected virtual bool containsASimpleValueType() {return isSimpleValueType;}


        public virtual TJSONValue GetAsJSONValue() 
        {
		    throwInvalidAccess();
		    return null;
	    }

	    public virtual void SetAsStream(TStream Value) 
        {
		    throwInvalidAccess();
	    }

	    public virtual TStream GetAsStream()
        {
		    throwInvalidAccess();
		    return null;
	    }

        public virtual long GetAsUInt64() 
        {
		    throwInvalidAccess();
		    return 0;
	    }

        public virtual long GetAsUInt32()
        {
		    throwInvalidAccess();
		    return 0;
	    }

        public virtual void SetAsTable(TableType Value)
        {
		    throwInvalidAccess();
	    }

        public virtual Object GetAsTable()
        {
		    throwInvalidAccess();
		    return null;
	    }

	    public DBXValue() : base() 
        {
		    setDBXType(DBXDataTypes.UnknownType);
	    }
	
	    protected void throwInvalidAccess() {
		    throw new DBXException("Invalid type access");
	    }
	
	    public int getDBXType() {
		    return CurrentDBXType;
	    }

        public virtual void setDBXType(int value)
        {
		    CurrentDBXType = value;
		    isSimpleValueType = false;
	    }

	    public virtual bool isNull() {
		    return CurrentDBXType == DBXDataTypes.UnknownType;
	    }

	    public virtual void Clear() {
		    setDBXType(DBXDataTypes.UnknownType);
	    }

	    protected virtual void throwInvalidValue()  {
		    throw new DBXException("Invalid value for param");
	    }


	    public virtual void SetAsAnsiString(String value)  {
		    throwInvalidAccess();
	    }

        public virtual String GetAsAnsiString()
        {
		    throwInvalidAccess();
		    return null;		
	    }

        public virtual void SetAsBoolean(bool Value)
        {
		    throwInvalidAccess();
	    }

        public virtual void SetAsUInt8(int Value)
        {
		    throwInvalidAccess();
	    }

        public virtual void SetAsInt8(int Value)
        {
		    throwInvalidAccess();
	    }

        public virtual void SetAsUInt16(int Value)
        {
		    throwInvalidAccess();
	    }

        public virtual void SetAsInt16(int Value)
        {
		    throwInvalidAccess();
	    }

        public virtual void SetAsInt32(int Value)
        {
		    throwInvalidAccess();
	    }

        public virtual void SetAsInt64(long Value)
        {
		    throwInvalidAccess();
	    }

        public virtual void SetAsString(String Value)
        {
		    throwInvalidAccess();
	    }

        public virtual void SetAsSingle(float Value)
        {
		    throwInvalidAccess();
	    }

        public virtual void SetAsDouble(double Value)
        {
		    throwInvalidAccess();
	    }

        public virtual void SetAsDate(DateTime Value)
        {
		    throwInvalidAccess();
	    }

        public virtual void SetAsTime(DateTime Value)
        {
		    throwInvalidAccess();
	    }

        public virtual void SetAsDateTime(DateTime Value)
        {
		    throwInvalidAccess();
	    }

        public virtual void SetAsTimeStamp(DateTime Value)
        {
		    throwInvalidAccess();
	    }

        public virtual void SetAsBcd(double Value)
        {
		    throwInvalidAccess();
	    }

        public virtual void SetAsCurrency(double Value)
        {
		    throwInvalidAccess();
	    }

        public virtual void SetAsUInt32(uint Value)
        {
		    throwInvalidAccess();
	    }

        public virtual void SetAsUInt64(long Value)
        {
		    throwInvalidAccess();
	    }

	    protected virtual void checkCurrentDBXType(int value)  {
		    throw new DBXException("Invalid type access");
	    }

        public virtual bool GetAsBoolean()
        {
		    throwInvalidAccess();
		    return false;
	    }

        public virtual int GetAsUInt8()
        {
		    throwInvalidAccess();
		    return 0;
	    }

        public virtual int GetAsInt8()
        {
		    throwInvalidAccess();
		    return 0;

	    }

        public virtual int GetAsUInt16()
        {
		    throwInvalidAccess();
		    return 0;
	    }

        public virtual int GetAsInt16()
        {
		    throwInvalidAccess();
		    return 0;
	    }

        public virtual int GetAsInt32()
        {
		    throwInvalidAccess();
		    return 0;
	    }

        public virtual long GetAsInt64()
        {
		    throwInvalidAccess();
		    return 0;
	    }

        public virtual String GetAsString()
        {
		    throwInvalidAccess();
		    return null;
	    }

        public virtual float GetAsSingle()
        {
		    throwInvalidAccess();
		    return 0;
	    }

        public virtual double GetAsDouble()
        {
		    throwInvalidAccess();
		    return 0;
	    }

        public virtual DateTime GetAsDate()
        {
            throwInvalidAccess();
            return DateTime.MinValue;
        }

        public virtual DateTime GetAsTimeStamp()
        {
		    throwInvalidAccess();
            return DateTime.MinValue;
	    }
        public virtual DateTime GetAsTime()
        {
		    throwInvalidAccess();
            return DateTime.MinValue;
	    }

        public virtual DateTime GetAsDateTime()
        {
		    throwInvalidAccess();
            return DateTime.MinValue;
	    }

        public virtual double GetAsCurrency()
        {
		    throwInvalidAccess();
		    return 0;
	    }

        public virtual double GetAsBcd()
        {
		    throwInvalidAccess();
		    return 0;
	    }

        public virtual int GetAsTDBXTime()
        {
		    throwInvalidAccess();
		    return 0;
	    }

        public virtual void SetAsTDBXTime(int Value)
        {
		    throwInvalidAccess();
	    }

        public virtual void SetAsBlob(TStream value)
        {
		    throwInvalidAccess();
	    }

        public virtual TStream GetAsBlob()
        {
		    throwInvalidAccess();
		    return null;
	    }

	
	    /**
	     * Convert to String a DBXValue
	     */
	    public override String ToString() {
		    try {
			    if (containsASimpleValueType())
                    if (GetAsDBXValue().isNull())
                    {
                        return new TJSONNull().asJSONString();
                    }
                    else
                    {
                        return GetAsDBXValue().ToString();
                    }
			    switch (CurrentDBXType) {
			    case DBXDataTypes.AnsiStringType: {
				    return DBXDefaultFormatter.getInstance().AnsiStringToString(
						    GetAsString());
			    }
			    case DBXDataTypes.WideStringType: {
					    return DBXDefaultFormatter.getInstance().WideStringToString(
						    GetAsString());
			    }
			    case DBXDataTypes.Int8Type: {
				    return DBXDefaultFormatter.getInstance().Int8ToString(
						    GetAsInt8());
			    }
			    case DBXDataTypes.Int16Type: {
				    return DBXDefaultFormatter.getInstance().Int16ToString(
						    GetAsInt16());
			    }
			    case DBXDataTypes.Int32Type: {
				    return DBXDefaultFormatter.getInstance().Int32ToString(
						    GetAsInt32());
			    }
			    case DBXDataTypes.Int64Type: {
				    return DBXDefaultFormatter.getInstance().Int64ToString(
						    GetAsInt64());
			    }
			    case DBXDataTypes.UInt8Type: {
                    return DBXDefaultFormatter.getInstance().UInt8ToString(GetAsUInt8());
			    }
			    case DBXDataTypes.UInt16Type: {
				    return DBXDefaultFormatter.getInstance().UInt16ToString(
						    GetAsUInt16());
			    }
			    case DBXDataTypes.UInt32Type: {
				    return DBXDefaultFormatter.getInstance().UInt32ToString(
						    GetAsUInt32());
			    }
			    case DBXDataTypes.UInt64Type: {
				    return DBXDefaultFormatter.getInstance().UInt64ToString(
						    GetAsUInt64());
			    }

			    case DBXDataTypes.DateTimeType: {
				    return DBXDefaultFormatter.getInstance().DateTimeToString(
						    GetAsDateTime());
			    }
			    case DBXDataTypes.TimeStampType: {
				    return DBXDefaultFormatter.getInstance().DateTimeToString(
						    GetAsTimeStamp());
			    }
			    case DBXDataTypes.DateType: {
				    String s = DBXDefaultFormatter.getInstance().TDBXDateToString(
						    GetAsTDBXDate());
				    return s;
			    }
			    case DBXDataTypes.TimeType: {
				    String s = DBXDefaultFormatter.getInstance().TDBXTimeToString(
						    GetAsTDBXTime());
				    return s;
			    }
			    case DBXDataTypes.DoubleType: {
				    return DBXDefaultFormatter.getInstance().doubleToString(
						    GetAsDouble());
			    }
			    case DBXDataTypes.SingleType: {
				    return DBXDefaultFormatter.getInstance().floatToString(
						    GetAsSingle());
			    }
			    case DBXDataTypes.CurrencyType: {
				    return DBXDefaultFormatter.getInstance().currencyToString(
						    GetAsCurrency());
			    }
                case DBXDataTypes.BooleanType:{
                    return DBXDefaultFormatter.getInstance().booleanToString(GetAsBoolean());
			    }
                case DBXDataTypes.BcdType: {
                    return DBXDefaultFormatter.getInstance().doubleToString(GetAsBcd());
                }
			    default:
				    throw new DBXException("Cannot convert this type to string");
			    }
		    } catch (DBXException) {
			    return "<CANNOT CONVERT DBXType [" + Convert.ToString(CurrentDBXType) + "] TO STRING>";
		    }
	    }


        public virtual void SetTDBXNull(String TypeName)
        {
		    throwInvalidAccess();
	    }
	
	    public virtual void setNull()  {
		    throwInvalidAccess();
	    }

    }
}
