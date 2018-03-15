//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

package com.embarcadero.javablackberry;

import java.util.Date;

/**
 * Abstract class that can hold all supported simple (String, int, double, etc)
 * and complex types (TDBXReader, TParams, etc)
 * 
 */

public abstract class DBXValue {
	protected int CurrentDBXType = DBXDataTypes.UnknownType;
	protected boolean isSimpleValueType = false;

	/* VALUES HOLDER */
	protected boolean booleanValue;
	protected int INT32Value;
	protected long UINT32Value;
	protected long INT64Value;
	protected long UINT64Value;
	protected String stringValue;
	protected float singleValue;
	protected double doubleValue;
	protected Date dateTimeValue;
	protected TStream streamValue;
	protected Object objectValue;
	protected TJSONValue jsonValueValue;
	protected double bcdValue;
	protected DBXValue DBXValueValue;
	protected Date TimeStampValue;

	/**
	 * Adds in a JSONArray DBXValue and then use it in the body of the request
	 * 
	 * @throws DBXException
	 * 
	 */
	public void appendTo(TJSONArray json) throws DBXException{
		try {
			if (containsASimpleValueType()) {
				if (GetAsDBXValue().isNull()) {
					json.add(new TJSONNull());
				} else {
					GetAsDBXValue().appendTo(json);
				}
				return;
			}
			switch (CurrentDBXType) {
			case DBXDataTypes.Int8Type: {
				json.add(GetAsInt8());
				break;
			}
			case DBXDataTypes.Int16Type: {
				json.add(GetAsInt16());
				break;
			}
			case DBXDataTypes.Int32Type: {
				json.add(GetAsInt32());
				break;
			}
			case DBXDataTypes.Int64Type: {
				json.add(GetAsInt64());
				break;
			}
			case DBXDataTypes.UInt8Type: {
				json.add(GetAsUInt8());
				break;
			}
			case DBXDataTypes.UInt16Type: {
				json.add(GetAsUInt16());
				break;
			}
			case DBXDataTypes.UInt32Type: {
				json.add(GetAsUInt32());
				break;
			}
			case DBXDataTypes.UInt64Type: {
				json.add(GetAsUInt64());
				break;
			}
			case DBXDataTypes.AnsiStringType:
			case DBXDataTypes.WideStringType: {
				json.add(GetAsString());
				break;
			}
			case DBXDataTypes.DateTimeType: {
				json.add(DBXDefaultFormatter.getInstance().DateTimeToString(
						dateTimeValue));
				break;
			}
			case DBXDataTypes.TimeStampType: {
				json.add(DBXDefaultFormatter.getInstance().DateTimeToString(
						TimeStampValue));
				break;
			}
			case DBXDataTypes.DateType: {
				json.add(DBXDefaultFormatter.getInstance().TDBXDateToString(
						GetAsTDBXDate()));
				break;
			}
			case DBXDataTypes.TimeType: {
				json.add(DBXDefaultFormatter.getInstance().TDBXTimeToString(
						GetAsTDBXTime()));
				break;
			}
			case DBXDataTypes.JsonValueType: {
				Object o = GetAsJSONValue().getInternalObject();
				json.add(o);
				break;
			}
			case DBXDataTypes.TableType: {
				json.add(((JSONSerializable) objectValue).asTJSONObject());
				break;
			}
			case DBXDataTypes.CurrencyType: {
				json.add(GetAsCurrency());
				break;
			}
			case DBXDataTypes.DoubleType: {
				json.add(GetAsDouble());
				break;
			}
			case DBXDataTypes.SingleType: {
				json.add(GetAsSingle());
				break;
			}
			case DBXDataTypes.BinaryBlobType: {
				json.add(StreamToJson());
				break;
			}
			default:
				throw new DBXException("Cannot convert this type to string");
			}
		} catch (DBXException ex) {
			return;
		}
	}

	/**
	 * Returns the JSON representation of the internal Stream
	 * 
	 * @return a JSONArray with the ByteArray of Stream
	 * @throws DBXException
	 */
	protected TJSONArray StreamToJson() throws DBXException {
		try {
			byte[] b = DBXTools.streamToByteArray(streamValue);
			TJSONArray jsArr = new TJSONArray();
			for (int i = 0; i < b.length; i++)
				jsArr.add(b[i]);
			return jsArr;
		} catch (Exception e) {
			throw new DBXException(e.getMessage());
		}
	}

	public int GetAsTDBXDate() throws DBXException {
		throw new DBXException("Invalid Type Access");
	}

	public void SetAsTDBXDate(int Value) throws DBXException {
		throw new DBXException("Invalid Type Access");
	}

	public void SetAsJSONValue(TJSONValue Value) throws DBXException {
		throw new DBXException("Invalid Type Access");
	}

	// ********** DBXValueType ******************************
	// Setter
	public void SetAsDBXValue(DBXValue Value) throws DBXException {
		throw new DBXException("Invalid Type Access");
	}

	// Getter
	public DBXValue GetAsDBXValue() throws DBXException {
		throw new DBXException("Invalid Type Access");
	}

	protected boolean containsASimpleValueType() {
		return isSimpleValueType;
	}

	public TJSONValue GetAsJSONValue() throws DBXException {
		throwInvalidAccess();
		return null;
	}

	public void SetAsStream(TStream Value) throws DBXException {
		throwInvalidAccess();
	}

	public TStream GetAsStream() throws DBXException {
		throwInvalidAccess();
		return null;
	}

	public long GetAsUInt64() throws DBXException {
		throwInvalidAccess();
		return 0;
	}

	public long GetAsUInt32() throws DBXException {
		throwInvalidAccess();
		return 0;
	}

	public void SetAsTable(TableType Value) throws DBXException {
		throwInvalidAccess();
	}

	public Object GetAsTable() throws DBXException {
		throwInvalidAccess();
		return null;
	}

	public DBXValue() {
		super();
		setDBXType(DBXDataTypes.UnknownType);
	}

	protected void throwInvalidAccess() throws DBXException {
		throw new DBXException("Invalid type access");
	}

	/**
	 * returns the internal DBXDataType
	 * 
	 * @return CurrentDBXType
	 */

	public int getDBXType() {
		return CurrentDBXType;
	}

	protected void setDBXType(int value) {
		CurrentDBXType = value;
		isSimpleValueType = false;
	}

	/**
	 * Returns true if the internal value is set to null, false otherwise.
	 * 
	 * @return
	 */
	public boolean isNull() {
		return CurrentDBXType == DBXDataTypes.UnknownType;
	}

	/**
	 * It clears internal structure with defaults.
	 */
	public void Clear() {
		setDBXType(DBXDataTypes.UnknownType);
	}

	protected void throwInvalidValue() throws DBXException {
		throw new DBXException("Invalid value for param");
	}

	// /SETTERS

	public void SetAsAnsiString(String value) throws DBXException {
		throwInvalidAccess();
	}

	public String GetAsAnsiString() throws DBXException {
		throwInvalidAccess();
		return null;
	}

	public void SetAsBoolean(boolean Value) throws DBXException {
		throwInvalidAccess();
	}

	public void SetAsUInt8(int Value) throws DBXException {
		throwInvalidAccess();
	}

	public void SetAsInt8(int Value) throws DBXException {
		throwInvalidAccess();
	}

	public void SetAsUInt16(int Value) throws DBXException {
		throwInvalidAccess();
	}

	public void SetAsInt16(int Value) throws DBXException {
		throwInvalidAccess();
	}

	public void SetAsInt32(int Value) throws DBXException {
		throwInvalidAccess();
	}

	public void SetAsInt64(long Value) throws DBXException {
		throwInvalidAccess();
	}

	public void SetAsString(String Value) throws DBXException {
		throwInvalidAccess();
	}

	public void SetAsSingle(float Value) throws DBXException {
		throwInvalidAccess();
	}

	public void SetAsDouble(double Value) throws DBXException {
		throwInvalidAccess();
	}

	public void SetAsDate(Date Value) throws DBXException {
		throwInvalidAccess();
	}

	public void SetAsTime(Date Value) throws DBXException {
		throwInvalidAccess();
	}

	public void SetAsDateTime(Date Value) throws DBXException {
		throwInvalidAccess();
	}

	public void SetAsTimeStamp(Date Value) throws DBXException {
		throwInvalidAccess();
	}

	public void SetAsBcd(double Value) throws DBXException {
		throwInvalidAccess();
	}

	public void SetAsCurrency(double Value) throws DBXException {
		throwInvalidAccess();
	}

	public void SetAsUInt32(int Value) throws DBXException {
		throwInvalidAccess();
	}

	public void SetAsUInt64(long Value) throws DBXException {
		throwInvalidAccess();
	}

	protected void checkCurrentDBXType(int value) throws DBXException {
		throw new DBXException("Invalid type access");
	}

	// // /GETTER
	public boolean GetAsBoolean() throws DBXException {
		throwInvalidAccess();
		return false;
	}

	public int GetAsUInt8() throws DBXException {
		throwInvalidAccess();
		return 0;
	}

	public int GetAsInt8() throws DBXException {
		throwInvalidAccess();
		return 0;

	}

	public int GetAsUInt16() throws DBXException {
		throwInvalidAccess();
		return 0;
	}

	public int GetAsInt16() throws DBXException {
		throwInvalidAccess();
		return 0;
	}

	public int GetAsInt32() throws DBXException {
		throwInvalidAccess();
		return 0;
	}

	public long GetAsInt64() throws DBXException {
		throwInvalidAccess();
		return 0;
	}

	public String GetAsString() throws DBXException {
		throwInvalidAccess();
		return null;
	}

	public float GetAsSingle() throws DBXException {
		throwInvalidAccess();
		return 0;
	}

	public double GetAsDouble() throws DBXException {
		throwInvalidAccess();
		return 0;
	}

	public Date GetAsDate() throws DBXException {
		throwInvalidAccess();
		return null;
	}

	public Date GetAsTimeStamp() throws DBXException {
		throwInvalidAccess();
		return null;
	}

	public Date GetAsTime() throws DBXException {
		throwInvalidAccess();
		return null;
	}

	public Date GetAsDateTime() throws DBXException {
		throwInvalidAccess();
		return null;
	}

	public double GetAsCurrency() throws DBXException {
		throwInvalidAccess();
		return 0;
	}

	public double GetAsBcd() throws DBXException {
		throwInvalidAccess();
		return 0;
	}

	public int GetAsTDBXTime() throws DBXException {
		throwInvalidAccess();
		return 0;
	}

	public void SetAsTDBXTime(int Value) throws DBXException {
		throwInvalidAccess();
	}

	public void SetAsBlob(TStream value) throws DBXException {
		throwInvalidAccess();
	}

	public TStream GetAsBlob() throws DBXException {
		throwInvalidAccess();
		return null;
	}

	/**
	 * Convert to String a DBXValue
	 */
	public String toString() {
		try {
			if (containsASimpleValueType()) {
				if (GetAsDBXValue().isNull())
					return new TJSONNull().asJSONString();
				else
					return GetAsDBXValue().toString();
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
				return String.valueOf(GetAsUInt8());
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
			case DBXDataTypes.BooleanType: {
				return String.valueOf(GetAsBoolean());
			}
			case DBXDataTypes.BcdType: {
				return String.valueOf(GetAsBcd());
			}
			default:
				throw new DBXException("Cannot convert this type to string");
			}
		} catch (DBXException ex) {
			return "<CANNOT CONVERT DBXType [" + String.valueOf(CurrentDBXType)
					+ "] TO STRING>";
		}
	}

	public void SetTDBXNull(String TypeName) throws DBXException {
		throwInvalidAccess();
	}

	public void setNull() throws DBXException {
		throwInvalidAccess();
	}
}
