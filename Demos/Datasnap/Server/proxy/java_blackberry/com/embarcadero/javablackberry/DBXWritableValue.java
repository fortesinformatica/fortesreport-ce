//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

package com.embarcadero.javablackberry;

import java.util.Date;

import javax.microedition.global.Formatter;

/**
 * 
 * This class extends DBXValue and overrides the setter and getter methods of
 * DBXValue to allows to set and get specified types
 * 
 */

public class DBXWritableValue extends DBXValue {

	/**
	 * It clears internal structure with defaults. 
	 */
	
	public void Clear() {
		booleanValue = false;
		INT32Value = 0;
		UINT32Value = 0;
		INT64Value = 0;
		UINT64Value = 0;
		stringValue = "";
		singleValue = 0;
		doubleValue = 0;
		dateTimeValue = null;
		streamValue = null;
		objectValue = null;
		jsonValueValue = null;
		bcdValue = 0;
		DBXValueValue = null;
		TimeStampValue = null;
	}
	
	public int GetAsTDBXDate() throws DBXException {
		checkCurrentDBXType(DBXDataTypes.DateType);
		return this.INT32Value;
	}

	public void SetAsTDBXDate(int Value) {
		Clear();
		this.INT32Value = Value;
		setDBXType(DBXDataTypes.DateType);
	}

	public void SetAsJSONValue(TJSONValue Value) {
		Clear();
		jsonValueValue = Value;
		setDBXType(DBXDataTypes.JsonValueType);
	}

	public TJSONValue GetAsJSONValue() throws DBXException {
		checkCurrentDBXType(DBXDataTypes.JsonValueType);
		return jsonValueValue;
	}

	public void SetAsStream(TStream Value) {
		Clear();
		streamValue = Value;
		setDBXType(DBXDataTypes.BinaryBlobType);
	}

	public TStream GetAsStream() throws DBXException {
		checkCurrentDBXType(DBXDataTypes.BinaryBlobType);
		return streamValue;
	}

	public long GetAsUInt64() throws DBXException {
		checkCurrentDBXType(DBXDataTypes.UInt64Type);
		return UINT64Value;
	}

	public long GetAsUInt32() throws DBXException {
		checkCurrentDBXType(DBXDataTypes.UInt32Type);
		return UINT32Value;
	}

	public void SetAsTable(TableType Value) {
		Clear();
		objectValue = (Object) Value;
		setDBXType(DBXDataTypes.TableType);
	}

	public Object GetAsTable() throws DBXException {
		checkCurrentDBXType(DBXDataTypes.TableType);
		return objectValue;
	}

	public static Double CurrencyRound(double value) {
		Formatter format = new Formatter();
		return Double.valueOf(format.formatNumber(value, 4));
	}

	public DBXWritableValue() {
		super();
	}

	protected void throwInvalidValue() throws DBXException {
		throw new DBXException("Invalid value for param");
	}

	// /SETTERS

	public void SetAsAnsiString(String value) {
		SetAsString(value);
	}

	public String GetAsAnsiString() throws DBXException {
		checkCurrentDBXType(DBXDataTypes.WideStringType);
		return stringValue;
	}

	public void SetAsBoolean(boolean Value) {
		Clear();
		this.booleanValue = Value;
		setDBXType(DBXDataTypes.BooleanType);
	};

	public void SetAsUInt8(int Value) throws DBXException {
		if (Value >= 0 && Value <= 255) 
		{
			Clear();
			this.INT32Value = Value;
			setDBXType(DBXDataTypes.UInt8Type);
		} else
			throwInvalidValue();

	}

	public void SetAsInt8(int Value) throws DBXException {
		if (Value >= -128 && Value <= 127) 
		{
			Clear();
			this.INT32Value = Value;
			setDBXType(DBXDataTypes.Int8Type);
		} else
			throwInvalidValue();
	}

	public void SetAsUInt16(int Value) throws DBXException {
		if (Value >= 0 && Value <= 65535) 
		{
			Clear();
			this.INT32Value = Value;
			setDBXType(DBXDataTypes.UInt16Type);
		} else
			throwInvalidValue();
	}

	public void SetAsInt16(int Value) throws DBXException {
		if (Value >= -32767 && Value <= 32768) 
		{
			Clear();
			this.INT32Value = Value;
			setDBXType(DBXDataTypes.Int16Type);
		} else
			throwInvalidValue();
	}

	public void SetAsInt32(int Value) throws DBXException {
		if (Value >= -2147483648 && Value <= 2147483647) 
		{
			Clear();
			this.INT32Value = Value;
			setDBXType(DBXDataTypes.Int32Type);
		} else
			throwInvalidValue();
	}

	public void SetAsInt64(long Value) throws DBXException {
		if (Value >= -9223372036854775808L && Value <= 9223372036854775807L) 
		{
			Clear();
			this.INT64Value = Value;
			setDBXType(DBXDataTypes.Int64Type);
		} else
			throwInvalidValue();
	}

	public void SetAsString(String Value) {
		Clear();
		this.stringValue = Value;
		setDBXType(DBXDataTypes.WideStringType);
	}

	public void SetAsSingle(float Value) throws DBXException {
		Clear();
		this.singleValue = Value;
		setDBXType(DBXDataTypes.SingleType);
	}

	public void SetAsDouble(double Value) throws DBXException {
		Clear();
		this.doubleValue = Value;
		setDBXType(DBXDataTypes.DoubleType);
	}

	public void SetAsDate(Date Value) {
		Clear();
		this.dateTimeValue = Value;
		setDBXType(DBXDataTypes.DateType);
	}

	public void SetAsTime(Date Value) {
		Clear();
		this.dateTimeValue = Value;
		setDBXType(DBXDataTypes.TimeType);
	}

	public void SetAsDateTime(Date Value) {
		Clear();
		this.dateTimeValue = Value;
		setDBXType(DBXDataTypes.DateTimeType);
	}

	public void SetAsTimeStamp(Date Value) {
		Clear();
		this.TimeStampValue = Value;
		setDBXType(DBXDataTypes.TimeStampType);
	}

	public void SetAsBcd(double Value) throws DBXException {
		Clear();
		this.bcdValue = Value;
		setDBXType(DBXDataTypes.BcdType);
	}

	public void SetAsCurrency(double Value) throws DBXException {
		Clear();
		this.doubleValue = Value;
		setDBXType(DBXDataTypes.CurrencyType);
	}

	public void SetAsUInt32(long Value) throws DBXException {
		if (Value >= 0 && Value <= 4294967295L) 
		{
			Clear();
			this.UINT32Value = Value;
			setDBXType(DBXDataTypes.UInt32Type);
		} else
			throwInvalidValue();
	}

	public void SetAsUInt64(long Value) throws DBXException {
		Clear();
		this.UINT64Value = Value;
		setDBXType(DBXDataTypes.UInt64Type);
	}

	/**
	 * Checks if the internal type is the same with the value held. 
	 * @param value
	 * @throws DBXException
	 */
	protected void checkCurrentDBXType(int value) throws DBXException {
		if (value != CurrentDBXType)
			throw new DBXException("Incorrect type in DBXValue");
	}

	// /GETTER
	public boolean GetAsBoolean() throws DBXException {
		checkCurrentDBXType(DBXDataTypes.BooleanType);
		return this.booleanValue;
	}

	public int GetAsUInt8() throws DBXException {
		checkCurrentDBXType(DBXDataTypes.UInt8Type);
		return this.INT32Value;
	}

	public int GetAsInt8() throws DBXException {
		checkCurrentDBXType(DBXDataTypes.Int8Type);
		return this.INT32Value;
	}

	public int GetAsUInt16() throws DBXException {
		checkCurrentDBXType(DBXDataTypes.UInt16Type);
		return this.INT32Value;
	}

	public int GetAsInt16() throws DBXException {
		checkCurrentDBXType(DBXDataTypes.Int16Type);
		return this.INT32Value;
	}

	public int GetAsInt32() throws DBXException {
		checkCurrentDBXType(DBXDataTypes.Int32Type);
		return this.INT32Value;
	}

	public long GetAsInt64() throws DBXException {
		checkCurrentDBXType(DBXDataTypes.Int64Type);
		return this.INT64Value;
	}

	public String GetAsString() throws DBXException {
		checkCurrentDBXType(DBXDataTypes.WideStringType);
		return this.stringValue;
	}

	public float GetAsSingle() throws DBXException {
		checkCurrentDBXType(DBXDataTypes.SingleType);
		return this.singleValue;
	}

	public double GetAsDouble() throws DBXException {
		setDBXType(DBXDataTypes.DoubleType);
		return this.doubleValue;
	}

	public Date GetAsDate() throws DBXException {
		checkCurrentDBXType(DBXDataTypes.DateType);
		return this.dateTimeValue;
	}

	public Date GetAsTime() throws DBXException {
		checkCurrentDBXType(DBXDataTypes.TimeType);
		return this.dateTimeValue;
	}

	public Date GetAsDateTime() throws DBXException {
		checkCurrentDBXType(DBXDataTypes.DateTimeType);
		return this.dateTimeValue;
	}

	public Date GetAsTimeStamp() throws DBXException {
		checkCurrentDBXType(DBXDataTypes.TimeStampType);
		return this.TimeStampValue;
	}

	public double GetAsCurrency() throws DBXException {
		checkCurrentDBXType(DBXDataTypes.CurrencyType);
		return this.doubleValue;
	}

	public double GetAsBcd() throws DBXException {
		checkCurrentDBXType(DBXDataTypes.BcdType);
		return this.bcdValue;
	}

	public int GetAsTDBXTime() throws DBXException {
		checkCurrentDBXType(DBXDataTypes.TimeType);
		return this.INT32Value;
	}

	public void SetAsTDBXTime(int Value) {
		Clear();
		this.INT32Value = Value;
		setDBXType(DBXDataTypes.TimeType);
	}

	public void SetAsBlob(TStream value) {
		Clear();
		this.objectValue = value;
		setDBXType(DBXDataTypes.BlobType);
	}

	public TStream GetAsBlob() throws DBXException {
		checkCurrentDBXType(DBXDataTypes.BlobType);
		return (TStream) this.objectValue;
	}

	// ********** DBXValueType ******************************
	public void SetAsDBXValue(DBXValue Value) throws DBXException {
		setDBXType(Value.getDBXType());
		isSimpleValueType = true;
		DBXValueValue = Value;
	}

	public DBXValue GetAsDBXValue() throws DBXException {
		if (!isSimpleValueType)
			throw new DBXException("Invalid DBX type");
		return this.DBXValueValue;
	}

	/**
	 * Set null a DBXValue based on its TypeName
	 */
	
	public void SetTDBXNull(String TypeName) throws DBXException {
		if (TypeName.equals("TDBXStringValue")) {
			TDBXStringValue v = new TDBXStringValue();
			v.setNull();
			SetAsDBXValue(v);
		} else if (TypeName.equals("TDBXAnsiCharsValue")) {
			TDBXAnsiCharsValue v = new TDBXAnsiCharsValue();
			v.setNull();
			SetAsDBXValue(v);
		} else if (TypeName.equals("TDBXAnsiStringValue")) {
			TDBXAnsiStringValue v = new TDBXAnsiStringValue();
			v.setNull();
			SetAsDBXValue(v);
		} else if (TypeName.equals("TDBXSingleValue")) {
			TDBXSingleValue v = new TDBXSingleValue();
			v.setNull();
			SetAsDBXValue(v);
		} else if (TypeName.equals("TDBXWideStringValue")) {
			TDBXWideStringValue v = new TDBXWideStringValue();
			v.setNull();
			SetAsDBXValue(v);
		} else if (TypeName.equals("TDBXDateValue")) {
			TDBXDateValue v = new TDBXDateValue();
			v.setNull();
			SetAsDBXValue(v);
		} else if (TypeName.equals("TDBXTimeValue")) {
			TDBXTimeValue v = new TDBXTimeValue();
			v.setNull();
			SetAsDBXValue(v);
		} else if (TypeName.equals("TDBXBooleanValue")) {
			TDBXBooleanValue v = new TDBXBooleanValue();
			v.setNull();
			SetAsDBXValue(v);
		} else if (TypeName.equals("TDBXDoubleValue")) {
			TDBXDoubleValue v = new TDBXDoubleValue();
			v.setNull();
			SetAsDBXValue(v);
		} else if (TypeName.equals("TDBXInt64Value")) {
			TDBXInt64Value v = new TDBXInt64Value();
			v.setNull();
			SetAsDBXValue(v);
		} else if (TypeName.equals("TDBXInt32Value")) {
			TDBXInt32Value v = new TDBXInt32Value();
			v.setNull();
			SetAsDBXValue(v);
		} else if (TypeName.equals("TDBXInt16Value")) {
			TDBXInt16Value v = new TDBXInt16Value();
			v.setNull();
			SetAsDBXValue(v);
		} else if (TypeName.equals("TDBXInt8Value")) {
			TDBXInt8Value v = new TDBXInt8Value();
			v.setNull();
			SetAsDBXValue(v);
		} else if (TypeName.equals("TDBXStreamValue")) {
			TDBXStreamValue v = new TDBXStreamValue();
			v.setNull();
			SetAsDBXValue(v);
		} else if (TypeName.equals("TDBXReaderValue")) {
			TDBXReaderValue v = new TDBXReaderValue();
			v.setNull();
			SetAsDBXValue(v);
		}

	}

}

