//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

package com.embarcadero.javablackberry;

/**
 * 
 * Wrap the Time type and allows it to be null
 *
 */


public class TDBXTimeValue extends DBXValue {
	protected boolean ValueNull = false;
	private int DBXTimeValue;

	public TDBXTimeValue() {
		super();
		setDBXType(DBXDataTypes.TimeType);
	}
	
	public boolean isNull() {
		return ValueNull;
	}

	public void setNull() {
		ValueNull = true;
		DBXTimeValue = 0;
	}
	
	public void SetAsTDBXTime(int Value) throws DBXException {
		DBXTimeValue = Value;
		ValueNull = false;
	}

	public int GetAsTDBXTime() throws DBXException {
		return DBXTimeValue;
	}

}