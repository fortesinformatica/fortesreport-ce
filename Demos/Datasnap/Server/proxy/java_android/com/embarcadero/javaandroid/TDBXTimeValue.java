//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

package com.embarcadero.javaandroid;

/**
 * 
 * Wraps the Time type and allows it to be null
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

	@Override
	public void setNull() {
		ValueNull = true;
		DBXTimeValue = 0;
	}
	
	@Override
	public void SetAsTDBXTime(int Value) throws DBXException {
		DBXTimeValue = Value;
		ValueNull = false;
	}

	@Override
	public int GetAsTDBXTime() throws DBXException {
		return DBXTimeValue;
	}

}
