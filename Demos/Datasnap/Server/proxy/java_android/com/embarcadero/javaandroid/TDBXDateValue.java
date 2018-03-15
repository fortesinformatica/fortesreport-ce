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
 * Wraps the Date type and allows it to be null
 *
 */

public class TDBXDateValue extends DBXValue  {
	protected boolean ValueNull = false;
	private int DBXDateValue;

	public TDBXDateValue() {
		super();
		setDBXType(DBXDataTypes.DateType);
	}
	
	public boolean isNull() {
		return ValueNull;
	}
	
	@Override
	public void setNull() {
		ValueNull = true;
		DBXDateValue = 0;
	}
	
	@Override
	public void SetAsTDBXDate(int Value) throws DBXException {		
		DBXDateValue = Value;
		ValueNull = false;
	}

	public int GetAsTDBXDate() throws DBXException {
		return DBXDateValue;
	}

}
