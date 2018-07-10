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
 * Wraps the Boolean type and allows it to be null
 *
 */

public class TDBXBooleanValue extends DBXValue {

	protected boolean ValueNull = false;
	private boolean DBXBooleanValue;
	
	public TDBXBooleanValue() {
		super();
		setDBXType(DBXDataTypes.BooleanType);
	}
	
	@Override
	public void setNull() {
		ValueNull = true;
		DBXBooleanValue = false;
	}

	public boolean isNull() {
		return ValueNull;
	}

	@Override
	public void SetAsBoolean(boolean Value) throws DBXException {		
		DBXBooleanValue = Value;
		ValueNull = false;
	}
	
	@Override
	public boolean GetAsBoolean() throws DBXException {
		return DBXBooleanValue;
	}


}
