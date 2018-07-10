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
 * Wraps the WideString type and allows it to be null
 *
 */


public class TDBXWideStringValue extends DBXValue {
	protected boolean ValueNull = false;
	private String DBXWideStringValue;

	public TDBXWideStringValue() {
		super();
		setDBXType(DBXDataTypes.WideStringType);
	}
	
	public void setNull() {
		ValueNull = true;
		DBXWideStringValue = null;
	}

	public boolean isNull() {
		return ValueNull;
	}

	@Override
	public void SetAsString(String Value) throws DBXException {
		ValueNull = false;
		DBXWideStringValue = Value;
	}

	@Override
	public String GetAsString() throws DBXException {
		return DBXWideStringValue;
	}
}
