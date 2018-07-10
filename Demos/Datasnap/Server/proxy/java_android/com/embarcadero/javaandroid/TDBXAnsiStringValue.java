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
 * Wraps the AnsiString type and allows it to be null
 *
 */

public class TDBXAnsiStringValue extends DBXValue {
	protected boolean ValueNull = false;
	private String DBXAnsiStringValue;

	public TDBXAnsiStringValue() {
		super();
		setDBXType(DBXDataTypes.WideStringType);
	}
	
	@Override
	public void setNull() {
		ValueNull = true;
		DBXAnsiStringValue = "";
	}

	public boolean isNull() {
		return ValueNull;
	}

	@Override
	public void SetAsString(String Value) throws DBXException {
		ValueNull = false;
		DBXAnsiStringValue = Value;
	}

	@Override
	public String GetAsString() throws DBXException {
		return DBXAnsiStringValue;
	}
}
