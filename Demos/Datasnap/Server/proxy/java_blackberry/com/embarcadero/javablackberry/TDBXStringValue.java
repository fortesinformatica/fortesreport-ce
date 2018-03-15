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
 * Wrap the String type and allows it to be null
 *
 */


public class TDBXStringValue extends DBXValue{
	protected boolean ValueNull = false;
	private String DBXStringValue;	
	
	public TDBXStringValue() {
		super();
		setDBXType(DBXDataTypes.WideStringType);
	}
	
	public void setNull() {
		ValueNull = true;
		DBXStringValue = "";
	}

	public boolean isNull() {
		return ValueNull;
	}
	
	public void SetAsString(String Value) throws DBXException {
		ValueNull = false;
		DBXStringValue = Value;
	}
	
	public String GetAsString() throws DBXException {
		return DBXStringValue;
	}
}
