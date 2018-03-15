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
 * Wrap the Int8 type and allows it to be null
 *
 */

public class TDBXInt8Value extends DBXValue {

	protected boolean ValueNull = false;
	private int DBXIntValue;
	
	public TDBXInt8Value() {
		super();
		setDBXType(DBXDataTypes.Int8Type);
	}
	
	public void setNull() {
		ValueNull = true;
		DBXIntValue = 0;
	}

	public boolean isNull() {
		return ValueNull;
	}
	
	public void SetAsInt8(int Value) throws DBXException {
		DBXIntValue = Value;
		ValueNull = false;
	}

	public int GetAsInt8() throws DBXException {
		return DBXIntValue;
	}

}
