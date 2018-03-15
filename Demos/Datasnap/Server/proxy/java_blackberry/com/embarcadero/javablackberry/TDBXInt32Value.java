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
 * Wrap the Int32 type and allows it to be null
 *
 */

public class TDBXInt32Value extends DBXValue {
	protected boolean ValueNull = false;
	private int DBXInt32Value;	
	
	public TDBXInt32Value() {
		super();
		setDBXType(DBXDataTypes.Int32Type);
	}
	
	
	public void setNull() {
		ValueNull = true;
		DBXInt32Value = 0;
	}

	public boolean isNull() {
		return ValueNull;
	}
	
	public void SetAsInt32(int Value) throws DBXException {
		ValueNull = false;
		DBXInt32Value = Value;
	}
	
	public int GetAsInt32() throws DBXException {
		return DBXInt32Value;
	}	
}
