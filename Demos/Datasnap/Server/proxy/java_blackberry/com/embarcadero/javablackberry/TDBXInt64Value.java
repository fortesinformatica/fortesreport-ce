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
 * Wrap the Int64 type and allows it to be null
 *
 */

public class TDBXInt64Value extends DBXValue {

	protected boolean ValueNull = false;
	private long DBXInt64Value;
	
	public TDBXInt64Value() {
		super();
		setDBXType(DBXDataTypes.Int64Type);
	}
	
	public boolean isNull() {
		return ValueNull;
	}

	public void setNull() throws DBXException {
		ValueNull = true;
		DBXInt64Value = 0;
	}
	
	
	public void SetAsInt64(long Value) throws DBXException {
		DBXInt64Value = Value;
		ValueNull = false;
	}

	public long GetAsInt64() throws DBXException {
		return DBXInt64Value;
	}

}
