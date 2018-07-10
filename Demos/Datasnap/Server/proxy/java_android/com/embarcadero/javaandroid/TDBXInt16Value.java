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
 * Wraps the Int16 type and allows it to be null
 *
 */

public class TDBXInt16Value extends DBXValue {
	protected boolean ValueNull = false;
	private int DBXInt16Value;	
	
	public TDBXInt16Value() {
		super();
		setDBXType(DBXDataTypes.Int16Type);
	}
	
	
	@Override
	public void setNull() {
		ValueNull = true;
		DBXInt16Value = 0;
	}

	public boolean isNull() {
		return ValueNull;
	}
	
	@Override
	public void SetAsInt16(int Value) throws DBXException {
		ValueNull = false;
		DBXInt16Value = Value;
	}
	
	@Override
	public int GetAsInt16() throws DBXException {
		return DBXInt16Value;
	}	

}
