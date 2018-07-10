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
 * Wraps a binary-compressed decimal values and allows it to be null.
 *
 */

public class TDBXBcdValue extends DBXValue {
	protected boolean ValueNull = false;	

	public TDBXBcdValue() {
		super();
		setDBXType(DBXDataTypes.BcdType);
	}
	
	public boolean isNull() {
		return ValueNull;
	}
	
	@Override
	public void setNull() {
		ValueNull = true;
		bcdValue = 0;
	}
	
	@Override
	public void SetAsBcd(double Value) throws DBXException {
		bcdValue = Value;
	}
	
	@Override
	public double GetAsBcd() throws DBXException {
		return bcdValue;
	}

}
