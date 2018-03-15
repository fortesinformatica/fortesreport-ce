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
 * Wraps the Single type and allows it to be null
 *
 */

public class TDBXSingleValue extends DBXValue {

	protected boolean ValueNull = false;
	private float DBXSingleValue;
	
	public TDBXSingleValue() {
		super();
		setDBXType(DBXDataTypes.SingleType);
	}
	
	public boolean isNull() {
		return ValueNull;
	}

	public void setNull() {
		ValueNull = true;
		DBXSingleValue = 0;
	}
	
	@Override
	public void SetAsSingle(float Value) throws DBXException {
		if (!isNull())
			DBXSingleValue = Value;
	}

	@Override
	public float GetAsSingle() throws DBXException {
		return DBXSingleValue;
	}

}
