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
 * Wraps the Double type and allows it to be null
 *
 */

public class TDBXDoubleValue extends DBXValue {

	protected boolean ValueNull = false;
	private double DBXDoubleValue;
	

	public TDBXDoubleValue() {
		super();
		setDBXType(DBXDataTypes.DoubleType);
	}
	
	public boolean isNull() {
		return ValueNull;
	}

	@Override
	public void setNull() {
		ValueNull = true;
		DBXDoubleValue = 0;
	}
	
	public void SetAsDouble(double Value) throws DBXException {
		DBXDoubleValue = Value;
		ValueNull = false;
	}

	public double GetAsDouble() throws DBXException {
		return DBXDoubleValue;
	}

}
