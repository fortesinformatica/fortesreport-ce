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
 * Wraps the {@link TStream} type and allows it to be null
 *
 */

public class TDBXStreamValue extends DBXValue {
	protected boolean ValueNull = false;
	
	public TDBXStreamValue() {
		super();
		setDBXType(DBXDataTypes.BinaryBlobType);
	}
	
	public void setNull() {
		ValueNull = true;
		streamValue = null;
	}

	public boolean isNull() {
		return ValueNull;
	}
	
	@Override
	public void SetAsStream(TStream Value) {
		streamValue = Value;
	}

	@Override
	public TStream GetAsStream() throws DBXException {
		return streamValue;
	}

}
