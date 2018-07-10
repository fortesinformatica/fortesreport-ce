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
 * Wrap the {@link TStream} type and allows it to be null
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
	
	public void SetAsStream(TStream Value) {
		streamValue = Value;
	}

	public TStream GetAsStream() throws DBXException {
		return streamValue;
	}

}
