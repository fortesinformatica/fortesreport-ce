//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

package com.embarcadero.javablackberry;

import java.util.Date;

/**
 * 
 * Wrap the TimeStamp type and allows it to be null
 *
 */


public class TDBXTimeStampValue extends DBXValue {
	protected boolean ValueNull = false;
	private Date DBXTimeStampValue;

	public TDBXTimeStampValue() {
		super();
		setDBXType(DBXDataTypes.TimeStampType);
	}
	
	public boolean isNull() {
		return ValueNull;
	}

	public void setNull() {
		ValueNull = true;
		DBXTimeStampValue = null;
	}
	
	public void SetAsTimeStamp(Date Value) throws DBXException {
		DBXTimeStampValue = Value;
		ValueNull = false;
	}

	public Date GetAsTimeStamp() throws DBXException {
		return DBXTimeStampValue;
	}

}
