//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

package com.embarcadero.javaandroid;

import java.util.Date;

/**
 * 
 * Wraps the TimeStamp type and allows it to be null
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

	@Override
	public void setNull() {
		ValueNull = true;
		DBXTimeStampValue = null;
	}
	
	@Override
	public void SetAsTimeStamp(Date Value) throws DBXException {
		DBXTimeStampValue = Value;
		ValueNull = false;
	}

	@Override
	public Date GetAsTimeStamp() throws DBXException {
		return DBXTimeStampValue;
	}

}
