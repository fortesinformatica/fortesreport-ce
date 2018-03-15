//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

package com.embarcadero.javaandroid;

/**
 * Wraps the {@link TDBXReader} and allows it to be null.
 */

public class TDBXReaderValue extends DBXValue {
	protected boolean ValueNull = false;
	
	public TDBXReaderValue() {
		super();
		setDBXType(DBXDataTypes.TableType);
	}
	
	public void setNull() {
		ValueNull = true;
		objectValue = null;
	}

	public boolean isNull() {
		return ValueNull;
	}
	
	@Override
	public void SetAsTable(TableType Value) {
		objectValue = (Object) Value;
	}

	@Override
	public Object GetAsTable() throws DBXException {
		return objectValue;
	}
	
	public TDBXReader GetAsDBXReader() throws DBXException {
		return (TDBXReader) objectValue;
	}
	
	public void SetAsDBXReader(TableType Value) {
		SetAsTable(Value);
	}

}
