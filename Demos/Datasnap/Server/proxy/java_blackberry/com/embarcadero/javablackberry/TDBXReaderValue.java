//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

package com.embarcadero.javablackberry;

/**
 * Wrap the {@link TDBXReader} and allows it to be null.
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
	
	public void SetAsTable(TableType Value) {
		objectValue = (Object) Value;
	}

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
