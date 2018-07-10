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
 * TDBXReader provides a unidirectional reader for a collection of database
 * rows.
 * 
 */

public class TDBXReader implements JSONSerializable, TableType {
	protected long currentPosition = -1;
	private TParams columns;
	private TJSONObject internalDataStore;

	protected void setParameters(TParams params) {
		columns = params;
	}

	public TDBXReader(TParams params, TJSONObject value) {
		super();
		internalDataStore = value;
		setParameters(params);
	}

	public TParams getColumns() {
		return columns;
	}

	public DBXWritableValue getValue(int position) {
		return columns.getParameter(position).getValue();
	}

	public DBXWritableValue getValue(String name) throws DBXException {
		return columns.getParamByName(name).getValue();
	}

	public boolean next() {
		currentPosition++;
		try {
			return TParams.LoadParametersValues(this.columns,
					this.internalDataStore, (int) currentPosition);
		} catch (Exception ex) {
			return false;
		}
	}

	public static TDBXReader createFrom(TJSONObject value) throws DBXException {
		TParams params = TParams.CreateParametersFromMetadata(value
				.getJSONArray("table"));
		TDBXReader rdr = new TDBXReader(params, value);
		return rdr;
	}

	public TJSONObject asJSONObject() throws DBXException {
		TJSONObject result = null;
		long lastPosition = currentPosition;
		try {
			reset();
			result = DBXJSONTools.DBXReaderToJSONObject(this);
		} finally {
			currentPosition = lastPosition;
		}
		return result;
	}

	public void reset() {
		currentPosition = -1;
	}

}
