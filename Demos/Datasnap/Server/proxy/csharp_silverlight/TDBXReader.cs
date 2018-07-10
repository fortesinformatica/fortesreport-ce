//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

using System;

namespace Embarcadero.Datasnap.WindowsPhone7
{

    /**
     * 
     * TDBXReader provides a unidirectional reader for a collection of database
     * rows.
     * 
     */
    public class TDBXReader : JSONSerializable, TableType {
	    protected long currentPosition = -1;
	    private TParams columns;
	    private TJSONObject internalDataStore;

	    protected void setParameters(TParams parameters) {
		    columns = parameters;
	    }

	    public TDBXReader(TParams parameters, TJSONObject value) : base() {
		    internalDataStore = value;
		    setParameters(parameters);
	    }

	    public TParams getColumns() {
		    return columns;
	    }

	    public DBXWritableValue getValue(int position) {
		    return columns.getParameter(position).getValue();
	    }

	    public DBXWritableValue getValue(String name){
		    return columns.getParamByName(name).getValue();
	    }

	
	    public bool next() {
		    currentPosition++;
		    try {
			    return TParams.LoadParametersValues(this.columns,
					    this.internalDataStore, (int) currentPosition);
		    } catch (Exception) {
			    return false;
		    }		
	    }

	    public static TDBXReader createFrom(TJSONObject value){
		    TParams parameters = TParams.CreateParametersFromMetadata(value
                    .getJSONArray("table"));
            TDBXReader rdr = new TDBXReader(parameters, value);
		    return rdr;
	    }

	    public TJSONObject asJSONObject(){
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

        public void reset()
        {
            currentPosition = -1;
        }

    }
}
