//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

namespace Embarcadero.Datasnap.WindowsPhone7
{
    /**
     * 
     * TDataSet is the base class for all dataset components that represent data in rows and columns.
     *
     */
    public class TDataSet : TDBXReader {
	    public TDataSet(TParams parameters, TJSONObject value) : base(parameters, value) {
	    }

      	/**
    	 *  Returns a TDataSet created by the information contained in the JSONObject
    	 * @param value a JSONObject that contains the parameters for create the TDataSet  
    	 * @return return the TDataSet object created
    	 */
	 
	    public static new TDataSet createFrom(TJSONObject value){
		    TParams parameters = TParams.CreateParametersFromMetadata(value
                    .getJSONArray("table"));
		    TDataSet dst = new TDataSet(parameters, value);
		    return dst;
	    }
    }
}
