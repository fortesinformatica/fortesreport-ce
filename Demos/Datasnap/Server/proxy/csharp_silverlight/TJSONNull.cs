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
     * Contains and returns the 'null' value like a JSON . 
     *
     */
    public class TJSONNull : TJSONValue {

	    public override JSONValueType getJsonValueType() {
		    return JSONValueType.JSONNull;
	    }	
	
		/**
    	 * Returns the String "null" 
    	 * @return String "null" 
    	 */
	    public String asJSONString() {
		    return "null";
	    }
	
	    public override Object getInternalObject() {
		    return null;
	    }

	    public override String ToString() {
		    return asJSONString();
	    }	
    }
}
