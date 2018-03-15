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
     * Contains and returns the boolean value false like a JSON .
     * 
     */
    public class TJSONFalse : TJSONValue {

	    public override JSONValueType getJsonValueType() {
		    return JSONValueType.JSONFalse;
	    }	
	
  	/**
  	 * Returns the String "false" 
  	 * @return String "false"
  	 */
	    public String asJSONString() {
		    return "false";
	    }

	    public override Object getInternalObject() {
		    return false;
	    }

	    public override String ToString() {
		    return asJSONString();
	    }

    }
}
