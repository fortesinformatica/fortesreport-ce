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
    public class TJSONString : TJSONValue {
	    protected String value;

        /**
    	 * Class constructor, initialized the internal value to null
    	 */
	    public TJSONString() : base()
        {
		    value = null;
	    }

       /**
    	 * Class constructor, initialized the internal value with the value passed
    	 */
	    public TJSONString(String value) : base()
        {
		    this.value = value;
	    }

	    public override Object getInternalObject() {
		    return value;
	    }

	    public override String ToString() {
		    if (value != null)
			    return "\"" + value + "\"";
		    return NullString;
	    }

	    public override JSONValueType getJsonValueType() {
		    return JSONValueType.JSONString;
	    }
	
		/**
    	 * Gets the internal value 
    	 * @return String
    	 */
	    public String getValue(){
		    return value;
	    }
    }
}
