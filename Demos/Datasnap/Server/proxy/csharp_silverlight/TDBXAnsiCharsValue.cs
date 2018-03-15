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
     * Wraps the AnsiChars type and allows it to be null
     *
     */
    public class TDBXAnsiCharsValue : DBXValue {
	    protected bool ValueNull = false;
	    private String DBXStringValue;

        
    	/**
    	 * Class constructor, initialized this {@link DBXDataTypes} like a WideStringType 
    	 */
	    public TDBXAnsiCharsValue() : base() {
		    setDBXType(DBXDataTypes.WideStringType);
	    }

        /**
    	 * Sets this object to null
    	 */
        public override void setNull()
        {
		    ValueNull = true;
		    DBXStringValue = "";
	    }

       /**
	     * Returns true if this object is null false otherwise.
	    */
        public override bool isNull()
        {
		    return ValueNull;
	    }

        /**
    	 * Sets the internal value with the value passed
    	 */
        public override void SetAsString(String Value)
        {
		    ValueNull = false;
		    DBXStringValue = Value;
	    }

        /**
    	 * Returns the internal value
    	 */
        public override String GetAsString()
        {
		    return DBXStringValue;
	    }
    }
}
