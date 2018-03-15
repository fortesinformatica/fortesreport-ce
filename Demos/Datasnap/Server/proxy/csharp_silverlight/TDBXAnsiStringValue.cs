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
     * Wraps the AnsiString type and allows it to be null
     *
     */

    public class TDBXAnsiStringValue : DBXValue {
	    protected bool ValueNull = false;
	    private String DBXAnsiStringValue;

        /**
    	 * Class constructor, initialized this {@link DBXDataTypes} like a WideStringType 
    	 */
	    public TDBXAnsiStringValue() : base() {
		    setDBXType(DBXDataTypes.WideStringType);
	    }

         /**
    	 * Sets this object to null
    	 */
        public override void setNull()
        {
		    ValueNull = true;
		    DBXAnsiStringValue = "";
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
		    DBXAnsiStringValue = Value;
	    }

        /**
    	 * Returns the internal value
    	 */
        public override String GetAsString()
        {
		    return DBXAnsiStringValue;
	    }
    }
}
