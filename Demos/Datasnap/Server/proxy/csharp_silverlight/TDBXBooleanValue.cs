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
     * Wraps the Boolean type and allows it to be null
     *
     */

    public class TDBXBooleanValue : DBXValue {

	    protected bool ValueNull = false;
	    private bool DBXBooleanValue;
	
	    /**
    	 * Class constructor, initialized this {@link DBXDataTypes} like a BooleanType 
    	 */
	    public TDBXBooleanValue() : base() {
		    setDBXType(DBXDataTypes.BooleanType);
	    }

        /**
    	 * Sets this object to null
    	 */
        public override void setNull()
        {
		    ValueNull = true;
		    DBXBooleanValue = false;
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
        public override void SetAsBoolean(bool Value)
        {		
		    DBXBooleanValue = Value;
		    ValueNull = false;
	    }

        /**
    	 * Returns the internal value
    	 */
        public override bool GetAsBoolean()
        {
		    return DBXBooleanValue;
	    }


    }
}
