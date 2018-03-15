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
     * Wraps the Time type and allows it to be null
     *
     */
    public class TDBXTimeValue : DBXValue {
	    protected bool ValueNull = false;
	    private int DBXTimeValue;

         /**
    	 * Class constructor, initialized this {@link DBXDataTypes} like a TimeType 
    	 */
	    public TDBXTimeValue() : base() {
		    setDBXType(DBXDataTypes.TimeType);
	    }

        /**
	     * Returns true if this object is null false otherwise.
	    */
        public override bool isNull()
        {
		    return ValueNull;
	    }

         /**
    	 * Sets this object to null
    	 */
        public override void setNull()
        {
		    ValueNull = true;
		    DBXTimeValue = 0;
	    }

         /**
    	 * Sets the internal value with the value passed
    	 */
        public override void SetAsTDBXTime(int Value)
        {
		    DBXTimeValue = Value;
		    ValueNull = false;
	    }

         /**
    	 * Returns the internal value
    	 */
        public override int GetAsTDBXTime()
        {
		    return DBXTimeValue;
	    }

    }
}
