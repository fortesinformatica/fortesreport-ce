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
     * Wraps the Double type and allows it to be null
     *
     */
    public class TDBXDoubleValue : DBXValue {

	    protected bool ValueNull = false;
	    private double DBXDoubleValue;
	
            	/**
    	 * Class constructor, initialized this {@link DBXDataTypes} like a DoubleType 
    	 */
	    public TDBXDoubleValue() : base() {
		    setDBXType(DBXDataTypes.DoubleType);
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
		    DBXDoubleValue = 0;
	    }

        /**
    	 * Sets the internal value with the value passed
    	 */
        public override void SetAsDouble(double Value)
        {
		    DBXDoubleValue = Value;
		    ValueNull = false;
	    }

         /**
    	 * Returns the internal value
    	 */
        public override double GetAsDouble()
        {
		    return DBXDoubleValue;
	    }

    }
}
