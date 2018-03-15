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
     * Wraps the Int64 type and allows it to be null
     *
     */
    public class TDBXInt64Value : DBXValue {

	    protected bool ValueNull = false;
	    private long DBXInt64Value;
	
	        
    	/**
    	 * Class constructor, initialized this {@link DBXDataTypes} like a Int64Type 
    	 */
	    public TDBXInt64Value() : base() {
		    setDBXType(DBXDataTypes.Int64Type);
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
		    DBXInt64Value = 0;
	    }

        /**
    	 * Sets the internal value with the value passed
    	 */
        public override void SetAsInt64(long Value)
        {
		    DBXInt64Value = Value;
		    ValueNull = false;
	    }

         /**
    	 * Returns the internal value
    	 */
        public override long GetAsInt64()
        {
		    return DBXInt64Value;
	    }

    }
}
