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
     * Wraps the Int32 type and allows it to be null
     *
     */
    public class TDBXInt32Value : DBXValue {
	    protected bool ValueNull = false;
	    private int DBXInt32Value;	
	
	
	    	/**
    	 * Class constructor, initialized this {@link DBXDataTypes} like a Int32Type 
    	 */
	    public TDBXInt32Value() : base() {
		    setDBXType(DBXDataTypes.Int32Type);
	    }
	
	    /**
    	 * Sets this object to null
    	 */
	    public override void setNull() {
		    ValueNull = true;
		    DBXInt32Value = 0;
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
        public override void SetAsInt32(int Value)
        {
		    ValueNull = false;
		    DBXInt32Value = Value;
	    }

        /**
    	 * Returns the internal value
    	 */
        public override int GetAsInt32()
        {
		    return DBXInt32Value;
	    }	
    }
}
