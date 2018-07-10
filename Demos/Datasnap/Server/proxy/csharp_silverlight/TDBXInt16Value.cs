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
     * Wraps the Int16 type and allows it to be null
     *
     */
    public class TDBXInt16Value : DBXValue {
	    protected bool ValueNull = false;
	    private int DBXInt16Value;	
	            
    	/**
    	 * Class constructor, initialized this {@link DBXDataTypes} like a Int16Type 
    	 */
	    public TDBXInt16Value() : base() {
		    setDBXType(DBXDataTypes.Int16Type);
	    }

         /**
    	 * Sets this object to null
    	 */
        public override void setNull()
        {
		    ValueNull = true;
		    DBXInt16Value = 0;
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
        public override void SetAsInt16(int Value)
        {
		    ValueNull = false;
		    DBXInt16Value = Value;
	    }

        /**
    	 * Returns the internal value
    	 */
        public override int GetAsInt16()
        {
		    return DBXInt16Value;
	    }	

    }
}
