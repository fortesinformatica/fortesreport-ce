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
     * Wraps the {@link TStream} type and allows it to be null
     *
     */
    public class TDBXStreamValue : DBXValue 
    {
	    protected bool ValueNull = false;
	
	    /**
    	 * Class constructor, initialized this {@link DBXDataTypes} like a BinaryBlobType 
    	 */
	    public TDBXStreamValue() : base()
        {
		    setDBXType(DBXDataTypes.BinaryBlobType);
	    }
	
	     /**
    	 * Sets this object to null
    	 */
	    public override void setNull() {
		    ValueNull = true;
		    streamValue = null;
	    }

        /**
	     * Returns true if this object is null false otherwise.
	    */
	    public override bool isNull() {
		    return ValueNull;
	    }
	
	    /**
    	 * Sets the internal value with the value passed
    	 */
	    public override void SetAsStream(TStream Value) {
		    streamValue = Value;
	    }

        /**
    	 * Returns the internal value
    	 */
	    public override TStream GetAsStream() 
        {
		    return streamValue;
	    }

    }
}
