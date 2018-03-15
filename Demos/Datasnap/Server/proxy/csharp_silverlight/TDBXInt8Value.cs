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
     * Wraps the Int8 type and allows it to be null
     *
     */

    public class TDBXInt8Value : DBXValue {

	    protected bool ValueNull = false;
	    private int DBXIntValue;
	
	    /**
    	 * Class constructor, initialized this {@link DBXDataTypes} like a Int8Type 
    	 */
	    public TDBXInt8Value() : base() {
		    setDBXType(DBXDataTypes.Int8Type);
	    }

        /**
    	 * Sets this object to null
    	 */
        public override void setNull()
        {
		    ValueNull = true;
		    DBXIntValue = 0;
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
        public override void SetAsInt8(int Value)
        {
		    DBXIntValue = Value;
		    ValueNull = false;
	    }

        /**
    	 * Returns the internal value
    	 */
        public override int GetAsInt8()
        {
		    return DBXIntValue;
	    }

    }
}
