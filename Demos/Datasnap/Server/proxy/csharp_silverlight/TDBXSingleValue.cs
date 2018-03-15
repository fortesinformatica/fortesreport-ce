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
     * Wraps the Single type and allows it to be null
     *
     */
    public class TDBXSingleValue : DBXValue {

	    protected bool ValueNull = false;
	    private float DBXSingleValue;
	
	    	/**
    	 * Class constructor, initialized this {@link DBXDataTypes} like a SingleType 
    	 */
	    public TDBXSingleValue() : base() {
		    setDBXType(DBXDataTypes.SingleType);
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
		    DBXSingleValue = 0;
	    }

         /**
    	 * Sets the internal value with the value passed
    	 */
        public override void SetAsSingle(float Value)
        {
		    if (!isNull())
			    DBXSingleValue = Value;
	    }

         /**
    	 * Returns the internal value
    	 */
        public override float GetAsSingle()
        {
		    return DBXSingleValue;
	    }

    }
}
