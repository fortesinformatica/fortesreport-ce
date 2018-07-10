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
     * Wraps the Date type and allows it to be null
     *
     */
    public class TDBXDateValue : DBXValue  {
	    protected bool ValueNull = false;
	    private int DBXDateValue;

        /**
    	 * Class constructor, initialized this {@link DBXDataTypes} like a DateType 
    	 */
	    public TDBXDateValue() : base(){
		    setDBXType(DBXDataTypes.DateType);
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
		    DBXDateValue = 0;
	    }

         /**
    	 * Sets the internal value with the value passed
    	 */
        public override void SetAsTDBXDate(int Value)
        {		
		    DBXDateValue = Value;
		    ValueNull = false;
	    }

         /**
    	 * Returns the internal value
    	 */
        public override int GetAsTDBXDate()
        {
		    return DBXDateValue;
	    }

    }
}
