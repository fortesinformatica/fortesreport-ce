//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

using System;

namespace Embarcadero.Datasnap.WindowsPhone7
{

/**
 * Wraps the {@link TDBXReader} and allows it to be null.
 */
 
    public class TDBXReaderValue : DBXValue 
    {
	    protected bool ValueNull = false;
	
	    /**
    	 * Class constructor, initialized this {@link DBXDataTypes} like a TableType 
    	 */
	    public TDBXReaderValue() : base()
        {
		    setDBXType(DBXDataTypes.TableType);
	    }
	
	    /**
    	* Sets this object to null
    	*/
	    public override void setNull() 
        {
		    ValueNull = true;
		    objectValue = null;
	    }

        /**
	     * Returns true if this object is null false otherwise.
	    */
	    public override bool isNull() {
		    return ValueNull;
	    }
	
	    public override void SetAsTable(TableType Value) {
		    objectValue = (Object) Value;
	    }

	    public override Object GetAsTable() 
        {
		    return objectValue;
	    }
	
	     /**
    	 * Returns the internal value
    	 */
	    public TDBXReader GetAsDBXReader()
        {
		    return (TDBXReader) objectValue;
	    }
	
	     /**
    	 * Sets the internal value with the value passed
    	 */
	    public void SetAsDBXReader(TableType Value) {
		    SetAsTable(Value);
	    }
    }
}
