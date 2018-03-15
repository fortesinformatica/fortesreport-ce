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
     * 
     * Wraps the TimeStamp type and allows it to be null
     *
     */
    public class TDBXTimeStampValue : DBXValue {
	protected bool ValueNull = false;
	private DateTime DBXTimeStampValue;

    /**
    * Class constructor, initialized this {@link DBXDataTypes} like a TimeStampType 
    */
	public TDBXTimeStampValue() : base() {
		setDBXType(DBXDataTypes.TimeStampType);
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
		DBXTimeStampValue = DateTime.MinValue;
	}

     /**
     * Sets the internal value with the value passed
     */
    public override void SetAsTimeStamp(DateTime Value)
    {
		DBXTimeStampValue = Value;
		ValueNull = false;
	}

    /**
    * Returns the internal value
    */
    public override DateTime GetAsTimeStamp()
    {
		return DBXTimeStampValue;
	}

}
}
