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
 * Wraps a binary-compressed decimal values and allows it to be null.
 *
 */

public class TDBXBcdValue : DBXValue {
	protected bool ValueNull = false;	

	public TDBXBcdValue():base() {
		setDBXType(DBXDataTypes.BcdType);
	}
	
     /**
     * Returns true if this object is null false otherwise.
    */
	public override bool isNull() {
		return ValueNull;
	}
	
     /**
	 * Sets this object to null
	 */	
	public override void setNull() {
		ValueNull = true;
		bcdValue = 0;
	}
	
	
	/**
	 * Sets this object to binary-coded decimal
	 */     	
	public override void SetAsBcd(double Value) {
		bcdValue = Value;
	}
	
	/*
	*  return the binary-coded decimal value
	*/
	public override double GetAsBcd() {
		return bcdValue;
	}
}

}
