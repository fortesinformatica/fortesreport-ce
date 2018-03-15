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
     * Contains and returns the boolean value true like a JSON . 
     *
     */

    public class TJSONTrue : TJSONValue {    
	
	
	/**
	 * Returns the String "true" 
	 * @return String
	 */
	public String asJSONString() {
		return "true";
	}

	public override Object getInternalObject() {
		return true;
	}

	public override JSONValueType getJsonValueType() {
		return JSONValueType.JSONTrue;
	}

	public override String ToString() {
		return asJSONString();
	}
}
}
