//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

package com.embarcadero.javablackberry;


/**
 * Interface that can be implemented by objects that know how to serialize themselves to {@link JSONObject}.
 */

public interface JSONSerializable {
	public TJSONObject asTJSONObject() throws DBXException;	
}