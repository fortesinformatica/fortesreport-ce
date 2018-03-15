//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

package com.embarcadero.javaandroid;

/**
 * 
 * Interface that can be implemented by objects that know how to serialize themselves to {@link JSONObject}.
 *
 */

public interface JSONSerializable {
	public TJSONObject asJSONObject() throws DBXException;	
}
