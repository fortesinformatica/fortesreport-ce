//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

package com.embarcadero.javaandroid;

/**
 * Represents the ancestor class for the TJSON classes .
 */

public abstract class TJSONValue {

	/**
	 * A String that represents the JSON Null String that is "null"
	 */
	final protected String NullString = "null";

	/**
	 * Returns the specified internal JSONValue wrapped
	 * 
	 * @return the internal object
	 */
	public abstract Object getInternalObject();

	/**
	 * Returns the specified {@link JSONValueType} of the internal JSONValue
	 * wrapped
	 * 
	 * @return
	 */
	public abstract JSONValueType getJsonValueType();

	/**
	 * Returns the JSON String representation for this object
	 */
	public abstract String toString();
}
