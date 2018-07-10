//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

package com.embarcadero.javaandroid;

/**
 * Base class to represent parameters to send to the server.
 * 
 */
public class DBXValueType {
	private String Name;
	private String Caption;
	private int Ordinal;
	private int SubType;
	private long Size;
	private long Precision;
	private int Scale;
	private int ChildPosition;
	private boolean Nullable;
	private int ParameterDirection;
	private boolean Hidden;
	private boolean ValueParameter;
	private boolean Literal;

	/**
	 * Sets the name
	 * 
	 * @param name
	 */
	public void setName(String name) {
		Name = name;
	}

	/**
	 * Gets the name
	 * 
	 * @return
	 */
	public String getName() {
		return Name;
	}

	/**
	 * sets the caption
	 * 
	 * @param caption
	 */
	public void setCaption(String caption) {
		Caption = caption;
	}

	/**
	 * Gets the caption
	 * 
	 * @return
	 */
	public String getCaption() {
		return Caption;
	}

	/**
	 * Sets ordinal size.
	 * 
	 * @param ordinal
	 */
	public void setOrdinal(int ordinal) {
		Ordinal = ordinal;
	}

	/**
	 * Gets ordinal size.
	 * 
	 * @return
	 */
	public int getOrdinal() {
		return Ordinal;
	}

	public void setDataType(int dataType) throws DBXException {
		throw new DBXException("Must be overridden in the descendant classes");
	}

	public int getDataType() throws DBXException {
		throw new DBXException("Must be overridden in the descendant classes");
	}

	/**
	 * Sets the subType
	 * 
	 * @param subType
	 */
	public void setSubType(int subType) {
		SubType = subType;
	}

	/**
	 * Gets the subType
	 * 
	 * @return
	 */
	public int getSubType() {
		return SubType;
	}

	/**
	 * Sets the size
	 * 
	 * @param size
	 */
	public void setSize(long size) {
		Size = size;
	}

	/**
	 * Gets the size
	 * 
	 * @return
	 */
	public long getSize() {
		return Size;
	}

	/**
	 * Sets the precision
	 * 
	 * @param precision
	 */
	public void setPrecision(long precision) {
		Precision = precision;
	}

	/**
	 * Gets the precision
	 * 
	 * @return
	 */
	public long getPrecision() {
		return Precision;
	}

	/**
	 * Sets the scale
	 * 
	 * @param scale
	 */
	public void setScale(int scale) {
		Scale = scale;
	}

	/**
	 * Gets the scale
	 * 
	 * @return
	 */
	public int getScale() {
		return Scale;
	}

	/**
	 * Sets the child position
	 * 
	 * @param childPosition
	 */
	public void setChildPosition(int childPosition) {
		ChildPosition = childPosition;
	}

	/**
	 * Gets the child position
	 * 
	 * @return
	 */
	public int getChildPosition() {
		return ChildPosition;
	}

	/**
	 * Sets parameter direction
	 * 
	 * @param parameterDirection
	 */
	public void setParameterDirection(int parameterDirection) {
		ParameterDirection = parameterDirection;
	}

	/**
	 * Gets parameter direction
	 * 
	 * @return
	 */
	public int getParameterDirection() {
		return ParameterDirection;
	}

	/**
	 * Gets if the value is nullable
	 * 
	 * @return
	 */
	public boolean getNullable() {
		return Nullable;
	}

	/**
	 * Specifies whether the value can be nullable
	 * 
	 * @param nullable
	 */
	public void setNullable(boolean nullable) {
		Nullable = nullable;
	}

	/**
	 * Returns a boolean that represents if the param is hidden
	 * 
	 * @return
	 */
	public boolean getHidden() {
		return Hidden;
	}

	/**
	 * Specifies if the param is hidden
	 * 
	 * @param hidden
	 */
	public void setHidden(boolean hidden) {
		Hidden = hidden;
	}

	/**
	 * Returns a boolean that represents if it is a value parameter
	 * 
	 * @return
	 */
	public boolean getValueParameter() {
		return ValueParameter;
	}

	/**
	 * Specifies if it is a value parameter
	 * 
	 * @param valueParameter
	 */
	public void setValueParameter(boolean valueParameter) {
		ValueParameter = valueParameter;
	}

	/**
	 * Returns a boolean that represents if it is a literal
	 * 
	 * @return
	 */
	public boolean getLiteral() {
		return Literal;
	}

	/**
	 * Specifies if it is literal
	 * 
	 * @param literal
	 */
	public void setLiteral(boolean literal) {
		Literal = literal;
	}
}
