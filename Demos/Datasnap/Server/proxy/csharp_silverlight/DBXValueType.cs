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
    public class DBXValueType {
	    private String Name;
	    private String Caption;
	    private int Ordinal;
	    private int SubType;
	    private long Size;
	    private long Precision;
	    private int Scale;
	    private int ChildPosition;
	    private bool Nullable;
	    private int ParameterDirection;
	    private bool Hidden;
	    private bool ValueParameter;
	    private bool Literal;
	
		/**
    	 * Sets the name 
    	 * @param name
    	 */
	    public void setName(String name) {
		    Name = name;
	    }
	   
	   	/**
    	 * Gets the name 
    	 * @return String
    	 */
	    public String getName() {
		    return Name;
	    }
	   
	   	/**
    	 * sets the caption 
    	 * @param caption
    	 */
	    public void setCaption(String caption) {
		    Caption = caption;
	    }
	   
	   	/**
    	 * Gets the caption 
    	 * @return String
    	 */
	    public String getCaption() {
		    return Caption;
	    }
	   
	   	/**
    	 * Sets ordinal size. 
    	 * @param int ordinal
    	 */
	    public void setOrdinal(int ordinal) {
		    Ordinal = ordinal;
	    }
	   
	/**
	 * Gets ordinal size. 
	 * @return int 
	 */
	    public int getOrdinal() {
		    return Ordinal;
	    }
	
	    public virtual void setDataType(int dataType){
		    throw new DBXException("Must be overridden in the descendant classes");
	    }

        public virtual int getDataType()
        {
		    throw new DBXException("Must be overridden in the descendant classes");
	    }
	
		/**
    	 * Sets the subType 
    	 * @param subType
    	 */
	    public void setSubType(int subType) {
		    SubType = subType;
	    }
	   
	   	/**
    	 * Gets the subType 
    	 * @return int
    	 */
	    public int getSubType() {
		    return SubType;
	    }
	   
	   	/**
    	 * Sets the size 
    	 * @param size
    	 */
	    public void setSize(long size) {
		    Size = size;
	    }
	   
	   	/**
    	 * Gets the size 
    	 * @return long
    	 */
	    public long getSize() {
		    return Size;
	    }
	   
	   	/**
    	 * Sets the precision 
    	 * @param precision
    	 */
	    public void setPrecision(long precision) {
		    Precision = precision;
	    }
	   
	   	/**
    	 * Gets the precision 
    	 * @return long
    	 */
	    public long getPrecision() {
		    return Precision;
	    }
	   
	   	/**
    	 * Sets the scale 
    	 * @param scale
    	 */
	    public void setScale(int scale) {
		    Scale = scale;
	    }
	   
	   	/**
    	 * Gets the scale 
    	 * @return int
    	 */
	    public int getScale() {
		    return Scale;
	    }
	   
	   
	   	/**
    	 * Sets the child position 
    	 * @param childPosition
    	 */
	    public void setChildPosition(int childPosition) {
		    ChildPosition = childPosition;
	    }
	   
	   	/**
    	 * Gets the child position 
    	 * @return int
    	 */
	    public int getChildPosition() {
		    return ChildPosition;
	    }
	   
	   	/**
    	 * Sets parameter direction 
    	 * @param parameterDirection
    	 */
	    public void setParameterDirection(int parameterDirection) {
		    ParameterDirection = parameterDirection;
	    }
	   
	   
	   	/**
    	 * Gets parameter direction 
    	 * @return int
    	 */
	    public int getParameterDirection() {
		    return ParameterDirection;
	    }
	   
	   	/**
    	 * Gets if the value is nullable 
    	 * @return bool
    	 */
	    public bool getNullable() {
		    return Nullable;
	    }
	   
	   
    	/**
    	 * Specifies whether the value can be nullable 
    	 * @param nullable
    	 */
	    public void setNullable(bool nullable) {		
		    Nullable = nullable;
	    }
	   
	   	/**
    	 * Returns a boolean that represents if the param is hidden 
    	 * @return bool
    	 */
	    public bool getHidden() {
		    return Hidden;
	    }
	   
	   	/**
    	 * Specifies if the param is hidden 
    	 * @param hidden
    	 */
	    public void setHidden(bool hidden) {		
		    Hidden = hidden;
	    }
	   
	   	/**
    	 * Returns a boolean that represents if it is a value parameter 
    	 * @return bool
    	 */
	    public bool getValueParameter() {
		    return ValueParameter;
	    }
	   
	   	/**
    	 * Specifies if it is a value parameter 
    	 * @param valueParameter
    	 */
	    public void setValueParameter(bool valueParameter) {		
		    ValueParameter = valueParameter;
	    }
    
    	/**
    	 * Returns a boolean that represents if it is a literal 
    	 * @return bool
    	 */		
	    public bool getLiteral() {
		    return Literal;
	    }
	   
	   	/**
    	 * Specifies if it is literal 
    	 * @param literal
    	 */
	    public void setLiteral(bool literal) {		
		    Literal = literal;
	    }			
    }
}
