//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

package com.embarcadero.javablackberry;

/**
 *  Represents and incorporates all relevant information that describe a {@link DSRESTParameter}.
 */

public class DSRESTParameterMetaData
{
    public String Name;
    public int Direction;
    public int DBXType;
    public String TypeName;
    
    /**
	 * The constructor class, builds a new DSRESTParameterMetaData assigning the values ​​passed as input.
	 * 
	 * @param Name the name of the parameter.
	 * @param Direction a value of type integer that represents the parameter direction, respecting the conventions of the {@link DSRESTParamDirection}.  
	 * @param DBXType an integer value that represents the DBXType of this parameter , respecting the conventions of the {@link DBXValueType}.
	 * @param TypeName a string that contains the TypeName of this parameter.
	 */
    
    public DSRESTParameterMetaData(String Name, int Direction, int DBXType, String TypeName)
    {
    	super();
        this.Name = Name;
        this.Direction = Direction;
        this.DBXType = DBXType;
        this.TypeName = TypeName;
    }
}
