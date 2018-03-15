//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

package com.embarcadero.javablackberry;

/**
 * 
 *  Contains the possible directions that can take a {@link DSRESTParameter},
 *	are used as a comparison to understand the direction of the parameters (Unknown,Input,Output,InputOutput,ReturnValue)
 *	so then you can use it in the right way.
 *
 */
public class DSRESTParamDirection {

	public static final int Unknown = 0;
	public static final int Input = 1;
	public static final int Output = 2;
	public static final int InputOutput = 3;
	public static final int ReturnValue = 4;

}