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
     *  Contains the possible directions that can take a {@link DSRESTParameter},
     *	are used as a comparison to understand the direction of the parameters (Unknown,Input,Output,InputOutput,ReturnValue)
     *	so then you can use it in the right way.
     *
     */
    public sealed class DSRESTParamDirection {
	public const int Unknown = 0;
	public const int Input = 1;
	public const int Output = 2;
	public const int InputOutput = 3;
    public const int ReturnValue = 4;
}

}
