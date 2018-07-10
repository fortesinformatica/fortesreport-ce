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
     * Base class to implement callbacks. The class is used to implement responses for callbacks . 
     *
     */
 
    public abstract class DBXCallback {	
    
    /**
	 * override this method to implement callbacks actions 
	 * @param pars
	 */
        public abstract TJSONValue Execute(TJSONValue value, int JSONType);
    }
}
