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
   * Exception descendant to raise error in proxy
   *
   */
 
    class DBXException : Exception
    {
        public DBXException(String value): base(value) {
	    }

	    /**
	     * 
	     */
	    private const long serialVersionUID = 154857855606412539L;

    }
}
