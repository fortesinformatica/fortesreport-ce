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
   * Represents json pair objects. A JSONPair is an object that has a name an
   * value that represents a {@link TJSONValue}.
   * 
   */
    public class TJSONPair
    {
        
        /**
    	 * Class constructor, initializes the TJSONPair with a TJSONValue with the
    	 * specified name. 
    	 * @param String name
    	 * @param TJSONValue value
    	 */
        public TJSONPair(String name, TJSONValue value)
        {
            this.name = name;
            this.value = value;
        }

        /**
    	 * Initializes the TJSONPair with a String with the specified name. 
    	 * @param String name
    	 * @param String value
    	 */
        public TJSONPair(String name, String value)
        {
            this.name = name;
            this.value = new TJSONString(value);
        }

        public String name;
        public TJSONValue value;
    }
}
