//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

using System.IO;
using Newtonsoft.Json.Linq;
using System;

namespace Embarcadero.Datasnap.WindowsPhone7
{

    /**
     * 
     * TStream is the base class type for stream objects that can read from or write to various kinds of storage media, such as disk files, dynamic memory, and so on.
     *
     */
    public class TStream : MemoryStream {

    	/**
    	 * Class constructor , create a new TStream initialized with the specified array of byte.
    	 * @param buf
    	 */
	    public TStream(byte[] buf) : base(buf) {
	    }

    	/**
    	 * Returns a new TStream create by the value contained in the specified {@link JSONArray}
    	 * @param value
    	 * @return TStream
    	 */
	    public static TStream CreateFrom(TJSONArray value)
        {
		    byte[] b1 = new byte[value.size()];
            for (int i = 0; i < value.size(); i++)
            {
			    b1[i] = Convert.ToByte(value.getInt(i).Value);
		    }
		    return new TStream(b1);
	    }
	   
	   	/**
    	 * Returns this Object as an array of byte.
    	 * @return byte[]
    	 */
        public byte[] asByteArray()
        {
            return ToArray();
        }
    }
}
