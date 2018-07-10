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
     * Interface that can be implemented by objects that know how to serialize themselves to {@link JSONObject}.
     *
     */
     
    public interface JSONSerializable
    {
        TJSONObject asJSONObject();
    }

}
