//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

namespace Embarcadero.Datasnap.WindowsPhone7
{
    public class DSAdminRestClient
    {
        private DSRESTConnection Connection = null;

        protected DSRESTConnection getConnection()
        {
            return Connection;
        }

        public DSAdminRestClient(DSRESTConnection Connection) : base()
        {
            this.Connection = Connection;
        }
    }
}
