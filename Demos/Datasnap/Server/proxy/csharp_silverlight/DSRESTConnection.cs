//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Net;
using System.Text;
using System.Threading;
using Newtonsoft.Json.Linq;
using System.Windows.Threading;

namespace Embarcadero.Datasnap.WindowsPhone7
{
    /*! \mainpage Windows Phone 7 DataSnap Connector
     *
     * \section intro_sec Introduction
     *
     *   Windows Phone 7 DataSnap Connector is a framework that allows to connect
     *   to a Datasnap REST server. 
     *    
     *
     */

    /**
     * Allows you to manage and make REST requests based on the protocol, the target
     * host , the context and other features. Supports authentication and
     * authorization.
     * 
     * Use the properties and methods of DSRESTConnection to: <br>
     * &nbsp;&nbsp;&nbsp;&nbsp;- Set the general parameters of a connection as
     * target host and port, context, protocol , URL path etc... <br>
     * &nbsp;&nbsp;&nbsp;&nbsp;- Setting Username and Password for authentication
     * and authorization. <br>
     * &nbsp;&nbsp;&nbsp;&nbsp;- Execute a REST request from a {@link DSRESTCommand}
     * . <br>
     * &nbsp;&nbsp;&nbsp;&nbsp;- Rebuild and save the parameters contained in the
     * response.
     * 
     */

    public class DSRESTConnection
    {

        /**
         * This method returns the URL encoded string starting from the input string
         * URL. The characters encoding is UTF-8.
         * @param value the URL string to encode
         * @return the URL string encoded
         */

        private string encodeURIComponent(string value)
        {
            return URLUTF8Encoder.encode(value);
        }


        /** This method returns the URL encoded string starting from the input
      	 * DSRESTParameter
      	 * 
      	 * @param parameter
      	 *            the DSRESTParameter to encode
      	 * @return the DSRESTParameter encoded
      	 */
        private String encodeURIComponent(DSRESTParameter parameter)
        {
            return encodeURIComponent(parameter.getValue().ToString());
        }

        /**
      	 * Clone the current connection. The session is not cloned, so the cloned
      	 * connection will not have the same session as its parent.
      	 * 
      	 * @return the new DSRESTConnection
      	 */

        public DSRESTConnection Clone()
        {
            return Clone(false);
        }

        /**
         * Clone the current connection. The session is optionally included in the clone.
         * 
         * @param includeSession true to include session information in the new connection, false to exclude it.
         * 
         * @return the new DSRESTConnection
         */

        public DSRESTConnection Clone(bool includeSession)
        {
            DSRESTConnection connection = new DSRESTConnection();
            connection.setHost(this.getHost());
            connection.setPort(this.getPort());
            connection.setProtocol(this.getProtocol());
            connection.setUserName(this.getUserName());
            connection.setPassword(this.getPassword());
            connection.setUrlPath(this.getUrlPath());
            connection.setConnectionTimeout(this.getConnectionTimeout());
            if (includeSession)
            {
                connection.setSessionID(this.getSessionID());
                connection.SessionIDExpires = this.SessionIDExpires;
            }
            return connection;
        }

        protected Uri uri = null;
        private int Port = 0;
        private String UrlPath = "";
        private String Host = "";
        private String Protocol = "";
        private String Context = "";
        private String UserName = "";
        private String Password = "";

        private int ConnectionTimeout = 0;

        public SynchronizationContext syncContext = null;

        private String SessionID;
        private long SessionIDExpires;

        public void setBaseURI(Uri uri)
        {
            this.uri = uri;
        }

        public DSRESTConnection()
            : base()
        {
            CloseSession();
            syncContext = SynchronizationContext.Current;
        }

        /**
         * This method close the active session
         */

        public void CloseSession()
        {
            SessionID = null;
            SessionIDExpires = -1;
        }

        public DSRESTConnection(Uri uri)
            : base()
        {
            setBaseURI(uri);
            syncContext = SynchronizationContext.Current;
        }

        /**
         * Creates a brand new command that belong to this connection.
         * 
         * @return a new DSRESTCommand
         */
        /**
         * This method create a new DSRESTCommand
         * 
         * @return a new DSRESTCommand
         */

        public DSRESTCommand CreateCommand()
        {
            return new DSRESTCommand(this);
        }

        /**
      	 * Collects all the informations contained in this object like the target
      	 * host, the protocol; the more information contained in
      	 * {@link DSRESTCommand} input parameter as the type of request, the method
      	 * to call then to return the url to run
      	 * 
      	 * 
      	 * @param the
      	 *            specific DSRESTCommand
      	 * @return a requested url
      	 */

        private String BuildRequestURL(DSRESTCommand command)
        {
            String LPathPrefix = getUrlPath();
            int LPort = getPort();
            String LHost = getHost();
            String LMethodName = command.getText();
            String LProtocol = getProtocol();
            if (LProtocol.Equals(""))
                LProtocol = "http";
            if (LHost.Equals(""))
                LHost = "localhost";
            if (!LPathPrefix.Equals(""))
                LPathPrefix = "/" + LPathPrefix;
            String LPortString = "";
            if (LPort > 0)
                LPortString = ":" + Convert.ToInt32(LPort);
            if (command.getRequestType() == DSHTTPRequestType.GET
                || command.getRequestType() == DSHTTPRequestType.DELETE)
            {
                LMethodName = LMethodName.Substring(0, LMethodName.IndexOf(".")) + "/" + LMethodName.Substring(LMethodName.IndexOf(".") + 1);
                LMethodName = LMethodName.Replace("\"", "");
            }
            else
            {
                LMethodName = LMethodName.Substring(0, LMethodName.IndexOf(".")) + "/%22" + LMethodName.Substring(LMethodName.IndexOf(".") + 1) + "%22";
                LMethodName = LMethodName.Replace("\"", "%22");
            }
            String LContext = getContext();
            if (LContext.Equals(""))
                LContext = "datasnap/";
            String LUrl = LProtocol + "://" +
                    encodeURIComponent(LHost) + LPortString + LPathPrefix + "/" + LContext + "rest/" + LMethodName + "/";
            SessionID = getSessionID();
            return LUrl;
        }

        /**
         * Execute the request from a specific {@link DSRESTCommand} input, that
         * will contain useful information to construct the URL as the type of
         * request, the method to execute and the parameters to be passed. This
         * information be added to those contained in this object as protocol,
         * target host, context... They form the complete request to execute. This
         * method is need to pass parameters correctly or under the parameter
         * direction, it will be append on the url string or written in the body of
         * the request. Upon receipt of the response will have to check the
         * correctness of the received parameters and set them in the
         * {@link DSRESTCommand}.
         * 
         * @param command the specific {@link DSRESTCommand}
         * @param Sender DSAdmin
         * @param callback Delegate
         * @param EXCallback Delegate       	 
         */
        public void execute(DSRESTCommand command, DSAdmin Sender, Delegate callback, Delegate EXCallback = null)
        {
            TJSONArray _parameters = null;
            String URL = BuildRequestURL(command);
            LinkedList<DSRESTParameter> ParametersToSend = new LinkedList<DSRESTParameter>();
            if (command.getParameters().Count > 0)
                foreach (DSRESTParameter parameter in command.getParameters())
                {
                    if (parameter.Direction == DSRESTParamDirection.Input ||
                            parameter.Direction == DSRESTParamDirection.InputOutput)
                        ParametersToSend.AddLast(parameter);
                }
            if (command.getRequestType() == DSHTTPRequestType.GET ||
                    command.getRequestType() == DSHTTPRequestType.DELETE)
            {
                foreach (DSRESTParameter parameter in ParametersToSend)
                    URL += encodeURIComponent(parameter) + '/';
            }
            else // POST or PUT
            {
                bool CanAddParamsToUrl = true;
                _parameters = new TJSONArray();
                foreach (DSRESTParameter parameter in ParametersToSend)
                    if (CanAddParamsToUrl && isURLParameter(parameter))
                        URL += encodeURIComponent(parameter) + '/';
                    else // add the json rapresentation in the body
                    {
                        CanAddParamsToUrl = false;
                        parameter.getValue().appendTo(_parameters);
                    }
            }
            HttpWebRequest Client = (HttpWebRequest)WebRequest.Create(URL + "?" + DateTime.Now.Ticks.ToString());

            HTTPExecutor _executor = null;
            try
            {
                switch (command.getRequestType())
                {
                    case DSHTTPRequestType.GET:
                        {
                            _executor = new HTTPGETExecutor(this, Client, command, Sender, callback, EXCallback);
                            break;
                        }
                    case DSHTTPRequestType.DELETE:
                        {
                            _executor = new HTTPDELETEExecutor(this, Client, command, Sender, callback, EXCallback);
                            break;
                        }
                    case DSHTTPRequestType.POST:
                        {
                            _executor = new HTTPPOSTExecutor(this, Client, command, Sender, callback, EXCallback, _parameters);
                            break;
                        }
                    case DSHTTPRequestType.PUT:
                        {
                            _executor = new HTTPPUTExecutor(this, Client, command, Sender, callback, EXCallback, _parameters);
                            break;
                        }
                    default: { break; }
                }

                if (_executor != null)
                {
                    try
                    {
                        _executor.execute();
                    }
                    catch (Exception ex)
                    {
                        _executor.stop();
                        throw new DBXException(ex.Message);
                    }
                }
            }
            catch (DBXException e)
            {
                throw new DBXException(e.Message);
            }
        }

        private void setSessionIdentifier(HttpWebResponse response)
        {
            string STRDSSESSION = "dssession=";
            string STRDSSESSIONEXPIRES = "dssessionexpires=";
            bool found = false;
            foreach (String h in response.Headers.AllKeys)
            {
                if (h.Equals("Pragma", StringComparison.OrdinalIgnoreCase))
                {
                    string[] parts = response.Headers[h].Split(new char[] { ',' });
                    foreach (string part in parts)
                    {
                        if (part.Contains(STRDSSESSION))
                        {
                            SessionID = part.Substring(part.IndexOf(STRDSSESSION) + STRDSSESSION.Length);
                            found = true;
                        }
                        if (part.Contains(STRDSSESSIONEXPIRES))
                            SessionIDExpires = Convert.ToInt64(part.Substring(part.IndexOf(STRDSSESSIONEXPIRES) + STRDSSESSIONEXPIRES.Length));
                    }
                }
            }
            if (!found)
                CloseSession();
        }

        private bool isURLParameter(DSRESTParameter parameter)
        {
            return ((parameter.getDBXType() != DBXDataTypes.JsonValueType)
                && (parameter.getDBXType() != DBXDataTypes.BinaryBlobType) && (parameter
                .getDBXType() != DBXDataTypes.TableType));
        }

        /**
        * Throw an exception only if the HTTP ERROR CODE is != 200
        * 
        * @param response
        */
        private void throwExceptionIfNeeded(HttpWebResponse response)
        {
            if (response.StatusCode != HttpStatusCode.OK)
            {
                JObject json = null;
                try
                {
                    string resultStr = null;
                    using (StreamReader rder = new StreamReader(response.GetResponseStream()))
                        resultStr = rder.ReadToEnd();
                    json = new JObject(resultStr);
                }
                catch (Exception)
                {
                }
                if (json == null)
                    throw new DBXException(response.StatusDescription);
                else
                    throwExceptionIfNeeded(json);
            }
        }

        /**
         * Throw an Exception inspecting the returned json object
         * 
         * @param json
         */
        private void throwExceptionIfNeeded(JObject json)
        {
            if (json.ToString().Contains("error"))
                throw new DBXException(json.Value<string>("error"));
            if (json.ToString().Contains("SessionExpired"))
            {
                CloseSession();
                throw new DBXException(json.Value<string>("SessionExpired"));
            }
        }

        /**
          * 
          * @return the session id of this connection
          */

        public String getSessionID()
        {
            return SessionID;
        }

        /**
     * 
     * @return the Session ID expires
     */

        public long getSessionExpires()
        {
            return SessionIDExpires;
        }


        /**
      	 * 
      	 * @return the context of this connection
      	 */

        public String getContext()
        {
            return Context;
        }

        /**
    	 * This method returns the protocol used
    	 * 
    	 * @return the protocol of this connection
    	 */

        public String getProtocol()
        {
            return Protocol;
        }

        /**
      	 * This method returns the target host of connection
      	 * 
      	 * @return the target host of connection
      	 */

        public String getHost()
        {
            return Host;
        }

        /**
     * This method returns the target host port of connection
     * 
     * @return the target host port of this connection
     */

        public int getPort()
        {
            return Port;
        }

        /**
     * This method set the target host port of this connection
     * 
     * @param Port
     *            the value of the target host port
     */

        public void setPort(int Port)
        {
            this.Port = Port;
        }

        /**
    	 * This method returns the target url path of this connection
    	 * 
    	 * @return the target url path of this connection
    	 */

        public String getUrlPath()
        {
            return UrlPath;
        }

        /**
    	 * This method set the url Path of the request
    	 * 
    	 * @param urlPath
    	 *            the target url path
    	 */

        public void setUrlPath(String urlPath)
        {
            UrlPath = urlPath;
        }

        /**
    	 * This method set the target host of this connection
    	 * 
    	 * @param host
    	 *            the target host
    	 */

        public void setHost(String host)
        {
            Host = host;
        }


        /**
      	 * This method set the protocol of the requested connection
      	 * 
      	 * @param protocol
      	 *            the protocol of this connection
      	 */

        public void setProtocol(String protocol)
        {
            Protocol = protocol;
        }

        /**
      	 * This method set the context of this connection
      	 * 
      	 * @param context
      	 *            the context of this connection
      	 */

        public void setContext(String context)
        {
            Context = context;
        }

        protected void setSessionID(String sessionID)
        {
            SessionID = sessionID;
        }

        /**
      	 * This method set the Username parameter to use for authentication
      	 * 
      	 * @param userName
      	 *            the Username to use for authentication
      	 */

        public void setUserName(String userName)
        {
            UserName = userName;
        }

        /**
      	 * Returns the username setting that will be used for authentication.
      	 * 
      	 * @return the username used
      	 */

        public String getUserName()
        {
            return UserName;
        }

        /**
      	 * This method set the password parameter to use for authentication
      	 * 
      	 * @param password
      	 *            the password to use for authentication
      	 */

        public void setPassword(String password)
        {
            Password = password;
        }


        /**
      	 * Returns the password setting that will be used for authentication.
      	 * 
      	 * @return the password used
      	 */

        public String getPassword()
        {
            return Password;
        }

        /**
         * Sets the time in milliseconds before a connection's request times out.
         * This property should not be set (should be a number below 1) if this connection will
         * be used for a heavyweight callback (DSCallbackChannelManager).
         * 
         * @param connectionTimeout the communicationTimeout to set
         */
        public void setConnectionTimeout(int connectionTimeout)
        {
            ConnectionTimeout = connectionTimeout;
        }

        /**
         * Returns the time in milliseconds before a connection's request times out.
         * 
         * @return the connectionTimeout
         */
        public int getConnectionTimeout()
        {
            return ConnectionTimeout;
        }

        private abstract class HTTPExecutor
        {
            protected DSRESTConnection connection;
            protected HttpWebRequest Client;
            protected DSRESTCommand command;
            protected DSAdmin Sender;
            protected Delegate callback;
            protected Delegate EXCallback;
            protected DispatcherTimer _Timer;
            protected bool _TimedOut;

            protected HTTPExecutor(DSRESTConnection connection, HttpWebRequest Client, DSRESTCommand command, DSAdmin Sender, Delegate callback, Delegate EXCallback)
            {
                this.connection = connection;
                this.Client = Client;
                this.command = command;
                this.Sender = Sender;
                this.callback = callback;
                this.EXCallback = EXCallback;
                this._TimedOut = false;
                this._Timer = null;

                //don't enable timeout if the request is for a heavyweight callback. Heavyweight callbacks should be timed out
                //with custom code, which uses a call to close the channel with the server when the timeout happens.
                if (connection.getConnectionTimeout() > 0 && !isHeavyweightCallbackRequest(Client))
                {
                    connection.syncContext.Send(new SendOrPostCallback(x => initTimer()), null);
                }
            }

            private Boolean isHeavyweightCallbackRequest(HttpWebRequest Client)
            {
                return Client != null && Client.RequestUri.OriginalString.Contains("/datasnap/rest/DSAdmin/%22ConsumeClientChannel%22/");
            }

            protected void initTimer()
            {
                try
                {
                    _Timer = new DispatcherTimer();
                    _Timer.Interval = new TimeSpan(0, 0, 0, 0, connection.getConnectionTimeout());
                    _TimedOut = false;

                    // Set the delegate to be notified of a timeout
                    _Timer.Tick += delegate
                    {
                        _TimedOut = true;
                        _Timer.Stop();
                        if (EXCallback != null) connection.syncContext.Send(new SendOrPostCallback(x => EXCallback.DynamicInvoke(new DBXException("Request timed out."))), null);
                    };
                }
                catch
                {
                    _Timer = null;
                    _TimedOut = false;
                }
            }

            public bool isTimedOut()
            {
                return _TimedOut;
            }

            public void stop()
            {
                if (_Timer != null) _Timer.Stop();
            }

            protected void syncStop()
            {
                if (_Timer != null) connection.syncContext.Send(new SendOrPostCallback(x => _Timer.Stop()), null);
            }

            protected void start()
            {
                if (connection.getConnectionTimeout() > 0 && _Timer != null) _Timer.Start();
            }

            protected void SetUpHeaders(HttpWebRequest Client)
            {
                if (Client.Method == "POST" || Client.Method == "PUT")
                    Client.ContentType = "text/plain;charset=UTF-8";
                Client.Accept = "application/JSON";
                Client.UserAgent = "Mozilla/3.0 (compatible; Indy Library)";
                if (connection.SessionID == null)
                    SetUpAuthorizationHeader(Client);
                else
                    SetUpSessionHeader(Client);
            }

            protected void SetUpSessionHeader(HttpWebRequest Client)
            {
                if (connection.SessionID != null)
                    Client.Headers["Pragma"] = "dssession=" + connection.SessionID;
            }

            protected void SetUpAuthorizationHeader(HttpWebRequest Client)
            {
                if (connection.UserName == null || connection.UserName.Equals(""))
                    Client.Headers["Authorization"] = "Basic Og=="; // no auth
                else
                {
                    String auth = DBXDefaultFormatter.getInstance().Base64Encode(
                            connection.UserName + ":" + connection.Password);
                    Client.Headers["Authorization"] = "Basic " + auth; // auth
                }
            }

            protected bool isThereOnlyOneStreamInOutput(List<DSRESTParameter> parameters)
            {
                if (isOnlyOneParameterInOutput(parameters))
                {
                    foreach (DSRESTParameter param in parameters)
                    {
                        if (((param.Direction == DSRESTParamDirection.ReturnValue)
                                || (param.Direction == DSRESTParamDirection.InputOutput) || (param.Direction == DSRESTParamDirection.Output))
                                && (param.getDBXType() == DBXDataTypes.BinaryBlobType))
                        {
                            return true;
                        } // if
                    } // for
                }
                return false;
            }

            protected bool isOnlyOneParameterInOutput(List<DSRESTParameter> parameters)
            {
                int Count = 0;
                foreach (DSRESTParameter param in parameters)
                {
                    if (((param.Direction == DSRESTParamDirection.ReturnValue)
                            || (param.Direction == DSRESTParamDirection.InputOutput) || (param.Direction == DSRESTParamDirection.Output)))
                    {
                        Count++;
                    } // if
                } // for
                return Count == 1;
            }

            public abstract void execute();
        }

        private class HTTPGETExecutor : HTTPExecutor
        {
            public HTTPGETExecutor(DSRESTConnection connection, HttpWebRequest Client, DSRESTCommand command, DSAdmin Sender, Delegate callback, Delegate EXCallback)
                : base(connection, Client, command, Sender, callback, EXCallback)
            {
                Client.Method = "GET";
                SetUpHeaders(Client);
            }

            public override void execute()
            {
                Client.BeginGetResponse((IAsyncResult asynchResult) =>
                {
                    HttpWebRequest request = (HttpWebRequest)asynchResult.AsyncState;
                    HttpWebResponse response = null;
                    try
                    {
                        response = (HttpWebResponse)request.EndGetResponse(asynchResult);
                    }
                    catch (WebException e)
                    {
                        try
                        {
                            Stream s = ((HttpWebResponse)e.Response).GetResponseStream();
                            TextReader txt = new StreamReader(s);
                            if (s.Length > 0)
                            {
                                connection.throwExceptionIfNeeded(JObject.Parse(txt.ReadToEnd()));
                            }
                            else
                            {
                                throw new WebException(e.Message);
                            }
                        }
                        catch (Exception ex)
                        {
                            if (EXCallback != null) connection.syncContext.Send(new SendOrPostCallback(x => EXCallback.DynamicInvoke(ex)), null);
                            else connection.syncContext.Send(new SendOrPostCallback(x => Sender.BaseExCal.DynamicInvoke(ex)), null);
                            return;
                        }
                    }
                    finally
                    {
                        syncStop();
                    }
                    connection.throwExceptionIfNeeded(response);
                    connection.setSessionIdentifier(response);
                    if (!isThereOnlyOneStreamInOutput(command.getParameters()))
                    {
                        string resultString = null;
                        using (StreamReader rdr = new StreamReader(response.GetResponseStream()))
                        {
                            resultString = rdr.ReadToEnd();
                            rdr.Close();
                        }
                        response.Close();
                        try
                        {
                            JObject obj = JObject.Parse(resultString);
                            JArray arr = obj.Value<JArray>("result");
                            int returnParIndex = 0;
                            foreach (DSRESTParameter param in command.getParameters())
                            {
                                if ((param.Direction == DSRESTParamDirection.ReturnValue)
                                        || (param.Direction == DSRESTParamDirection.InputOutput)
                                        || (param.Direction == DSRESTParamDirection.Output))
                                {
                                    DBXJSONTools.JSONtoDBX(arr[returnParIndex],
                                                    param.getValue(), param.TypeName);
                                    returnParIndex++;
                                } // if
                            } // for
                        }
                        catch (DBXException e)
                        {
                            throw new DBXException(e.Message);
                        }
                        if (!(_TimedOut)) connection.syncContext.Send(new SendOrPostCallback(x => callback.DynamicInvoke()), null);
                    }
                    else
                    {
                        Stream inputstream = response.GetResponseStream();
                        byte[] b1 = DBXTools.streamToByteArray(inputstream);
                        inputstream.Close();
                        response.Close();
                        TStream ins = new TStream(b1);
                        foreach (DSRESTParameter param in command.getParameters())
                        {
                            if ((param.Direction == DSRESTParamDirection.ReturnValue)
                                    || (param.Direction == DSRESTParamDirection.InputOutput)
                                    || (param.Direction == DSRESTParamDirection.Output))
                            {
                                param.getValue().SetAsStream(ins);
                                if (!(_TimedOut)) connection.syncContext.Send(new SendOrPostCallback(x => callback.DynamicInvoke()), null);
                            }
                        }
                    }
                }, Client);
                start();
            }
        }

        private class HTTPPOSTExecutor : HTTPExecutor
        {
            protected TJSONArray _parameters;
            public HTTPPOSTExecutor(DSRESTConnection connection, HttpWebRequest Client, DSRESTCommand command, DSAdmin Sender, Delegate callback, Delegate EXCallback, TJSONArray parameters)
                : base(connection, Client, command, Sender, callback, EXCallback)
            {
                this._parameters = parameters;

                Client.Method = "POST";
                SetUpHeaders(Client);
            }

            public override void execute()
            {
                if (_parameters == null)
                    throw new DBXException(
                            "Parameters cannot be null in a POST request");
                TJSONObject body = new TJSONObject();
                body.addPairs("_parameters", _parameters);
                Client.BeginGetRequestStream((IAsyncResult asynchronousResult) =>
                {
                    Stream postStream = Client.EndGetRequestStream(asynchronousResult);
                    byte[] postBytes = Encoding.UTF8.GetBytes(body.ToString());
                    postStream.Write(postBytes, 0, postBytes.Length);
                    postStream.Close();
                    Client.BeginGetResponse((IAsyncResult asynchResult) =>
                    {
                        HttpWebRequest request = (HttpWebRequest)asynchResult.AsyncState;
                        HttpWebResponse response = null;
                        try
                        {
                            response = (HttpWebResponse)request.EndGetResponse(asynchResult);
                        }
                        catch (WebException e)
                        {
                            try
                            {
                                Stream s = ((HttpWebResponse)e.Response).GetResponseStream();
                                TextReader txt = new StreamReader(s);
                                if (s.Length > 0)
                                {
                                    connection.throwExceptionIfNeeded(JObject.Parse(txt.ReadToEnd()));
                                }
                                else
                                {
                                    throw new DBXException(e.Message);
                                }
                            }
                            catch (Exception ex)
                            {
                                if (EXCallback != null)
                                    connection.syncContext.Send(new SendOrPostCallback(x => EXCallback.DynamicInvoke(ex)), null);
                                else
                                    if (Sender.BaseExCal != null)
                                        connection.syncContext.Send(new SendOrPostCallback(x => Sender.BaseExCal.DynamicInvoke(ex)), null);
                                    else
                                        throw ex;
                                return;
                            }
                        }
                        finally
                        {
                            syncStop();
                        }
                        connection.setSessionIdentifier(response);
                        connection.throwExceptionIfNeeded(response);
                        if (!isThereOnlyOneStreamInOutput(command.getParameters()))
                        {
                            string resultString = null;
                            using (StreamReader rdr = new StreamReader(response.GetResponseStream()))
                            {
                                resultString = rdr.ReadToEnd();
                                rdr.Close();
                            }
                            response.Close();
                            try
                            {
                                JObject obj = JObject.Parse(resultString);
                                JArray arr = obj.Value<JArray>("result");
                                int returnParIndex = 0;
                                foreach (DSRESTParameter param in command.getParameters())
                                {
                                    if ((param.Direction == DSRESTParamDirection.ReturnValue)
                                            || (param.Direction == DSRESTParamDirection.InputOutput)
                                            || (param.Direction == DSRESTParamDirection.Output))
                                    {
                                        DBXJSONTools.JSONtoDBX(arr[returnParIndex],
                                            param.getValue(), param.TypeName);
                                        returnParIndex++;
                                    } // if
                                } // for
                            }
                            catch (DBXException e)
                            {
                                throw new DBXException(e.Message);
                            }
                            if (!(_TimedOut)) connection.syncContext.Send(new SendOrPostCallback(x => callback.DynamicInvoke()), null);
                        }
                        else
                        {
                            Stream inputstream = response.GetResponseStream();
                            byte[] b1 = DBXTools.streamToByteArray(inputstream);
                            inputstream.Close();
                            response.Close();
                            TStream ins = new TStream(b1);
                            foreach (DSRESTParameter param in command.getParameters())
                            {
                                if ((param.Direction == DSRESTParamDirection.ReturnValue)
                                        || (param.Direction == DSRESTParamDirection.InputOutput)
                                        || (param.Direction == DSRESTParamDirection.Output))
                                {
                                    if (param.TypeName.StartsWith("TDBX") && param.TypeName.EndsWith("Value"))
                                    {
                                        param.getValue().GetAsDBXValue().SetAsStream(ins);
                                    }
                                    else
                                    {
                                        param.getValue().SetAsStream(ins);
                                    }
                                    if (!(_TimedOut)) connection.syncContext.Send(new SendOrPostCallback(x => callback.DynamicInvoke()), null);
                                }
                            }
                        };
                    }, Client);
                }, Client);
                start();
            }
        }

        private class HTTPPUTExecutor : HTTPExecutor
        {
            protected TJSONArray _parameters;

            public HTTPPUTExecutor(DSRESTConnection connection, HttpWebRequest Client, DSRESTCommand command, DSAdmin Sender, Delegate callback, Delegate EXCallback, TJSONArray parameters)
                : base(connection, Client, command, Sender, callback, EXCallback)
            {
                this._parameters = parameters;

                Client.Method = "PUT";
                SetUpHeaders(Client);
            }

            public override void execute()
            {
                if (_parameters == null)
                    throw new DBXException(
                            "Parameters cannot be null in a PUT request");
                TJSONObject body = new TJSONObject();
                body.addPairs("_parameters", _parameters);
                Client.BeginGetRequestStream((IAsyncResult asynchronousResult) =>
                {
                    Stream postStream = Client.EndGetRequestStream(asynchronousResult);
                    byte[] postBytes = Encoding.UTF8.GetBytes(body.ToString());
                    postStream.Write(postBytes, 0, postBytes.Length);
                    postStream.Close();
                    Client.BeginGetResponse((IAsyncResult asynchResult) =>
                    {
                        HttpWebRequest request = (HttpWebRequest)asynchResult.AsyncState;
                        HttpWebResponse response = null;
                        try
                        {
                            response = (HttpWebResponse)request.EndGetResponse(asynchResult);
                        }
                        catch (WebException e)
                        {
                            try
                            {
                                Stream s = ((HttpWebResponse)e.Response).GetResponseStream();
                                TextReader txt = new StreamReader(s);
                                if (s.Length > 0)
                                {
                                    connection.throwExceptionIfNeeded(JObject.Parse(txt.ReadToEnd()));
                                }
                                else
                                {
                                    throw new WebException(e.Message);
                                }
                            }
                            catch (Exception ex)
                            {
                                if (EXCallback != null) connection.syncContext.Send(new SendOrPostCallback(x => EXCallback.DynamicInvoke(ex)), null);
                                else connection.syncContext.Send(new SendOrPostCallback(x => Sender.BaseExCal.DynamicInvoke(ex)), null);
                                return;
                            }
                        }
                        finally
                        {
                            syncStop();
                        }
                        connection.setSessionIdentifier(response);
                        connection.throwExceptionIfNeeded(response);
                        if (!isThereOnlyOneStreamInOutput(command.getParameters()))
                        {
                            string resultString = null;
                            using (StreamReader rdr = new StreamReader(response.GetResponseStream()))
                                resultString = rdr.ReadToEnd();
                            try
                            {
                                JObject obj = JObject.Parse(resultString);
                                JArray arr = obj.Value<JArray>("result");
                                int returnParIndex = 0;
                                foreach (DSRESTParameter param in command.getParameters())
                                {
                                    if ((param.Direction == DSRESTParamDirection.ReturnValue)
                                            || (param.Direction == DSRESTParamDirection.InputOutput)
                                            || (param.Direction == DSRESTParamDirection.Output))
                                    {
                                        DBXJSONTools.JSONtoDBX(arr[returnParIndex],
                                            param.getValue(), param.TypeName);
                                        returnParIndex++;
                                    } // if
                                } // for
                            }
                            catch (DBXException e)
                            {
                                throw new DBXException(e.Message);
                            }
                            if (!(_TimedOut)) connection.syncContext.Send(new SendOrPostCallback(x => callback.DynamicInvoke()), null);
                        }
                        else
                        {

                            Stream inputstream = response.GetResponseStream();
                            byte[] b1 = DBXTools.streamToByteArray(inputstream);
                            TStream ins = new TStream(b1);
                            foreach (DSRESTParameter param in command.getParameters())
                            {
                                if ((param.Direction == DSRESTParamDirection.ReturnValue)
                                        || (param.Direction == DSRESTParamDirection.InputOutput)
                                        || (param.Direction == DSRESTParamDirection.Output))
                                {
                                    if (param.TypeName.StartsWith("TDBX") && param.TypeName.EndsWith("Value"))
                                    {
                                        param.getValue().GetAsDBXValue().SetAsStream(ins);
                                    }
                                    else
                                    {
                                        param.getValue().SetAsStream(ins);
                                    }
                                    if (!(_TimedOut)) connection.syncContext.Send(new SendOrPostCallback(x => callback.DynamicInvoke()), null);
                                }
                            }
                        };
                    }, Client);
                }, Client);
                start();
            }
        }

        private class HTTPDELETEExecutor : HTTPExecutor
        {
            public HTTPDELETEExecutor(DSRESTConnection connection, HttpWebRequest Client, DSRESTCommand command, DSAdmin Sender, Delegate callback, Delegate EXCallback)
                : base(connection, Client, command, Sender, callback, EXCallback)
            {
                Client.Method = "DELETE";
                SetUpHeaders(Client);
            }

            public override void execute()
            {
                Client.BeginGetResponse((IAsyncResult asynchResult) =>
                {
                    HttpWebRequest request = (HttpWebRequest)asynchResult.AsyncState;
                    HttpWebResponse response = null;
                    try
                    {
                        response = (HttpWebResponse)request.EndGetResponse(asynchResult);
                    }
                    catch (WebException e)
                    {
                        try
                        {
                            Stream s = ((HttpWebResponse)e.Response).GetResponseStream();
                            TextReader txt = new StreamReader(s);
                            if (s.Length > 0)
                            {
                                connection.throwExceptionIfNeeded(JObject.Parse(txt.ReadToEnd()));
                            }
                            else
                            {
                                throw new WebException(e.Message);
                            }
                        }
                        catch (Exception ex)
                        {
                            if (EXCallback != null) connection.syncContext.Send(new SendOrPostCallback(x => EXCallback.DynamicInvoke(ex)), null);
                            else connection.syncContext.Send(new SendOrPostCallback(x => Sender.BaseExCal.DynamicInvoke(ex)), null);
                            return;
                        }
                    }
                    finally
                    {
                        syncStop();
                    }
                    connection.throwExceptionIfNeeded(response);
                    connection.setSessionIdentifier(response);
                    if (!isThereOnlyOneStreamInOutput(command.getParameters()))
                    {
                        string resultString = null;
                        using (StreamReader rdr = new StreamReader(response.GetResponseStream()))
                            resultString = rdr.ReadToEnd();
                        try
                        {
                            JObject obj = JObject.Parse(resultString);
                            JArray arr = obj.Value<JArray>("result");
                            int returnParIndex = 0;
                            foreach (DSRESTParameter param in command.getParameters())
                            {
                                if ((param.Direction == DSRESTParamDirection.ReturnValue)
                                        || (param.Direction == DSRESTParamDirection.InputOutput)
                                        || (param.Direction == DSRESTParamDirection.Output))
                                {
                                    DBXJSONTools.JSONtoDBX(arr[returnParIndex],
                                                    param.getValue(), param.TypeName);
                                    returnParIndex++;
                                } // if
                            } // for
                        }
                        catch (DBXException e)
                        {
                            throw new DBXException(e.Message);
                        }
                        if (!(_TimedOut)) connection.syncContext.Send(new SendOrPostCallback(x => callback.DynamicInvoke()), null);
                    }
                    else
                    {
                        Stream inputstream = response.GetResponseStream();
                        byte[] b1 = DBXTools.streamToByteArray(inputstream);
                        TStream ins = new TStream(b1);
                        foreach (DSRESTParameter param in command.getParameters())
                        {
                            if ((param.Direction == DSRESTParamDirection.ReturnValue)
                                    || (param.Direction == DSRESTParamDirection.InputOutput)
                                    || (param.Direction == DSRESTParamDirection.Output))
                            {
                                param.getValue().SetAsStream(ins);
                                if (!(_TimedOut)) connection.syncContext.Send(new SendOrPostCallback(x => callback.DynamicInvoke()), null);
                            }
                        }
                    }
                }, Client);
                start();
            }
        }
    }
}