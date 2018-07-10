//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

package com.embarcadero.javablackberry;

import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Vector;

import javax.microedition.io.Connection;
import javax.microedition.io.HttpConnection;

import net.rim.device.api.io.IOUtilities;
import net.rim.device.api.io.transport.ConnectionDescriptor;
import net.rim.device.api.io.transport.ConnectionFactory;

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

public class DSRESTConnection {

	/**
	 * This method returns the URL encoded string starting from the input string
	 * URL. The characters encoding is UTF-8.
	 * 
	 * @param value
	 *            the URL string to encode
	 * @return the URL string encoded
	 */

	private String encodeURIComponent(String value) {
		try {
			return replaceAllOccurenceOf(URLUTF8Encoder.encode(value), "\\+",
					"%20");
		} catch (Exception e) {
			return "";
		}
	}

	/**
	 * This method returns the URL encoded string starting from the input
	 * {@link DSRESTParameter}
	 * 
	 * @param parameter
	 *            the DSRESTParameter to encode
	 * @return the DSRESTParameter encoded
	 */
	private String encodeURIComponent(DSRESTParameter parameter) {
		return encodeURIComponent(parameter.getValue().toString());
	}

	private int Port = 0;
	private String UrlPath = "";
	private String Host = "";
	private String Protocol = "";
	private String Context = "";
	private String SessionID;
	private long SessionIDExpires;
	private String UserName = "";
	private String Password = "";

	private int connectionTimeout = 0;
	private boolean deviceSide = true;
	private boolean wifiInterface = false;
	private String connectionType = null;
	private String connectionStringSuffix = null;

	/**
	 * Class constructor
	 */
	public DSRESTConnection() {
		super();
		CloseSession();
	}

	/**
	 * Clone the current connection. The session is not cloned, so the cloned
	 * connection will not have the same session as its parent.
	 * 
	 * @return the new DSRESTConnection
	 */

	public DSRESTConnection Clone() {
		return Clone(false);
	}

	/**
	 * Clone the current connection. The session is optionally included in the
	 * clone.
	 * 
	 * @param includeSession
	 *            true to include session information in the new connection,
	 *            false to exclude it.
	 * 
	 * @return the new DSRESTConnection
	 */

	public DSRESTConnection Clone(boolean includeSession) {
		DSRESTConnection connection = new DSRESTConnection();
		connection.setHost(this.getHost());
		connection.setPort(this.getPort());
		connection.setProtocol(this.getProtocol());
		connection.setUserName(this.getUserName());
		connection.setPassword(this.getPassword());
		connection.setUrlPath(this.getUrlPath());
		connection.setConnectionTimeout(this.getConnectionTimeout());
		connection.setDeviceSide(this.isDeviceSide());
		connection.setWifiInterface(this.isWifiInterface());
		connection.setConnectionType(this.getConnectionType());
		connection.setConnectionStringSuffix(this.getConnectionStringSuffix());
		if (includeSession) {
			connection.setSessionID(this.getSessionID());
			connection.SessionIDExpires = this.SessionIDExpires;
		}
		return connection;
	}

	/**
	 * Factory method to quick create a command.
	 * 
	 * @return a new DSRESTCommand
	 */
	public DSRESTCommand CreateCommand() {
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

	private String BuildRequestURL(DSRESTCommand command) {
		String LPathPrefix = getUrlPath();
		int LPort = getPort();
		String LHost = getHost();
		String LMethodName = command.getText();
		String LProtocol = getProtocol();
		if (LProtocol.equals(""))
			LProtocol = "http";
		if (LHost.equals(""))
			LHost = "localhost";
		if (!LPathPrefix.equals(""))
			LPathPrefix = "/" + LPathPrefix;
		String LPortString = "";
		if (LPort > 0)
			LPortString = ":" + String.valueOf(LPort);
		if (command.getRequestType() == DSHTTPRequestType.GET
				|| command.getRequestType() == DSHTTPRequestType.DELETE) {
			LMethodName = replaceFirstOccurenceOf(LMethodName, '.', "/");
			LMethodName = replaceAllOccurenceOf(LMethodName, "\"", "");
		} else {
			// POST
			LMethodName = replaceFirstOccurenceOf(LMethodName, '.', "/%22");
			LMethodName = replaceAllOccurenceOf(LMethodName, "\"", "%22");
			LMethodName = LMethodName + "%22";
		}
		String LContext = getContext();
		if (LContext.equals(""))
			LContext = "datasnap/";
		String LUrl = LProtocol + "://" + encodeURIComponent(LHost)
				+ LPortString + LPathPrefix + "/" + LContext + "rest/"
				+ LMethodName + "/";
		SessionID = getSessionID();
		return LUrl;
	}

	private String replaceFirstOccurenceOf(String s, char old_char,
			String new_char) {

		int index = s.indexOf(old_char);
		String app = s.substring(0, index);
		return app.concat(String.valueOf(new_char)).concat(
				s.substring(index + 1));
	}

	private String replaceAllOccurenceOf(String s, String expr, String sub) {

		if (s.indexOf(expr) == -1)
			return s;
		else {
			String app = "";
			int i = 0;
			while (s.indexOf(expr, i) != -1) {
				app = app.concat(s.substring(i, s.indexOf(expr, i)))
						.concat(sub);
				i = s.indexOf(expr, i) + expr.length();
			}
			return app;
		}

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
	 * @param command
	 *            the specific {@link DSRESTCommand}
	 * @throws DBXException
	 *             where the parameter type is not operated or in case that the
	 *             json response is an error message.
	 */

	public void execute(DSRESTCommand command) throws DBXException {
		HttpConnection response = null;
		InputStream inputstream = null;
		try {
			response = CreateRequest(command);
			setSessionIdentifier(response);
			throwExceptionIfNeeded(response);
			if (isThereOnlyOneStreamInOutput(command.getParameters())) {
				inputstream = response.openInputStream();
				byte[] b1 = DBXTools.streamToByteArray(inputstream);
				TStream is = new TStream(b1);
				for (int i = 0; i < command.getParameters().size(); i++) {
					DSRESTParameter param = command.getParameter(i);
					if ((param.Direction == DSRESTParamDirection.ReturnValue)
							|| (param.Direction == DSRESTParamDirection.InputOutput)
							|| (param.Direction == DSRESTParamDirection.Output)) {
						if (param.TypeName.startsWith("TDBX")
								&& param.TypeName.endsWith("Value")) {
							param.getValue().GetAsDBXValue().SetAsStream(is);
						} else {
							param.getValue().SetAsStream(is);
						}
						break;
					} // if
				} // for
			} else {
				try {
					inputstream = response.openInputStream();
					String s = new String(
							IOUtilities.streamToBytes(inputstream));
					TJSONObject json = TJSONObject.Parse(s);
					throwExceptionIfNeeded(json);
					TJSONArray results = json.getJSONArray("result");
					int returnParIndex = 0;
					for (int i = 0; i < command.getParameters().size(); i++) {
						DSRESTParameter param = command.getParameter(i);
						if ((param.Direction == DSRESTParamDirection.ReturnValue)
								|| (param.Direction == DSRESTParamDirection.InputOutput)
								|| (param.Direction == DSRESTParamDirection.Output)) {
							DBXJSONTools.JSONtoDBX(results.get(returnParIndex),
									param.getValue(), param.TypeName);
							returnParIndex++;
						} // if
					} // for
				} catch (Exception e) {
					throw new DBXException(e);
				}
			}

		} catch (Exception e) {
			throw new DBXException(e);
		} finally {
			try {

				if (response != null)
					response.close();

				if (inputstream != null)
					inputstream.close();

			} catch (IOException e) {
				throw new DBXException(e.getMessage());
			}
		}
	}

	private void setSessionIdentifier(HttpConnection response)
			throws IOException {
		boolean found = false;
		String e = response.getHeaderField("Pragma"); //HttpProtocolConstants.HEADER_PRAGMA
		if (!(e == null)) {
			Vector header = split(e, ',');
			if (e.indexOf("dssession=") >= 0) {
				SessionID = (String) split((String) header.elementAt(0), '=')
						.elementAt(1);
				found = true;
			}
			if (e.indexOf("dssessionexpires=") >= 0)
				SessionIDExpires = Long.parseLong((String) split(
						(String) header.elementAt(1), '=').elementAt(1));
		}
		if (!found)
			CloseSession();
	}

	private boolean isThereOnlyOneStreamInOutput(Vector parameters) {
		if (isOnlyOneParameterInOutput(parameters)) {
			for (int i = 0; i < parameters.size(); i++) {
				DSRESTParameter param = (DSRESTParameter) parameters
						.elementAt(i);
				if (((param.Direction == DSRESTParamDirection.ReturnValue)
						|| (param.Direction == DSRESTParamDirection.InputOutput) || (param.Direction == DSRESTParamDirection.Output))
						&& (param.getDBXType() == DBXDataTypes.BinaryBlobType)) {
					return true;
				} // if
			} // for
		}
		return false;
	}

	private boolean isOnlyOneParameterInOutput(Vector parameters) {
		int Count = 0;
		for (int i = 0; i < parameters.size(); i++) {
			DSRESTParameter param = (DSRESTParameter) parameters.elementAt(i);
			if (((param.Direction == DSRESTParamDirection.ReturnValue)
					|| (param.Direction == DSRESTParamDirection.InputOutput) || (param.Direction == DSRESTParamDirection.Output))) {
				Count++;
			} // if
		} // for
		return Count == 1;
	}

	/**
	 * Throw an exception if the HTTP ERROR CODE is != 200 or the json response
	 * is an error message.
	 * 
	 * @param response
	 *            the response received after the request
	 * @throws DBXException
	 */
	private void throwExceptionIfNeeded(HttpConnection response)
			throws DBXException, IOException {
		if (response.getResponseCode() != 200) {
			TJSONObject json = null;
			try {
				json = TJSONObject.Parse(new String(IOUtilities
						.streamToBytes(response.openInputStream())));
			} catch (Exception ex) {
			}
			if (json == null)
				throw new DBXException(response.getResponseMessage());
			else
				throwExceptionIfNeeded(json);
		}
	}

	/**
	 * Throw an Exception inspecting the returned json object
	 * 
	 * @param json
	 *            the json response
	 * @throws DBXException
	 */
	private void throwExceptionIfNeeded(TJSONObject json) throws DBXException {
		if (json.has("error"))
			throw new DBXException(json.getString("error"));
		if (json.has("SessionExpired")) {
			CloseSession();
			throw new DBXException(json.getString("SessionExpired"));
		}
	}

	/**
	 * This method close the active session
	 */

	public void CloseSession() {
		SessionID = null;
		SessionIDExpires = -1;
	}

	private HttpConnection CreateRequest(DSRESTCommand command)
			throws IOException, DBXException, Exception {
		String URL = BuildRequestURL(command);
		TJSONArray _parameters = null;
		Vector ParametersToSend = new Vector();
		if (command.getParameters().size() > 0)
			for (int i = 0; i < command.getParameters().size(); i++) {
				DSRESTParameter parameter = command.getParameter(i);
				if (parameter.Direction == DSRESTParamDirection.Input
						|| parameter.Direction == DSRESTParamDirection.InputOutput)
					ParametersToSend.addElement(parameter);
			}
		if (command.getRequestType() == DSHTTPRequestType.GET
				|| command.getRequestType() == DSHTTPRequestType.DELETE) {
			for (int i = 0; i < ParametersToSend.size(); i++) {
				DSRESTParameter parameter = (DSRESTParameter) ParametersToSend
						.elementAt(i);
				URL += encodeURIComponent(parameter) + '/';
			}
		} else { // POST or PUT
			boolean CanAddParamsToUrl = true;
			_parameters = new TJSONArray();
			for (int i = 0; i < ParametersToSend.size(); i++) {
				DSRESTParameter parameter = (DSRESTParameter) ParametersToSend
						.elementAt(i);
				if (CanAddParamsToUrl && isURLParameter(parameter))
					URL += encodeURIComponent(parameter) + '/';
				else // add the json rapresentation in the body
				{
					CanAddParamsToUrl = false;
					parameter.getValue().appendTo(_parameters);
				}
			}
		}
		HttpConnection req;
		if (_parameters == null)
			req = BuildRequest(command.getRequestType(), URL, null);
		else
			req = BuildRequest(command.getRequestType(), URL, _parameters);
		SetUpHeaders(req);
		return req;
	}

	private HttpConnection BuildRequest(DSHTTPRequestType requestType,
			String URL, TJSONArray _parameters) throws DBXException,
			IOException {
			
		final String peerRefusedConnection = "Peer refused the connection";

		// optionally provide the connection timeout property
		ConnectionFactory connFact = new ConnectionFactory();
		if (connectionTimeout > 0) {
			connFact.setTimeoutSupported(true);
			connFact.setTimeLimit(connectionTimeout);
			connFact.setConnectionTimeout(connectionTimeout);
			URL += ";ConnectionTimeout=" + Integer.toString(connectionTimeout);
		} else {
			connFact.setTimeoutSupported(false);
		}

		URL += ";deviceside=" + String.valueOf(deviceSide);

		if (wifiInterface) {
			URL += ";interface=wifi";
		}

		if (connectionType != null) {
			URL += ";ConnectionType=" + connectionType;
		}

		if (connectionStringSuffix != null
				&& connectionStringSuffix.length() > 0) {

			if (!connectionStringSuffix.startsWith(";")) {
				URL += ";" + connectionStringSuffix;
			} else {
				URL += connectionStringSuffix;
			}
		}

		if (requestType.toString().equals("GET")) {
			HttpConnection HttpGet = null;
			ConnectionDescriptor connDesc;
			connDesc = connFact.getConnection(URL);
			if (connDesc == null)
				throw new DBXException(peerRefusedConnection);
			Connection con = connDesc.getConnection();
			HttpGet = (HttpConnection) con;
			HttpGet.setRequestMethod(HttpConnection.GET);
			return HttpGet;
		}

		if (requestType.toString().equals("DELETE")) {
			HttpConnection HttpDelete = null;
			ConnectionDescriptor connDesc;
			connDesc = connFact.getConnection(URL);
			if (connDesc == null)
				throw new DBXException(peerRefusedConnection);
			HttpDelete = (HttpConnection) connDesc.getConnection();
			HttpDelete.setRequestMethod("DELETE");
			return HttpDelete;
		}

		if (requestType.toString().equals("POST")) {
			DataOutputStream os = null;
			HttpConnection p = null;
			try {
				ConnectionDescriptor connDesc;
				connDesc = connFact.getConnection(URL);
				if (connDesc == null)
					throw new DBXException(peerRefusedConnection);
				p = (HttpConnection) connDesc.getConnection();
				p.setRequestMethod(HttpConnection.POST);
				SetUpHeaders(p);
				if (_parameters == null)
					throw new DBXException(
							"Parameters cannot be null in a POST request");
				if (_parameters.size() > 1) {
					TJSONObject body = new TJSONObject();
					body.addPairs("_parameters", _parameters);
					os = p.openDataOutputStream();
					os.write(body.toString().getBytes());
					return p;
				} else {
					os = p.openDataOutputStream();
					os.write(_parameters.get(0).toString().getBytes());
					return p;
				}
			} catch (Exception ex) {
				throw new DBXException(ex.getMessage());
			} finally {
				if (os != null)
					os.close();
			}
		}

		if (requestType.toString().equals("PUT")) {
			DataOutputStream os = null;
			HttpConnection p = null;
			try {
				ConnectionDescriptor connDesc;
				connDesc = connFact.getConnection(URL);
				if (connDesc == null)
					throw new DBXException(peerRefusedConnection);
				p = (HttpConnection) connDesc.getConnection();
				p.setRequestMethod("PUT");
				SetUpHeaders(p);
				if (_parameters == null)
					throw new DBXException(
							"Parameters cannot be null in a PUT request");
				if (_parameters.size() > 1) {
					TJSONObject body = new TJSONObject();
					body.addPairs("_parameters", _parameters);
					os = p.openDataOutputStream();
					os.write(body.toString().getBytes());
					return p;
				} else {
					os = p.openDataOutputStream();
					os.write(_parameters.get(0).toString().getBytes());
					return p;
				}
			} catch (Exception ex) {
				throw new DBXException(ex.getMessage());
			} finally {
				if (os != null)
					os.close();
			}
		}
		return null;
	}

	private void SetUpSessionHeader(HttpConnection request) throws IOException {
		if (SessionID != null)
			request.setRequestProperty("Pragma", "dssession=" + SessionID);
	}

	private void SetUpAuthorizationHeader(HttpConnection request)
			throws IOException {
		if (UserName == null || UserName.equals(""))
			request.setRequestProperty("Authorization", "Basic Og=="); // no
																		// auth
		else {
			String auth = DBXDefaultFormatter.getInstance().Base64Encode(
					this.UserName + ":" + this.Password);
			request.setRequestProperty("Authorization", "Basic " + auth); // auth
		}
	}

	private void SetUpHeaders(HttpConnection request) throws IOException {
		request.setRequestProperty("If-Modified-Since",
				"Mon, 1 Oct 1990 05:00:00 GMT");
		request.setRequestProperty("Connection", "Keep-Alive");
		request.setRequestProperty("Content-Type", "text/plain;charset=UTF-8");
		request.setRequestProperty("Accept", "application/JSON");
		request.setRequestProperty("Accept-Encoding", "identity");
		request.setRequestProperty("User-Agent",
				"Mozilla/3.0 (compatible; Indy Library)");

		if (SessionID == null)
			SetUpAuthorizationHeader(request);
		else
			SetUpSessionHeader(request);
	}

	private boolean isURLParameter(DSRESTParameter parameter) {
		return ((parameter.getDBXType() != DBXDataTypes.JsonValueType)
				&& (parameter.getDBXType() != DBXDataTypes.BinaryBlobType) && (parameter
				.getDBXType() != DBXDataTypes.TableType));
	}

	/**
	 * 
	 * @return the session id of this connection
	 */

	public String getSessionID() {
		return SessionID;
	}

	/**
	 * 
	 * @return the Session ID expires
	 */

	public long getSessionExpires() {
		return SessionIDExpires;
	}

	/**
	 * 
	 * @return the context of this connection
	 */

	public String getContext() {
		return Context;
	}

	/**
	 * This method returns the protocol used
	 * 
	 * @return the protocol of this connection
	 */

	public String getProtocol() {
		return Protocol;
	}

	/**
	 * This method returns the target host of connection
	 * 
	 * @return the target host of connection
	 */

	public String getHost() {
		return Host;
	}

	/**
	 * This method returns the target host port of connection
	 * 
	 * @return the target host port of this connection
	 */

	public int getPort() {
		return Port;
	}

	/**
	 * This method set the target host port of this connection
	 * 
	 * @param Port
	 *            the value of the target host port
	 */

	public void setPort(int Port) {
		this.Port = Port;
	}

	/**
	 * This method returns the target url path of this connection
	 * 
	 * @return the target url path of this connection
	 */

	public String getUrlPath() {
		return UrlPath;
	}

	/**
	 * This method set the url Path of the request
	 * 
	 * @param urlPath
	 *            the target url path
	 */

	public void setUrlPath(String urlPath) {
		UrlPath = urlPath;
	}

	/**
	 * This method set the target host of this connection
	 * 
	 * @param host
	 *            the target host
	 */

	public void setHost(String host) {
		Host = host;
	}

	/**
	 * This method set the protocol of the requested connection
	 * 
	 * @param protocol
	 *            the protocol of this connection
	 */

	public void setProtocol(String protocol) {
		Protocol = protocol;
	}

	/**
	 * This method set the context of this connection
	 * 
	 * @param context
	 *            the context of this connection
	 */

	public void setContext(String context) {
		Context = context;
	}

	protected void setSessionID(String sessionID) {
		SessionID = sessionID;
	}

	/**
	 * This method set the Username parameter to use for authentication
	 * 
	 * @param userName
	 *            the Username to use for authentication
	 */

	public void setUserName(String userName) {
		UserName = userName;
	}

	/**
	 * Returns the username setting that will be used for authentication.
	 * 
	 * @return the username used
	 */

	public String getUserName() {
		return UserName;
	}

	/**
	 * This method set the password parameter to use for authentication
	 * 
	 * @param password
	 *            the password to use for authentication
	 */

	public void setPassword(String password) {
		Password = password;
	}

	/**
	 * Returns the password setting that will be used for authentication.
	 * 
	 * @return the password used
	 */

	public String getPassword() {
		return Password;
	}

	private Vector split(String data, char splitChar) {
		Vector v = new Vector();

		String working = data;
		int index = working.indexOf(splitChar);

		while (index != -1) {
			String tmp = "";
			if (index > 0)
				tmp = working.substring(0, index);
			v.addElement(tmp);

			working = working.substring(index + 1);

			// Find the next index
			index = working.indexOf(splitChar);
		}

		// Add the rest of the working string
		v.addElement(working);

		return v;
	}

	/**
	 * Returns the time in milliseconds before a connection's request times out.
	 * This flag is only a hint to the protocol handler, and it does not
	 * guarantee that an exceptions will actually be thrown.
	 * 
	 * @return the connectionTimeout
	 */
	public int getConnectionTimeout() {
		return connectionTimeout;
	}

	/**
	 * Sets the time in milliseconds before a connection's request times out.
	 * This flag is only a hint to the protocol handler, and it does not
	 * guarantee that an exceptions will actually be thrown.
	 * 
	 * @param connectionTimeout
	 *            the connectionTimeout to set
	 */
	public void setConnectionTimeout(int connectionTimeout) {
		this.connectionTimeout = connectionTimeout;
	}

	/**
	 * Returns the value for the deviceside URL property
	 * 
	 * @return the deviceSide
	 */
	public boolean isDeviceSide() {
		return deviceSide;
	}

	/**
	 * Sets the value for the deviceside URL property
	 * 
	 * @param deviceSide
	 *            the deviceSide to set
	 */
	public void setDeviceSide(boolean deviceSide) {
		this.deviceSide = deviceSide;
	}

	/**
	 * Returns the value for the interface=wifi URL property. If true, then
	 * interface=wifi will be included in the URL. Otherwise, it will not.
	 * 
	 * @return the wifiInterface
	 */
	public boolean isWifiInterface() {
		return wifiInterface;
	}

	/**
	 * Sets if interface-wifi should be used in the connection URLs or not.
	 * 
	 * @param wifiInterface
	 *            the wifiInterface to set
	 */
	public void setWifiInterface(boolean wifiInterface) {
		this.wifiInterface = wifiInterface;
	}

	/**
	 * Returns the connection type for this connection, or null.
	 * 
	 * @return the connectionType
	 */
	public String getConnectionType() {
		return connectionType;
	}

	/**
	 * Sets the connection type for this connection, or null to not specify any
	 * type. When set, the URL will have a ";ConnectionType=[type]" property
	 * added to the URL.
	 * 
	 * @param connectionType
	 *            the connectionType to set
	 */
	public void setConnectionType(String connectionType) {
		this.connectionType = connectionType;
	}

	/**
	 * Returns the currently set connection string suffix, or null
	 * 
	 * @return the connectionStringSuffix
	 */
	public String getConnectionStringSuffix() {
		return connectionStringSuffix;
	}

	/**
	 * Sets the connection string suffix. If it is not null, this value will be
	 * added to the end of all URLs created for this connection. You can use
	 * this to make a semicolon separated list of connection parameters not
	 * directly supported by the properties of this connection class.
	 * 
	 * @param connectionStringSuffix
	 *            the connectionStringSuffix to set
	 */
	public void setConnectionStringSuffix(String connectionStringSuffix) {
		if (connectionStringSuffix != null) {
			connectionStringSuffix = connectionStringSuffix.trim();
		}
		this.connectionStringSuffix = connectionStringSuffix;
	}
}
