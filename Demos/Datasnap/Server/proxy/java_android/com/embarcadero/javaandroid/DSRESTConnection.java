//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

package com.embarcadero.javaandroid;

import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.security.KeyManagementException;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.UnrecoverableKeyException;
import java.util.LinkedList;
import java.util.List;

import org.apache.http.Header;
import org.apache.http.HeaderElement;
import org.apache.http.HttpResponse;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpDelete;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.methods.HttpPut;
import org.apache.http.client.methods.HttpUriRequest;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.DefaultHttpClient;
import org.apache.http.params.BasicHttpParams;
import org.apache.http.params.HttpConnectionParams;
import org.apache.http.params.HttpParams;
import org.apache.http.util.EntityUtils;
import org.json.JSONException;

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
	public final static String TAG = "DataSnap";

	/**
	 * This method returns the URL encoded string starting from the input string
	 * URL. The characters encoding is UTF-8.
	 * 
	 * 
	 * @param value
	 *            the URL string to encode
	 * @return the URL string encoded
	 */

	private String encodeURIComponent(String value) {
		try {
			String encodedURI = URLEncoder.encode(value, "UTF-8");
			encodedURI = encodedURI.replaceAll("\\+", "%20");
			encodedURI = encodedURI.replaceAll("\\%21", "!");
			encodedURI = encodedURI.replaceAll("\\%27", "'");
			encodedURI = encodedURI.replaceAll("\\%28", "(");
			encodedURI = encodedURI.replaceAll("\\%29", ")");
			encodedURI = encodedURI.replaceAll("\\%7E", "~");
			return encodedURI;
		} catch (UnsupportedEncodingException e) {
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
	private String UserName = "";
	private String Password = "";

	private int connectionTimeout = 5000;
	private int communicationTimeout = 0;

	private String SessionID;
	private long SessionIDExpires;
	private DSRESTSSLFactory SSLFactory;
	private boolean isHttps;

	/**
	 * Class constructor
	 */

	public DSRESTConnection() {
		super();
		InitSSLFactory();
		CloseSession();
	}

	/**
	 * Initialize the SSL factory that will manage HTTP and HTTPS connections
	 * 
	 * @throws UnrecoverableKeyException
	 * @throws KeyStoreException
	 * @throws NoSuchAlgorithmException
	 * @throws KeyManagementException
	 */
	private void InitSSLFactory() {
		if (SSLFactory == null)
			try {
				SSLFactory = new DSRESTSSLFactory(null);
			} catch (Exception ex) {
				// throw new DBXException(ex.getMessage());
				// Later will use the http interface silently
			}
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
		connection.setCommunicationTimeout(this.getCommunicationTimeout());
		connection.setConnectionTimeout(this.getConnectionTimeout());
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

		if (!LProtocol.equals("https")) // empty string, invalid protocol and
										// http, become "http". Otherwise
										// "https".
			LProtocol = "http";
		isHttps = LProtocol.equals("https");

		if (LHost.equals(""))
			LHost = "localhost";
		if (!LPathPrefix.equals(""))
			LPathPrefix = "/" + LPathPrefix;
		String LPortString = "";
		if (LPort > 0)
			LPortString = ":" + String.valueOf(LPort);
		if (command.getRequestType() == DSHTTPRequestType.GET
				|| command.getRequestType() == DSHTTPRequestType.DELETE) {
			LMethodName = LMethodName.replace(".", "/").replaceAll("\"", "");
		} else {
			// POST
			LMethodName = LMethodName.replace(".", "/%22").replaceAll("\"",
					"%22");
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

	/**
	 * Returns a new instance of HttpClient. If connectionTimeout or
	 * communicationTimeout have been set, then this client will be using those
	 * values.
	 * 
	 * @return a new instance of HttpClient
	 */
	private HttpClient getHttpClient() {
		if (communicationTimeout > 0 || connectionTimeout > 0) {
			HttpParams httpParameters = new BasicHttpParams();
			if (connectionTimeout > 0)
				HttpConnectionParams.setConnectionTimeout(httpParameters,
						connectionTimeout);
			if (communicationTimeout > 0)
				HttpConnectionParams.setSoTimeout(httpParameters,
						communicationTimeout);
			return new DefaultHttpClient(httpParameters);
		} else {
			return new DefaultHttpClient();
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
	 *             where the parameter type is not allowed or in case that the
	 *             json response is an error message.
	 */

	public void execute(DSRESTCommand command) throws DBXException {
		HttpClient client = null;

		HttpUriRequest method = CreateRequest(command);
		try {
			if (isHttps) {
				if (SSLFactory != null) {
					client = SSLFactory.getHttpClient(connectionTimeout,
							communicationTimeout);
				} else {
					throw new DBXException("Cannot create https connection");
				}
			} else
				client = getHttpClient(); // fallback to the http one
			
			HttpResponse response = client.execute(method);
			setSessionIdentifier(response);
			throwExceptionIfNeeded(response);
			if (isThereOnlyOneStreamInOutput(command.getParameters())) {
				InputStream inputstream = response.getEntity().getContent();
				byte[] b1 = DBXTools.streamToByteArray(inputstream);
				TStream is = new TStream(b1);
				for (DSRESTParameter param : command.getParameters()) {
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
					String s = EntityUtils.toString(response.getEntity());
					TJSONObject json = TJSONObject.Parse(s);
					throwExceptionIfNeeded(json);
					TJSONArray results = json.getJSONArray("result");
					int returnParIndex = 0;
					for (DSRESTParameter param : command.getParameters()) {
						if ((param.Direction == DSRESTParamDirection.ReturnValue)
								|| (param.Direction == DSRESTParamDirection.InputOutput)
								|| (param.Direction == DSRESTParamDirection.Output)) {
							DBXJSONTools.JSONtoDBX(results.get(returnParIndex),
									param.getValue(), param.TypeName);
							returnParIndex++;
						} // if
					} // for
				} catch (DBXException e) {
					throw new DBXException(e);
				}
			}
		} catch (Exception e) {
			throw new DBXException(e);
		}
	}

	private void setSessionIdentifier(HttpResponse response) {
		boolean found = false;
		Header[] pragma = response.getHeaders("Pragma");
		for (Header h : pragma) {
			if (h.getValue().indexOf("dssession") >= 0)
				for (HeaderElement e : h.getElements()) {
					if (e.getName().equals("dssession")) {
						SessionID = e.getValue();
						found = true;
					}
					if (e.getName().equals("dssessionexpires"))
						SessionIDExpires = Long.valueOf(e.getValue());
				}
		}
		if (!found)
			CloseSession();
	}

	private boolean isThereOnlyOneStreamInOutput(
			List<DSRESTParameter> parameters) {
		if (isOnlyOneParameterInOutput(parameters)) {
			for (DSRESTParameter param : parameters) {
				if (((param.Direction == DSRESTParamDirection.ReturnValue)
						|| (param.Direction == DSRESTParamDirection.InputOutput) || (param.Direction == DSRESTParamDirection.Output))
						&& (param.getDBXType() == DBXDataTypes.BinaryBlobType)) {
					return true;
				} // if
			} // for
		}
		return false;
	}

	private boolean isOnlyOneParameterInOutput(List<DSRESTParameter> parameters) {
		int Count = 0;
		for (DSRESTParameter param : parameters) {
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
	private void throwExceptionIfNeeded(HttpResponse response)
			throws DBXException {
		if (response.getStatusLine().getStatusCode() != 200) {
			TJSONObject json = null;
			try {
				json = TJSONObject.Parse(EntityUtils.toString(response
						.getEntity()));
			} catch (Exception ex) {
			}
			if (json == null)
				throw new DBXException(response.getStatusLine()
						.getReasonPhrase());
			else
				throwExceptionIfNeeded(json);
		}
	}

	/**
	 * Throw an Exception inspecting the returned json object
	 * 
	 * @param json
	 *            the json response
	 * @throws JSONException
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

	private HttpUriRequest CreateRequest(DSRESTCommand command)
			throws DBXException {
		String URL = BuildRequestURL(command);
		TJSONArray _parameters = null;
		List<DSRESTParameter> ParametersToSend = new LinkedList<DSRESTParameter>();
		if (command.getParameters().size() > 0)
			for (DSRESTParameter parameter : command.getParameters()) {
				if (parameter.Direction == DSRESTParamDirection.Input
						|| parameter.Direction == DSRESTParamDirection.InputOutput)
					ParametersToSend.add(parameter);
			}
		if (command.getRequestType() == DSHTTPRequestType.GET
				|| command.getRequestType() == DSHTTPRequestType.DELETE) {
			for (DSRESTParameter parameter : ParametersToSend)
				URL += encodeURIComponent(parameter) + '/';
		} else // POST or PUT
		{
			boolean CanAddParamsToUrl = true;
			_parameters = new TJSONArray();
			for (DSRESTParameter parameter : ParametersToSend)
				if (CanAddParamsToUrl && isURLParameter(parameter))
					URL += encodeURIComponent(parameter) + '/';
				else // add the json rapresentation in the body
				{
					CanAddParamsToUrl = false;
					parameter.getValue().appendTo(_parameters);
				}
		}
		HttpUriRequest req = BuildRequest(command.getRequestType(), URL,
				_parameters);
		SetUpHeaders(req);
		return req;
	}

	private HttpUriRequest BuildRequest(DSHTTPRequestType requestType,
			String URL, TJSONArray _parameters) throws DBXException {
		switch (requestType) {
		case GET:
			return new HttpGet(URL);
		case DELETE:
			return new HttpDelete(URL);
		case POST: {
			try {
				HttpPost p = new HttpPost(URL);
				if (_parameters == null)
					throw new DBXException(
							"Parameters cannot be null in a POST request");
				if (_parameters.size() > 1) {
					TJSONObject body = new TJSONObject();
					body.addPairs("_parameters", _parameters);
					p.setEntity(new StringEntity(body.toString(), "utf-8"));
					return p;
				} else {
					if (_parameters.get(0) != null)
						p.setEntity(new StringEntity(_parameters.get(0)
								.toString(), "utf-8"));
					else
						p.setEntity(new StringEntity("null", "utf-8"));

					return p;
				}

			} catch (Exception ex) {
				throw new DBXException(ex.getMessage());
			}
		}
		case PUT: {
			try {
				HttpPut p = new HttpPut(URL);
				if (_parameters == null)
					throw new DBXException(
							"Parameters cannot be null in a POST request");
				if (_parameters.size() > 1) {
					TJSONObject body = new TJSONObject();
					body.addPairs("_parameters", _parameters);
					p.setEntity(new StringEntity(body.toString(), "utf-8"));
					return p;
				} else {
					p.setEntity(new StringEntity(_parameters.get(0).toString(),
							"utf-8"));
					return p;
				}
			} catch (Exception ex) {
				throw new DBXException(ex.getMessage());
			}
		}
		}
		return null;
	}

	private void SetUpSessionHeader(HttpUriRequest request) {
		if (SessionID != null)
			request.addHeader("Pragma", "dssession=" + SessionID);
	}

	private void SetUpAuthorizationHeader(HttpUriRequest request) {
		if (UserName == null || UserName.equals(""))
			request.addHeader("Authorization", "Basic Og=="); // no auth
		else {
			String auth = DBXDefaultFormatter.getInstance().Base64Encode(
					this.UserName + ":" + this.Password);
			request.addHeader("Authorization", "Basic " + auth); // auth
		}
	}

	private void SetUpHeaders(HttpUriRequest request) {
		request.addHeader("If-Modified-Since", "Mon, 1 Oct 1990 05:00:00 GMT");
		request.addHeader("Connection", "Keep-Alive");
		request.addHeader("Content-Type", "text/plain;charset=UTF-8");
		request.addHeader("Accept", "application/JSON");
		request.addHeader("Accept-Encoding", "identity");
		request.addHeader("User-Agent",
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

	/**
	 * Returns the time in milliseconds before a connection attempt times out.
	 * 
	 * @return the connectionTimeout
	 */
	public int getConnectionTimeout() {
		return connectionTimeout;
	}

	/**
	 * Sets the time in milliseconds before a connection attempt times out.
	 * 
	 * @param connectionTimeout
	 *            the connectionTimeout to set
	 */
	public void setConnectionTimeout(int connectionTimeout) {
		this.connectionTimeout = connectionTimeout;
	}

	/**
	 * Returns the time in milliseconds before a connection's request times out.
	 * 
	 * @return the communicationTimeout
	 */
	public int getCommunicationTimeout() {
		return communicationTimeout;
	}

	/**
	 * Sets the time in milliseconds before a connection's request times out.
	 * 
	 * @param communicationTimeout
	 *            the communicationTimeout to set
	 */
	public void setCommunicationTimeout(int communicationTimeout) {
		this.communicationTimeout = communicationTimeout;
	}
}
