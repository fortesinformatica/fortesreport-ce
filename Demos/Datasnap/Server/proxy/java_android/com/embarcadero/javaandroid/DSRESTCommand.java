//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

package com.embarcadero.javaandroid;

import java.io.IOException;
import java.util.LinkedList;
import java.util.List;

import org.apache.http.client.ClientProtocolException;

/**
 * Allows you to prepare and execute a request of type REST.
 * 
 * Use the properties and methods of DSRESTCommand to: <br>
 * &nbsp;&nbsp;&nbsp;&nbsp;- Set and get the request type. <br>
 * &nbsp;&nbsp;&nbsp;&nbsp;- Set and get the specific method to call. <br>
 * &nbsp;&nbsp;&nbsp;&nbsp;- Prepare the command from a metadata input
 * parameter. <br>
 * &nbsp;&nbsp;&nbsp;&nbsp;- Get all the parameters contained. <br>
 * &nbsp;&nbsp;&nbsp;&nbsp;- Get a parameter by index or name.
 * 
 */


public class DSRESTCommand {

	private DSHTTPRequestType RequestType;
	private String FullyQualifiedMethodName;
	private List<DSRESTParameter> parameters = null;
	private DSRESTConnection Connection = null;

	DSAdmin Admin = null;

	/**
	 * Class constructor, create a new DSRESTCommand initialized with the specified DSRESTConnection
	 * @param Connection
	 */
	public DSRESTCommand(DSRESTConnection Connection) {
		super();
		this.Connection = Connection;
		Admin = new DSAdmin(Connection);
	}

	/**
	 * Set the Kind of request (GET,PUT,POST,DELETE)
	 * @param RequestType
	 */
	public void setRequestType(DSHTTPRequestType RequestType) {
		this.RequestType = RequestType;
	}

	/**
	 * Returns the Kind of request (GET,PUT,POST,DELETE) 
	 * @return
	 */
	public DSHTTPRequestType getRequestType() {
		return this.RequestType;
	}

	/**
	 * Set the fully qualified method name to execute on the server 
	 * @param FullyQualifiedMethodName
	 */
	public void setText(String FullyQualifiedMethodName) {
		this.FullyQualifiedMethodName = FullyQualifiedMethodName;
	}

	/**
	 * Fully qualified method name to execute on the server 
	 * @return
	 */
	public String getText() {
		return FullyQualifiedMethodName;
	}

	/**
	 * Prepares internal parameter list using information passed.<br>
	 * It prepares parameters list using information in an {@link DSRESTParameterMetaData}
	 * @param metadatas
	 */
	public void prepare(DSRESTParameterMetaData[] metadatas) {
		parameters = new LinkedList<DSRESTParameter>();
		for (DSRESTParameterMetaData param : metadatas) {
			DSRESTParameter p = new DSRESTParameter(param.Name,
					param.Direction, param.DBXType, param.TypeName);
			parameters.add(p);
		}
	}

	/**
	 * Prepares internal parameters list asking the server.<br>
	 * 
	 * It prepares internal parameters array getting information from the server.
	 * @throws DBXException
	 */
	public void prepare() throws DBXException {
		String LMethodName = getText();
		TDBXReader MetaDatas = Admin.GetServerMethodParameters();
		parameters = new LinkedList<DSRESTParameter>();
		boolean Cicla = true;
		boolean IsEqual = false;
		while (MetaDatas.next()) {
			if (Cicla && IsEqual)
				break;
			if (LMethodName.equals(MetaDatas.getValue("MethodAlias").GetAsString())) {
				IsEqual = true;
				Cicla = false;
				// only if is a valid type
				if (MetaDatas.getValue("DBXType").GetAsInt32() > 0){
					DSRESTParameter p = new DSRESTParameter(MetaDatas.getValue(
							"Name").GetAsString(), // param.Name,
							MetaDatas.getValue("DBXParameterDirection")
									.GetAsInt32(),// param.Direction,
							MetaDatas.getValue("DBXType").GetAsInt32(),// param.DBXType,
							MetaDatas.getValue("ParameterTypeName").GetAsString());// param.TypeName);
					parameters.add(p);
				}
			} else {
				Cicla = true;
			}
		}
	}

	public List<DSRESTParameter> getParameters() {
		return parameters;
	}

	/**
	 * Returns internal parameter by index 
	 * @param index
	 * @return
	 */
	public DSRESTParameter getParameter(int index) {
		return parameters.get(index);
	}

	/**
	 * Returns internal parameter by name
	 * @param ParamName
	 * @return
	 */
	public DSRESTParameter getParameter(String ParamName) {
		for (DSRESTParameter p : getParameters())
			if (ParamName.equals(p.Name))
				return p;
		return null;
	}

	/**
	 * Invokes the execute method of the internal {@link DSRESTConnection}
	 * passing this DSRESTCommand.
	 * 
	 * @throws IOException
	 * @throws DBXException
	 * @throws ClientProtocolException
	 */
	public void execute() throws ClientProtocolException, IOException,
			DBXException {
		Connection.execute(this);
	}

}
