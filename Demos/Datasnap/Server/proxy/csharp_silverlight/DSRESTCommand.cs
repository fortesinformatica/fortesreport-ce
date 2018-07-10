//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

using System;
using System.Collections.Generic;

namespace Embarcadero.Datasnap.WindowsPhone7
{
   public class DSRESTCommand {

	private DSHTTPRequestType RequestType;
	private String FullyQualifiedMethodName;
	private List<DSRESTParameter> parameters = null;
	private DSRESTConnection Connection = null;

	DSAdmin Admin = null;
	
	
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
	
	public DSRESTCommand(DSRESTConnection Connection) : base (){
		
		this.Connection = Connection;
        Admin = new DSAdmin(Connection, (ex) => 
        {
            throw ex;
        });
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
	 * @return RequestType
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
	 * @return String
	 */
	public String getText() {
		return FullyQualifiedMethodName;
	}

     /**
	 * Prepares internal parameter list using information passed
	 * It prepares parameters list using information in an {@link DSRESTParameterMetaData}
	 * @param metadatas
	 */

	public void prepare(DSRESTParameterMetaData[] metadatas) {
		parameters = new List<DSRESTParameter>();
		foreach (DSRESTParameterMetaData param in metadatas) {
			DSRESTParameter p = new DSRESTParameter(
						param.Name,
						param.Direction,
						param.DBXType,
						param.TypeName);
			parameters.Add(p);
		}
	}

	public void PrepareAndExecute(Delegate PrepareCallback, Delegate UserCallback) {
		String LMethodName = getText();
        Admin.GetServerMethodParameters((TDBXReader MetaDatas) =>
        {
            parameters = new List<DSRESTParameter>();
            bool Cicla = true;
            bool IsEqual = false;
            while (MetaDatas.next())
            {
                if (Cicla && IsEqual) break;
                if (LMethodName.Equals(MetaDatas.getValue("MethodAlias").GetAsString()))
                {
                    IsEqual = true;
                    Cicla = false;
                    DSRESTParameter p = new DSRESTParameter(
                                MetaDatas.getValue("Name").GetAsString(), //param.Name,
                                MetaDatas.getValue("DBXParameterDirection").GetAsInt32(),//param.Direction,
                                MetaDatas.getValue("DBXType").GetAsInt32(),//param.DBXType,
                                MetaDatas.getValue("ParameterTypeName").GetAsString());//param.TypeName);
                    parameters.Add(p);
                }
                else
                {
                    Cicla = true;
                }
            }
            PrepareCallback.DynamicInvoke();
            Connection.execute(this, Admin, UserCallback);
        });
	}
	
	public List<DSRESTParameter> getParameters() {
		return parameters;
	}

    /**
	 * Returns internal parameter by index 
	 * @param index
	 * @return paramter
	 */

	public DSRESTParameter getParameter(int index) {
		return parameters[index];
	}


    	/**
	 * Returns internal parameter by name
	 * @param ParamName
	 * @return
	 */
	public DSRESTParameter getParameter(String ParamName) {
		foreach (DSRESTParameter p in getParameters())
			if (ParamName.Equals(p.Name))
				return p;
		return null;
	}
}
}
