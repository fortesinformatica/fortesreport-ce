//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

package com.embarcadero.javaandroid;

import java.util.ArrayList;
import java.util.List;

/**
 * 
 * TParams manages a list of parameters.
 * 
 * Use the properties and methods of TParams to: <br>
 * &nbsp;&nbsp;&nbsp;&nbsp;- Get a parameter by name or index. <br>
 * &nbsp;&nbsp;&nbsp;&nbsp;- Find parameter by name. <br>
 * &nbsp;&nbsp;&nbsp;&nbsp;- Add field parameters from the list. <br>
 * &nbsp;&nbsp;&nbsp;&nbsp;- Create a TParams from a JSONObject or a metadata
 * JSONArray. <br>
 * &nbsp;&nbsp;&nbsp;&nbsp;- Loads parameters values.
 */

public class TParams implements JSONSerializable, TableType {
	private List<DBXParameter> Params;

	/**
	 * Class constructor, create a new TParams and initialized the internal list
	 * of parameters
	 */
	public TParams() {
		super();
		Params = new ArrayList<DBXParameter>();
	}

	/**
	 * Get a parameter by name
	 * 
	 * @param Value
	 * @return
	 */
	public DBXParameter findParamByName(String Value) {
		for (DBXParameter p : Params)
			if (p.getName().equals(Value))
				return p;
		return null;
	}

	/**
	 * Get a parameter by name
	 * 
	 * @param Value
	 *            the name of the parameter
	 * @return the parameter
	 */
	public DBXParameter getParamByName(String Value) throws DBXException {
		DBXParameter p;
		if ((p = findParamByName(Value)) != null)
			return p;
		throw new DBXException("Parameter not found [" + Value + "]");
	}

	/**
	 * Adds the passed parameter into the internal list
	 * 
	 * @param parameter
	 * @return
	 * @throws DBXException
	 *             if the list contained also a parameter with the same name of
	 *             the parameter passed
	 */
	public TParams addParameter(DBXParameter parameter) throws DBXException {
		if (findParamByName(parameter.getName()) == null)
			Params.add(parameter);
		else
			throw new DBXException("Parameter name must be unique");
		return this;
	}

	/**
	 * Get a parameter by index.
	 * 
	 * @param Index
	 * @return
	 */
	public DBXParameter getParameter(int Index) {
		return Params.get(Index);
	}

	/**
	 * Returns the number of parameters contained in the internal list
	 * 
	 * @return
	 */
	public int size() {
		return Params.size();
	}

	/**
	 * 
	 * Returns a new TParams initialized to the value represented by the
	 * specified {@link JSONObject}
	 * 
	 * @param value
	 *            a compatible JSONObject that describe a DBXParameters
	 * @return a fully configured DBXParameters
	 * @throws JSONException
	 * @throws DBXException
	 */
	public static TParams CreateFrom(TJSONObject value) throws DBXException {
		TParams params = CreateParametersFromMetadata(value
				.getJSONArray("table"));
		LoadParametersValues(params, value);
		return params;
	}

	/**
	 * Loads parameter values from a json object specifying the offset
	 * 
	 * @param params
	 * @param value
	 * @param Offset
	 * @return
	 * @throws JSONException
	 * @throws DBXException
	 */
	public static boolean LoadParametersValues(TParams params,
			TJSONObject value, int Offset) throws  DBXException {
		TJSONArray parValue;
		DBXParameter par;
		if (params.size() <= 0) {
			return false;
		}
		for (int i = 0; i < params.size(); i++) {
			par = params.getParameter(i);
			parValue = value.getJSONArray(par.getName());
			if (parValue.size() < Offset + 1)
				return false;
			DBXJSONTools.JSONtoDBX(parValue.get(Offset), par.getValue(), "");
		}
		return true;
	}

	/**
	 * invokes LoadParametersValues with offset setting to 0.
	 * 
	 * @param params
	 * @param value
	 * @throws JSONException
	 * @throws DBXException
	 */
	public static void LoadParametersValues(TParams params, TJSONObject value)
			throws DBXException {
		LoadParametersValues(params, value, 0);
	}

	/**
	 * Returns a new TParams created by the metadata represented in the
	 * specified {@link JSONArray}
	 * 
	 * @param paramsMetadata
	 * @return
	 * @throws DBXException
	 */
	public static TParams CreateParametersFromMetadata(TJSONArray paramsMetadata)
			throws DBXException {
		TParams o = new TParams();
		TJSONArray paramMetadata;
		DBXParameter parameter;
			for (int i = 0; i < paramsMetadata.size(); i++) {
				paramMetadata = paramsMetadata.getJSONArray(i);
				parameter = new DBXParameter();
				DBXJSONTools.JSONToValueType(paramMetadata, parameter);
				o.addParameter(parameter);
			}
		return o;
	}

	/**
	 * Returns this Object like a {@link JSONObject}
	 */
	public TJSONObject asJSONObject() throws DBXException {
		return DBXJSONTools.DBXParametersToJSONObject(this);
	}

}
