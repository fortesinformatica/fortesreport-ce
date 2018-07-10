//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

using System;
using System.Collections.Generic;
using Newtonsoft.Json.Linq;

namespace Embarcadero.Datasnap.WindowsPhone7
{

    /**
     * 
     * TParams manages a list of parameters.
     *
     * Use the properties and methods of TParams to:
     *	<br>&nbsp;&nbsp;&nbsp;&nbsp;- Get a parameter by name or index.
     *	<br>&nbsp;&nbsp;&nbsp;&nbsp;- Find parameter by name.
     * 	<br>&nbsp;&nbsp;&nbsp;&nbsp;- Add field parameters from the list.
     *	<br>&nbsp;&nbsp;&nbsp;&nbsp;- Create a TParams from a JObject or a metadata JArray.
     *	<br>&nbsp;&nbsp;&nbsp;&nbsp;- Loads parameters values.
     */
    public class TParams : JSONSerializable, TableType {
	    private List<DBXParameter> Params;

	    public TParams() : base() {
		    Params = new List<DBXParameter>();
	    }

	    public DBXParameter findParamByName(String Value) {
		    foreach (DBXParameter p in Params)
			    if (p.getName().Equals(Value))
				    return p;
		    return null;
	    }

    /**
	 * Get a parameter by name 
	 * @param Value
	 *            the name of the parameter
	 * @return the parameter
	 */
	    public DBXParameter getParamByName(String Value){
		    DBXParameter p;
		    if ((p = findParamByName(Value)) != null)
			    return p;
		    throw new DBXException("Parameter not found [" + Value + "]");
	    }

     /**
	 * Adds the passed parameter into the internal list 
	 * @param parameter
	 */
	    public TParams addParameter(DBXParameter parameter){
		    if (findParamByName(parameter.getName()) == null)
			    Params.Add(parameter);
		    else
			    throw new DBXException("Parameter name must be unique");
		    return this;
	    }

        /**
    	 * Get a parameter by index. 
    	 * @param Index
    	 * @return  DBXParameter
    	 */
	    public DBXParameter getParameter(int Index) {
		    return Params[Index];
	    }

     /**
	 * Returns the number of parameters contained in the internal list 
	 * @return int
	 */
	    public int size() {
		    return Params.Count;
	    }

	    /**
	     * 
	     * @param value
	     *            a compatible JSONObject that describe a DBXParameters
	     * @return a fully configured DBXParameters
	     */
	    public static TParams CreateFrom(TJSONObject value){
		    TParams parameters = CreateParametersFromMetadata(value
                    .getJSONArray("table"));
		    LoadParametersValues(parameters, value);
		    return parameters;
	    }

        /**
    	 * Loads parameter values from a json object specifying the offset
    	 * @param params
    	 * @param value
    	 * @param Offset
    	 * @return bool
    	 */
	    public static bool LoadParametersValues(TParams parameters,
			    TJSONObject value, int Offset){
		    JArray parValue;
            JObject jvalue = (JObject)value.getInternalObject();
		    DBXParameter par;
		    if (parameters.size() <= 0) {
			    return false;
		    }
		    for (int i = 0; i < parameters.size(); i++) {
			    par = parameters.getParameter(i);
			    parValue = jvalue.Value<JArray>(par.getName());
			    if (parValue.Count < Offset + 1)
				    return false;
                if (parValue[Offset] is JArray)
                    DBXJSONTools.JSONtoDBX(parValue[Offset].Value<JArray>(), par.getValue(), "");
                else
                    DBXJSONTools.JSONtoDBX(parValue[Offset].Value<object>(), par.getValue(), "");
		    }
		    return true;
	    }

        	/**
    	 * invokes LoadParametersValues with offset setting to 0. 
    	 * @param params
    	 * @param value
    	 */
	    public static void LoadParametersValues(TParams parameters, TJSONObject value) {
		    LoadParametersValues(parameters, value, 0);
	    }

         /**
    	 * Returns a new TParams created by the metadata represented in the specified {@link JSONArray}
    	 * @param paramsMetadata
    	 * @return TParams
    	 */
	    public static TParams CreateParametersFromMetadata(TJSONArray parametersMetadata) {
		    TParams o = new TParams();
		    JArray paramMetadata;
            JArray paramsMetadata = parametersMetadata.asJSONArray();
		    DBXParameter parameter;
		    try {
			    for (int i = 0; i < paramsMetadata.Count; i++) {
				    paramMetadata = paramsMetadata.Value<JArray>(i);
				    parameter = new DBXParameter();
				    DBXJSONTools.JSONToValueType(new TJSONArray(paramMetadata), parameter);
				    o.addParameter(parameter);
			    }
		    } catch (Exception e) {
			    throw new DBXException(e.Message);
		    }
		    return o;
	    }

    	/**
    	 * Returns this Object like a {@link JSONObject}
    	 */
	    public TJSONObject asJSONObject() {
		    return DBXJSONTools.DBXParametersToJSONObject(this);
	    }

    }
}
