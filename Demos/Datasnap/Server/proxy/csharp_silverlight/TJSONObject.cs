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
     * Implements a JSON object.
     * 
     */
    public class TJSONObject : TJSONValue {

	    protected List<TJSONPair> Elements;

	    public override JSONValueType getJsonValueType() {
		    return JSONValueType.JSONObject;
	    }

	    protected List<TJSONPair> buildElements(JObject o) {
		    try {
			    String pname;
			    List<TJSONPair> res = new List<TJSONPair>();
                IEnumerator<KeyValuePair<String,JToken>> keys = o.GetEnumerator();
			    while (keys.MoveNext())
                {
				    pname = keys.Current.Key;
                    JToken jtk = keys.Current.Value;
                    JTokenType jttype = keys.Current.Value.Type;
                    switch (jttype) {
                        case JTokenType.Null: {
                            res.Add(new TJSONPair(pname, new TJSONNull()));
                            break;
                        }
                        case JTokenType.String:
                        {
                            res.Add(new TJSONPair(pname, new TJSONString(jtk.Value<string>())));
                            break;
                        }
                        case JTokenType.Float:
                        {
                            res.Add(new TJSONPair(pname, new TJSONNumber(jtk.Value<float>())));
                            break;
                        }
                        case JTokenType.Integer:
                        {
                            res.Add(new TJSONPair(pname, new TJSONNumber(jtk.Value<Int32>())));
                            break;
                        }
                        case JTokenType.Array:
                        {
                            res.Add(new TJSONPair(pname, new TJSONArray(jtk.Value<JArray>())));
                            break;
                        }
                        case JTokenType.Object:
                        {
                            res.Add(new TJSONPair(pname, new TJSONObject(jtk.Value<JObject>())));
                            break;
                        }
                        case JTokenType.Boolean:
                        {
                            if (jtk.Value<bool>())
                                res.Add(new TJSONPair(pname, new TJSONTrue()));
                            else
                                res.Add(new TJSONPair(pname, new TJSONFalse()));
                            break;
                        }
                    }
			    } 
			    return res;
		    } catch (Exception) {
			    return null;
		    }
	    }

	    protected JObject asJSONObject() {
		    try {
			    JObject j = new JObject();
			    foreach (TJSONPair pair in Elements) {
				    switch (pair.value.getJsonValueType()) {
				    case JSONValueType.JSONObject: {
					    j.Add(pair.name, ((TJSONObject) pair.value).asJSONObject());
					    break;
				    }
                    case JSONValueType.JSONArray:
                        {
					    j.Add(pair.name, ((TJSONArray) pair.value).asJSONArray());
					    break;
				    }
                    case JSONValueType.JSONString:
                        {
					    j.Add(pair.name, ((TJSONString) pair.value).getValue());
					    break;
				    }
                    case JSONValueType.JSONNumber:
                        {
                         if(((TJSONNumber) pair.value).isDouble  == true )
					      j.Add(pair.name, ((TJSONNumber) pair.value).getValue());
                         else
                          j.Add(pair.name, ((TJSONNumber)pair.value).getValueInt());

					    break;
				    }
                    case JSONValueType.JSONTrue:
                        {
					    j.Add(pair.name, true);
					    break;
				    }
                    case JSONValueType.JSONFalse:
                        {
					    j.Add(pair.name, false);
					    break;
				    }
                    case JSONValueType.JSONNull:
                        {
					    j.Add(pair.name, null);
					    break;
				    }
				    }
			    }
			    return j;
		    } catch (Exception) {
			    return null;
		    }
	    }

	    public TJSONObject() : base()
        {
		    Elements = new List<TJSONPair>();
	    }

     /**
	 * Parse the passed String into a new TJSONObject
	 * 
	 * @param value
	 * @return
	 */
	    public static TJSONObject Parse(String value) {
		    try {
			    JObject o = JObject.Parse(value);
			    return new TJSONObject(o);
		    } catch (Exception) {
			    return null;
		    }
	    }

        /**
    	 * Initialized the instance with a JSONObject;
    	 * 
    	 * @param json
    	 */
	    public TJSONObject(JObject json) : base()
        {
		    Elements = buildElements(json);
	    }

        	/**
    	 * Class constructor, create the internal list and add the passed
    	 * {@link TJSONPair}
    	 * 
    	 * @param pair
    	 */
	    public TJSONObject(TJSONPair pair) : base()
        {
		    Elements = new List<TJSONPair>();
		    addPairs(pair);
	    }

	    public override String ToString() {
		    return asJSONObject().ToString(Newtonsoft.Json.Formatting.None);
	    }

	    public override Object getInternalObject() {
		    return asJSONObject();
	    }

        /**
    	 * Returns a String value by the name
    	 * @param name
    	 * @return String
    	 */
	    public String getString(String name) {
		    TJSONPair p;
		    return ((p = get(name)) == null) ? null : ((TJSONString) p.value)
				    .getValue();
	    }


        /**
    	 * Returns a Boolean value by the name 
    	 * @param name
    	 * @return boolean?
    	 */
	    public Boolean? getBoolean(String name) {
		    TJSONPair p = get(name);
		    if (p == null)
			    return null;
		    if (p.value is TJSONTrue)
			    return true;
		    else
			    return false;
	    }

        /**
    	 * Returns a Double value by the name
    	 * @param name
    	 * @return double?
    	 */
	    public Double? getDouble(String name) {
            TJSONPair p = get(name);
            if (p == null)
                return null;
            return Convert.ToDouble(((TJSONNumber)p.value).getValue());
	    }


         /**
    	 * Returns a TJSONObject value by the name
    	 * @param name
    	 * @return TJSONObject
    	 */
	    public TJSONObject getJSONObject(String name) {
		    TJSONPair p;
		    return ((p = get(name)) == null) ? null : ((TJSONObject) (p.value));
	    }

        /**
    	 * Returns a Integer value by the name 
    	 * @param name
    	 * @return int?
    	 */
	    public int? getInt(String name) {
            TJSONPair p = get(name);
            if (p == null)
                return null;
            return Convert.ToInt32(((TJSONNumber) p.value).getValue());
	    }

       /**
    	 * Returns a {@link TJSONArray} value by the name 
    	 * @param name
    	 * @return TJSONArray 
    	 */
	    public TJSONArray getJSONArray(String name) {
		    TJSONPair p;
		    return ((p = get(name)) == null) ? null : ((TJSONArray) p.value);
	    }

        /**
    	 * Detects if it has a key with the name specified. 
    	 * @param name
    	 * @return boolean
    	 */
	    public Boolean has(String name) {
		    return get(name) != null;
	    }

        /**
    	 * Gets a {@link TJSONPair} from a name
    	 * @param name
    	 * @return  TJSONPair
    	 */
	    public TJSONPair get(String name) {
		    foreach (TJSONPair v in Elements) {
			    if (v.name.Equals(name))
				    return v;
		    }
		    return null;
	    }

        /**
    	 * Adds {@link TJSONPair}
    	 * @param pair
    	 * @return TJSONObject
    	 */
	    public TJSONObject addPairs(TJSONPair pair) {
		    Elements.Add(pair);
		    return this;
	    }

        /**
    	 * Adds a pair with name and a {@link TJSONValue}
    	 * @param String name
    	 * @param TJSONValue value
    	 * @return TJSONObject
    	 */
	    public TJSONObject addPairs(String name, TJSONValue value) {
		    return addPairs(new TJSONPair(name, value));
	    }

        /**
    	 * Adds a pair with name and an int value
    	 * @param String name
    	 * @param int value
    	 * @return TJSONObject
    	 */
	    public TJSONObject addPairs(String name, int value) {
		    return addPairs(name, new TJSONNumber(value));
	    }

        /**
    	 * Adds a pair with name and a String value
    	 * @param String name
    	 * @param String value
    	 * @return TJSONObject
    	 */
	    public TJSONObject addPairs(String name, String value) {
		    return addPairs(name, new TJSONString(value));
	    }

        /**
    	 * Adds a pair with name and a long value 
    	 * @param String name
    	 * @param long value
    	 * @return TJSONObject
    	 */
	    public TJSONObject addPairs(String name, long value) {
		    return addPairs(name, new TJSONNumber(value));
	    }

        /**
    	 * Adds a pair with name and a double value
    	 * @param String name
    	 * @param double value
    	 * @return TJSONObject
    	 */
	    public TJSONObject addPairs(String name, double value) {
		    return addPairs(name, new TJSONNumber(value));
	    }

        /**
    	 * Adds a pair with name and a boolean value 
    	 * @param String name
    	 * @param bool value
    	 * @return TJSONObject
    	 */
	    public TJSONObject addPairs(String name, bool value) {
		    if (value)
			    return addPairs(name, new TJSONTrue());
		    else
			    return addPairs(name, new TJSONFalse());
	    }
    }
}