//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

package com.embarcadero.javablackberry;

import org.json.me.bc.*;

/**
 * 
 * Implements a JSON object.
 * 
 */

public class TJSONObject extends TJSONValue {

	protected JSONPairList Elements;

	public int getJsonValueType() {
		return JSONValueType.JSONObject;
	}

	protected JSONPairList buildElements(JSONObject o) {
		try {
			String pname;
			JSONPairList res = new JSONPairList();
			JSONArray keys = o.names();
			for (int i = 0; i < keys.length(); i++) {
				pname = keys.getString(i);
				Object obj = o.get(pname);
				if (obj == JSONObject.NULL) {
					res.add(new TJSONPair(pname, new TJSONNull()));
				} else if (obj instanceof String) {
					res.add(new TJSONPair(pname, ((String) obj)));
				} else if (obj instanceof Double) {
					res.add(new TJSONPair(pname, new TJSONNumber(((Double) obj)
							.doubleValue())));
				} else if (obj instanceof Integer) {
					res.add(new TJSONPair(pname, new TJSONNumber(
							((Integer) obj).intValue())));
				} else if (obj instanceof JSONArray) {
					res.add(new TJSONPair(pname, new TJSONArray(
							((JSONArray) obj))));
				} else if (obj instanceof JSONObject) {
					res.add(new TJSONPair(pname, new TJSONObject(
							((JSONObject) obj))));
				} else if (obj instanceof Boolean) {
					if (((Boolean) obj).booleanValue())
						res.add(new TJSONPair(pname, new TJSONTrue()));
					else
						res.add(new TJSONPair(pname, new TJSONFalse()));
				}
			}
			return res;
		} catch (JSONException ex) {
			return null;
		}

	}

	protected JSONObject asJSONObject() {
		try {
			JSONObject j = new JSONObject();
			TJSONPair pair;
			for (int i = 0; i < Elements.size(); i++) {
				pair = Elements.get(i);
				switch (pair.value.getJsonValueType()) {
				case JSONValueType.JSONObject: {
					j.put(pair.name, ((TJSONObject) pair.value).asJSONObject());
					break;
				}
				case JSONValueType.JSONArray: {
					j.put(pair.name, ((TJSONArray) pair.value).asJSONArray());
					break;
				}
				case JSONValueType.JSONString: {
					j.put(pair.name, ((TJSONString) pair.value).getValue());
					break;
				}
				case JSONValueType.JSONNumber: {
					j.put(pair.name, ((TJSONNumber) pair.value).getValue());
					break;
				}
				case JSONValueType.JSONTrue: {
					j.put(pair.name, true);
					break;
				}
				case JSONValueType.JSONFalse: {
					j.put(pair.name, false);
					break;
				}
				case JSONValueType.JSONNull: {
					j.put(pair.name, JSONObject.NULL);
					break;
				}
				}
			}
			return j;
		} catch (JSONException ex) {
			return null;
		}
	}

	/**
	 * Class constructor, initialized the internal list
	 */
	public TJSONObject() {
		super();
		Elements = new JSONPairList();
	}

	/**
	 * Parse the passed String into a new TJSONObject
	 * 
	 * @param value
	 * @return
	 */
	public static TJSONObject Parse(String value) {
		try {
			JSONObject o = new JSONObject(value);
			return new TJSONObject(o);
		} catch (JSONException e) {
			return null;
		}
	}

	/**
	 * Initialized the instance with a JSONObject;
	 * 
	 * @param json
	 */
	public TJSONObject(JSONObject json) {
		super();
		Elements = buildElements(json);
	}

	/**
	 * Class constructor, create the internal list and add the passed
	 * {@link TJSONPair}
	 * 
	 * @param pair
	 */
	public TJSONObject(TJSONPair pair) {
		super();
		Elements = new JSONPairList();
		addPairs(pair);
	}

	public String toString() {
		return asJSONObject().toString();
	}

	public Object getInternalObject() {
		return asJSONObject();
	}

	/**
	 * Returns a String value by the name
	 * 
	 * @param name
	 * @return
	 */
	public String getString(String name) {
		TJSONPair p;
		return ((p = get(name)) == null) ? null : ((TJSONString) p.value)
				.getValue();
	}

	/**
	 * Returns a Boolean value by the name
	 * 
	 * @param name
	 * @return
	 */
	public Boolean getBoolean(String name) {
		TJSONPair p = get(name);
		if (p == null)
			return null;
		if (p.value instanceof TJSONTrue)
			return Boolean.TRUE;
		else
			return Boolean.FALSE;
	}

	/**
	 * Returns a Double value by the name
	 * 
	 * @param name
	 * @return
	 */
	public Double getDouble(String name) {
		TJSONPair p;
		return ((p = get(name)) == null) ? null : ((TJSONNumber) p.value)
				.getValue();
	}

	/**
	 * Returns a TJSONObject value by the name
	 * 
	 * @param name
	 * @return
	 */
	public TJSONObject getJSONObject(String name) {
		TJSONPair p;
		return ((p = get(name)) == null) ? null : ((TJSONObject) (p.value));
	}

	/**
	 * Returns a Integer value by the name
	 * 
	 * @param name
	 * @return
	 */
	public Integer getInt(String name) {
		TJSONPair p;
		return ((p = get(name)) == null) ? null : new Integer(
				((TJSONNumber) p.value).getValue().intValue());
	}

	/**
	 * Returns a {@link TJSONArray} value by the name
	 * 
	 * @param name
	 * @return
	 */
	public TJSONArray getJSONArray(String name) {
		TJSONPair p;
		return ((p = get(name)) == null) ? null : ((TJSONArray) p.value);
	}

	/**
	 * Detects if it has a key with the name specified.
	 * 
	 * @param name
	 * @return
	 */
	public boolean has(String name) {
		return get(name) != null;
	}

	/**
	 * Gets a {@link TJSONPair} from a name
	 * 
	 * @param name
	 * @return
	 */
	public TJSONPair get(String name) {
		TJSONPair v;
		for (int i = 0; i < Elements.size(); i++) {
			v = (TJSONPair) Elements.get(i);
			if (v.name.equals(name))
				return v;
		}
		return null;
	}

	/**
	 * Adds {@link TJSONPair}
	 * 
	 * @param pair
	 * @return
	 */
	public TJSONObject addPairs(TJSONPair pair) {
		Elements.add(pair);
		return this;
	}

	/**
	 * Adds a pair with name and a {@link TJSONValue}
	 * 
	 * @param name
	 * @param value
	 * @return
	 */
	public TJSONObject addPairs(String name, TJSONValue value) {
		return addPairs(new TJSONPair(name, value));
	}

	/**
	 * Adds a pair with name and an int value
	 * 
	 * @param name
	 * @param value
	 * @return
	 */
	public TJSONObject addPairs(String name, int value) {
		return addPairs(name, new TJSONNumber(value));
	}

	/**
	 * Adds a pair with name and a String value
	 * 
	 * @param name
	 * @param value
	 * @return
	 */
	public TJSONObject addPairs(String name, String value) {
		return addPairs(name, new TJSONString(value));
	}

	/**
	 * Adds a pair with name and a long value
	 * 
	 * @param name
	 * @param value
	 * @return
	 */
	public TJSONObject addPairs(String name, long value) {
		return addPairs(name, new TJSONNumber(value));
	}

	/**
	 * Adds a pair with name and a double value
	 * 
	 * @param name
	 * @param value
	 * @return
	 */
	public TJSONObject addPairs(String name, double value) {
		return addPairs(name, new TJSONNumber(value));
	}

	/**
	 * Adds a pair with name and a boolean value
	 * 
	 * @param name
	 * @param value
	 * @return
	 */
	public TJSONObject addPairs(String name, boolean value) {
		if (value)
			return addPairs(name, new TJSONTrue());
		else
			return addPairs(name, new TJSONFalse());
	}
}