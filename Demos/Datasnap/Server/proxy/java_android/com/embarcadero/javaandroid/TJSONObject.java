//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

package com.embarcadero.javaandroid;

import java.util.LinkedList;
import java.util.List;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

/**
 * 
 * Implements a JSON object.
 * 
 */

public class TJSONObject extends TJSONValue {

	protected List<TJSONPair> Elements;

	@Override
	public JSONValueType getJsonValueType() {
		return JSONValueType.JSONObject;
	}

	protected List<TJSONPair> buildElements(JSONObject o) {
		try {
			String pname;
			List<TJSONPair> res = new LinkedList<TJSONPair>();
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
			for (TJSONPair pair : Elements) {
				switch (pair.value.getJsonValueType()) {
				case JSONObject: {
					j.put(pair.name, ((TJSONObject) pair.value).asJSONObject());
					break;
				}
				case JSONArray: {
					j.put(pair.name, ((TJSONArray) pair.value).asJSONArray());
					break;
				}
				case JSONString: {
					j.put(pair.name, ((TJSONString) pair.value).getValue());
					break;
				}
				case JSONNumber: {
					j.put(pair.name, ((TJSONNumber) pair.value).getValue());
					break;
				}
				case JSONTrue: {
					j.put(pair.name, true);
					break;
				}
				case JSONFalse: {
					j.put(pair.name, false);
					break;
				}
				case JSONNull: {
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
		Elements = new LinkedList<TJSONPair>();
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
		Elements = new LinkedList<TJSONPair>();
		addPairs(pair);
	}

	@Override
	public String toString() {
		return asJSONObject().toString();
	}

	@Override
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
			return true;
		else
			return false;
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
		return ((p = get(name)) == null) ? null : ((TJSONNumber) p.value)
				.getValue().intValue();
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
		for (TJSONPair v : Elements) {
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