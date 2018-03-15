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
 * Implements a JSON array.
 * 
 */

public final class TJSONArray extends TJSONValue {

	protected JSONValueList Elements; // List<TJSONValue>

	/**
	 * Parse the passed String into a new TJSONArray
	 * 
	 * @param JSONString
	 * @return
	 * @throws DBXException 
	 */
	public static TJSONArray Parse(String JSONString) throws DBXException {
		try {
			return new TJSONArray(new JSONArray(JSONString));
		} catch (JSONException e) {
			throw new DBXException(e.getMessage());
		}
	}

	/**
	 * Initialized the instance with a TJSONValueList
	 * @param JSONValues
	 * @throws JSONException
	 */
	public TJSONArray(JSONValueList JSONValues) throws JSONException {
		super();
		Elements = JSONValues;
	}

	public String toString() {
		return asJSONArray().toString();
	}

	/**
	 * Initialized the instance
	 */
	public TJSONArray() {
		super();
		Elements = new JSONValueList();
	}

	/**
	 * Initialized the instance with a JSONArray; 
	 * @param json
	 */
	public TJSONArray(JSONArray json) {
		super();
		Elements = buildElements(json);
	}

	protected JSONValueList buildElements(JSONArray arr) {
		try {
			JSONValueList res = new JSONValueList();
			for (int i = 0; i < arr.length(); i++) {
				Object obj = arr.get(i);
				if (obj == JSONObject.NULL) {
					res.add(new TJSONNull());
				} else if (obj instanceof String) {
					res.add(new TJSONString((String) obj));
				} else if (obj instanceof Double) {
					res.add(new TJSONNumber(((Double) obj).doubleValue()));
				} else if (obj instanceof Long) {
					res.add(new TJSONNumber(((Long) obj).longValue()));
				}else if (obj instanceof Integer) {
					res.add(new TJSONNumber(((Integer) obj).intValue()));
				} else if (obj instanceof JSONArray) {
					res.add(new TJSONArray((JSONArray) obj));
				} else if (obj instanceof JSONObject) {
					res.add(new TJSONObject((JSONObject) obj));
				} else if (obj instanceof Boolean) {
					if (((Boolean) obj).booleanValue())
						res.add(new TJSONTrue());
					else
						res.add(new TJSONFalse());
				}
			}
			return res;
		} catch (JSONException ex) {
			return null;
		}

	}
	
	public TJSONArray add (int value){
		JSONArray app = (JSONArray) getInternalObject();
		app.put(value);
		Elements = buildElements(app);
		return this;
	}

	public TJSONArray add (long value){
		JSONArray app = (JSONArray) getInternalObject();
		app.put(value);
		Elements = buildElements(app);
		return this;
	}
	
	public TJSONArray add (boolean value){
		JSONArray app = (JSONArray) getInternalObject();
		app.put(value);
		Elements = buildElements(app);
		return this;
	}
	
	public TJSONArray add (double value) throws DBXException {
		JSONArray app = (JSONArray) getInternalObject();
		try {
			app.put(value);
		} catch (JSONException e) {
			throw new DBXException(e.getMessage());
		}
		Elements = buildElements(app);
		return this;
	}
	
	public TJSONArray add (String value){
		JSONArray app = (JSONArray) getInternalObject();
		app.put(value);
		Elements = buildElements(app);
		return this;
	}
	
	public TJSONArray add (Object value){
		JSONArray app = (JSONArray) getInternalObject();
		app.put(value);
		Elements = buildElements(app);
		return this;
	}
	/**
	 * Converts into JSONArray
	 * 
	 * @return
	 */
	protected JSONArray asJSONArray() {
		JSONArray arr = new JSONArray();
		TJSONValue v;
		for (int i = 0; i < Elements.size(); i++) {
			v = Elements.get(i);
			arr.put(v.getInternalObject());
		}
		return arr;
	}

	public Object getInternalObject() {
		return asJSONArray();
	}

	/**
	 * Adds a TJSonValue
	 * 
	 * @param value
	 * @return
	 */
	public TJSONArray add(TJSONValue value) {
		Elements.add(value);
		return this;
	}

	/**
	 * Returns a String value by the index
	 * 
	 * @param index
	 * @return
	 */
	public String getString(int index) {
		TJSONValue p;
		return ((p = get(index)) == null) ? null : ((TJSONString) p).getValue();
	}

	/**
	 * Returns a double value by the index
	 * 
	 * @param index
	 * @return
	 */
	public Double getDouble(int index) {
		TJSONValue p;
		return ((p = get(index)) == null) ? null : ((TJSONNumber) p).getValue();
	}

	/**
	 * Returns a TJSONObject value by the index
	 * 
	 * @param index
	 * @return
	 */
	public TJSONObject getJSONObject(int index) {
		TJSONValue p;
		return ((p = get(index)) == null) ? null : (TJSONObject) p;
	}

	/**
	 * Returns a Integer value by the index
	 * 
	 * @param index
	 * @return
	 */
	public Integer getInt(int index) {
		return new Integer(getDouble(index).intValue());
	}

	/**
	 * Returns a boolean value by the index
	 * 
	 * @param index
	 * @return
	 */
	public Boolean getBoolean(int index) {
		TJSONValue p = get(index);
		if (p == null)
			return null;
		if (p instanceof TJSONTrue)
			return Boolean.TRUE;
		else
			return Boolean.FALSE;
	}

	/**
	 * Returns a {@link TJSONArray} value by the index
	 * 
	 * @param index
	 * @return
	 */
	public TJSONArray getJSONArray(int index) {
		TJSONValue p;
		return ((p = get(index)) == null) ? null : (TJSONArray) p;
	}

	/**
	 * Returns a {@link TJSONValue} value by the index
	 * 
	 * @param index
	 * @return
	 */
	public TJSONValue get(int index) {
		return Elements.get(index);
	}

	/**
	 * Returns a {@link TJSONString} value by the index
	 * 
	 * @param index
	 * @return
	 */
	public TJSONString getAsJsonString(int index) {
		return (TJSONString) get(index);
	}

	/**
	 * Returns a {@link TJSONObject} value by the index
	 * 
	 * @param index
	 * 
	 * @return
	 */
	public TJSONObject getAsJsonObject(int index) {
		return (TJSONObject) get(index);
	}
	/**
	 * Returns a {@link TJSONArray} value by the index
	 * 
	 * @param index
	 * @return
	 */
	public TJSONArray getAsJsonArray(int index) {
		return (TJSONArray) get(index);
	}

	/**
	 * Remove a internal {@link TJSONValue} by specified index
	 * 
	 * @param index
	 * @return
	 */
	public TJSONArray remove(int index) {
		Elements.remove(index);
		return this;
	}

	/**
	 * Returns the number of elements contained in the internal list.
	 * @return
	 */
	public long size() {
		return Elements.size();
	}
	
	public int getJsonValueType() {
		return JSONValueType.JSONArray;
	}
}
