//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

package com.embarcadero.javablackberry;

import java.util.Vector;

public class JSONValueList {
	private Vector items;

	public JSONValueList() {
		super();
		items = new Vector();
	}

	public JSONValueList add(TJSONValue JSONValue) {
		items.addElement(JSONValue);
		return this;
	}

	public TJSONValue get(int index) {
		return (TJSONValue) items.elementAt(index);
	}

	public JSONValueList remove(int index) {
		items.removeElementAt(index);
		return this;
	}

	public int size() {
		return items.size();
	}
}
