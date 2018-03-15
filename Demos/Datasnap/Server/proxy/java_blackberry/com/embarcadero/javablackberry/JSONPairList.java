//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

package com.embarcadero.javablackberry;

import java.util.Vector;

public class JSONPairList {
	private Vector items;

	public JSONPairList() {
		super();
		items = new Vector();
	}

	public JSONPairList add(TJSONPair JSONPair) {
		items.addElement(JSONPair);
		return this;
	}

	public TJSONPair get(int index) {
		return (TJSONPair) items.elementAt(index);
	}

	public JSONPairList remove(int index) {
		items.removeElementAt(index);
		return this;
	}

	public int size() {
		return items.size();
	}
}
