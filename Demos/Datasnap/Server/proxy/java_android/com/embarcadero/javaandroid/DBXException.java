//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

package com.embarcadero.javaandroid;

/**
 * Exception descendant to raise error in proxy
 */
public class DBXException extends Exception {

	private Throwable Internal;

	public DBXException(String value) {
		super(value);
	}
	
	public DBXException(Throwable value) {
		super(value.getMessage());
		Internal = value;
	}

	public Throwable getInternal()
	{
		return Internal;
	}	
	
	public String getMessage()
	{
		String res;
		if ((res = super.getMessage()) == null)
			if (getInternal() != null)
				res = getThrowableMessage(getInternal());
		return res;
	}
	
	private String getThrowableMessage(Throwable t)
	{
		String res = null;
		if (t != null && ((res = t.getMessage()) == null) && 
		    (t.getCause() != null))
			res = getThrowableMessage(t.getCause());
		return res;
	}
	
	private static final long serialVersionUID = 154857855606412539L;

}
