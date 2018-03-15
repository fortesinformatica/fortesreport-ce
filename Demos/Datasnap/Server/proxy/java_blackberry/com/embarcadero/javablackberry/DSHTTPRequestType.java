//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

package com.embarcadero.javablackberry;

/**
 * 
 * A class by implementing the type safe enum pattern, represents the request types supported (Get, Post, Put, Delete)
 *
 */

public class DSHTTPRequestType {

	public static final DSHTTPRequestType GET = new DSHTTPRequestType("GET");
	public static final DSHTTPRequestType POST = new DSHTTPRequestType("POST");
	public static final DSHTTPRequestType PUT = new DSHTTPRequestType("PUT");
	public static final DSHTTPRequestType DELETE = new DSHTTPRequestType("DELETE");

	private final String name;

	private DSHTTPRequestType(String name) {
		this.name = name;
	}

	public String toString() {
		return name;
	}

}