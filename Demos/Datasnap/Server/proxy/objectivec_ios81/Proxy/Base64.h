 /**
    * Base64.java Porting to Objective-c
    *
    * Brazil project web application toolkit,
    * export version: 2.0 
    * Copyright (c) 2000-2002 Sun Microsystems, Inc.
    *
    * Sun Public License Notice
    *
    * The contents of this file are subject to the Sun Public License
    * Version 1.0 (the "License"). You may not use this file except in
    * compliance with the License. A copy of the License is included as
    * the file "license.terms", and also available at
    * http://www.sun.com/
    * 
    * The Original Code is from:
    *    Brazil project web application toolkit release 2.0.
    * The Initial Developer of the Original Code is: cstevens.
    * Portions created by cstevens are Copyright (C) Sun Microsystems,
    * Inc. All Rights Reserved.
    * 
    * Contributor(s): cstevens, suhler.
    *
    * Version:  1.9
    * Created by cstevens on 00/04/17
	
    * Last modified by Embarcadero Technologies Inc. on 17/06/2011
    */
	
   /**
    * Utility to base64 encode and decode a string.
    * 
    * @author Stephen Uhler
    * @version 1.9, 02/07/24
    */
	
#import <Foundation/Foundation.h>
/**
 * @brief  NSString extension for string and stream base64 conversion. Extension to NSString object to encode  a string using base64 encoding
 */ 

@interface NSString (NSStringBase64) 
/**
 * Encode a string using base64 encoding
 * @param strData is the string to be encoded
 * @return retunr the base64 string encoding for the string parameter.  
 */

+ (NSString *)encodeBase64WithString:(NSString *)strData;
/**
 * Encode an NSdata using base64 encoding
 * @param objData is the NSData to be encoded
 * @return retunr the base64 string encoding for the  parameter.  
 */

+ (NSString *)encodeBase64WithData:(NSData *)objData; 

@end

