//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

#import <Foundation/Foundation.h>
#import "TJSONValue.h"

/**
 * @brief Represents json string objects
 */

@interface TJSONString : TJSONValue {
	NSString * value;
}
/**
 * Initialises the istance with a string.
 * @param aValue the value to hold
 * @return itself  
 */
-(id) initWithString:(NSString *) aValue;
/**
 * Returns the string value holded.
 */
-(NSString *) getValue;
@end

@interface TJSONString(jsonstringCreation)
/**
 *  Create a JSONString object with the given string
 *  @param aValue the string to hold 
 */
+(id) JSONStringWithString:(NSString *) aValue;

@end