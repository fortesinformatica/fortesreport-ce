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
  *@brief Handles an internal array of JSONValue. 
  */ 
@interface TJSONValueList : NSObject {
	NSMutableArray * internalArray;
}
/**
 * Adds a TJSONValue
 * @param value TJSONValue to add
 * @return the position inside the list  
 */
-(int) addValue:(TJSONValue *)value;
/**
 * Gets a valu by its index
 * @param value the index to retrive
 * @return the TJSONValue or null  
 */
-(TJSONValue *) valueByIndex:(int) value;
/**
 * Removes a value using its index
 * @param value the index of the object
 *   
 */
-(void) removeValueByIndex:(int) value;
/**
 * The number of elements in the list
 */
-(int) count;



@end
