//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

#import <Foundation/Foundation.h>
#import "TJSONValue.h"
#import "TJSONValueList.h"
@class TJSONString;
/**
 * 
 * @brief Implements a JSON array.
 * 
 */
@interface TJSONArray : TJSONValue {
@protected 
	 TJSONValueList *	elements;

	  
}
/**
 *Converts  into JSONArray
 */
- (id) asJSONArray;
/**
 * Convert into a formatted json string.
 */
- (NSString *) asJSONString;
/**
 * Initialized the istance with a formatted json string;
 * @param stringJson the formatted json string
 * @return a TJSONValue  
 */
-(id) initWithJSONString:(NSString *) stringJson;
/**
 * Initialized the instance with a JSON object
 * @param json a json object
 * @return a TJSONValue  
 */

-(id) initWithJSONArray:(NSArray *) json;

/**
 * Initialized the instance with a TJSONValueList
 * @param value the TJSONValueList
 * @return a TJSONValue  
 */

-(id) initWithJSONValues:(TJSONValueList *) value;
/**
 * Adds a TJSonValue
 * @param value a TJSONValue
 * @returns a itlsef 
 */
-(id) addValue:(TJSONValue*) value;
/**
 * Removes a value by its index
 * @param index  index of the value to remove
 * @return itself  
*/
-(id) removeValueByIndex:(int) index;

/*
   Returns the number of elements
   @return the number of elements
*/
-(int) count;
/**
 *Returns a value by its index
 *@param index  index of the value to get
 *@return the value or null  
 */   
-(TJSONValue*) valueByIndex:(int)index;

/**
 *  Returns a string value by the index
 *  @param index  the index of the value
 *  @return the value as string  
 */

-(NSString *) stringByIndex:(int) index;
/**
 *  Returns a double value by the index
 *  @param index  the index of the value
 *  @return the value as doble  
 */
-(double) doubleByIndex:(int) index ;
/**
 *  Returns a TJSONObject value by the index
 *  @param index  the index of the value
 *  @return the value as TSJONObject  
 */
-(TJSONObject *) JSONObjectByIndex:(int) index ;
/**
 *  Returns an integer value by the index
 *  @param index  the index of the value
 *  @return the value as integer  
 */
-(int) intByIndex:(int) index;

/**
 *  Returns an integer value by the index
 *  @param index  the index of the value
 *  @return the value as integer  
 */
-(long) longByIndex:(int) index;
/**
 *  Returns a boolean value by the index
 *  @param index  the index of the value
 *  @return the value as boolean  
 */
-(BOOL) boolByIndex:(int) index ;
/**
 *  Returns a TJSONString value by the index
 *  @param index  the index of the value
 *  @return the value as TJSONString  
 */
-(TJSONString *) JSONStringByIndex:(int) index ;


@end
