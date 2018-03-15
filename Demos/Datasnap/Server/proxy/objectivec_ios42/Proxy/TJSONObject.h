//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

#import <Foundation/Foundation.h>
#import "TJSONValue.h"
#import "TJSONPairList.h"
/**
 * 
 * @brief Implements a JSON object.
 * 
 */
@interface TJSONObject : TJSONValue {
	@protected
    TJSONPairList * elements;
	

}
/**
 * Initializes with a formatted json string
 * @param stringJson the formatted json string
 * @return a TJSonObject  
 */

-(id) initWithJSONString:(NSString *) stringJson;
/**
 * Initializes with a  json object
 * @param json the json object
 * @return a TJSonObject  
 */
-(id) initWithJSONObject:(NSDictionary *) json;
/**
 * Adds a TJSONPair
 * @param value the TJSONPair object
 * @return itself  
 */
-(id) addPairs:(TJSONPair *)value;
/**
 * Adds a pair with name and a TJSONValue
 * @param value the TJSONPair object
 * @return itself  
 */

-(id) addPairs: (NSString*) name withJSONValue:(TJSONValue *) value;
/**
 * Adds a pair with name and an integer
 * @param value the integer value
 * @return itself  
 */
-(id) addPairs:(NSString *)name withInt:(int)value;
/**
 * Adds a pair with name and an integer
 * @param value the integer value
 * @return itself  
 */
-(id) addPairs:(NSString *)name withLong:(long)value;
/**
 * Adds a pair with name and a double
 * @param value the double value
 * @return itself  
 */
-(id) addPairs:(NSString *)name withDouble:(double)value;
/**
 * Adds a pair with name and a string
 * @param value the NSString value
 * @return itself  
 */
-(id) addPairs:(NSString *) name withString:(NSString *)value;
/**
 * Adds a pair with name and a boolean
 * @param value the boolean value
 * @return itself  
 */
-(id) addPairs:(NSString *)name withBool:(BOOL)value;

/**
 * Gets a  string for the key using its name.
 * @param name the key name to find
 * @return the string value  
 */
-(NSString *) getStringForKey:(NSString *)name;
/**
 * Gets a  TJSONArray for the key using its name.
 * @param name the key name to find
 * @return the TJSONArray value  
 */
-(TJSONArray *) getJSONArrayForKey:(NSString *)name;
/**
 * Gets a  TJSONObject  for the key using its name.
 * @param name the key name to find
 * @return the TJSONObject value  
 */
-(TJSONObject *) getJSONObjectForKey:(NSString * ) name;
/**
 * Gets an integer for the key using its name.
 * @param name the key name to find
 * @return the integer value  
 */
-(int) getIntForKey:(NSString * ) name;
/**
 * Gets an  integer for the key using its name.
 * @param name the key name to find
 * @return the integer value  
 */
-(long) getLongForKey:(NSString * ) name;
/**
 * Gets a  double for the key using its name.
 * @param name the key name to find
 * @return the double value  
 */
-(double) getDoubleForKey:(NSString * ) name;
/**
 * Gets a  boolean for the key using its name.
 * @param name the key name to find
 * @return the boolean value  
 */
-(BOOL) getBoolForKey:(NSString * ) name;
/**
 * Detects if it has a key with the name specified.
 * @param name the key name to find
 * @return YES or NO according if the key exists 
 */
-(BOOL) hasKey:(NSString *)name;
/**
 * Gets a  TJSONPari for the key using its name.
 * @param name the key name to find
 * @return the TJSONKey value  
 */
-(TJSONPair *)getPairForKey:(NSString *)name;
/**
 *The number of elements in object.
 */
-(int) count;
@end

@interface TJSONObject(jsonobjectCreations)
/**
 * Create a TJSONObject parsing a standar json formatted string
 * @param value it the formatted json string
 * @return returns a TJSONObject istance  
 */
+(id)parse:(NSString *) value;
@end

