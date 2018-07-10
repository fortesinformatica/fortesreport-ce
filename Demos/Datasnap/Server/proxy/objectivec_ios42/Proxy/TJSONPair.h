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
 * @brief Represents json pair objetcs.
 * A JSONPair is an object that has a name an value that rappresents a TJSONValue 
 */

@interface TJSONPair : NSObject {
	NSString * name;
	TJSONValue * value;

}
/**
 *The name that identified the value .
*/
@property (retain,nonatomic)NSString * name;
/**
 *The value is TJSONValue .
 */
@property (retain,nonatomic)TJSONValue * value;

/**
 *Initializes the TJSONPari with a TJSOnValue with the specified name.
*/
-(id) initWithName:(NSString* )aName andValue:(TJSONValue*)aValue;
@end
/**
 *Creates the TJSONPari with a TJSOnValue with the specified name.
*/
@interface TJSONPair(JSONPairCreation)

+(id) JSONPairWithName:(NSString* )aName andValue:(TJSONValue*)aValue;
@end



