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
 * 
 * @brief handle conversion for numbers and boolean for jsonvalues. 
 *
 */
@interface TJSONNumber : TJSONValue {
	NSNumber * value;
}
/**
 * Initializes  the instance with a string
 * @param  aValue the string value
 * @return itself 
 */
-(id) initWithString:(NSString *)aValue;
/**
 * Initializes  the instance with a double
 * @param  aValue the double value
 * @return itself 
 */
-(id) initWithDouble:(double) aValue ;

/**
 * Initializes  the instance with an integer
 * @param  aValue the integer value
 * @return itself 
 */
-(id) initWithLong:(long) aValue;

/**
 * Initializes  the instance with an integer
 * @param  aValue the integer value
 * @return itself 
 */
-(id) initWithInt:(int) aValue ;

/**
 * Initializes  the instance with a NSNumber 
 * @param  aValue the NSNumber value
 * @return itself 
 */
-(id) initWithNumber:(NSNumber*)aValue;

/** 
 *  get the internal value as NSNumber
 *  @return an NSNumber 
 */
-(NSNumber *) getValue;


@end
@interface TJSONNumber(jsonnumberCreation)

/**
 * Create a TJSONNumber with a NSNumber
 * @param value an NSNumber
 * @return a TJSONNumber  
 */
+ (id) jsonNumberWithNumber:(NSNumber *) value;
@end