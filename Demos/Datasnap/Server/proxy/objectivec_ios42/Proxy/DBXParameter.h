//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

#import <Foundation/Foundation.h>
#import "DBXValueType.h"
#import "DBXValue.h"

/**
 *@brief Wraps a DBXValue and allows it to represent itself as a json.
 */
@interface DBXParameter : DBXValueType {
@private
	DBXWritableValue * value;

}
/**
 *It returns internal DBXWritableValue.
*/
-(DBXWritableValue *) getValue;
/**
 *It set the data Type according to DBXDataTypes values.
 *@param dataType a DBXDataTypes value; 
 */
-(void) setDataType:(int)dataType;

/**
 *Returns the DataType
 *@return a DBXDataTypes value; 
 */
-(int) getDataType;
/**
	 * JSON representation of a TParam
	 * 
	 * @return a NSArray that represents a TParam 
	 */
-(id) toJson;
@end
