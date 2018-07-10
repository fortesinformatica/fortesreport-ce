//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************


#import <Foundation/Foundation.h>
typedef enum  {
	JSONObject=1 ,
	JSONArray=2,
	JSONString=3,
	JSONNumber=4,
	JSONTrue=5,
	JSONFalse=6,
	JSONNull=7
} JSONValueType;

@class TJSONObject;
@class TJSONArray;
/**
 *@brief  Represents the ancestor class for the TJSON classes .
 */
@interface TJSONValue : NSObject {


}
/**
 * It returns as a json array
*/
-(NSArray *) asJSONArray;  
/**
 * It returns as a json formatted string
 */
-(NSString *) asJSONString;
/**
 *It returns as a JSON Object
 */
-(NSDictionary *) asJSONObject;
/**
 * Gets the internal object
 */
-(id) getInternalObject;
/**
 * Converts to a json formatted string
 */
-(NSString *) toString ;
/**
 * It returns the json null string "null"
 */
- (NSString *) nullString;
/**
 *  It returns the json value type.
 */
-(JSONValueType) getJSONValueType;
@end
