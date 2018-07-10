//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

#import "TJSONObject.h"


/**
 * 
 * @brief used to label the types that should be managed as a table
 *
 */
@protocol TableType <NSObject>

@end 
/**
 * 
 * @brief Interface that can be implemented by objects that know how to serialize themselves to {@link JSONObject}.
 *
 */
@protocol JSONSerializable<NSObject>
-(TJSONObject *) asJSONObject;	
@end
 /**
 * 
 * @brief Interface that can be implemented by objects that can be in null state.
 *
 */
@protocol NullableDBXValueType <NSObject>
/**
 *Sets the internal state to null
 *Classes that implement this protocol must set the appropriate value 
*/
- (void) setNull;
/**
 * Tests if the object is setted to null
 */
- (bool) isNull;
/**
 *Returns interlan object string rappresentation
 * 
 */
- (NSString*) getAsString;
@end