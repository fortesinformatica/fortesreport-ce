//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

#import <Foundation/Foundation.h>
/**
 *@brief Base class to rapresent parameters to send to the server
*/

@interface DBXValueType : NSObject {

	 NSString *name;
	 NSString *caption;
	 int ordinal;
	 int subType;
	 long size;
	 long precision;
	 int scale;
	 int childPosition;
	 bool nullable;
	 int parameterDirection;
	 bool hidden;
	 bool valueParameter;
	 bool literal;
	
}
/**
 * Specifies the name
 */
@property (nonatomic,strong) NSString *name;
/**
 * Specifies the caption
 */
@property (nonatomic,strong) NSString *caption;

/**
 *  Specifies ordinal size.
 */
@property (nonatomic) int ordinal;
/**
 * Gets the subType
 */

@property (nonatomic) int subType;
/**
*Specifies the number of characters in a string-type parameter.  
*/
@property (nonatomic) long size;
/**
 *Specifies the number of digits allowed for a numeric parameter. 
 */
@property (nonatomic) long precision;
/**
 *Specifies the number of decimal places for the parameter. 
 */
@property (nonatomic) int scale;
/**
 *  Specifies the child position
 */

@property (nonatomic) int childPosition;
/**
 * Specifies whether the value can be nullable
 */
@property (nonatomic) bool nullable;
/**
 * Specifies the param direction input, output or input/output
 */
@property (nonatomic) int parameterDirection;
/**
 * Specifies if the param is hidden
 */

@property (nonatomic) bool hidden;
/**
 *   Specifies if it is a value parameter
 */
@property (nonatomic) bool valueParameter;
/**
 *  Specifies if it is literal
 */
@property (nonatomic) bool literal;
/**
 *Sets the type of field whose value the parameter represents. 
 */
-(void) setDataType:(int) dataType;
/**
 *Indicates the type of field whose value the parameter represents. 
 */
-(int) DataType;
@end
