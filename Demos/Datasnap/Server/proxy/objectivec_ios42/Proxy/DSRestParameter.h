//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

#import <Foundation/Foundation.h>
#import "DSRestParamDirection.h"
#import "DBXDataTypes.h"
#import "DBXValue.h"

/**
 * 
 * @brief Represents and incorporates all relevant information to form a parameter you 
 *  can send or receive in a request.
 */

@interface DSRestParameter : NSObject {
@private
	NSString * name;
	DSRESTParamDirection direction;
	DBXDataTypes DBXType;
	NSString * typeName;
	DBXValue * value;
}
/**
 * the name of the parameter. It can be empty if it is the return value-
 */
@property (nonatomic,retain) NSString * name;
/**
 * Direction of the parameter. It is an input, output or input/output .
 **/

@property (nonatomic)DSRESTParamDirection direction;
/**
 * The type rappresenting the parameter.
 */
@property (nonatomic)DBXDataTypes DBXType;
/**
 * String Name of the type. 
 */
@property (nonatomic,retain) NSString * typeName;

/**
	 * Initializes the parameters
	 * 
	 * @param aname the name of the parameter.
	 * @param adirection a value of type integer that represents the parameter direction, respecting the conventions of the {@link DSRESTParamDirection}.  
	 * @param aTypeName a string that contains the TypeName of this parameter.
	 */
- (id) initWithParameter:(NSString *) aname  withDirection: (DSRESTParamDirection) adirection 
			withTypeName: (NSString *) aTypeName ;

 /**
	 * Returns the {@link DBXValue} of this parameter
	 * 
	 * @return the DBXValue value contained in this object.
	 */
-(DBXValue *) getValue;
@end
