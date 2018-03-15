//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

#import <Foundation/Foundation.h>
#import"DSRestParamDirection.h"
#import "DBXDataTypes.h"

/**
 * @brief Represents and incorporates all relevant information that describe a {@link DSRESTParameter}.
 */
@interface DSRESTParameterMetaData : NSObject {
	NSString* Name;
	DSRESTParamDirection Direction;
	DBXDataTypes DBXType;
	NSString* TypeName;
	

}
/**
 * Name of the paramter. Empty is a return parameter.
 *  
 */
@property (nonatomic,strong) NSString* Name;
/**
 * Parameter direction. Input, output or input/output parameter.
 */
@property (nonatomic) DSRESTParamDirection Direction;
/**
 * Type the parameter  holds. Accordin to DBXDataTypes
 */
@property (nonatomic) DBXDataTypes DBXType;
/**
 * Data type string name.
 */
@property (nonatomic,strong) NSString* TypeName;

 /**
	 * initializes an instance of DSRESTParameterMetaData
	 * 
	 * @param aname the name of the parameter.
	 * @param adirection a value of type integer that represents the parameter direction, respecting the conventions of the {@link DSRESTParamDirection}.  
	 * @param aDBXType an integer value that represents the DBXType of this parameter , respecting the conventions of the {@link DBXValueType}.
	 * @param aTypeName a string that contains the TypeName of this parameter.
	 */
-(id) initWithMetadata:(NSString*)aname withDirection:(DSRESTParamDirection)adirection 
		  withDBXType: (DBXDataTypes) aDBXType withTypeName:(NSString *) aTypeName;
/**
	 * Create an instance of DSRESTParameterMetaData
	 * 
	 * @param aname the name of the parameter.
	 * @param adirection a value of type integer that represents the parameter direction, respecting the conventions of the {@link DSRESTParamDirection}.  
	 * @param aDBXType an integer value that represents the DBXType of this parameter , respecting the conventions of the {@link DBXValueType}.
	 * @param aTypeName a string that contains the TypeName of this parameter.
	 */
+(id) parameterWithName: (NSString *) aname withDirection:(DSRESTParamDirection)adirection 
			withDBXType: (DBXDataTypes) aDBXType withTypeName:(NSString *) aTypeName;
@end
