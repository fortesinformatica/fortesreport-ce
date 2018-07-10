//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

#import "DSRESTParameterMetaData.h"


@implementation DSRESTParameterMetaData

@synthesize Name;
@synthesize Direction;
@synthesize DBXType;
@synthesize TypeName;


+(id) parameterWithName: (NSString *) aname withDirection:(DSRESTParamDirection)adirection 
			withDBXType: (DBXDataTypes) aDBXType withTypeName:(NSString *) aTypeName{

	return [[DSRESTParameterMetaData alloc] initWithMetadata:aname withDirection:adirection
												 withDBXType:aDBXType withTypeName:aTypeName];
}
-(id) initWithMetadata:(NSString*)aname withDirection:(DSRESTParamDirection)adirection 
		  withDBXType: (DBXDataTypes) aDBXType withTypeName:(NSString *)aTypeName{
	self = [super init];
	if (self) {
		Name = aname;
		Direction = adirection;
		DBXType = aDBXType; 
		TypeName = aTypeName;
	}
	return self;

}





@end
