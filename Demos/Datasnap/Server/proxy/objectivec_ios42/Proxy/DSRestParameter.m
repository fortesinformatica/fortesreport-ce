//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

#import "DSRestParameter.h"


@implementation DSRestParameter

@synthesize name;
@synthesize direction;
@synthesize typeName;
@synthesize DBXType;

- (id) init{
	self = [super init]; 
	if (self) {
	  value = [[[DBXWritableValue alloc]init] autorelease];	
	  [value retain];
   }
	return self;
}
- (id) initWithParameter:(NSString *) aname  withDirection: (DSRESTParamDirection) adirection 
			withTypeName: (NSString *) aTypeName {
	self = [self init];
	if (self) {
		name = [NSString stringWithString:aname];
		direction = adirection;
		typeName = [NSString stringWithString:aTypeName];
		
	}
	return self;
	
}

-(void) dealloc {

	[value release];
	[super dealloc];
	
}
-(DBXValue*) getValue {
	return value  ;
}


@end
