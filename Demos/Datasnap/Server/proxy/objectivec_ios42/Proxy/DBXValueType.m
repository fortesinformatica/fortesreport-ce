//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

#import "DBXValueType.h"
#import "DBException.h"


@implementation DBXValueType

@synthesize name;
@synthesize caption;
@synthesize ordinal;
@synthesize subType;
@synthesize size;
@synthesize precision;
@synthesize scale;
@synthesize childPosition;
@synthesize nullable;
@synthesize parameterDirection;
@synthesize hidden;
@synthesize valueParameter;
@synthesize literal;

-(void) setDataType:(int) dataType{
	@throw [DBXException exceptionWithName:@"AbstractMethdo" 
									reason:@"Must be overridden in the descendant classes" userInfo:nil]; 
	
}
-(int) DataType;{
	@throw [DBXException exceptionWithName:@"AbstractMethdo" 
									reason:@"Must be overridden in the descendant classes" userInfo:nil]; 
}
-(void) dealloc {
	[name release];
	[caption release];
	[super dealloc];
}

@end