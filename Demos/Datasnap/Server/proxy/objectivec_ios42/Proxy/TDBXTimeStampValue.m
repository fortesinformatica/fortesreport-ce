//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

#import "TDBXTimeStampValue.h"


@implementation TDBXTimeStampValue
-(id)init{
	self = [super init];
	if (self) {
		[self setDBXType:TimeStampType];
	}
	return self;
}
-(void) dealloc{
	[DBXInternalValue release];
	[super dealloc];
}
-(void) SetNull {
	ValueNull = YES;
	[DBXInternalValue release];
	DBXInternalValue = nil;
}

-(bool) isNull {
	return ValueNull;
}

-(void) SetAsTimeStamp:(NSDate*) value{
	ValueNull = NO;
	[DBXInternalValue release];
	DBXInternalValue = [value retain];
}

-(NSDate*) GetAsTimeStamp {
	return DBXInternalValue;
}

@end
