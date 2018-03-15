//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

#import "TDBXDoubleValue.h"


@implementation TDBXDoubleValue
-(id)init{
	self = [super init];
	if (self) {
		[self setDBXType:DoubleType];
	}
	return self;
}
-(void) dealloc{
	[super dealloc];
}
-(void) SetNull {
	ValueNull = YES;
	DBXInternalValue = 0;
}

-(bool) isNull {
	return ValueNull;
}

-(void) SetAsDouble:(double) value{
	ValueNull = NO;
	DBXInternalValue = value;
}

-(double) GetAsDouble {
	return DBXInternalValue;
}

@end
