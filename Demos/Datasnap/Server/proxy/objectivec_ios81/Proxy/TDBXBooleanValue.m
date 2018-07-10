//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

#import "TDBXBooleanValue.h"


@implementation TDBXBooleanValue
-(id)init{
	self = [super init];
	if (self) {
		[self setDBXType:BooleanType];
	}
	return self;
}
-(void) SetNull {
	ValueNull = YES;
	DBXBoolValue = NO;
}

-(bool) isNull {
	return ValueNull;
}

-(void) SetAsBoolean:(bool) value{
	ValueNull = NO;
	DBXBoolValue = value;
}

-(bool) GetAsBoolean {
	return DBXBoolValue;
}

@end
