//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

#import "TDBXUInt8Value.h"


@implementation TDBXUInt8Value
-(id)init{
	self = [super init];
	if (self) {
		[self setDBXType:UInt8Type];
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
-(void) SetAsUInt8:(unsigned int)value{
	ValueNull = NO;
	DBXInternalValue = value;
	
}
-(unsigned int) GetAsUInt8{
	return DBXInternalValue;
}


@end
