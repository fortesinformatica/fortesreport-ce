//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

#import "TDBXUInt16Value.h"


@implementation TDBXUInt16Value
-(id)init{
	self = [super init];
	if (self) {
		[self setDBXType:UInt16Type];
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
-(void) SetAsUInt16:(unsigned int)value{
	DBXInternalValue = value;
}
-(unsigned int) GetAsUInt16{
	return DBXInternalValue;
}
@end
