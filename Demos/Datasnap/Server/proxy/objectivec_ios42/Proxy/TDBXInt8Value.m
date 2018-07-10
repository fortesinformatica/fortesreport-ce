//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

#import "TDBXInt8Value.h"


@implementation TDBXInt8Value
-(id)init{
	self = [super init];
	if (self) {
		[self setDBXType:Int8Type];
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
-(void) SetAsInt8:(int)value{
	ValueNull = NO;
	DBXInternalValue = value;
	
}
-(int) GetAsInt8{
	return DBXInternalValue;
}

@end
