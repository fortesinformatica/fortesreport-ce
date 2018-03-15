//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

#import "TDBXInt32Value.h"


@implementation TDBXInt32Value
-(id)init{
	self = [super init];
	if (self) {
		[self setDBXType:Int32Type];
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
-(void) SetAsInt32:(long)value{
	ValueNull = NO;
	DBXInternalValue = value;
}
-(long) GetAsInt32{
	return DBXInternalValue;
}

@end
