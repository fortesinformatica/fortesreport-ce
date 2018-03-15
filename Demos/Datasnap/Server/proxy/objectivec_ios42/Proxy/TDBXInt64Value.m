//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

#import "TDBXInt64Value.h"


@implementation TDBXInt64Value
-(id)init{
	self = [super init];
	if (self) {
		[self setDBXType:Int64Type];
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
-(void) SetAsInt64:(long long)value{
	DBXInternalValue = value;
}
-(long long) GetAsInt64{
	return DBXInternalValue;
}


@end
