//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

#import "TDBXTimeValue.h"


@implementation TDBXTimeValue
-(id)init{
	self = [super init];
	if (self) {
		[self setDBXType:TimeType];
	}
	return self;
}
-(void) SetNull {
	ValueNull = YES;
	DBXInternalValue = 0;
}
-(bool) isNull {
	return ValueNull;
}
-(void) SetAsTDBXTime:(long)value{
	DBXInternalValue = value;
}
-(long) GetAsTDBXTime{
	return DBXInternalValue;
}


@end
