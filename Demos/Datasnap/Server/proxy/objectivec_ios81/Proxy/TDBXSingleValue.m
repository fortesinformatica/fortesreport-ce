//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

#import "TDBXSingleValue.h"


@implementation TDBXSingleValue
-(id)init{
	self = [super init];
	if (self) {
		[self setDBXType:SingleType];
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
-(void) SetAsSingle:(float)value{
	DBXInternalValue = value;
}
-(float) GetAsSingle{
	return DBXInternalValue;
}

@end
