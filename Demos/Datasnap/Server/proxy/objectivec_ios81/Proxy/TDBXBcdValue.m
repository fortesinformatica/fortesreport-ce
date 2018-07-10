//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

#import "TDBXBcdValue.h"


@implementation TDBXBcdValue
-(id)init {
	self =  [super init];
	if (self) {
		[self setDBXType:BcdType];
	}
	return self;
}
-(bool) isNull{
	return valueNull;
}
-(void) SetNull{
	valueNull = YES;
	bcdValue = 0;
}
-(void) SetAsBcd:(double)value{
	bcdValue = value;
}

-(double) GetAsBcd{
	return bcdValue;
}

@end
