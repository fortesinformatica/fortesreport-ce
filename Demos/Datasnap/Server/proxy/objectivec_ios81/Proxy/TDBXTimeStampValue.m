//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

#import "TDBXTimeStampValue.h"


@implementation TDBXTimeStampValue
-(id)init{
	self = [super init];
	if (self) {
		[self setDBXType:TimeStampType];
	}
	return self;
}
-(void) SetNull {
	ValueNull = YES;
	DBXInternalValue = nil;
}

-(bool) isNull {
	return ValueNull;
}

-(void) SetAsTimeStamp:(NSDate*) value{
	ValueNull = NO;
	DBXInternalValue = value ;
}

-(NSDate*) GetAsTimeStamp {
	return DBXInternalValue;
}

@end
