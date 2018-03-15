//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

#import "TDBXInt16Value.h"


@implementation TDBXInt16Value
-(id)init{
	self = [super init];
	if (self) {
		[self setDBXType:Int16Type];
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
-(void) SetAsInt16:(int)value{
	DBXInternalValue = value;
}
-(int) GetAsInt16{
	return DBXInternalValue;
}
@end
