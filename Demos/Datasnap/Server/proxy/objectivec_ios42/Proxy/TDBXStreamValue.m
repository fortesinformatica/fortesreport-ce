//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

#import "TDBXStreamValue.h"


@implementation TDBXStreamValue
-(id) init {
	self = [super init];
	if (self) {
		[self setDBXType:BinaryBlobType];
		ValueNull =  NO;
	}
	return self;
}
-(void) SetNull {
	ValueNull = YES;
	streamValue = nil;
}

-(bool) isNull {
	return ValueNull;
}
-(NSData*) GetAsStream{
	[self checkCurrentDBXType:BinaryBlobType];
	return streamValue;
}
-(void) SetAsStream:(NSData *) value{
	[streamValue release];
	streamValue = [value retain];
	[self setDBXType:BinaryBlobType];
}




@end
