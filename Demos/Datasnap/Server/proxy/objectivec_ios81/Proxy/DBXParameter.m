//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

#import "DBXParameter.h"


@implementation DBXParameter
-(id) init {
	self = [super init];
	if (self) {
		value = [[DBXWritableValue alloc]init];
		[value clear];
	}
	return self;
}
-(DBXWritableValue*) getValue{
	return value;
} 
-(void) setDataType:(int) dataType{
	[value setDBXType: dataType];
}
-(int) getDataType{
	return [value DBXType];
}
-(id) toJson{

	NSArray * a =	[NSArray arrayWithObjects: 
			name,
			[NSNumber numberWithInt:[self getDataType]],					 
			[NSNumber numberWithInt: self.ordinal],
			[NSNumber numberWithInt:self.subType],
            [NSNumber numberWithInt:self.scale],
			[NSNumber numberWithLong:self.size],
			[NSNumber numberWithLong:self.precision],
			[NSNumber numberWithInt:self.childPosition],
			[NSNumber numberWithBool:self.nullable],
			[NSNumber numberWithBool:self.hidden],
			[NSNumber numberWithInt:self.parameterDirection],
			[NSNumber numberWithBool:self.valueParameter],
			[NSNumber numberWithBool:self.literal],
			 nil]; 
	return a ;
}




@end
