//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

#import "TJSONNumber.h"


@implementation TJSONNumber

-(id) init{
	self = [super init];
	if (self) {
	//	value = [[NSNumber alloc ]init ];
		value = nil;
	}
	return self;
}
-(id) initWithNumber:(NSNumber*)aValue{
	self =[self init];
	if (self) {
		value = aValue;
	}
	return self;
	

}
-(id) initWithString:(NSString *) aValue {
	self =[self init];
	if (self) {
		NSNumberFormatter * f = [[NSNumberFormatter alloc] init];
		[f setNumberStyle:NSNumberFormatterDecimalStyle];
		value = [f numberFromString:aValue];
		
	}
	return self;
}

-(id) initWithDouble:(double) aValue {
	self =[self init];
	if (self) {
		value = [NSNumber numberWithDouble:aValue];
	}
	return self;
}

-(id) initWithLong:(long) aValue {
	self =[self init];
	if (self) {
		value = [NSNumber numberWithLong:aValue];
	}
	return self;
}
-(id) initWithInt:(int) aValue {
	self =[self init];
	if (self) {
		value = [NSNumber numberWithInt:aValue];
	}
	return self;
}

-(id) getInternalObject{
	return value;
}

-(NSString *) toString{
	if (value) {
		return [value stringValue];
	}

    return [self nullString];	
}
-(JSONValueType) getJSONValueType{
	return JSONNumber;
}
-(NSNumber *) getValue{
	return value;
}

@end
@implementation TJSONNumber(jsonnumberCreation)
+ (id) jsonNumberWithNumber:(NSNumber *) value{
	return [[TJSONNumber alloc]initWithNumber:value];
}
@end
