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
-(void) dealloc {
	[value release];
	[super dealloc];
}
-(id) initWithNumber:(NSNumber*)aValue{
	self =[self init];
	if (self) {
		[value release];
		value = [aValue retain];
	}
	return self;
	

}
-(id) initWithString:(NSString *) aValue {
	self =[self init];
	if (self) {
		[value release];		
		NSNumberFormatter * f = [[NSNumberFormatter alloc] init];
		[f setNumberStyle:NSNumberFormatterDecimalStyle];
		value = [[f numberFromString:aValue]retain];
		[f release];
		
	}
	return self;
}

-(id) initWithDouble:(double) aValue {
	self =[self init];
	if (self) {
		[value release];
		value = [[NSNumber numberWithDouble:aValue]retain];
	}
	return self;
}

-(id) initWithLong:(long) aValue {
	self =[self init];
	if (self) {
		[value release];
		value = [[NSNumber numberWithLong:aValue] retain];
	}
	return self;
}
-(id) initWithInt:(int) aValue {
	self =[self init];
	if (self) {
		[value release];
		value = [[NSNumber numberWithInt:aValue] retain];
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
	return [[[TJSONNumber alloc]initWithNumber:value]autorelease];
}
@end
