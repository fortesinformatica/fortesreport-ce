//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

#import "TJSONValueList.h"


@implementation TJSONValueList
-(id) init{
	self = [super init];
	if (self) {
		internalArray = [NSMutableArray arrayWithCapacity:0];
	}
	return self;
}
-(int) addValue:(TJSONValue* )value{
	[internalArray addObject:value ];
	return [internalArray count]-1;
}
-(void) removeValueByIndex:(int) value{
	[internalArray removeObjectAtIndex:value];
	
}


-(TJSONValue *) valueByIndex:(int) value{
	return (TJSONValue *)[internalArray objectAtIndex:value];
	
}

-(int)count{
	return [internalArray count];
}


@end
