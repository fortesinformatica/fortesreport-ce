//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

#import "TJSONPairList.h"
#import "DBXJsonTools.h"
#import "TJSONString.h"
#import "TJSONNumber.h"

@implementation TJSONPairList

-(id) init{
	self = [super init];
	if (self) {
		internalArray = [NSMutableArray arrayWithCapacity:0];
	}
	return self;
}
-(int) addPair:(TJSONPair *)value{
	[internalArray addObject:value ];
	return [internalArray count]-1;
}
-(void) removePairByIndex:(int) value{
	[internalArray removeObjectAtIndex:value];

}


-(TJSONPair *) PairByIndex:(int) value{
	return (TJSONPair *)[internalArray objectAtIndex:value];

}

-(TJSONPair *) PairByName:(NSString *) value{
	for ( TJSONPair * obj  in internalArray) {

		if ([value isEqualToString:obj.name]) {
			return obj; 
			
		}
	}
	return nil;
	

}
-(int) findPairByName:(NSString *) value{
	int i = -1;
	for ( TJSONPair * obj  in internalArray) {
		i++;
		if ([value isEqualToString:obj.name]) {
			return i; 
			
		}
	}
	return i;
}

-(void) removePairByName:(NSString*) value{
	[self removePairByIndex:[self findPairByName:value]];
}
-(int)count{
	return [internalArray count];
}
-(id) asJSONObject {

	NSMutableDictionary * d = [NSMutableDictionary dictionaryWithCapacity:0] ;
	
	for (int i= 0;i< [self count];i++) {
	 TJSONPair * pair =	[self PairByIndex:i];
		JSONValueType jtype =[pair.value getJSONValueType];
		switch (jtype) {
			case JSONObject: {
				[d setObject:[pair.value asJSONObject] forKey:  pair.name];
				break;
			}
			case JSONArray: {
				[d setObject:[pair.value asJSONArray] forKey:  pair.name];
				break;
			}
			case JSONString: {
				[d setObject:[(TJSONString *) pair.value getValue] forKey:  pair.name];
				break;
			}
			case JSONNumber: {
				[d setObject:[(TJSONNumber *) pair.value getInternalObject] forKey:  pair.name];
				break;
			}
			case JSONTrue: {
				[d setObject:[NSNumber numberWithBool:YES] forKey:pair.name];
				break;
			}
			case JSONFalse: {
				[d setObject:[NSNumber numberWithBool:FALSE] forKey:pair.name];
				break;
			}
			case JSONNull: {
				[d setObject:[NSNull null] forKey:pair.name];
				break;
			}
		}	
	
	}
	return  d;
}

@end
