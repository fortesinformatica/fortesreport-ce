//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

#import "TJSONArray.h"
#import "DBXJsonTools.h"
#import "TJSONNull.h"
#import "TJSONString.h"
#import "TJSONNumber.h"
#import "TJSONObject.h"
#import "TJSONTrue.h"
#import "TJSONFalse.h"
#import "SBJSON.h"

@implementation TJSONArray

- (NSArray * ) JSONArrayFromString:(NSString *)json_string{
	SBJsonParser * parser = [[SBJsonParser alloc] init];
	@try {
		return [parser objectWithString:json_string] ;
	}
		@finally {
			[parser release];
	}
	

}
-(NSString * )JSONArrayToString:(NSArray * )json{
	SBJsonWriter * parser = [[SBJsonWriter alloc] init];
	@try {
		return [parser stringWithObject:json] ;
	}
	@finally {
		[parser release];
	}
}
-(id) init{
	self = [super init];
	if (self){
		elements = [[TJSONValueList alloc]init];
	}
	return self;
	

}
-(void) dealloc {
	[elements release];
	[super dealloc];
}

-(id) getInternalObject{
	return [self asJSONArray];
}
-(TJSONValueList *) buildElements:(NSArray *)arr{
	@try {
		TJSONValueList * res = [[[TJSONValueList alloc]init] autorelease] ;
		for (int i = 0; i < [arr count]; i++) {
			id obj = [arr objectAtIndex:i];				
			if ([obj isKindOfClass:[NSNull class]]) {
				[res addValue:[[[TJSONNull alloc]init]autorelease] ];
			} else if ([obj isKindOfClass:[NSString class]]) {
				[res addValue:[[[TJSONString alloc]initWithString:(NSString *)obj]autorelease] ];
			} else if ([obj isKindOfClass:[NSArray class]]) {
				[res addValue:[[[TJSONArray alloc]initWithJSONArray:obj]autorelease ]];
			} else if ([obj isKindOfClass:[NSDictionary class]]) {
				[res addValue:[[[TJSONObject alloc]initWithJSONObject:obj]autorelease] ];
			}else if ([obj isKindOfClass:[TJSONObject class]]) {
				[res addValue:obj ];
			} else if ([obj isKindOfClass:[TJSONArray class]]) {
				[res addValue:obj ];
			}else if ([obj isKindOfClass:[NSNumber class]]) {
							if (strcmp([obj objCType], @encode(BOOL)) == 0){
								if ([obj boolValue]) {
									[res addValue:[[[TJSONTrue alloc]init ]autorelease]];}
								else{
									[res addValue:[[[TJSONFalse alloc]init ]autorelease]]; }
							}else{
									[res addValue:[[[TJSONNumber alloc]initWithNumber:(NSNumber*)obj] autorelease]];}
					}
		}
		
		return res;
	} @catch (NSException *ex) {
		@throw ex;
	}
	
	
}

-(id) initWithJSONValues:(TJSONValueList *) value{
	self = [self init];
	if (self){
		[elements release];
		elements = [value retain ] ;   
	}
	return self;
	

}
- (id) initWithJSONString: (NSString*) jsonString{
	self = [self init];
	if (self){
		[elements release];
		elements = [[self buildElements:[self JSONArrayFromString:jsonString]]retain ]    ;   

	   
	} 

	return self;
}
- (id) initWithJSONArray: (NSArray *) json{
	self = [self init];
	if (self){
		[elements release];
		elements = [[self buildElements:json] retain] ;   
	}
	return self;
}
-(NSString *) asJSONString{
	return [self JSONArrayToString:[self asJSONArray]];
}
-(id) asJSONArray {
	NSMutableArray * arr = [NSMutableArray arrayWithCapacity:0];
	
	for (int i = 0;i< [elements count];i++){
		[arr addObject: [[elements valueByIndex:i]getInternalObject]];};
	return arr;
	
	
	
}


-(NSString *) toString{
	return [self asJSONString];
}
-(id) addValue:(TJSONValue*) value{
    [elements addValue :value];
	return self;
}
-(id) removeValueByIndex:(int) index{
	[elements removeValueByIndex:index];
	return self;
}
-(int) count{
	return [elements count];
}
-(TJSONValue*) valueByIndex:(int)index{
	return [elements valueByIndex:index];
}
-(NSString *) stringByIndex:(int) index {
	TJSONValue * p = [self valueByIndex:index];
	if (p) {
		return [(TJSONString *) p getValue ];
	}
	return nil;
}
-(double) doubleByIndex:(int) index {
	TJSONValue * p = [self valueByIndex:index];
	if (p) {
		return [[(TJSONNumber *) p getInternalObject ] doubleValue];
	}
	return 0;
}
-(TJSONObject *) JSONObjectByIndex:(int) index {
	TJSONValue * p = [self valueByIndex:index];
	if (p) {
		return (TJSONObject *) p;
	}
	return nil;
}
-(int) intByIndex:(int) index {
	TJSONValue * p = [self valueByIndex:index];
	if (p) {
		return [[(TJSONNumber *) p getInternalObject ] intValue];
	}
	return 0;
}
-(long) longByIndex:(int) index {
	TJSONValue * p = [self valueByIndex:index];
	if (p) {
		return [[(TJSONNumber *) p getInternalObject ] longValue];
	}
	return 0;
}
-(BOOL) boolByIndex:(int) index {
	TJSONValue * p = [self valueByIndex:index];
	if (p) {
		if ([p isKindOfClass:[TJSONTrue class]])
		return YES;
	}
	return NO;
}
-(TJSONString *) JSONStringByIndex:(int) index {
	TJSONValue * p = [self valueByIndex:index];
	if (p) {
		return (TJSONString *) p  ;
	}
	return nil;
}
-(JSONValueType) getJSONValueType{
	return JSONArray;
}


@end
