//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************


#import "TJSONObject.h"
#import "TJSONNull.h"
#import "TJSONString.h"
#import "TJSONNumber.h"
#import "TJSONArray.h"
#import "TJSONTrue.h"
#import "TJSONFalse.h"
#import "SBJSON.h"


#import "DBXJsonTools.h"


@implementation TJSONObject
- (NSDictionary * ) JSONObjectFromString:(NSString *)json_string{
	SBJsonParser * parser = [[SBJsonParser alloc] init];
	@try {
		return [parser objectWithString:json_string];
	}
	@finally {
		[parser release];
	}
	
	
}
-(NSString * )JSONObjectToString:(NSDictionary * )json{
	SBJsonWriter * parser = [[SBJsonWriter alloc] init];
	@try {
		return [parser stringWithObject:json];
	}
	@finally {
		[parser release];
	}
}



-(TJSONPairList *) buildElements:(NSDictionary * )o {
	@try {
		
		TJSONPairList* res =[[[TJSONPairList alloc] init] autorelease]  ;
		for (NSString * pname in o) {
			id obj = [o objectForKey:pname];
			if ([obj isKindOfClass:[NSNull class]]) {
				[res addPair: [TJSONPair JSONPairWithName:pname andValue:[[[TJSONNull alloc]init]autorelease]]];
			} else if ([obj isKindOfClass:[NSString class]]) {
				[res addPair: [TJSONPair JSONPairWithName:pname andValue:[[[TJSONString alloc]initWithString:(NSString *)obj]autorelease]]];
			} else if ([obj isKindOfClass:[NSArray class]]) {
				[res addPair: [TJSONPair JSONPairWithName:pname andValue:[[[TJSONArray alloc]initWithJSONArray:obj]autorelease]]];
			}else if ([obj isKindOfClass:[TJSONObject class]]) {
				[res addPair:[TJSONPair JSONPairWithName:pname andValue:obj]];
			}else if ([obj isKindOfClass:[NSDictionary class]]) {
				[res addPair: [TJSONPair JSONPairWithName:pname andValue:[[[TJSONObject alloc]initWithJSONObject:obj]autorelease]]];
			} else if ([obj isKindOfClass:[NSNumber class]]) {
				if (strcmp([obj objCType], @encode(BOOL)) == 0){
					if ([obj boolValue]) {
						[res addPair: [TJSONPair JSONPairWithName:pname andValue:[[[TJSONTrue alloc]init]autorelease]]];}
					else{
						[res addPair: [TJSONPair JSONPairWithName:pname andValue:[[[TJSONFalse alloc]init]autorelease]]];}
				}else{
					[res addPair: [TJSONPair JSONPairWithName:pname andValue:[[[TJSONNumber alloc]initWithNumber:obj]autorelease]]];}
			}			
			
		}
		return res;		
	} @catch (NSException *ex) {
		return nil;
		@throw ex;
	}
	
}

-(id) init{
	self = [super init];
	if (self) {
	  elements = [[TJSONPairList alloc]init];
	}
	
	return self;
}
-(id) initWithJSONString:(NSString *) stringJson{
	
	self = [self init];
	if (self) {
		[elements release];
		elements  = [[self buildElements: [self JSONObjectFromString:stringJson]] retain];

	}
	return self;
}
-(id) initWithJSONObject:(NSDictionary *) json{
	self = [self init];
	if (self) {
		[elements release];
		elements  = [[self buildElements: json ] retain];

	}
	return self;
	
}
-(NSString *) asJSONString{
	return [self JSONObjectToString:[self asJSONObject]];  
}

-(NSString *) toString {
	return [self asJSONString];
}
-(NSDictionary *)asJSONObject {
	return [elements asJSONObject];
}
-(NSArray *) asJSONArray {
	return nil;
}

-(id) getInternalObject{
	return [elements asJSONObject ];
}
-(void) dealloc {
	[elements release];
	[super dealloc];
}
-(JSONValueType) getJSONValueType{
	return JSONObject;
}
-(id) addPairs:(TJSONPair *)value{
	[elements addPair:value];
	return self;
}
-(id) addPairs: (NSString*) name withJSONValue:(TJSONValue *) value{
	[elements addPair:[TJSONPair JSONPairWithName:name andValue:value]];
	return self;
}
-(id) addPairs:(NSString *) name withString:(NSString *)value{
	[elements addPair:[TJSONPair JSONPairWithName:name andValue:
					   [TJSONString JSONStringWithString:value]] ] ;
	
	return self;
	
}

-(id) addPairs:(NSString *)name withInt:(int)value{
	
	[elements addPair:[TJSONPair JSONPairWithName:name andValue:
					   [TJSONNumber jsonNumberWithNumber:[NSNumber numberWithInt:value]]]];
	
	return self;
}
-(id) addPairs:(NSString *)name withLong:(long)value{
	[elements addPair:[TJSONPair JSONPairWithName:name andValue:
					   [TJSONNumber jsonNumberWithNumber:[NSNumber numberWithLong:value]]]];
	
	return self;
}
-(id) addPairs:(NSString *)name withDouble:(double)value{
	[elements addPair:[TJSONPair JSONPairWithName:name andValue:
					   [TJSONNumber jsonNumberWithNumber:[NSNumber numberWithDouble:value]]]];
	
	return self;
}

-(id) addPairs:(NSString *)name withBool:(BOOL)value{
if (value) {
	[elements addPair:[TJSONPair JSONPairWithName:name andValue:[[[TJSONTrue alloc]init]autorelease]]];
}else{
	[elements addPair:[TJSONPair JSONPairWithName:name andValue:[[[TJSONFalse alloc]init]autorelease]]];
}
	return self;
}


-(NSString *) getStringForKey:(NSString *)name{
	TJSONPair* p =  [elements PairByName:name];
   	return [(TJSONString *)p.value getValue];
}
-(TJSONArray *) getJSONArrayForKey:(NSString *)name{
	TJSONPair* p =  [elements PairByName:name];
	return (TJSONArray *) p.value ;
}
-(TJSONObject *) getJSONObjectForKey:(NSString *)name{
	TJSONPair* p =  [elements PairByName:name];
	return (TJSONObject *) p.value ;
}
-(int) getIntForKey:(NSString * ) name{
	TJSONPair* p =  [elements PairByName:name];
	return [[p.value getInternalObject]intValue] ;
}
-(long) getLongForKey:(NSString * ) name{
	TJSONPair* p =  [elements PairByName:name];
	return [[p.value getInternalObject]longValue] ;
}
-(double) getDoubleForKey:(NSString * ) name{
	TJSONPair* p =  [elements PairByName:name];
	return [[p.value getInternalObject]doubleValue] ;
}
-(BOOL) getBoolForKey:(NSString * ) name{
	TJSONPair* p =  [elements PairByName:name];
	if ([p.value isKindOfClass:[TJSONTrue class]]) {
        return YES;
	}
	return NO;
}
-(TJSONPair *)getPairForKey:(NSString *)name{
  return[elements PairByName:name];
}
-(BOOL) hasKey:(NSString *)name{
	return !([elements PairByName:name] == nil);
}
-(int) count{
	return [elements count];
}

@end
@implementation TJSONObject(jsonobjectCreations)
+(id)parse:(NSString *) value{
	return [[[TJSONObject alloc]initWithJSONString:value]autorelease];
}
@end
