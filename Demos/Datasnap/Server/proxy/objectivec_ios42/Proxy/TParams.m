//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

#import "TParams.h"
#import "DBException.h"
#import "DBXJsonTools.h"
#import "DBXParameter.h"




@implementation TParams
-(id) init{
	self = [super init];
	if (self) {
		params = [[NSMutableArray alloc]initWithCapacity:0];
	}
	return self;
} 

-(DBXParameter*) findParamByName:(NSString*) value {
	for (DBXParameter* p in params)
		if ([p.name isEqualToString:value])
			return p;
	return nil;
}

-(DBXParameter*) getParamByName:(NSString *)value{
	DBXParameter * p;
	if ((p = [self findParamByName:value])){
		return p;}
	@throw [DBXException
			exceptionWithName:@"ParamNotFound"
			reason:@"Cannot find param in list"
			userInfo:nil];
 	
	
}


-(TParams*) addParameter:(DBXParameter*) parameter{
	
	if (![self findParamByName:parameter.name]){
		[params addObject: parameter];}
	else
	{
		@throw [DBXException
				exceptionWithName:@"ParamNotUnique"
				reason:@"Parameter name must be unique"
				userInfo:nil];
	}
	return self;
}
-(DBXParameter *) getParamByIndex: (int) index{
	return  [params objectAtIndex:index];
}
-(int) size {
	return [params count];
}
+ (TParams*) createParametersWithMetadata:(TJSONArray *) paramsMetadata {
	TParams* o = [[[TParams alloc]init]autorelease ];
	
	NSArray * paramMetadata;
	NSArray *  arrParamsMetadata = [[paramsMetadata asJSONArray]retain];	
	@try {
		for (int i = 0; i < [arrParamsMetadata count]; i++) {
			paramMetadata = [arrParamsMetadata objectAtIndex:i];
			DBXParameter *  parameter = [[[DBXParameter alloc ]init] autorelease]; 
			TJSONArray * ajr = [[[TJSONArray alloc] initWithJSONArray: paramMetadata] autorelease];
			[DBXJsonTools JSONToValueType: ajr   withValueType: &parameter];
			[o addParameter:parameter];
		}
	} @finally  {
		[arrParamsMetadata release];
	}
	return o;
}
-(id) asJSONObject {
	return [DBXJsonTools DBXParametersToJSONObject:self];
}
+(bool)loadParametersValues:(TParams **) params withJSON:(TJSONObject *) value andOffSet: (int) offset{
	NSArray * parValue;
	if ([*params size] <= 0) {
		return NO;
	}
	for (int i = 0; i < [*params size]; i++) {
		DBXParameter * par = [[*params  getParamByIndex:i]retain];
		TJSONArray * jarray =[value getJSONArrayForKey:[par name]];
		
		parValue = [jarray asJSONArray];
		
		@try{
		if ([parValue count] < offset + 1){
			return NO;}
		DBXWritableValue * val = [par getValue];
			[DBXJsonTools jsonToDBX:[parValue objectAtIndex:offset]
				   withDbxValue:&val withDBXTypeName:@""]	;
		}@finally {
			[par release];
		}	
		
	}
	return YES;
}

+(bool)loadParametersValues:(TParams **) params withJSON:(TJSONObject *) value{
	return [TParams loadParametersValues:params withJSON:value andOffSet:0];

}
/**
 * 
 * @param value
 *            a compatible JSONObject that describe a DBXParameters
 * @return a fully configured DBXParameters
 * @throws JSONException
 * @throws DBXException
 */
+(id) paramsWithJSON:(TJSONObject *) value {
	TParams* params = [TParams createParametersWithMetadata:[value getJSONArrayForKey:@"table"]];
	[TParams loadParametersValues:&params withJSON: value];
	return params ;
}
-(void) dealloc{
	[params release];
	[super dealloc];
}


@end
