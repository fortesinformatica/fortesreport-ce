//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

#import "TDBXReader.h"
#import "DBXJsonTools.h"


@implementation TDBXReader

@synthesize columns;
@synthesize internalDataStore;


-(id) initWithParams: (TParams * )params andJSONObject: (TJSONObject *)json{
	self = [self init];
	if (self) {
		self.columns = params;
		self.internalDataStore = json;
	}
	return self;
}

-(id) init{
	self = [super init];
	if (self) {
		currentPosition = -1;
	}
	return self;	
}
-(DBXWritableValue *) getValueByIndex: (int) index{
	return [[columns getParamByIndex:index]getValue ];
}
-(DBXWritableValue *) getValueByName: (NSString *) name{
	return [[columns getParamByName:name] getValue];

}
-(bool) next{
	currentPosition++;
    TParams *tempcolumns = nil;
	@try {
		return [TParams loadParametersValues:&tempcolumns withJSON:internalDataStore andOffSet:currentPosition];
	} @catch (NSException* ex) {
			@throw ex;
			}
    columns = tempcolumns;
}
-(TJSONObject *) asJSONObject {
	return [DBXJsonTools  DBXReaderToJSONObject:self];
}
-(DBXWritableValue*) getValue:(int) position {
	return [[[self columns] getParamByIndex:position] getValue];
}
-(void) reset{
	currentPosition = -1;		
}
@end


@implementation TDBXReader (TDBXReaderCreation)
+(id) DBXReaderWithJSON:(TJSONObject *) value {
	TParams * params = [TParams createParametersWithMetadata:[value getJSONArrayForKey:@"table" ]]  ;
	
	return [ TDBXReader DBXReaderWithParams:params andJSONObject: value];
}
+(id) DBXReaderWithParams: (TParams * )params andJSONObject: (TJSONObject *)json{
	return [[TDBXReader alloc]initWithParams:params andJSONObject:json];
}


@end
