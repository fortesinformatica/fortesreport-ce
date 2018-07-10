//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

#import "TDataSet.h"


@implementation TDataSet

@end

@implementation TDataSet (TDatasetCreation)

+(id) DataSetWithJSON:(TJSONObject *) value {
	TParams * params = [TParams createParametersWithMetadata:[value getJSONArrayForKey:@"table" ]] ;
	
	return [ TDBXReader DBXReaderWithParams:params andJSONObject: value];
}
+(id) DataSetWithParams: (TParams * )params andJSONObject: (TJSONObject *)json{
	return [[TDataSet alloc]initWithParams:params andJSONObject:json];
}
@end
