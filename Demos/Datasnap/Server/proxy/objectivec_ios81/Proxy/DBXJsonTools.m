//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

#import "DBXJsonTools.h"
#import "DBXDefaultFormatter.h"
#import "TStream.h"
#import "TJSONValue.h"
#import "TJSONNull.h"

#import "TJSONTrue.h"
#import "TJSONFalse.h"
#import "TDataSet.h"








@implementation DBXJsonTools

+(TJSONValue *) JSONToJSONValue:(id) o{
	if ([o isKindOfClass: [NSNull class]]) // TJSONNull
		return [[TJSONNull alloc] init];
	if ([o isKindOfClass: [NSDictionary class]]){ // TJSONObject
		return [[ TJSONObject alloc] initWithJSONObject: o];}
	if ([o isKindOfClass: [NSArray class]]){ // TJSONArray
		return [[ TJSONArray alloc] initWithJSONArray: o] ;}
	if ([o isKindOfClass:[NSNumber class]]){
		if ([o boolValue]){
	 		return [[TJSONTrue alloc]init];		}
		else {
			return [[TJSONFalse alloc]init];	
		}
	}

  else{
	@throw [DBXException
			exceptionWithName:@"InvalidJsonClass"
			reason:  [NSString stringWithFormat: @"%@ is not a valid JSONValue",[o class]]
			userInfo:nil];
	}

}

/**
 * 
 * @param param
 *            is an InputOutput, Output or ReturnValue (JSONValueType)
 * @param value
 *            is a generic object readed from the json result array
 * @return an Object that rapresent the result JSON or the JSONObject itself
 * @throws DBXException
 * @throws JSONException
 */

+(id) JSONToTableType:(id) value withDBXType: (NSString*) DBXTypeName{
	if ([DBXTypeName isEqualToString:@"TParams"]) {
		return [TParams paramsWithJSON: [[TJSONObject alloc] initWithJSONObject:value]];
	} else {
		if ([DBXTypeName isEqualToString:@"TDBXReader"]||
			[DBXTypeName isEqualToString:@"TDBXReaderValue"]) {
			return [TDBXReader  DBXReaderWithJSON: [[TJSONObject alloc] initWithJSONObject:value]];
		} else if ([DBXTypeName isEqualToString:@"TDataSet"]) {
			
			return [TDataSet DataSetWithJSON: [[TJSONObject alloc] initWithJSONObject:value]];
		}
	}
	@throw [DBXException
			exceptionWithName:@"NotValidTableType"
			reason:@"Json is not a valid TableType"
			userInfo:nil];
	
}
/**
 * 
 * @param results
 *            The results JSONArray
 * @param i
 *            current index of the result array (0,1,2,3). The last one is
 *            the return value
 * @param p
 *            current parameter
 * @throws DBXException
 */
+ (void) jsonToDBX:(id) obj withDbxValue: (DBXValue **) value
   withDBXTypeName: (NSString *) dbxTypeName{
	
	@try{
		
		if ([dbxTypeName hasPrefix:@"TDBX"] && 
			[dbxTypeName hasSuffix:@"Value"]&&
			//&& ((!obj)||[obj isKindOfClass:[NSNull class]])
			[obj isKindOfClass:[NSNull class]]
			)			{
			[[*value GetAsDBXValue]SetNull];
		} else {
			if (!(([obj isKindOfClass:[ NSNull class]]) && ([dbxTypeName isEqualToString:@""]))) {	
				switch ([*value DBXType]) {
					case Int8Type:
						if ([dbxTypeName isEqualToString:@"TDBXInt8Value"]) {
							[[*value GetAsDBXValue] SetAsInt8:[obj intValue]];	
						}else {
							[*value SetAsInt8:[obj intValue]];
						}
						break;
					case Int16Type:						
						if ([dbxTypeName isEqualToString:@"TDBXInt16Value"]) {
							[[*value GetAsDBXValue] SetAsInt16:[obj intValue]];
						}else {
							[*value SetAsInt16:[obj intValue]];}
						break;
					case Int32Type:
						if ([dbxTypeName isEqualToString:@"TDBXInt32Value"]) {
							[[*value GetAsDBXValue] SetAsInt32:[obj longValue]];
						}else {
							[*value SetAsInt32:[obj longValue]];
						}						
						break;
					case Int64Type:
						if ([dbxTypeName isEqualToString:@"TDBXInt64Value"]) {
							[[*value GetAsDBXValue] SetAsInt64:[obj  longLongValue]];
						}else{
							[*value SetAsInt64:[obj longLongValue]];}
						break;
					case BcdType:
						if ([dbxTypeName isEqualToString:@"TDBXBcdValue"]) {
							[[*value GetAsDBXValue] SetAsBcd:[obj  doubleValue]];
						}else{
								[*value SetAsBcd:[obj doubleValue]];}
						
					
						break;	
					case UInt8Type:
						if ([dbxTypeName isEqualToString:@"TDBXUInt8Value"]) {
							[[*value GetAsDBXValue] SetAsUInt8:[obj unsignedIntValue]];
						}else{
							[*value SetAsUInt8:[obj unsignedIntValue]];}
						break;
					case UInt16Type:
						if ([dbxTypeName isEqualToString:@"TDBXUInt16Value"]) {
							[[*value GetAsDBXValue] SetAsUInt16:[obj unsignedIntValue]];	
						}else {
							[*value SetAsUInt16:[obj unsignedIntValue]]; }
						break;					
					case UInt32Type:
							if ([dbxTypeName isEqualToString:@"TDBXUInt32Value"]) {
								[[*value GetAsDBXValue] SetAsUInt32:[obj unsignedLongValue]];	
							}else {
								[*value SetAsUInt32:[obj unsignedLongValue]]; }
							break;					
					case UInt64Type:
							if ([dbxTypeName isEqualToString:@"TDBXUInt64Value"]) {
								[[*value GetAsDBXValue] SetAsUInt64:[obj unsignedLongLongValue]];	
							}else {
								[*value SetAsUInt64:[obj unsignedLongLongValue]]; }
							break;							
						case DoubleType:
							if ([dbxTypeName isEqualToString:@"TDBXDoubleValue"]) {
								[[*value GetAsDBXValue] SetAsDouble:[obj doubleValue]];	
							}else {
								[*value SetAsDouble:[obj doubleValue]];}
							break;
						case SingleType:
							if ([dbxTypeName isEqualToString:@"TDBXSingleValue"]) {
								[[*value GetAsDBXValue] SetAsSingle:[obj floatValue]];	
							}else {
								[*value SetAsSingle:[obj floatValue]];}
							break;
						case CurrencyType:
						if ([dbxTypeName isEqualToString:@"TDBXCurrencyValue"]) {
							[[*value GetAsDBXValue] SetAsCurrency:[obj doubleValue]];	
						}else {
							[*value SetAsCurrency:[obj doubleValue]];}
							break;						
						case JsonValueType:
							[*value SetAsJSONValue:(TJSONValue *) [self JSONToJSONValue:obj]];	
							break;						
						case BinaryBlobType:
						if ([dbxTypeName isEqualToString:@"TDBXStreamValue"]) {
							[[*value GetAsDBXValue]  SetAsStream:[TStream streamWithJsonArray:[[TJSONArray alloc] initWithJSONArray:obj]]];
						}else {
							
						
							[*value SetAsStream:[TStream streamWithJsonArray:[[TJSONArray alloc] initWithJSONArray:obj]]];
						}
							break;
						case BlobType:
						    
						    [*value SetAsBlob:[TStream streamWithJsonArray:[[TJSONArray alloc] initWithJSONArray:obj]]];
							break;
					case TimeStampType:{
							NSDate * d = [[DBXDefaultFormatter getInstance]StringToDateTime:(NSString*) obj];
							if (d == nil){
								@throw [DBXException
									exceptionWithName:@"InvalidDateFormat"
									reason:@"Invalid DateTimeStamp value"
									userInfo:nil];		
							}
							if ([dbxTypeName isEqualToString:@"TDBXTimeStampValue"]) {
								[[*value GetAsDBXValue] SetAsTimeStamp:d];	
							}else {
								[*value SetAsTimeStamp:d];
							}}
							break;

						case AnsiStringType:
							if([dbxTypeName isEqualToString:@"TDBXAnsiStringValue"]||
							   [dbxTypeName isEqualToString:@"TDBXStringValue"]||
							   [dbxTypeName isEqualToString:@"TDBXAnsiCharsValue"]
							   ){
								[[*value GetAsDBXValue] SetAsAnsiString:(NSString*) obj ];
							} else{					
								[*value SetAsAnsiString: (NSString*) obj ];}
							break;
						case WideStringType:
							if([dbxTypeName isEqualToString:@"TDBXAnsiStringValue"]||
							   [dbxTypeName isEqualToString:@"TDBXStringValue"]||
							   [dbxTypeName isEqualToString:@"TDBXAnsiCharsValue"]||
							   [dbxTypeName isEqualToString:@"TDBXWideStringValue"]
						   ){
							[[*value GetAsDBXValue] SetAsString:(NSString*) obj ];
						} else{					
							[*value SetAsString:(NSString*)  obj ];}
							break;
						case TimeType:{
							int t =[[DBXDefaultFormatter getInstance]StringToTDBXTime:(NSString*) obj];
							if ([dbxTypeName isEqualToString:@"TDBXTimeValue"]) {
								[[*value GetAsDBXValue] SetAsTDBXTime:t];	
							}else {
								[*value SetAsTDBXTime:t];}
						break;}
						case DateType:{
							int t =[[DBXDefaultFormatter getInstance]StringToTDBXDate:(NSString*) obj];
							if ([dbxTypeName isEqualToString:@"TDBXDateValue"]) {
								[[*value GetAsDBXValue] SetAsTDBXDate:t];	
							}else {
								[*value SetAsTDBXDate:t];}	
							break;
							}
						case DateTimeType:
							
							[*value SetAsDateTime:[[DBXDefaultFormatter getInstance]StringToDateTime:(NSString*) obj]];
							break;
						case BooleanType: {
							if ([dbxTypeName isEqualToString:@"TDBXBooleanValue"]) {
							[[*value GetAsDBXValue] SetAsBoolean:[obj boolValue]];																 
							} else{
								[*value SetAsBoolean:[obj boolValue]];}	
							break;
							}
							
						case TableType:
						if ([dbxTypeName isEqualToString:@"TDBXReaderValue"]) {
							[[*value GetAsDBXValue] SetAsTable:[DBXJsonTools JSONToTableType:obj withDBXType:dbxTypeName]];	
							
						} else {
							[*value SetAsTable:[DBXJsonTools JSONToTableType:obj withDBXType:dbxTypeName]];
						}
							break;
							
							
							
						default:
							@throw [DBXException
									exceptionWithName:@"TypeNotFound"
									reason:@"Cannot convert Json to DBX"
									userInfo:nil];
							
							break;
					}		
	
			}else {
				[*value clear]; 
			}

		}
	}
	@finally {
	}
	
}


+(void) JSONToValueType:(TJSONArray*) json  withValueType:(DBXValueType **) valueType{
	DBXValueType * vt = *valueType;
	
	vt.name = [json stringByIndex: 0];
	[vt setDataType:  [json intByIndex: 1]];
	vt.ordinal = [json intByIndex: 2];
	vt.subType = [json intByIndex:3];
	vt.scale = [json intByIndex:4];
	vt.size = [json longByIndex: 5];
	vt.precision = [json longByIndex: 6];
	vt.childPosition = [json intByIndex: 7];
	vt.nullable = [json boolByIndex: 8];
	vt.hidden = [json boolByIndex: 9];
	vt.parameterDirection = [json intByIndex: 10];
	vt.valueParameter = [json boolByIndex: 11];
	vt.literal = [json boolByIndex: 12];
}


+(DBXValueType *) JSONToValueType:(TJSONArray *) json {
	DBXValueType * vt = [[DBXValueType alloc]init];
	[self JSONToValueType:json withValueType: &vt];
	return vt;
}

/**
 * Create a JSONObject from a DBXParameters
 * 
 * @param dbxParameters
 * @return JSONObject
 * @throws JSONException
 */

+(id) DBXParametersToJSONObject:(TParams*) dbxParameters {
	NSMutableDictionary* json = [[NSMutableDictionary alloc]initWithCapacity:0];
	
	NSMutableArray * arr2 = [[NSMutableArray alloc]initWithCapacity:1];
	for (int i = 0; i < [dbxParameters size]; i++) {
		[arr2 addObject:[[dbxParameters getParamByIndex:i]toJson]];
	}
	[json setObject:arr2 forKey:@"table"];
	
	for (int i = 0; i < [dbxParameters size]; i++) {
		NSMutableArray * arr3 = [[NSMutableArray alloc]initWithCapacity:1];
		[arr3 addObject:[[[dbxParameters getParamByIndex:i]getValue] toString ] ];
		[json setObject:arr3 forKey:[dbxParameters getParamByIndex:i].name];		
	}
	return json;

}

/**
 * Create a JSONObject from a DBXReader
 * 
 * @param dbxReader
 * @return JSONObject
 * @throws JSONException
 */

+(TJSONObject *) DBXReaderToJSONObject: (TDBXReader*) dbxReader {
	
	NSMutableDictionary * json = [[NSMutableDictionary alloc]initWithCapacity:0];
	NSMutableArray * arrayParams;
	TParams* columns = [dbxReader columns];
	arrayParams = [[NSMutableArray alloc]initWithCapacity:0];
	for (int i = 0; i < [columns size]; i++) {
		[arrayParams addObject:[[columns getParamByIndex:i]toJson]];
		// Create the empty JSONArray for the data. Will be filled after
		[json setObject:[[NSMutableArray alloc]initWithCapacity:0] forKey:[columns getParamByIndex:i].name];
	}
	
	[dbxReader reset];
	while ([dbxReader next]) {
		for (int c = 0; c < [columns size]; c++)
			[[[[dbxReader columns] getParamByIndex:c] getValue] appendTo: [json objectForKey:[columns getParamByIndex:c].name] ];
	}
	[json setObject:arrayParams forKey:@"table"];
	
	return  [[TJSONObject alloc]initWithJSONObject: json];
	
}

+(TJSONArray *) ValueTypeToJSON:(DBXValueType *) dataType {
	NSArray * Meta = [ NSArray arrayWithObjects:
    dataType.name,
	[NSNumber  numberWithInt:[dataType DataType] ],
	[NSNumber  numberWithInt:dataType.ordinal],
	[NSNumber  numberWithInt:dataType.subType],
	[NSNumber  numberWithLong:dataType.scale],
	[NSNumber  numberWithInt:dataType.size],
	[NSNumber  numberWithLong:dataType.precision],
	[NSNumber  numberWithLong:dataType.childPosition],
	[NSNumber  numberWithBool: dataType.nullable],
	[NSNumber  numberWithBool:dataType.hidden],
	[NSNumber  numberWithInt:dataType.parameterDirection],
    [NSNumber numberWithBool:dataType.valueParameter],
    [NSNumber numberWithInt:dataType.literal],
    nil];
	return  [[TJSONArray alloc]initWithJSONArray: Meta];
}


@end
