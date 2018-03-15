//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

#import "DBXValue.h"
#import "DBException.h"
#import "DBXDefaultFormatter.h"
#import "TStream.h"
#import "DBXProtocols.h"
#import "TDBXAnsiStringValue.h"
#import "TDBXAnsiCharsValue.h"
#import "TDBXBooleanValue.h"
#import "TDBXDoubleValue.h"
#import "TDBXUInt8Value.h"
#import "TDBXInt8Value.h"
#import "TDBXUInt16Value.h"
#import "TDBXInt16Value.h"
#import "TDBXInt32Value.h"
#import "TDBXInt64Value.h"
#import "TDBXSingleValue.h"
#import "TDBXStringValue.h"
#import "TDBXTimeValue.h"
#import "TDBXTimeStampValue.h"
#import "TDBXWideStringValue.h"
#import "TDBXDateValue.h"
#import "TDBXReaderValue.h"
#import "TDBXStreamValue.h"
#import "TJSONNull.h"



@implementation DBXValue


-(DBXDataTypes) DBXType {
	return CurrentDBXType;
}
-(void)setDBXType:(DBXDataTypes) value {
	CurrentDBXType = value;	
	isSimpleValueType = NO;
}

-(id) init {

	self = [super init];
	if (self){ 
		[self setDBXType: UnknownType];
	}
	return self;
}

-(void) checkCurrentDBXType: (DBXDataTypes) value{
	if (value != CurrentDBXType) {
		@throw [DBXException
						  exceptionWithName:@"IncorrectDBXType"
						  reason:@"Incorrect type in DBXValue"
						  userInfo:nil];
		 
	}
}
-(void) abstractMethodException{
	@throw [DBXException
			exceptionWithName:@"AbtractMethod"
			reason:@""
			userInfo:nil];
	
	
	
}
-(bool) containsASimpleValueType {return isSimpleValueType;}

-(bool) isNull {
	return CurrentDBXType == UnknownType;
}
-(void) SetNull{
	[self abstractMethodException];
}

-(void) clear{
	[self abstractMethodException];	
}

/*** setter ****/
-(void) SetAsInt8:(int) value {
	[self abstractMethodException];
}
-(void) SetAsInt16:(int) value {
	[self abstractMethodException];
}
-(void) SetAsInt32:(long) value {
	[self abstractMethodException];
}
-(void) SetAsInt64:(long long) value {
	[self abstractMethodException];
}


-(void) SetAsUInt8:(unsigned int) value {
	[self abstractMethodException];
}
-(void) SetAsUInt16:(unsigned int) value {
	[self abstractMethodException];
}
-(void) SetAsUInt32:(unsigned long) value {
	[self abstractMethodException];
}
-(void) SetAsUInt64:(unsigned long long) value {
	[self abstractMethodException];
}


-(void) SetAsString: (NSString*) value{
	[self abstractMethodException];	
}

-(void) SetAsAnsiString: (NSString *) value {
	[self abstractMethodException];
}
-(void) SetAsWideString: (NSString *) value {
	[self abstractMethodException];
}
-(void) SetAsDateTime:(NSDate *) value {
[self abstractMethodException];
}
-(void) SetAsTimeStamp:(NSDate *) value {
	[self abstractMethodException];

}
-(void) SetAsSingle:(float) value {
	[self abstractMethodException];
}
-(void) SetAsTDBXDate:(long) value{
	[self abstractMethodException];
}
-(void) SetAsTDBXTime:(long) value{
	[self abstractMethodException];
}
-(void) SetAsStream:(NSData *)value{
	[self abstractMethodException];
}
-(void) SetAsTable:(id<TableType>)value{
	[self abstractMethodException];
}

-(void) SetAsJSONValue:(TJSONValue*)value{
	[self abstractMethodException];
}
-(void) SetAsBoolean:(bool)value{
	[self abstractMethodException];
}

-(void) SetAsBlob:(NSData *)value{
	[self abstractMethodException];

} 
-(void) SetAsBcd:(double)value{
	[self abstractMethodException];
	
} 
-(void) SetAsDBXValue:(DBXValue *)value{
	[self abstractMethodException];
	
} 
-(void) SetTDBXNull:(NSString *) TypeName{
		[self abstractMethodException];
}
/*** Getters ****/
-(int) GetAsInt8{
	[self abstractMethodException];	
	return 0;
}
-(int) GetAsInt16{
	[self abstractMethodException];	
	return 0;
}
-(long) GetAsInt32{
	[self abstractMethodException];	
	return 0;
}
-(long long) GetAsInt64{
	[self abstractMethodException];	
	return 0;
}

-(unsigned int) GetAsUInt8{
	[self abstractMethodException];	
	return 0;
}
-(unsigned int) GetAsUInt16{
	[self abstractMethodException];	
	return 0;
}
-(unsigned long) GetAsUInt32{
	[self abstractMethodException];	
	return 0;
}
-(unsigned long long) GetAsUInt64{
	[self abstractMethodException];	
	return 0;
}

-(void) SetAsCurrency:(double ) value {
	[self abstractMethodException];
}
-(void) SetAsDouble:(double ) value {
	[self abstractMethodException];
}

-(NSString *) GetAsString{
	[self abstractMethodException];
	return 0;
	
}

-(NSString *) GetAsAnsiString {
	[self abstractMethodException];
	return 0;
}
-(NSString *) GetAsWideString {
	[self abstractMethodException];
	return 0;
}


-(NSDate *) GetAsDateTime{
	[self abstractMethodException];
	return 0 ;
}
-(NSDate *) GetAsTimeStamp{
	[self abstractMethodException];
	return 0 ;
}
-(double ) GetAsCurrency{
	[self abstractMethodException];
	return 0 ;
}
-(double ) GetAsDouble{
	[self abstractMethodException];
	return 0 ;
}
-(float ) GetAsSingle{
	[self abstractMethodException];
	return 0 ;
}
-(long) GetAsTDBXDate{
	[self abstractMethodException];
	return 0;
}
-(long) GetAsTDBXTime{
	[self abstractMethodException];
	return 0;
}
-(NSData *) GetAsStream{
	[self abstractMethodException];
	return nil;
}
-(id<TableType>) GetAsTable{
	[self abstractMethodException];
	return nil;
}

-(NSString *) toString {
	@try {
        if ([ self containsASimpleValueType]){
            if ([ [self GetAsDBXValue] isNull]) {
                return [[[ TJSONNull alloc ] init] asJSONString];
            }else{
                return [[ self GetAsDBXValue ] toString];
            }
		}
		switch (CurrentDBXType) {
				
			case Int32Type: {
				return   [ [DBXDefaultFormatter getInstance ] Int32ToString:[self GetAsInt32]] ;
				
				break;
			}
			case Int64Type: {
				return   [ [DBXDefaultFormatter getInstance ] Int64ToString:[self GetAsInt64]] ;
				
				break;
			}
			case UInt8Type: {
				return   [ [DBXDefaultFormatter getInstance ] UInt8ToString:[self GetAsUInt8]] ;
				
				break;
			}
			case UInt16Type: {
				return   [ [DBXDefaultFormatter getInstance ] UInt16ToString:[self GetAsUInt16]] ;
				
				break;
			}
			case UInt32Type:
				return   [ [DBXDefaultFormatter getInstance ] UInt32ToString:[self GetAsUInt32]] ;
				
				break;
			case UInt64Type:
				return   [ [DBXDefaultFormatter getInstance ] UInt64ToString:[self GetAsUInt64]] ;
				
				break;
				
				
			case Int16Type: {
				return   [ [DBXDefaultFormatter getInstance ] Int16ToString:[self GetAsInt16]] ;
				break;
			}				
			case Int8Type: {
				return   [ [DBXDefaultFormatter getInstance ] Int8ToString:[self GetAsInt8]] ;
				
				break;
			}
				
			case AnsiStringType: {
				return   [ [DBXDefaultFormatter getInstance ] AnsiStringToString:[self GetAsString]] ;
				break;
			}
			case WideStringType: {
				return   [ [DBXDefaultFormatter getInstance ] WideStringToString:[self GetAsString]] ;
				break;
			}
			case DateTimeType: {
				return   [ [DBXDefaultFormatter getInstance ] DateTimeToString:[self GetAsDateTime]] ;
				break;
			}
			case CurrencyType:{
				return   [ [DBXDefaultFormatter getInstance ] currencyToString:[self GetAsCurrency]] ;
				break;
			}
				
			case DoubleType:
				return   [ [DBXDefaultFormatter getInstance ] doubleToString:[self GetAsDouble]] ;
				break;
	        case SingleType:
				return   [ [DBXDefaultFormatter getInstance ] floatToString:[self GetAsSingle]] ;
				break;
			case DateType:
				return   [ [DBXDefaultFormatter getInstance ] TDBXDateToString:[self GetAsTDBXDate]] ;
				break;
				
			case TimeType:
				return   [ [DBXDefaultFormatter getInstance ] TDBXTimeToString:[self GetAsTDBXTime]] ;
				break;
			case TimeStampType:
				return   [ [DBXDefaultFormatter getInstance ] DateTimeToString:[self GetAsTimeStamp]] ;
				break;
			case BooleanType:
				return   [ [DBXDefaultFormatter getInstance ] BoolToString:[self GetAsBoolean]] ;
				break;
			case BcdType:
				return [[DBXDefaultFormatter getInstance]doubleToString:[self GetAsBcd]];
				break;

				
				
				
			default:{
				@throw [DBXException
						exceptionWithName:@"TypeStringConversion"
						reason:@"Cannot convert this type to string"
						userInfo:nil];
				
				
			}
				
		}
	} 
	@catch (NSException * ex) {
		
		return @"<CANNOT CONVERT TO STRING>";
	}
}
-(void) appendTo:(NSMutableArray *) json{
	if ([ self containsASimpleValueType]){
	[[ self GetAsDBXValue ] appendTo:json];
		
	}else {
		switch (CurrentDBXType) {
				
				
			case Int8Type: {
				[json addObject: [NSNumber numberWithInt: [self GetAsInt8]]];
				break;
			}
			case Int16Type: {
				[json addObject: [NSNumber numberWithInt: [self GetAsInt16]]];

				break;
			}
			case Int32Type: {
				[json addObject: [NSNumber numberWithLong:[self GetAsInt32]]];

				break;
			}
			case Int64Type: {
				[json addObject: [NSNumber numberWithLongLong: [self GetAsInt64]]];

				break;
			}
			case UInt8Type: {
				[json addObject: [NSNumber numberWithUnsignedInt: [self GetAsUInt8]]];

		
				break;
			}
			case UInt16Type: {
				[json addObject: [NSNumber numberWithUnsignedInt: [self GetAsUInt16]]];

				break;
			}
			case UInt32Type: {
				[json addObject: [NSNumber numberWithUnsignedLong: [self GetAsUInt32]]];

				break;
			}
			case UInt64Type: {
				[json addObject: [NSNumber numberWithUnsignedLongLong: [self GetAsUInt64]]];

				break;
			}
			case AnsiStringType:
			case WideStringType: {
				[json addObject:[self GetAsString]];

				break;
			}
			case DateTimeType: {
				[json addObject:[[DBXDefaultFormatter getInstance]DateTimeToString:DateTimeValue ]];
				break;
			}
			case TimeStampType: {
				[json addObject:[[DBXDefaultFormatter getInstance]DateTimeToString:TimeStampValue ]];
				break;
			}
			case DateType: {
				[json addObject:[[DBXDefaultFormatter getInstance]TDBXDateToString:[self GetAsTDBXDate] ]];
				break;
			}
			case TimeType: {
				[json addObject:[[DBXDefaultFormatter getInstance]TDBXTimeToString:[self GetAsTDBXTime] ]];
				break;
			}
			case CurrencyType: {
				[json addObject: [NSNumber numberWithDouble: [self GetAsCurrency]]];
				break;
			}
			case DoubleType: {
				[json addObject: [NSNumber numberWithDouble: [self GetAsDouble]]];
				break;
			}
			case SingleType: {
				[json addObject: [NSNumber numberWithFloat: [self GetAsSingle]]];
				break;
			}
				
			case BinaryBlobType:{
				
				[ json addObject:[TStream StreamToJson:[self GetAsStream]]]; 
				
				break;}
			case TableType:{
				if (objectValue) {
					
				
				[json addObject:[ (id<JSONSerializable>) objectValue  asJSONObject]];
				}
				break;
			}
			case JsonValueType:{
				id o = [[self GetAsJSONValue] getInternalObject];
				[json addObject:o];		
				break;
			}
			default:
				break;
		}
	}
	
}
-(TJSONValue *) GetAsJSONValue{
	[self abstractMethodException];
	return nil;
}
-(bool) GetAsBoolean{
	[self abstractMethodException];
	return "NO";
}
-(NSData *) GetAsBlob{
	[self abstractMethodException];
	return nil;
}
-(double) GetAsBcd{
	[self abstractMethodException];
	return 0;
}
-(DBXValue*) GetAsDBXValue{
	[self abstractMethodException];
	return nil;
}
@end
@implementation DBXWritableValue



-(void) SetAsInt8:(int) value {
	if ((value >= -128) && (value<= 127)) {
		INT32Value = value;}
	else
	{
		@throw [DBXException
				exceptionWithName:@"InvalidInt8Value"
				reason:@"value  %i is not a valid Int8 (-127..128)"
				userInfo:nil];
		
	}
	[self setDBXType:Int8Type];
}
-(void) SetAsInt16:(int) value {
	if ((value >= -32767) && (value<= 32767)) {
		INT32Value = value;}
	else
	{
		@throw [DBXException
				exceptionWithName:@"InvalidInt16Value"
				reason:@"value  %i is not a valid Int16 (-32767..32767)"
				userInfo:nil];
		
	}
	[self setDBXType:Int16Type];
}
-(void) SetAsInt32:(long) value {
	if ((value >=   -2147483647) && (value<= 2147483647)) {
		INT32Value = value;}
	else
	{
		@throw [DBXException
				exceptionWithName:@"InvalidInt32Value"
				reason:@"value  %i is not a valid Int32 "
				userInfo:nil];		
	}
	[self setDBXType:Int32Type];
}
-(void) SetAsInt64:(long long)value{
	if ((value >=   LLONG_MIN) && (value<= LLONG_MAX)) {
		INT64Value = (long)value;}
	else
	{
		@throw [DBXException
				exceptionWithName:@"InvalidInt64Value"
				reason:@"value  %i is not a valid Int64 "
				userInfo:nil];		
	}
	[self setDBXType:Int64Type];
	
}


-(void) SetAsUInt8:(unsigned int) value {
	if (value<= 255) {
		UINT32Value = value;}
	else
	{
		@throw [DBXException
				exceptionWithName:@"InvalidUInt8Value"
				reason:@"value  %i is not a valid UInt8 (0..255)"
				userInfo:nil];
		
	}
	[self setDBXType:UInt8Type];
}
-(void) SetAsUInt16:(unsigned int) value {
	if (value<= 65535) {
		UINT32Value = value;}
	else
	{
		@throw [DBXException
				exceptionWithName:@"InvalidUInt16Value"
				reason:@"value  %i is not a valid UInt16 (0..655335)"
				userInfo:nil];
		
	}
	[self setDBXType:UInt16Type];
}
-(void) SetAsUInt32:(unsigned long) value {
	
	if (value<= ULONG_MAX) {
		UINT32Value = value;
	}
	else
	{
		@throw [DBXException
				exceptionWithName:@"InvalidUInt32Value"
				reason:@"value  %i is not a valid UInt32 "
				userInfo:nil];		
	}
	[self setDBXType:UInt32Type];
	
}
-(void) SetAsUInt64:(unsigned long long)value{
	if (value<= ULLONG_MAX) {
		UINT64Value = (unsigned long)value;}
	else
	{
		@throw [DBXException
				exceptionWithName:@"InvalidUInt64Value"
				reason:@"value  %i is not a valid UInt64 "
				userInfo:nil];		
	}
	[self setDBXType:UInt64Type];
	
}

-(void) SetAsString: (NSString*) value{
	stringValue = value;
	[self setDBXType:WideStringType];
	
}

-(void) SetAsAnsiString: (NSString *) value {
	[self SetAsString: value];
}
-(void) SetAsWideString: (NSString *) value {
	[self SetAsString: value];
}
-(void) SetAsDateTime:(NSDate *) value {
	DateTimeValue = value;
	[self setDBXType:DateTimeType];
}
-(void) SetAsTimeStamp:(NSDate *) value {
	TimeStampValue = value;
	[self setDBXType:TimeStampType];
}
-(void) SetAsCurrency:(double) value {
 	doubleValue =   value;
	[self setDBXType:CurrencyType];
}
-(void) SetAsDouble:(double) value {
 	doubleValue =   value;
	[self setDBXType:DoubleType];
}
-(void) SetAsSingle:(float) value {
 	singleValue =   value;
	[self setDBXType:SingleType];
}
-(void) SetAsTDBXDate:(long) value{
 	INT32Value =   value;
	[self setDBXType:DateType];
	
}
-(void) SetAsTDBXTime:(long) value{
 	INT32Value =   value;
	[self setDBXType:TimeType];
	
}
-(void) SetAsStream:(NSData *) value{
	streamValue = value;
	[self setDBXType:BinaryBlobType];
}
-(void) SetAsTable:(id<TableType>)value{
	objectValue = value;
	[self setDBXType:TableType];}
-(void) SetAsJSONValue:(TJSONValue*)value{
	JSONValueValue = value;
	[self setDBXType:JsonValueType];
}	

-(void) SetAsBoolean:(bool)value{
	booleanValue = value;
	[self setDBXType:BooleanType];
}
-(void) SetAsBlob:(NSData *)value{
	objectValue = value;
	[self setDBXType:BlobType];
}
-(void) SetAsBcd:(double) value {
 	bcdValue =   value;
	[self setDBXType:BcdType];
}

-(void) SetTDBXNull:(NSString *)TypeName{
	if ([TypeName isEqualToString:@"TDBXStringValue"]) {
		TDBXStringValue* v = [[TDBXStringValue alloc]init];
		[v SetNull];
		[self SetAsDBXValue:v];
	} else if ([TypeName isEqualToString:@"TDBXAnsiCharsValue"]) {
		TDBXAnsiCharsValue * v = [[TDBXAnsiCharsValue alloc]init];
		[v SetNull];
		[self SetAsDBXValue:v];
	} else if ([TypeName isEqualToString:@"TDBXAnsiStringValue"]) {
		TDBXAnsiStringValue *v = [[ TDBXAnsiStringValue alloc]init];
		[v SetNull];
		[self SetAsDBXValue:v];
	} else if ([TypeName isEqualToString:@"TDBXSingleValue"]) {
		TDBXSingleValue *v = [[TDBXSingleValue alloc]init];
		[v SetNull];
		[self SetAsDBXValue:v];
	} else if ([TypeName isEqualToString:@"TDBXWideStringValue"]) {
		TDBXWideStringValue *v = [[TDBXWideStringValue alloc]init];
		[v SetNull];
		[self SetAsDBXValue:v];
	} else if ([TypeName isEqualToString:@"TDBXDateValue"]) {
		TDBXDateValue *v = [[TDBXDateValue alloc]init];
		[v SetNull];
		[self SetAsDBXValue:v];
	} else if ([TypeName isEqualToString:@"TDBXTimeValue"]) {
		TDBXTimeValue *v = [[ TDBXTimeValue alloc]init];
		[v SetNull];
		[self SetAsDBXValue:v];
	} else if ([TypeName isEqualToString:@"TDBXBooleanValue"]) {
		TDBXBooleanValue *v = [[TDBXBooleanValue alloc]init];
		[v SetNull];
		[self SetAsDBXValue:v];
	} else if ([TypeName isEqualToString:@"TDBXDoubleValue"]) {
		TDBXDoubleValue *v = [[TDBXDoubleValue alloc]init];
		[v SetNull];
		[self SetAsDBXValue:v];
	} else if ([TypeName isEqualToString:@"TDBXInt64Value"]) {
		TDBXInt64Value* v = [[TDBXInt64Value alloc]init];
		[v SetNull];
		[self SetAsDBXValue:v];
	} else if ([TypeName isEqualToString:@"TDBXInt32Value"]) {
		TDBXInt32Value *v = [[TDBXInt32Value alloc]init];
		[v SetNull];
		[self SetAsDBXValue:v];
	} else if ([TypeName isEqualToString:@"TDBXInt16Value"]) {
		TDBXInt16Value *v = [[TDBXInt16Value alloc]init];
		[v SetNull];
		[self SetAsDBXValue:v];
	} else if ([TypeName isEqualToString:@"TDBXInt8Value"]) {
		TDBXInt8Value *v = [[TDBXInt8Value alloc]init];
		[v SetNull];
		[self SetAsDBXValue:v];
	}else if ([TypeName isEqualToString:@"TDBXStreamValue"]) {
		TDBXStreamValue *v = [[TDBXStreamValue alloc]init];
		[v SetNull];
		[self SetAsDBXValue:v];
	}else if ([TypeName isEqualToString:@"TDBXReaderValue"]) {
		TDBXReaderValue *v = [[TDBXReaderValue alloc]init];
		[v SetNull];
		[self SetAsDBXValue:v];
	}
	
}
-(void) SetAsDBXValue:(DBXValue *)value{
	[self  setDBXType:value.DBXType];
	isSimpleValueType = true;

	DBXValueValue = value;
	
}


/****** Getters******/

-(int) GetAsInt8{
	[self  checkCurrentDBXType:Int8Type];
	return INT32Value;
	
}
-(int) GetAsInt16{
	[self  checkCurrentDBXType:Int16Type];
	return INT32Value;
	
}
-(long) GetAsInt32{
	[self  checkCurrentDBXType:Int32Type];
	return INT32Value;
	
}
-(long long) GetAsInt64{
	[self  checkCurrentDBXType:Int64Type];
	return INT64Value;
	
}

-(unsigned int) GetAsUInt8{
	[self  checkCurrentDBXType:UInt8Type];
	return UINT32Value;
	
}
-(unsigned int) GetAsUInt16{
	[self  checkCurrentDBXType:UInt16Type];
	return UINT32Value;
	
}
-(unsigned long) GetAsUInt32{
	[self  checkCurrentDBXType:UInt32Type];
	return UINT32Value;
	
}
-(unsigned long long) GetAsUInt64{
	[self  checkCurrentDBXType:UInt64Type];
	return UINT64Value;
	
}


-(NSString *) GetAsString{
	[self checkCurrentDBXType: WideStringType];
	return stringValue ;
	
}



-(NSString *) GetAsAnsiString {
	[self checkCurrentDBXType:CurrentDBXType];
	return stringValue;
}
-(NSString *) GetAsWideString {
	[self checkCurrentDBXType:CurrentDBXType];
	return stringValue;
}


-(NSDate *) GetAsDateTime{
	[self checkCurrentDBXType: DateTimeType];
	return DateTimeValue ;
}
-(NSDate *) GetAsTimeStamp{
	[self checkCurrentDBXType: TimeStampType];
	return TimeStampValue ;
}
-(double) GetAsCurrency{
	[self checkCurrentDBXType: CurrencyType];
	return doubleValue ;
}
-(double) GetAsDouble{
	[self checkCurrentDBXType: DoubleType];
	return doubleValue ;
}
-(float) GetAsSingle{
	[self checkCurrentDBXType: SingleType];
	return singleValue ;
}
-(long) GetAsTDBXDate{
	[self  checkCurrentDBXType:DateType];
	return INT32Value;
	
}
-(long) GetAsTDBXTime{
	[self  checkCurrentDBXType:TimeType];
	return INT32Value;
	
}
-(NSData*) GetAsStream{
	[self checkCurrentDBXType:BinaryBlobType];
	return streamValue;
}
-(id<TableType>) GetAsTable{
	[self checkCurrentDBXType:TableType];
	return objectValue ;
}
-(TJSONValue *) GetAsJSONValue{
	[self checkCurrentDBXType:JsonValueType];
	return JSONValueValue;
}

-(bool) GetAsBoolean{
	[self checkCurrentDBXType:BooleanType];
	return booleanValue;
}
-(NSData *) GetAsBlob{
	[self checkCurrentDBXType:BlobType];
	return objectValue;

}
-(double) GetAsBcd{
	[self checkCurrentDBXType: BcdType];
	return bcdValue ;
}
-(DBXValue *) GetAsDBXValue {
	if (!isSimpleValueType){
		@throw [DBXException
				exceptionWithName:@"InvalidDBXType"
				reason:@"value  %i is not a valid DBXType"
				userInfo:nil];
	}
	return DBXValueValue;
}

-(void) clear{

	
	booleanValue = NO;
	INT32Value = 0;
	UINT32Value = 0;
	INT64Value = 0;
	UINT64Value = 0;
	stringValue = @"";
	singleValue = 0;
	doubleValue = 0;
	DateTimeValue = nil;
	streamValue = nil;
	objectValue = nil;
	JSONValueValue = nil;
	bcdValue = 0;
	DBXValueValue = nil;
	TimeStampValue = nil;		
	
}
@end