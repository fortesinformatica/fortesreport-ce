//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

#import <Foundation/Foundation.h>
#import "DBXDataTypes.h"
#import "DBException.h"
#import "DBXProtocols.h"
#import "TJSONValue.h"



/**
 * @brief Abstract class that can hold all supported simple (String, int, double, etc)
 * and complex types (TDBXReader, TParams, etc)
 * 
 */
@interface DBXValue: NSObject{
@protected	 
	DBXDataTypes CurrentDBXType;
	bool isSimpleValueType ;
	NSDate * DateTimeValue;
	NSDate * TimeStampValue;
	id objectValue;
	NSData * streamValue;


	
	
}
/**
 *It clears internal structure with defauls.
 */
-(void) clear;
/**
 *returns the internal DBXDataType
 *@return  the internal Data type according to DBXDataTypes. 
 */
-(DBXDataTypes) DBXType;
/**
 *Sets the DBX data type  for the value held.
 *@param   value [in] is a DBXDataTypes value.
 */
-(void)setDBXType:(DBXDataTypes) value;
/**
 * Converts internal data into string
 * @return  internal data into string fromat. 
 */
-(NSString *) toString;

/**
 * Adds itself to an TJSONArray.
 * @param json is a json array. 
 */
-(void) appendTo:(NSMutableArray *) json;
/**
 * Detects if its value is null. When setted to null
 * returned value could be wrong. 
 */
-(bool) isNull ;
/**
 *Checks if the internal type is the same with the value held.
 *@param value The DBXDataTypes it should hold. 
 */
-(void) checkCurrentDBXType: (DBXDataTypes) value;
//setter

/**
 *Sets Internal object as integer.
 *@param value an integer value. 
 */
-(void) SetAsInt8:(int) value ;

/**
 *Sets Internal object as  16 bytes integer.
 *@param value an 16 bytes integer value. 
 */
-(void) SetAsInt16:(int) value ; 

/**
 *Sets Internal object as  32 bytes integer.
 *@param value an 32 bytes integer value. 
 */
-(void) SetAsInt32:(long )value ;

/**
 *Sets Internal object as 64 bytes integer.
 *@param value an 64 bytes integer value. 
 */
-(void) SetAsInt64:(long long) value ;
/**
 *Sets Internal object as unsinged 8 bytes integer.
 *@param value an unsinged 8 bytes  integer value. 
 */
-(void) SetAsUInt8:(unsigned int) value ;

/**
 *Sets Internal object as unsinged 16 bytes integer.
 *@param value an unsinged 16 bytes  integer value. 
 */
-(void) SetAsUInt16:(unsigned int) value ;

/**
 *Sets Internal object as unsinged 32 bytes integer.
 *@param value an unsinged 32 bytes  integer value. 
 */
-(void) SetAsUInt32:(unsigned long )value ;

/**
 *Sets Internal object as unsinged 64 bytes integer.
 *@param value an unsinged 64 bytes  integer value. 
 */
-(void) SetAsUInt64:(unsigned long long)value;

/**
 *Sets Internal object as string.
 *@param value a string value. 
 */
-(void) SetAsString: (NSString*) value;
/**
 *Sets Internal object as string.
 *@param value a string value. 
 */
-(void) SetAsAnsiString: (NSString *) value ;
/**
 *Sets Internal object as string.
 *@param value a string value. 
 */
-(void) SetAsWideString: (NSString *) value ;
/**
 *Sets Internal object as an NSDate.
 *@param value a NSDate value. 
 */
-(void) SetAsDateTime:(NSDate *) value;
/**
 *Sets Internal object as an NSDate.
 *@param value a NSDate value. 
 */
-(void) SetAsTimeStamp:(NSDate *) value;
/**
 *Sets Internal object as currency.
 *@param value a currency value. 
 */
-(void) SetAsCurrency:(double) value;
/**
 *Sets Internal object as  double.
 *@param value a double value. 
 */
-(void) SetAsDouble:(double) value; 
/**
 *Sets Internal object as afloat.
 *@param value a float value. 
 */
-(void) SetAsSingle:(float) value;
/**         
 *Sets Internal object as an TDBXDate.
 *@param value an TDBXDate value. Number of days since  0001-01-01 pure gregoria calendar.
 */
-(void) SetAsTDBXDate:(long) value;

 /**         
 *Sets Internal object as an TDBXTime.
 *@param value an TDBXTime value. Hour expressed in milleseconds.
 */
-(void) SetAsTDBXTime:(long) value;

/**         
 *Sets Internal object as an NSData.
 *@param value an NSData value.
 */
-(void) SetAsStream:(NSData*) value; 
/**         
 *Sets Internal object as an id<TableType>.
 *@param value an id<TableType> value.
 */
-(void) SetAsTable:(id<TableType>)value;
/**         
 *Sets Internal object as an TJSONValue.
 *@param value an TJSONValue value.
 */
-(void) SetAsJSONValue:(TJSONValue*)value;
 /**         
 *Sets Internal object as boolean value.
 *@param value a boolean value.
 */
-(void) SetAsBoolean:(bool)value;
/**         
 *Sets Internal object as Blob.
 *@param value an NSData value.
 */
-(void) SetAsBlob:(NSData *) value;
/**         
 *Sets Internal object as an Bcd.
 *@param value a double value.
 */
-(void) SetAsBcd:(double) value;
/**         
 *Sets Internal object as an DBXValue.
 *@param value an DBXValue value.
 */
-(void) SetAsDBXValue:(DBXValue *)value;
/**         
 *Sets Internal object as null.

 */
-(void) SetNull;
/**         
 *Sets Internal object as an TBXNull.
 *@param  an TBXNull value.
 */
-(void) SetTDBXNull:(NSString *) TypeName;

//Getter
/**         
 *Gets the internal stored  8 integer object .
 *@return  an 8 integer.
 */
-(int) GetAsInt8;
/**         
 *Gets the internal stored  16 integer object .
 *@return  an 16 integer.
 */
-(int) GetAsInt16;
/**         
 *Gets the internal stored  32 integer object .
 *@return  an 32 integer.
 */
-(long) GetAsInt32;
/**         
 *Gets the internal stored  32 integer object .
 *@return  an 32 integer.
 */
-(long long) GetAsInt64;
/**         
 *Gets the internal stored unsigned 8 integer object .
 *@return  an unsinged 8 integer.
 */
-(unsigned int) GetAsUInt8;

/**         
 *Gets the internal stored unsigned 16 integer object .
 *@return  an unsinged 16 integer.
 */
-(unsigned int) GetAsUInt16;

/**         
 *Gets the internal stored unsigned 32 integer object .
 *@return  an unsinged 32 integer.
 */
-(unsigned long) GetAsUInt32;
/**         
 *Gets the internal stored unsigned 64 integer object .
 *@return  an unsinged 64 integer.
 */
-(unsigned long long) GetAsUInt64; 

/**         
 *Gets the internal stored TDBXDate object .
 *@return TDBXDate integer. Number of days since 0001-01-01 using pure Gregorian calendar.
 */
-(long) GetAsTDBXDate;
                        /**         
 *Gets the internal stored TDBXTime object .
 *@return TBDTime value. Time expressed in milleseconds.
 */   
-(long) GetAsTDBXTime;

/**Gets the internal stored string object .
 *@return string value.
 */
-(NSString *) GetAsString; 

/**Gets the internal stored string object .
 *@return string value.
 */
-(NSString *) GetAsAnsiString;

/**Gets the internal stored string object .
 *@return string value.
 */
-(NSString *) GetAsWideString;

/**Gets the internal stored datetime object .
 *@return datetime value.
 */
-(NSDate*)GetAsDateTime;

/**Gets the internal stored datetime object .
 *@return datetime value.
 */
-(NSDate*)GetAsTimeStamp;

/**Gets the internal stored currency object .
 *@return currency value.
 */
-(double) GetAsCurrency;

/**Gets the internal stored double object .
 *@return double value.
 */
-(double) GetAsDouble;

/**Gets the internal stored float object .
 *@return float value.
 */
-(float) GetAsSingle;

/**Gets the internal stored NSData object .
 *@return NSData value.
 */
-(NSData *) GetAsStream;

/**Gets the internal stored id<TableType> object .
 *@return id<TableType> value.
 */
-(id<TableType>)GetAsTable;

/**Gets the internal stored TJSONValue object .
 *@return TJSONValue value.
 */
-(TJSONValue *) GetAsJSONValue;

/**Gets the internal stored boolean object .
 *@return boolean value.
 */
-(bool) GetAsBoolean;

/**Gets the internal stored Blob object .
 *@return NSData value.
 */
-(NSData *) GetAsBlob;

/**Gets the internal stored bcd object .
 *@return double value.
 */
-(double) GetAsBcd;

/**Gets the internal stored DBXValue object .
 *@return DBXValue value.
 */
-(DBXValue * ) GetAsDBXValue;
@end
/**
 * @brief This class extends DBXValue and overrides the setter and getter methods of
 * DBXValue to allows to set and get specified types
 */
@interface DBXWritableValue: DBXValue{
@private	  
	bool booleanValue;
	long INT32Value;
	unsigned long UINT32Value;
	long INT64Value;
	unsigned long UINT64Value;
	NSString *stringValue;
	float singleValue;
	double doubleValue;
	id JSONGenericElement;	

	TJSONValue * JSONValueValue;
	double bcdValue;	
	DBXValue* DBXValueValue;
}


@end

