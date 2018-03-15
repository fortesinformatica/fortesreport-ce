//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

#import <Foundation/Foundation.h>
#import "DBXValue.h"
#import "DBXValueType.h"
#import "TParams.h"
#import "TDBXReader.h"
#import "TJSONObject.h"
#import "TJSONArray.h"
/**
 * @brief  Some utility functions to convert json object into String, TJSOnValue,
 * TDBXReader,TParams.
 * 
 * Interna JSON Parser considers:
 *    
 * JSONArray  is an NSArray 
 * JSONObject is an NSDictiornay       
 */

@interface DBXJsonTools : NSObject {

}
/**
 *  Insert a jsonObject into a DBXValue converting it to the right value
 *  @param json [in] it is the value to be inserted into the DBXValue  it can be a NSArray, NSNumber,ecc
 *  @param value [in/out] it is the dbxValue that hold the value rapresented by the jsonobject  
 *    
 */
+ (void) jsonToDBX:(id) obj withDbxValue: (DBXValue **) value
   withDBXTypeName: (NSString *) dbxTypeName;
   
   /**
	 * create a DBXValueType from a NSArray
	 * 
	 * @param json [in] NSArray with the values to set into DBXValuesType
	 * @return a DBXValueType

	 */
+(DBXValueType *) JSONToValueType:(TJSONArray *) json;

	/**
	 * Add a NSArray to a DBXValueType 
	 * 
	 * @param json [in] NSArray to old the values
	 * @param valueType [in/out] the DBXValueType to set
	 */
+(void) JSONToValueType:(TJSONArray*) json  withValueType:(DBXValueType**) valueType;

	/**
	 * Create a DBXValueType from a NSArray
	 * 
	 * @param json [in]  NSArray with the values
	 * @param  return a DBXValueType 
	 */
//+(DBXValueType *) JSONToValueType:(TJSONArray *) json ;
	/**
	 * Convert a DBXParameter to a JsonObject (NSDictionary)
	 * 
	 * @param  dbxParameters [in] dbx parameter to convert into a jsonobject
	 *            
	 * @param return an NSDictionary)
	 */
+(TJSONObject *) DBXParametersToJSONObject:(TParams*) dbxParameters ;
/**
 * Create TJSonValue from a Json Object  (NSArray or NSDictionary)
 * @param o [in] the json object to convert NSArray or NSDictionary
 * @param return  returns the a TJSonValue that rappresents the json object.  
 */ 

+(TJSONValue *) JSONToJSONValue:(id) o;

/**
 *  Create a json object (NSDictionay) from a TDBXReader
 *  @param dbxReader [in] is the dbxReader to convert
 *  @param return an NSDictionary with the values of the dbxReader  
 */
+(id) DBXReaderToJSONObject: (TDBXReader*) dbxReader;
/**
	 * Converts a DBXValueType to a json object
	 * @param dataType [in]  the DBXValueType to set into the NSArray
	 * @return a NSArray with the values of the DBXValueType  setted
	 */
+(TJSONArray *) ValueTypeToJSON:(DBXValueType *) dataType ;
@end

