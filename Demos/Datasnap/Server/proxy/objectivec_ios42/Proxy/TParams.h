//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

#import <Foundation/Foundation.h>
#import "DBXProtocols.h"
#import "DBXParameter.h"
#import "TJSONObject.h"
#import "TJSONArray.h"


 /**
 * 
 * @brief TParams manages a list of parameters.
 *
 * Use the properties and methods of TParams to:
 *	<br>&nbsp;&nbsp;&nbsp;&nbsp;- Get a parameter by name or index.
 *	<br>&nbsp;&nbsp;&nbsp;&nbsp;- Find parameter by name.
 * 	<br>&nbsp;&nbsp;&nbsp;&nbsp;- Add field parameters from the list.
 *	<br>&nbsp;&nbsp;&nbsp;&nbsp;- Create a TParams from a JSONObject or a metadata JSONArray.
 *	<br>&nbsp;&nbsp;&nbsp;&nbsp;- Loads parameters values.
 */

@interface TParams : NSObject< TableType> {
	NSMutableArray * params;

}
/**
 * Loads parameter values from a json object
 * @param params the list of parameters
 * @param value json object with values
 * @param offset  index to start reading information default 0.   
 */

+(bool)loadParametersValues:(TParams **) params withJSON:(TJSONObject *) value andOffSet: (int) offset;

/**
 * Loads parameter values from a json object
 * @param params the list of parameters
 * @param value json object with values
 
 */

+(bool)loadParametersValues:(TParams **) params withJSON:(TJSONObject *) value;

/**
 * Loads parameter  from  metatdata
 * @param paramsMetaData the list of parameters  metadata
   
 */

+ (TParams*) createParametersWithMetadata:(TJSONArray *) paramsMetadata ;

/**
 * Loads parameter  from  a json object
 * @param value json object with parameters  information.
   
 */
+(id) paramsWithJSON:(TJSONObject *) value ;


/**
 *Finds a parameter using its name
 *@param value the name of the param
 *@return  the parameter found or null
 */
-(DBXParameter*) findParamByName:(NSString*) value ;
/**
 *Gets a parameter using its name
 *@param value the name of the param
 *@return  the parameter found or null
 */
-(DBXParameter*) getParamByName:(NSString *)value;
/**
 *add  a parameter 
 *@param parameter the parameter to add
 *@return  the list of parameters
 */
-(TParams*) addParameter:(DBXParameter*) parameter;
/**
 *gets a parameter using its index
 *@param index the index of the param
 *@return  the parameter found or null
 */
-(DBXParameter *) getParamByIndex: (int) index;
/**
 *  
 * @return the number of parameters
 */
-(int) size;
/**Returns  TParams as json object
 */
-(id) asJSONObject;

@end

