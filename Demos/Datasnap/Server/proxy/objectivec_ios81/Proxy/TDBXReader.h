//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

#import <Foundation/Foundation.h>
#import "DBXProtocols.h"
#import "TParams.h"
#import "DBXValue.h"
#import "TJSONObject.h"
/**
 * 
 * @brief TDBXReader provides a unidirectional reader for a collection of database
 * rows.
 * 
 */
@interface TDBXReader :NSObject< TableType,JSONSerializable>  {
@private
	TParams * columns;
	id internalDataStore;
@protected
	long currentPosition ;
	
}

/**
 * The list of parameters.
 */
@property (nonatomic,strong) TParams * columns;
/**
 *  the interna data stored.
*/
@property (nonatomic,strong) id internalDataStore;
/**
 *  Initialiaze the TDBXReaer with a list of params and a json object that holds the values.
 */
-(id) initWithParams: (TParams * )params andJSONObject: (TJSONObject *)json;

/**
*  Get the value by index
*  @param the index fo the parameter.
*  @return the parameter 
*/
-(DBXWritableValue *) getValueByIndex: (int) index;
/**
*  Get the value by name
*  @param the name fo the parameter.
*  @return the parameter 
*/
-(DBXWritableValue *) getValueByName: (NSString *) name;
/**
 * Move to the next value
 * @return NO if no others value founded or is empty. 
 */
-(bool) next;
/**
 * Resets the TDBXReader object
 */
-(void) reset;

@end
@interface TDBXReader (TDBXReaderCreation)
/**
 * Builds a TDBXreader using parames and and a json object
 * @param params is the list of params
 * @param json the json object with the information 
 * @return a TDBXReader  
 */
+(id) DBXReaderWithParams: (TParams * )params andJSONObject: (TJSONObject *)json;

/**
 * Builds a TDBXreader using a json object
 * @param json the json object with the information
 * @return a TDBXReader  
 */
+(id) DBXReaderWithJSON:(TJSONObject *) value;

@end
