//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

#import <Foundation/Foundation.h>
#import "TDBXReader.h"
/**
 * 
 * @brief used to label the types that should be managed as a table
 *
 */


@interface TDataSet : TDBXReader {

}


@end
@interface TDataSet (TDatasetCreation)
/**
 *  Returns a TDataSet created by the information contained in the JSONObject
 * 
 * @param value a JSONObject that contains the parameters for create the TDataSet  
 * @param params a TParams wich contains the parameters to create the TDataset
 * @return return the TDataSet object created

 */
+(id) DataSetWithParams: (TParams * )params andJSONObject: (TJSONObject* )json;
/**
 *  Returns a TDataSet created by the information contained in the JSONObject
 * 
 * @param value a JSONObject that contains the parameters for create the TDataSet  
 * @return return the TDataSet object created
 
 */

+(id) DataSetWithJSON:(TJSONObject *) value;
@end