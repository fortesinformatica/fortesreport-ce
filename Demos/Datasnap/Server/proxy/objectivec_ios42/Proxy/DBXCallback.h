//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

#import <Foundation/Foundation.h>
#import "TJSONArray.h"
 /**
  * @brief Base class to implement callbacks.  The class is used to implement
  * responses for callbacks.  
  */   
@interface DBXCallback : NSObject {

}
/**
 *  override this method to implement callbacks actions
 *  @param value [in] the jsonvalue returned by the server
 *  @param jsonType  the type of the value returned 
 *  The first array element holds the real information, the second holds 1 if the server has sent an object in this case the json has a certain kind of format and 2 otherwise.  
 *  @param [out] it may returns a TJSONValue 
 */ 
-(TJSONValue *) execute:(TJSONValue *) value andJSONTYPE:(int)jsonType;

@end
