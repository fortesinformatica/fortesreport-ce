//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

#import <Foundation/Foundation.h>


#import <Foundation/Foundation.h>
#import "DBXValue.h"


/**
 * 
 * @brief Wraps the UInt16 type and allows it to be null
 *
 */
@interface TDBXUInt16Value : DBXValue {
@protected bool ValueNull;
@private  unsigned int DBXInternalValue;
}



@end
