//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

#import <Foundation/Foundation.h>
#import "DBXValue.h"

/**
 * 
 * @brief Wraps the Int8 type and allows it to be null
 *
 */

@interface TDBXInt16Value : DBXValue {
@protected bool ValueNull;
@private  int DBXInternalValue;
}



@end
