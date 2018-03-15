//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

/**
 * 
 * @brief Wraps the Int8 type and allows it to be null
 *
 */
#import <Foundation/Foundation.h>
#import "DBXValue.h"



@interface TDBXInt32Value : DBXValue {
@protected bool ValueNull;
@private long DBXInternalValue;
}


@end
