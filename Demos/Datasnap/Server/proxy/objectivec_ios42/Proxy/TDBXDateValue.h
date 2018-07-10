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
 * @brief Wraps the Date type and allows it to be null
 *
 */

@interface TDBXDateValue : DBXValue {
@protected bool ValueNull;
@private long DBXInternalValue;
}

@end
