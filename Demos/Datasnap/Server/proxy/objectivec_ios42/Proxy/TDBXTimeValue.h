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
 * @brief Wraps the Time type and allows it to be null
 *
 */
@interface TDBXTimeValue : DBXValue {
@protected bool ValueNull;
@private long DBXInternalValue;
}


@end
