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
 * @brief Wraps the {@link TDBXReader} and allows it to be null.
 */
@interface TDBXSingleValue : DBXValue {
@protected bool ValueNull;
@private float DBXInternalValue;
}


@end
