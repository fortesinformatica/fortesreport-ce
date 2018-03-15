//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

/**
 * 
 * @brief Wraps the Boolean type and allows it to be null
 *
 */

#import <Foundation/Foundation.h>
#import "DBXValue.h"

@interface TDBXBooleanValue : DBXValue {
@protected bool ValueNull;
@private bool DBXBoolValue;
}


@end



