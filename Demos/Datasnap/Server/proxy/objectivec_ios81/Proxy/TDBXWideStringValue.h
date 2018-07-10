//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

/**
 * 
 * @brief Wraps the WideString type and allows it to be null
 *
 */

#import <Foundation/Foundation.h>
#import "DBXValue.h"

@interface TDBXWideStringValue : DBXValue {
@protected bool ValueNull;
@private NSString *DBXStringValue;
}




@end
