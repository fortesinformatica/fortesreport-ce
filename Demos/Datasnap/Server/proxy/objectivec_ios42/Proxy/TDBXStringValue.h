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
 * @brief Wraps the Single type and allows it to be null
 *
 */
@interface TDBXStringValue : DBXValue {
@protected bool ValueNull;
@private NSString *DBXStringValue;
}



@end
