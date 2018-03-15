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
 *@brief  Wraps the Double type and allows it to be null
 *
 */
@interface TDBXDoubleValue : DBXValue {
@protected bool ValueNull;
@private double DBXInternalValue;
}


@end

