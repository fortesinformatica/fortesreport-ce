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
 * @brief Wraps the {@link TStream} type and allows it to be null
 *
 */
@interface TDBXStreamValue : DBXValue {
	BOOL ValueNull;
}

@end
