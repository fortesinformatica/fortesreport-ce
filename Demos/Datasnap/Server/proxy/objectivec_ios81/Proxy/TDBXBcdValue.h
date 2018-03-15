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
 * @brief Wraps a binary-compressed decimal values and allows it to be null.
 *
 */

@interface TDBXBcdValue : DBXValue {
	bool valueNull;
	double bcdValue;
	
}

@end
