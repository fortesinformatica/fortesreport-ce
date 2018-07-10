//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

#import <Foundation/Foundation.h>
#import "DBXValue.h"
#import "TDBXReader.h"
/**
 *@brief  Wraps the {@link TDBXReader} and allows it to be null.
 */

@interface TDBXReaderValue : DBXValue {
	BOOL ValueNull ;
}
-(TDBXReader *) GetAsDBXReader;
-(void) SetAsDBXReader:(id<TableType>) value;
@end
