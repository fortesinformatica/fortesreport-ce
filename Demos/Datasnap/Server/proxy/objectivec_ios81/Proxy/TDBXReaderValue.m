//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

#import "TDBXReaderValue.h"



@implementation TDBXReaderValue
-(id) init {
	self = [super init];
	if (self) {
		[self setDBXType:TableType];
		ValueNull =  NO;
	}
	return self;
}
-(void) SetNull {
	ValueNull = YES;
	objectValue = nil;
}

-(bool) isNull {
	return ValueNull;
}


-(void) SetAsTable:(id<TableType>)value{
	objectValue = value;
	[self setDBXType:TableType];}


-(id<TableType>) GetAsTable{
	[self checkCurrentDBXType:TableType];
	return objectValue ;
}

-(TDBXReader *) GetAsDBXReader {
	return (TDBXReader *) objectValue;
}

-(void) SetAsDBXReader:(id<TableType>) value {
	[self SetAsTable:value];
}

@end
