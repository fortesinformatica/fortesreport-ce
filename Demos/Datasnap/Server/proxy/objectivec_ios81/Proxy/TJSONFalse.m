//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

#import "TJSONFalse.h"


@implementation TJSONFalse
-(NSString *) asJSONString{
	return @"false";
}
-(id) getInternalObject {
		return [NSNumber numberWithBool:FALSE];
}
-(NSString *) toString{
	return [self asJSONString];
}
-(JSONValueType) getJSONValueType{
	return JSONFalse;
}

@end
