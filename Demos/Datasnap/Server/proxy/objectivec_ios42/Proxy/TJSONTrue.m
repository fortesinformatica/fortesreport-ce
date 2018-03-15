//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

#import "TJSONTrue.h"


@implementation TJSONTrue
-(NSString *) asJSONString{
 return @"true";
}
-(id) getInternalObject {
	return [NSNumber numberWithBool:YES];
}
-(NSString *) toString{
	return [self asJSONString];
}
-(JSONValueType) getJSONValueType{
	return JSONTrue;
}

@end
