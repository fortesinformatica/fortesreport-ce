//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

#import "TJSONValue.h"


@implementation TJSONValue


- (NSString *) nullString{
	 return @"null";
}

-(id) getInternalObject{
	return nil;	
}

-(NSArray*) asJSONArray{
	return nil;
}

-(NSString *) asJSONString{
	return nil;
}
-(NSDictionary *) asJSONObject{
	return nil;
}
-(NSString *) toString{
	return [self nullString];

}
-(JSONValueType) getJSONValueType{
	return JSONNull;
}
@end
