//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

#import "TStream.h"
#import "DBXTools.h"
@implementation TStream
+(id) streamWithJsonArray:(TJSONArray*) json{
	NSArray * jarr = [json asJSONArray];
	Byte* b =	[DBXTools arrayToByteArray:jarr];
	NSData *  str =	 [TStream streamWithBytes:b length: [jarr count]];
	free(b);
	return str;
}

+(id) streamWithBytes:(Byte *) barray length:(NSUInteger) len{

	return [NSData dataWithBytes:barray length:len];


}
+(TJSONArray *) StreamToJson:(NSData *)str{
	Byte * b = [DBXTools streamToByteArray: str];
	NSMutableArray * a=[NSMutableArray arrayWithCapacity:sizeof(b)]; 
	for (int i = 0; i < [str length]; i++){
		NSNumber * n = [NSNumber numberWithInt:b[i]];
		[a addObject:n];
		
		//NSMutableArray * arr  = [NSMutableArray arrayWithCapacity:0];
	}
	free(b);
	return [[[TJSONArray alloc] initWithJSONArray:a] autorelease];
}

@end
