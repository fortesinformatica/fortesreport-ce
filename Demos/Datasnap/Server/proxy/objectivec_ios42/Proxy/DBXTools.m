//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

#import "DBXTools.h"


@implementation DBXTools
+(Byte *)streamToByteArray: (NSData *)str{

	NSUInteger len =  [str length];
	Byte *byteData = (Byte*)malloc(len);
	memcpy(byteData, [str bytes], len);
	return byteData;	
}
+(Byte *) arrayToByteArray:(NSArray *) a{

	
 NSUInteger len =  [a count];
 Byte *byteData = (Byte*)malloc(len);
	int i =0;
	
	for( NSNumber* n in a){
		byteData[i]= [n intValue];
		i++;
	}	
	return byteData;
} 
/**
 * 
 * @param is the input stream
 * @return a String that represents the param Stream 
 * @throws IOException
 */
+(NSString *) convertStreamToString:(NSData *) is  {
	if (is) {
		return [[[NSString alloc] initWithData:is encoding:NSUTF8StringEncoding]autorelease];
	}
    return @"";

}


@end
