//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

#import <Foundation/Foundation.h>
#import "TJSONArray.h"

 /**
 * @brief Utitily methods to convert a stream to and from  json object
*/
@interface TStream : NSData {

}
/* 
  Creates a Stream from a json array (NSArray)
  @param json an NSArray rappresenting the json array
  @return a stream with the values from the json array
*/

+(id) streamWithJsonArray:(TJSONArray*) json;

/* 
  Creates a Stream from a an array of bytes
  @param barray the array of bytes 
  @param len the length of the bytes array
  @return a stream 
*/  
+(id) streamWithBytes:(Byte *) barray length:(NSUInteger) len;

/* 
  Converts a Stream into a formatted json array 
  @param str the stream to convert
  @return returns a NSArray  (Jsonarray)
*/

+(TJSONArray *) StreamToJson:(NSData *)str;

@end
