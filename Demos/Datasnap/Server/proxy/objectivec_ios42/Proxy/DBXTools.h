//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

#import <Foundation/Foundation.h>
/**
 *  @brief Utility function to work with DBX
 **/ 

@interface DBXTools : NSObject {

}

/**
 *  Converts a Stream into am array of byte for json rappresentation 
 *  @param str [in] Strema to convert
 *  @return returns the stream in bytes 
 */
+(Byte *)streamToByteArray: (NSData *)str;

/**
 *  Converts an NSarray in an array of bytes
 *  @param a [in] 
 *  @return an array of bytes 
 */
+(Byte *) arrayToByteArray:(NSArray *) a;

/**
 *  Converts a stream into into a string 
 *  @param is the stream to convert
 *  @return  stream string rappresentation 
 */

+(NSString *) convertStreamToString:(NSData *) is ;

@end
