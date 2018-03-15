//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

#import <Foundation/Foundation.h>
#import "TJSONPair.h"

/**
 *@brief Handles a JSONPair list. It is an array of JsonPair objects.
 */
@interface TJSONPairList : NSObject {
	NSMutableArray * internalArray;
}
/**
 * Add a TJSONPair Object.
 * @param value the TJSONPair object to add
 * @return the index of the pair added in the list.  
 */
-(int) addPair:(TJSONPair *)value;
/**
 * Searches a pair by its index
 * @param value pair index to get
 * @return it retuns the TJSONPair object or null  
 */
-(TJSONPair *) PairByIndex:(int) value;
/**
 * Searches a pair by its name
 * @param  value pair name to get
 * @return it retuns the TJSONPair object or null  
 */
-(TJSONPair *) PairByName:(NSString *) value;

/**
 * Removes a pair by its index
 * @param  pair index to remove
 */
-(void) removePairByIndex:(int) value;
/**
 * Removes a pair by its name
 * @param  pair name to remove
 */
-(void) removePairByName:(NSString*) value;
/**
 * Finds a pair using it name 
 * @param value   the pair name to search
 * @return the index position of the pair in the list 
 */
-(int) findPairByName:(NSString *) value;
/**
 *The number of elements in the list
 *@return the number of elements in the list 
 **/
-(int) count;
/**
 * Get the internal list as a JSONObject
 * @return a JSONObject 
 */
-(id) asJSONObject ;

@end
