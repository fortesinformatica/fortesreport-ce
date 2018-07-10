//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

#import <Foundation/Foundation.h>

extern NSString * const DATETIMEFORMAT;
extern NSString * const DATEFORMAT;
extern NSString * const TIMEFORMAT;
extern int  const CURRENCYDECIMALPLACE;

/**
 *  @brief Utility methods to format dates, time and numbers using 
 *  the following format strings as shown by the costants below:
 *   
 *  DATETIMEFORMAT = @"yyyy-MM-dd HH:mm:ss.SSS"
 *  DATEFORMAT = @"yyyy-MM-dd" 
 *  TIMEFORMAT = @"HH:mm:ss.SSS"
 *  CURRENCYDECIMALPLACE =4  
 *   
 */ 
@interface DBXDefaultFormatter : NSObject {

	
	NSCalendar *gregorian;  //internal gregoria calendar
	NSDateFormatter *timeFormatter; //time formatter
	NSDateFormatter *dateFormatter; //Date formatter
	NSDateFormatter *datetimeFormatter; //DateTme formatter
	NSNumberFormatter * numberFormatter;//Number Formatter
	NSLocale *locale;
	
	
}
/**
 * Interna calendar used to format and on calc for dates and time
 */
@property (nonatomic,readonly)NSCalendar *calendar;
/*
  singleton method to return a single istance for the formatter
*/
+ (id) getInstance;

/**
 *Converts a date formatted string into date
 *@param stringValue [in]  date formatted string
 *@return an NSDate   containing the date
*/
-(NSDate*) StringToDate: (NSString *) stringvalue;

/**
 * Converts a time formatted string into time 
   @param StringValue [in]  time formatted string
   @return an NSDate containg the time
*/ 
-(NSDate *)StringtoTime: (NSString *) stringvalue;

/**
 * Formats a double  into a string
 * 
 * @param value [in] the double value to convert
 * @return a double formatted string
 */
-(NSString *) doubleToString: (double) value;

/**
 * format a float as a string with max allowed decimal positions
 * 
 * @param value [in] the double to convert
 * @return a float formatted string
 */
-(NSString *) floatToString: (float) value;
/**
 * Formats a TDBXTime into  a string
 * 
 * @param [in] value to convert
 * @return a time formatted string
 */
-(NSString *) TDBXTimeToString: (long) value;
/**
 * Formats a TDBXDate as a string
 * 
 * @param value [in] a TDBXTime value
 * @return a date formatted string
 */
-(NSString *) TDBXDateToString: (long) value ;
/**
 * Converts a time formatted string into a TDBXTime 
 * 
 * @param value [in]  the string to convert
 * @return a TDBXTime
 */
-(int) StringToTDBXTime: (NSString *) value;
/**
 * Converts a date formatted string into a TDBXDate
 * 
 * @param value [in]  Time string value
 * @return a TDBXDate
 */
-(long) StringToTDBXDate:(NSString *) value;

/**
 * Converts a float value into a string
 * 
 * @param value [in]  float value to convert
 * @return a float formatted string
 */
//-(NSString *) floatToString: (float) value;

/**
 * Converts a currency value into a string
 * 
 * @param value [in]  float value to convert
 * @return a currency formatted string
 */
-(NSString *) currencyToString:(double) value;

/**
 * Converts a DateTime value into a string
 * 
 * @param value [in]  DateTime value to convert
 * @return a datetime formatted string
 */
-(NSString *) DateTimeToString:(NSDate*) value;

/**
 * Converts a Date value into a string
 * 
 * @param value [in]  Date value to convert
 * @return a date formatted string
 */
-(NSString *) DateToString:(NSDate*) value;

/**
 * Converts a Time value into a string
 * 
 * @param value [in]  time value to convert
 * @return a time formatted string
 */
-(NSString *) TimeToString:(NSDate*) value;
/**
 * Converts a time value into a string without seconds
 * 
 * @param value [in]  time value to convert
 * @return a time formatted string
 */
-(NSString *) TimeToStringWOms:(NSDate*) value;

/**
 * Converts a Date formatted string into a DateTime
 * @param [in] datetime formatted strign to convert
 * @return an NSDate with the date and time  
 **/ 
-(NSDate *)StringToDateTime:(NSString *) value;

/**
 * Converts a currency formatted string into a DateTime
 * @param [in] currency formatted strign to convert
 * @return an currency value  
 **/
-(double) stringToCurrency:(NSString *) value;

/**
 * Converts a  string into using Base64 
 * @param [in] string to convert
 * @return a base64 string  
 **/   
-(NSString *)Base64Encode:(NSString *)value;

/**
 * Converts an  AnsiString into a string 
 * @param [in] an ansistring
 * @return a string 
 **/  
-(NSString*)  AnsiStringToString:(NSString *) value ;

/**
 * Converts an  WideString into a string 
 * @param [in] an WideString  value
 * @return a string 
 **/ 
-(NSString*) WideStringToString:(NSString*) value ;

/**
 * Converts an  Int8 into a string 
 * @param [in] an int8 value
 * @return a string 
 **/ 
-(NSString*) Int8ToString:(int )value ;

/**
 * Converts an  Int16 into a string 
 * @param [in] an Int16 value
 * @return a string 
 **/ 
-(NSString*) Int16ToString:(int) value ;
/**
 * Converts an  Int32 into a string 
 * @param [in] an Int32 value
 * @return a string 
 **/ 
-(NSString*) Int32ToString:(long) value ;
/**
 * Converts an  Int64 into a string 
 * @param [in] an Int64 value
 * @return a string 
 **/ 
-(NSString*) Int64ToString:(long long) value ;
/**
 * Converts an  UInt8 into a string 
 * @param [in] an UInt8 value
 * @return a string 
 **/ 
-(NSString*) UInt8ToString:(int) value ;

/**
 * Converts an  UInt16 into a string 
 * @param [in] an UInt16 value
 * @return a string 
 **/ 
-(NSString*) UInt16ToString:(int) value ;

/**
 * Converts an  UInt32 into a string 
 * @param [in] an UInt32 value
 * @return a string 
 **/ 
-(NSString*) UInt32ToString:(long) value ;

/**
 * Converts an  UInt64 into a string 
 * @param [in] an UInt64
 * @return a string 
 **/ 
-(NSString*) UInt64ToString:(long long) value ;
/**
 * Converts an  Boolean to string 
 * @param [in] a boolean value
 * @return a string with YES OR NO 
 **/ 
-(NSString*) BoolToString:(bool) value;
/**
 * Converts an  double into a string 
 * @param [in] an double value
 * @return a string 
 **/ 
-(double) StringToDouble:(NSString *) value;
						   

@end
