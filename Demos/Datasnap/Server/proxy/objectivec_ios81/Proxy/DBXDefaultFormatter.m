//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

#import "DBXDefaultFormatter.h"
#import "Base64.h"

NSString * const DATETIMEFORMAT = @"yyyy-MM-dd HH:mm:ss.SSS";
NSString * const DATEFORMAT = @"yyyy-MM-dd";
NSString * const TIMEFORMAT = @"HH:mm:ss.SSS";
NSInteger const JULIAN_JANUARY_01 =1721426;//Julian calendar days beetween 01 January 4713 and 01 January 01 JULIAN CALENDAR
NSInteger const JULIAN_1970 =2440588;//Julian calendar days beetween 01 January 4713 and 01 January 1970 JULIAN CALENDAR
NSInteger const SECONDSXDAY =86400;
int  const CURRENCYDECIMALPLACE =4;

@implementation DBXDefaultFormatter
@synthesize calendar = gregorian;
static DBXDefaultFormatter *istance = NULL;

-(id) init {
	self = [super init];
	if (self) {
		timeFormatter = [[NSDateFormatter alloc] init];
		dateFormatter = [[NSDateFormatter alloc] init];
		datetimeFormatter = [[NSDateFormatter alloc] init];
		numberFormatter  = [[NSNumberFormatter alloc] init];
				
		[datetimeFormatter setDateFormat:DATETIMEFORMAT];
		[datetimeFormatter setTimeZone:[NSTimeZone timeZoneWithAbbreviation:@"GMT"]];

		[dateFormatter setDateFormat:DATEFORMAT];
		[dateFormatter setTimeZone:[NSTimeZone timeZoneWithAbbreviation:@"GMT"]];

		[timeFormatter setDateFormat:TIMEFORMAT];
		[timeFormatter setTimeZone:[NSTimeZone timeZoneWithAbbreviation:@"GMT"]];

		locale  = [[NSLocale alloc] initWithLocaleIdentifier:@"en_US"];		
		
		[datetimeFormatter setLocale:locale];
		[dateFormatter setLocale:locale];
		[timeFormatter setLocale:locale];
		[numberFormatter setLocale:locale];
		
		[numberFormatter setMaximumFractionDigits: CURRENCYDECIMALPLACE];
		gregorian = [[NSCalendar alloc] initWithCalendarIdentifier:NSCalendarIdentifierGregorian] ;
		[gregorian setLocale:locale];
		[gregorian setTimeZone:[NSTimeZone timeZoneWithAbbreviation:@"GMT"]];

	}
	return self;
}

-(void) dealloc {
	istance = nil;
}

+(id) getInstance{
	@synchronized (self) {
		if (!istance){
			istance = [[DBXDefaultFormatter alloc] init] ;}
		
	}
	return istance    ;
}

-(NSString*)  AnsiStringToString:(NSString *) value { return [NSString stringWithString:   value]; }
-(NSString*) WideStringToString:(NSString*) value { return [NSString stringWithString:   value]; }	
-(NSString*) Int8ToString:(int )value { return [NSString stringWithFormat:@"%i", value]; }	
-(NSString*) Int16ToString:(int) value { return [NSString stringWithFormat:@"%i", value]; }	
-(NSString*) Int32ToString:(long) value { return [NSString stringWithFormat:@"%li", value]; }
-(NSString*) Int64ToString:(long long) value { return [NSString stringWithFormat:@"%qi", value]; }	
-(NSString*) UInt8ToString:(int) value { return [NSString stringWithFormat:@"%i", value]; }	
-(NSString*) UInt16ToString:(int) value { return [NSString stringWithFormat:@"%i", value]; }	
-(NSString*) UInt32ToString:(long) value { return [NSString stringWithFormat:@"%li", value]; }
-(NSString*) UInt64ToString:(long long) value { return [NSString stringWithFormat:@"%qi", value]; }
-(NSString *)BoolToString:(bool) value{return[ NSString stringWithFormat:@"%@", value ? @"True" : @"False"];}


-(NSString *)Base64Encode:(NSString *)value{
	
	return     [NSString encodeBase64WithString:value];
	
}
-(NSDate*) StringToDate: (NSString *) stringvalue{
	return [dateFormatter dateFromString:stringvalue] ;
}

-(NSDate *)StringtoTime: (NSString *) stringvalue{
	return [timeFormatter dateFromString:stringvalue];
	
}


-(double) StringToDouble:(NSString *) value {
	return [value doubleValue];
}



-(NSString *) doubleToString: (double) value{	
	return [numberFormatter stringFromNumber:[NSNumber numberWithDouble:value]];
}
-(NSString *) SingleToString: (double) value{	
	return  [NSString stringWithFormat:@"%f",value] ;
}


-(NSString *) TDBXTimeToString:(long) value{
	NSDateComponents *components = [[NSDateComponents alloc] 
									 init];
	value = value / 1000;
	[components setSecond:value];
	// create a calendar
	NSDate *dt = [gregorian dateFromComponents:components];

    NSDateFormatter *	tt =[[NSDateFormatter alloc]init];
	[tt setDateFormat:@"HH:mm:ss"];
	[tt setTimeZone:[NSTimeZone timeZoneWithAbbreviation:@"GMT"]];

    NSString * r = [tt stringFromDate:dt];
	return r;
}
-(long)gregorianDateToJulianDate:(int)DAY withMonth:(int) MONTH withYear:(int)YEAR{
	int JD,I,J,K;
	I= YEAR;
	J= MONTH;
	K= DAY;
	JD= K-32075+1461*(I+4800+(J-14)/12)/4+367*(J-2-(J-14)/12*12)
	/12-3*((I+4900+(J-14)/12)/100)/4;
	return JD+1;
	
}
-(long) DateToJulianDate: (NSDate *) value{
	NSString * s =   [dateFormatter stringFromDate:value];
	 NSArray * a =   [s componentsSeparatedByString:@"-"];
	NSInteger day =[[a objectAtIndex:2]intValue];
	NSInteger month =[[a objectAtIndex:1]intValue ];
	NSInteger year = [[a  objectAtIndex:0]intValue];
	return [self gregorianDateToJulianDate: day withMonth:month withYear: year];
	
	
}
-(NSDate *) JulianDateToGregorianDate:(long)jd{
	
	
	NSInteger j,y,m,d;
	j = jd;
	j = j - 1721119 ;
	y = (4 * j - 1) / 146097 ; j = 4 * j - 1 - 146097 * y ; d = j / 4 ;
	j = (4 * d + 3) / 1461 ; d = 4 * d + 3 - 1461 * j ; d = (d + 4) / 4 ;
	m = (5 * d - 3) / 153 ; d = 5 * d - 3 - 153 * m ; d = (d + 5) / 5 ;
	y = 100 * y + j ;
	if (m < 10) {
		m = m + 3;}
	else{
		m = m - 9 ; y = y + 1;
	}
	NSString* s = [NSString stringWithFormat:@"%04d-%02d-%02d",y,m,d];
	return [dateFormatter dateFromString:s];
	
}

-(NSTimeInterval)timeIntervalSince1582:(NSDate*) date{
	NSDate *dt = [datetimeFormatter dateFromString:@"1582-10-01 12:00:00.000"];	
	
	NSTimeInterval dtInterval =   [date timeIntervalSinceDate:dt ];
	return dtInterval;
}
-(NSTimeInterval) timeIntervalSince0001:(NSDate *)dt {
	NSDate *dt1970 = [datetimeFormatter dateFromString:@"1970-01-01 12:00:00.000"];	
	NSTimeInterval dtInterval1970 = [self timeIntervalSince1582:dt1970 ];
	NSTimeInterval dtInterval =   [self timeIntervalSince1582:dt ];
	//62135596800 is second til OCT 1852
	dtInterval = dtInterval + 62135596800-dtInterval1970;
	return dtInterval;
}


-(NSString *) TDBXDateToString: (long) value {
	if (value == 0) {value = 1; }
	return [self DateToString:  [self JulianDateToGregorianDate:value+JULIAN_JANUARY_01-1]];
	
}

-(int) StringToTDBXTime: (NSString *) value {	
  
    NSDateFormatter *tt = [[NSDateFormatter alloc]init];
	[tt setTimeZone:[NSTimeZone timeZoneWithAbbreviation:@"GMT"]];

	[tt setDateFormat:@"HH:mm:ss"];
	NSDate * dt = [tt dateFromString:value ];
	
	NSDateComponents *h = [gregorian components:NSCalendarUnitHour
										   fromDate:dt];	
	NSDateComponents *m = [gregorian components:NSCalendarUnitMinute
										   fromDate:dt];	
    NSDateComponents *s = [gregorian components:NSCalendarUnitSecond
										   fromDate:dt];		
	return ([h hour]*3600+ [m minute]* 60 + [s second])*1000;
	
}

-(long) StringToTDBXDate:(NSString *) value{
	
	NSString * dtValue = [NSString stringWithFormat:@"%@ 12:00:00.000",value];
	NSDate * endDate = [datetimeFormatter dateFromString:dtValue ];
	
	NSTimeInterval  endDateInterval = [self timeIntervalSince1582:endDate];
	if (endDateInterval < 0) {
		//do conversion from juliandate to gregorian
		endDateInterval= [self DateToJulianDate: endDate]-JULIAN_JANUARY_01;
		
	}else {
		endDateInterval = [self timeIntervalSince0001:endDate];
		endDateInterval = (endDateInterval + SECONDSXDAY)/SECONDSXDAY;
		
	}
	return endDateInterval ;
	
}

-(NSString *) floatToString: (float) value{
	return [NSString stringWithFormat:@"%f",value];
}
-(NSString *) currencyToString:(double) value {
	[numberFormatter setNumberStyle:NSNumberFormatterDecimalStyle ];
	NSNumber * num =[NSNumber numberWithDouble:value];
	NSString * s =[numberFormatter stringFromNumber:num]; 
	
	return s;

}

-(NSString *) DateTimeToString:(NSDate*) value{
	return [datetimeFormatter stringFromDate:value];	 
}

-(NSString *) TimeToString:(NSDate*) value{
	return [timeFormatter stringFromDate:value];
}
-(NSString *) TimeToStringWOms:(NSDate*) value{
	NSDateFormatter * df =[[NSDateFormatter alloc]init];
	[df setDateFormat:@"HH:mm:ss"];
	[df setTimeZone:[NSTimeZone timeZoneWithAbbreviation:@"GMT"]];
    [df setLocale:locale];
	@try {
		return [df stringFromDate:value];
	}
	@finally {
		df = nil;
	}
}

-(NSString*) Datetostring:(NSDate *) value{
return [datetimeFormatter stringFromDate:value];
}
-(NSDate *)StringToDateTime:(NSString *) value{
	return [datetimeFormatter dateFromString:value];
}
-(NSString *) DateToString:(NSDate *) value{
	return [dateFormatter stringFromDate:value];
}
-(double) stringToCurrency:(NSString *) value{
	 NSNumber * nm = 
	 [numberFormatter numberFromString:value ];
	double Num =[ nm doubleValue];
    return   Num;
}
@end
