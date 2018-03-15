//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

using System;
using System.Globalization;
using System.Text;

namespace Embarcadero.Datasnap.WindowsPhone7
{

    /** 
     * Contains methods to represent a TDBX type to a string format and vice versa.
     */
    
    class DBXDefaultFormatter {
	  
	    public const String DATETIMEFORMAT = "yyyy-MM-dd HH:mm:ss.fff";
	    public const String DATEFORMAT = "yyyy-MM-dd";
	    public const String TIMEFORMAT = "HH:mm:ss.fff";
        public const String TIMEFORMAT_WO_MS = "HH:mm:ss";
	    public const int CURRENCYDECIMALPLACE = 4;
	    private static DBXDefaultFormatter instance;
        private static CultureInfo Culture;

	    public static DBXDefaultFormatter getInstance() {
		    if (instance == null)
			    instance = new DBXDefaultFormatter();
		    return instance;
	    }

	    private DBXDefaultFormatter() : base() {
            System.Threading.Thread.CurrentThread.CurrentCulture =
                new System.Globalization.CultureInfo("en-US");
            Culture = CultureInfo.InvariantCulture;
	    }

   /**
	 * create a Date from a string 
	 * @param StringValue
	 * @return 
	 */
	    public DateTime StringToDate(String StringValue)
        {		
		    return DateTime.ParseExact(StringValue, DATEFORMAT, Culture);
	    }

    /**
	 * create a Time from a string
	 * @param StringValue
	 * @return
	 */
	    public DateTime StringToTime(String StringValue)
        {
		    return DateTime.ParseExact(StringValue, TIMEFORMAT, Culture);
	    }

	    /**
	     * format a double as a string with max allowed decimal positions 
	     * @param value
	     * @return The formatted string
	     */
	    public String doubleToString(double value) {
            return Convert.ToString(value);
		}

	    /**
	     * format a string as a base64string
	     * @param value
	     * @return The base64 string
	     */
	    public String Base64Encode(String value) {
		    return Base64.encode(value);
	    }	
	
	
	    /**
	     * format a TDBXTime as a string
	     * @param value
	     * @return The formatted string
	     */
	    public string TDBXTimeToString(int value) {
            TimeSpan ts = TimeSpan.FromMilliseconds(value);
		    DateTime d = new DateTime(1,1,1,0,0,0,0) + ts;
            return d.ToString(TIMEFORMAT);
	    }

	    /**
	     * format a TDBXDate as a string
	     * @param value
	     * @return The formatted string
	     */
	    private const long MILLISECONDSINADAY = 1000 * 60 * 60 * 24;

	    private String multiplyString(String c, int howManyTimes) {
		    if (howManyTimes == 0)
			    return "";
            StringBuilder sb = new StringBuilder();
		    for (int i = 0; i < howManyTimes; i++)
			    sb.Append(c);
		    return sb.ToString();
	    }


     	/**
	     * format a TDBXDate as a string 
	     * @param value
	     * @return The formatted string
	     */
        public string TDBXDateToString(int d)
        {
            TimeSpan ts = TimeSpan.FromDays(d);
            DateTime dt = DateTime.MinValue + ts - TimeSpan.Parse("1");
            return dt.ToString(DATEFORMAT);
        }

	    /**
	     * create a TDBXTime (int) from a string
	     * @param value
	     * @return The int value
	     */
	    public int StringToTDBXTime(String value) {
		    String[] parts = value.Split(':');
		    return (Int32.Parse(parts[0]) * 3600000)
                    + (Int32.Parse(parts[1]) * 60000)
                    + (Int32.Parse(parts[2]) * 1000);
	    }

	    /**
	     * create a TDBXDate (int) from a string
	     * @param value
	     * @return The int value
	     */
        public int StringToTDBXDate(string s)
        {
            String[] parts = s.Split('-');
            int yyyy = Int32.Parse(parts[0]);
            int mm = (Int32.Parse(parts[1]));
            int dd = (Int32.Parse(parts[2]));
            DateTime d = new DateTime(yyyy, mm, dd);
            return Convert.ToInt32(((TimeSpan)(d - DateTime.MinValue + TimeSpan.Parse("1"))).TotalDays);
        }

	    /**
	     * format a float as a string with max allowed decimal positions
	     * @param value
	     * @return The formatted string
	     */
	    public String floatToString(float value) {
            return value.ToString("R");
	    }

	    /**
	     * format a double as a string with only 4 decimal position
	     * @param value
	     * @return The formatted string
	     */
	    public String currencyToString(double value)
        {
            return String.Format("{0:0.0000}", value);
	    }

	    private CultureInfo GetCulture() {
		    return Culture;
	    }

      	/**
	       * convert a String to Double 
	       * @param value
      	 * @return
	      */
	 
	    public double StringToDouble(String value) {
            return Double.Parse(value);
	    }

      	/**
	       * Convert a DateTime to String 
	       * @param dateValue
	       * @return
	      */
	    public String DateTimeToString(DateTime dateValue) {
            return dateValue.ToString(DATETIMEFORMAT);
	    }

      /**
    	 * Convert a Date to String
    	 * @param dateValue
    	 * @return
    	 */
   	
	    public String DateToString(DateTime dateValue) {
            return dateValue.ToString(DATEFORMAT);
	    }

      /**
    	 * Convert a Time to String including milliseconds 
    	 * @param timeValue
    	 * @return
    	 */
      	 
	    public String TimeToString(DateTime dateValue) {
            return dateValue.ToString(TIMEFORMAT);
	    }

       /**
    	 * Convert a Time to String without milliseconds 
    	 * @param timeValue
    	 * @return
    	 */
       
       public String TimeToStringWOms(DateTime dateValue)
        {
            return dateValue.ToString(TIMEFORMAT_WO_MS);
        }

        /**
      	 * Create a DateTime from a String 
      	 * @param value
      	 * @return
      	 */
	    public DateTime StringToDateTime(String value)
        {
            value = value.PadRight(23,'0');
            return DateTime.ParseExact(value, DATETIMEFORMAT, CultureInfo.InvariantCulture);
	    }
	
	    /**
    	 * Returns the string representation of the AnsiString argument. 
    	 * @param value
    	 * @return String
    	 */
	    public String AnsiStringToString(String value) { return value; }
	   
	    /**
    	 * Returns the string representation of the WideString argument. 
    	 * @param value
    	 * @return String
    	 */
	    public String WideStringToString(String value) { return value; }
    
    	/**
    	 * Returns the string representation of the int argument. 
    	 * @param value
    	 * @return the string representation.
    	 */
	    public String Int8ToString(int value) { return value.ToString(); }
	   
	   	/**
    	 * Returns the string representation of the int16 argument. 
    	 * @param value
    	 * @return String
    	 */
        public String Int16ToString(int value) { return value.ToString(); }
    
    
    	/**
    	 * Returns the string representation of the int32 argument. 
    	 * @param value
    	 * @return String
    	 */
        public String Int32ToString(int value) { return value.ToString(); }
    
    	/**
    	 * Returns the string representation of the int64 argument. 
    	 * @param value
    	 * @return String
    	 */
        public String Int64ToString(long value) { return value.ToString(); }
    
    	/**
    	 * Returns the string representation of the UInt8 argument. 
    	 * @param value
    	 * @return String
    	 */
        public String UInt8ToString(int value) { return value.ToString(); }
    
    	/**
    	 * Returns the string representation of the UInt16 argument. 
    	 * @param value
    	 * @return String
    	 */
        public String UInt16ToString(int value) { return value.ToString(); }
    
    	/**
    	 * Returns the string representation of the Uint32 argument. 
    	 * @param value
    	 * @return String
    	 */
        public String UInt32ToString(long value) { return value.ToString(); }
    
    	/**
    	 * Returns the string representation of the Uint64 argument. 
    	 * @param value
    	 * @return String
    	 */
        public String UInt64ToString(long value) { return value.ToString(); }


       /**
    	 * try to convert a Object to a String 
    	 * @param value
    	 * @return
    	 */
	    public String tryObjectToString(Object value) {
		    if (value == null)
			    return "null";
		    if (value is String)
			    return (String)value;		
		    //AtomicInteger, AtomicLong, BigDecimal, BigInteger, Byte, Double, Float, Integer, Long, Short
			if (value is Double)
			    return doubleToString((Double)value);
			if (value is float)
			    return floatToString((float)value);
            if (value is long)
                return Int64ToString((long)value);
		    if (value is DateTime)
			    return DateTimeToString((DateTime)value);
		    if (value is Boolean)
			    return booleanToString((Boolean)value);
		    return "unsupportedtype(" + value.GetType().FullName+")";
	    }


      /**
    	 * Convert a Boolean to String 
    	 * @param booleanValue
    	 * @return string
    	 */
	 
	    public String booleanToString(bool booleanValue) {
		    return booleanValue?"true":"false";
	    }
	
	
    	/**
    	 * Convert a String to Boolean value 
    	 * @param stringValue
    	 * @return boolean value
    	 */
	    public bool stringToBoolean(String stringValue)
        {
            if (stringValue.Equals("true", StringComparison.InvariantCultureIgnoreCase))
			    return true;
            if (stringValue.Equals("false", StringComparison.InvariantCultureIgnoreCase))
			    return false;		
		    throw new DBXException("[" + stringValue + "] is not a valid boolean");
	    }	
	
    }
}
