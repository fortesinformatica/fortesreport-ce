//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

package com.embarcadero.javaandroid;

import java.text.DecimalFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.Formatter;
import java.util.GregorianCalendar;
import java.util.Locale;
import java.util.TimeZone;

import android.text.format.DateFormat;

/**
 * Contains methods to represent a TDBX type to a string format and vice versa.
 */

public class DBXDefaultFormatter {
	public static final String DATETIMEFORMAT = "yyyy-MM-dd HH:mm:ss.SSS";
	public static final String DATEFORMAT = "yyyy-MM-dd";
	public static final String TIMEFORMAT_MS = "HH:mm:ss.SSS";
	public static final String TIMEFORMAT_WO_MS = "HH:mm:ss";
	public static final String ANDROID_TIMEFORMAT_WO_MS = "kk:mm:ss";
	public static final int CURRENCYDECIMALPLACE = 4;
	private static DBXDefaultFormatter instance;
	private static SimpleDateFormat timeFormatterWOms;
	private static SimpleDateFormat timeFormatterms;
	private static SimpleDateFormat dateFormatter;
	private static SimpleDateFormat datetimeFormatter;
	private static Locale locale;

	public static DBXDefaultFormatter getInstance() {
		if (instance == null)
			instance = new DBXDefaultFormatter();
		return instance;
	}

	private DBXDefaultFormatter() {
		super();
		locale = Locale.US;
		timeFormatterms = new SimpleDateFormat(TIMEFORMAT_MS);
		timeFormatterWOms = new SimpleDateFormat(TIMEFORMAT_WO_MS);
		dateFormatter = new SimpleDateFormat(DATEFORMAT);
		datetimeFormatter = new SimpleDateFormat(DATETIMEFORMAT);
	}

	/**
	 * create a Date from a string
	 * 
	 * @param StringValue
	 * @return
	 * @throws ParseException
	 */
	public Date StringToDate(String StringValue) throws ParseException {
		return dateFormatter.parse(StringValue);
	}

	/**
	 * create a Time from a string
	 * 
	 * @param StringValue
	 * @return
	 * @throws ParseException
	 */
	public Date StringToTime(String StringValue) throws ParseException {
		return timeFormatterms.parse(StringValue);
	}

	/**
	 * format a double as a string with max allowed decimal positions
	 * 
	 * @param value
	 * @return The formatted string
	 */
	public String doubleToString(double value) {
		DecimalFormat decimalFormat = new DecimalFormat(
				"###############.###############");
		return decimalFormat.format(value);
	}

	/**
	 * format a string as a base64string
	 * 
	 * @param value
	 * @return The base64 string
	 */
	public String Base64Encode(String value) {
		return Base64.encode(value);
	}

	/**
	 * format a TDBXTime as a string
	 * 
	 * @param value
	 * @return The formatted string
	 */
	public String TDBXTimeToString(int value) {
		Calendar c = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
		Date d = new Date(value);
		c.setTime(d);
		return DateFormat.format(ANDROID_TIMEFORMAT_WO_MS, c).toString();
	}

	private static final long MILLISECONDSINADAY = 1000 * 60 * 60 * 24;

	/**
	 * format a TDBXDate as a string
	 * 
	 * @param value
	 * @return The formatted string
	 */
	public String TDBXDateToString(int value) {
		GregorianCalendar gcal = (GregorianCalendar) GregorianCalendar
				.getInstance(getLocale());
		gcal.setGregorianChange(new Date(Long.MIN_VALUE));
		gcal.clear();
		gcal.set(Calendar.YEAR, 1);
		gcal.set(Calendar.MONTH, 0);
		gcal.set(Calendar.DAY_OF_MONTH, 1);
		gcal.add(Calendar.DATE, value - 1);
		SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");
		sdf.setCalendar(gcal);
		String s = sdf.format(gcal.getTime());
		return s;
	}

	/**
	 * create a TDBXTime (int) from a string
	 * 
	 * @param value
	 * @return The int value
	 */
	public int StringToTDBXTime(String value) {
		String[] parts = value.split(":");
		return (Integer.valueOf(parts[0]).intValue() * 3600000)
				+ (Integer.valueOf(parts[1]).intValue() * 60000)
				+ (Integer.valueOf(parts[2]).intValue() * 1000);
	}

	/**
	 * create a TDBXDate (int) from a string
	 * 
	 * @param value
	 * @return The int value
	 */
	public int StringToTDBXDate(String value) {
		String[] parts = value.split("-");
		GregorianCalendar cal = (GregorianCalendar) GregorianCalendar
				.getInstance(getLocale());
		cal.setGregorianChange(new Date(Long.MIN_VALUE));
		cal.clear();
		cal.set(Calendar.YEAR, Integer.parseInt(parts[0]));
		cal.set(Calendar.MONTH, Integer.parseInt(parts[1]) - 1);
		cal.set(Calendar.DAY_OF_MONTH, Integer.parseInt(parts[2]));
		// 62135596800000l are the milliseconds between 0001-01-01 and
		// 1970-01-01
		long ms = cal.getTimeInMillis() + 62135596800000l + MILLISECONDSINADAY;
		int val = (int) (ms / MILLISECONDSINADAY);
		return val;
	}

	/**
	 * format a float as a string with max allowed decimal positions
	 * 
	 * @param value
	 * @return The formatted string
	 */
	public String floatToString(float value) {
		DecimalFormat decimalFormat = new DecimalFormat(
				"###############.###############");
		return decimalFormat.format(value);
	}

	/**
	 * format a double as a string with only 4 decimal position
	 * 
	 * @param value
	 * @return The formatted string
	 */
	public String currencyToString(double value) {
		Formatter formatter = new Formatter(getLocale());
		return formatter.format("%.4f", value).toString();
	}

	private Locale getLocale() {
		return locale;
	}

	/**
	 * convert a String to Double
	 * 
	 * @param value
	 * @return
	 */
	public double StringToDouble(String value) {
		return Double.valueOf(value).doubleValue();
	}

	/**
	 * Convert a DateTime to String
	 * 
	 * @param dateValue
	 * @return
	 */
	public String DateTimeToString(Date dateValue) {
		return datetimeFormatter.format(dateValue);
	}

	/**
	 * Convert a Time to String including milliseconds
	 * 
	 * @param timeValue
	 * @return
	 */
	public String TimeToString(Date timeValue) {
		return timeFormatterms.format(timeValue);
	}

	/**
	 * Convert a Time to String without milliseconds
	 * 
	 * @param timeValue
	 * @return
	 */
	public String TimeToStringWOms(Date timeValue) {
		return timeFormatterWOms.format(timeValue);
	}

	/**
	 * Convert a Date to String
	 * 
	 * @param dateValue
	 * @return
	 */
	public String DateToString(Date dateValue) {
		return dateFormatter.format(dateValue);
	}

	/**
	 * Create a DateTime from a String
	 * 
	 * @param value
	 * @return
	 * @throws ParseException
	 */

	public Date StringToDateTime(String value) throws ParseException {
		return datetimeFormatter.parse(value);
	}

	/**
	 * Returns the string representation of the AnsiString argument.
	 * 
	 * @param value
	 * @return
	 */
	public String AnsiStringToString(String value) {
		return value;
	}

	/**
	 * Returns the string representation of the WideString argument.
	 * 
	 * @param value
	 * @return
	 */
	public String WideStringToString(String value) {
		return value;
	}

	/**
	 * Returns the string representation of the int8 argument.
	 * 
	 * @param value
	 * @return
	 */
	public String Int8ToString(int value) {
		return String.valueOf(value);
	}

	/**
	 * Returns the string representation of the int16 argument.
	 * 
	 * @param value
	 * @return
	 */
	public String Int16ToString(int value) {
		return String.valueOf(value);
	}

	/**
	 * Returns the string representation of the int32 argument.
	 * 
	 * @param value
	 * @return
	 */
	public String Int32ToString(int value) {
		return String.valueOf(value);
	}

	/**
	 * Returns the string representation of the int64 argument.
	 * 
	 * @param value
	 * @return
	 */
	public String Int64ToString(long value) {
		return String.valueOf(value);
	}

	/**
	 * Returns the string representation of the Uint8 argument.
	 * 
	 * @param value
	 * @return
	 */
	public String UInt8ToString(int value) {
		return String.valueOf(value);
	}

	/**
	 * Returns the string representation of the Uint16 argument.
	 * 
	 * @param value
	 * @return
	 */
	public String UInt16ToString(int value) {
		return String.valueOf(value);
	}

	/**
	 * Returns the string representation of the Uint32 argument.
	 * 
	 * @param value
	 * @return
	 */
	public String UInt32ToString(long value) {
		return String.valueOf(value);
	}

	/**
	 * Returns the string representation of the Uint64 argument.
	 * 
	 * @param value
	 * @return
	 */
	public String UInt64ToString(long value) {
		return String.valueOf(value);
	}

	/**
	 * try to convert a Object to a String
	 * 
	 * @param value
	 * @return
	 */
	public String tryObjectToString(Object value) {
		if (value == null)
			return "null";
		if (value instanceof String)
			return (String) value;
		if (value instanceof Number) {
			if (value instanceof Double)
				return doubleToString(((Double) value).doubleValue());
			if (value instanceof Float)
				return floatToString(((Float) value).floatValue());
			return Int64ToString(((Number) value).longValue());
		}
		if (value instanceof Date)
			return DateTimeToString((Date) value);
		if (value instanceof Boolean)
			return booleanToString(((Boolean) value).booleanValue());
		return "unsupportedtype(" + value.getClass().getName() + ")";
	}

	/**
	 * Convert a Boolean to String
	 * 
	 * @param booleanValue
	 * @return string
	 */
	public String booleanToString(boolean booleanValue) {
		return booleanValue ? "true" : "false";
	}

	/**
	 * Convert a String to Boolean value
	 * 
	 * @param stringValue
	 * @return boolean value
	 * @throws DBXException
	 */
	public boolean stringToBoolean(String stringValue) throws DBXException {
		if (stringValue.equalsIgnoreCase("true"))
			return true;
		if (stringValue.equalsIgnoreCase("false"))
			return false;
		throw new DBXException("[" + stringValue + "] is not a valid boolean");
	}

}
