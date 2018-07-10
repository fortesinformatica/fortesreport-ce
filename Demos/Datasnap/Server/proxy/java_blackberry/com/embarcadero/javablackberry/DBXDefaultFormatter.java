//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

package com.embarcadero.javablackberry;

import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;
import java.util.Vector;
import javax.microedition.global.Formatter;

import net.rim.device.api.i18n.Locale;
import net.rim.device.api.i18n.SimpleDateFormat;

/**
 * 
 * Contains methods to represent a TDBX type to a string format and vice versa.
 * 
 */

public class DBXDefaultFormatter {
	/*
	 * static DateFormat: TFormatSettings; TimeFormat: TFormatSettings;
	 * TimeStampFormat: TFormatSettings;
	 */

	public static final String DATETIMEFORMAT = "yyyy-MM-dd HH:mm:ss.SSS";
	public static final String DATEFORMAT = "yyyy-MM-dd";
	public static final String TIMEFORMAT = "HH:mm:ss.SSS";
	public static final String TIMEFORMAT_WO_MS = "HH:mm:ss";
	public static final int CURRENCYDECIMALPLACE = 4;
	private static DBXDefaultFormatter instance;
	private static SimpleDateFormat timeFormatter;
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
		locale = Locale.get(Locale.LOCALE_en_US);
		timeFormatter = new SimpleDateFormat(TIMEFORMAT);
		dateFormatter = new SimpleDateFormat(DATEFORMAT);
		datetimeFormatter = new SimpleDateFormat(DATETIMEFORMAT);
	}

	public Date StringToDate(String StringValue) {
		return null;
	}

	public Date StringToTime(String StringValue) {
		return null;
	}

	/**
	 * format a double as a string with max allowed decimal positions
	 * 
	 * @param value
	 * @return The formatted string
	 */
	public String doubleToString(double value) {
		Formatter formatter = new Formatter(getLocale().toString());
		return formatter.formatNumber(value);
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
		String ret;
		if (c.get(Calendar.HOUR_OF_DAY) < 9)
			ret = "0" + c.get(Calendar.HOUR_OF_DAY);
		else
			ret = String.valueOf(c.get(Calendar.HOUR_OF_DAY));
		if (c.get(Calendar.MINUTE) < 9)
			ret += ":0" + c.get(Calendar.MINUTE);
		else
			ret += ":" + c.get(Calendar.MINUTE);
		if (c.get(Calendar.SECOND) < 9)
			ret += ":0" + c.get(Calendar.SECOND);
		else
			ret += ":" + c.get(Calendar.SECOND);
		return ret;
	}

	private static final long MILLISECONDSINADAY = 1000 * 60 * 60 * 24;

	private static final long JULIAN_JANUARY_01 = 1721426;// Julian calendar
															// days beetween 01
															// January 4713 and
															// 01 January 01
															// JULIAN CALENDAR

	private String multiplyString(String c, int howManyTimes) {
		if (howManyTimes == 0)
			return "";
		StringBuffer sb = new StringBuffer();
		for (int i = 0; i < howManyTimes; i++)
			sb.append(c);
		return sb.toString();
	}


	/**
	 * create a TDBXTime (int) from a string
	 * 
	 * @param value
	 * @return The int value
	 */
	public int StringToTDBXTime(String value) {
		Vector parts = split(value, ':');
		int result = (Integer.valueOf((String) parts.elementAt(0)).intValue() * 3600000)
				+ (Integer.valueOf((String) parts.elementAt(1)).intValue() * 60000)
				+ (Integer.valueOf((String) parts.elementAt(2)).intValue() * 1000);
		return result;
	}


	/**
	 * format a float as a string with max allowed decimal positions
	 * 
	 * @param value
	 * @return The formatted string
	 */
	public String floatToString(float value) {
		return Float.toString(value);
	}

	/**
	 * format a double as a string with only 4 decimal position
	 * 
	 * @param value
	 * @return The formatted string
	 */
	public String currencyToString(double value) {
		Formatter formatter = new Formatter(getLocale().toString());
		return formatter.formatNumber(value, 4);
	}

	private Locale getLocale() {
		return locale;
	}

	/**
	 * Returns a new double initialized to the value represented by the
	 * specified String
	 * 
	 * @param value
	 * @return
	 */
	public double StringToDouble(String value) {
		return Double.parseDouble(value);
	}

	/**
	 * Converts this Date object to a String using this pattern yyyy-MM-dd
	 * HH:mm:ss.SSS
	 * 
	 * @param dateValue
	 * @return
	 */
	public String DateTimeToString(Date dateValue) {
		return datetimeFormatter.format(dateValue);
	}

	/**
	 * Converts this Date object to a String using this pattern yyyy-MM-dd
	 * 
	 * @param dateValue
	 * @return
	 */

	public String DateToString(Date dateValue) {
		return dateFormatter.format(dateValue);
	}

	/**
	 * Converts this Date object to a String using this pattern HH:mm:ss.SSS
	 * 
	 * @param dateValue
	 * @return
	 */

	public String TimeToString(Date dateValue) {
		return timeFormatter.format(dateValue);
	}

	/**
	 * Converts this Date object to a String using this pattern HH:mm:ss
	 * 
	 * @param dateValue
	 * @return
	 */

	public String TimeToStringWOms(Date dateValue) {
		Calendar c = Calendar.getInstance();
		clearCalendar(c);
		c.setTime(dateValue);
		String ret;
		if (c.get(Calendar.HOUR_OF_DAY) < 9)
			ret = "0" + c.get(Calendar.HOUR_OF_DAY);
		else
			ret = String.valueOf(c.get(Calendar.HOUR_OF_DAY));
		if (c.get(Calendar.MINUTE) < 9)
			ret += ":0" + c.get(Calendar.MINUTE);
		else
			ret += ":" + c.get(Calendar.MINUTE);
		if (c.get(Calendar.SECOND) < 9)
			ret += ":0" + c.get(Calendar.SECOND);
		else
			ret += ":" + c.get(Calendar.SECOND);
		return ret;
	}

	/**
	 * Returns a new Date initialized to the value represented by the specified
	 * String
	 * 
	 * @param value
	 * @return
	 */

	public Date StringToDateTime(String value) {
		Vector date_time = split(value, ' ');
		Calendar c = Calendar.getInstance();

		// setting the date
		String date = (String) date_time.elementAt(0);
		Vector _date = split(date, '-');
		int year = Integer.parseInt((String) _date.elementAt(0));
		int month = Integer.parseInt((String) _date.elementAt(1));
		int day = Integer.parseInt((String) split((String) _date.elementAt(2),
				'.').elementAt(0));
		c.set(Calendar.YEAR, year);
		c.set(Calendar.MONTH, month - 1);// month is 12 based
		c.set(Calendar.DAY_OF_MONTH, day);

		// setting the time
		if (date_time.size() > 1) {
			String time = (String) date_time.elementAt(1);
			Vector _time = split(time, ':');
			int hour = Integer.parseInt((String) _time.elementAt(0));
			int minute = Integer.parseInt((String) _time.elementAt(1));
			int second = Integer.parseInt((String) split(
					(String) _time.elementAt(2), '.').elementAt(0));
			int millisecond = Integer.parseInt((String) split(
					(String) _time.elementAt(2), '.').elementAt(1));
			c.set(Calendar.HOUR_OF_DAY, hour);
			c.set(Calendar.MINUTE, minute);
			c.set(Calendar.SECOND, second);
			c.set(Calendar.MILLISECOND, millisecond);
		}
		return c.getTime();
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
	 * Returns the string representation of the int argument.
	 * 
	 * @param value
	 *            int argument.
	 * @return the string representation.
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
	 * Returns the string representation of the UInt8 argument.
	 * 
	 * @param value
	 * @return
	 */
	public String UInt8ToString(int value) {
		return String.valueOf(value);
	}

	/**
	 * Returns the string representation of the UInt16 argument.
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
	 * Tries to convert an object to a String
	 * 
	 * @param value
	 * @return
	 */
	public String tryObjectToString(Object value) {
		if (value == null)
			return "null";
		if (value instanceof String)
			return (String) value;
		if (value instanceof Double)
			return doubleToString(((Double) value).doubleValue());
		if (value instanceof Float)
			return floatToString(((Float) value).floatValue());
		if (value instanceof Long)
			return Int64ToString(((Long) value).longValue());
		if (value instanceof Date)
			return DateTimeToString((Date) value);
		if (value instanceof Boolean)
			return booleanToString(((Boolean) value).booleanValue());
		return "unsupportedtype(" + value.getClass().getName() + ")";
	}

	public void clearCalendar(Calendar cal) {

		cal.set(Calendar.YEAR, 1);
		cal.set(Calendar.MONTH, 0);
		cal.set(Calendar.DAY_OF_MONTH, 1);
		cal.set(Calendar.HOUR_OF_DAY, 0);
		cal.set(Calendar.MINUTE, 0);
		cal.set(Calendar.SECOND, 0);
		cal.set(Calendar.MILLISECOND, 0);

	}

	public String booleanToString(boolean booleanValue) {
		return booleanValue ? "true" : "false";
	}

	public boolean stringToBoolean(String stringValue) throws DBXException {
		if (stringValue.equalsIgnoreCase("true"))
			return true;
		if (stringValue.equalsIgnoreCase("false"))
			return false;
		throw new DBXException("[" + stringValue + "] is not a valid boolean");
	}

	private Vector split(String data, char splitChar) {
		Vector v = new Vector();

		String working = data;
		int index = working.indexOf(splitChar);

		while (index != -1) {
			String tmp = "";
			if (index > 0)
				tmp = working.substring(0, index);
			v.addElement(tmp);

			working = working.substring(index + 1);

			// Find the next index
			index = working.indexOf(splitChar);
		}

		// Add the rest of the working string
		v.addElement(working);

		return v;
	}

	private Calendar JulianDateToGregorianDate(long jd) {

		long j, y, m, d;
		j = jd;
		j = j - 1721119;
		y = (4 * j - 1) / 146097;
		j = 4 * j - 1 - 146097 * y;
		d = j / 4;
		j = (4 * d + 3) / 1461;
		d = 4 * d + 3 - 1461 * j;
		d = (d + 4) / 4;
		m = (5 * d - 3) / 153;
		d = 5 * d - 3 - 153 * m;
		d = (d + 5) / 5;
		y = 100 * y + j;
		if (m < 10) {
			m = m + 3;
		} else {
			m = m - 9;
			y = y + 1;
		}

		Calendar date = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
		date.set(Calendar.YEAR, (int) y);
		date.set(Calendar.MONTH, (int) m - 1);
		date.set(Calendar.DAY_OF_MONTH, (int) d);
		date.set(Calendar.MILLISECOND, 0);

		return date;

	}

	private long DateToJulianDate(Calendar value) {
		int day = value.get(Calendar.DAY_OF_MONTH);
		int month = value.get(Calendar.MONTH) + 1;
		int year = value.get(Calendar.YEAR);
		return gregorianDateToJulianDate(day, month, year);
	}

	private long gregorianDateToJulianDate(int DAY, int MONTH, int YEAR) {
		int JD, I, J, K;
		I = YEAR;
		J = MONTH;
		K = DAY;
		JD = K - 32075 + 1461 * (I + 4800 + (J - 14) / 12) / 4 + 367
				* (J - 2 - (J - 14) / 12 * 12) / 12 - 3
				* ((I + 4900 + (J - 14) / 12) / 100) / 4;

		return JD + 1;

	}

	/**
	 * Returns the String representation of the value represented by the
	 * specified TDBXDate
	 * 
	 * @param value
	 * @return
	 */
	public String TDBXDateToString(long value) {
		if (value == 0)
			value = 1;
		String s = DateToString(JulianDateToGregorianDate(
				value + JULIAN_JANUARY_01 - 1).getTime());
		int l = s.length();
		return multiplyString("0", 10 - l) + s;

	}

	private long timeIntervalSince1582(Calendar date) {

		Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
		cal.set(Calendar.YEAR, 1582);
		cal.set(Calendar.MONTH, 9);
		cal.set(Calendar.DAY_OF_MONTH, 01);
		cal.set(Calendar.HOUR_OF_DAY, 12);
		cal.set(Calendar.MINUTE, 0);
		cal.set(Calendar.SECOND, 0);
		cal.set(Calendar.MILLISECOND, 0);

		long dtInterval = date.getTime().getTime() - cal.getTime().getTime();
		return dtInterval;
	}

	private long timeIntervalSince0001(Calendar dt) {

		Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
		cal.set(Calendar.YEAR, 1970);
		cal.set(Calendar.MONTH, 0);
		cal.set(Calendar.DAY_OF_MONTH, 01);
		cal.set(Calendar.HOUR_OF_DAY, 12);
		cal.set(Calendar.MINUTE, 0);
		cal.set(Calendar.SECOND, 0);
		cal.set(Calendar.MILLISECOND, 0);

		long dtInterval1970 = timeIntervalSince1582(cal);
		long dtInterval = timeIntervalSince1582(dt);
		// 52842175200 is second til OCT 1852
		dtInterval = dtInterval + 62135596800000L - dtInterval1970;
		return dtInterval;
	}

	/**
	 * Returns a new TDBXDate initialized to the value represented by the
	 * specified String
	 * 
	 * @param value
	 * @return
	 */

	public int StringToTDBXDate(String value) {
		
		String dtValue = value + " 12:00:00.000";

		Date endDate = StringToDateTime(dtValue);
		Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
		cal.setTime(endDate);

		long endDateInterval = timeIntervalSince1582(cal);
		if (endDateInterval < 0) {
			// do conversion from juliandate to gregorian
			endDateInterval = DateToJulianDate(cal) - JULIAN_JANUARY_01;

		} else {
			endDateInterval = timeIntervalSince0001(cal);
			endDateInterval = (endDateInterval + MILLISECONDSINADAY)
					/ MILLISECONDSINADAY;

		}
		return (int) endDateInterval;

	}
}
