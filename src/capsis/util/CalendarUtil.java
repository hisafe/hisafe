package capsis.util;

import java.util.Calendar;
import java.util.Calendar.Builder;
import java.util.GregorianCalendar;

/**
 * Toll methods on dates.
 * 
 * @author F. de Coligny - April 2023
 */
public class CalendarUtil {

	private static GregorianCalendar calendar;

	/**
	 * Returns true if the given year is 'bissextile' (i.e. leap year).
	 */
	public static boolean isLeap(int year) {
		
		// N. Beudez - September 2017
		// NOTE: this method was in a class named Calendar, which forbids the use of
		// java.util.Calendar. Renamed the class into CalendarUtil
		
		if (calendar == null)
			calendar = new GregorianCalendar();

		return calendar.isLeapYear(year);
	}


	/**
	 * Returns the number of days in the given month.
	 */
	static public int getNbDays(int year, int month_0_11) throws Exception {
		// fc+vvk-5.4.2023
		
		if (month_0_11 < 0 || month_0_11 > 11)
			throw new Exception("Error in getNbDays(), wrong month_0_11: " + month_0_11);

		Calendar cal = new Calendar.Builder().setCalendarType("gregorian")
				.setFields(Calendar.YEAR, year, Calendar.MONTH, month_0_11, Calendar.DAY_OF_MONTH, 1).build();
		
		int lastDayOfMonth = cal.getActualMaximum(GregorianCalendar.DAY_OF_MONTH);

		return lastDayOfMonth;

	}


	/**
	 * Returns true if the given doy of the given year is the last day of a month.
	 */
	public static boolean isLastDayOfMonth(int year, int doy) {
		
		// fc+tp-10.3.2023
		Calendar cal = new Calendar.Builder().setCalendarType("gregorian")
				.setFields(Calendar.YEAR, year, Calendar.DAY_OF_YEAR, doy).build();
	
		int lastDayOfMonth = cal.getActualMaximum(Calendar.DAY_OF_MONTH);
		
		int month = cal.get(Calendar.MONTH);
		
		Calendar cal2 = new Calendar.Builder().setCalendarType("gregorian").setFields(Calendar.YEAR, year,
		                      Calendar.MONTH, month,
		                      Calendar.DAY_OF_MONTH, lastDayOfMonth).build();
		return doy == cal2.get(Calendar.DAY_OF_YEAR);
	}
	/**
	 * Returns the month number [0,11] of a given doy, for a given year.
	 */
	public static int getMonth0_11(int year, int doy) {
		
		//tp-7.3.2024
		Calendar cal = new Calendar.Builder().setCalendarType("gregorian")
				.setFields(Calendar.YEAR, year, Calendar.DAY_OF_YEAR, doy).build();
	
		
		return (cal.get(Calendar.MONTH));
	}
	

	/**
	 * Returns the last day of the month matching the given doy of the given year.
	 */
	public static int getLastDayOfMonth(int year, int doy) {
		// fc+tp-10.3.2023 Checked, ok
		Calendar cal = new Calendar.Builder().setCalendarType("gregorian")
				.setFields(Calendar.YEAR, year, Calendar.DAY_OF_YEAR, doy).build();
	
		int lastDayOfMonth = cal.getActualMaximum(Calendar.DAY_OF_MONTH);
	
		return lastDayOfMonth;
	}

	
}
