package capsis.util;

import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.List;

import jeeb.lib.util.DefaultNumberFormat;

/**
 * A date in fit2018, available fields: year, month, dayOfMonth, dayOfYear,
 * dayOfYearReversed (phenofit4 notation e.g. -122 -> 366).
 * 
 * @author F. de Coligny - September 2018
 */
public class Fit2018Date {

	private static NumberFormat nf = DefaultNumberFormat.getInstance(2);

	// For internal use
	private static GregorianCalendar utilCalendar = new GregorianCalendar();

	private double dayOfYear; // 1-366

	private int year; // e.g. 2013
	private int month; // 1-12
	private int dayOfMonth; // 1-31

	/**
	 * Internal constructor. See the getInstance () methods.
	 */
	private Fit2018Date() {
	}

	/**
	 * Returns an instance of Fit2018Date matching the given date: year + day of
	 * year.
	 */
	static public Fit2018Date getInstance(int year, double dayOfYear) throws Exception {
//	static public Fit2018Date getInstance(int year, int dayOfYear) throws Exception {
		
		// fc-15.4.2019 changed dayOfYear to double
		
		Fit2018Date instance = getInstance(year - 1, 12, 31);
		return instance.addDays(dayOfYear);
	}

	/**
	 * Returns an instance of Fit2018Date matching the given date: year + month
	 * + dayOfMonth.
	 */
	static public Fit2018Date getInstance(int year, int month1_12, int dayOfMonth1_31) throws Exception {

		if (month1_12 < 1 || month1_12 > 12)
			throw new Exception("Fit2018Date.getInstance() Wrong value for month1_12: " + month1_12);
		if (dayOfMonth1_31 < 1 || dayOfMonth1_31 > 31)
			throw new Exception("Fit2018Date.getInstance() Wrong value for dayOfMonth1_31: " + dayOfMonth1_31);

		Fit2018Date instance = new Fit2018Date();

		// dayOfYear is always set in Fit2018Date. Calculated if not given
		instance.dayOfYear = getDayOfYear(year, month1_12, dayOfMonth1_31);

		instance.year = year;
		instance.month = month1_12;
		instance.dayOfMonth = dayOfMonth1_31;

		return instance;

	}

	/**
	 * Returns DayAndMonth of this date.
	 */
	public Fit2018DayAndMonth getDayAndMonth() throws Exception {
		return Fit2018DayAndMonth.getInstance(dayOfMonth, month);
	}

	/**
	 * Returns a copy of this date.
	 */
	public Fit2018Date getCopy() {
		Fit2018Date copy = new Fit2018Date();
		copy.dayOfYear = dayOfYear;
		copy.year = year;
		copy.month = month;
		copy.dayOfMonth = dayOfMonth;
		return copy;
	}

	/**
	 * Tells if the given year is leap year (i.e. 'bissextile').
	 */
	public static boolean isLeapYear(int year) {
		return utilCalendar.isLeapYear(year);
	}

	/**
	 * Returns the number of days in the given year.
	 */
	public static int getNbDays(int year) {
		int nbDays = 365;
		if (Fit2018Date.isLeapYear(year))
			nbDays++;

		return nbDays;
	}

	public static Fit2018Date getLastDay(int year) throws Exception {
		int dayOfYear = isLeapYear(year) ? 366 : 365;
		return Fit2018Date.getInstance(year, dayOfYear);

	}

	/**
	 * Returns the relative day of year, Phenofit4 notation (i.e. doy from
	 * January 1st of the given reference year, e.g. 2001, September 1st 2001 ->
	 * 244 ; 2001, September 1st 2000 -> -121
	 */
	public static double getRelativeDayOfYear(int referenceYear, Fit2018Date date) {

		int yyyy = date.getYear();

		double relativeDoy = 0;
		if (yyyy >= referenceYear) { // relativeDoy will be > 0
			for (int y = referenceYear; y < yyyy; y++) {
				relativeDoy += getNbDays(y);
			}
			// e.g. 0 + 244
			relativeDoy += date.getDayOfYear();

		} else { // relativeDoy will be <= 0
			for (int y = yyyy + 1; y < referenceYear; y++) {
				relativeDoy -= getNbDays(y);
			}
			double doy = date.getDayOfYear(); // 1,366 from Jan 1st
			// 0,-365 from Dec 31 of the date's year, positive value
			double yearRelativeDoy = getNbDays(date.getYear()) - doy;
			// e.g. 0,-121
			relativeDoy -= yearRelativeDoy;

		}

		return relativeDoy;
	}

	/**
	 * Returns a GragorianCalendar object matching this Fit2018Date.
	 */
	public GregorianCalendar getGregorianCalendar() {
		// Get date (in GregorianCalendar, month is in the range 0-11)
		int month_0_11 = month - 1;
		GregorianCalendar calendar = new GregorianCalendar(year, month_0_11, dayOfMonth);
		return calendar;
	}

	/**
	 * Updates the current Fit2018Date to match the date in the given calendar.
	 */
	public void updateOnCalendar(GregorianCalendar gc) {

		dayOfYear = gc.get(Calendar.DAY_OF_YEAR);

		year = gc.get(Calendar.YEAR);
		month = gc.get(Calendar.MONTH) + 1;
		dayOfMonth = gc.get(Calendar.DAY_OF_MONTH);

	}

	/**
	 * Returns true if this is strictly greater than other. Null is not accepted
	 * and will result in an error.
	 */
	public boolean greaterThan(Fit2018Date other) {
		int a = getGregorianCalendar().compareTo(other.getGregorianCalendar());
		return a > 0;
	}

	/**
	 * Returns true if this is greater or equal than other. Null is not accepted
	 * and will result in an error.
	 */
	public boolean greaterOrEqualThan(Fit2018Date other) {
		return greaterThan(other) || equals(other);
	}

	/**
	 * Returns true if this is equal to other. Null is not accepted and will
	 * result in an error.
	 */
	public boolean equals(Fit2018Date other) {
		return this.dayOfYear == other.getDayOfYear() && this.year == other.getYear() && this.month == other.getMonth()
				&& this.dayOfMonth == other.getDayOfMonth();
	}

	/**
	 * Returns true if this is strictly lower than other. Null is not accepted
	 * and will result in an error.
	 */
	public boolean lowerThan(Fit2018Date other) {
		int a = getGregorianCalendar().compareTo(other.getGregorianCalendar());
		return a < 0;
	}

	/**
	 * Returns true if this is lower or equal than other. Null is not accepted
	 * and will result in an error.
	 */
	public boolean lowerOrEqualThan(Fit2018Date other) {
		return lowerThan(other) || equals(other);
	}

	/**
	 * Returns true if this date's day and month are before the given
	 * dayAndMonth.
	 */
	public boolean before(Fit2018DayAndMonth dayAndMonth) throws Exception {
		return toDayAndMonth().lowerThan(dayAndMonth);
	}

	/**
	 * Returns true if this date's day and month are equal to the given
	 * dayAndMonth.
	 */
	public boolean matches(Fit2018DayAndMonth dayAndMonth) throws Exception {
		return toDayAndMonth().equals(dayAndMonth);
	}

	/**
	 * Returns true if this date's day and month are after the given
	 * dayAndMonth.
	 */
	public boolean after(Fit2018DayAndMonth dayAndMonth) throws Exception {
		return toDayAndMonth().greaterThan(dayAndMonth);
	}

	public Fit2018DayAndMonth toDayAndMonth() throws Exception {
		return Fit2018DayAndMonth.getInstance(this.getDayOfMonth(), this.getMonth());
	}

	/**
	 * Adds a number of days to this date, returns the resulting date. nbDays
	 * may be negative. Warning, nbDays may be decimal.
	 */
	public Fit2018Date addDays(double nbDays) throws Exception {

		// We return a copy
		Fit2018Date aux = getCopy();
		GregorianCalendar gc = aux.getGregorianCalendar();

		int n = (int) Math.floor(nbDays); // int part
		double d = Math.abs(nbDays - (int) nbDays); // abs decimal part

		// System.out.println("Fit2018Date: "+this+", addDays ("+nbDays+"), n: "
		// + n + " d: " + d + "...");

		if (nbDays > 0) {

			gc.add(Calendar.DAY_OF_MONTH, n);

			// add int part
			aux.updateOnCalendar(gc);

			// add decimal part
			aux.dayOfYear += d;

		} else if (nbDays < 0) {

			// add int part
			gc.add(Calendar.DAY_OF_MONTH, n);
			aux.updateOnCalendar(gc);

			// nbDays is decimal, adjust doy
			if (d != 0) {

				// add decimal part: add d complementary
				aux.dayOfYear += (1 - d);
			}

		}

		// System.out.println("-> " + aux);

		return aux;
	}

	/**
	 * Returns the date for the day before this date.
	 */
	public Fit2018Date getDayBefore() throws Exception {
		GregorianCalendar gc = getGregorianCalendar();
		gc.add(Calendar.DAY_OF_MONTH, -1);

		return Fit2018Date.getInstance(gc.get(Calendar.YEAR), gc.get(Calendar.MONTH) + 1,
				gc.get(Calendar.DAY_OF_MONTH));
	}

	/**
	 * Build a list of Fit2018Dates starting at the given date for the given
	 * numberOfYears
	 */
	static public List<Fit2018Date> nYearsDayList(Fit2018Date startDay, int numberOfYears) throws Exception {

		if (startDay == null)
			throw new Exception("Fit2018Date.nYearsDayList() Wrong value for startDay: " + startDay);
		if (numberOfYears <= 0)
			throw new Exception("Fit2018Date.nYearsDayList() Wrong value for numberOfYears: " + numberOfYears);

		// See how many days are requested
		int numberOfDays = 0;

		for (int year = startDay.getYear(); year <= numberOfYears; year++) {
			numberOfDays += getNbDays(year);
		}

		return nDaysDayList(startDay, numberOfDays);

	}

	/**
	 * Build a list of Fit2018Dates starting at the given date for the given
	 * numberOfDays
	 */
	static public List<Fit2018Date> nDaysDayList(Fit2018Date startDay, int numberOfDays) throws Exception {

		List<Fit2018Date> list = new ArrayList<>();

		int startYear = startDay.getYear();
		int startMonth = startDay.getMonth();
		int startDayOfMonth = startDay.getDayOfMonth();

		GregorianCalendar calendar = startDay.getGregorianCalendar();

		int d = 1;
		do {

			int dayOfYear = calendar.get(Calendar.DAY_OF_YEAR);
			int year = calendar.get(Calendar.YEAR);
			// First MONTH is 0...
			int month = calendar.get(Calendar.MONTH) + 1;
			int dayOfMonth = calendar.get(Calendar.DAY_OF_MONTH);

			list.add(Fit2018Date.getInstance(year, month, dayOfMonth));

			// System.out.println("d: " + d + " dayOfYear: " + dayOfYear + "
			// year: " + year + " month: " + month
			// + " dayOfMonth: " + dayOfMonth);

			d++;
			calendar.add(Calendar.DAY_OF_YEAR, 1); // next day

		} while (d <= numberOfDays);

		return list;
	}

	/**
	 * Returns the day of year for the given date, e.g. February 1st 2000 -> 32
	 */
	public static int getDayOfYear(int year, int month1_12, int dayOfMonth1_31) {
		// Get date (in GregorianCalendar, month is in the range 0-11)
		int month_0_11 = month1_12 - 1;
		GregorianCalendar calendar = new GregorianCalendar(year, month_0_11, dayOfMonth1_31);
		int dayOfYear = calendar.get(Calendar.DAY_OF_YEAR);

		return dayOfYear;
	}

	public double getDayOfYear() {
		return dayOfYear;
	}

	public int getYear() {
		return year;
	}

	public int getMonth() {
		return month;
	}

	public int getDayOfMonth() {
		return dayOfMonth;
	}

	public String getDate() {
		return "" + dayOfMonth + "." + month + "." + year;
	}

	@Override
	public String toString() {

		// dayOfYear with 2 fraction digits
		return "" + dayOfMonth + "." + month + "." + year + " (" + nf.format(dayOfYear) + ")";

	}

	// ------------------------------ no matter below this line

	// Test method
	public static void main(String[] args) throws Exception {

		System.out.println("getNbDays(1999): " + getNbDays(1999));
		System.out.println("getNbDays(2000): " + getNbDays(2000));
		System.out.println("getNbDays(2001): " + getNbDays(2001));

		int referenceYear = 2001;

		Fit2018Date date = Fit2018Date.getInstance(1999, 9, 1);
		System.out.println("refYear: " + referenceYear + " date: " + date + " relativeDayOfYear / " + referenceYear
				+ ": " + getRelativeDayOfYear(referenceYear, date));

		date = Fit2018Date.getInstance(2000, 9, 1);
		System.out.println("refYear: " + referenceYear + " date: " + date + " relativeDayOfYear / " + referenceYear
				+ ": " + getRelativeDayOfYear(referenceYear, date));

		date = Fit2018Date.getInstance(2001, 9, 1); // correct
		System.out.println("refYear: " + referenceYear + " date: " + date + " relativeDayOfYear / " + referenceYear
				+ ": " + getRelativeDayOfYear(referenceYear, date));

		date = Fit2018Date.getInstance(2002, 9, 1);
		System.out.println("refYear: " + referenceYear + " date: " + date + " relativeDayOfYear / " + referenceYear
				+ ": " + getRelativeDayOfYear(referenceYear, date));

		// Checked result:
		// getNbDays(1999): 365
		// getNbDays(2000): 366
		// getNbDays(2001): 365
		// refYear: 2001 date: Fit2018Date: 1.9.1999 (244) relativeDayOfYear:
		// -487
		// refYear: 2001 date: Fit2018Date: 1.9.2000 (245) relativeDayOfYear:
		// -121
		// refYear: 2001 date: Fit2018Date: 1.9.2001 (244) relativeDayOfYear:
		// 244
		// refYear: 2001 date: Fit2018Date: 1.9.2002 (244) relativeDayOfYear:
		// 609

		date = Fit2018Date.getInstance(2001, 1, 1);
		Fit2018Date dayBefore = date.getDayBefore();
		Fit2018Date twoDaysBefore = dayBefore.getDayBefore();
		System.out.println("date: " + date + " dayBefore:     " + dayBefore);
		System.out.println("date: " + date + " twoDaysBefore: " + twoDaysBefore);

		System.out.println();
		System.out.println("date: " + date + " twoDaysAfter:         " + date.addDays(2));
		System.out.println("date: " + date + " twoDaysAndAHalfAfter: " + date.addDays(2.5));
		System.out.println("date: " + date + " sixtyDaysAfter:       " + date.addDays(60));

		System.out.println();
		System.out.println("Math.floor (3.3): " + Math.floor(3.3));
		System.out.println("Math.floor (-3.3): " + Math.floor(-3.3));

		System.out.println();
		System.out.println("date: " + date + " threeDaysBefore:          " + date.addDays(-3));
		System.out.println("date: " + date + " threeDaysDotThreeBefore:  " + date.addDays(-3.3));
		System.out.println("date: " + date + " OneHudredAnd22DaysBefore: " + date.addDays(-122));

		// fc-1.4.2019
		date = Fit2018Date.getInstance(1951, 12, 31);
		System.out.println();
		System.out.println("date: " + date + " dayAfter:          " + date.addDays(1));
		
		date = Fit2018Date.getInstance(1952, 12, 30);
		System.out.println("date: " + date + " dayAfter:          " + date.addDays(1));
		
		date = Fit2018Date.getInstance(1952, 12, 31);
		System.out.println("date: " + date + " dayAfter:          " + date.addDays(1));

	}

}
