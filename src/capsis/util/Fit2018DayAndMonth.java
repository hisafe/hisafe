package capsis.util;


/**
 * A day and month in a year (whatever year), e.g. November 1st: day 1,
 * dayOfMonth 11.
 * 
 * @author F. de Coligny - September 2018
 */
public class Fit2018DayAndMonth {

	// Note: we can not have dayOfYear (doy, [1,366]) because we do not know
	// anything about the year, leap or not (is there a February 29 ?).

	private int month; // 1-12
	private int dayOfMonth; // 1-31

	/**
	 * Internal constructor. See the getInstance () methods.
	 */
	private Fit2018DayAndMonth() {
	}

	/**
	 * Returns an instance of Fit2018DayAndMonth matching the given day of year
	 * (in whatever not leap year).
	 */
	static public Fit2018DayAndMonth getInstance(double doy) throws Exception {

		// doy in [-365,366]
		if (doy < -365 || doy > 366)
			throw new Exception("Fit2018DayAndMonth, wrong value for doy: " + doy + ", expected in [-365,366]");

		// we do not know about leap year, we consider not leap, e.g. 1998 (we
		// will not keep the year)

		// fc-25.9.2018 if negative, keep it, addDays() below accepts a negative
		// argument, this manages better negative doys like -62 (no leap year
		// problem)
		// // e.g. -62 == 303
		// if (doy < 0)
		// doy += 365;

		Fit2018Date aux = Fit2018Date.getInstance(1998, 12, 31);
		aux = aux.addDays(doy);

		return Fit2018DayAndMonth.getInstance(aux.getDayOfMonth(), aux.getMonth());

	}

	/**
	 * Returns doy of this daysAndMonth in the given referenceYear (which is
	 * leap or not)
	 */
	public int getDayOfYear(int referenceYear) throws Exception {

		return Fit2018Date.getDayOfYear(referenceYear, month, dayOfMonth);

	}

	/**
	 * Returns an instance of Fit2018DayAndMonth matching the values.
	 */
	static public Fit2018DayAndMonth getInstance(int dayOfMonth1_31, int month1_12) throws Exception {

		if (dayOfMonth1_31 < 1 || dayOfMonth1_31 > 31)
			throw new Exception("Fit2018Date.getInstance() Wrong value for dayOfMonth1_31: " + dayOfMonth1_31);
		if (month1_12 < 1 || month1_12 > 12)
			throw new Exception("Fit2018Date.getInstance() Wrong value for month1_12: " + month1_12);

		Fit2018DayAndMonth instance = new Fit2018DayAndMonth();

		instance.month = month1_12;
		instance.dayOfMonth = dayOfMonth1_31;

		return instance;

	}

	/**
	 * Returns true if this is strictly greater than other. Null is not accepted
	 * and will result in an error.
	 */
	public boolean greaterThan(Fit2018DayAndMonth other) {

		if (month > other.getMonth()) {
			return true;
		} else if (month < other.getMonth()) {
			return false;
		} else {
			return dayOfMonth > other.getDayOfMonth();
		}

	}

	/**
	 * Returns true if this is equal to other. Null is not accepted and will
	 * result in an error.
	 */
	public boolean equals(Fit2018DayAndMonth other) {
		return this.month == other.month && this.dayOfMonth == other.dayOfMonth;
	}

	/**
	 * Returns true if this is strictly lower than other. Null is not accepted
	 * and will result in an error.
	 */
	public boolean lowerThan(Fit2018DayAndMonth other) {

		if (month < other.getMonth()) {
			return true;
		} else if (month > other.getMonth()) {
			return false;
		} else {
			return dayOfMonth < other.getDayOfMonth();
		}

	}

	public int getMonth() {
		return month;
	}

	public int getDayOfMonth() {
		return dayOfMonth;
	}

}
