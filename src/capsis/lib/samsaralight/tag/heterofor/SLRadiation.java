package capsis.lib.samsaralight.tag.heterofor;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeSet;

import jeeb.lib.util.ListMap;
import jeeb.lib.util.Log;

/**
 * A superclass of HetAverageRadiation to allow the use of SamsaraLight Tag mode
 * from PDGLight.
 * 
 * @author F. de Coligny, C. Rouet - January 2022
 */
public class SLRadiation implements Serializable {

	static public class Key implements Comparable, Serializable {
		public int doy;
		public int hour;

		public Key(int doy, int hour) {
			this.doy = doy;
			this.hour = hour;
		}

		@Override
		public int compareTo(Object o) {
			Key otherKey = (Key) o;
			int val = doy * 1000 + hour;
			int otherVal = otherKey.doy * 1000 + otherKey.hour;
			return val - otherVal;
		}

		// fc-9.6.2017
		@Override
		public int hashCode() {
			return doy * 1000 + hour;
		}

		// fc-9.6.2017
		@Override
		public boolean equals(Object other) {
			return this.hashCode() == other.hashCode();
		}

		public String toString() {
			return "SLRadiation doy: " + doy + " hour: " + hour;
		}

	}

	protected Map<Key, SLRadiationLine> radiationMap;

	/**
	 * Constructor
	 */
	public SLRadiation() {
		
		// LinkedHashMap must be kept here, keeps insertion order, comes from HetAverageRadiation
		radiationMap = new LinkedHashMap<>();
	}

	/**
	 * Adds the given radiation line.
	 */
	public void addLine(SLRadiationLine radLine) {
		
		radiationMap.put(new Key(radLine.doy, radLine.hour), radLine);
	}

	
	public Map<Key, SLRadiationLine> getRadiationMap() {
		return radiationMap;
	}
	
	/**
	 * Returns the incident global radiation for the given period and hour.
	 */
	public double getIncidentGlobalRadiation(int startDoy, int endDoy, int hour) throws RuntimeException {
		double sumGlobalRadiation_MJm2 = 0;

		for (int doy = startDoy; doy <= endDoy; doy++) {
			sumGlobalRadiation_MJm2 += getIncidentGlobalRadiation(doy, hour);
		}

		return sumGlobalRadiation_MJm2;
	}

	// fa-22.6.2017
	/**
	 * Returns the incident direct radiation for the given period and hour.
	 */
	public double getIncidentDirectRadiation(int startDoy, int endDoy, int hour) throws RuntimeException {
		double sumDirectRadiation_MJm2 = 0;

		for (int doy = startDoy; doy <= endDoy; doy++) {
			sumDirectRadiation_MJm2 += getIncidentDirectRadiation(doy, hour);
		}

		return sumDirectRadiation_MJm2;
	}

	// fa-22.6.2017
	/**
	 * Returns the incident diffuse radiation for the given period and hour.
	 */
	public double getIncidentDiffuseRadiation(int startDoy, int endDoy, int hour) throws RuntimeException {
		double sumDiffuseRadiation_MJm2 = 0;

		for (int doy = startDoy; doy <= endDoy; doy++) {
			sumDiffuseRadiation_MJm2 += getIncidentDiffuseRadiation(doy, hour);
		}

		return sumDiffuseRadiation_MJm2;
	}

	/**
	 * Returns the incident global radiation for the given doy and hour.
	 */
	public double getIncidentGlobalRadiation(int doy, int hour) throws RuntimeException {
		SLRadiationLine line = radiationMap.get(new Key(doy, hour));

		if (line == null) {
			Log.println(traceRadiationap()); // fc-9.6.2017
			throw new RuntimeException(
					"Error in SLRadiation, calculateIncidentGlobalRadiation() could not get the average line for doy: "
							+ doy + ", hour: " + hour);
		}

		return line.globalRadiation_MJm2;
	}

	// fa-22.6.2017
	/**
	 * Returns the incident direct radiation for the given doy and hour.
	 * MJ / m2 horizontal to soil
	 */
	public double getIncidentDirectRadiation(int doy, int hour) throws RuntimeException {
		SLRadiationLine line = radiationMap.get(new Key(doy, hour));

		if (line == null) {
			Log.println(traceRadiationap()); // fc-9.6.2017
			throw new RuntimeException(
					"Error in SLRadiation, calculateIncidentDirectRadiation() could not get the average line for doy: "
							+ doy + ", hour: " + hour);
		}

		return line.globalRadiation_MJm2 * (1 - line.diffuseToGlobalRatio);
	}

	// fa-22.6.2017
	/**
	 * Returns the incident diffuse radiation for the given doy and hour.
	 */
	public double getIncidentDiffuseRadiation(int doy, int hour) throws RuntimeException {
		SLRadiationLine line = radiationMap.get(new Key(doy, hour));

		if (line == null) {
			Log.println(traceRadiationap()); // fc-9.6.2017
			throw new RuntimeException(
					"Error in SLRadiation, calculateIncidentDiffuseRadiation() could not get the average line for doy: "
							+ doy + ", hour: " + hour);
		}

		return line.globalRadiation_MJm2 * line.diffuseToGlobalRatio;
	}

	public List<SLRadiationLine> getLines(int doy) {
		ArrayList<SLRadiationLine> list = new ArrayList<>();
		for (SLRadiationLine line : radiationMap.values()) {
			if (line.doy == doy)
				list.add(line);
		}

		return list;

	}

	/**
	 * Returns the average radiation lines in a list map which Key is month.
	 */
	public ListMap<Integer, SLRadiationLine> getRadiationsPerMonth() {

		ListMap<Integer, SLRadiationLine> radiationsPerMonth = new ListMap<>();

		for (SLRadiationLine line : radiationMap.values()) {
			radiationsPerMonth.addObject(line.month, line);
		}
		return radiationsPerMonth;
	}

	// fc-9.6.2017
	public String traceRadiationap() {
		
		StringBuffer b = new StringBuffer("SLRadiation.radiationMap...");
		
		if (radiationMap == null)
			b.append("   radiationMap = null");
		else if (radiationMap.isEmpty())
			b.append("   radiationMap is empty");
		else {
			for (Key k : new TreeSet<>(radiationMap.keySet())) {
				SLRadiationLine line = radiationMap.get(k);
				b.append("\n   " + k.doy + "." + k.hour + ": " + line);
			}
			b.append("\n   -> " + radiationMap.size() + " lines.");
		}

		return b.toString();
	}

}
