package capsis.lib.samsaralight.tag.heterofor;

import java.io.Serializable;

/**
 * A superclass of HetAverageRadiationLine to allow the use of SamsaraLight Tag
 * mode from PDGLight.
 * 
 * @author F. de Coligny, C. Rouet - January 2022
 */
public class SLRadiationLine implements Serializable {

	public int month;
	public int doy; // day of year [1,366]
	public int hour;
	public double globalRadiation_MJm2;
	public double diffuseToGlobalRatio; // [0, 1]

	public SLRadiationLine(int month, int doy, int hour, double radiation_MJm2, double diffuseToGlobalRatio) {

		this.month = month;
		this.doy = doy;
		this.hour = hour;

		this.globalRadiation_MJm2 = radiation_MJm2;
		this.diffuseToGlobalRatio = diffuseToGlobalRatio;

	}

	public String toString() {

		return "SLRadiationLine month: " + month + " doy: " + doy + " hour: " + hour + " globalRadiation_MJm2: "
				+ globalRadiation_MJm2 + " diffuseToGlobalRatio: " + diffuseToGlobalRatio;
	}

}
