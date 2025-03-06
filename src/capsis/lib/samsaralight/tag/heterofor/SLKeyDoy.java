package capsis.lib.samsaralight.tag.heterofor;

import java.io.Serializable;

/**
 * A key day in fine resolution radiative balance mode, e.g. 2 specific days in
 * the leaf development period, characterized by a leaf development coefficient
 * (ladProportion). Comes from Heterofor, generalized for PDGLight.
 *
 * @author F. de Coligny, C. Rouet - January 2022
 */
public class SLKeyDoy implements Serializable {
	
	public int doy; // day of year [1, 366]
	public String name; // for tag creation...

	/**
	 * Constructor.
	 */
	public SLKeyDoy(int doy, String name) {
		this.doy = doy;
		this.name = name;
	}

}
