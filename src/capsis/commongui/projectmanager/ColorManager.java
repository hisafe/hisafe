package capsis.commongui.projectmanager;

import java.awt.Color;

/**
 * A manager to return colors.
 * 
 * @author F. de Coligny - September 2012
 */
public class ColorManager {

	static private final double EPSILON = 0.01;
	
	/**
	 * Return a color given a hue ant a gradient
	 * 
	 * @param hue
	 * @param gradient : float between 0 and 1
	 */
	static public Color getColor (float hue, float gradient) {
	
		float s, b0, b;
		
		b0 = Math.min (gradient * 2f, 1f);
		b = 0.15f + b0 * 0.5f; // fc-21.9.2012 keep brightness low, 0 is the darkest
		
		s = Math.min (gradient * -2f + 2f, 1f);
		
		return ColorManager.getColor (hue, s, b);
	
	}

	/** Return the color for the given hsb */
	static public Color getColor (float h, float s, float b) {
		int rgb = Color.HSBtoRGB (h, s, b);
		return new Color (rgb);
	}

	/** Return hsb for the given color */
	
	static public float[] getHSB (Color c) {
		return Color.RGBtoHSB (c.getRed (), c.getGreen (), c.getBlue (), null);
	}

	/**
	 * Get a lighter color by dividing saturation by 2
	 */
	static public Color getLighterColor (Color color) {
		float hsb[] = getHSB (color); // hue, saturation, brightness
		return getColor (hsb[0], hsb[1] / 2, hsb[2]);
	}

	/** Get a darker color by multiplying saturation by 2 */
	static public Color getDarkerColor (Color color) {
		float hsb[] = getHSB (color); // hue, saturation, brightness
		float newS = Math.min (1f, hsb[1] * 2);
		return getColor (hsb[0], newS, hsb[2]);
	}

	/**
	 * Returns two colors based on the given color: one brighter and one darker. The two colors have
	 * hues which difference is equal to the given interval. if the interval if outside ]0, 1[, a
	 * default interval is used.
	 */
	static public Color[] get2Colors (Color color, float interval) {
		if (interval <= 0 || interval >= 1) interval = 0.3f;
	
		float hsb[] = getHSB (color); // hue, saturation, brightness
		float s = hsb[1];
		float sh = Math.min (1f, s + interval);
		float sl = sh - interval;
		// If the low value is below zero, shift both values up to 0 and interval
		if (sl < 0) {
			float shift = -sl;
			sh += shift;
			sl += shift;
		}
		Color cl = getColor (hsb[0], sl, hsb[2]);
		Color ch = getColor (hsb[0], sh, hsb[2]);
	
		return new Color[] {cl, ch};
	}

	/** Get a foreground color visible for the given background color */
	static public Color getForegroundColor (Color backgroundColor) {
		float[] f = getHSB (backgroundColor);
		double saturation = f[1];
		double brightness = f[2];
		// First color in our candidateSB will need a lignt foreground color
		if (Math.abs (saturation - 1) < ColorManager.EPSILON && Math.abs (brightness - 0.4) < ColorManager.EPSILON) {
			return Color.LIGHT_GRAY;
		} else {
			return Color.BLACK;
		}
	
	}

	/**
	 * Calculates a color from the color0 hue and the color1 saturation and brightness.
	 */
	static public Color getGradientColor (Color color0, Color color1) {
	
		float[] hsb0 = Color
				.RGBtoHSB (color0.getRed (), color0.getGreen (), color0.getBlue (), null);
		float[] hsb1 = Color
				.RGBtoHSB (color1.getRed (), color1.getGreen (), color1.getBlue (), null);
	
		return Color.getHSBColor (hsb0[0], hsb1[1], hsb1[2]);
	
	}

}
