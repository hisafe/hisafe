package capsis.extension.standviewer;


/**
 * SVLoxSettings.
 *
 * @author Ph. Dreyfus - January 2003 - March 2003
 */
public class SVLoxSettings extends SVSimpleSettings {
//upgraded for c4.1.1_11 - fc - 17.2.2003

			// see SVLox.java and SVLox_en.properties or SVLOx_fr.properties for the meaning of these booleans
	protected boolean cellLines;
	protected boolean crownProjection;
	protected boolean nhaValue;
	protected boolean ghaTint;
	protected boolean ghaValue;
	protected boolean refined;
	protected boolean drawUnder130;
	protected boolean drawLT1pix;
	protected boolean CTO_grid;
	protected boolean CTO_nogrid;
	protected boolean viewFromAbove;
	protected boolean squareCells;
	protected boolean compName;
	protected boolean drawSp1;
	protected boolean drawSp2;

	public SVLoxSettings () {
		resetSettings ();
	}

	public void resetSettings () {
		super.resetSettings ();
		cellLines = true;
		crownProjection = true;
		nhaValue = false;
		ghaTint = true;
		ghaValue = false;
		refined = false;
		drawUnder130 = false;	// fc - 18.12.2003 - On large stands, this option may result in very long drawings : default set to false
		drawLT1pix = false;		// fc - 18.12.2003 - On large stands, this option may result in very long drawings : default set to false
		CTO_grid = false;
		CTO_nogrid = true;
		viewFromAbove = true;
		squareCells = true;
		compName = false;
		drawSp1 = true;
		drawSp2 = true;
	}

	public String toString () {
		return " SVLox settings = "
				+super.toString ()
				+" cellLines="+cellLines
				+" crownProjection="+crownProjection
				+" nhaValue="+nhaValue
				+" ghaTint="+ghaTint
				+" ghaValue="+ghaValue
				+" refined="+refined
				+" CTO_grid="+CTO_grid
				+" CTO_nogrid="+CTO_nogrid
				+" viewFromAbove="+viewFromAbove
				+" squareCells="+squareCells
				+" drawUnder130="+drawUnder130
				+" drawLT1pix="+drawLT1pix
				+" compName="+compName
				+" drawSp1="+drawSp1
				+" drawSp2="+drawSp2;
	}

}

