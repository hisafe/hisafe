package capsis.util.extendeddefaulttype.dataextractor;

import capsis.kernel.extensiontype.GenericExtensionStarter;
import repicea.util.REpiceaTranslator;
import repicea.util.REpiceaTranslator.TextableEnum;

public class DETimeProdWithCI extends AbstractDETimeWithCI {

	protected enum MessageID implements TextableEnum {
		Description("Average production with confidence intervals", "Production moyenne avec intervalles de confiances"),
		Name("CI - Average production", "IC - Production moyenne"),
		;
		MessageID(String englishText, String frenchText) {
			setText(englishText, frenchText);
		}
		
		@Override
		public void setText(String englishText, String frenchText) {
			REpiceaTranslator.setString(this, englishText, frenchText);
		}
		
		@Override
		public String toString() {return REpiceaTranslator.getString(this);}
	}

	
	/**
	 * Phantom constructor. Only to ask for extension properties (authorName,
	 * version...).
	 */
	public DETimeProdWithCI() {
		super(Variable.P);
	}

	/**
	 * Official constructor. It uses the standard Extension starter.
	 */
	public DETimeProdWithCI(GenericExtensionStarter s) {
		super(s, Variable.P);
	}

	/**
	 * From DataFormat interface.
	 */
	@Override
	public String getName() {
		return getNamePrefix() + MessageID.Name;
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getAuthor() {
		return "M. Fortin";
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getDescription() {
		return MessageID.Description.toString();
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getVersion() {
		return "1.0";
	}

	// nb-09.08.2018
	//public static final String VERSION = "1.0";

	@Override
	public String getText() {
		return text;
	}

	@Override
	protected String getYAxisLabelName() {
		return variable.toString();
	}

}
