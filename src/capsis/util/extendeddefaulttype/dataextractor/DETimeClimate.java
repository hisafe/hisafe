package capsis.util.extendeddefaulttype.dataextractor;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;
import java.util.Vector;

import capsis.kernel.MethodProvider;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.GenericExtensionStarter;
import capsis.util.extendeddefaulttype.ExtCompositeStand;
import capsis.util.extendeddefaulttype.ExtModel;
import capsis.util.extendeddefaulttype.methodprovider.ClimateVariableProvider;
import repicea.simulation.climate.REpiceaClimateVariableMap.ClimateVariable;
import repicea.util.REpiceaTranslator;
import repicea.util.REpiceaTranslator.TextableEnum;

public class DETimeClimate extends AbstractDETime {

	protected enum MessageID implements TextableEnum {
		Description("Plot the average climate trends", "Trace la tendances climatiques moyennes"),
		Name("Climate evolution", "Evolution climatique"),
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

	
	private Map<String, ClimateVariable> variableMap; 
	
	
	/**
	 * Phantom constructor
	 */
	public DETimeClimate() {}
	
	
	/**
	 * Official constructor. It uses the standard Extension starter.
	 */
	public DETimeClimate(GenericExtensionStarter s) {
		super(s);
		variableMap = new HashMap<String, ClimateVariable>();
		for (ClimateVariable v : ClimateVariable.values()) {
			variableMap.put(v.toString(), v);
		}
	}

	
	@Override
	public String getName() {return MessageID.Name.toString();}

	@Override
	public String getText() {
		return "";
	}

	@Override
	public String getVersion() {return "1.0";}

	@Override
	public String getAuthor() {return "Mathieu Fortin";}

	@Override
	public String getDescription() {return MessageID.Description.toString();}

	@Override
	public boolean matchWith(Object referent) {
		if (referent instanceof ExtModel) {
			MethodProvider mp = ((ExtModel) referent).getMethodProvider();
			if (mp instanceof ClimateVariableProvider) {
				return true;
			}
		}
		return false;
	}

	@Override
	protected String getYAxisLabelName() {
		return getSelectedVariable().toString();
	}

	@Override
	protected Vector<String> extractLabels(Vector values) {
		Vector<String> output = new Vector<String>();
		return output;
	}

	
	private ClimateVariable getSelectedVariable() {
		String selectedVariable = getComboProperty("Selected variable");
		ClimateVariable v = variableMap.get(selectedVariable);
		return v;
	}
	
	@Override
	protected Vector<Vector<? extends Number>> extractValues() {
		ClimateVariable v = getSelectedVariable();
		Vector<Vector<? extends Number>> output = new Vector<Vector<? extends Number>>();
		
		// Retrieve Steps from root to this step
		Vector<Step> steps = step.getProject().getStepsFromRoot(step);

		Vector<Integer> c1 = new Vector<Integer>(); // x coordinates
		Vector<Double> c2 = new Vector<Double>(); // y coordinates
		output.add(c1);
		output.add(c2);

		// Data extraction : points with (Integer, Double) coordinates
		
		for (int i = 1; i < steps.size(); i++) {
			Step currentStep = steps.get(i);
			
			ExtCompositeStand stand = (ExtCompositeStand) currentStep.getScene();
			int date = currentStep.getScene().getDate();
			double value = ((ClimateVariableProvider) methodProvider).getClimateVariable(stand, v);

			c1.add(new Integer(date));
			c2.add(new Double(value));
		}

		return output;
	}

	@Override
	public void setConfigProperties() {
		LinkedList<String> variableList = new LinkedList<String>();
		for (ClimateVariable v : ClimateVariable.values()) {
			variableList.add(v.toString());
		}
		addComboProperty("Selected variable", variableList);
	}

}
