package capsis.extension.standviewer;

import java.awt.GridLayout;

import javax.swing.JComponent;

import capsis.commongui.projectmanager.StepButton;
import capsis.commongui.util.Tools;
import capsis.extension.AbstractStandViewer;
import capsis.kernel.GModel;
import capsis.kernel.InitialParameters;
import capsis.kernel.Step;
import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;

/**
 * An inspector showing the initialParameters of the project to check the
 * initial configuration at any time of the simulation.
 * 
 * @author F. de Coligny - January 2023
 */
public class SVInitialParameters extends AbstractStandViewer {

	static {
		Translator.addBundle("capsis.extension.standviewer.SVInitialParameters");
	}

	private String modelName;

	/**
	 * Opens and inits the tool on the given stepButton
	 */
	@Override
	public void init(GModel model, Step s, StepButton but) throws Exception {
		super.init(model, s, but);
		setLayout(new GridLayout(1, 1)); // important for viewer appearance

		modelName = model.getIdCard().getModelName();

		update(stepButton);
	}

	/**
	 * Extension dynamic compatibility mechanism. This matchwith method checks if
	 * the extension can deal (i.e. is compatible) with the referent.
	 */
	static public boolean matchWith(Object referent) {
		try {
			return referent instanceof GModel;

		} catch (Exception e) {
			Log.println(Log.ERROR, "SVInitialParameters.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}

	}

	@Override
	public String getName() {

		// modelName is available after init () has been called
		String suffix = "";
		if (modelName != null)
			suffix = " (" + modelName + ")";

		return Translator.swap("SVInitialParameters.name") + suffix;
	}

	@Override
	public String getAuthor() {
		return "F. de Coligny";
	}

	@Override
	public String getDescription() {
		return Translator.swap("SVInitialParameters.description");
	}

	@Override
	public String getVersion() {
		return "1.0";
	}

	/**
	 * Synchronizes the tool on the given stepButton
	 */
	public void update(StepButton sb) {
		super.update(sb);

		InitialParameters ip = stepButton.getStep().getProject().getModel().getSettings();

		JComponent comp = Tools.getIntrospectionPanel(ip);

		removeAll();
		add(comp);

		revalidate();
		repaint();

	}

}
