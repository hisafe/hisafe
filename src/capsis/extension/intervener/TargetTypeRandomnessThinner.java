package capsis.extension.intervener;

import java.util.Collection;

import capsis.defaulttype.Numberable;
import capsis.defaulttype.TreeList;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.AbstractGroupableIntervener;
import capsis.kernel.extensiontype.Intervener;
import capsis.util.group.GroupableIntervener;
import capsis.util.group.GroupableType;
import jeeb.lib.util.AmapTools;
import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;

public class TargetTypeRandomnessThinner extends AbstractGroupableIntervener implements GroupableIntervener, Intervener {

	static {
		Translator.addBundle("capsis.extension.intervener.TargetTypeRandomnessThinner");
	}

	private boolean constructionCompleted;

	/**
	 * Extension dynamic compatibility mechanism. This matchWith method checks
	 * if the extension can deal (i.e. is compatible) with the referent.
	 */
	static public boolean matchWith(Object referent) {

		try {
			GModel model = (GModel) referent;
			GScene scene = ((Step) model.getProject().getRoot()).getScene();
			if (!(scene instanceof TreeList)) { return false; }
			TreeList tl = (TreeList)scene;
			Collection reps = AmapTools.getRepresentatives(tl.getTrees());
			for(Object o : reps){
				if ((o instanceof Numberable)) { return false; }
			}
		} catch (Exception e) {
			Log.println (Log.ERROR, "TargetTypeRandomnessThinner.matchWith()", "Error in matchWith() (returned false)", e);
			return false;
		}

		return true;		
	}

	@Override
	public String getName() {
		return Translator.swap("TargetTypeRandomnessThinner.name");
	}

	@Override
	public String getAuthor() {
		return "N. Beudez, G. Ligot";
	}

	@Override
	public String getDescription() {
		return Translator.swap("TargetTypeRandomnessThinner.description");
	}

	@Override
	public String getVersion() {
		return "1.0";
	}

	// fc-14.12.2020 Unused, deprecated
//	public void activate() {
//	}

	@Override
	public void init(GModel model, Step step, GScene scene, Collection elements) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public boolean initGUI() throws Exception {

		TargetTypeRandomnessThinnerDialog dlg = new TargetTypeRandomnessThinnerDialog(this);

		constructionCompleted = false;
		if (dlg.isValidDialog()) {
			constructionCompleted = true;
		}
		dlg.dispose();

		return constructionCompleted;
	}

	@Override
	public boolean isReadyToApply() {

		// Cancel on dialog in interactive mode -> constructionCompleted = false
		return constructionCompleted;
	}

	@Override
	public Object apply() throws Exception {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String getSubType() {
		return Translator.swap("TargetTypeRandomnessThinner.subType");
	}

	@Override
	public GroupableType getGrouperType() {
		// TODO Auto-generated method stub
		return null;
	}

}
