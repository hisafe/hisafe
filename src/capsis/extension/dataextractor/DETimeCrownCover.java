package capsis.extension.dataextractor;

import capsis.extension.PaleoDataExtractor;
import capsis.extension.dataextractor.format.DFCurves;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.MethodProvider;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.GenericExtensionStarter;
import capsis.util.methodprovider.CrownCoverProvider;
import capsis.util.methodprovider.OutOfCoverCrownCoverProvider;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;

/**
 *
 * @author Thomas Bronner <thomas.bronner@gmail.com>
 */
public class DETimeCrownCover extends PaleoDataExtractor implements DFCurves {

    static {
        Translator.addBundle("capsis.extension.dataextractor.DETimeCrownCover");
    }

    // nb-08.08.2018
    //public static final String VERSION = "1.0";

    protected List<List<? extends Number>> curves = new ArrayList<>();
    protected List<List<String>> labels = new ArrayList<>();

    public DETimeCrownCover() {
    }

    public DETimeCrownCover(GenericExtensionStarter s) {
        super(s);
    }

    @Override
    public void setConfigProperties() {
        //addBooleanProperty("perHectare"); // fc-27.8.2021 Removed HECTARE property
    }

    @Override
    public boolean doExtraction() throws Exception {
        if (upToDate) {
            return true;
        }
        if (step == null) {
            return false;
        }
        // Retrieve method provider
        MethodProvider methodProvider = step.getProject().getModel().getMethodProvider();
        try {
            // Retrieve Steps from root to this step
            List<Step> steps = step.getProject().getStepsFromRoot(step);
            List<Integer> age = new ArrayList<>();
            List<Double> tcc = new ArrayList<>();//total crown cover %
            List<Double> occc = new ArrayList<>();//out of cover crown cover %
            for (Step s : steps) {
                GScene stand = s.getScene();
                Collection trees = doFilter(stand);
                age.add(stand.getDate());
                tcc.add(100d * ((CrownCoverProvider) methodProvider).getCrownCover(stand, trees) / 10000d);
                occc.add(100d * ((OutOfCoverCrownCoverProvider) methodProvider).getOutOfCoverCrownCover(stand, trees) /10000d);
            }
            labels.clear();
            //labels
            labels.add(new ArrayList<>()); // no x labels
            List<String> y1Labels = new ArrayList<>();
            y1Labels.add(Translator.swap("DETimeCrownCover.totalCrownCoverPct"));
            labels.add(y1Labels);
            List<String> y2Labels = new ArrayList<>();
            y2Labels.add(Translator.swap("DETimeCrownCover.outOfCoverCrownCoverPct"));
            labels.add(y2Labels);
            //data
            curves.clear();
            curves.add(age);
            curves.add(tcc);
            curves.add(occc);

        } catch (Exception exc) {
            Log.println(Log.ERROR, "DETimeCrownCover.doExtraction ()", "Exception caught : ", exc);
            return false;
        }

        upToDate = true;
        return true;
    }

    @Override
    public boolean matchWith(Object referent) {
        try {
            if (!(referent instanceof GModel)) {
                return false;
            }
            GModel m = (GModel) referent;
            MethodProvider mp = m.getMethodProvider();
            if (!(mp instanceof CrownCoverProvider) || !(mp instanceof OutOfCoverCrownCoverProvider)) {
                return false;
            }
        } catch (Exception e) {
            Log.println(Log.ERROR, "DETimeCrownCover.matchWith ()", "Error in matchWith ()", e);
            return false;
        }
        return true;
    }

    @Override
    public String getName() {
        return getNamePrefix() + Translator.swap("DETimeCrownCover.name");
    }

    @Override
    public String getAuthor() {
        return "Thomas Bronner <thomas.bronner@gmail.com>";
    }

    @Override
    public String getDescription() {
        return Translator.swap("DETimeCrownCover.description");
    }

    @Override
    public String getVersion() {
        return "1.0";
    }
   
    @Override
    public List<List<? extends Number>> getCurves() {
        return curves;
    }

    @Override
    public List<List<String>> getLabels() {
        return labels;
    }

    @Override
    public List<String> getAxesNames() {
        return Arrays.asList(new String[]{
            Translator.swap("DETimeCrownCover.age"),
            Translator.swap("DETimeCrownCover.crownCoverPct")});
    }

    @Override
    public int getNY() {
        return 2;
    }
}
