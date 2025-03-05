package capsis.extension.intervener.rankintervener;

import capsis.defaulttype.Tree;
import capsis.defaulttype.TreeCollection;
import capsis.defaulttype.TreeList;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.Intervener;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.OptionalInt;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JPanel;
import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;


/**
 * Experimental Dbh rank intervener for Simcop/Gymnos
 *
 * @author thomas.bronner@gmail.com 2024
 */
public class RankIntervener implements Intervener {

    protected TreeList scene;
    protected GModel model;
    protected Step step;
    //input collections from ranks file
    private Map<Integer, Integer> targetByYear = new HashMap<>();
    private Map<Integer, List<Integer>> ranksPerYear = new HashMap<>();
    //output collections resulting of the thinning algorithm
    protected Set<Integer> treeIdsToThin = new HashSet<>();
    protected Set<Integer> failedPicks = new HashSet<>();
    protected Set<Integer> successfulPicks = new HashSet<>();
    protected List<String> log = new ArrayList<>();

    protected File csvFile;

    static {
        Translator.addBundle("capsis.extension.intervener.rankintervener.RankIntervener");
    }

    public RankIntervener(TreeList scene, GModel model, Step step, File csvFile) throws Exception {
        this.scene = scene;
        this.model = model;
        this.step = step;
        this.csvFile = csvFile;
        readThinByRankFile(csvFile);
    }

    static public boolean matchWith(Object referent) {
        try {
            if (!(referent instanceof GModel)) {
                return false;
            }
            GModel m = (GModel) referent;
            GScene s = ((Step) m.getProject().getRoot()).getScene();
            if (!(s instanceof TreeList)) {
                return false;
            }


        } catch (Exception e) {
            Log.println(Log.ERROR, "RankIntervener.matchWith ()", "Error in matchWith () (returned false)", e);
            return false;
        }

        return true;
    }

    @Override
    public void init(GModel model, Step step, GScene scene, Collection concernedTrees) {
        this.model = model;
        this.step = step;
        this.scene = (TreeList) scene;
    }

    @Override
    public boolean initGUI() throws Exception {

        return true;
    }

    private void gui() throws Exception {
        JDialog dialog = new JDialog();
        // Create a JButton to trigger the file chooser
        JButton openButton = new JButton(Translator.swap("thinByRank.pleaseInputRankFile"));
        // Create a JFileChooser
        JFileChooser fileChooser = new JFileChooser();
        // Add action listener to the button
        openButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                // Show the file chooser dialog
                int result = fileChooser.showOpenDialog(dialog);
                // Check if a file was selected
                if (result == JFileChooser.APPROVE_OPTION) {
                    File selectedFile = fileChooser.getSelectedFile();
                    try {
                        csvFile = selectedFile;
                        dialog.dispose();
                    } catch (Exception ex) {
                        Log.println(Log.ERROR, "RankIntervener.initGUI ()",
                                ex.getLocalizedMessage());
                    }
                }
            }
        });
        // Create a JPanel and add the button to it
        JPanel panel = new JPanel();
        panel.add(openButton);
        // Add the panel to the dialog
        dialog.getContentPane().add(panel);
        // Set dialog properties
        dialog.setTitle(getName());
        dialog.setSize(300, 150);
        dialog.setLocationRelativeTo(null); // Center the dialog on the screen
        dialog.setModal(true);
        dialog.setVisible(true);
        //read the provided csv
        readThinByRankFile(csvFile);
    }

    @Override
    public boolean isReadyToApply() {
        if (model == null) {
            Log.println(Log.ERROR, "RankIntervener.isReadyToApply ()",
                    "model is null. RankIntervener is not appliable.");
            return false;
        }
        if (scene == null) {
            Log.println(Log.ERROR, "RankIntervener.isReadyToApply ()",
                    "stand is null. RankIntervener is not appliable.");
            return false;
        }
        if (step == null) {
            Log.println(Log.ERROR, "RankIntervener.isReadyToApply ()",
                    "step is null. RankIntervener is not appliable.");
            return false;
        }
        if (ranksPerYear.isEmpty()) {
            Log.println(Log.ERROR, "RankIntervener.isReadyToApply ()",
                    "Rank per age is empty. RankIntervener is not appliable.");
            return false;
        }
        if (targetByYear.isEmpty()) {
            Log.println(Log.ERROR, "RankIntervener.isReadyToApply ()",
                    "Target number of tree per age is empty. RankIntervener is not appliable.");
            return false;
        }
        return true;
    }

    @Override
    public Object apply() throws Exception {
        if (!isReadyToApply()) {
            throw new Exception("RankIntervener.apply () - Wrong input parameters, see Log");
        }
        prepareThinByRank();
        if (!treeIdsToThin.isEmpty()) {
            //cut trees 
            for (Object id : treeIdsToThin) {
                Tree tree = ((TreeCollection) scene).getTree((int) id);
                ((TreeCollection) scene).removeTree(tree);
            }
            scene.setInterventionResult(true);
            showLog();
        } else {
            scene.setInterventionResult(false);
        }
        return scene;
    }

    public void prepareThinByRank() throws Exception {
        //triggered if current year/age is part of the list
        if (targetByYear.containsKey(scene.getDate())) {
            TreeList interventionScene = (TreeList) scene.getInterventionBase();
            interventionScene.setInterventionResult(false);
            //order trees by dbh
            List<Tree> byDbh = interventionScene.getTrees("alive").stream()
                    .sorted(Comparator.comparing(Tree::getDbh))
                    .collect(Collectors.toList());
            //202401 reverse dbh ordering
            Collections.reverse(byDbh);
            int initialAliveTrees = byDbh.size();
            //target nb of tree define the size of the thinning
            int targetNumberOfTreeToThin = scene.getTrees("alive").size() - targetByYear.get(scene.getDate());
            //get the list of ranks for this year
            List<Integer> ranksToThin = ranksPerYear.get(interventionScene.getDate());
            //202401 make sure ranks are ordered from big to small
            Collections.sort(ranksToThin);
            Collections.reverse(ranksToThin);
            int initialRanksToThinSize = ranksToThin.size();
            if (initialRanksToThinSize == 0) {
                throw new Exception("Rank list empty for step " + step);
            }
            //when there is more trees in rank list (observation) than in the simulation :
            //remove from the the frst ranks in the ranks list
            List<Integer> removedFromRanks = new ArrayList<>();
            while (ranksToThin.iterator().hasNext() && ranksToThin.size() > targetNumberOfTreeToThin) {
                Integer first = ranksToThin.iterator().next();
                ranksToThin.remove(first);
                removedFromRanks.add(first);
            }
            //when there is more alive tree in simulation than in ranks(obersvation), expand ranks to adjust:
            List<Integer> addedToRanks = new ArrayList<>();
            //add only non existing largest ranks
            OptionalInt oMax = ranksToThin.stream().mapToInt(r -> r.intValue()).max();
            if (oMax.isPresent()) {
                int max = oMax.getAsInt() + 1;
                while (ranksToThin.size() < targetNumberOfTreeToThin) {
                    if (!ranksToThin.contains(max) && max < (byDbh.size() + 1)) {
                        ranksToThin.add(0, max);
                        addedToRanks.add(0, max);
                    }
                    max++;
                }
            }
            //pick the tree to thin using ranks, 
            List<Tree> treesToThin = new ArrayList<>();
            List<Integer> failedPicks = new ArrayList<>();
            List<Integer> successfulPicks = new ArrayList<>();
            int nbPicked = 0;
            for (Integer rank : ranksToThin) {
                try {
                    Tree t = byDbh.get(rank - 1);
                    treesToThin.add(t);
                    successfulPicks.add(rank);
                    //stop if number of tree reached
                    nbPicked++;
                    if (nbPicked == targetNumberOfTreeToThin) {
                        break;
                    }
                } catch (Exception e) {
                    failedPicks.add(rank);
                }
            }
            //diplay  log
            System.out.println();
            System.out.println("------------------------Thin by rank for year/age " + scene.getDate() + "------------------------");
            System.out.println("Target is " + targetNumberOfTreeToThin + " trees to thin out of " + initialAliveTrees + " trees in scene whith " + initialRanksToThinSize + " ranks from wich to pick");
            if (!removedFromRanks.isEmpty()) {
                System.out.println("Rank list adjusted by removing : " + removedFromRanks);
            }
            if (!addedToRanks.isEmpty()) {
                System.out.println("Rank list adjusted by adding : " + addedToRanks);
            }
            if (!treesToThin.isEmpty()) {
                String ranks = successfulPicks.stream()
                        .map(String::valueOf)
                        .collect(Collectors.joining(","));
                System.out.println(successfulPicks.size() + " thinned ranks : " + ranks);
            }
            if (!failedPicks.isEmpty()) {
                String failedRanks = failedPicks.stream()
                        .map(String::valueOf)
                        .collect(Collectors.joining(","));
                System.out.println(failedPicks.size() + " failed thins for ranks : " + failedRanks);
            }
            System.out.println("Remaining trees in scene : " + interventionScene.getTrees().size());
            System.out.println("----------------------------------------------------------------------------");
        }
    }

    private void showLog() {
        for (String s : log) {
            System.out.println(s);
        }
    }

    /**
     * read the file describing thin by age using dbh ranks with a target number
     * of tree the file is organised like a database table : 1 line per
     * association of age, rank and target_trees, but there is only one
     * target_trees per age, hence the repetition
     */
    public void readThinByRankFile(File file) throws IOException, Exception {
       
    }

    @Override
    public String getSubType() {
        return Translator.swap("thinByRank.subType");
    }

    @Override
    public String getName() {
        return Translator.swap("thinByRank.name");
    }

    @Override
    public String getAuthor() {
        return "Thomas Bronner";
    }

    @Override
    public String getDescription() {
        return Translator.swap("thinByRank.description");
    }

    @Override
    public String getVersion() {
        return "1.0";

    }

}
