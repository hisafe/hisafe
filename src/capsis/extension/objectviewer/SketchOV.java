/*
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 *
 * Copyright (C) 2000-2001  Francois de Coligny
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 */
package capsis.extension.objectviewer;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;
import javax.swing.JToolBar;

import capsis.defaulttype.SceneConnected;
import capsis.extension.AbstractObjectViewer;
import capsis.gui.DialogWithClose;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.Relay;
import capsis.kernel.Step;
import jeeb.lib.sketch.gui.Panel3D;
import jeeb.lib.sketch.gui.SketcherManager;
import jeeb.lib.sketch.item.Grid;
import jeeb.lib.sketch.kernel.AddInfo;
import jeeb.lib.sketch.kernel.CenterManager;
import jeeb.lib.sketch.kernel.SimpleAddInfo;
import jeeb.lib.sketch.kernel.SketchController;
import jeeb.lib.sketch.kernel.SketchEvent;
import jeeb.lib.sketch.kernel.SketchLinkable;
import jeeb.lib.sketch.kernel.SketchLinker;
import jeeb.lib.sketch.kernel.SketchModel;
import jeeb.lib.sketch.scene.gui.TreeView;
import jeeb.lib.sketch.scene.kernel.DigitalTerrainModel;
import jeeb.lib.sketch.scene.kernel.SceneFacade;
import jeeb.lib.sketch.util.SketchTools;
import jeeb.lib.util.AmapDialog;
import jeeb.lib.util.AmapTools;
import jeeb.lib.util.Disposable;
import jeeb.lib.util.IconLoader;
import jeeb.lib.util.Log;
import jeeb.lib.util.Namable;
import jeeb.lib.util.Translator;
import jeeb.lib.util.Vertex3d;

/**
 * A 3D viewer for a scene based on the sketch library. Based on the
 * SketchLinker of the module. Compatible only if a SketchLinker is found.
 * 
 * @author F. de Coligny - September 2009
 */
public class SketchOV extends AbstractObjectViewer implements Disposable, ActionListener, SketchController, Namable {

	static {
		Translator.addBundle("capsis.extension.objectviewer.SketchOV");
	}
	
	// nb-07.08.2018
	//static public final String NAME = Translator.swap("SketchOV");
	//static public final String DESCRIPTION = Translator.swap("SketchOV.description");
	//static public final String AUTHOR = "F. de Coligny";
	//static public final String VERSION = "1.1";

	// Main components of the 3D scene editor
	private SceneFacade sketchFacade;
	private SketchModel sceneModel;
	private Panel3D panel3D;
	private SketcherManager sketcherManager;

	private JTextField statusBar;
	private JButton preferences;
	private JComponent preferencePanel;
	private AmapDialog preferenceDialog;

	private SketchLinker linker;

	/**
	 * Default constructor.
	 */
	public SketchOV() {
	}

	public void init(Collection s) throws Exception {
		try {
			createUI();

			// fc-5.7.2018 added thisIsAReselection
			boolean thisIsAReselection = false;
			show(new ArrayList(s), thisIsAReselection);

		} catch (Exception e) {
			Log.println(Log.ERROR, "SketchOV.init ()", "Exception at init time", e);
			throw e; // fc-4.11.2003 object viewers may throw exception
		}
	}

	/**
	 * Extension dynamic compatibility mechanism. This matchwith method checks
	 * if the extension can deal (i.e. is compatible) with the referent.
	 */
	static public boolean matchWith(Object referent) {
		try {

			// referent is a Collection, a candidate selection
			// Warning: there may be trees and cells in the referent collection
			// Do not match only if all the elements are trees of a given type
			// (cells also)

			// Look if the relay is SketchLinkable to get a SketchLinker
			SketchLinker linker = getSketchLinker((Collection) referent);
			return linker != null;

		} catch (Exception e) {
			Log.println(Log.ERROR, "SketchOV.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}
	}

	@Override
	public String getName() {
		return Translator.swap("SketchOV.name");
	}

	@Override
	public String getAuthor() {
		return "F. de Coligny";
	}

	@Override
	public String getDescription() {
		return Translator.swap("SketchOV.description");
	}

	@Override
	public String getVersion() {
		return "1.1";
	}

	/**
	 * Ask the module SketchLinker to the Relay of the module. We expect at
	 * least one instance of SceneConnected in the candidateSelection. If not
	 * found or Relay not instanceof SketchLinkable, returns null.
	 */
	static private SketchLinker getSketchLinker(Collection candidateSelection) {
		try {

			// fc-7.6.2018 review the strategy to find the SketchLinkable, added
			// an interface for getScene ()

			// 1. make a working set (no duplicates needed) with all the objects
			// in candidateSelection (we possibly could add extra linked objects
			// that would implement SceneConnected in this working set)
			Set workingSet = new HashSet(candidateSelection);

			// 2. Try to find a getScene () method and get a Scene reference
			GScene scene = null;
			for (Object o : workingSet) {

				if (o instanceof SceneConnected)
					scene = ((SceneConnected) o).getScene();

				if (scene != null)
					break; // found

			}

			// If scene could not be found, no way to find the SketchLinker
			if (scene == null)
				return null;

			// 3. Try the module relay (generally knows the SketchLinker)
			Step step = scene.getStep();
			GModel model = step.getProject().getModel();
			Relay relay = model.getRelay();

			// If relay is not SketchLinkable, return null
			if (!(relay instanceof SketchLinkable))
				return null;

			// Found the linker
			SketchLinker linker = ((SketchLinkable) relay).getSketchLinker();
			return linker;

		} catch (Exception e) {
			Log.println(Log.INFO, "SketchOV.getSketchLinker ()",
					"Could not get a sketchLinker from any element of this collection: "
							+ AmapTools.toString(candidateSelection),
					e);

			return null;
		}
	}

	/**
	 * Disposable.
	 */
	public void dispose() {
	}

	/**
	 * Some button was hit...
	 */
	public void actionPerformed(ActionEvent evt) {
		if (evt.getSource().equals("Status")) { // fc - 1.6.2007
			String message = (String) evt.getActionCommand();
			statusBar.setText(message);

		} else if (evt.getSource().equals(preferences)) {

			if (preferenceDialog == null) {
				boolean modal = false;
				boolean withControlPanel = false;

				// memoSize = true, memoLocation = true
				preferenceDialog = new DialogWithClose(this, preferencePanel, Translator.swap("Shared.preferences"),
						modal, withControlPanel, true, true);

			} else {
				// Make it visible / invisible...
				preferenceDialog.setVisible(!preferenceDialog.isVisible());
			}

		}
	}

	/**
	 * OVSelector framework.
	 */
	public Collection show(Collection candidateSelection, boolean thisIsAReselection) {

		realSelection = updateUI(candidateSelection);

		// fc-6.7.2018 In case this is not a reselection (e.g. on new step
		// synchronisation), we can center the scene
		Collection items = linker.getUpdatedItems();
		if (items != null && !items.isEmpty() && !thisIsAReselection)
			try {

				// fc-6.7.2018 Centering must occur at opening time and when
				// user changes selection. It must not occur when the opened 3D
				// tool is synchronized on another step (must keep its
				// centering and point of view). thisIsAReselection means the
				// pov and center must not be changed

				// Make a copy for centering only
				Collection copy = new ArrayList(items);

				// If there is a terrain (or a grid), remove it (covers all the
				// scene, not helping for accurate centering if a part only of
				// the scene
				// was selected)
				for (Iterator i = copy.iterator(); i.hasNext();) {
					Object o = i.next();
					if (o instanceof DigitalTerrainModel || o instanceof Grid) {

						i.remove();
					}
				}

				Vertex3d proposedCenter = CenterManager.getCenterVertex(copy); // fc-5.7.2018

				SketchEvent e = new SketchEvent(this, sceneModel, SketchEvent.CENTER_CHANGED, proposedCenter);
				sceneModel.broadcastEvent(e); // in particular for Panel3D

			} catch (Exception e) {
				// Trouble during centering, ignore
				Log.println(Log.ERROR, "SketchOV.show ()", "Exception during centering, ignored", e);
				e.printStackTrace(System.out); // for debugging only
			}

		return realSelection;
	}

	/**
	 * User interface definition.
	 */
	protected void createUI() {
		setLayout(new BorderLayout());

		try {
			statusBar = new JTextField();
			statusBar.setEditable(false);

			JDialog topDialog = null; // we are in a JPanel...

			sketchFacade = new SceneFacade(topDialog);
			sceneModel = sketchFacade.getSketchModel();
			sceneModel.setEditable(this, true); // possible to add items in it
												// (by program)
			panel3D = sketchFacade.getPanel3D();
			sketchFacade.addStatusListener(this);

			JPanel p = panel3D.getPanel3DWithToolBar(BorderLayout.EAST, true, false, false, false, false);
			panel3D.setPovCenter(0, 0, 0);

			// fc-3.6.2014 change Initial zoom factor
			panel3D.setZoomFactor(0.2f);

			sketcherManager = sketchFacade.getSketcherManager();

			add(p, BorderLayout.CENTER);
			add(statusBar, BorderLayout.SOUTH);

			// Add a preferences button in the toolbar of the Panel3D
			createPreferencePanel(panel3D.getToolBar());

			// Add a grid
			Grid grid = new Grid(); // Grid item
			AddInfo addInfo = new SimpleAddInfo(grid.getType(), SketchTools.inSet(grid));
			sceneModel.getUndoManager().undoableAddItems(this, addInfo);

		} catch (Exception e) {
			Log.println(Log.ERROR, "SketchOV.createUI ()", "Exception, ignored", e);
		}
	}

	/**
	 * Shows subject, returns what it effectively shown.
	 */
	protected Collection updateUI(Collection subject) {
		try {

			if (subject == null) {
				subject = new ArrayList();
			} // fc - 7.12.2007 - ovs should accept null subject

			// Ask the linker to 'draw the scene'
			linker = getSketchLinker(subject);
			linker.updateSketch(null, subject, sceneModel); // scene is null:
															// we do not
															// have the ref
															// here

			// Reset selection / clear undo stack
			sceneModel.getUndoManager().undoableResetSelection(this);
			sceneModel.getUndoManager().clearMemory();

			// Add a preferences button in the toolbar of the Panel3D
			// Moved here - fc - 22.5.2009
			// createPreferencePanel (panel3D.getToolBar ());

			// Centering is now managed in Panel3D and occurs at opening
			// time and when user chenges selection but not during steps
			// synchronisation // fc-6.7.2018

			Collection accurateSelection = linker.getUpdatedUserObjects();
			return accurateSelection; // shown trees

		} catch (Exception e) {
			Log.println(Log.ERROR, "SketchOV.updateUI ()", "Exception", e);
			statusBar.setText(Translator.swap("SketchOV.couldNotOpenSketcherSeeLog"));
		}
		return Collections.EMPTY_LIST; // showed nothing
	}

	/**
	 * Build a preference panel for the ObjectViewer
	 */
	private void createPreferencePanel(JToolBar toolBar) {

		if (preferencePanel != null)
			return;

		JTabbedPane tabs = new JTabbedPane();

		tabs.add(sketcherManager.getName(), sketcherManager);

		// Add a table
		TreeView treeView = sketchFacade.getTreeView();
		tabs.add(treeView.getName(), treeView);

		preferencePanel = tabs;

		ImageIcon icon = IconLoader.getIcon("option_24.png");
		preferences = new JButton(icon);
		preferences.setToolTipText(Translator.swap("Shared.preferences"));
		preferences.addActionListener(this);
		toolBar.add(preferences);

	}

}
