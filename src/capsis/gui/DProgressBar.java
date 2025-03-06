package capsis.gui;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JProgressBar;
import javax.swing.SwingWorker;

import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.ProgressDispatcher;
import jeeb.lib.util.ProgressListener;

/**
 * This dialog only contains a progress bar that listen to the ProgressDispatcher
 * static object.
 * @author Mathieu Fortin - April 2010
 *
 */
@SuppressWarnings("serial")
public class DProgressBar extends JDialog implements ProgressListener, PropertyChangeListener {

	public static final Font LABEL_FONT = new Font("ArialBold12",Font.BOLD,12);

	protected JProgressBar progressBar;
	private String titleString;
	private String labelString;
	private Component caller;

	public DProgressBar(String titleString, String labelString) {
		super();
		init(titleString, labelString);
	}

	public DProgressBar(JDialog caller, String titleString, String labelString) {
		super(caller);
		this.caller = caller;
		init(titleString, labelString);
	}
	
	private void init(String titleString, String labelString) {
		progressBar = new JProgressBar();
		progressBar.setStringPainted(true);

		this.titleString = titleString;
		this.labelString = labelString;
		
		createUI();
		setModal(true);
		pack();
		ProgressDispatcher.addListener(this);
	}
	
	@Override
	public void setMinMax(int min, int max) {
		progressBar.setMinimum(min);
		progressBar.setMaximum(max);
	}

	@Override
	public void setValue(int value) {
		progressBar.setValue(value);
	}

	@Override
	public void stop() {
		ProgressDispatcher.removeListener(this);
		if (this.isVisible()) {
			dispose();
		}
	}

	
	private void createUI() {
		ColumnPanel c1 = new ColumnPanel();
		LinePanel l1 = new LinePanel();

		JLabel label = new JLabel(labelString);
		label.setFont(LABEL_FONT);
		c1.add(label);
		l1.add(progressBar);
		l1.addStrut0();
		c1.add(l1);
		getContentPane().add(c1);
		setTitle(titleString);
		Dimension dim = new Dimension(250,100);
		setMinimumSize(dim);
		setSize(dim);
		setLocationRelativeTo(caller);
		setDefaultCloseOperation(JDialog.DO_NOTHING_ON_CLOSE);
	}
	
	
	@SuppressWarnings("rawtypes")
	public static void showProgressBarWhileJob(JDialog caller,
			SwingWorker job, 
			String title, 
			String message) {
		
		if (title == null) {
			title = "Progress";
		}
		
		if (message == null) {
			message = "Performing task";
		}
		
		DProgressBar dlgProgress; 
		if (caller != null) {
			dlgProgress = new DProgressBar(caller, title, message);
		} else {
			dlgProgress = new DProgressBar(title, message);
		}
		
		job.addPropertyChangeListener(dlgProgress);
		dlgProgress.setMinMax(0, 100);
		job.execute();
		
		dlgProgress.setVisible(true);
		
	}
	
	@SuppressWarnings("rawtypes")
	public static void showProgressBarWhileJob(SwingWorker job, 
			String title, 
			String message) {
		showProgressBarWhileJob(null, job, title, message);
	}


	@Override
	public void propertyChange(PropertyChangeEvent evt) {
		if ("state".equals(evt.getPropertyName()) && SwingWorker.StateValue.DONE == evt.getNewValue()) {
            setVisible(false);
            ProgressDispatcher.removeListener(this);
            dispose();
        } else if ("progress".equals(evt.getPropertyName())) {
			if (isVisible() == false) {
				setVisible(true);
			}
			setValue((Integer) evt.getNewValue());
		}  
	}

}
