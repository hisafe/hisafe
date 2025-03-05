package capsis.extension.dataextractor.configuration.propertiesmanager;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import capsis.util.Spiable;
import capsis.util.Spy;
import jeeb.lib.util.AmapTools;
import jeeb.lib.util.Log;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.Translator;
import jeeb.lib.util.annotation.Ignore;
import jeeb.lib.util.annotation.Param;

/**
 * This class is related to setProperties. A tool to choose selected values
 * among a list of possible values. E.g. key=treeIds
 * value=SetPropertyItemSelector with possibleValues = 1, 2, 3 and
 * SelectedValues = 2, 3.
 * 
 * @author F. de Coligny - September 2021
 */
public class SetPropertyItemSelector implements Spiable, Serializable {

	// fc-13.9.2021 Was an inner class in AbstractDataExtractor

	// fc-3.7.2018 Changed all sets by lists (keep order) below

	// fc-3.8.2018 The possibleValues list is not saved (@Ignore) because it
	// changes often (open the extractor in another project) and
	// selecteValues is saved (@Param) because this is was we would like to
	// remember for the user. See additional saving system with
	// Settings.setProperty ()

	// @Ignore: do not save the possible values (may change at reopening
	// time)
	@Ignore
	public List<String> possibleValues; // contains Strings

	// @Param: save the selected values
	@Param
	public List<String> selectedValues; // contains Strings

	/**
	 * Constructor
	 */
	public SetPropertyItemSelector(String[] possibleValues, String[] selectedValues) {

		this.possibleValues = new ArrayList<>();
		this.selectedValues = new ArrayList<>();

		setPossibleValues(possibleValues);
		setSelectedValues(selectedValues);

	}

	public void setPossibleValues(String[] possibleValues) {
		try {

			// fc-14.12.2007
			this.possibleValues.clear();

			for (int i = 0; i < possibleValues.length; i++) {
				this.possibleValues.add(possibleValues[i]);
			}
			clearSelectedValues();
		} catch (Exception e) {
			Log.println(Log.ERROR, "SetPropertyItemSelector.setPossibleValues ()", "Exception, passed", e);
		}
	}

	public void setSelectedValues(String[] selectedValues) {
		try {
			for (int i = 0; i < selectedValues.length; i++) {
				this.selectedValues.add(selectedValues[i]);
			}
		} catch (Exception e) {
			Log.println(Log.ERROR, "SetPropertyItemSelector.setSelectedValues ()", "Exception, passed", e);
		}
	}

	public void clearSelectedValues() {
		this.selectedValues.clear();
		action(null);
	}

	/**
	 * Check the format of the selected values, must be a list of strings included
	 * in possible values separated by commas. Returns true if all is correct.
	 */
	public boolean checkSelectedValues() {
		
		// fc-13.9.2021
		
		if (selectedValues.isEmpty ())
			return true; // no problem (...)
		
		// Faster on contains than a list
		Set<String> copy = new HashSet<String>(possibleValues);
		
		for (String item : selectedValues) {
			if (!copy.contains(item)) {
				
				MessageDialog.print(this, Translator.swap("SetPropertyItemSelector.selectionContainsAnUnknownItem")+" : "+item);
				return false;
				
			}
				
		}
		return true;
		
	}

	@Override
	public String toString() {
		return "SetPropertyItemSelector, possibleValues: " + AmapTools.toString(possibleValues) + ", selectedValues: "
				+ AmapTools.toString(selectedValues);
	}

	// Spiable equipment
	// fc-9.2.2011 Trying to fix a xml serialization bug (saves too much)
	transient private Spy spy;

	public void setSpy(Spy s) {
		spy = s;
	}

	public Spy getSpy() {
		return spy;
	}

	public void action(Object sth) {
		try {
			spy.action(this, sth);
		} catch (Exception e) {
		} // spy may be null
	}
	// end-of Spiable equipment

}
