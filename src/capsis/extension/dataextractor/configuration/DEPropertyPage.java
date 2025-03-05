/** 
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 1999-2010 INRA 
 * 
 * Authors: F. de Coligny, S. Dufour-Kowalski, 
 * 
 * This file is part of Capsis
 * Capsis is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * Capsis is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU lesser General Public License
 * along with Capsis.  If not, see <http://www.gnu.org/licenses/>.
 *
 */

package capsis.extension.dataextractor.configuration;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextPane;
import javax.swing.text.Style;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyleContext;
import javax.swing.text.StyledDocument;

import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;
import capsis.app.CapsisExtensionManager;
import capsis.extensiontype.DataBlock;
import capsis.extensiontype.DataExtractor;
import capsis.kernel.GModel;
import capsis.util.MuteConfigurationPanel;
import capsis.util.TextInterface;

/**
 * A property panel for data extractors.
 * 
 * @author F. de Coligny - april 2003
 */
public class DEPropertyPage extends MuteConfigurationPanel {

	/**
	 * Constructor. Receives one extractor instance and the name of some
	 * methodProvider to be documented for each model in the extractor's data
	 * block. No doc found -> nothing appears.
	 */
	public DEPropertyPage(DataExtractor ex, DataBlock db) {
		super(Translator.swap("Shared.properties"), new JPanel()); // JPanel
																	// will be
																	// replaced
																	// later

		// 1. Extension property panel
		String className = ex.getClass().getName();
		JTextPane area = new JTextPane();
		CapsisExtensionManager.getInstance().getPropertyPanel(className, area);

		// Extra text
		StringBuffer b = new StringBuffer();

		// 2. Optional: MethodProviders' documentation
		Collection<String> documentKeys = ex.getDocumentationKeys();

		if (documentKeys != null && !documentKeys.isEmpty()) {

			Set<GModel> models = new HashSet<GModel>(); // no duplicates
			for (Iterator i = db.getDataExtractors().iterator(); i.hasNext();) {
				DataExtractor e = (DataExtractor) i.next();
				GModel m = e.getStep().getProject().getModel();
				models.add(m);
			}

			for (GModel m : models) {
				String modelName = m.getIdCard().getModelName();
				String modelPackageName = m.getIdCard().getModelPackageName();

				StringBuffer aux = new StringBuffer();
				aux.append(modelName);
				aux.append(" : ");
				boolean changed = false;
				for (String key : documentKeys) {
					// ex: pp3.NProvider, mountain.NProvider,
					// samsara.BelowGroundBiomProvider...
					String k = modelPackageName + "." + key;
					String doc = Translator.swap(k);
					if (doc.equals(k)) {
						continue;
					} // no translation -> next key

					changed = true;
					aux.append(doc);
					aux.append("\n");
				}
				if (changed) {
					b.append(aux.toString());
				}
			}

		}

		// Additional text (optional)
		if (ex instanceof TextInterface) {

			String t = ((TextInterface) ex).getText();
			b.append("\n" + t);

		}

		// Write b to area if non empty
		if (b.length() != 0) {

			// Trace to check the good writing of b
			System.out.println("MethodProviders' documentation: " + b.toString());

			try {
				// Append the new information in the propertyPanel
				StyledDocument doc = area.getStyledDocument();
				Style def = StyleContext.getDefaultStyleContext().getStyle(StyleContext.DEFAULT_STYLE);

				Style regular = doc.addStyle("regular", def);
				StyleConstants.setFontFamily(def, "SansSerif");

				Style s = doc.addStyle("italic", regular);
				StyleConstants.setItalic(s, true);

				s = doc.addStyle("bold", regular);
				StyleConstants.setBold(s, true);

				doc.insertString(doc.getLength(), "\n\n" + b.toString(), doc.getStyle("regular"));

			} catch (Exception e) {
				Log.println(Log.ERROR, "DEPropertyPage.c ()", "Could not append text in JTextPane: \n" + b.toString(),
						e);
			}

		}

		JScrollPane scroll = new JScrollPane(area);

		scroll.setPreferredSize(new Dimension (200, 200)); // fc-26.11.2014
		
		add(scroll, BorderLayout.CENTER);
	}

}
