package capsis.kernel.automation;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.Vector;

import jeeb.lib.util.StringUtil;
import capsis.kernel.GModel;
import capsis.kernel.Step;
import capsis.util.SummaryExtractor;


/**
 * Represent the results of an automation
 * @author sdufour
 * AutomationSummary is Thread safe
 *
 */
public class AutomationSummary extends Vector<List<Object>> {
	
	private List<String> headers;
	private List<Map<String, Object>> varColumn; // supplementatary column index by header
	private SummaryExtractor extractor;

	
	public AutomationSummary (Automation automation, GModel model) {
		
		super();
		if(model.getSummaryClass() == null) { return; }
		extractor = new SummaryExtractor(model.getSummaryClass());
		headers = automation.getInputHeader();
		headers.addAll(extractor.getSummaryHeaders(model));
		varColumn = new ArrayList<Map<String, Object>>();
	}
	
	/** accessor */
	public SummaryExtractor getExtractor() {
		return extractor;
	}
	
	@Override
	synchronized public String toString() {
		
		if(extractor == null) { return "no summary"; }
		Set<String> vh = getVarHeaders();
		
		String ret = StringUtil.join(headers, "\t") + "\t" + StringUtil.join(vh, "\t") + "\n";
		
		for(int i=0; i<this.size(); i++) {
			List<Object> l = this.get(i);
			Map<String, Object> m = varColumn.get(i);
			
			String line = StringUtil.join(l, "\t");
			
			// add variable column
			for(String h : vh) {
				line += "\t";
				if(m != null && m.containsKey(h)) {
					line += m.get(h);
				}
			}
			
			ret += line + "\n";
		}
		
		
		return ret;
	}
	
	/** return header for variableColumn */
	protected Set<String> getVarHeaders() {
		
		TreeSet<String> vh = new TreeSet<String>();
		for(Map<String, Object> m : varColumn) {
			if(m != null) {
				vh.addAll(m.keySet());
			}
		}
		
		return vh;
		
	}
	
	@Override
	synchronized public boolean add(List<Object> l) {
		varColumn.add(null);
		return super.add(l);
		
	}
	
	synchronized public boolean add(List<Object> l, Map<String, Object> m) {
		varColumn.add(m);
		return super.add(l);
	}
	
	synchronized public boolean addLine(Automation a, GModel m, Step s) {
		
		List<Object> l = a.getInputParameters();
		l.addAll(extractor.getStepSummary(m, s));
				
		return this.add(l, extractor.getVarStepSummary(m, s));	
	}
	
	
	
}
