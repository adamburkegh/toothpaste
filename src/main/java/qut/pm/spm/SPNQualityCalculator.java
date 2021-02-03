package qut.pm.spm;

import org.deckfour.xes.classification.XEventClassifier;
import org.deckfour.xes.model.XLog;
import org.processmining.framework.plugin.PluginContext;

public interface SPNQualityCalculator {

	public String getReadableId();
	public void calculate(PluginContext context, AcceptingStochasticNet net, XLog log, 
			XEventClassifier classifier, TaskStats stats) throws Exception;

}