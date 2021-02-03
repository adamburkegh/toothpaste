package qut.pm.spm.conformance;

import org.deckfour.xes.classification.XEventClassifier;
import org.deckfour.xes.model.XLog;
import org.processmining.framework.plugin.PluginContext;

import qut.pm.spm.AcceptingStochasticNet;
import qut.pm.spm.SPNQualityCalculator;
import qut.pm.spm.TaskStats;

/**
 * Does nothing, unsuccessfully.
 * 
 * For testing.
 * 
 * @author burkeat
 *
 */
public class ErrorCalculator implements SPNQualityCalculator{

	@Override
	public String getReadableId() {
		return "Calculator Which Errors";
	}

	@Override
	public void calculate(PluginContext context, AcceptingStochasticNet net, XLog log, 
			XEventClassifier classifier, TaskStats stats) throws Exception 
	{
		throw new Exception("ErrorCalculator always errors");
	}

}
