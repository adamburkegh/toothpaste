package qut.pm.spm.miner;

import java.io.File;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.deckfour.xes.classification.XEventClassifier;
import org.deckfour.xes.model.XLog;
import org.processmining.contexts.uitopia.UIPluginContext;
import org.processmining.framework.plugin.PluginContext;
import org.processmining.models.graphbased.directed.petrinet.StochasticNet;
import org.processmining.models.semantics.petrinet.Marking;

import qut.pm.spm.StochasticNetLogMiner;
import qut.pm.spm.AcceptingStochasticNet;

/**
 * Very like <class>RoggeSoltiSMP</class>, but invoking a local copy which has additional tracing
 * and tweaks
 * 
 * @author burkeat
 *
 */
public class RoggeSoltiSMPTracing implements StochasticNetLogMiner {
	
	private static Logger LOGGER = LogManager.getLogger();
	
	private AcceptingStochasticNet snDescriptor = null;

	public String getShortID() {
		return "rssmt";
	}
	
	public String getReadableID() {
		return "Rogge-Solti StochasticMinerPlugin (Tracing)";
	}
	
	public void run(PluginContext pc, XLog log, XEventClassifier classifier, File outputModelFile)
					throws Exception
	{
		// This is the one Sander uses in his CAISE paper
		Object[] objects = StochasticMinerPlugin.discoverStochNetModel((UIPluginContext)pc, log);
		LOGGER.debug("Discovery complete");
		StochasticNet net = (StochasticNet) objects[0];
		Marking marking = (Marking) objects[1];
		snDescriptor = new AcceptingStochasticNet(outputModelFile.getName(),net, marking);
	}

	@Override
	public AcceptingStochasticNet getStochasticNetDescriptor() {
		return snDescriptor;
	}

	public boolean isDirectDiscovery() {
		return true;
	}
	
}
