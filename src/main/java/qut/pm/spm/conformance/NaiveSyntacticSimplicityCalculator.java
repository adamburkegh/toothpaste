package qut.pm.spm.conformance;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.deckfour.xes.classification.XEventClassifier;
import org.deckfour.xes.info.XLogInfoFactory;
import org.deckfour.xes.model.XLog;
import org.processmining.framework.plugin.PluginContext;
import org.processmining.models.graphbased.directed.petrinet.StochasticNet;

import qut.pm.spm.AcceptingStochasticNet;
import qut.pm.spm.Measure;
import qut.pm.spm.SPNQualityCalculator;
import qut.pm.spm.TaskStats;

/**
 * Just count the entities. Not very good for comparing abstractions due to problems of 
 * representational bias, but pretty good for optimization within a representation type. 
 * 
 * @author burkeat
 *
 */
public class NaiveSyntacticSimplicityCalculator implements SPNQualityCalculator {

	private static Logger LOGGER = LogManager.getLogger();
	
	@Override
	public String getReadableId() {
		return "Naive SPN entity count";
	}
	
	@Override
	public void calculate(PluginContext context, AcceptingStochasticNet netD, XLog log, 
			XEventClassifier classifier, TaskStats stats) throws Exception 
	{
		StochasticNet net = netD.getNet();
		stats.setMeasure(Measure.MODEL_ENTITY_COUNT, 
				net.getPlaces().size() + net.getTransitions().size());
		stats.setMeasure(Measure.MODEL_EDGE_COUNT, net.getEdges().size() );
		eventBasedStats(log, net, classifier, stats);
	}

	private void eventBasedStats(XLog log, StochasticNet net, XEventClassifier classifier, 
			TaskStats stats) 
	{
		if (log.getInfo(classifier) == null) {
			XLogInfoFactory.createLogInfo(log, classifier);
		}
		int eventCount = log.getInfo(classifier).getNumberOfEvents();
		double npnes = normalizedEntitySimplicity(net.getEdges().size(), eventCount);
		LOGGER.debug("Normalized Simplicity: {}", npnes);
		stats.setMeasure(Measure.NORMALIZED_PETRI_NET_EDGE_SIMPLICITY, npnes );
	}

	public static double normalizedEntitySimplicity(int edgeCount, int eventCount) {
		double npnes = 1.0 - 
						Math.min( 
									((double) edgeCount / (double)eventCount) ,
									1.0d) ;
		return npnes;
	}

}
