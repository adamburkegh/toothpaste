package qut.pm.spm.conformance;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.deckfour.xes.classification.XEventClassifier;
import org.deckfour.xes.model.XLog;
import org.processmining.earthmoversstochasticconformancechecking.parameters.EMSCParametersDefault;
import org.processmining.earthmoversstochasticconformancechecking.parameters.EMSCParametersLogModel;
import org.processmining.earthmoversstochasticconformancechecking.parameters.EMSCParametersLogModelAbstract;
import org.processmining.earthmoversstochasticconformancechecking.plugins.EarthMoversStochasticConformancePlugin;
import org.processmining.earthmoversstochasticconformancechecking.tracealignments.StochasticTraceAlignmentsLogModel;
import org.processmining.framework.plugin.PluginContext;
import org.processmining.framework.plugin.ProMCanceller;

import qut.pm.spm.AcceptingStochasticNet;
import qut.pm.spm.Measure;
import qut.pm.spm.SPNQualityCalculator;
import qut.pm.spm.TaskStats;

public class EarthMoversCalculator implements SPNQualityCalculator {

	private class EMSCParametersLogModelNoAlignments extends EMSCParametersLogModelAbstract {

		public EMSCParametersLogModelNoAlignments(XEventClassifier classifier) {
			super(EMSCParametersDefault.defaultDistanceMatrix, 
					classifier,
					EMSCParametersDefault.defaultTerminationStrategy, 
					EMSCParametersDefault.defaultDebug, 
					false);
		}

	}
	
	private static Logger LOGGER = LogManager.getLogger();

	@Override
	public String getReadableId() {
		return "Earth Movers Similarity";
	}

	@Override
	public void calculate(PluginContext context, AcceptingStochasticNet net, XLog log, 
			XEventClassifier classifier, TaskStats stats) throws Exception 
	{
		LOGGER.info("Computing earth-movers' distance (SL) ");
		EMSCParametersLogModel parameters = new EMSCParametersLogModelNoAlignments(classifier);
		
		LOGGER.debug("Initial marking {}",net.getInitialMarking());
		StochasticTraceAlignmentsLogModel stAlign = 
					EarthMoversStochasticConformancePlugin.measureLogModel(log, net.getNet(),
							net.getInitialMarking(), parameters, new ProMCanceller() {
								public boolean isCancelled() {
									return context.getProgress().isCancelled();
								}
							});
		stats.setMeasure(Measure.EARTH_MOVERS_SIMILARITY, stAlign.getSimilarity()); 
	}

}
