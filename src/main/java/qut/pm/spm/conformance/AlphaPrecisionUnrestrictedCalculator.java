package qut.pm.spm.conformance;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.deckfour.xes.classification.XEventClasses;
import org.deckfour.xes.classification.XEventClassifier;
import org.deckfour.xes.info.XLogInfo;
import org.deckfour.xes.model.XLog;
import org.deckfour.xes.model.XTrace;
import org.processmining.framework.plugin.PluginContext;

import qut.pm.spm.AcceptingStochasticNet;
import qut.pm.spm.Measure;
import qut.pm.spm.SPNQualityCalculator;
import qut.pm.spm.TaskStats;
import qut.pm.spm.TraceFreq;
import qut.pm.spm.TraceFreqGenerator;
import qut.pm.spm.playout.PlayoutGenerator;
import qut.pm.spm.playout.StochasticPlayoutGenerator;

public class AlphaPrecisionUnrestrictedCalculator implements SPNQualityCalculator {
	
	
	private static final int PLAYOUT_LOG_SIZE = 1000;
	private static final int MAX_TRACE_LENGTH = 100;
	private static final double ALPHA_SIGNIFICANCE = 0.0;
	private static Logger LOGGER = LogManager.getLogger();
	
	private TraceFreqGenerator traceFreqGen = new TraceFreqGenerator();
	private PlayoutGenerator playout 
		= new StochasticPlayoutGenerator(MAX_TRACE_LENGTH,PLAYOUT_LOG_SIZE);
	private AlphaPrecisionCalculation alphaCalc = new AlphaPrecisionCalculation();

	@Override
	public String getReadableId() {
		return "Alpha Precision Unrestricted alpha-sig " + ALPHA_SIGNIFICANCE;
	}

	@Override
	public void calculate(PluginContext context, AcceptingStochasticNet net, XLog log, 
			XEventClassifier classifier, TaskStats stats) throws Exception 
	{
		LOGGER.info("Computing " + getReadableId() );
		LOGGER.debug("Initial marking {}",net.getInitialMarking());
		double result = calculateAlphaPrecision(net,log,ALPHA_SIGNIFICANCE,classifier); 
		stats.setMeasure(Measure.ALPHA_PRECISION_UNRESTRICTED, result); 
	}

	private double calculateAlphaPrecision(AcceptingStochasticNet net, XLog log, 
									       double alphaSig, XEventClassifier classifier) 
	{
		int size = calculateLogLanguageSize(log,classifier);
		int maxTraceLength = calculateMaxTraceLength(log);
		TraceFreq tfLog = traceFreqGen.calculateForLog(log,classifier);
		TraceFreq tfModel = playout.buildPlayoutTraceFreq(net);
		return alphaCalc.calculatePrecision(tfLog, tfModel, 
				log.getInfo(classifier).getNumberOfEvents(), alphaSig, size, maxTraceLength);
	}


	
	private int calculateMaxTraceLength(XLog log) {
		int max = 0;
		for( XTrace trace: log) {
			if (trace.size() > max)
				max = trace.size();
		}
		return max;
	}


	private int calculateLogLanguageSize(XLog log, XEventClassifier classifier) {
		XLogInfo logInfo = log.getInfo(classifier);
		XEventClasses ec = logInfo.getEventClasses();
		return ec.size();
	}


	
}
