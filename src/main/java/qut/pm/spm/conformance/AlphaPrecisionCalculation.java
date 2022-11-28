package qut.pm.spm.conformance;

import java.util.List;
import java.util.Map;

import qut.pm.spm.TraceFreq;

public class AlphaPrecisionCalculation {

	public double calculatePrecision(TraceFreq tfLog, TraceFreq tfModel, int logTotalEvents, 
			double alphaSig,int numberActivities, int maxTraceLength) 
	{
		double supportSize = calculateSystemSupportSize(numberActivities,maxTraceLength );
		Map<List<String>, Double> tfl = tfLog.getTraceFreq();
		double sumFreq = 0.0;
		for (List<String> trace: tfl.keySet()) {
			double probEstimate = (1+tfl.get(trace))/(supportSize + logTotalEvents);
			if (probEstimate > alphaSig)
				sumFreq += tfModel.getFreq(trace);
		}
		return sumFreq/tfModel.getTraceTotal();
	}

	public double calculateSystemSupportSize(int numberActivities, int maxTraceLength) {
		double result = 0;
		for (int i=1; i<maxTraceLength; i++) {
			result += Math.pow(numberActivities,i);
		}
		return result;
	}

}
