package qut.pm.spm.conformance;

import java.util.List;
import java.util.Map;

import qut.pm.spm.TraceFreq;

/**
* Implements a variation on the Alpha Precision measure, from
* Depaire et al (2022) - Alpha Precision: Estimating the Significant System 
* Behavior in a Model 
* 
* Current implementation is 
* P_na(M,S) = sum_{sigma in L} prob_M(sigma) I_alpha(sigma)
* 
* not 
* P_alpha(M,S) = sum_{sigma in M} prob_M(sigma) I_alpha(sigma)
* 
* as in the paper.
*/
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
