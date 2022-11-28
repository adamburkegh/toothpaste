package qut.pm.spm.playout;

import org.deckfour.xes.model.XLog;

import qut.pm.spm.AcceptingStochasticNet;
import qut.pm.spm.TraceFreq;

public interface PlayoutGenerator {

	/** 
	 * GSPNs only (even though the type interface takes GDT_SPNs).
	 * 
	 * @param net
	 * @param targetSize
	 * @param ctMarking
	 * @return
	 */
	XLog buildPlayoutLog(AcceptingStochasticNet net);
	XLog buildPlayoutLog(AcceptingStochasticNet net, long targetSize);

	/** 
	 * GSPNs only (even though the type interface takes GDT_SPNs).
	 * 
	 * @param net
	 * @param targetSize
	 * @param ctMarking
	 * @return
	 */
	TraceFreq buildPlayoutTraceFreq(AcceptingStochasticNet net);
	TraceFreq buildPlayoutTraceFreq(AcceptingStochasticNet net, long targetSize);

	public long getTargetSize();
	
	public TraceFreq scaleTo(TraceFreq tf, int size);
	
}