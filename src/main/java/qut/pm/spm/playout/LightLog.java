package qut.pm.spm.playout;

import java.util.Iterator;

import com.google.common.collect.Multiset;
import com.google.common.collect.TreeMultiset;

import qut.pm.spm.TraceFreq;

public class LightLog implements Iterable<LightTrace>{
	private Multiset<LightTrace> traces = TreeMultiset.create();
	
	public void addTrace(LightTrace trace) {
		traces.add(trace);
	}

	@Override
	public Iterator<LightTrace> iterator() {
		return traces.iterator();
	}
	
	public int size() {
		return traces.size();
	}
	
	public TraceFreq convertToTraceFreq() {
		TraceFreq result = new TraceFreq();
		traces.entrySet().forEach( traceEntry ->
				result.putFreq( traceEntry.getElement().asList(), traceEntry.getCount() ) );
		return result;
	}
	
}