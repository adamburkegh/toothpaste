package qut.pm.spm;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Allows R+ valued frequencies. This makes it possible to model scaled play-out logs with non-natural
 * frequencies.  
 * 
 * @author burkeat
 *
 */
public class TraceFreq{
	private Map<List<String>,Double> traceFreq = new HashMap<>();
	private double traceTotal = 0;
	
	public Map<List<String>, Double> getTraceFreq() {
		return traceFreq;
	}

	public double getTraceTotal() {
		return traceTotal;
	}

	public double getFreq(List<String> trace) {
		Double result = traceFreq.get(trace);
		if (result == null) {
			return 0;
		}
		return result;
	}
	
	public void incTraceFreq(List<String> trace) {
		double freq = getFreq(trace);
		traceFreq.put(trace,freq+1);
		traceTotal += 1;
	}
	
	public void incTraceFreq(List<String> trace, double plusFreq) {
		double freq = getFreq(trace);
		traceFreq.put(trace,freq+plusFreq);
		traceTotal += plusFreq;
	}
	
	public void putFreq(List<String> trace, double freq) {
		double oldFreq = getFreq(trace);
		traceFreq.put(trace,freq);
		traceTotal += freq - oldFreq;
	}

	public void forceTraceTotal(double traceTotal) {
		this.traceTotal = traceTotal;
	}
	
	public double getInternalTraceTotal() {
		double sum = 0;
		for ( List<String> key : traceFreq.keySet() ) {
			sum += traceFreq.get(key);
		}
		return sum;
	}
	
	public void scaleBy(double factor) {
		traceTotal = traceTotal* factor;
		for ( List<String> key : traceFreq.keySet() ) {
			traceFreq.put( key, traceFreq.get(key)*factor );
		}
	}
	
	public Set<List<String>> keySet() {
		return traceFreq.keySet();
	}

	public String format() {
		return "TraceFreq [traceFreq=" + traceFreq + "]";
	}
	
	public String toString() {
		return format();
	}


	
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((traceFreq == null) ? 0 : traceFreq.hashCode());
		long temp;
		temp = Double.doubleToLongBits(traceTotal);
		result = prime * result + (int) (temp ^ (temp >>> 32));
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		TraceFreq other = (TraceFreq) obj;
		if (traceFreq == null) {
			if (other.traceFreq != null)
				return false;
		} else if (!traceFreq.equals(other.traceFreq))
			return false;
		if (Double.doubleToLongBits(traceTotal) != Double.doubleToLongBits(other.traceTotal))
			return false;
		return true;
	}
	
	
}