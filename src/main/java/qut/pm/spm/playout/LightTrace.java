package qut.pm.spm.playout;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

public class LightTrace implements Comparable<LightTrace>, Iterable<String>{
	private List<String> events = new LinkedList<String>();
	private boolean forcedTruncate = false;
	
	public LightTrace(LightTrace parentTrace) {
		this.events.addAll(parentTrace.events);
	}

	public LightTrace() {
		super();
	}

	public void addEvent(String event) {
		events.add(event);
	}
	
	public void forceTruncate() {
		this.forcedTruncate = true;
	}
	
	public boolean isForcedTruncate() {
		return forcedTruncate;
	}
	
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((events == null) ? 0 : events.hashCode());
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
		LightTrace other = (LightTrace) obj;
		if (events == null) {
			if (other.events != null)
				return false;
		} else if (!events.equals(other.events))
			return false;
		return true;
	}

	@Override
	public int compareTo(LightTrace other) {
		Iterator<String> iter = events.iterator();
		Iterator<String> otherIter = other.events.iterator();
		while (iter.hasNext()) {
			if (!otherIter.hasNext())
				return 1;
			String elem = iter.next();
			String otherElem = otherIter.next();
			int elemCompare = elem.compareTo(otherElem);
			if (elemCompare != 0)
				return elemCompare;
		}
		if (otherIter.hasNext())
			return -1;
		return 0; 
	}

	public int size() {
		return events.size();
	}

	@Override
	public Iterator<String> iterator() {
		return events.iterator();
	}
	
	public String toString() {
		return events.toString() + (isForcedTruncate()?" (truncated)":"");
	}
	
	public List<String> asList(){
		return events; 
	}
	
}