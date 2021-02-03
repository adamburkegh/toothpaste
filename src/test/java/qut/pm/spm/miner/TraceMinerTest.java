package qut.pm.spm.miner;

import static org.junit.Assert.assertEquals;

import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.deckfour.xes.classification.XEventNameClassifier;
import org.deckfour.xes.model.XLog;
import org.junit.Test;
import org.processmining.framework.plugin.PluginContext;
import org.processmining.models.graphbased.directed.petrinet.StochasticNet;
import org.processmining.models.graphbased.directed.petrinet.elements.Place;
import org.processmining.models.graphbased.directed.petrinet.elements.Transition;
import org.processmining.models.graphbased.directed.petrinet.impl.StochasticNetImpl;


import qut.pm.prom.helpers.ConsoleUIPluginContext;
import qut.pm.prom.helpers.HeadlessDefinitelyNotUIPluginContext;
import qut.pm.prom.helpers.StochasticTestUtils;
import qut.pm.spm.AcceptingStochasticNet;
import qut.pm.xes.helpers.DelimitedTraceToXESConverter;

public class TraceMinerTest {

	private static Logger LOGGER = LogManager.getLogger();
	
	static {
		StochasticTestUtils.initializeLogging();
	}

	public static StochasticNet mineFromTraces(String ... traces) 
	{
		DelimitedTraceToXESConverter converter = new DelimitedTraceToXESConverter();
		XLog log = converter.convertTextArgs(traces);
		TraceMiner miner = new TraceMiner();
		PluginContext context = 
				new HeadlessDefinitelyNotUIPluginContext(new ConsoleUIPluginContext(), "test");
		AcceptingStochasticNet result = miner.runMiner(context, log, 
												new XEventNameClassifier());
		return result.getNet();		
	}
	
	@Test
	public void oneDupe() {
		StochasticNet net = new StochasticNetImpl("test");
		// parser doesn't support dupe transitions so have to do it the old fashioned way
		Place start = net.addPlace("source");
		Place end = net.addPlace("sink");
		Transition ta = net.addImmediateTransition("a");
		net.addArc(start, ta);
		Place p1 = net.addPlace("p1");
		net.addArc(ta,p1);
		Transition tb = net.addImmediateTransition("b");
		net.addArc(p1, tb);
		net.addArc(tb,end);
		//
		Transition ta2 = net.addImmediateTransition("a");
		net.addArc(start, ta2);
		Place p2 = net.addPlace("p2");
		net.addArc(ta2,p2);
		Transition tb2 = net.addImmediateTransition("b");
		net.addArc(p2, tb2);
		net.addArc(tb2,end);
		//
		Transition tb3 = net.addImmediateTransition("b");
		net.addArc(start, tb3);
		Place p3 = net.addPlace("p3");
		net.addArc(tb3,p3);
		Transition tc = net.addImmediateTransition("c");
		net.addArc(p3, tc);
		net.addArc(tc,end);
		//
		StochasticNet result = mineFromTraces("a b","a b","b c");
		// Can't even use StochasticTestUtils.checkEqual() due to dupes
		assertEquals(net.getTransitions().size(),  result.getTransitions().size() );
		assertEquals(net.getPlaces().size(),  result.getPlaces().size() );
		assertEquals(net.getEdges().size(),  result.getEdges().size() );
	}

	@Test
	public void findRepeatPattern() {
		assertEquals(0, findRepeated("a").size());
		assertEquals(0, findRepeated("ab").size());
		assertEquals(1, findRepeated("aa").size());
		assertEquals(1, findRepeated("aaa").size());
		assertEquals(1, findRepeated("abab").size());
		assertEquals(2, findRepeated("ababab").size());
		assertEquals(0, findRepeated("abac").size());
		assertEquals(0, findRepeated("abcd").size());
		// no tolerance for repeated characters in longer strings
		assertEquals(0, findRepeated("abacabac").size()); 
	}
	
	private static class RepeatPattern{
		private StringBuffer pattern ;
		private boolean building = true;
		private boolean finished = false;
		private int repeats = 0;
		private int ctIndex = 0;
		
		public RepeatPattern(char initial) {
			pattern = new StringBuffer(String.valueOf(initial));
		}
		
		public String getPattern() {
			return pattern.toString();
		}
		
		public boolean isBuilding() {
			return building;
		}
		
		public void stopBuilding() {
			building = false;
			repeats = 1;
			ctIndex = 0;
		}
		
		public void nextChar(char next) {
			if (building) {
				pattern.append(next);
				return;
			}
			if (!matchesNextChar(next)) {
				finished = true;
				return;
			}
			if (ctIndex < pattern.length()-1) {
				ctIndex++;
			}else {
				ctIndex = 0;
				repeats++;
			}
		}

		public boolean matchesNextChar(char next) {
			char cmpChar = pattern.charAt(ctIndex);
			return cmpChar == next;
		}
		
		public boolean isFinished() {
			return finished;
		}
				
		public boolean didRepeat() {
			return repeats > 1;
		}
		
		public String toString() {
			return "Pattern: " + pattern.toString() + " (x" + repeats + ")";
		}
		
		
	}
	
	/**
	 * Doesn't support extended codepoints.
	 * @param in
	 */
	private List<RepeatPattern> findRepeated(String in) {
		LOGGER.debug("findRepeated({})",in);
		if (in.isEmpty())
			return new LinkedList<>();
		List<RepeatPattern> ctPatterns = new LinkedList<>();
		List<RepeatPattern> confirmedPatterns = new LinkedList<>();
		for (int i=0; i<in.length(); i++) {
			char next = in.charAt(i);
			RepeatPattern rp = new RepeatPattern(next);
			Iterator<RepeatPattern> iter = ctPatterns.iterator();
			while (iter.hasNext()) {
				RepeatPattern ctp = iter.next();
				if (ctp.matchesNextChar(next)) {
					if (ctp.isBuilding()) {
						ctp.stopBuilding();						
					}
				}
				ctp.nextChar(next);
				if (ctp.isFinished()) {
					iter.remove();
					if (ctp.didRepeat()) {
						confirmedPatterns.add(ctp);
					}
				}
			}
			ctPatterns.add(rp);
			LOGGER.debug("Current patterns - {}", ctPatterns);
			LOGGER.debug("Confirmed patterns - {}", confirmedPatterns);
		}
		Set<String> existing = new HashSet<>();
		for (RepeatPattern rp: ctPatterns) {
			if (rp.didRepeat()) {
				if (!existing.contains(rp.getPattern())) {
					confirmedPatterns.add(rp);
					existing.add(rp.getPattern());
				}
			}
		}
		return confirmedPatterns;
	}
	
}
