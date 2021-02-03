package qut.pm.toothpaste;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.deckfour.xes.classification.XEventClassifier;
import org.deckfour.xes.model.XEvent;
import org.deckfour.xes.model.XLog;
import org.deckfour.xes.model.XTrace;
import org.processmining.models.graphbased.directed.petrinet.PetrinetEdge;
import org.processmining.models.graphbased.directed.petrinet.PetrinetNode;
import org.processmining.models.graphbased.directed.petrinet.StochasticNet;
import org.processmining.models.graphbased.directed.petrinet.elements.Place;
import org.processmining.models.graphbased.directed.petrinet.elements.TimedTransition;
import org.processmining.models.graphbased.directed.petrinet.elements.Transition;
import org.processmining.models.graphbased.directed.petrinet.impl.StochasticNetImpl;
import org.processmining.models.semantics.petrinet.Marking;

import qut.pm.spm.AcceptingStochasticNet;
import qut.pm.xes.helpers.XESLogUtils;


/**
 * Lab tinkering with an algo that creates a frequency-annotated decision tree from log traces, 
 * by working from the head of the trace. A more sophisticated workup of this can be found in 
 * the Haskell prototype in src/main/haskell/Toothpaste.hs
 * 
 * It squeezes together the traces from the head end, like squeezing a toothpaste tube.
 * 
 * This early work is super-naive. No loops, blatantly exploits finite nature of log. Results 
 * in a monster Petri Net that can not even be visualized for many BPIC logs, as long traces, 
 * concurrency and loops get ignored. 
 * 
 * Uses a GSPN to represent the decision tree but ignores timing, just to see what happens
 * 
 * Not even any tests atm
 * 
 * @author burkeat
 *
 */
public class ToothpasteTreeMiner {

	private static final String FINAL = "FINAL";
	private static final String INITIAL = "INITIAL";
	private static Logger LOGGER = LogManager.getLogger();
	private AcceptingStochasticNet netDescriptor;
	private int placeCount;
	private XEventClassifier classifier;
	
	private static class DigraphNode{
		private String label;
		private int siblingRelativeEventCount = 0;
		private int childTotalEventCount = 0;
		private Map<String,DigraphNode> children = new HashMap<String,DigraphNode>();
		
		public DigraphNode(String label) {
			this.label = label;
		}

		public DigraphNode newObservedEvent(String eventLabel) {
			DigraphNode existing = children.get(eventLabel);
			if (existing == null) {
				DigraphNode newNode = new DigraphNode(eventLabel);
				children.put(eventLabel, newNode);
				existing = newNode;
			}
			existing.siblingRelativeEventCount += 1;
			childTotalEventCount += 1;
			return existing;
		}
		
		/**
		 * Note only considers label
		 */
		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + ((label == null) ? 0 : label.hashCode());
			return result;
		}

		/**
		 * Note only considers label
		 */
		@Override
		public boolean equals(Object obj) {
			if (this == obj)
				return true;
			if (obj == null)
				return false;
			if (getClass() != obj.getClass())
				return false;
			DigraphNode other = (DigraphNode) obj;
			if (label == null) {
				if (other.label != null)
					return false;
			} else if (!label.equals(other.label))
				return false;
			return true;
		}

		@Override
		public String toString() {
			return "Node [label=" + label + ", siblingRelativeEventCount=" + siblingRelativeEventCount
					+ ", childTotalEventCount=" + childTotalEventCount 
					+ ", #children=" + children.size() + "]";
		}

		public String formatAsTree() {
			return "\n" + formatAsTree(0);
		}
		
		private String formatAsTree(int offset) {
			StringBuffer result = new StringBuffer("");
			for (int i = 0; i < offset; i++) {
				result.append(" - ");
			}
			result.append( "Node [label = " + label + ", "
					+ "siblingRelativeEventCount=" + siblingRelativeEventCount + "]\n" );
			for (String childLabel: children.keySet()) {
				result.append( 
						children.get(childLabel).formatAsTree(offset+1) );
			}
			return result.toString();			
		}
		
	}
	
	private class ChainPair{
		public TimedTransition transition;
		public Place place;
		
		public ChainPair(TimedTransition transition, Place place) {
			this.transition = transition;
			this.place = place;
		}
	}

		
	public AcceptingStochasticNet mine(XLog log, XEventClassifier classifier) 
			throws Exception
	{
		reset(classifier);
		DigraphNode decisionTree = buildDecisionTree(log);
		LOGGER.info("Found decision tree: {}", decisionTree);
		netDescriptor = transformDecisonDigraphToStochasticNet(decisionTree, XESLogUtils.getLogName(log));
		LOGGER.info("Transformed to SPN: {}", netDescriptor.getNet());
		return netDescriptor;
	}
	
	private void squeezeFromFinal(StochasticNet net, Place finalPlace) {
		// All synonymous transitions coming into FINAL which have probability of 1.0
		// can be consolidated
		// Formal proof pending, but it totally works honest guv
		LOGGER.debug("squeezeFromFinal({})", finalPlace.getLabel());
		Map<String,ChainPair> incoming = new HashMap<String,ChainPair>();
		for (PetrinetEdge<? extends PetrinetNode, ? extends PetrinetNode> edge: net.getInEdges(finalPlace)){
			String incomingTransitionLabel = edge.getSource().getLabel();
			TimedTransition sourceTransition = (TimedTransition)edge.getSource();
			Collection<PetrinetEdge<? extends PetrinetNode, ? extends PetrinetNode>> outEdges = 
					net.getOutEdges(sourceTransition);
			if (outEdges.size() != 1) { 	// only if choiceless 
				continue;
			}
			LOGGER.debug("Found 1.0 chain {} :: {}",incomingTransitionLabel,sourceTransition.getId());
			Collection<PetrinetEdge<? extends PetrinetNode, ? extends PetrinetNode>> inEdges = 
					net.getInEdges(sourceTransition);
			for (PetrinetEdge<? extends PetrinetNode, ? extends PetrinetNode> placeEdge: inEdges) {
				// Downcast is bad, but don't see another way to do it
				Place sourcePlace = (Place) placeEdge.getSource();
				Collection<PetrinetEdge<? extends PetrinetNode, ? extends PetrinetNode>> placeOutgoing = 
						net.getOutEdges(sourcePlace);
				Collection<PetrinetEdge<? extends PetrinetNode, ? extends PetrinetNode>> placeIncoming = 
						net.getInEdges(sourcePlace);
				if (placeOutgoing.size() != 1 || placeIncoming.size() != 1) { // only if choiceless
					continue;
				}
				if (incoming.containsKey(incomingTransitionLabel )) {
					ChainPair existing = incoming.get(incomingTransitionLabel);
					LOGGER.debug("Replacing {} (from {}) with edge from {} to {} ({})", edge, 
							edge.getSource(), sourcePlace, existing.transition, 
							existing.transition.getId());
					net.removeEdge(edge);
					net.removeTransition(sourceTransition);
					net.removePlace(sourcePlace);
					Transition upstreamTransition = (Transition)placeIncoming.iterator().next().getSource();
					net.addArc(upstreamTransition, existing.place);
					sourcePlace = existing.place;
				}else {
					LOGGER.debug("Tracking new transition {} from {} ({})", incomingTransitionLabel, 
							sourceTransition, sourceTransition.getId());
					ChainPair chainPair = new ChainPair(sourceTransition,sourcePlace);
					incoming.put(incomingTransitionLabel, chainPair);
				}
				squeezeFromFinal(net,sourcePlace);
			}
		}
	}

	private void reset(XEventClassifier classifier) {
		placeCount = 0;
		this.classifier = classifier;		
	}

	private AcceptingStochasticNet transformDecisonDigraphToStochasticNet(DigraphNode decisionTree, String name) {
		StochasticNet net = new StochasticNetImpl(name);
		Place initialPlace 	= net.addPlace(INITIAL);
		Place finalPlace 	= net.addPlace(FINAL); 
		transformSubdigraph(decisionTree, net, initialPlace, finalPlace);
		squeezeFromFinal(net,finalPlace);
		Marking initialMarking = new Marking();
		initialMarking.add(initialPlace);
		Marking finalMarking = new Marking();
		finalMarking.add(finalPlace);
		Set<Marking> finalMarkings = new HashSet<>();
		finalMarkings.add(finalMarking);
		AcceptingStochasticNet result = new AcceptingStochasticNet(net.getLabel(), net, initialMarking);
		return result;
	}

	private void transformSubdigraph(DigraphNode decisionDigraph, StochasticNet resultNet, Place lastPlace, 
									Place finalPlace) 
	{
		for (String childLabel: decisionDigraph.children.keySet()) {
			DigraphNode child = decisionDigraph.children.get(childLabel);
			TimedTransition transition = resultNet.addImmediateTransition(childLabel);
			transition.setImmediate(true);
			transition.setWeight(child.siblingRelativeEventCount);
			resultNet.addArc(lastPlace, transition);
			if (child.children.size() > 0) {
				Place landingPlace = resultNet.addPlace(newPlaceLabel());
				resultNet.addArc(transition, landingPlace);
				transformSubdigraph(child,resultNet,landingPlace,finalPlace);
			}else {
				resultNet.addArc(transition, finalPlace);
			}
		}
	}

	private String newPlaceLabel() {
		String label = "p" + placeCount;
		placeCount++;
		return label;
	}

	private DigraphNode buildDecisionTree(XLog log) {
		// This wants to be recursive, but Java 8 and XES make it a little awkward
		DigraphNode root = new DigraphNode(INITIAL);
		for (XTrace trace: log) {
			LOGGER.debug(root);
			addTraceToTree(root, trace);
		}
		LOGGER.debug(root.formatAsTree());
		return root;
	}

	private void addTraceToTree(DigraphNode parent, XTrace trace) {
		LOGGER.debug("addTraceToTree {} {}",parent, parent.children);
		DigraphNode current = parent;
		DigraphNode nextChild = null;
		for (XEvent event: trace) {
			String label = classifier.getClassIdentity(event);
			nextChild = current.newObservedEvent(label);
			current = nextChild;
		}
	}

	
}
