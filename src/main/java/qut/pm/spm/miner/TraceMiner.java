package qut.pm.spm.miner;

import java.io.File;
import java.util.Iterator;

import org.deckfour.xes.classification.XEventClassifier;
import org.deckfour.xes.model.XEvent;
import org.deckfour.xes.model.XLog;
import org.deckfour.xes.model.XTrace;
import org.processmining.framework.plugin.PluginContext;
import org.processmining.models.graphbased.directed.petrinet.StochasticNet;
import org.processmining.models.graphbased.directed.petrinet.elements.Place;
import org.processmining.models.graphbased.directed.petrinet.elements.TimedTransition;
import org.processmining.models.graphbased.directed.petrinet.impl.StochasticNetImpl;
import org.processmining.models.semantics.petrinet.Marking;

import qut.pm.spm.AcceptingStochasticNet;
import qut.pm.spm.StochasticNetLogMiner;
import qut.pm.xes.helpers.XESLogUtils;

public class TraceMiner implements StochasticNetLogMiner {

	private AcceptingStochasticNet result = null;
	
	@Override 
	public String getShortID() {
		return "trace";
	}
	
	@Override
	public String getReadableID() {
		return "Trace Model With Unity Weights";
	}

	@Override
	public void run(PluginContext pc, XLog log, XEventClassifier classifier, File outputModelFile) 
			throws Exception 
	{
		result = runMiner(pc, log,classifier);
	}

	public AcceptingStochasticNet runMiner(PluginContext pc, XLog log, XEventClassifier classifier) {
		AcceptingStochasticNet result = minePetrinet(log,classifier);
		return result;
	}

	@Override
	public AcceptingStochasticNet getStochasticNetDescriptor() {
		return result;
	}

	public static AcceptingStochasticNet minePetrinet(XLog log, XEventClassifier classifier) {
		StochasticNet net = new StochasticNetImpl("trace_" + XESLogUtils.getLogName(log) );
		Place source = net.addPlace("source");
		Place sink = net.addPlace("sink");
		int placeCounter = 0;
		for( XTrace trace: log) {
			Place addedPlace = source;
			TimedTransition addedTransition = null;
			Iterator<XEvent> eventIter = trace.iterator();
			while (eventIter.hasNext()) {
				XEvent event = eventIter.next();
				String activity = classifier.getClassIdentity(event);
				addedTransition = net.addImmediateTransition(activity);
				addedTransition.setWeight(1.0);
				net.addArc(addedPlace, addedTransition);
				if (eventIter.hasNext()) {
					addedPlace = net.addPlace("p" + placeCounter); placeCounter++;
					net.addArc(addedTransition, addedPlace);
				}
			}
			net.addArc(addedTransition, sink);
		}
		Marking initialMarking = new Marking();
		initialMarking.add(source);
		AcceptingStochasticNet result = new AcceptingStochasticNet(net.getLabel(), net, initialMarking);
		return result;
	}

	@Override
	public boolean isStochasticNetProducer() {
		return true;
	}


}
