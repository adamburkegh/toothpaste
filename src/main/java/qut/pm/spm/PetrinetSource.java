package qut.pm.spm;

import org.processmining.models.graphbased.directed.petrinet.Petrinet;

public class PetrinetSource {

	private AcceptingStochasticNet net;
	private String sourceId; // eg a filename
	
	public PetrinetSource(AcceptingStochasticNet source, String sourceId) {
		super();
		this.net = source;
		this.sourceId = sourceId;
	}
	
	public Petrinet getNet() {
		return net.getNet();
	}
	
	public String getSourceId() {
		return sourceId;
	}
	
	public AcceptingStochasticNet getAcceptingPetriNet() {
		return net;
	}
	
}
