package qut.pm.spm;

import org.processmining.models.graphbased.directed.petrinet.Petrinet;

public class PetrinetSource {

	private AcceptingStochasticNet net;
	private String sourceId; // eg a filename
	private int kIndex = 0;
	
	public PetrinetSource(AcceptingStochasticNet source, String sourceId) {
		super();
		this.net = source;
		this.sourceId = sourceId;
	}
	
	public PetrinetSource(AcceptingStochasticNet source, String sourceId, int kIndex) {
		this(source,sourceId);
		this.kIndex = kIndex;
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
	
	public int getKIndex() {
		return kIndex ;
	}
	
}
