package qut.pm.spm;

import java.util.Collection;

import org.junit.Test;
import org.processmining.models.graphbased.directed.petrinet.PetrinetEdge;
import org.processmining.models.graphbased.directed.petrinet.PetrinetNode;
import org.processmining.models.graphbased.directed.petrinet.elements.Transition;

import qut.pm.prom.helpers.PetrinetExportUtils;

public class PetriNetConsistencyTest {


	@Test
	public void test() throws Exception {
		AcceptingStochasticNet pnet = PetrinetExportUtils.readStochasticPNMLModel("var/BPIC2013_closed_k1.pnml");
		Collection<Transition> trans = pnet.getNet().getTransitions();
		for (Transition tran: trans) {
			Collection<PetrinetEdge<? extends PetrinetNode, ? extends PetrinetNode>> outEdges = tran.getGraph().getOutEdges(tran);
			if (outEdges.size() == 0) {
				System.out.println( "Zero outgoing arcs for transition " + tran.getLabel() + " id " + tran.getId() );
			}
		}
	}

}
