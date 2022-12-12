package qut.pm.spm;

import java.io.File;
import java.util.Collections;

import org.processmining.framework.plugin.PluginContext;
import org.processmining.models.graphbased.directed.petrinet.StochasticNet;
import org.processmining.models.semantics.petrinet.Marking;
import org.processmining.plugins.pnml.importing.StochasticNetDeserializer;
import org.processmining.plugins.pnml.simple.PNMLRoot;
import org.simpleframework.xml.Serializer;
import org.simpleframework.xml.core.Persister;

import qut.pm.prom.helpers.ConsoleUIPluginContext;
import qut.pm.prom.helpers.HeadlessDefinitelyNotUIPluginContext;

public class TestUtil {

	private static final String PNML_SUFFIX = ".pnml";
	
	public static AcceptingStochasticNet loadNet(File file) throws Exception {
		PluginContext context = new HeadlessDefinitelyNotUIPluginContext(new ConsoleUIPluginContext(),
				"modelrunner_loadnet");
		String shortName = file.getName().replace("osmodel_", "");
		shortName = shortName.substring(0, shortName.indexOf(PNML_SUFFIX) );
		Serializer serializer = new Persister();
		PNMLRoot pnml = serializer.read(PNMLRoot.class, file);
		StochasticNetDeserializer converter = new StochasticNetDeserializer();
		Object[] objs = converter.convertToNet(context, pnml, file.getName(), true);
		AcceptingStochasticNet apn = new AcceptingStochasticNetImpl(shortName, (StochasticNet) objs[0],
				(Marking) objs[1], Collections.singleton((Marking) objs[2]));
		return apn;
	}
	
}
