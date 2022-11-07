package qut.pm.spm.miner;

import java.io.File;
import java.io.IOException;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.deckfour.xes.classification.XEventClassifier;
import org.deckfour.xes.model.XLog;
import org.processmining.framework.plugin.PluginContext;
import org.processmining.models.graphbased.directed.petrinet.StochasticNet;
import org.processmining.models.semantics.petrinet.Marking;
import org.processmining.plugins.pnml.importing.StochasticNetDeserializer;
import org.processmining.plugins.pnml.simple.PNMLRoot;
import org.simpleframework.xml.Serializer;
import org.simpleframework.xml.core.Persister;

import qut.pm.prom.helpers.ConsoleUIPluginContext;
import qut.pm.prom.helpers.HeadlessDefinitelyNotUIPluginContext;
import qut.pm.prom.helpers.StochasticPetriNetUtils;
import qut.pm.spm.AcceptingStochasticNet;
import qut.pm.spm.AcceptingStochasticNetImpl;
import qut.pm.spm.StochasticNetLogMiner;

public class ToothpasteMinerHP implements StochasticNetLogMiner{

	private static final Logger LOGGER = LogManager.getLogger();
	
	private AcceptingStochasticNet netDescriptor;
	private String logFile;
	
	@Override
	public String getShortID() {
		return "tmh";
	}
	
	@Override
	public String getReadableID() {
		return "ToothpasteMinerHP";
	}
	
	@Override
	public AcceptingStochasticNet getStochasticNetDescriptor() {
		return netDescriptor;
	}

	public void run(PluginContext uipc, XLog log, XEventClassifier classifier, File outputModelFile) 
			throws Exception
	{
		File logFileObj = new File(logFile);
		LOGGER.info("Invoking Toothpaste miner prototype out of process on {}", logFile);
		callToothpasteMinerHP( logFileObj, outputModelFile );
		Serializer serializer = new Persister();
		PNMLRoot pnml = serializer.read(PNMLRoot.class, outputModelFile);
		StochasticNetDeserializer converter = new StochasticNetDeserializer();
		PluginContext pc = 
				new HeadlessDefinitelyNotUIPluginContext(new ConsoleUIPluginContext(), 
						"tpminer_loadnet");
		Object[] cResult = converter.convertToNet(pc, pnml, outputModelFile.getName(), true);
		StochasticNet net = (StochasticNet)cResult[0];
		Marking initialMarking = (Marking)cResult[1];
		Marking sInitMarking = 
				StochasticPetriNetUtils.findEquivalentInitialMarking(initialMarking, net);
		netDescriptor = new AcceptingStochasticNetImpl(net.getLabel(), net, sInitMarking );

	}
	
	/**
	 * Calling externally from Java never looks pretty
	 * 
	 * @param logFile
	 * @param modelFile
	 * @throws InterruptedException
	 * @throws IOException
	 */
	public static void callToothpasteMinerHP(File logFile, File modelFile) 
			throws InterruptedException, IOException 
	{
		ProcessBuilder pb = new ProcessBuilder("tpminer.bat", logFile.getAbsolutePath(), 
															  modelFile.getAbsolutePath());
		String workingDir = modelFile.getAbsoluteFile().getParent() ;
		LOGGER.info("Toothpaste miner working directory {} ", workingDir);
		pb.directory( new File(workingDir) );
		pb.inheritIO().start().waitFor();
	}
	
	@Override
	public boolean isStochasticNetProducer() {
		return true;
	}

	@Override
	public boolean isFilesytemLogReader() {
		return true;
	}

	public boolean isDirectDiscovery() {
		return true;
	}
	
	public void setLogFile(String logFile) {
		this.logFile = logFile;
	}

	
}
