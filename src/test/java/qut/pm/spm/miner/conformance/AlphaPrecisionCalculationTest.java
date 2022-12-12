package qut.pm.spm.miner.conformance;

import static org.junit.Assert.*;

import java.io.File;
import java.util.Arrays;

import org.deckfour.xes.classification.XEventNameClassifier;
import org.deckfour.xes.info.XLogInfoFactory;
import org.deckfour.xes.model.XLog;
import org.junit.Before;
import org.junit.Test;
import org.processmining.stochasticawareconformancechecking.automata.StochasticDeterministicFiniteAutomatonMapped;
import org.processmining.stochasticawareconformancechecking.helperclasses.StochasticPetriNet2StochasticDeterministicFiniteAutomaton2;
import org.processmining.xeslite.plugin.OpenLogFileLiteImplPlugin;

import qut.pm.prom.helpers.ConsoleUIPluginContext;
import qut.pm.prom.helpers.HeadlessDefinitelyNotUIPluginContext;
import qut.pm.spm.AcceptingStochasticNet;
import qut.pm.spm.TaskStats;
import qut.pm.spm.TestUtil;
import qut.pm.spm.TraceFreq;
import qut.pm.spm.conformance.AlphaPrecisionCalculation;
import qut.pm.spm.conformance.AlphaPrecisionUnrestrictedCalculator;

public class AlphaPrecisionCalculationTest {

	private static final double EPSILON = 0.001;
	private AlphaPrecisionCalculation alphaCalc;
	private static XEventNameClassifier NAME;
	
	@Before
	public void setUp() throws Exception {
		alphaCalc = new AlphaPrecisionCalculation();
	}

	public void assertClose(double expected, double result) {
		assertEquals(expected, result, EPSILON);
	}
	
	@Test
	public void testPerfect() {
		TraceFreq tfLog = new TraceFreq();
		tfLog.putFreq( Arrays.asList(new String[]{"a","b","c"}) ,4);
		tfLog.putFreq( Arrays.asList(new String[]{"a","d","e"}) ,2);
		assertClose(1.0, alphaCalc.calculatePrecision(tfLog, tfLog, 18, 0.0, 5, 3));
	}

	@Test
	public void testZero() {
		TraceFreq tfLog = new TraceFreq();
		tfLog.putFreq( Arrays.asList(new String[]{"a","b","c"}) ,4);
		tfLog.putFreq( Arrays.asList(new String[]{"a","d","e"}) ,2);
		TraceFreq tfModel = new TraceFreq();
		tfModel.putFreq( Arrays.asList(new String[]{"b","c"}), 6);
		tfModel.putFreq( Arrays.asList(new String[]{"d","e"}), 9);
		assertClose(0.0, alphaCalc.calculatePrecision(tfLog, tfModel, 48, 0.0, 5, 3));
	}

	@Test
	public void testAnother() {
		TraceFreq tfLog = new TraceFreq();
		tfLog.putFreq( Arrays.asList(new String[]{"a","b","c"}) ,4);
		tfLog.putFreq( Arrays.asList(new String[]{"a","d","e"}) ,2);
		TraceFreq tfModel = new TraceFreq();
		tfModel.putFreq( Arrays.asList(new String[]{"a","b","c"}), 2);
		tfModel.putFreq( Arrays.asList(new String[]{"d","e"}), 3);
		assertClose(0.4, alphaCalc.calculatePrecision(tfLog, tfModel, 48, 0.0, 5, 3));
	}

	@Test
	public void testInvalidBPIC2018_control() throws Exception{
		// AcceptingStochasticNet net = TestUtil.loadNet(new File("results/2022_jn/osmodel_tmh_BPIC2018_control_k1.pnml"));
		AcceptingStochasticNet net = TestUtil.loadNet(new File("var/BPIC2018_control_k1.pnml"));
		HeadlessDefinitelyNotUIPluginContext uipc = new HeadlessDefinitelyNotUIPluginContext(new ConsoleUIPluginContext(),
				"test");
		NAME = new XEventNameClassifier();
		XLog log = (XLog) new OpenLogFileLiteImplPlugin().importFile(uipc, "var/BPIC2018_control_nk1.xes");
		XLogInfoFactory.createLogInfo(log, NAME);
		AlphaPrecisionUnrestrictedCalculator calculator = new AlphaPrecisionUnrestrictedCalculator();
		TaskStats stats = new TaskStats("test");
		calculator.calculate(uipc, net, log, NAME, stats);
	}
	
}
