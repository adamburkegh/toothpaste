package qut.pm.spm.miner.conformance;

import static org.junit.Assert.*;

import java.util.Arrays;

import org.junit.Before;
import org.junit.Test;

import qut.pm.spm.TraceFreq;
import qut.pm.spm.conformance.AlphaPrecisionCalculation;

public class AlphaPrecisionCalculationTest {

	private static final double EPSILON = 0.001;
	private AlphaPrecisionCalculation alphaCalc;
	
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


}
