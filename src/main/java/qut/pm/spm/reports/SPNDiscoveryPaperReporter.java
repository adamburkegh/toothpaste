package qut.pm.spm.reports;

import java.text.DecimalFormat;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import qut.pm.spm.Measure;
import qut.pm.spm.RunState;
import qut.pm.spm.RunStats;
import qut.pm.spm.StochasticNetLogMiner;

public class SPNDiscoveryPaperReporter extends SPNDiscoverReporter{

	private static final Set<Measure> INCLUDED_MEASURES;
	
	private static Map<String,String> mapMinerName = new HashMap<>();
	
	static {
		INCLUDED_MEASURES = new TreeSet<>();
		registerMeasures();
		registerMinerNames();
	}

	private static void registerMeasures() {
		INCLUDED_MEASURES.add(Measure.MODEL_ENTITY_COUNT);
		INCLUDED_MEASURES.add(Measure.EARTH_MOVERS_LIGHT_COVERAGE);
	}
	
	private static void registerMinerNames() {
		mapMinerName.put("align-inductive","walign-inductive");
		mapMinerName.put("align-split","walign-split");
		mapMinerName.put("fe-inductive","wfreq-inductive");
		mapMinerName.put("fe-split","wfreq-split");
		mapMinerName.put("msaprh-inductive","wpairscale-inductive");
		mapMinerName.put("msaprh-split","wpairscale-split");
		mapMinerName.put("rssmt","GDT_SPN discovery");
		mapMinerName.put("tmh","Toothpaste");
		mapMinerName.put("trace","Trace");
	}

	public SPNDiscoveryPaperReporter(String reportDir) {
		super(reportDir, new LatexTableFormatter(), System.out );
	}

	protected void outputReports() {
		reportFormatter.startReport();
		final int extraCols = 3;
		String[] headers = new String[INCLUDED_MEASURES.size() + extraCols];
		headers[0] = "Miner"; 
		headers[1] = "Log"; 
		headers[2] = "Duration (ms)"; 		
		int i=extraCols;
		for (Measure measure: INCLUDED_MEASURES) {
			headers[i] = measure.getText();
			i++;
		}
		reportFormatter.reportHeader("l l r r r", headers);
		Set<ReportStats> groupedStats = groupStats(statsDb);
		for ( ReportStats reportStats: groupedStats ) {
			String minerAlgoStr = "";
			if (reportStats.minerAlgo.equals(reportStats.artifactCreatorId) ) {					
				StochasticNetLogMiner minerAlgo = MINER_MAP.get( reportStats.minerAlgo );
				if (minerAlgo != null && !minerAlgo.isStochasticNetProducer()) {
					continue;
				}
			} 
			reportFormatter.reportStartLine();
			reportFormatter.reportCells(
					rewriteArtifactCreator( reportStats.getArtifactCreator() + minerAlgoStr )
					);		
			reportFormatter.reportCells(rewriteLogNameUnderscores(reportStats.getInputLogFileName()) );
			reportFormatter.reportCells(
					reportStats.minerDuration +  
					reportStats.estimatorDuration);
			Set<Measure> runMeasures = reportStats.getAllMeasures().keySet();
			for (Measure measure: INCLUDED_MEASURES) {
				if (runMeasures.contains(measure)) {
					DecimalFormat dFormat = new DecimalFormat("#.####");
					Number value = reportStats.getAllMeasures().get(measure);
					if (value.equals(Double.NaN)){
						reportFormatter.reportCells("-");
					}else {
						reportFormatter.reportCells(dFormat.format(value));
					}
				}else {
					RunState state = reportStats.getState();
					if ( RunState.FAILED.equals( state ) || RunState.RUNNING.equals( state )) {
						reportFormatter.reportCells(state.toString());
					}else {
						reportFormatter.reportCells("    ");
					}
				}
			}
			reportFormatter.reportNewline();
		}
		reportFormatter.endReport();
		printStream.println(reportFormatter.format());
	}
	
	private String rewriteLogNameUnderscores(String inputLogFileName) {
		if ("sepsis".equals(inputLogFileName)) {
			return "Sepsis";
		}
		if ("teleclaims".equals(inputLogFileName)) {
			return "Teleclaims";
		}
		return inputLogFileName.replace("_"," ");
	}

	// This group by is very hacky but my latex formatting was stuck in Java
	private static class GroupStats{
		public String log;
		public String miner;
		public int nSamples = 0;
		public long totalDuration = 0;
		public int totalEntityCount = 0;
		public double totalEMSC = 0;
		public double nEMSC = 0;
	}

	private Set<ReportStats> groupStats(Map<ReportStats, ReportStats> statsDb) {
		final String keySep = "___";
		Set<ReportStats> result = new TreeSet<ReportStats>();
		Map<String, GroupStats> grouper = new HashMap<String,GroupStats>();
		for (ReportStats reportStats: statsDb.keySet()) {
			String logShortName = reportStats.getInputLogFileName().substring( 
					0, reportStats.getInputLogFileName().indexOf("_k") );
			String key = reportStats.artifactCreatorId + keySep + logShortName;
			GroupStats existing = grouper.get(key);
			if (existing == null) {
				existing = new GroupStats();
				String minerName = mapMinerName.get(reportStats.artifactCreatorId);
				if (minerName != null) {
					existing.miner = minerName;
				}else {
					existing.miner = reportStats.artifactCreatorId;
				}
				existing.log = logShortName;
			}
			Map<Measure, Number> allMeasures = reportStats.getAllMeasures();
			Number modelCount = allMeasures.get(Measure.MODEL_ENTITY_COUNT);
			if (modelCount == null)
				continue;
			existing.nSamples++;
			existing.totalDuration += (reportStats.minerDuration + reportStats.estimatorDuration);
			existing.totalEntityCount += modelCount.intValue();
			Number emsc = allMeasures.get(Measure.EARTH_MOVERS_LIGHT_COVERAGE);
			if (emsc != null) {
				existing.nEMSC ++;
				existing.totalEMSC += emsc.doubleValue();
			}
			grouper.put(key,existing);
		}
		for (String groupStatsKey: grouper.keySet()) {
			GroupStats groupStats = grouper.get(groupStatsKey);
			ReportStats groupedReport = 
					new ReportStats( new RunStats(groupStats.log,"dummy",groupStats.miner ), "",
							groupStats.miner,"");
			Map<Measure, Number> allMeasures = groupedReport.getAllMeasures();
			allMeasures.put(Measure.MODEL_ENTITY_COUNT, 
								groupStats.totalEntityCount / groupStats.nSamples);
			allMeasures.put(Measure.EARTH_MOVERS_LIGHT_COVERAGE, 
					groupStats.totalEMSC / groupStats.nEMSC);
			groupedReport.minerDuration = groupStats.totalDuration / groupStats.nSamples;
			result.add(groupedReport);
		}
		return result;
	}

	
}
