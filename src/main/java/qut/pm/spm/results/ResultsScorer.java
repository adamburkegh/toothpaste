package qut.pm.spm.results;

import java.io.File;
import java.util.Date;

import org.simpleframework.xml.Serializer;
import org.simpleframework.xml.core.Persister;

import qut.pm.spm.RunStats;
import qut.pm.spm.TaskStats;

/**
 * Utility to make marking errors in the results file easier, eg if there is an OutOfMemoryError, 
 * or otherwise uncaught problem that requires cleaning up.
 * 
 * @author burkeat
 *
 */
public class ResultsScorer {

	public static void main(String[] args) throws Exception{
		if (args.length < 3 || args.length > 4) {
			System.out.println("Usage: ResultsScorer resultFile stat reason [resultDir]");
		}
		String resultFile = args[0];
		String stat = args[1];
		String reason = args[2];
		String reportingDir = "var";
		if (args.length == 4) {
			reportingDir = args[3];
		}
		Serializer serializer = new Persister();
		String pathname = reportingDir + File.separator + resultFile;
		File runFile = new File(pathname);
		System.out.println("Marking error for " + stat + " in " + runFile.getAbsolutePath());
		RunStats runStats = serializer.read(RunStats.class, runFile );
		TaskStats errorTask = new TaskStats(stat);
		errorTask.markFailed(reason + ". Manual annotation at " + new Date() );
		runStats.addTask(errorTask);
		serializer.write(runStats, runFile);
	}

}
