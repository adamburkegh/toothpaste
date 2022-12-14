package qut.pm.spm.results;

import java.io.File;
import java.util.Date;
import java.util.List;

import org.simpleframework.xml.Serializer;
import org.simpleframework.xml.core.Persister;

import qut.pm.spm.RunStats;
import qut.pm.spm.TaskStats;

public class CopyTask {

	
	public void copyTask(String sourceFile, String stat, String targetFile) throws Exception{
		Serializer serializer = new Persister();
		File sourceRunFile = new File(sourceFile);
		File targetRunFile = new File(targetFile);
		RunStats sourceRunStats = serializer.read(RunStats.class, sourceRunFile );
		RunStats targetRunStats = serializer.read(RunStats.class, targetRunFile );
		TaskStats sourceTask = null; 
		List<TaskStats> taskRunStats = sourceRunStats.getTaskRunStats();
		for(TaskStats tr: taskRunStats) {
			if (stat.equals( tr.getTaskName() )) {
				sourceTask = tr;
				break;
			}
		}
		if (sourceTask == null) {
			throw new Exception("Task " + stat + " not found in " + sourceFile);
		}
		sourceTask.setNote("Copied from " + sourceFile + " on " + new Date());
		targetRunStats.addTask(sourceTask);
		serializer.write(targetRunStats, targetRunFile);
	}
	
	public static void main(String[] args) throws Exception{
		if (args.length < 3 || args.length > 3) {
			System.out.println("Usage: CopyTask sourceFile stat targetFile");
		}
		String sourceFile = args[0];
		String stat = args[1];
		String targetFile = args[2];
		new CopyTask().copyTask(sourceFile, stat, targetFile);
		System.out.println("Copied " + stat + " from " + sourceFile + " into " + targetFile);
	}
	
}
