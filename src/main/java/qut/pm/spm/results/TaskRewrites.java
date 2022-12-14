package qut.pm.spm.results;

import java.io.File;
import java.util.Date;
import java.util.List;

import org.simpleframework.xml.Serializer;
import org.simpleframework.xml.core.Persister;

import qut.pm.spm.RunStats;
import qut.pm.spm.TaskStats;

public class TaskRewrites {

	public void addNote(String sourceFile, String stat, String note) throws Exception{
		File sourceRunFile = new File(sourceFile);
		Serializer serializer = new Persister();
		RunStats sourceRunStats = serializer.read(RunStats.class, sourceRunFile );
		TaskStats sourceTask = findSourceTask(sourceFile, stat, sourceRunStats);
		sourceTask.setNote(note);
		sourceRunStats.addTask(sourceTask);
		serializer.write(sourceRunStats, sourceRunFile);		
	}
	
	public void copyTask(String sourceFile, String stat, String targetFile) throws Exception{
		Serializer serializer = new Persister();
		File sourceRunFile = new File(sourceFile);
		File targetRunFile = new File(targetFile);
		RunStats sourceRunStats = serializer.read(RunStats.class, sourceRunFile );
		RunStats targetRunStats = serializer.read(RunStats.class, targetRunFile );
		TaskStats sourceTask = findSourceTask(sourceFile, stat, sourceRunStats);
		sourceTask.setNote("Copied from " + sourceFile + " on " + new Date());
		targetRunStats.addTask(sourceTask);
		serializer.write(targetRunStats, targetRunFile);
	}

	private TaskStats findSourceTask(String sourceFile, String stat, RunStats sourceRunStats)
			throws Exception 
	{
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
		return sourceTask;
	}
	
	
}
