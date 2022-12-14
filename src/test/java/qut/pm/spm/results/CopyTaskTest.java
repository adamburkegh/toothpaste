package qut.pm.spm.results;

import static org.junit.Assert.*;

import java.io.File;
import java.util.List;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.simpleframework.xml.Serializer;
import org.simpleframework.xml.core.Persister;

import qut.pm.spm.RunStats;
import qut.pm.spm.TaskStats;

public class CopyTaskTest {

	private static final String CREATOR = CopyTaskTest.class.getName();
	private TaskRewrites copyTask = null;
	private File rs1f = null;
	private File rs2f = null;
	
	@Before
	public void setUp() throws Exception {
		copyTask = new TaskRewrites();
		rs1f = File.createTempFile("ctt", "xml");
		rs2f = File.createTempFile("ctt", "xml");
	}
	
	@After
	public void tearDown() throws Exception{
		rs1f.delete();
		rs2f.delete();
	}

	@Test
	public void testWorking() throws Exception {
		Serializer serializer = new Persister();
		RunStats rs1 = new RunStats("test.xes","test.pnml",CREATOR);
		TaskStats taskStats = new TaskStats("testActivity1");
		rs1.addTask(taskStats);
		taskStats = new TaskStats("testActivity2");
		rs1.addTask(taskStats);
		serializer.write(rs1,rs1f);
		RunStats rs2 = new RunStats("test.xes","test.pnml",CREATOR);
		serializer.write(rs2,rs2f);
		copyTask.copyTask(rs1f.getAbsolutePath(), "testActivity2", rs2f.getAbsolutePath());
		RunStats result = serializer.read(RunStats.class, rs2f.getAbsoluteFile() );
		List<TaskStats> taskStatList = result.getTaskRunStats();
		assertEquals(1,taskStatList.size());
	}

	@Test(expected = Exception.class)
	public void testNoTask() throws Exception{
		Serializer serializer = new Persister();
		RunStats rs1 = new RunStats("test.xes","test.pnml",CREATOR);
		TaskStats taskStats = new TaskStats("testActivityRight");
		rs1.addTask(taskStats);
		serializer.write(rs1,rs1f);
		RunStats rs2 = new RunStats("test.xes","test.pnml",CREATOR);
		serializer.write(rs2,rs2f);
		copyTask.copyTask(rs1f.getAbsolutePath(), "testActivityWrong", rs2f.getAbsolutePath());
	}

	
}
