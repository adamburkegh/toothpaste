package qut.pm.spm;

public class NumUtils {

	public static final double EPSILON = 0.001;

	public static boolean nearEquals(double d1, double d2, double epsilon) {
		return Math.abs(d1-d2) < epsilon;
	}
	
	public static boolean nearEquals(double d1, double d2) {
		return nearEquals(d1,d2,EPSILON);
	}
	
}
