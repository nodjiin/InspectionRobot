package it.unibo.qarobotcontroller;

public class Utils {

	public static void suspend(int ms){
		try {
			java.lang.Thread.sleep(ms);
		} catch (InterruptedException e) {
			// TODO Auto-generated catch block
			System.out.println("Suspend failed");
			e.printStackTrace();
		}
	}
	
}
