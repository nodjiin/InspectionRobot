/* Generated by AN DISI Unibo */ 
package it.unibo.ctxRobot;
import it.unibo.qactors.ActorContext;
import java.io.InputStream;
import alice.tuprolog.SolveInfo;
import alice.tuprolog.Term;
import alice.tuprolog.Var;
import it.unibo.is.interfaces.IActivity;
import it.unibo.is.interfaces.IBasicEnvAwt;
import it.unibo.is.interfaces.IIntent;
import it.unibo.is.interfaces.IOutputEnvView;
import it.unibo.system.SituatedSysKb;

public class MainCtxRobot extends ActorContext implements IActivity{
private IBasicEnvAwt env; 
private it.unibo.qactor.robot.RobotActor robot; 
 
	public MainCtxRobot(String name, IOutputEnvView outEnvView,
			InputStream sysKbStream, InputStream sysRulesStream) throws Exception {
		super(name, outEnvView, sysKbStream, sysRulesStream);
		this.outEnvView = outEnvView;
		env = outEnvView.getEnv();
 	}
	@Override
	public void configure() {
		try {
		println("Starting the http server ... ");
		new  it.unibo.qactors.web.QActorHttpServer(outEnvView,"./srcMore/it/unibo/ctxRobot",8080).start();
				IBasicEnvAwt env = outEnvView.getEnv();
				if( env != null){
 					env.addInputPanel(60);
					env.addCmdPanel("input", new String[]{"INPUT"}, this);					
				}
		println("Starting the baseRobot (two robots cannot run in the same context).... ");
		it.unibo.iot.executors.baseRobot.IBaseRobot baseRobot = 
				it.unibo.qactor.robot.RobotSysKb.setRobotBase(this,  "enterprise" );
		Thread.sleep(300); //Give time to the baseRobot to start
		robot= new it.unibo.qarobotexecutor.Qarobotexecutor("qarobotexecutor", this, outEnvView, baseRobot);
//			new it.unibo.qarobotexecutor.Qarobotexecutor("qarobotexecutor", this, outEnvView, baseRobot);
		println("Starting the handlers .... ");
		println("Starting the actors .... ");
new it.unibo.qarobotcontroller.Qarobotcontroller("qarobotcontroller", this, outEnvView);
		
 		} catch (Exception e) {
 		  e.printStackTrace();
		} 		
	}
	@Override
	public void execAction(String cmd) {
		String input = env.readln();
		//println("input=" + input);
		try {
			Term.createTerm(input);
			String goal = "executeInput(" + input +").";
			SolveInfo sol = robot.getPrologEngine().solve( goal );
			println("> " + sol);
		} catch (Exception e) {
 			println("Input error " + e.getMessage());
		}
 	}
	@Override
	public void execAction() {}
	@Override
	public void execAction(IIntent input) {}
	@Override
	public String execActionWithAnswer(String cmd) {return null;}
 
	
/*
* ----------------------------------------------
* MAIN
* ----------------------------------------------
*/
	public static void main(String[] args) throws Exception{
		IOutputEnvView outEnvView = SituatedSysKb.standardOutEnvView;
 		InputStream sysKbStream    = //MainCtxRobot.class.getResourceAsStream("finalrobotproject.pl");
 			new java.io.FileInputStream("./srcMore/it/unibo/ctxRobot/finalrobotproject.pl");
		InputStream sysRulesStream = MainCtxRobot.class.getResourceAsStream("sysRules.pl");
		new MainCtxRobot("ctxRobot", outEnvView, sysKbStream, sysRulesStream ).configure();
 	}
}
