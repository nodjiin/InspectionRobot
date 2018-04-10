/* Generated by AN DISI Unibo */ 
package it.unibo.qarobotexecutor;
import alice.tuprolog.Struct;
import alice.tuprolog.Term;
import it.unibo.qactors.ActorContext;
import it.unibo.is.interfaces.IOutputEnvView;
import it.unibo.qactors.planned.QActorPlanned;
import it.unibo.qactors.action.ActionDummy;
import it.unibo.qactors.action.AsynchActionResult;
import it.unibo.qactors.action.IActorAction;
import it.unibo.qactors.action.IActorAction.ActionExecMode;
import it.unibo.iot.configurator.Configurator;
import it.unibo.iot.executors.baseRobot.IBaseRobot; 
import it.unibo.iot.models.sensorData.distance.IDistanceSensorData;
import it.unibo.iot.models.sensorData.line.ILineSensorData;
import it.unibo.iot.models.sensorData.magnetometer.IMagnetometerSensorData;
import it.unibo.iot.sensors.ISensor; 
import it.unibo.iot.sensors.ISensorObserver;
import it.unibo.iot.sensors.distanceSensor.DistanceSensor;
import it.unibo.iot.sensors.lineSensor.LineSensor;
import it.unibo.iot.sensors.magnetometerSensor.MagnetometerSensor;

public class AbstractQarobotexecutor extends it.unibo.qactor.robot.RobotActor { 
protected AsynchActionResult aar = null;
protected boolean actionResult = true;
protected alice.tuprolog.SolveInfo sol;

		protected static IOutputEnvView setTheEnv(IOutputEnvView outEnvView ){
			return outEnvView;
		}


	public AbstractQarobotexecutor(String actorId, ActorContext myCtx, IOutputEnvView outEnvView ,it.unibo.iot.executors.baseRobot.IBaseRobot baserobot)  throws Exception{
		super(actorId, myCtx, "./srcMore/it/unibo/qarobotexecutor/plans.txt", 
		"./srcMore/it/unibo/qarobotexecutor/WorldTheory.pl",
		setTheEnv( outEnvView ) ,baserobot , "init");		
 	}
	@Override
	protected void doJob() throws Exception {
 		initSensorSystem();
		boolean res = init();
		//println(getName() + " doJob " + res );
	} 
	/* 
	* ------------------------------------------------------------
	* PLANS
	* ------------------------------------------------------------
	*/
    public boolean init() throws Exception{	//public to allow reflection
    try{
    	curPlanInExec =  "init";
    	boolean returnValue = suspendWork;
    while(true){
    nPlanIter++;
    		temporaryStr = "qarobotexecutor(start)";
    		println( temporaryStr );  
    		{ String parg = "consult( \"./robotTalkTheory.pl\" )";
    		  aar = solveGoal( parg , 0, "","" , "" );
    		//println(getName() + " plan " + curPlanInExec  +  " interrupted=" + aar.getInterrupted() + " action goon="+aar.getGoon());
    		if( aar.getInterrupted() ){
    			curPlanInExec   = "init";
    			if( ! aar.getGoon() ) break;
    		} 			
    		if( aar.getResult().equals("failure")){
    		if( ! switchToPlan("prologFailure").getGoon() ) break;
    		}else if( ! aar.getGoon() ) break;
    		}
    		temporaryStr = " \"ROBOT: WAITS FOR A COMMAND\" ";
    		println( temporaryStr );  
    		if( ! switchToPlan("executeCommands").getGoon() ) break;
    break;
    }//while
    return returnValue;
    }catch(Exception e){
    println( getName() + " ERROR " + e.getMessage() );
    throw e;
    }
    }
    public boolean executeCommands() throws Exception{	//public to allow reflection
    try{
    	curPlanInExec =  "executeCommands";
    	boolean returnValue = suspendWork;
    while(true){
    nPlanIter++;
    		//ReceiveMsg
    		 		 aar = receiveAMsg(2000000, "" , "" ); 	//could block
    				if( aar.getInterrupted() ){
    					curPlanInExec   = "playTheGame";
    					if( ! aar.getGoon() ) break;
    				} 			
    				if( ! aar.getGoon() ){
    					System.out.println("			WARNING: receiveMsg in " + getName() + " TOUT " + aar.getTimeRemained() + "/" +  2000000);
    					addRule("tout(receive,"+getName()+")");
    				} 		 
    				//println(getName() + " received " + aar.getResult() );
    		//onMsg
    		if( currentMessage.msgId().equals("robotcommand") ){
    			String parg = "";
    			parg = updateVars(null, Term.createTerm("robotcommand(X)"), Term.createTerm("robotcommand(h(medium))"), 
    				    		  					Term.createTerm(currentMessage.msgContent()), parg);
    				if( parg != null ){
    					 if( ! switchToPlan("stopCmd").getGoon() ) break; 
    				}//else println("guard  fails");  //parg is null when there is no guard (onEvent)
    		}//onMsg
    		if( currentMessage.msgId().equals("robotcommand") ){
    			String parg = "MOVE";
    			parg = updateVars(null, Term.createTerm("robotcommand(X)"), Term.createTerm("robotcommand(MOVE)"), 
    				    		  					Term.createTerm(currentMessage.msgContent()), parg);
    				if( parg != null ) println( parg );  
    		}//onMsg
    		if( currentMessage.msgId().equals("robotcommand") ){
    			String parg="MOVE";
    			parg = updateVars(null, Term.createTerm("robotcommand(X)"), Term.createTerm("robotcommand(MOVE)"), 
    				    		  					Term.createTerm(currentMessage.msgContent()), parg);
    				if( parg != null ) {
    					aar = solveGoal( parg , 0, "","" , "");
    					//println(getName() + " plan " + curPlanInExec  +  " interrupted=" + aar.getInterrupted() + " action goon="+aar.getGoon());
    					if( aar.getInterrupted() ){
    						curPlanInExec   = "executeCommands";
    						if( ! aar.getGoon() ) break;
    					} 			
    					if( aar.getResult().equals("failure")){
    						if( ! aar.getGoon() ) break;
    					}else if( ! aar.getGoon() ) break;
    				}
    		}//onMsg
    		if( currentMessage.msgId().equals("robotcommand") ){
    			String parg="picture";
    			parg = updateVars(null,Term.createTerm("robotcommand(X)"),  Term.createTerm("robotcommand(shootAPic)"), 
    				    		  					Term.createTerm(currentMessage.msgContent()), parg);
    			if( parg != null ) sendMsg("picture","qarobotcontroller", ActorContext.dispatch, parg ); 
    		}//onMsg
    		if( currentMessage.msgId().equals("robotcommandd") ){
    			String parg="robotdur(MOVE,DUR)";
    			parg = updateVars(null, Term.createTerm("robotcommandd(X,Y)"), Term.createTerm("robotcommandd(MOVE,DUR)"), 
    				    		  					Term.createTerm(currentMessage.msgContent()), parg);
    				if( parg != null ) {
    					aar = solveGoal( parg , 0, "","" , "");
    					//println(getName() + " plan " + curPlanInExec  +  " interrupted=" + aar.getInterrupted() + " action goon="+aar.getGoon());
    					if( aar.getInterrupted() ){
    						curPlanInExec   = "executeCommands";
    						if( ! aar.getGoon() ) break;
    					} 			
    					if( aar.getResult().equals("failure")){
    						if( ! aar.getGoon() ) break;
    					}else if( ! aar.getGoon() ) break;
    				}
    		}if( repeatPlan(0).getGoon() ) continue;
    break;
    }//while
    return returnValue;
    }catch(Exception e){
    println( getName() + " ERROR " + e.getMessage() );
    throw e;
    }
    }
    public boolean startLed() throws Exception{	//public to allow reflection
    try{
    	curPlanInExec =  "startLed";
    	boolean returnValue = suspendWork;
    while(true){
    nPlanIter++;
    		temporaryStr = " \"EXECUTOR: blinking the led\" ";
    		println( temporaryStr );  
    		{ String parg = "blinkLed";
    		  aar = solveGoal( parg , 0, "","" , "" );
    		//println(getName() + " plan " + curPlanInExec  +  " interrupted=" + aar.getInterrupted() + " action goon="+aar.getGoon());
    		if( aar.getInterrupted() ){
    			curPlanInExec   = "startLed";
    			if( ! aar.getGoon() ) break;
    		} 			
    		if( aar.getResult().equals("failure")){
    		if( ! aar.getGoon() ) break;
    		}else if( ! aar.getGoon() ) break;
    		}
    		returnValue = continueWork;  
    break;
    }//while
    return returnValue;
    }catch(Exception e){
    println( getName() + " ERROR " + e.getMessage() );
    throw e;
    }
    }
    public boolean stopCmd() throws Exception{	//public to allow reflection
    try{
    	curPlanInExec =  "stopCmd";
    	boolean returnValue = suspendWork;
    while(true){
    nPlanIter++;
    		//stop
    		if( ! execRobotMove("stopCmd","stop",40,0,10, "harmful" , "startLed") ) break;
    		returnValue = continueWork;  
    break;
    }//while
    return returnValue;
    }catch(Exception e){
    println( getName() + " ERROR " + e.getMessage() );
    throw e;
    }
    }
    public boolean prologFailure() throws Exception{	//public to allow reflection
    try{
    	curPlanInExec =  "prologFailure";
    	boolean returnValue = suspendWork;
    while(true){
    nPlanIter++;
    		temporaryStr = " \"Prolog goal FAILURE\" ";
    		println( temporaryStr );  
    		returnValue = continueWork;  
    break;
    }//while
    return returnValue;
    }catch(Exception e){
    println( getName() + " ERROR " + e.getMessage() );
    throw e;
    }
    }
    /* 
    * ------------------------------------------------------------
    * SENSORS
    * ------------------------------------------------------------
    */
    protected void initSensorSystem(){		
        	try {
        		String goal = "consult( \"./srcMore/it/unibo/qarobotexecutor/sensorTheory.pl\" )";
    //    		AsynchActionResult aar = 
        		solveGoal( goal , 0, "","" , "" );
    		addSensorObservers();
    	} catch (Exception e) {
     			e.printStackTrace();
    	}
    }
    /*
    //COMPONENTS
     RobotComponent motorleft 
     RobotComponent motorright 
    sensor distanceRadar  todo  
    Composed component motors
    */
    protected void addSensorObservers(){
    	for (ISensor<?> sensor : Configurator.getInstance().getSensors()) {
    		//println( "qarobotexecutor sensor= "  + sensor.getDefStringRep() );
    		//println( "qarobotexecutor sensor class= "  + sensor.getClass().getName() );
        	if( sensor instanceof DistanceSensor){
        		DistanceSensor sensorDistance  = (DistanceSensor) sensor;
        		ISensorObserver<IDistanceSensorData> obs = new SensorObserver<IDistanceSensorData>(this,outView);
        //		println( "avatar add observer to  "  + sensorDistance.getDefStringRep() );
        		sensorDistance.addObserver(  obs  ) ;
        	}
        	if( sensor instanceof LineSensor){
        		LineSensor sensorLine = (LineSensor) sensor;
         		ISensorObserver<ILineSensorData> obs = new SensorObserver<ILineSensorData>(this,outView);
        //		println( "avatar add observer to  "  + sensorLine.getDefStringRep() );
        		sensorLine.addObserver(  obs  ) ;
        	}
         	if( sensor instanceof MagnetometerSensor){
        		MagnetometerSensor sensorMagneto = (MagnetometerSensor) sensor;
         		ISensorObserver<IMagnetometerSensorData> obs = new SensorObserver<IMagnetometerSensorData>(this,outView);
        //		println( "avatar add observer to  "  + sensorMagneto.getDefStringRep() );
        		sensorMagneto.addObserver(  obs  ) ;
        	}
    	//OLD	
    	}		
    }	
    
 
	/* 
	* ------------------------------------------------------------
	* APPLICATION ACTIONS
	* ------------------------------------------------------------
	*/
	
  }
