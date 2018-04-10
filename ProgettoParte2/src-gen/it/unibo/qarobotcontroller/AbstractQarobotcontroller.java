/* Generated by AN DISI Unibo */ 
package it.unibo.qarobotcontroller;
import alice.tuprolog.Term;
import alice.tuprolog.Struct;
import it.unibo.qactors.ActorContext;
import it.unibo.is.interfaces.IOutputEnvView;
import it.unibo.qactors.planned.QActorPlanned;
import it.unibo.qactors.action.ActionDummy;
import it.unibo.qactors.action.AsynchActionResult;
import it.unibo.qactors.action.IActorAction;
import it.unibo.qactors.action.IActorAction.ActionExecMode;

public abstract class AbstractQarobotcontroller extends QActorPlanned { 
	protected AsynchActionResult aar = null;
	protected boolean actionResult = true;
	protected alice.tuprolog.SolveInfo sol;
	
			protected static IOutputEnvView setTheEnv(IOutputEnvView outEnvView ){
				return outEnvView;
			}
	
	
		public AbstractQarobotcontroller(String actorId, ActorContext myCtx, IOutputEnvView outEnvView )  throws Exception{
			super(actorId, myCtx, "./srcMore/it/unibo/qarobotcontroller/plans.txt", 
			"./srcMore/it/unibo/qarobotcontroller/WorldTheory.pl",
			setTheEnv( outEnvView )  , "init");		
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
	    		temporaryStr = "qacontroller(start)";
	    		println( temporaryStr );  
	    		{ String parg = "retractall(sensor(_))";
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
	    		{ String parg = "consult( \"./utils.pl\" )";
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
	    		if( ! switchToPlan("sendUserMoveCommands").getGoon() ) break;
	    break;
	    }//while
	    return returnValue;
	    }catch(Exception e){
	    println( getName() + " ERROR " + e.getMessage() );
	    throw e;
	    }
	    }
	    public boolean sendUserMoveCommands() throws Exception{	//public to allow reflection
	    try{
	    	curPlanInExec =  "sendUserMoveCommands";
	    	boolean returnValue = suspendWork;
	    while(true){
	    nPlanIter++;
	    		if( (guardVars = evalTheGuard( " ??harmful" )) != null ){
	    		temporaryStr = " \"rimosso harmful\" ";
	    		temporaryStr = substituteVars(guardVars,temporaryStr);
	    		println( temporaryStr );  
	    		}
	    		//senseEvent
	    		timeoutval = 200000;
	    		aar = senseEvents( timeoutval,"harmful,inputcmd,usercmd,sensordata","continue,continue,continue,continue",
	    		"" , "",ActionExecMode.synch );
	    		if( ! aar.getGoon() || aar.getTimeRemained() <= 0 ){
	    			println("			WARNING: sense timeout");
	    			addRule("tout(senseevent,"+getName()+")");
	    			//break;
	    		}
	    		if( (guardVars = evalTheGuard( " ??tout(X,Y)" )) != null ){
	    		if( ! switchToPlan("handleTimeOut").getGoon() ) break;
	    		}
	    		printCurrentEvent(false);
	    		//onEvent
	    		if( currentEvent.getEventId().equals("sensordata") ){
	    		 		String parg="lastMove";
	    		 		parg = updateVars(null, Term.createTerm("sensordata(X)"), Term.createTerm("sensordata(X)"), 
	    		 			    		  					Term.createTerm(currentEvent.getMsg()), parg);
	    		 			if( parg != null ) {
	    		 				aar = solveGoal( parg , 0, "","" , "");
	    		 				//println(getName() + " plan " + curPlanInExec  +  " interrupted=" + aar.getInterrupted() + " action goon="+aar.getGoon());
	    		 				if( aar.getInterrupted() ){
	    		 					curPlanInExec   = "sendUserMoveCommands";
	    		 					if( ! aar.getGoon() ) break;
	    		 				} 			
	    		 				if( aar.getResult().equals("failure")){
	    		 					if( ! switchToPlan("prologFailure").getGoon() ) break;
	    		 				}else if( ! aar.getGoon() ) break;
	    		 			}
	    		 }
	    		//onEvent
	    		if( currentEvent.getEventId().equals("sensordata") ){
	    		 		String parg="duplicateMoves";
	    		 		parg = updateVars(null, Term.createTerm("sensordata(X)"), Term.createTerm("sensordata(X)"), 
	    		 			    		  					Term.createTerm(currentEvent.getMsg()), parg);
	    		 			if( parg != null ) {
	    		 				aar = solveGoal( parg , 0, "","" , "");
	    		 				//println(getName() + " plan " + curPlanInExec  +  " interrupted=" + aar.getInterrupted() + " action goon="+aar.getGoon());
	    		 				if( aar.getInterrupted() ){
	    		 					curPlanInExec   = "sendUserMoveCommands";
	    		 					if( ! aar.getGoon() ) break;
	    		 				} 			
	    		 				if( aar.getResult().equals("failure")){
	    		 					if( ! switchToPlan("prologFailure").getGoon() ) break;
	    		 				}else if( ! aar.getGoon() ) break;
	    		 			}
	    		 }
	    		//onEvent
	    		if( currentEvent.getEventId().equals("sensordata") ){
	    		 		String parg = "";
	    		 		parg = updateVars(null, Term.createTerm("sensordata(X)"), Term.createTerm("sensordata(X)"), 
	    		 			    		  					Term.createTerm(currentEvent.getMsg()), parg);
	    		 			if( parg != null ){
	    		 				 if( ! switchToPlan("inspection").getGoon() ) break; 
	    		 			}//else println("guard  fails");  //parg is null when there is no guard (onEvent)
	    		 }
	    		//onEvent
	    		if( currentEvent.getEventId().equals("usercmd") ){
	    		 		String parg="robotcommand(CMD)";
	    		 		parg = updateVars(null,Term.createTerm("usercmd(X)"),  Term.createTerm("usercmd(CMD)"), 
	    		 			    		  					Term.createTerm(currentEvent.getMsg()), parg);
	    		 		if( parg != null ) sendMsg("robotcommand","qarobotexecutor", ActorContext.dispatch, parg ); 
	    		 }
	    		//onEvent
	    		if( currentEvent.getEventId().equals("usercmd") ){
	    		 		String parg="addMove(CMD)";
	    		 		parg = updateVars(null, Term.createTerm("usercmd(X)"), Term.createTerm("usercmd(robotgui(CMD))"), 
	    		 			    		  					Term.createTerm(currentEvent.getMsg()), parg);
	    		 			if( parg != null ) {
	    		 				aar = solveGoal( parg , 0, "","" , "");
	    		 				//println(getName() + " plan " + curPlanInExec  +  " interrupted=" + aar.getInterrupted() + " action goon="+aar.getGoon());
	    		 				if( aar.getInterrupted() ){
	    		 					curPlanInExec   = "sendUserMoveCommands";
	    		 					if( ! aar.getGoon() ) break;
	    		 				} 			
	    		 				if( aar.getResult().equals("failure")){
	    		 					if( ! switchToPlan("prologFailure").getGoon() ) break;
	    		 				}else if( ! aar.getGoon() ) break;
	    		 			}
	    		 }
	    		//onEvent
	    		if( currentEvent.getEventId().equals("usercmd") ){
	    		 		String parg="robotcommand(stopLed)";
	    		 		parg = updateVars(null,Term.createTerm("usercmd(X)"),  Term.createTerm("usercmd(CMD)"), 
	    		 			    		  					Term.createTerm(currentEvent.getMsg()), parg);
	    		 		if( parg != null ) sendMsg("robotcommand","qarobotexecutor", ActorContext.dispatch, parg ); 
	    		 }
	    		if( repeatPlan(0).getGoon() ) continue;
	    		returnValue = continueWork;  
	    break;
	    }//while
	    return returnValue;
	    }catch(Exception e){
	    println( getName() + " ERROR " + e.getMessage() );
	    throw e;
	    }
	    }
	    public boolean inspection() throws Exception{	//public to allow reflection
	    try{
	    	curPlanInExec =  "inspection";
	    	boolean returnValue = suspendWork;
	    while(true){
	    nPlanIter++;
	    		temporaryStr = " \"START INSPECTION\" ";
	    		println( temporaryStr );  
	    		temporaryStr = unifyMsgContent("robotcommand(X)","robotcommand(blinkLed)", guardVars ).toString();
	    		sendMsg("robotcommand","qarobotexecutor", ActorContext.dispatch, temporaryStr ); 
	    		temporaryStr = unifyMsgContent("robotcommandd(X,Y)","robotcommandd(a(low),1000)", guardVars ).toString();
	    		sendMsg("robotcommandd","qarobotexecutor", ActorContext.dispatch, temporaryStr ); 
	    		temporaryStr = unifyMsgContent("robotcommandd(X,Y)","robotcommandd(w(low),2000)", guardVars ).toString();
	    		sendMsg("robotcommandd","qarobotexecutor", ActorContext.dispatch, temporaryStr ); 
	    		//delay
	    		aar = delayReactive(3000,"" , "");
	    		if( aar.getInterrupted() ) curPlanInExec   = "inspection";
	    		if( ! aar.getGoon() ) break;
	    		temporaryStr = " \"Controller to robot --> please shoot a photo!\" ";
	    		println( temporaryStr );  
	    		temporaryStr = unifyMsgContent("robotcommand(X)","robotcommand(shootAPic)", guardVars ).toString();
	    		sendMsg("robotcommand","qarobotexecutor", ActorContext.dispatch, temporaryStr ); 
	    		//ReceiveMsg
	    		 		 aar = receiveAMsg(10000, "" , "" ); 	//could block
	    				if( aar.getInterrupted() ){
	    					curPlanInExec   = "playTheGame";
	    					if( ! aar.getGoon() ) break;
	    				} 			
	    				if( ! aar.getGoon() ){
	    					System.out.println("			WARNING: receiveMsg in " + getName() + " TOUT " + aar.getTimeRemained() + "/" +  10000);
	    					addRule("tout(receive,"+getName()+")");
	    				} 		 
	    				//println(getName() + " received " + aar.getResult() );
	    		//onMsg
	    		if( currentMessage.msgId().equals("picture") ){
	    			String parg = " \"Analizing pic\" ";
	    			parg = updateVars(null, Term.createTerm("picture"), Term.createTerm("picture"), 
	    				    		  					Term.createTerm(currentMessage.msgContent()), parg);
	    				if( parg != null ) println( parg );  
	    		}temporaryStr = unifyMsgContent("robotcommandd(X,Y)","robotcommandd(d(low),1000)", guardVars ).toString();
	    		sendMsg("robotcommandd","qarobotexecutor", ActorContext.dispatch, temporaryStr ); 
	    		temporaryStr = unifyMsgContent("robotcommandd(X,Y)","robotcommandd(w(low),4000)", guardVars ).toString();
	    		sendMsg("robotcommandd","qarobotexecutor", ActorContext.dispatch, temporaryStr ); 
	    		//delay
	    		aar = delayReactive(5000,"" , "");
	    		if( aar.getInterrupted() ) curPlanInExec   = "inspection";
	    		if( ! aar.getGoon() ) break;
	    		temporaryStr = " \"Controller to robot --> please shoot a photo!\" ";
	    		println( temporaryStr );  
	    		temporaryStr = unifyMsgContent("robotcommand(X)","robotcommand(shootAPic)", guardVars ).toString();
	    		sendMsg("robotcommand","qarobotexecutor", ActorContext.dispatch, temporaryStr ); 
	    		//ReceiveMsg
	    		 		 aar = receiveAMsg(10000, "" , "" ); 	//could block
	    				if( aar.getInterrupted() ){
	    					curPlanInExec   = "playTheGame";
	    					if( ! aar.getGoon() ) break;
	    				} 			
	    				if( ! aar.getGoon() ){
	    					System.out.println("			WARNING: receiveMsg in " + getName() + " TOUT " + aar.getTimeRemained() + "/" +  10000);
	    					addRule("tout(receive,"+getName()+")");
	    				} 		 
	    				//println(getName() + " received " + aar.getResult() );
	    		//onMsg
	    		if( currentMessage.msgId().equals("picture") ){
	    			String parg = " \"Analizing pic\" ";
	    			parg = updateVars(null, Term.createTerm("picture"), Term.createTerm("picture"), 
	    				    		  					Term.createTerm(currentMessage.msgContent()), parg);
	    				if( parg != null ) println( parg );  
	    		}temporaryStr = unifyMsgContent("robotcommandd(X,Y)","robotcommandd(d(low),1000)", guardVars ).toString();
	    		sendMsg("robotcommandd","qarobotexecutor", ActorContext.dispatch, temporaryStr ); 
	    		temporaryStr = unifyMsgContent("robotcommandd(X,Y)","robotcommandd(w(low),4000)", guardVars ).toString();
	    		sendMsg("robotcommandd","qarobotexecutor", ActorContext.dispatch, temporaryStr ); 
	    		//delay
	    		aar = delayReactive(5000,"" , "");
	    		if( aar.getInterrupted() ) curPlanInExec   = "inspection";
	    		if( ! aar.getGoon() ) break;
	    		temporaryStr = " \"Controller to robot --> please shoot a photo!\" ";
	    		println( temporaryStr );  
	    		temporaryStr = unifyMsgContent("robotcommand(X)","robotcommand(shootAPic)", guardVars ).toString();
	    		sendMsg("robotcommand","qarobotexecutor", ActorContext.dispatch, temporaryStr ); 
	    		//ReceiveMsg
	    		 		 aar = receiveAMsg(10000, "" , "" ); 	//could block
	    				if( aar.getInterrupted() ){
	    					curPlanInExec   = "playTheGame";
	    					if( ! aar.getGoon() ) break;
	    				} 			
	    				if( ! aar.getGoon() ){
	    					System.out.println("			WARNING: receiveMsg in " + getName() + " TOUT " + aar.getTimeRemained() + "/" +  10000);
	    					addRule("tout(receive,"+getName()+")");
	    				} 		 
	    				//println(getName() + " received " + aar.getResult() );
	    		//onMsg
	    		if( currentMessage.msgId().equals("picture") ){
	    			String parg = " \"Analizing pic\" ";
	    			parg = updateVars(null, Term.createTerm("picture"), Term.createTerm("picture"), 
	    				    		  					Term.createTerm(currentMessage.msgContent()), parg);
	    				if( parg != null ) println( parg );  
	    		}temporaryStr = unifyMsgContent("robotcommandd(X,Y)","robotcommandd(d(low),1000)", guardVars ).toString();
	    		sendMsg("robotcommandd","qarobotexecutor", ActorContext.dispatch, temporaryStr ); 
	    		temporaryStr = unifyMsgContent("robotcommandd(X,Y)","robotcommandd(w(low),4000)", guardVars ).toString();
	    		sendMsg("robotcommandd","qarobotexecutor", ActorContext.dispatch, temporaryStr ); 
	    		//delay
	    		aar = delayReactive(5000,"" , "");
	    		if( aar.getInterrupted() ) curPlanInExec   = "inspection";
	    		if( ! aar.getGoon() ) break;
	    		temporaryStr = " \"Controller to robot --> please shoot a photo!\" ";
	    		println( temporaryStr );  
	    		temporaryStr = unifyMsgContent("robotcommand(X)","robotcommand(shootAPic)", guardVars ).toString();
	    		sendMsg("robotcommand","qarobotexecutor", ActorContext.dispatch, temporaryStr ); 
	    		//ReceiveMsg
	    		 		 aar = receiveAMsg(10000, "" , "" ); 	//could block
	    				if( aar.getInterrupted() ){
	    					curPlanInExec   = "playTheGame";
	    					if( ! aar.getGoon() ) break;
	    				} 			
	    				if( ! aar.getGoon() ){
	    					System.out.println("			WARNING: receiveMsg in " + getName() + " TOUT " + aar.getTimeRemained() + "/" +  10000);
	    					addRule("tout(receive,"+getName()+")");
	    				} 		 
	    				//println(getName() + " received " + aar.getResult() );
	    		//onMsg
	    		if( currentMessage.msgId().equals("picture") ){
	    			String parg = " \"Analizing pic\" ";
	    			parg = updateVars(null, Term.createTerm("picture"), Term.createTerm("picture"), 
	    				    		  					Term.createTerm(currentMessage.msgContent()), parg);
	    				if( parg != null ) println( parg );  
	    		}temporaryStr = unifyMsgContent("robotcommandd(X,Y)","robotcommandd(d(low),1000)", guardVars ).toString();
	    		sendMsg("robotcommandd","qarobotexecutor", ActorContext.dispatch, temporaryStr ); 
	    		temporaryStr = unifyMsgContent("robotcommandd(X,Y)","robotcommandd(w(low),2000)", guardVars ).toString();
	    		sendMsg("robotcommandd","qarobotexecutor", ActorContext.dispatch, temporaryStr ); 
	    		//delay
	    		aar = delayReactive(5000,"" , "");
	    		if( aar.getInterrupted() ) curPlanInExec   = "inspection";
	    		if( ! aar.getGoon() ) break;
	    		temporaryStr = unifyMsgContent("robotcommandd(X,Y)","robotcommandd(a(low),1000)", guardVars ).toString();
	    		sendMsg("robotcommandd","qarobotexecutor", ActorContext.dispatch, temporaryStr ); 
	    		//delay
	    		aar = delayReactive(1000,"" , "");
	    		if( aar.getInterrupted() ) curPlanInExec   = "inspection";
	    		if( ! aar.getGoon() ) break;
	    		temporaryStr = unifyMsgContent("robotcommand(X)","robotcommand(stopLed)", guardVars ).toString();
	    		sendMsg("robotcommand","qarobotexecutor", ActorContext.dispatch, temporaryStr ); 
	    		temporaryStr = " \"Sending the result to the A S C\" ";
	    		println( temporaryStr );  
	    		temporaryStr = unifyMsgContent("inspectionresult(X)","inspectionresult(harmful)", guardVars ).toString();
	    		sendMsg("inspectionresult","asc", ActorContext.dispatch, temporaryStr ); 
	    		temporaryStr = " \"START AUTONOMOUS\" ";
	    		println( temporaryStr );  
	    		if( ! switchToPlan("autonomousPhase").getGoon() ) break;
	    break;
	    }//while
	    return returnValue;
	    }catch(Exception e){
	    println( getName() + " ERROR " + e.getMessage() );
	    throw e;
	    }
	    }
	    public boolean autonomousPhase() throws Exception{	//public to allow reflection
	    try{
	    	curPlanInExec =  "autonomousPhase";
	    	boolean returnValue = suspendWork;
	    while(true){
	    nPlanIter++;
	    		if( (guardVars = evalTheGuard( " !?harmful" )) != null ){
	    		temporaryStr = " \"Alarm Detected: Blinking led\" ";
	    		temporaryStr = substituteVars(guardVars,temporaryStr);
	    		println( temporaryStr );  
	    		}
	    		if( (guardVars = evalTheGuard( " !?harmful" )) != null ){
	    		temporaryStr = unifyMsgContent("robotcommand(X)","robotcommand(blinkLed)", guardVars ).toString();
	    		sendMsg("robotcommand","qarobotexecutor", ActorContext.dispatch, temporaryStr ); 
	    		}
	    		if( (guardVars = evalTheGuard( " !?clonedMoved(MOVE,DURATION)" )) != null ){
	    		temporaryStr = unifyMsgContent("robotcommandd(X,Y)","robotcommandd(MOVE,DURATION)", guardVars ).toString();
	    		sendMsg("robotcommandd","qarobotexecutor", ActorContext.dispatch, temporaryStr ); 
	    		}
	    		if( (guardVars = evalTheGuard( " ??clonedMoved(MOVE,DURATION)" )) != null ){
	    		{ String parg = "suspend(DURATION)";
	    		parg = substituteVars(guardVars,parg);
	    		  aar = solveGoal( parg , 0, "","" , "" );
	    		//println(getName() + " plan " + curPlanInExec  +  " interrupted=" + aar.getInterrupted() + " action goon="+aar.getGoon());
	    		if( aar.getInterrupted() ){
	    			curPlanInExec   = "autonomousPhase";
	    			if( ! aar.getGoon() ) break;
	    		} 			
	    		if( aar.getResult().equals("failure")){
	    		if( ! switchToPlan("prologFailure").getGoon() ) break;
	    		}else if( ! aar.getGoon() ) break;
	    		}
	    		}
	    		{ String parg = "clonedMoved(MOVE,DURATION)";
	    		  aar = solveGoal( parg , 0, "","" , "" );
	    		//println(getName() + " plan " + curPlanInExec  +  " interrupted=" + aar.getInterrupted() + " action goon="+aar.getGoon());
	    		if( aar.getInterrupted() ){
	    			curPlanInExec   = "autonomousPhase";
	    			if( ! aar.getGoon() ) break;
	    		} 			
	    		if( aar.getResult().equals("failure")){
	    		if( ! switchToPlan("sendUserMoveCommands").getGoon() ) break;
	    		}else if( ! aar.getGoon() ) break;
	    		}
	    		if( repeatPlan(0).getGoon() ) continue;
	    break;
	    }//while
	    return returnValue;
	    }catch(Exception e){
	    println( getName() + " ERROR " + e.getMessage() );
	    throw e;
	    }
	    }
	    public boolean sendSecondRobot() throws Exception{	//public to allow reflection
	    try{
	    	curPlanInExec =  "sendSecondRobot";
	    	boolean returnValue = suspendWork;
	    while(true){
	    nPlanIter++;
	    		{ String parg = "clonedMoved(MOVE,DURATION)";
	    		  aar = solveGoal( parg , 0, "","" , "" );
	    		//println(getName() + " plan " + curPlanInExec  +  " interrupted=" + aar.getInterrupted() + " action goon="+aar.getGoon());
	    		if( aar.getInterrupted() ){
	    			curPlanInExec   = "sendSecondRobot";
	    			if( ! aar.getGoon() ) break;
	    		} 			
	    		if( aar.getResult().equals("failure")){
	    		if( ! switchToPlan("sendUserMoveCommands").getGoon() ) break;
	    		}else if( ! aar.getGoon() ) break;
	    		}
	    		if( repeatPlan(0).getGoon() ) continue;
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
	    public boolean handleAlarm() throws Exception{	//public to allow reflection
	    try{
	    	curPlanInExec =  "handleAlarm";
	    	boolean returnValue = suspendWork;
	    while(true){
	    nPlanIter++;
	    		temporaryStr = " \"handleAlarm\" ";
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
	    public boolean handleTimeOut() throws Exception{	//public to allow reflection
	    try{
	    	curPlanInExec =  "handleTimeOut";
	    	boolean returnValue = suspendWork;
	    while(true){
	    nPlanIter++;
	    		temporaryStr = " \"handleTout and stop\" ";
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
	    protected void initSensorSystem(){
	    	//doing nothing in a QActor
	    }
	    
	 
		/* 
		* ------------------------------------------------------------
		* APPLICATION ACTIONS
		* ------------------------------------------------------------
		*/
		
	  }
	
