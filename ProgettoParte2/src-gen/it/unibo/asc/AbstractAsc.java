/* Generated by AN DISI Unibo */ 
package it.unibo.asc;
import alice.tuprolog.Term;
import alice.tuprolog.Struct;
import it.unibo.qactors.ActorContext;
import it.unibo.is.interfaces.IOutputEnvView;
import it.unibo.qactors.planned.QActorPlanned;
import it.unibo.qactors.action.ActionDummy;
import it.unibo.qactors.action.AsynchActionResult;
import it.unibo.qactors.action.IActorAction;
import it.unibo.qactors.action.IActorAction.ActionExecMode;

public abstract class AbstractAsc extends QActorPlanned { 
	protected AsynchActionResult aar = null;
	protected boolean actionResult = true;
	protected alice.tuprolog.SolveInfo sol;
	
			protected static IOutputEnvView setTheEnv(IOutputEnvView outEnvView ){
				return outEnvView;
			}
	
	
		public AbstractAsc(String actorId, ActorContext myCtx, IOutputEnvView outEnvView )  throws Exception{
			super(actorId, myCtx, "./srcMore/it/unibo/asc/plans.txt", 
			"./srcMore/it/unibo/asc/WorldTheory.pl",
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
	    		temporaryStr = "asc(starts)";
	    		println( temporaryStr );  
	    		if( ! switchToPlan("handlingInspectionResults").getGoon() ) break;
	    break;
	    }//while
	    return returnValue;
	    }catch(Exception e){
	    println( getName() + " ERROR " + e.getMessage() );
	    throw e;
	    }
	    }
	    public boolean handlingInspectionResults() throws Exception{	//public to allow reflection
	    try{
	    	curPlanInExec =  "handlingInspectionResults";
	    	boolean returnValue = suspendWork;
	    while(true){
	    nPlanIter++;
	    		temporaryStr = " \"Asc waiting\" ";
	    		println( temporaryStr );  
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
	    		if( currentMessage.msgId().equals("inspectionresult") ){
	    			String parg="harmful";
	    			parg = updateVars(null,Term.createTerm("inspectionresult(X)"),  Term.createTerm("inspectionresult(harmful)"), 
	    				    		  					Term.createTerm(currentMessage.msgContent()), parg);
	    			if( parg != null ) emit( "harmful", parg );
	    		}//onMsg
	    		if( currentMessage.msgId().equals("inspectionresult") ){
	    			String parg = " \"ASC: bag is harmful\" ";
	    			parg = updateVars(null, Term.createTerm("inspectionresult(X)"), Term.createTerm("inspectionresult(harmful)"), 
	    				    		  					Term.createTerm(currentMessage.msgContent()), parg);
	    				if( parg != null ) println( parg );  
	    		}//onMsg
	    		if( currentMessage.msgId().equals("inspectionresult") ){
	    			String parg = " \"ASC: bag is safe\" ";
	    			parg = updateVars(null, Term.createTerm("inspectionresult(X)"), Term.createTerm("inspectionresult(safe)"), 
	    				    		  					Term.createTerm(currentMessage.msgContent()), parg);
	    				if( parg != null ) println( parg );  
	    		}if( repeatPlan(0).getGoon() ) continue;
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
	
