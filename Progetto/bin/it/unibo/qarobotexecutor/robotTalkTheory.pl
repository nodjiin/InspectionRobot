/*
===============================================================
robotTalkTheory.pl
===============================================================
*/
curPlan("pdefault").
%%currentsolution("nosolution").
/*
------------------------------------------------------------
Robot GUI usercmd 
------------------------------------------------------------
*/
		
robotgui(CMD) :-
	%% actorPrintln(   usercmd(CMD) ),
	moveCmdTable(CMD, MOVE, SPEED ),
	myname(Robot),
	%% actorPrintln(   usercmd(Robot,MOVE, SPEED) ),
	Angle=0,
	Duration=0,	
	Robot <- execute(MOVE, SPEED, Angle, Duration, '', '').

moveCmdTable( w(low), forward, 40 ).
moveCmdTable( w(medium), forward, 70 ).
moveCmdTable( w(high), forward, 100 ).
moveCmdTable( a(low), left, 40 ).
moveCmdTable( a(medium), left, 70 ).
moveCmdTable( a(high), left, 100 ).
moveCmdTable( s(low), backward, 40 ).
moveCmdTable( s(medium), backward, 70 ).
moveCmdTable( s(high), backward, 100 ).
moveCmdTable( d(low), right, 40 ).
moveCmdTable( d(medium), right, 70 ).
moveCmdTable( d(high), right, 100 ).
moveCmdTable( h(low), stop, 0 ).
moveCmdTable( h(medium), stop, 70 ).
moveCmdTable( h(high), stop, 100 ).
/*
------------------------------------------------------------
Acquire input sentence from System.in
------------------------------------------------------------
*/
execFromInput:-
  	class("it.unibo.robot.interpreter.RobotInterpreter") <- readInput returns INPUT,
	executeInput(INPUT).

executeInput( do(GUARD,MOVE,EVENTS, PLANS) ) :-
	!,
	myname(Robot),
	actorPrintln(   do(GUARD,MOVE,EVENTS, PLANS)  ),
	runTheSentence(Robot, sentence( GUARD, MOVE, EVENTS, PLANS ) ).
	
%% 1) Attempt to execute Input as a Prolog term
executeInput( true  ) :- !, actorPrintln( solution(true)  ) . %%WARNING: actorPrintln( true  ) fails
executeInput( Input  ) :-
	%% actorPrintln( executeInput(Input)  ),
 	Input, !,
 	actorPrintln( Input  ).
 
%% 2) Convert a string into a Prolog term
%% Useful if the InputStr is a message
executeInput( InputS ) :-
	class("it.unibo.qactors.web.GuiUiKb") <- buildCorrectPrologString(InputS) returns InputStr,
  	%% actorPrintln(   executeInputStr(InputStr)  ),
 	text_term(InputStr,Input),
  	%% actorPrintln(   attemptToSolve(Input)  ),
	Input,
	actorPrintln(   result(Input)  ),
	!.

%% 3) Handle the input string as a sentence for the robot
executeInput(InputStr):-
	myname(Robot),
	runSentence(Robot,InputStr).

runSentence(Robot,Input):-
 	 %% actorPrintln(  runSentence( Robot,Input )  ),
 	 curPlan(P),
 	 class("it.unibo.robot.interpreter.RobotInterpreter") <- checkSentence( Input ) returns SENTENCE,
 	 class("it.unibo.robot.interpreter.TalkUtils")        <- storeMoveRep(Robot,SENTENCE,P)  ,
	 removeRule( input(CURSENTENCE) ),
	 %% actorPrintln(  toexecute( Robot,CURSENTENCE )  ),
	 runTheSentence( Robot,CURSENTENCE ),
	 !.
/*
-----------------------------------
The case of Talk sentences
-----------------------------------
*/	
runTheSentence(Robot, sentence( GUARD, MOVE, EVENTS, PLANS ) ):-
	GUARD,	!,										%%evaluate the guard (a Prolog term)
 	%% actorPrintln(  sentence4( GUARD, MOVE, EVENTS, PLANS ) ),
  	executeCmd(Robot, MOVE, EVENTS, PLANS), 
  	!. 

/*
-----------------------------------
The case of Prolog Goal sentences
-----------------------------------
*/	 
runTheSentence(Robot, sentence( GUARD, GOAL, DURATION, ANSWEREV, EVENTS, PLANS ) ) :-
	GUARD,	!,										%%evaluate the guard (a Prolog term)
	%% actorPrintln(  sentence6( GUARD, GOAL, DURATION, ANSWEREV, EVENTS, PLANS ) ),
	addRule( unsolved(sentence( GUARD, GOAL, DURATION, ANSWEREV, EVENTS, PLANS )) ).
 	
%% Attempt to solve a Prolog goal  	
runTheSentence(Robot, CURSENTENCE ) :- 
	%% actorPrintln(  addRule( CURSENTENCE )  ), 	
  	addRule(CURSENTENCE).
  	

%%% ---------  Robot move	---------------
executeCmd( Robot, move(robotmove,CMD,SPEED,DURATION,ANGLE), Events, Plans ):-
 	!,
 	mapCmdToMove(CMD,MOVE),
	%% actorPrintln(  executeCmd(Robot,MOVE, SPEED, ANGLE, DURATION, Events, Plans) ),
	Robot <- execute(MOVE, SPEED, ANGLE, DURATION, Events, Plans).
%%% ---------  Move names	---------------
mapCmdToMove( mf, forward  ).	
mapCmdToMove( moveforward, forward  ).
mapCmdToMove( mb, backward ).
mapCmdToMove( movebackward, backward  ).
mapCmdToMove( ml, left ).
mapCmdToMove( moveleft, left  ).
mapCmdToMove( mr, right ).
mapCmdToMove( moveright, right  ).
mapCmdToMove( h,  stop ). 
mapCmdToMove( movehalt, stop  ).

%%% ---------  Play sound	---------------
executeCmd( Robot,  move(playsound,FNAME,DURATION,ANSWEREVENT), Events, Plans ):-
	%% actorPrintln(  executeCmd(Robot, playsound( FNAME,DURATION,ANSWEREVENT ), Events, Plans) ),
	!,
	Robot <- playSound( FNAME, DURATION, ANSWEREVENT, Events, Plans ).
executeCmd( Robot,  move(playsound, FNAME, DURATION), Events, Plans ):-
	%% actorPrintln(  executeCmd(Robot, playsound( FNAME,DURATION ), Events, Plans) ),
	!,
	Robot <- playSound( FNAME, DURATION, '', Events, Plans ).

%%% --------- Photo	---------------
executeCmd( Robot,  move( photo(DURATION,FNAME,ANSWEREVENT) ), Events, Plans ):-
	%% actorPrintln(  executeCmd(Robot, photo( DURATION,FNAME,ANSWEREVENT )) ),
	!,
	Robot <- photo( FNAME, DURATION, ANSWEREVENT, Events, Plans ).
executeCmd( Robot, move( photo(DURATION,FNAME) ) , Events, Plans ):-
	%% actorPrintln(  executeCmd(Robot, photo(DURATION,FNAME) )) ),
	Robot <- photo( FNAME, DURATION, '', Events, Plans ).
	
%%% ---------  Solve	---------------
/*
We cannot solve a goal in asynch way while pengine is engaged
Thus we ignore DURATION and Events, Plans
*/
executeCmd( Robot,   move(solve,GOAL,DURATION, ANSWEREVENT), Events, Plans ):-
	%%actorPrintln(  executeCmd(Robot, move(solve,GOAL,DURATION, ANSWEREVENT) ) ),
	executeCmd( Robot, move(solve,GOAL,DURATION), Events, Plans ).
	
executeCmd( Robot, move(solve,GOAL,DURATION), Events, Plans ):-
	%% actorPrintln(  executeCmd(Robot, move(solve,GOAL,DURATION) ) ),
	Robot <- solveSentence(sentence(true, GOAL, 0, '', '', '')) returns AAR.
	%% AAR <- getResult returns RES,
	%% actorPrintln( result(GOAL,RES) ).
	%% The following  DOES NOT WORK since pengine engaged
	%% Robot <- solveSentence(sentence(true, GOAL, DURATION, '', Events, Plans)) returns AAR.	
 	
%%% ---------  Println	---------------	
executeCmd( Robot,  move(print,ARG), Events, Plans ):-
	text_term(ARGS,ARG),
	actorPrintln(  ARGS ).
%%% ---------  Emit	---------------	
executeCmd( Robot,  move(emit,EVID,CONTENT), Events, Plans ):-
	actorPrintln(  move(emit,EVID,CONTENT) ),
	Robot <- emit( EVID,CONTENT ).
%%% ---------  Forward	---------------	
executeCmd( Robot,  move(forward, DEST, MSGID, MSGCNT), Events, Plans ):-
	%actorPrintln(  forward( 1,Robot,MSGID,DEST,MSGCNT ) ),	
	text_term(A1,DEST),  	 
	text_concat("''",A1,A2),
	text_concat(A2,"''",DESTSTR),
	%% text_term(B1,MSGCNT),   
	%% text_concat("@",B1,B2),
	%% text_concat(B2,"@",B3),	
 	Robot <- forwardFromProlog( MSGID , DESTSTR , MSGCNT ). 
 
 %%% ---------  Runplan	---------------	
executeCmd( Robot, move(runplan,P), Events, Plans ):-
 	%% actorPrintln( runplan(P) ),
	execPlan(Robot,P,0).
execPlan(Robot,P,PC):-
	plan(PC, P, S) ,
	%% actorPrintln( runplan(S) ),
	runTheSentence(Robot,S),
	PC1 is PC + 1,
	execPlan(Robot,P,PC1).
execPlan(_,_,_).

%%% ---------  Showplan	---------------		
executeCmd( Robot, move(showplan,P), Events, Plans ):-
	%% actorPrintln( showPlan(P) ),
	showPlan(P).
showPlan(P):-
	showPlan(P,0).
showPlan(P,PC):-
	plan(PC, P, S) ,
	actorPrintln(  plan(PC, P, S )  ),
	PC1 is PC + 1,
	showPlan(P,PC1).
showPlan(_,_).

showPlan:-
	curPlan(P),
	showPlan(P).

%%% --------- storePlan	--------------- 	
storePlan(FName,P):-
%%plan(Num,PlanName,Sentence) 	 
	bagof( plan(PC, P, S) , plan(PC, 'pdefault', S) , L ),
	%% actorPrintln( storePlan( FName, L) ),
	class("it.unibo.robot.interpreter.RobotInterpreter") <-  writeListInFile( FName ,L ).

loadPlan( FName ):-
	class("it.unibo.robot.interpreter.RobotInterpreter") <-  consultFromFile( FName  ).
 
/*
-----------------------------------
Used to test guards and actions
-----------------------------------
*/	
v(12).

%% Fibonacci with cache (to be used in guards)

fib(V):-
	fib( V,N ),!,
	actorPrintln( fib(V,N) ).
fib(V):-
	fibWithCache(V,N), 					
	actorPrintln( fib(V,N) ).

fib( 0,1 ).
fib( 1,1 ).
fibWithCache( V,N ) :-
	fib( V,N ),!.
fibWithCache( V,N ) :-
	V1 is V-1, V2 is V-2,
  	fibWithCache(V1,N1), fibWithCache(V2,N2),
  	N is N1 + N2,
	%% actorPrintln( fib( V,N ) ),
	assert( fib( V,N ) ).

welcome :-   actorPrintln("welcome from robotTalkTheory.pl").


/*
------------------------------------------------------------
initialize
------------------------------------------------------------
*/
initialize  :-  
	actorPrintln("initializing the system ..."),
	class("it.unibo.robot.interpreter.RobotInterpreter") <- initForParser,
	class("it.unibo.robot.interpreter.RobotInterpreter") <- startUserStandardInput("").

:- initialization(initialize).


/*
------------------------------------------------------------
My functions
------------------------------------------------------------
*/

robotdur(CMD,DUR) :-	
	getTimestamp(TIMEPRIMO),
	actorPrintln(CMD),
	moveCmdTable(CMD, MOVE, SPEED ),
	myname(Robot),
	Angle=0,
	Robot <- execute(MOVE, SPEED, Angle, DUR, "alarm" , "startLed"),
	getTimestamp(TIMESECONDO),
	NEWDUR is DUR - TIMESECONDO + TIMEPRIMO,
	NEWDUR > 500,
	Robot <- execute(MOVE, SPEED, Angle, NEWDUR, '' , '').	
	
	getTimestamp(TIME) :-
	 class("java.lang.System") <- currentTimeMillis returns TIME.  

          blinkLed :-
                class("it.unibo.qarobotexecutor.LedUtils") <- startBlinking.
          stopLed :-
          class("it.unibo.qarobotexecutor.LedUtils") <- stopBlinking.


	
	