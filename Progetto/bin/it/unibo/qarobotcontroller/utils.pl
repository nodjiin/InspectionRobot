/*sensor( distance(DIST,DIR,POS) ):-
	!, actorobj(Actor),
	( DIST < 21,!,
	  Actor <- emit( sensordata, sensordata(distance(DIST,DIR,POS)) );
	  true ).
	  */
	  
turn(450).	  
	  
getTimestamp(TIME) :-
	 class("java.lang.System") <- currentTimeMillis returns TIME.

subtract(PRIMO,SECONDO,RES) :-
	 class("java.lang.Math") <- subtractExact( PRIMO , SECONDO ) returns RES.
	 
duplicateMoves([HM | TM], [HD | TD]) :-
	duplicateMoves(TM,TD),
	assert(clonedMoved(HM, HD)),
	assert(wait(HD)).
duplicateMoves([],[]).

suspend(DUR) :-
	class("it.unibo.qarobotcontroller.Utils") <- suspend(DUR).
