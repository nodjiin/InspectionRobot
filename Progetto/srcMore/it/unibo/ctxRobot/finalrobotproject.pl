%====================================================================================
% Context ctxRobot  SYSTEM-configuration: file it.unibo.ctxRobot.finalRobotProject.pl 
%====================================================================================
context(ctxrobot, "localhost",  "TCP", "8020" ).  		 
context(ctxasc, "localhost",  "TCP", "8023" ).  		 
%%% -------------------------------------------
qactor( asc , ctxasc  ).
qactor( qarobotcontroller , ctxrobot  ).
%%% -------------------------------------------
qactor( qarobotexecutor , ctxrobot  ).

