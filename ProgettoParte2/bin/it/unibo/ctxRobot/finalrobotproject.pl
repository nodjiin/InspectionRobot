%====================================================================================
% Context ctxRobot  SYSTEM-configuration: file it.unibo.ctxRobot.finalRobotProject.pl 
%====================================================================================
context(ctxrobot, "localhost",  "TCP", "8020" ).  		 
%%% -------------------------------------------
qactor( asc , ctxrobot  ).
qactor( qarobotcontroller , ctxrobot  ).
%%% -------------------------------------------
qactor( qarobotexecutor , ctxrobot  ).

