%====================================================================================
% Context ctxSecondRobot  SYSTEM-configuration: file it.unibo.ctxSecondRobot.finalRobotProject.pl 
%====================================================================================
context(ctxrobot, "localhost",  "TCP", "8020" ).  		 
context(ctxsecondrobot, "localhost",  "TCP", "8022" ).  		 
%%% -------------------------------------------
qactor( asc , ctxrobot  ).
qactor( qarobotcontroller , ctxrobot  ).
%%% -------------------------------------------
qactor( qarobotexecutor , ctxrobot  ).

