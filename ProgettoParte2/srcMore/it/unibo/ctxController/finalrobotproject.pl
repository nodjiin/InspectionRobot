%====================================================================================
% Context ctxController  SYSTEM-configuration: file it.unibo.ctxController.finalRobotProject.pl 
%====================================================================================
context(ctxrobot, "localhost",  "TCP", "8020" ).  		 
context(ctxcontroller, "localhost",  "TCP", "8021" ).  		 
context(ctxsecondrobot, "localhost",  "TCP", "8022" ).  		 
context(ctxasc, "localhost",  "TCP", "8023" ).  		 
%%% -------------------------------------------
qactor( asc , ctxrobot  ).
qactor( qarobotcontroller , ctxrobot  ).
%%% -------------------------------------------
qactor( qarobotexecutor , ctxrobot  ).
qactor( qasecondrobot , ctxsecondrobot  ).

