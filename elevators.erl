-module(elevators).
-compile(export_all).

first(Position) ->
    [{dpid, DrawingPID}] = ets:lookup(pids, dpid),
    receive
        {getPos, Pid, Floor} ->
            io:format("Pozycja pierwszej ~p \n", [Position]),
            Pid ! {gotFirst, Position, Floor},
            first(Position);
        {go, Floor, SecPos} -> 
            io:format("Pierwsza jedzie na pietro ~p \n\n", [Floor]),
            DrawingPID ! {draw_elevator, Floor, SecPos},
            first(Floor)
    end.

second(Position) ->
    [{dpid, DrawingPID}] = ets:lookup(pids, dpid),
    receive
        {getPos, Pid, FirstPos, Floor} ->
            io:format("Pozycja drugiej ~p \n", [Position]),
            Pid ! {gotSecond, FirstPos, Position, Floor},
            second(Position);
        {go, Floor, FirstPos} -> 
            io:format("Druga jedzie na pietro ~p \n\n", [Floor]),
            DrawingPID ! {draw_elevator, FirstPos, Floor},
            second(Floor)
    end.