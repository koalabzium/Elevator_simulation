-module(computer).
-compile(export_all).

manage(FirstPID, SecondPID) ->
    receive
        {call, Floor} ->
            io:format("Pobieram pozycje\n"),
            FirstPID ! {getPos, self(), Floor},
            manage(FirstPID, SecondPID);
        {gotFirst, Position, Floor} ->
            SecondPID ! {getPos, self(), Position, Floor},
            manage(FirstPID, SecondPID);
        {gotSecond, FirtsPos, SecPos, Floor} ->
            {Pid, OtherPos} = getCloser({FirstPID, FirtsPos}, {SecondPID, SecPos}, Floor),
            Pid ! {go, Floor, OtherPos},
            manage(FirstPID, SecondPID)
    end.
    


getCloser({Pid1, Pos1}, {_, Pos2}, Floor) when abs(Pos1-Floor)=<abs(Pos2-Floor) ->
    {Pid1,Pos2};

getCloser({_, Pos1}, {Pid2, Pos2}, Floor) when abs(Pos1-Floor)>abs(Pos2-Floor) ->
    {Pid2, Pos1}.

