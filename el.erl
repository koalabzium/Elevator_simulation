-module(el).
-compile(export_all).

%init(Drawing) drawing = true | false


init(Drawing) when is_boolean(Drawing) ->
    DrawingPID = spawn(el, drawing, [Drawing]),
    FirstPID = spawn(elevators, first, [0]),
    SecondPID = spawn(elevators, second, [0]),
    ComputerPID = spawn(computer, manage, [FirstPID, SecondPID]),
    ErrorPID = spawn(el, error_handler, []),
    MainPID = spawn(el, main_loop, []),
    ets:new(pids, [set, named_table, public]),
    ets:new(globals, [set, named_table, public]),
    ets:insert(pids, [{dpid, DrawingPID}, {cpid, ComputerPID}, {erpid, ErrorPID}, {buttons, []},{mpid, MainPID}]),
    io:format(os:cmd(clear)),
    io:format("Witaj w symulacji windy!"),
    timer:sleep(1000),
    io:format(os:cmd(clear)),
    start().


start()->
        [{erpid, ErrorPID}] = ets:lookup(pids, erpid),
        [{dpid, DrawingPID}] = ets:lookup(pids, dpid),
        [{mpid, MainPID}] = ets:lookup(pids, mpid),
        FloorsAmount = io:fread("Podaj liczbe pieter: ", "~d"),
		
        case FloorsAmount of
            {ok, [Floors]} -> 
                if Floors > 0 ->
                    ets:insert(globals, [{floors, Floors}]),
                    MainPID ! {loop, 5000000000},
                    
                    DrawingPID ! {draw_elevator, 0,0},
                    spawn_buttons(Floors);
                true ->
                    ErrorPID ! {negative_floor_amount}
                end;
            {error, _} ->  ErrorPID ! {wrong_floor_amount}
        end.

error_handler() ->
    receive
        {wrong_floor_amount} ->
            io:format("Liczba pieter musi byc integerem!\n"),
            timer:sleep(500),
            start(),
            error_handler();
        {negative_floor_amount} ->
            io:format("Podaj dodatnia ilosc pieter!\n"),
            timer:sleep(500),
            start(),
            error_handler()
    end.

main_loop() ->
   
    receive
        {loop, Time} -> loop(Time)       
    end.

loop(0) -> halt();
loop(N) -> loop(N-1).

spawn_buttons(-1) ->
    io:format("\n")
;

spawn_buttons(FloorsAmount) ->
    Button = spawn(el, button, [FloorsAmount]),
    io:format("Tworzenie guzika ~p\n", [FloorsAmount]),
    [{buttons, Buttons}] = ets:lookup(pids, buttons),
    ets:insert(pids, {buttons, lists:append(Buttons,[Button]) }),
    spawn_buttons(FloorsAmount-1).

button(Floor) ->
    [{dpid, DrawingPID}] = ets:lookup(pids, dpid),
    % timer:sleep(trunc(rand:uniform()*20000+5000)),
    [{floors, FloorsAmount}] = ets:lookup(globals, floors),
    timer:sleep((((Floor rem 2)+1)*5+(trunc(rand:uniform()*FloorsAmount)))*1000),
    DrawingPID ! {draw_elevator, Floor},
    timer:sleep(500),
    send(Floor),
    timer:sleep(1000),
    button(Floor).
   
send(Floor) ->
    io:format("Guzik przycisniety na pietrze ~p\n", [Floor]),
    [{cpid, ComputerPID}] = ets:lookup(pids, cpid),
    ComputerPID ! {call, Floor}.

%%COMPUTER




%ELEVATOR




drawing(Draw) ->
    if Draw ->
        receive
            {draw_elevator, FirstElevPos, SecElevPos} ->

                [{floors, FloorsAmount}] = ets:lookup(globals, floors),
                io:format(os:cmd(clear)),
                draw_elevator(FloorsAmount, FirstElevPos, SecElevPos),
                drawing(Draw);
            {draw_elevator, Floor} ->
                [{floors, FloorsAmount}] = ets:lookup(globals, floors),
                io:format(os:cmd(clear)),
                draw_elevator(FloorsAmount, Floor),
                drawing(Draw)
        end;
    true -> 
        receive
            {draw_elevator, _, _} ->
                io:format("Rysowanie wind.\n"), drawing(Draw);
            {draw_elevator, _} ->
                io:format("Rysowanie przycisniecia.\n"), drawing(Draw)
        end
    end.
        

draw_elevator(-1, _)  ->
    io:format("\n\n");

draw_elevator(FloorsAmount, Floor) when FloorsAmount == Floor ->
    io:format("\n\t*|  ___  ___ "),
    draw_elevator(FloorsAmount-1, Floor);

draw_elevator(FloorsAmount, Floor) when FloorsAmount =/= Floor ->
    io:format("\n\t~p|  ___  ___ ", [FloorsAmount]),
    draw_elevator(FloorsAmount-1, Floor).       

draw_elevator(-1, _, _) ->
  io:format("\n\n");

draw_elevator(FloorsAmount, FirstElevPos, SecElevPos) when FirstElevPos == SecElevPos
                                                    andalso FirstElevPos == FloorsAmount ->
  io:format("\n\t~p|  _1_  _2_ ", [FloorsAmount]),
  draw_elevator(FloorsAmount-1, FirstElevPos, SecElevPos);

draw_elevator(FloorsAmount, FirstElevPos, SecElevPos) when FirstElevPos == FloorsAmount ->
  io:format("\n\t~p|  _1_  ___ ", [FloorsAmount]),
  draw_elevator(FloorsAmount-1, FirstElevPos, SecElevPos);

draw_elevator(FloorsAmount, FirstElevPos, SecElevPos) when SecElevPos == FloorsAmount ->
  io:format("\n\t~p|  ___  _2_ ", [FloorsAmount]),
  draw_elevator(FloorsAmount-1, FirstElevPos, SecElevPos);

draw_elevator(FloorsAmount, FirstElevPos, SecElevPos) ->
  io:format("\n\t~p|  ___  ___ ", [FloorsAmount]),
  draw_elevator(FloorsAmount-1, FirstElevPos, SecElevPos).

   


