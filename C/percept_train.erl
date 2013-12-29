-module(percept_train).
-export([start/1, stop/0, init/1]).
-export([get_perceptron/2,crt/2, per/0]).

get_weights(0,Acc) -> Acc;
get_weights(N,Acc) -> get_weights(N-1,[per()|Acc]).

get_perceptron(N,K) ->
    start("./extprg"),
    timer:sleep(500),
    crt(N,K),
    W=get_weights(K+1,[]),
    stop(),
    W.

start(ExtPrg) ->
    spawn(?MODULE, init, [ExtPrg]).
stop() ->
    complex ! stop.

crt(X,Y) ->
    call_port({crt, X,Y}).
per() ->
    call_port({per, 1,2}).

call_port(Msg) ->
    complex ! {call, self(), Msg},
    receive
	{complex, Result} ->
	    Result
    end.

init(ExtPrg) ->
    register(complex, self()),
    process_flag(trap_exit, true),
    Port = open_port({spawn, ExtPrg}, [{packet, 2}, binary]),
    loop(Port).

loop(Port) ->
    receive
	{call, Caller, Msg} ->
	    Port ! {self(), {command, term_to_binary(Msg)}},
	    receive
		{Port, {data, Data}} ->
		    Caller ! {complex, binary_to_term(Data)}
	    end,
	    loop(Port);
	stop ->
	    Port ! {self(), close},
	    receive
		{Port, closed} ->
		    exit(normal)
	    end;
	{'EXIT', Port, Reason} ->
	    exit(port_terminated)
    end.
