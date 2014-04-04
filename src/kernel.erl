%%%-------------------------------------------------------------------
%%% @author tjacek
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. kwi 2014 13:31
%%%-------------------------------------------------------------------
-module(kernel).
-author("tjacek").

%% API
-export([test/0]).

distance(X,Y) ->  distance(X,Y,0.0).
distance([],[],Acc) ->  math:sqrt(Acc);
distance([A|Ha],[B|Hb],Acc) ->
  Det=A-B,
  distance(Ha,Hb,Acc + Det*Det).

test() ->
  H=[1.0,1.0],
  T=[3.0,2.0],
  distance(H,T).