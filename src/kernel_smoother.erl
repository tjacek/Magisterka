%%%-------------------------------------------------------------------
%%% @author tjacek
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. kwi 2014 13:31
%%%-------------------------------------------------------------------
-module(kernel_smoother).
-author("tjacek").

%% API
-export([test/0]).

get_kernel(B) ->
  fun(X,Y) ->
    exp_kernel(X,Y,B)
  end.

exp_kernel(X,Y,B) ->
  D=distance(X,Y),
  Exponent= (-1.0*D*D) / (2* B),
  math:exp(Exponent).

distance(X,Y) ->  distance(X,Y,0.0).
distance([],[],Acc) ->  math:sqrt(Acc);
distance([A|Ha],[B|Hb],Acc) ->
  Det=A-B,
  distance(Ha,Hb,Acc + Det*Det).

test() ->
  H=[1.0,1.0],
  T=[3.0,2.0],
  MyKernel=get_kernel(2.0),
  Res=MyKernel(H,T),
  io:format("~p~n",[Res]).
