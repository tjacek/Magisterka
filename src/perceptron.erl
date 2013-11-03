%%%-------------------------------------------------------------------
%%% @author tjacek
%%% @copyright (C) 2013, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. lis 2013 19:26
%%%-------------------------------------------------------------------
-module(perceptron).
-author("tjacek").

%% API
-export([dot_product/2,test_dot/0,test_perceptron/0]).

dot_product(X,Y) -> dot_product(X,Y,0.0).
dot_product([],[],Acc) -> Acc;
dot_product([A|Ha],[B|Hb],Acc) -> dot_product(Ha,Hb, A*B +Acc).


getPercetron(W,B) ->
  Perceptron =
    fun(X) ->
      Dp=(dot_product(X,W)+B),
      if
        Dp>0.0 -> 0.0;
        true -> 1.0
      end
    end.

test_dot() ->
   X=dot_product([1.0,2.0,3.0],[2.0,2.0,3.0]),
   io:format("Obtained X: ~p~n", [X]).

test_perceptron() ->
    X=[1.0,2.0,3.0],
    B=-4.0,
    W=[1.0,1.0,1.0],
    P=getPercetron(W,B),
    io:format("Obtained X: ~p~n", [P(X)]).

