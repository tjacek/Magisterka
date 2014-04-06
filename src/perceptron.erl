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

-include("structures.hrl").
-include("debug.hrl").

%% API
-export([learn/4,classify/3,test_perceptron/0]).

applyPercetron(W,X) ->
    Dp=(utils:dot_product(X,W)),
    if
        Dp>0.0 -> 1.0;
        true -> -1.0
    end.

label2Category(Label) ->
  if
    Label==1.0 -> true;
    true -> false
  end.

classify(_Classifier=#classifier{ algorithm = perceptron,attributes = Attributes, class = _Class, specific_classifier = W}, Example,Options) ->
  X=[1.0|tuple_to_list(Example)],
  Label=applyPercetron(W,X),
  Cat=label2Category(Label),
  %io:format("Obtained Category: ~p~n", [X]),
  {ok, Cat}.

learn(Attributes, Class, NumberedExamples, Options) ->
  W = dummy_perceptron(),
  io:format(" ~p~n",[NumberedExamples]),
  file:write_file("C/train.txt", io_lib:fwrite("~p.\n", [NumberedExamples])),
  {ok, #classifier{ algorithm = perceptron, attributes = Attributes, class = Class, specific_classifier = W}}.

dummy_perceptron() ->
  W=[-5.0,1.0,1.0,0.0].

test_perceptron() ->
    X=[1.0,2.0,3.0],
    W=[1.0,1.0,1.0],
    P=applyPercetron(W,X),
    io:format("Obtained X: ~p~n", [P]).

