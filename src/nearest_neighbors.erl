%%%-------------------------------------------------------------------
%%% @author tjacek
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. kwi 2014 22:30
%%%-------------------------------------------------------------------
-module(nearest_neighbors).
-author("tjacek").

-include("structures.hrl").
-include("debug.hrl").

%% API
-export([learn/4,classify/3]).

learn(Attributes, Class, NumberedExamples, Options) ->
  LabeledPoints=lists:map(fun(X) -> get_point(X) end,NumberedExamples),
  {ok, #classifier{ algorithm = nearest_neighbors, attributes = Attributes, class = Class, specific_classifier = LabeledPoints}}.

classify(_Classifier=#classifier{ algorithm = nearest_neighbors,attributes = Attributes, class = _Class, specific_classifier = W}, Example,Options) ->
  Cat=true,
  Point=tuple_to_list(Example),
  io:format("Example: ~p~n", [Example]),
  {ok, Cat}.

nn_kernel(X,Points) ->
  D=fun(P) ->
    %utils:distance(P,X)
    get_point(P)
  end,
  Distances=lists:map(D,Points),
  %Sorted_distance=lists:sort(Distances),
  io:format("OK ~p",[Distances]).

get_point(NumberedExamples) ->
  Example=element(2,NumberedExamples),
  Label=element(2,Example),
  Point=tuple_to_list(element(1,Example)),
  {Point,Label}.