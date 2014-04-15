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

classify(_Classifier=#classifier{ algorithm = nearest_neighbors,attributes = Attributes, class = _Class, specific_classifier = LabeledPoints}, Example,Options) ->
  %Cat=true,
  Point=tuple_to_list(Example),
  Cat=nn_kernel(Point,LabeledPoints,3),
  %io:format("Example: ~p~n", [Distances]),
  {ok, Cat}.

nn_kernel(X,Points,K) ->
  Metric=fun(Instance) ->
    Point=element(1,Instance),
    Label=element(2,Instance),
    D=utils:distance(Point,X),
    {D,Label}
  end,
  Compare=fun(LabeledPoint1,LabeledPoint2) ->
    Distance1= element(1,LabeledPoint1),
    Distance2= element(1,LabeledPoint2),
    Distance1 < Distance2
  end,
  Distances=lists:map(Metric,Points),
  Sorted_distances=lists:sort(Compare,Distances),
  K_neighbors=element(1,lists:split(K, Sorted_distances)),
  choose_category(K_neighbors).

choose_category(K_neighbors) ->
  {Distances,Labels}=lists:unzip(K_neighbors),
  Categories=count_categories(Labels),
  Comp = fun(T1,T2)->
    Count1=element(2,T1),
    Count2=element(2,T2),
    Count1<Count2
  end,
  SortedCategories=lists:sort(Comp,Categories),
  Cat=lists:last(SortedCategories),
  element(1,Cat).

count_categories(Labels) -> count_categories(Labels,dict:new()).
count_categories([],Dict) -> dict:to_list(Dict);
count_categories([H|T],Dict) ->
  NewDict=dict:update_counter(H,1, Dict),
  count_categories(T,NewDict).


get_point(NumberedExamples) ->
  Example=element(2,NumberedExamples),
  Label=element(2,Example),
  Point=tuple_to_list(element(1,Example)),
  {Point,Label}.