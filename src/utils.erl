%%%-------------------------------------------------------------------
%%% @author tjacek
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. kwi 2014 20:44
%%%-------------------------------------------------------------------
-module(utils).
-author("tjacek").

%% API
-export([choose_category/2]).
-export([labels2reals/1,dot_product/2,subs/2,distance/2,substract/2]).


get_attribute_position(Attributes, AttributeName) ->
   Pos=mllib_tools:get_attribute_pos(Attributes, AttributeName),
   N= length(Attributes),
   case  Pos<N of
     true -> Pos;
     false -> N
   end. 

exclude(List,Number) ->
  case length(List) < Number of
    true  -> del_last(List);
    false -> del_last(List)
  end.

del_nth_from_list( N,List) ->
  {L1, [ToReturn|L2]} = lists:split(N-1, List),
  New_list=L1 ++ L2,
  {New_list,ToReturn}.

del_last(List) ->
  Label=lists:last(List),
  New_list=lists:delete(Label,List),
  {New_list,Label}.

labels2reals(Labels)->
  Conv=fun(Label) ->
    if
      Label==true -> 1.0;
      true -> -1.0
    end
  end,
  lists:map(Conv,Labels).

dot_product(X,Y) -> dot_product(X,Y,0.0).
dot_product([],[],Acc) -> Acc;
dot_product([A|Ha],[B|Hb],Acc) ->
  dot_product(Ha,Hb, A*B +Acc).

distance(X,Y) ->  distance(X,Y,0.0).
distance([],[],Acc) ->  math:sqrt(Acc);
distance([A|Ha],[B|Hb],Acc) ->
  Det=A-B,
  distance(Ha,Hb,Acc + Det*Det).

subs(X,Y) -> subs(X,Y,0.0).
subs([],[],Acc) -> Acc;
subs([A|Ha],[B|Hb],Acc) -> subs(Ha,Hb, A-B +Acc).

substract(X,Y) -> substract(X,Y,[]).
substract([],[],Acc) -> Acc;
substract([A|Ha],[B|Hb],Acc) ->
  Sub=abs(A-B),
  substract(Ha,Hb, [Sub|Acc]).

choose_category(Examples,DefaultCategory) ->
  %io:format("Examples  ~p \n",[Examples]),
  Labels=get_labels(Examples),
  %io:format("Labels  ~p \n",[Examples]),
  Categories=count_categories(Labels),
  io:format("Categories ~p \n",[dict:to_list(Categories)]),
  N = dict:size(Categories),
  case 0<N of
    true -> find_most_comon(Categories);
    false -> DefaultCategory
  end.

get_labels(Examples) ->
  lists:map(fun(Tuple) ->element(2,Tuple) end,Examples).

count_categories(Labels) -> count_categories(Labels,dict:new()).
count_categories([],Categories) -> Categories;
count_categories([H|T],Categories) ->  count_categories(T,dict:update_counter(H,1,Categories)).

find_most_comon(Categories) ->
  ListOfTuples=dict:to_list(Categories),
  Comp = fun(T1,T2)->
    Count1=element(2,T1),
    Count2=element(2,T2),
    Count1<Count2
  end,
  SortedCategories=lists:sort(Comp,ListOfTuples),
  Cat=lists:last(SortedCategories),
  element(1,Cat).

%max_element(L) -> max_element(L,Max,0,0).

curry(InnerFun,Arg) ->
  fun() ->
    InnerFun(Arg)
  end.
