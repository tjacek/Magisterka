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
-export([labels2reals/1,dot_product/2,subs/2,distance/2]).

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
dot_product([A|Ha],[B|Hb],Acc) -> dot_product(Ha,Hb, A*B +Acc).

distance(X,Y) ->  distance(X,Y,0.0).
distance([],[],Acc) ->  math:sqrt(Acc);
distance([A|Ha],[B|Hb],Acc) ->
  Det=A-B,
  distance(Ha,Hb,Acc + Det*Det).

subs(X,Y) -> subs(X,Y,0.0).
subs([],[],Acc) -> Acc;
subs([A|Ha],[B|Hb],Acc) -> subs(Ha,Hb, A-B +Acc).

%max_element(L) -> max_element(L,Max,0,0).

curry(InnerFun,Arg) ->
  fun() ->
    InnerFun(Arg)
  end.
