%%%-------------------------------------------------------------------
%%% @author tjacek
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. kwi 2014 19:34
%%%-------------------------------------------------------------------
-module(metrics).
-author("tjacek").

%% API
-export([compute/2]).

compute(TrueLabels,PredLabels) ->
  Confusion_matrix=confusion_matrix(TrueLabels,PredLabels),
  print_cm(Confusion_matrix).

print_cm(Confusion_matrix)->
  CM=dict:to_list(Confusion_matrix),
  Lambda= fun(Tuple) ->
    Category = element(1,Tuple),
    Result= dict:to_list(element(2,Tuple)),
    {Category,Result}
  end,
  CM2=lists:map(Lambda,CM),
  io:format("~p\n",[CM2]).

confusion_matrix(TrueLabels,PredLabels) ->
  confusion_matrix(TrueLabels,PredLabels,dict:new()).

confusion_matrix([],[],Dict) -> Dict;
confusion_matrix( [TrueCategory|TrueLabels],[PredCategory|PredLabels],Dict) ->
  UpdatedDict1 = dict:update(TrueCategory,fun(X)->X end,dict:new(),Dict),
  CategoryDict = dict:fetch(TrueCategory,UpdatedDict1),
  NewCategoryDict = dict:update_counter(PredCategory,1.0,CategoryDict),
  UpdatedDict=  dict:store(TrueCategory, NewCategoryDict, UpdatedDict1),
  confusion_matrix(TrueLabels,PredLabels,UpdatedDict).