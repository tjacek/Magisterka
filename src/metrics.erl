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
-export([accuracy/2,error_rate/2,sensitivity/2]).

compute(TrueLabels,PredLabels) ->
  Confusion_matrix=confusion_matrix(TrueLabels,PredLabels),
  Length=length(TrueLabels),
  print_cm(Confusion_matrix),
  Metrics=[fun metrics:accuracy/2,fun metrics:error_rate/2,fun metrics:sensitivity/2],
  Lambda=fun(F)->  F(Confusion_matrix,Length) end,
  Statistics = lists:map(Lambda,Metrics),
  io:format("~p ",[Statistics]).

error_rate(Confusion_matrix,Length) ->
  Accuracy=element(2,accuracy(Confusion_matrix,Length)),
  Error_rate = 1.0 - Accuracy,
  {error_rate,Error_rate}.

sensitivity(Confusion_matrix,Length) ->
  Lambda=fun(Category) ->
    TP=true_positives(Category,Confusion_matrix),
    P=all_positives(Category,Confusion_matrix),
    {Category,TP/P}
  end,
  {sensitivity,for_all_categories(Lambda,Confusion_matrix)}.

accuracy(Confusion_matrix,Length) ->
  Lambda =fun(Category) ->
    true_positives(Category,Confusion_matrix)
  end,
  TP=for_all_categories(Lambda,Confusion_matrix),
  Accuracy=lists:sum(TP)/Length,
  {accuracy,Accuracy}.

true_positives(Category,Confusion_matrix) ->
  get_value(Category,Category,Confusion_matrix).

all_positives(Category,Confusion_matrix) ->
  Lambda = fun(Key) ->
    get_value(Key,Category,Confusion_matrix)
  end,
  lists:sum(for_all_categories(Lambda,Confusion_matrix)).

for_all_categories(Lambda,Confusion_matrix) ->
  Categories=dict:fetch_keys(Confusion_matrix),
  lists:map(Lambda,Categories).

get_value(TrueCat,PredCat,Confusion_matrix) ->
  Predictions=dict:fetch(TrueCat,Confusion_matrix),
  dict:fetch(PredCat,Predictions).

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