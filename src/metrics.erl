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
-export([compute/2,save_stats/3]).
-export([accuracy/2,error_rate/2,recall/2,precision/2,f_measure/2,f_beta/2]).
-export([precision_metric/2,recall_metric/2,f_measure_metric/2]).

save_stats(TrueLabels,PredLabels,Output)->
  Statistics=compute(TrueLabels,PredLabels),
  io:format("~p \n",[Statistics]),
  Str=to_str(Statistics),
  io:format("~p \n",[Str]),
  file:write_file(Output, Str).

compute(TrueLabels,PredLabels) ->
  Confusion_matrix=confusion_matrix(TrueLabels,PredLabels),
  Length=length(TrueLabels),
  print_cm(Confusion_matrix),
  Metrics=[fun metrics:accuracy/2,fun metrics:error_rate/2,fun metrics:recall/2,
    fun metrics:precision/2,fun metrics:f_measure/2,fun metrics:f_beta/2],
  Lambda=fun(F)->  F(Confusion_matrix,Length) end,
  Statistics = lists:map(Lambda,Metrics).

error_rate(Confusion_matrix,Length) ->
  Accuracy=element(2,accuracy(Confusion_matrix,Length)),
  Error_rate = 1.0 - Accuracy,
  {error_rate,Error_rate}.

accuracy(Confusion_matrix,Length) ->
  Lambda =fun(Category) ->
    true_positives(Category,Confusion_matrix)
  end,
  TP=for_all_categories(Lambda,Confusion_matrix),
  Accuracy=lists:sum(TP)/Length,
  {accuracy,Accuracy}.

precision(Confusion_matrix,Length) ->
  apply_metrics(precision,fun metrics:precision_metric/2,Confusion_matrix).

precision_metric(Category,Confusion_matrix) ->
  Tp=true_positives(Category,Confusion_matrix),
  P=all_true(Category,Confusion_matrix),
  Tp/P.

recall(Confusion_matrix,Length) ->
  apply_metrics(recall,fun metrics:recall_metric/2,Confusion_matrix).

recall_metric(Category,Confusion_matrix)->
  TP=true_positives(Category,Confusion_matrix),
  P=all_positives(Category,Confusion_matrix),
  TP/P.

f_measure(Confusion_matrix,Length) ->
  apply_metrics(f_measure,fun metrics:f_measure_metric/2,Confusion_matrix).

f_measure_metric(Category,Confusion_matrix) ->
  Precision=precision_metric(Category,Confusion_matrix),
  Recall=recall_metric(Category,Confusion_matrix),
  2.0*Precision*Recall/(Precision+Recall).

f_beta(Confusion_matrix,Length) ->
  f_beta(Confusion_matrix,Length,2.0).

f_beta(Confusion_matrix,Length,Beta) ->
  Metric=fun(Category,Confusion_matrix) ->
    f_beta_metric(Category,Confusion_matrix,Beta)
  end,
  apply_metrics(f_beta,Metric,Confusion_matrix).

f_beta_metric(Category,Confusion_matrix,Beta) ->
  Precision=precision_metric(Category,Confusion_matrix),
  Recall=recall_metric(Category,Confusion_matrix),
 (1.0+Beta*Beta)*(Precision * Recall)/(Beta*Beta*(Precision+Recall)).

apply_metrics(Atom,Metric,Confusion_matrix) ->
  Lambda=fun(Category) ->
    Value=Metric(Category,Confusion_matrix),
    {Category,Value}
  end,
  {Atom,for_all_categories(Lambda,Confusion_matrix)}.

true_positives(Category,Confusion_matrix) ->
  get_value(Category,Category,Confusion_matrix).

all_positives(Category,Confusion_matrix) ->
  Lambda = fun(Key) ->
    get_value(Key,Category,Confusion_matrix)
  end,
  lists:sum(for_all_categories(Lambda,Confusion_matrix)).

all_true(Category,Confusion_matrix) ->
  Lambda = fun(Key) ->
    get_value(Category,Key,Confusion_matrix)
  end,
  lists:sum(for_all_categories(Lambda,Confusion_matrix)).

for_all_categories(Lambda,Confusion_matrix) ->
  Categories=dict:fetch_keys(Confusion_matrix),
  lists:map(Lambda,Categories).

get_value(TrueCat,PredCat,Confusion_matrix) ->
  Predictions=dict:fetch(TrueCat,Confusion_matrix),
  dict:fetch(PredCat,Predictions).

to_str(Statistics) ->
  to_str(Statistics,"").

to_str([],Acc) -> Acc;
to_str([H|T],Acc)->
  Key=atom_to_list(element(1,H)),
  Value=element(2,H),
  Line=case(is_list(Value)) of
    true  ->   io_lib:nl() ++ Key  ++ ":" ++ to_str(Value);
    false ->   StrValue =  io_lib:format("~.3f",[Value]),%float_to_list(Value),
               Key ++ ":" ++ StrValue
  end,
  NewStr= Acc ++ io_lib:nl() ++Line,
  to_str(T,NewStr).

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
