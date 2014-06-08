%%%-------------------------------------------------------------------
%%% @author tjacek
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. kwi 2014 12:38
%%%-------------------------------------------------------------------
-module(regression).
-author("tjacek").

%% API
-export([test_regression/2,create_model/3,parse_labels/1]).

test_regression(TrainFile,TestFile) ->
  {Attributes, TrainSet} = mllib:read(arff,[{file,TrainFile}]),
  {Attributes, TestSet} = mllib:read(arff,[{file,TestFile}]),
  X=remove_pred_variable(TestSet),
  Model=kernel_smoother:learn(TrainSet),
  Y=kernel_smoother:apply_regression(X,Model),
  %PredY=lists:map(R,X),
  %Error=utils:subs(TrueY,PredY),
  io:format("~p~n",[Y]).

create_model(Alg,Filename,Output) ->
  {Attributes, Training} = mllib:read(arff,[{file,Filename}]),
  Model=kernel_smoother:learn(Training).%Alg(Training),
  %file:write_file(Output, io_lib:fwrite("~p.\n", [Model])).

read_dataset(Filename) ->
  {Attributes, Samples} = mllib:read(arff,[{file,Filename}]),
  lists:map(fun(X)->tuple_to_list(X) end,Samples).

get_regresion_function(Model)->
  fun(X) ->
    kernel_smoother:regression(X,Model)
  end.

pred_variable(Samples) ->
  First_tuple=lists:nth(1,Samples),
  N=tuple_size(First_tuple),
  Lambda = fun(Tuple) ->
    element(N,Tuple)
  end,
  Y=lists:map(Lambda,Samples).

parse_labels(Samples) ->
  Extract_Labels=fun(Sample) ->
    N=size(Sample),
    element(N,Sample)
  end,
  Labels=lists:map(Extract_Labels,Samples),
  Instances =remove_pred_variable(Samples),
  {Labels,Instances}.

remove_pred_variable(Raw_samples) ->
  Extract_Instances=fun(Sample) ->
    ListSample=tuple_to_list(Sample) ,
    Label=lists:last(ListSample),
    lists:delete(Label,ListSample)
  end,
  lists:map(Extract_Instances,Raw_samples).