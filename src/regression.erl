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

test_regression(TestFile,ModelFile) ->
  X =read_dataset(TestFile),
  %Model =mllib:read_classifier(ModelFile),
  Model=create_model(c45,"data/linearInput.arff","kl"),
  R=get_regresion_function(Model),
  PredY=lists:map(R,X),
  %Error=utils:subs(TrueY,PredY),
  io:format("~p~n",[PredY]).

create_model(Alg,Filename,Output) ->
  {Attributes, Training} = mllib:read(arff,[{file,Filename}]),
  Model=kernel_smoother:learn(Training).%Alg(Training),
  %file:write_file(Output, io_lib:fwrite("~p.\n", [Model])).

read_dataset(Filename) ->
  {Attributes, Samples} = mllib:read(arff,[{file,Filename}]),
 % Con = fun(X) ->
 %   tuple_to_list(X)
 % end.
  lists:map(fun(X)->tuple_to_list(X) end,Samples).
  %{Labels,Instances}=parse_labels(Samples).

get_regresion_function(Model)->
  fun(X) ->
    kernel_smoother:regression(X,Model)
  end.

parse_labels(Samples) ->
  Extract_Labels=fun(Sample) ->
    N=size(Sample),
    element(N,Sample)
  end,
  Extract_Instances=fun(Sample) ->
    ListSample=tuple_to_list(Sample) ,
    Label=lists:last(ListSample),
    lists:delete(Label,ListSample)
  end,
  Labels=lists:map(Extract_Labels,Samples),
  Instances =lists:map(Extract_Instances,Samples),
  {Labels,Instances}.