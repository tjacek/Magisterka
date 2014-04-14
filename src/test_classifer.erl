%%%-------------------------------------------------------------------
%%% @author tjacek
%%% @copyright (C) 2013, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. lis 2013 16:00
%%%-------------------------------------------------------------------
-module(test_classifer).
-author("tjacek").

%% API
-export([experiment/2,createClassifer/3,testClassifer/3 ]).

experiment(Algorithm,Filename) ->
  {Attributes, Instances} = mllib:read(arff,[{file,Filename}]),
  Pred=get_rand(0.3),
  {TrainingSet,TestSet} = split(Instances,Pred),
  ClassName=cat,
  {ok, Classifier} = mllib:learn(Attributes, ClassName, TrainingSet, Algorithm, [ ]),
  {TrueLabels,TestList}=regression:parse_labels(TestSet),
  TestInstances=lists:map(fun(X)-> to_tuple(X) end,TestList),
  PredLabels=learn(Classifier,TestInstances),
  Sucess=count_sucess(TrueLabels,PredLabels)/ length(TestInstances),
  io:format("~p",[Sucess]).

count_sucess(TrueLabels,PredLabels) -> count_sucess(TrueLabels,PredLabels,0.0).
count_sucess([T|Ht],[P|Pt],Counter) ->
  if T==P -> count_sucess(Ht,Pt,Counter+1.0);
     true -> count_sucess(Ht,Pt,Counter)
  end;
count_sucess([],[],Counter) ->  Counter.


  to_tuple(L) ->
  X=lists:nth(1,L),
  Y=lists:nth(2,L),
  Z=lists:nth(3,L),
  {X,Y,Z}.

split(Instances,Rand) -> split([],[],Instances,Rand).
split(TestSet,TrainingSet,[T|H],Rand) ->
  R=Rand(),
  if R -> split(TestSet,[T|TrainingSet],H,Rand);
     true -> split([T|TestSet],TrainingSet,H,Rand)
  end;
split(TestSet,TrainingSet,[],Rand) ->
  {TestSet,TrainingSet}.

get_rand(P) ->
  fun() ->
    rand(P)
  end.

rand(P) ->
  RanNumber=random:uniform(),
  if
    RanNumber>P -> false;
    true -> true
  end.

getClassifier(Filename,Alg) ->
  {Attributes, TrainingExamples} = mllib:read(arff,[{file,Filename}]),
  ClassName = cat,
  {ok, Classifier} = mllib:learn(Attributes, ClassName, TrainingExamples, Alg, [ ]).

createClassifer(Alg,Training,Name) ->
  {ok, Classifier}=getClassifier(Training,Alg),
  mllib:write_classifier(Name,Classifier).

testClassifer(Filename,Name,Output) ->
  {ok, Classifier}=mllib:read_classifier(Name),
  {Attributes, TestExamples} = mllib:read(arff,[{file,Filename}]),
  Labeled=learn(Classifier,TestExamples),
%  io:format("Obtained Category: ~p~n", [Labeled]),
  file:write_file(Output, io_lib:fwrite("~p.\n", [Labeled])).

learn(Classifier,TestExamples) ->  learn(Classifier,TestExamples,[]).
learn(Classifier,[],LabeledExamples) -> LabeledExamples;
learn(Classifier,[A|Ha],LabeledExamples) ->
    {ok,Label}=mllib:classify(Classifier, A, [ ]),
    learn(Classifier ,Ha,[Label|LabeledExamples]).