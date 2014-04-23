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
  %Sucess=accuracy(TrueLabels,PredLabels)/ length(TestInstances),
  Sucess=confusion_matrix(TrueLabels,PredLabels),
  io:format("~p",[Sucess]).

accuracy(TrueLabels,PredLabels) ->
  Size =length(TrueLabels) + length(PredLabels),
  accuracy(TrueLabels,PredLabels,0.0)/Size.
accuracy([T|Ht],[P|Pt],Counter) ->
  if T==P -> accuracy(Ht,Pt,Counter+1.0);
     true -> accuracy(Ht,Pt,Counter)
  end;
accuracy([],[],Counter) ->  Counter.

confusion_matrix(TrueLabels,PredLabels) ->
  confusion_matrix(TrueLabels,PredLabels,dict:new()).

confusion_matrix([],[],Dict) -> Dict;
confusion_matrix( [TrueCategory|TrueLabels],[PredCategory|PredLabels],Dict) ->
  UpdatedDict1 = dict:update(TrueCategory,fun(X)->X end,dict:new(),Dict),
  CategoryDict = dict:fetch(TrueCategory,UpdatedDict1),
  NewCategoryDict = dict:update_counter(PredCategory,1.0,CategoryDict),
  UpdatedDict=  dict:store(TrueCategory, NewCategoryDict, UpdatedDict1),
  confusion_matrix(TrueLabels,PredLabels,UpdatedDict).

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