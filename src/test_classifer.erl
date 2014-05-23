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
-export([run_exp/1,experiment/5,createClassifer/3,testClassifer/3,test_c45/0,build_c45/0 ]).

run_exp(Args) ->
  Algorithm = list_to_atom(lists:nth(1,Args)),
  TrainFile = lists:nth(2,Args),
  TestFile = lists:nth(3,Args),
  Output = lists:nth(4,Args),
  %io:format("~p",[Algorithm]),
  Options=[],
  experiment(Algorithm,TrainFile,TestFile,Output,Options).

experiment(Algorithm,TrainFile,TestFile,Output,Options) ->
  %io:format("~p",[TrainFile]),
  {Attributes, TrainSet} = mllib:read(arff,[{file,TrainFile}]),
  {Attributes, TestSet} = mllib:read(arff,[{file,TestFile}]),
  ClassName=cat,
  {ok, Classifier} = mllib:learn(Attributes, ClassName, TrainSet, Algorithm, Options),
  {TrueLabels,TestList}=regression:parse_labels(TestSet),
  TestInstances=lists:map(fun(X)-> list_to_tuple(X)  end,TestList),
  PredLabels=learn(Classifier,TestInstances),
  file:write_file(Output, io_lib:fwrite("~p.\n", [PredLabels])),
  %Pred=get_rand(0.3),
  %{TrainingSet,TestSet} = split(Instances,Pred),
  %Sucess=accuracy(TrueLabels,PredLabels)/ length(TestInstances),
  %Sucess=confusion_matrix(TrueLabels,PredLabels),
  io:format("~p",[PredLabels]).

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
  file:write_file(Output, io_lib:fwrite("~p.\n", [Labeled])).

learn(Classifier,TestExamples) ->  learn(Classifier,TestExamples,[]).
learn(Classifier,[],LabeledExamples) -> lists:reverse(LabeledExamples);
learn(Classifier,[A|Ha],LabeledExamples) ->
    {ok,Label}=mllib:classify(Classifier, A, [ ]),
    learn(Classifier ,Ha,[Label|LabeledExamples]).


test_c45() ->
  {ok, Classifier}=mllib:read_classifier("classifiers/c45Linear.txt"),
  {ok,Label}=mllib:classify(Classifier, {1.0,2.0,3.0}, [ ]),
  io:format("~p",[Label]),
  {ok,Label2}=mllib:classify(Classifier, {2.0,2.0,1.5}, [ ]),
  io:format("~p",[Label2]).

build_c45() ->
  {Attributes, TrainSet} = mllib:read(arff,[{file,"data/linearInput.arff"}]),
  ClassName=cat,
  Options =[{test_choice,information_gain_criterion},{trim,none}],
  {ok, Classifier} = mllib:learn(Attributes, ClassName, TrainSet, c45, Options),
  io:format("~p",[Classifier]).