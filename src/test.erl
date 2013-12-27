%%%-------------------------------------------------------------------
%%% @author tjacek
%%% @copyright (C) 2013, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. lis 2013 16:00
%%%-------------------------------------------------------------------
-module(test).
-author("tjacek").

%% API
-export([test_arff/0,testLearning/4,testBayes/1,createClassifer/3,testClassifer/3 ]).

test_arff() ->
  {Attributes, TrainingExamples} = mllib:read(arff,[{file,"data/Test3.arff"}]),
  io:format("OK").

showExample(Classifier, Example) ->
  {ok, Category} = mllib:classify(Classifier, Example, [ ]),
  io:format("Obtained Category: ~p~n", [Category]).

getData(Filename) -> mllib:read(arff,[{file,Filename}]).

getClassifier(Filename,Alg) ->
  {Attributes, TrainingExamples} = mllib:read(arff,[{file,"train/linear.arff"}]),
  ClassName = cat,
  {ok, Classifier} = mllib:learn(Attributes, ClassName, TrainingExamples, Alg, [ ]).

createClassifer(Alg,Training,Name) ->
  {ok, Classifier}=getClassifier(Training,Alg),
  mllib:write_classifier(Name,Classifier).

testClassifer(Filename,Name,Output) ->
  {ok, Classifier}=mllib:read_classifier(Name),
  {Attributes, TestExamples} = mllib:read(arff,[{file,Filename}]),
  Labeled=learn(Classifier,TestExamples),
  io:format("Obtained Category: ~p~n", [Labeled]),
  file:write_file(Output, io_lib:fwrite("~p.\n", [Labeled])).

testLearning(Alg,Training,Test,Output) ->
  {ok, Classifier} = getClassifier(Training,Alg),
  {Attributes, TestExamples} = mllib:read(arff,[{file,Test}]).
%  Labeled=learn(Classifier,TestExamples),
%    io:format("Obtained Category: ~p~n", [Labeled]),
%  file:write_file(Output, io_lib:fwrite("~p.\n", [Labeled])) .


learn(Classifier,TestExamples) ->  learn(Classifier,TestExamples,[]).
learn(Classifier,[],LabeledExamples) -> LabeledExamples;
learn(Classifier,[A|Ha],LabeledExamples) ->
    {ok,Label}=mllib:classify(Classifier, A, [ ]),
    learn(Classifier ,Ha,[Label|LabeledExamples]).

testBayes(Filename) ->
   {ok, Classifier} = getClassifier(Filename,naive_bayes),
   Odd = {1.0,2.0,3.0},
   Even= {2.0,1.0,0.0},
   showExample(Classifier,Odd),
   showExample(Classifier,Even).