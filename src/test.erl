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
-export([createClassifer/3,testClassifer/3 ]).

getClassifier(Filename,Alg) ->
  {Attributes, TrainingExamples} = mllib:read(arff,[{file,"train/linear.arff"}]),
  ClassName = cat,
  {ok, Classifier} = mllib:simple_learn(Attributes, ClassName, TrainingExamples, Alg, [ ]).

createClassifer(Alg,Training,Name) ->
  {ok, Classifier}=getClassifier(Training,Alg).
%  mllib:write_classifier(Name,Classifier).

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