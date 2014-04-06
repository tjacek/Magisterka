%%%-------------------------------------------------------------------
%%% @author tjacek
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. kwi 2014 13:31
%%%-------------------------------------------------------------------
-module(kernel_smoother).
-author("tjacek").

%% API
-export([test/0,test_regression/1]).

kernel_regression(X0,Samples) ->
  {Labels,Instances}=parse_labels(Samples),
  Y=utils:labels2reals(Labels),
  X=get_weights(X0,Instances),
  Norm=lists:sum(X),
  R=utils:dot_product(X,Y)/Norm,
  io:format("~p~n",[R]).

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

get_weights(X0,Instances) ->
  Exp_kernel=get_kernel(1.0),
  K=fun(X) ->
    Exp_kernel(X0,X)
  end,
  lists:map(K,Instances).

get_kernel(B) ->
  fun(X,Y) ->
    exp_kernel(X,Y,B)
  end.

exp_kernel(X,Y,B) ->
  D=distance(X,Y),
  Exponent= (-1.0*D*D) / (2.0* B),
  math:exp(Exponent).

distance(X,Y) ->  distance(X,Y,0.0).
distance([],[],Acc) ->  math:sqrt(Acc);
distance([A|Ha],[B|Hb],Acc) ->
  Det=A-B,
  distance(Ha,Hb,Acc + Det*Det).

test() ->
  H=[1.0,1.0],
  T=[3.0,2.0],
  MyKernel=get_kernel(2.0),
  Res=MyKernel(H,T),
  io:format("~p~n",[Res]).

test_regression(Filename) ->
  {Attributes, TrainingExamples} = mllib:read(arff,[{file,Filename}]),
  X0=[1.0,1.0,1.0],
  kernel_regression(X0,TrainingExamples).