-module(apriori).

%%
%% Include files
%%

-include("structures.hrl").
-include("debug.hrl").

%%
%% Exported Functions
%%

-export([mine/2,extCall/1, test/4, testseq/3, experiment/4]).
%% paralel
-export([apriori_worker/1, seq_apriori_worker/1]).
%%
%% API Functions
%%

-spec mine(Data :: [ [any()] ], Options :: []) -> [{Antecedent :: [any()],Consequent :: [any()],SupAntecedent::number(),SupNominator::number(),Confidence::number()}] | [{FrequentSubsequence :: [any()], Support :: number()}].
% @spec mine(Data, Options) -> [{Antecedent,Consequent,SupAntecedent,SupNominator,Confidence}] | [{FrequentSubsequence, Support}] where Data = [ [any()] ], Options = [], Antecedent = [any()],Consequent = [any()],SupAntecedent = number(),SupNominator=number(),Confidence=number(), FrequentSubsequence = [any()], Support = number()
% @doc Performs the data mining in the data. Generates associative rules describing the data or frequent sequences. Possible options are: * {min_sup, MinSup} - MinimalSupport, * {workers, Workers} - number of working threads, * {min_conf, MinConf} - MinimalConfidence, * seq - search for frequent sequences
mine(Data, Options) ->
  MinSup = case lists:keyfind(min_sup, 1, Options) of
             false -> 0; % default
             {min_sup, MinSupVal} -> MinSupVal * length(Data)
           end,
  Workers = case lists:keyfind(workers, 1, Options) of
              false -> 1; % default
              {workers, WorkersNo} -> WorkersNo
            end,
  MinConf = case lists:keyfind(min_conf, 1, Options) of
              false -> 0; % default
              {min_conf, MinConfVal} -> MinConfVal
            end,

%	Dict = get_dict(Data),
  case lists:member(seq, Options) of
    true ->	sequences_apriori(Data, Workers, MinSup);
    false -> freq_apriori(Data, Workers, MinSup, MinConf)
  end.

sequences_apriori(Data, Workers, MinSup) ->
  SplitData = split_seq_data(Data, Workers),
  WorkerIds = [ supervisor_manager:compute(seq_apriori_worker, [SData]) || SData <- SplitData ],
  DataL = [[D] || D <- lists:usort(lists:flatten(Data))],
  ?LOG("DataL~p\n", [DataL]),
  seq_apriori_first(DataL, WorkerIds, MinSup, dict:new()).

freq_apriori(Data, Workers, MinSup, MinConf) ->
  ?LOG("before split\n", []),
  SplitData = split_data(Data, Workers),
  %WorkerIds = [ spawn(node(), apriori, apriori_worker, [SData]) || SData <- SplitData ],
  ?LOG("before spawn\n", []),
  WorkerIds = [ supervisor_manager:compute(apriori_worker, [SData]) || SData <- SplitData ],
  ?LOG("before usort\n", []),
  DataL = [[D] || D <- lists:usort(lists:flatten(Data))],
  ?LOG("DataL~p\n", [DataL]),
  apriori_first(DataL, WorkerIds, MinSup, MinConf, dict:new()).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% @doc Worker thread for finding associative rules
apriori_worker(Data) ->
  io:format("I'm alive ~p\n", [self()]),

  receive
    {_, stop} ->
      io:format("Stop ~p\n", [self()]),
      ok;
    {Pid, {Dict, Id}} ->
      CDict = frequent_items(Dict, Data),
      Pid ! {Id, CDict},
      apriori_worker(Data)
  end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% @doc Working thread for frequent sequences checking
seq_apriori_worker(Data) ->
  receive
    {_, stop} ->
      io:format("Stop ~p\n", [self()]),
      ok;
    {Pid, {Dict, Id}} ->
      CDict = frequent_seqs(Dict, Data),
      Pid ! {Id, CDict},
      seq_apriori_worker(Data)
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% @doc Result : {Antecedent,Consequent,support(Antecedent),support(Antecedent u Consequent),Confidence}
apriori_first(Data, Workers, MinSup, MinConf, Result) ->
  Dict = get_dict(Data),
  ?LOG("God : Dict~p\n", [dict:to_list(Dict)]),
  % count and prune
  DictLk = let_workers_count(Workers, Dict),
  PrunedLk = prune(DictLk, MinSup),
  ?LOG("PrunedLk ~p\n", [dict:to_list(PrunedLk)]),

  % make new
  Initial = dict:fetch_keys(PrunedLk),
  NewSets = make_sets(PrunedLk, Initial, 2),
  ?LOG("NewSets~p\n", [NewSets]),
  apriori_steps(2, Initial, NewSets, Workers, MinSup, MinConf, dict:merge(fun (K, V1, V2) -> ?LOG("dicte merge error ~p, ~p, ~p \n",[K, V1, V2]) end, PrunedLk, Result)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
apriori_steps(_Step, _Initial, [], Workers, _MinSup, MinConf, ResultDict) ->
  stop_workers(Workers),
  Itemsets = dict:to_list(ResultDict),
  Rules = generate_rules(Itemsets, []),
  prune_rules(Rules, ResultDict, MinConf, []);
apriori_steps(Step, Initial, Data, Workers, MinSup, MinConf, Result) ->
  Dict = get_dict(Data),
  ?LOG("God : Dict~p\n", [dict:to_list(Dict)]),
  % count and prune
  DictLk = let_workers_count(Workers, Dict),
  PrunedLk = prune(DictLk, MinSup),
  ?LOG("PrunedLk ~p\n", [dict:to_list(PrunedLk)]),

  % make new
  NewSets = make_sets(PrunedLk, Initial, Step+1),
  ?LOG("NewSets~p\n", [NewSets]),
  apriori_steps(Step+1,Initial, NewSets, Workers, MinSup, MinConf, dict:merge(fun (K, V1, V2) -> ?LOG("dict merge error ~p, ~p, ~p \n",[K, V1, V2]) end, PrunedLk, Result)).
% if empty -> finish
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
seq_apriori_first(Data, Workers, MinSup, Result) ->
  Dict = get_dict(Data),
  ?DETAIL("God : Dict~p\n", [dict:to_list(Dict)]),
  % count and prune
  DictLk = let_workers_count(Workers, Dict),
  PrunedLk = prune(DictLk, MinSup),
  ?DETAIL("PrunedLk ~p\n", [dict:to_list(PrunedLk)]),

  % make new
  Initial = dict:fetch_keys(PrunedLk),
  NewSets = seq_make_sets(PrunedLk, Initial),
  ?DETAIL("NewSets~p\n", [NewSets]),
  seq_apriori_steps(Initial, NewSets, Workers, MinSup, dict:merge(fun (K, V1, V2) -> ?LOG("dict merge error~p, ~p, ~p \n",[K, V1, V2]) end, PrunedLk, Result)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
seq_apriori_steps(_Initial, [], Workers, _MinSup, Result) ->
  stop_workers(Workers),
  lists:sort(fun({_,SupportA}, {_,SupportB}) -> SupportA >= SupportB end, dict:to_list(Result));
seq_apriori_steps(Initial, Data, Workers, MinSup, Result) ->
  Dict = get_dict(Data),
  ?DETAIL("God : Dict~p\n", [dict:to_list(Dict)]),
  % count and prune
  DictLk = let_workers_count(Workers, Dict),
  PrunedLk = prune(DictLk, MinSup),
  ?DETAIL("PrunedLk ~p\n", [dict:to_list(PrunedLk)]),

  % make new
  NewSets = seq_make_sets(PrunedLk, Initial),
  ?DETAIL("NewSets~p\n", [NewSets]),
  seq_apriori_steps(Initial, NewSets, Workers, MinSup, dict:merge(fun (K, V1, V2) -> ?DETAIL("dict merge error~p, ~p, ~p \n",[K, V1, V2]) end, PrunedLk, Result)).
% if empty -> finish


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
let_workers_count(Workers, Dict) ->
  send_dict(Workers, Dict),
  receive_and_merge(Workers, Dict).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
send_dict([], _Dict) ->
  ok;
send_dict([Worker|Workers], Dict) ->
  case supervisor_manager:message(Worker, {Dict, Worker}) of
    ok -> send_dict(Workers, Dict);
    {error, Reason} -> {error, Reason}
  %Worker ! {self(), Dict},
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
receive_and_merge([], Result) ->
  Result;
receive_and_merge([Worker|Workers], Result) ->
  receive {Worker, Dict} ->
    NewResult = dict:merge(fun (_K, V1, V2) -> V1 + V2 end, Result, Dict),
    receive_and_merge(Workers, NewResult)
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
stop_workers([]) ->
  ok;
stop_workers([Worker|Workers]) ->
  %Worker ! stop,
  case supervisor_manager:message(Worker, stop) of
    ok -> stop_workers(Workers);
    {error, Reason} -> {error, Reason}
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_dict(Data) ->
  Dict = dict:new(),
  UniqueData = gb_sets:to_list(gb_sets:from_list(Data)), % Professor Arne Andersson's General Balanced Trees, works well with a lot of data
  ?DETAIL("UniqueData: ~p\n", [UniqueData]),
  initial_counters(Dict, UniqueData).% returns updated Dict

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
initial_counters(Dict, []) ->
  Dict;
initial_counters(Dict, [Elem|Data]) ->
  Dict1 = dict:store(Elem, 0, Dict),
  initial_counters(Dict1, Data).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
frequent_items(Dict, []) ->
  Dict;
frequent_items(Dict, [Itemset|Data]) ->
  Dict1 = update_counter(Dict, dict:fetch_keys(Dict), Itemset),
  frequent_items(Dict1, Data).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
update_counter(Dict, _Keys = [], _Itemset) ->
  Dict;
update_counter(Dict, [Key|Keys], Itemset) ->
  case is_subset(Key, Itemset) of
    true -> update_counter(dict:update_counter(Key, 1, Dict), Keys, Itemset);
    false -> update_counter(Dict, Keys, Itemset)
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
frequent_seqs(Dict, []) ->
  Dict;
frequent_seqs(Dict, [Itemset|Data]) ->
  Dict1 = seq_update_counter(Dict, dict:fetch_keys(Dict), Itemset),
  frequent_seqs(Dict1, Data).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
seq_update_counter(Dict, _Keys = [], _Itemset) ->
  Dict;
seq_update_counter(Dict, [Key|Keys], Itemset) ->
  case is_subsequence(Key, Itemset) of
    true -> seq_update_counter(dict:update_counter(Key, 1, Dict), Keys, Itemset);
    false -> seq_update_counter(Dict, Keys, Itemset)
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
is_subset([],_Set) ->
  true;
is_subset(_Subset, []) ->
  false;
is_subset([Subelem|_Subset], [Elem|_Set]) when Elem > Subelem ->
  false;
is_subset([Subelem|Subset], [Elem|Set]) when Elem < Subelem ->
  is_subset([Subelem|Subset], Set);
is_subset([Subelem|Subset], [Subelem|Set]) ->
  is_subset(Subset, Set).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
is_subsequence([],_Set) ->
  true;
is_subsequence(_Subseq, []) ->
  false;
is_subsequence([Subelem|Subseq], [Subelem|Seq]) ->
  is_subsequence(Subseq, Seq);
is_subsequence([Subelem|Subseq], [_Elem|Seq]) ->
  is_subsequence([Subelem|Subseq], Seq).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
prune(Dict, MinSup) ->
  dict:filter(fun(_ItemSet, Count) -> Count >= MinSup end, Dict).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
make_sets(Dict, Initial, Size) ->
  case dict:fetch_keys(Dict) of
    [] -> [];
    Keys -> combinations(Keys, Initial, Size, [])
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
combinations([_Elem], _Tails, _Size, Acu) ->
  lists:usort(Acu);
combinations([Elem|Elems], Tails, Size, Acu) ->
  Products = [ lists:usort(Elem++E) || E <- Tails],
  Valid = lists:filter(fun(L) -> length(L) == Size end, Products),
  combinations(Elems, Tails, Size, Valid++Acu).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
seq_make_sets(Dict, Initial) ->
  case dict:fetch_keys(Dict) of
    [] -> [];
    Keys -> repeat_combinations(Keys, Initial, [])
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
repeat_combinations([_Elem], _Initial, Acu) ->
  Acu;
repeat_combinations([Elem|Elems], Tails, Acu) ->
  Products = [ Elem++E || E <- Tails],
  repeat_combinations(Elems, Tails, Products++Acu).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
split_data(Data, Workers) ->
  Dict = dict:new(),
  split_workers_acu(Data, Workers, 1, Dict).
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
split_workers_acu([], _Workers, _N, Dict) ->
  [ List || {_No, List} <- dict:to_list(Dict)];
split_workers_acu([Elem|Data], Workers, Workers, Dict) ->
  split_workers_acu(Data, Workers, 1, append_to_dict(Workers, lists:sort(Elem), Dict));
split_workers_acu([Elem|Data], Workers, N, Dict) ->
  split_workers_acu(Data, Workers, N+1, append_to_dict(N, lists:sort(Elem), Dict)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
split_seq_data(Data, Workers) ->
  Dict = dict:new(),
  split_seq_workers_acu(Data, Workers, 1, Dict).
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
split_seq_workers_acu([], _Workers, _N, Dict) ->
  [ List || {_No, List} <- dict:to_list(Dict)];
split_seq_workers_acu([Elem|Data], Workers, Workers, Dict) ->
  split_seq_workers_acu(Data, Workers, 1, append_to_dict(Workers, Elem, Dict));
split_seq_workers_acu([Elem|Data], Workers, N, Dict) ->
  split_seq_workers_acu(Data, Workers, N+1, append_to_dict(N, Elem, Dict)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
append_to_dict(Key, Value, Dict) ->
  dict:update(Key, fun (Old) -> [Value|Old] end, [Value], Dict).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
generate_rules([], Acc) ->
  Acc;
generate_rules([{Set, _Support}|Sets], Acc) ->
  generate_rules(Sets, subset(Set, [], [], Set, []) ++ Acc).
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
subset([], [] = _Left, _Current, _FullSet, Acc) ->
  Acc;
subset([], _Left, [] = _Current, _FullSet, Acc) ->
  Acc;
subset([], Left, Current, FullSet, Acc) ->
  [{lists:reverse(Left), lists:reverse(Current), FullSet}|Acc];
subset([Item|Set], Left, Current, FullSet, Acc) ->
  % occlusion of item
  Acc1 = subset(Set, [Item|Left], Current, FullSet, Acc),
  % join to current
  subset(Set, Left, [Item|Current], FullSet, Acc1).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
prune_rules([] = _Rules, _Dict, _MinConfidence, Acc) ->
  lists:sort(fun({_,_,_,_,ConfA}, {_,_,_,_,ConfB}) -> ConfA >= ConfB end, Acc) ;
prune_rules([{Antecedent, Consequent, Nominator}|Rules], Dict, MinConfidence, Acc) ->
  SupAntecedent = dict:fetch(Antecedent, Dict),
  SupNominator = dict:fetch(Nominator, Dict),
  Confidence = SupNominator/SupAntecedent,
  if Confidence >= MinConfidence ->
    prune_rules(Rules, Dict, MinConfidence, [{Antecedent,Consequent,SupAntecedent,SupNominator,Confidence}|Acc]);
    true -> prune_rules(Rules, Dict, MinConfidence, Acc)
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
extCall(Args) ->
  MinSup= list_to_float(lists:nth(1,Args)), 
  MinConf=list_to_float(lists:nth(2,Args)),
  Workers=list_to_integer(lists:nth(3,Args)), 
  DatasetName=lists:nth(4,Args),
  apriori:experiment(MinSup, MinConf, Workers,DatasetName). 

experiment(MinSup, MinConf, Workers, DatasetName) ->
  {ok, Data} = mllib:read_mine_data(DatasetName),
  Nodes= [node()],
  Options = [{min_sup, MinSup}, {workers, Workers}, {min_conf, MinConf}, Nodes],
  %Result=apriori:mine(Data, Options),
  Result = mllib:mine(Data, apriori, Options,Nodes),
  ?LOG("Result: ~w\n", [Result]),
  Result.

test(MinSup, MinConf, Workers, Nodes) ->
  %Data = mllib:read_mine_data("mine_data"),
  {ok, Data} = mllib:read_mine_data("amazon_access.csv"),
  Options = [{min_sup, MinSup}, {workers, Workers}, {min_conf, MinConf}, Nodes],
  Result = mllib:mine(Data, apriori, Options, Nodes).

testseq(MinSup, Workers, Nodes) ->
  %Data = mllib:read_mine_data("mine_data"),
  {ok, Data} = mllib:read_mine_data("rest.txt"),
  Options = [{min_sup, MinSup}, {workers, Workers}, seq],
  Result = mllib:mine(Data, apriori, Options, Nodes),
  ?LOG("Result: ~w\n", [Result]),
  Result.

