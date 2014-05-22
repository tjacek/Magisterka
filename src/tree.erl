%%%-------------------------------------------------------------------
%%% @author tjacek
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. maj 2014 18:13
%%%-------------------------------------------------------------------
-module(tree).
-author("tjacek").

%% API
-export([is_root/1,is_decision_node/1,is_leaf/1]).
-export([node_name/1,node_value/1,left_child/1,right_child/1,is_root/1,is_leaf/1]).

is_root(Tree) ->
  Node_name=node_name(Tree),
  Node_name==root.

is_decision_node(Tree) ->
  (not is_root(Tree)) and (not is_leaf(Tree)).

is_leaf(Tree) ->
  Node_name=node_name(Tree),
  Node_name==leaf.

node_name(Tree) ->
  element(1,Tree).

node_value(Tree) ->
  element(2,Tree).

left_child(Tree) ->
  Child_list=element(3,Tree),
  lists:nth(1,Child_list).

right_child(Tree) ->
  Child_list=element(3,Tree),
  lists:nth(2,Child_list).