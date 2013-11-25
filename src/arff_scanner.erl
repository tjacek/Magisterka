-file("/usr/lib/erlang/lib/parsetools-2.0.6/include/leexinc.hrl", 0).
%% The source of this file is part of leex distribution, as such it
%% has the same Copyright as the other files in the leex
%% distribution. The Copyright is defined in the accompanying file
%% COPYRIGHT. However, the resultant scanner generated by leex is the
%% property of the creator of the scanner and is not covered by that
%% Copyright.

-module(arff_scanner).

-export([string/1,string/2,token/2,token/3,tokens/2,tokens/3]).
-export([format_error/1]).

%% User code. This is placed here to allow extra attributes.
-file("./arff_scanner.xrl", 21).

-file("/usr/lib/erlang/lib/parsetools-2.0.6/include/leexinc.hrl", 14).

format_error({illegal,S}) -> ["illegal characters ",io_lib:write_string(S)];
format_error({user,S}) -> S.

string(String) -> string(String, 1).

string(String, Line) -> string(String, Line, String, []).

%% string(InChars, Line, TokenChars, Tokens) ->
%% {ok,Tokens,Line} | {error,ErrorInfo,Line}.
%% Note the line number going into yystate, L0, is line of token
%% start while line number returned is line of token end. We want line
%% of token start.

string([], L, [], Ts) ->                     % No partial tokens!
  {ok,yyrev(Ts),L};
string(Ics0, L0, Tcs, Ts) ->
  case yystate(yystate(), Ics0, L0, 0, reject, 0) of
    {A,Alen,Ics1,L1} ->                  % Accepting end state
      string_cont(Ics1, L1, yyaction(A, Alen, Tcs, L0), Ts);
    {A,Alen,Ics1,L1,_S1} ->              % Accepting transistion state
      string_cont(Ics1, L1, yyaction(A, Alen, Tcs, L0), Ts);
    {reject,_Alen,Tlen,_Ics1,L1,_S1} ->  % After a non-accepting state
      {error,{L0,?MODULE,{illegal,yypre(Tcs, Tlen+1)}},L1};
    {A,Alen,_Tlen,_Ics1,L1,_S1} ->
      string_cont(yysuf(Tcs, Alen), L1, yyaction(A, Alen, Tcs, L0), Ts)
  end.

%% string_cont(RestChars, Line, Token, Tokens)
%% Test for and remove the end token wrapper. Push back characters
%% are prepended to RestChars.

string_cont(Rest, Line, {token,T}, Ts) ->
  string(Rest, Line, Rest, [T|Ts]);
string_cont(Rest, Line, {token,T,Push}, Ts) ->
  NewRest = Push ++ Rest,
  string(NewRest, Line, NewRest, [T|Ts]);
string_cont(Rest, Line, {end_token,T}, Ts) ->
  string(Rest, Line, Rest, [T|Ts]);
string_cont(Rest, Line, {end_token,T,Push}, Ts) ->
  NewRest = Push ++ Rest,
  string(NewRest, Line, NewRest, [T|Ts]);
string_cont(Rest, Line, skip_token, Ts) ->
  string(Rest, Line, Rest, Ts);
string_cont(Rest, Line, {skip_token,Push}, Ts) ->
  NewRest = Push ++ Rest,
  string(NewRest, Line, NewRest, Ts);
string_cont(_Rest, Line, {error,S}, _Ts) ->
  {error,{Line,?MODULE,{user,S}},Line}.

%% token(Continuation, Chars) ->
%% token(Continuation, Chars, Line) ->
%% {more,Continuation} | {done,ReturnVal,RestChars}.
%% Must be careful when re-entering to append the latest characters to the
%% after characters in an accept. The continuation is:
%% {token,State,CurrLine,TokenChars,TokenLen,TokenLine,AccAction,AccLen}

token(Cont, Chars) -> token(Cont, Chars, 1).

token([], Chars, Line) ->
  token(yystate(), Chars, Line, Chars, 0, Line, reject, 0);
token({token,State,Line,Tcs,Tlen,Tline,Action,Alen}, Chars, _) ->
  token(State, Chars, Line, Tcs ++ Chars, Tlen, Tline, Action, Alen).

%% token(State, InChars, Line, TokenChars, TokenLen, TokenLine,
%% AcceptAction, AcceptLen) ->
%% {more,Continuation} | {done,ReturnVal,RestChars}.
%% The argument order is chosen to be more efficient.

token(S0, Ics0, L0, Tcs, Tlen0, Tline, A0, Alen0) ->
  case yystate(S0, Ics0, L0, Tlen0, A0, Alen0) of
  %% Accepting end state, we have a token.
    {A1,Alen1,Ics1,L1} ->
      token_cont(Ics1, L1, yyaction(A1, Alen1, Tcs, Tline));
  %% Accepting transition state, can take more chars.
    {A1,Alen1,[],L1,S1} ->                  % Need more chars to check
      {more,{token,S1,L1,Tcs,Alen1,Tline,A1,Alen1}};
    {A1,Alen1,Ics1,L1,_S1} ->               % Take what we got
      token_cont(Ics1, L1, yyaction(A1, Alen1, Tcs, Tline));
  %% After a non-accepting state, maybe reach accept state later.
    {A1,Alen1,Tlen1,[],L1,S1} ->            % Need more chars to check
      {more,{token,S1,L1,Tcs,Tlen1,Tline,A1,Alen1}};
    {reject,_Alen1,Tlen1,eof,L1,_S1} ->     % No token match
      %% Check for partial token which is error.
      Ret = if Tlen1 > 0 -> {error,{Tline,?MODULE,
        %% Skip eof tail in Tcs.
        {illegal,yypre(Tcs, Tlen1)}},L1};
              true -> {eof,L1}
            end,
      {done,Ret,eof};
    {reject,_Alen1,Tlen1,Ics1,L1,_S1} ->    % No token match
      Error = {Tline,?MODULE,{illegal,yypre(Tcs, Tlen1+1)}},
      {done,{error,Error,L1},Ics1};
    {A1,Alen1,_Tlen1,_Ics1,L1,_S1} ->       % Use last accept match
      token_cont(yysuf(Tcs, Alen1), L1, yyaction(A1, Alen1, Tcs, Tline))
  end.

%% token_cont(RestChars, Line, Token)
%% If we have a token or error then return done, else if we have a
%% skip_token then continue.

token_cont(Rest, Line, {token,T}) ->
  {done,{ok,T,Line},Rest};
token_cont(Rest, Line, {token,T,Push}) ->
  NewRest = Push ++ Rest,
  {done,{ok,T,Line},NewRest};
token_cont(Rest, Line, {end_token,T}) ->
  {done,{ok,T,Line},Rest};
token_cont(Rest, Line, {end_token,T,Push}) ->
  NewRest = Push ++ Rest,
  {done,{ok,T,Line},NewRest};
token_cont(Rest, Line, skip_token) ->
  token(yystate(), Rest, Line, Rest, 0, Line, reject, 0);
token_cont(Rest, Line, {skip_token,Push}) ->
  NewRest = Push ++ Rest,
  token(yystate(), NewRest, Line, NewRest, 0, Line, reject, 0);
token_cont(Rest, Line, {error,S}) ->
  {done,{error,{Line,?MODULE,{user,S}},Line},Rest}.

%% tokens(Continuation, Chars, Line) ->
%% {more,Continuation} | {done,ReturnVal,RestChars}.
%% Must be careful when re-entering to append the latest characters to the
%% after characters in an accept. The continuation is:
%% {tokens,State,CurrLine,TokenChars,TokenLen,TokenLine,Tokens,AccAction,AccLen}
%% {skip_tokens,State,CurrLine,TokenChars,TokenLen,TokenLine,Error,AccAction,AccLen}

tokens(Cont, Chars) -> tokens(Cont, Chars, 1).

tokens([], Chars, Line) ->
  tokens(yystate(), Chars, Line, Chars, 0, Line, [], reject, 0);
tokens({tokens,State,Line,Tcs,Tlen,Tline,Ts,Action,Alen}, Chars, _) ->
  tokens(State, Chars, Line, Tcs ++ Chars, Tlen, Tline, Ts, Action, Alen);
tokens({skip_tokens,State,Line,Tcs,Tlen,Tline,Error,Action,Alen}, Chars, _) ->
  skip_tokens(State, Chars, Line, Tcs ++ Chars, Tlen, Tline, Error, Action, Alen).

%% tokens(State, InChars, Line, TokenChars, TokenLen, TokenLine, Tokens,
%% AcceptAction, AcceptLen) ->
%% {more,Continuation} | {done,ReturnVal,RestChars}.

tokens(S0, Ics0, L0, Tcs, Tlen0, Tline, Ts, A0, Alen0) ->
  case yystate(S0, Ics0, L0, Tlen0, A0, Alen0) of
  %% Accepting end state, we have a token.
    {A1,Alen1,Ics1,L1} ->
      tokens_cont(Ics1, L1, yyaction(A1, Alen1, Tcs, Tline), Ts);
  %% Accepting transition state, can take more chars.
    {A1,Alen1,[],L1,S1} ->                  % Need more chars to check
      {more,{tokens,S1,L1,Tcs,Alen1,Tline,Ts,A1,Alen1}};
    {A1,Alen1,Ics1,L1,_S1} ->               % Take what we got
      tokens_cont(Ics1, L1, yyaction(A1, Alen1, Tcs, Tline), Ts);
  %% After a non-accepting state, maybe reach accept state later.
    {A1,Alen1,Tlen1,[],L1,S1} ->            % Need more chars to check
      {more,{tokens,S1,L1,Tcs,Tlen1,Tline,Ts,A1,Alen1}};
    {reject,_Alen1,Tlen1,eof,L1,_S1} ->     % No token match
      %% Check for partial token which is error, no need to skip here.
      Ret = if Tlen1 > 0 -> {error,{Tline,?MODULE,
        %% Skip eof tail in Tcs.
        {illegal,yypre(Tcs, Tlen1)}},L1};
              Ts == [] -> {eof,L1};
              true -> {ok,yyrev(Ts),L1}
            end,
      {done,Ret,eof};
    {reject,_Alen1,Tlen1,_Ics1,L1,_S1} ->
      %% Skip rest of tokens.
      Error = {L1,?MODULE,{illegal,yypre(Tcs, Tlen1+1)}},
      skip_tokens(yysuf(Tcs, Tlen1+1), L1, Error);
    {A1,Alen1,_Tlen1,_Ics1,L1,_S1} ->
      Token = yyaction(A1, Alen1, Tcs, Tline),
      tokens_cont(yysuf(Tcs, Alen1), L1, Token, Ts)
  end.

%% tokens_cont(RestChars, Line, Token, Tokens)
%% If we have an end_token or error then return done, else if we have
%% a token then save it and continue, else if we have a skip_token
%% just continue.

tokens_cont(Rest, Line, {token,T}, Ts) ->
  tokens(yystate(), Rest, Line, Rest, 0, Line, [T|Ts], reject, 0);
tokens_cont(Rest, Line, {token,T,Push}, Ts) ->
  NewRest = Push ++ Rest,
  tokens(yystate(), NewRest, Line, NewRest, 0, Line, [T|Ts], reject, 0);
tokens_cont(Rest, Line, {end_token,T}, Ts) ->
  {done,{ok,yyrev(Ts, [T]),Line},Rest};
tokens_cont(Rest, Line, {end_token,T,Push}, Ts) ->
  NewRest = Push ++ Rest,
  {done,{ok,yyrev(Ts, [T]),Line},NewRest};
tokens_cont(Rest, Line, skip_token, Ts) ->
  tokens(yystate(), Rest, Line, Rest, 0, Line, Ts, reject, 0);
tokens_cont(Rest, Line, {skip_token,Push}, Ts) ->
  NewRest = Push ++ Rest,
  tokens(yystate(), NewRest, Line, NewRest, 0, Line, Ts, reject, 0);
tokens_cont(Rest, Line, {error,S}, _Ts) ->
  skip_tokens(Rest, Line, {Line,?MODULE,{user,S}}).

%%skip_tokens(InChars, Line, Error) -> {done,{error,Error,Line},Ics}.
%% Skip tokens until an end token, junk everything and return the error.

skip_tokens(Ics, Line, Error) ->
  skip_tokens(yystate(), Ics, Line, Ics, 0, Line, Error, reject, 0).

%% skip_tokens(State, InChars, Line, TokenChars, TokenLen, TokenLine, Tokens,
%% AcceptAction, AcceptLen) ->
%% {more,Continuation} | {done,ReturnVal,RestChars}.

skip_tokens(S0, Ics0, L0, Tcs, Tlen0, Tline, Error, A0, Alen0) ->
  case yystate(S0, Ics0, L0, Tlen0, A0, Alen0) of
    {A1,Alen1,Ics1,L1} ->                  % Accepting end state
      skip_cont(Ics1, L1, yyaction(A1, Alen1, Tcs, Tline), Error);
    {A1,Alen1,[],L1,S1} ->                 % After an accepting state
      {more,{skip_tokens,S1,L1,Tcs,Alen1,Tline,Error,A1,Alen1}};
    {A1,Alen1,Ics1,L1,_S1} ->
      skip_cont(Ics1, L1, yyaction(A1, Alen1, Tcs, Tline), Error);
    {A1,Alen1,Tlen1,[],L1,S1} ->           % After a non-accepting state
      {more,{skip_tokens,S1,L1,Tcs,Tlen1,Tline,Error,A1,Alen1}};
    {reject,_Alen1,_Tlen1,eof,L1,_S1} ->
      {done,{error,Error,L1},eof};
    {reject,_Alen1,Tlen1,_Ics1,L1,_S1} ->
      skip_tokens(yysuf(Tcs, Tlen1+1), L1, Error);
    {A1,Alen1,_Tlen1,_Ics1,L1,_S1} ->
      Token = yyaction(A1, Alen1, Tcs, Tline),
      skip_cont(yysuf(Tcs, Alen1), L1, Token, Error)
  end.

%% skip_cont(RestChars, Line, Token, Error)
%% Skip tokens until we have an end_token or error then return done
%% with the original rror.

skip_cont(Rest, Line, {token,_T}, Error) ->
  skip_tokens(yystate(), Rest, Line, Rest, 0, Line, Error, reject, 0);
skip_cont(Rest, Line, {token,_T,Push}, Error) ->
  NewRest = Push ++ Rest,
  skip_tokens(yystate(), NewRest, Line, NewRest, 0, Line, Error, reject, 0);
skip_cont(Rest, Line, {end_token,_T}, Error) ->
  {done,{error,Error,Line},Rest};
skip_cont(Rest, Line, {end_token,_T,Push}, Error) ->
  NewRest = Push ++ Rest,
  {done,{error,Error,Line},NewRest};
skip_cont(Rest, Line, skip_token, Error) ->
  skip_tokens(yystate(), Rest, Line, Rest, 0, Line, Error, reject, 0);
skip_cont(Rest, Line, {skip_token,Push}, Error) ->
  NewRest = Push ++ Rest,
  skip_tokens(yystate(), NewRest, Line, NewRest, 0, Line, Error, reject, 0);
skip_cont(Rest, Line, {error,_S}, Error) ->
  skip_tokens(yystate(), Rest, Line, Rest, 0, Line, Error, reject, 0).

yyrev(List) -> lists:reverse(List).
yyrev(List, Tail) -> lists:reverse(List, Tail).
yypre(List, N) -> lists:sublist(List, N).
yysuf(List, N) -> lists:nthtail(N, List).

%% yystate() -> InitialState.
%% yystate(State, InChars, Line, CurrTokLen, AcceptAction, AcceptLen) ->
%% {Action, AcceptLen, RestChars, Line} |
%% {Action, AcceptLen, RestChars, Line, State} |
%% {reject, AcceptLen, CurrTokLen, RestChars, Line, State} |
%% {Action, AcceptLen, CurrTokLen, RestChars, Line, State}.
%% Generated state transition functions. The non-accepting end state
%% return signal either an unrecognised character or end of current
%% input.

-file("./arff_scanner.erl", 276).
yystate() -> 30.

yystate(33, Ics, Line, Tlen, _, _) ->
  {1,Tlen,Ics,Line};
yystate(32, [10|Ics], Line, Tlen, _, _) ->
  yystate(32, Ics, Line+1, Tlen+1, 9, Tlen);
yystate(32, [C|Ics], Line, Tlen, _, _) when C >= 0, C =< 9 ->
  yystate(32, Ics, Line, Tlen+1, 9, Tlen);
yystate(32, [C|Ics], Line, Tlen, _, _) when C >= 11, C =< 32 ->
  yystate(32, Ics, Line, Tlen+1, 9, Tlen);
yystate(32, Ics, Line, Tlen, _, _) ->
  {9,Tlen,Ics,Line,32};
yystate(31, [101|Ics], Line, Tlen, Action, Alen) ->
  yystate(33, Ics, Line, Tlen+1, Action, Alen);
yystate(31, [69|Ics], Line, Tlen, Action, Alen) ->
  yystate(33, Ics, Line, Tlen+1, Action, Alen);
yystate(31, Ics, Line, Tlen, Action, Alen) ->
  {Action,Alen,Tlen,Ics,Line,31};
yystate(30, [125|Ics], Line, Tlen, Action, Alen) ->
  yystate(26, Ics, Line, Tlen+1, Action, Alen);
yystate(30, [123|Ics], Line, Tlen, Action, Alen) ->
  yystate(22, Ics, Line, Tlen+1, Action, Alen);
yystate(30, [95|Ics], Line, Tlen, Action, Alen) ->
  yystate(18, Ics, Line, Tlen+1, Action, Alen);
yystate(30, [64|Ics], Line, Tlen, Action, Alen) ->
  yystate(14, Ics, Line, Tlen+1, Action, Alen);
yystate(30, [46|Ics], Line, Tlen, Action, Alen) ->
  yystate(18, Ics, Line, Tlen+1, Action, Alen);
yystate(30, [45|Ics], Line, Tlen, Action, Alen) ->
  yystate(18, Ics, Line, Tlen+1, Action, Alen);
yystate(30, [44|Ics], Line, Tlen, Action, Alen) ->
  yystate(4, Ics, Line, Tlen+1, Action, Alen);
yystate(30, [39|Ics], Line, Tlen, Action, Alen) ->
  yystate(8, Ics, Line, Tlen+1, Action, Alen);
yystate(30, [37|Ics], Line, Tlen, Action, Alen) ->
  yystate(28, Ics, Line, Tlen+1, Action, Alen);
yystate(30, [10|Ics], Line, Tlen, Action, Alen) ->
  yystate(32, Ics, Line+1, Tlen+1, Action, Alen);
yystate(30, [C|Ics], Line, Tlen, Action, Alen) when C >= 0, C =< 9 ->
  yystate(32, Ics, Line, Tlen+1, Action, Alen);
yystate(30, [C|Ics], Line, Tlen, Action, Alen) when C >= 11, C =< 32 ->
  yystate(32, Ics, Line, Tlen+1, Action, Alen);
yystate(30, [C|Ics], Line, Tlen, Action, Alen) when C >= 48, C =< 57 ->
  yystate(18, Ics, Line, Tlen+1, Action, Alen);
yystate(30, [C|Ics], Line, Tlen, Action, Alen) when C >= 65, C =< 90 ->
  yystate(18, Ics, Line, Tlen+1, Action, Alen);
yystate(30, [C|Ics], Line, Tlen, Action, Alen) when C >= 97, C =< 122 ->
  yystate(18, Ics, Line, Tlen+1, Action, Alen);
yystate(30, Ics, Line, Tlen, Action, Alen) ->
  {Action,Alen,Tlen,Ics,Line,30};
yystate(29, [46|Ics], Line, Tlen, _, _) ->
  yystate(29, Ics, Line, Tlen+1, 2, Tlen);
yystate(29, [10|Ics], Line, Tlen, _, _) ->
  yystate(29, Ics, Line+1, Tlen+1, 2, Tlen);
yystate(29, Ics, Line, Tlen, _, _) ->
  {2,Tlen,Ics,Line,29};
yystate(28, [C|Ics], Line, Tlen, _, _) when C >= 0, C =< 9 ->
  yystate(28, Ics, Line, Tlen+1, 10, Tlen);
yystate(28, [C|Ics], Line, Tlen, _, _) when C >= 11 ->
  yystate(28, Ics, Line, Tlen+1, 10, Tlen);
yystate(28, Ics, Line, Tlen, _, _) ->
  {10,Tlen,Ics,Line,28};
yystate(27, [116|Ics], Line, Tlen, Action, Alen) ->
  yystate(31, Ics, Line, Tlen+1, Action, Alen);
yystate(27, [84|Ics], Line, Tlen, Action, Alen) ->
  yystate(31, Ics, Line, Tlen+1, Action, Alen);
yystate(27, Ics, Line, Tlen, Action, Alen) ->
  {Action,Alen,Tlen,Ics,Line,27};
yystate(26, Ics, Line, Tlen, _, _) ->
  {7,Tlen,Ics,Line};
yystate(25, [97|Ics], Line, Tlen, Action, Alen) ->
  yystate(29, Ics, Line, Tlen+1, Action, Alen);
yystate(25, [65|Ics], Line, Tlen, Action, Alen) ->
  yystate(29, Ics, Line, Tlen+1, Action, Alen);
yystate(25, Ics, Line, Tlen, Action, Alen) ->
  {Action,Alen,Tlen,Ics,Line,25};
yystate(24, [95|Ics], Line, Tlen, Action, Alen) ->
  yystate(16, Ics, Line, Tlen+1, Action, Alen);
yystate(24, [46|Ics], Line, Tlen, Action, Alen) ->
  yystate(16, Ics, Line, Tlen+1, Action, Alen);
yystate(24, [45|Ics], Line, Tlen, Action, Alen) ->
  yystate(16, Ics, Line, Tlen+1, Action, Alen);
yystate(24, [10|Ics], Line, Tlen, Action, Alen) ->
  yystate(24, Ics, Line+1, Tlen+1, Action, Alen);
yystate(24, [C|Ics], Line, Tlen, Action, Alen) when C >= 0, C =< 9 ->
  yystate(24, Ics, Line, Tlen+1, Action, Alen);
yystate(24, [C|Ics], Line, Tlen, Action, Alen) when C >= 11, C =< 32 ->
  yystate(24, Ics, Line, Tlen+1, Action, Alen);
yystate(24, [C|Ics], Line, Tlen, Action, Alen) when C >= 48, C =< 57 ->
  yystate(16, Ics, Line, Tlen+1, Action, Alen);
yystate(24, [C|Ics], Line, Tlen, Action, Alen) when C >= 65, C =< 90 ->
  yystate(16, Ics, Line, Tlen+1, Action, Alen);
yystate(24, [C|Ics], Line, Tlen, Action, Alen) when C >= 97, C =< 122 ->
  yystate(16, Ics, Line, Tlen+1, Action, Alen);
yystate(24, Ics, Line, Tlen, Action, Alen) ->
  {Action,Alen,Tlen,Ics,Line,24};
yystate(23, [117|Ics], Line, Tlen, Action, Alen) ->
  yystate(27, Ics, Line, Tlen+1, Action, Alen);
yystate(23, [85|Ics], Line, Tlen, Action, Alen) ->
  yystate(27, Ics, Line, Tlen+1, Action, Alen);
yystate(23, Ics, Line, Tlen, Action, Alen) ->
  {Action,Alen,Tlen,Ics,Line,23};
yystate(22, Ics, Line, Tlen, _, _) ->
  {6,Tlen,Ics,Line};
yystate(21, [116|Ics], Line, Tlen, Action, Alen) ->
  yystate(25, Ics, Line, Tlen+1, Action, Alen);
yystate(21, [84|Ics], Line, Tlen, Action, Alen) ->
  yystate(25, Ics, Line, Tlen+1, Action, Alen);
yystate(21, Ics, Line, Tlen, Action, Alen) ->
  {Action,Alen,Tlen,Ics,Line,21};
yystate(20, Ics, Line, Tlen, _, _) ->
  {5,Tlen,Ics,Line};
yystate(19, [98|Ics], Line, Tlen, Action, Alen) ->
  yystate(23, Ics, Line, Tlen+1, Action, Alen);
yystate(19, [66|Ics], Line, Tlen, Action, Alen) ->
  yystate(23, Ics, Line, Tlen+1, Action, Alen);
yystate(19, Ics, Line, Tlen, Action, Alen) ->
  {Action,Alen,Tlen,Ics,Line,19};
yystate(18, [95|Ics], Line, Tlen, _, _) ->
  yystate(18, Ics, Line, Tlen+1, 3, Tlen);
yystate(18, [46|Ics], Line, Tlen, _, _) ->
  yystate(18, Ics, Line, Tlen+1, 3, Tlen);
yystate(18, [45|Ics], Line, Tlen, _, _) ->
  yystate(18, Ics, Line, Tlen+1, 3, Tlen);
yystate(18, [C|Ics], Line, Tlen, _, _) when C >= 48, C =< 57 ->
  yystate(18, Ics, Line, Tlen+1, 3, Tlen);
yystate(18, [C|Ics], Line, Tlen, _, _) when C >= 65, C =< 90 ->
  yystate(18, Ics, Line, Tlen+1, 3, Tlen);
yystate(18, [C|Ics], Line, Tlen, _, _) when C >= 97, C =< 122 ->
  yystate(18, Ics, Line, Tlen+1, 3, Tlen);
yystate(18, Ics, Line, Tlen, _, _) ->
  {3,Tlen,Ics,Line,18};
yystate(17, Ics, Line, Tlen, _, _) ->
  {0,Tlen,Ics,Line};
yystate(16, [95|Ics], Line, Tlen, Action, Alen) ->
  yystate(16, Ics, Line, Tlen+1, Action, Alen);
yystate(16, [46|Ics], Line, Tlen, Action, Alen) ->
  yystate(16, Ics, Line, Tlen+1, Action, Alen);
yystate(16, [45|Ics], Line, Tlen, Action, Alen) ->
  yystate(16, Ics, Line, Tlen+1, Action, Alen);
yystate(16, [39|Ics], Line, Tlen, Action, Alen) ->
  yystate(12, Ics, Line, Tlen+1, Action, Alen);
yystate(16, [10|Ics], Line, Tlen, Action, Alen) ->
  yystate(16, Ics, Line+1, Tlen+1, Action, Alen);
yystate(16, [C|Ics], Line, Tlen, Action, Alen) when C >= 0, C =< 9 ->
  yystate(16, Ics, Line, Tlen+1, Action, Alen);
yystate(16, [C|Ics], Line, Tlen, Action, Alen) when C >= 11, C =< 32 ->
  yystate(16, Ics, Line, Tlen+1, Action, Alen);
yystate(16, [C|Ics], Line, Tlen, Action, Alen) when C >= 48, C =< 57 ->
  yystate(16, Ics, Line, Tlen+1, Action, Alen);
yystate(16, [C|Ics], Line, Tlen, Action, Alen) when C >= 65, C =< 90 ->
  yystate(16, Ics, Line, Tlen+1, Action, Alen);
yystate(16, [C|Ics], Line, Tlen, Action, Alen) when C >= 97, C =< 122 ->
  yystate(16, Ics, Line, Tlen+1, Action, Alen);
yystate(16, Ics, Line, Tlen, Action, Alen) ->
  {Action,Alen,Tlen,Ics,Line,16};
yystate(15, [105|Ics], Line, Tlen, Action, Alen) ->
  yystate(19, Ics, Line, Tlen+1, Action, Alen);
yystate(15, [73|Ics], Line, Tlen, Action, Alen) ->
  yystate(19, Ics, Line, Tlen+1, Action, Alen);
yystate(15, Ics, Line, Tlen, Action, Alen) ->
  {Action,Alen,Tlen,Ics,Line,15};
yystate(14, [114|Ics], Line, Tlen, Action, Alen) ->
  yystate(10, Ics, Line, Tlen+1, Action, Alen);
yystate(14, [100|Ics], Line, Tlen, Action, Alen) ->
  yystate(3, Ics, Line, Tlen+1, Action, Alen);
yystate(14, [97|Ics], Line, Tlen, Action, Alen) ->
  yystate(0, Ics, Line, Tlen+1, Action, Alen);
yystate(14, [82|Ics], Line, Tlen, Action, Alen) ->
  yystate(10, Ics, Line, Tlen+1, Action, Alen);
yystate(14, [68|Ics], Line, Tlen, Action, Alen) ->
  yystate(3, Ics, Line, Tlen+1, Action, Alen);
yystate(14, [65|Ics], Line, Tlen, Action, Alen) ->
  yystate(0, Ics, Line, Tlen+1, Action, Alen);
yystate(14, Ics, Line, Tlen, Action, Alen) ->
  {Action,Alen,Tlen,Ics,Line,14};
yystate(13, [110|Ics], Line, Tlen, Action, Alen) ->
  yystate(17, Ics, Line, Tlen+1, Action, Alen);
yystate(13, [78|Ics], Line, Tlen, Action, Alen) ->
  yystate(17, Ics, Line, Tlen+1, Action, Alen);
yystate(13, Ics, Line, Tlen, Action, Alen) ->
  {Action,Alen,Tlen,Ics,Line,13};
yystate(12, Ics, Line, Tlen, _, _) ->
  {4,Tlen,Ics,Line};
yystate(11, [114|Ics], Line, Tlen, Action, Alen) ->
  yystate(15, Ics, Line, Tlen+1, Action, Alen);
yystate(11, [82|Ics], Line, Tlen, Action, Alen) ->
  yystate(15, Ics, Line, Tlen+1, Action, Alen);
yystate(11, Ics, Line, Tlen, Action, Alen) ->
  {Action,Alen,Tlen,Ics,Line,11};
yystate(10, [101|Ics], Line, Tlen, Action, Alen) ->
  yystate(6, Ics, Line, Tlen+1, Action, Alen);
yystate(10, [69|Ics], Line, Tlen, Action, Alen) ->
  yystate(6, Ics, Line, Tlen+1, Action, Alen);
yystate(10, Ics, Line, Tlen, Action, Alen) ->
  {Action,Alen,Tlen,Ics,Line,10};
yystate(9, [111|Ics], Line, Tlen, Action, Alen) ->
  yystate(13, Ics, Line, Tlen+1, Action, Alen);
yystate(9, [79|Ics], Line, Tlen, Action, Alen) ->
  yystate(13, Ics, Line, Tlen+1, Action, Alen);
yystate(9, Ics, Line, Tlen, Action, Alen) ->
  {Action,Alen,Tlen,Ics,Line,9};
yystate(8, [95|Ics], Line, Tlen, Action, Alen) ->
  yystate(16, Ics, Line, Tlen+1, Action, Alen);
yystate(8, [46|Ics], Line, Tlen, Action, Alen) ->
  yystate(16, Ics, Line, Tlen+1, Action, Alen);
yystate(8, [45|Ics], Line, Tlen, Action, Alen) ->
  yystate(16, Ics, Line, Tlen+1, Action, Alen);
yystate(8, [39|Ics], Line, Tlen, Action, Alen) ->
  yystate(20, Ics, Line, Tlen+1, Action, Alen);
yystate(8, [10|Ics], Line, Tlen, Action, Alen) ->
  yystate(24, Ics, Line+1, Tlen+1, Action, Alen);
yystate(8, [C|Ics], Line, Tlen, Action, Alen) when C >= 0, C =< 9 ->
  yystate(24, Ics, Line, Tlen+1, Action, Alen);
yystate(8, [C|Ics], Line, Tlen, Action, Alen) when C >= 11, C =< 32 ->
  yystate(24, Ics, Line, Tlen+1, Action, Alen);
yystate(8, [C|Ics], Line, Tlen, Action, Alen) when C >= 48, C =< 57 ->
  yystate(16, Ics, Line, Tlen+1, Action, Alen);
yystate(8, [C|Ics], Line, Tlen, Action, Alen) when C >= 65, C =< 90 ->
  yystate(16, Ics, Line, Tlen+1, Action, Alen);
yystate(8, [C|Ics], Line, Tlen, Action, Alen) when C >= 97, C =< 122 ->
  yystate(16, Ics, Line, Tlen+1, Action, Alen);
yystate(8, Ics, Line, Tlen, Action, Alen) ->
  {Action,Alen,Tlen,Ics,Line,8};
yystate(7, [116|Ics], Line, Tlen, Action, Alen) ->
  yystate(11, Ics, Line, Tlen+1, Action, Alen);
yystate(7, [84|Ics], Line, Tlen, Action, Alen) ->
  yystate(11, Ics, Line, Tlen+1, Action, Alen);
yystate(7, Ics, Line, Tlen, Action, Alen) ->
  {Action,Alen,Tlen,Ics,Line,7};
yystate(6, [108|Ics], Line, Tlen, Action, Alen) ->
  yystate(2, Ics, Line, Tlen+1, Action, Alen);
yystate(6, [76|Ics], Line, Tlen, Action, Alen) ->
  yystate(2, Ics, Line, Tlen+1, Action, Alen);
yystate(6, Ics, Line, Tlen, Action, Alen) ->
  {Action,Alen,Tlen,Ics,Line,6};
yystate(5, [105|Ics], Line, Tlen, Action, Alen) ->
  yystate(9, Ics, Line, Tlen+1, Action, Alen);
yystate(5, [73|Ics], Line, Tlen, Action, Alen) ->
  yystate(9, Ics, Line, Tlen+1, Action, Alen);
yystate(5, Ics, Line, Tlen, Action, Alen) ->
  {Action,Alen,Tlen,Ics,Line,5};
yystate(4, Ics, Line, Tlen, _, _) ->
  {8,Tlen,Ics,Line};
yystate(3, [97|Ics], Line, Tlen, Action, Alen) ->
  yystate(21, Ics, Line, Tlen+1, Action, Alen);
yystate(3, [65|Ics], Line, Tlen, Action, Alen) ->
  yystate(21, Ics, Line, Tlen+1, Action, Alen);
yystate(3, Ics, Line, Tlen, Action, Alen) ->
  {Action,Alen,Tlen,Ics,Line,3};
yystate(2, [97|Ics], Line, Tlen, Action, Alen) ->
  yystate(1, Ics, Line, Tlen+1, Action, Alen);
yystate(2, [65|Ics], Line, Tlen, Action, Alen) ->
  yystate(1, Ics, Line, Tlen+1, Action, Alen);
yystate(2, Ics, Line, Tlen, Action, Alen) ->
  {Action,Alen,Tlen,Ics,Line,2};
yystate(1, [116|Ics], Line, Tlen, Action, Alen) ->
  yystate(5, Ics, Line, Tlen+1, Action, Alen);
yystate(1, [84|Ics], Line, Tlen, Action, Alen) ->
  yystate(5, Ics, Line, Tlen+1, Action, Alen);
yystate(1, Ics, Line, Tlen, Action, Alen) ->
  {Action,Alen,Tlen,Ics,Line,1};
yystate(0, [116|Ics], Line, Tlen, Action, Alen) ->
  yystate(7, Ics, Line, Tlen+1, Action, Alen);
yystate(0, [84|Ics], Line, Tlen, Action, Alen) ->
  yystate(7, Ics, Line, Tlen+1, Action, Alen);
yystate(0, Ics, Line, Tlen, Action, Alen) ->
  {Action,Alen,Tlen,Ics,Line,0};
yystate(S, Ics, Line, Tlen, Action, Alen) ->
  {Action,Alen,Tlen,Ics,Line,S}.

%% yyaction(Action, TokenLength, TokenChars, TokenLine) ->
%% {token,Token} | {end_token, Token} | skip_token | {error,String}.
%% Generated action function.

yyaction(0, _, _, TokenLine) ->
  yyaction_0(TokenLine);
yyaction(1, _, _, TokenLine) ->
  yyaction_1(TokenLine);
yyaction(2, _, _, TokenLine) ->
  yyaction_2(TokenLine);
yyaction(3, TokenLen, YYtcs, TokenLine) ->
  TokenChars = yypre(YYtcs, TokenLen),
  yyaction_3(TokenChars, TokenLine);
yyaction(4, TokenLen, YYtcs, TokenLine) ->
  TokenChars = yypre(YYtcs, TokenLen),
  yyaction_4(TokenChars, TokenLine);
yyaction(5, _, _, _) ->
  yyaction_5();
yyaction(6, _, _, TokenLine) ->
  yyaction_6(TokenLine);
yyaction(7, _, _, TokenLine) ->
  yyaction_7(TokenLine);
yyaction(8, _, _, TokenLine) ->
  yyaction_8(TokenLine);
yyaction(9, _, _, _) ->
  yyaction_9();
yyaction(10, _, _, _) ->
  yyaction_10();
yyaction(_, _, _, _) -> error.

-compile({inline,yyaction_0/1}).
-file("./arff_scanner.xrl", 6).
yyaction_0(TokenLine) ->
  { token, { decl_relation, TokenLine } } .

-compile({inline,yyaction_1/1}).
-file("./arff_scanner.xrl", 7).
yyaction_1(TokenLine) ->
  { token, { decl_attribute, TokenLine } } .

-compile({inline,yyaction_2/1}).
-file("./arff_scanner.xrl", 8).
yyaction_2(TokenLine) ->
  { end_token, { decl_data, TokenLine } } .

-compile({inline,yyaction_3/2}).
-file("./arff_scanner.xrl", 10).
yyaction_3(TokenChars, TokenLine) ->
  { token, { id, TokenLine, TokenChars } } .

-compile({inline,yyaction_4/2}).
-file("./arff_scanner.xrl", 11).
yyaction_4(TokenChars, TokenLine) ->
  { token, { id, TokenLine, string : strip (TokenChars, both, 39) } } .

-compile({inline,yyaction_5/0}).
-file("./arff_scanner.xrl", 12).
yyaction_5() ->
  { error, "Empty string" } .

-compile({inline,yyaction_6/1}).
-file("./arff_scanner.xrl", 13).
yyaction_6(TokenLine) ->
  { token, { '{', TokenLine } } .

-compile({inline,yyaction_7/1}).
-file("./arff_scanner.xrl", 14).
yyaction_7(TokenLine) ->
  { token, { '}', TokenLine } } .

-compile({inline,yyaction_8/1}).
-file("./arff_scanner.xrl", 15).
yyaction_8(TokenLine) ->
  { token, { ',', TokenLine } } .

-compile({inline,yyaction_9/0}).
-file("./arff_scanner.xrl", 16).
yyaction_9() ->
  skip_token .

-compile({inline,yyaction_10/0}).
-file("./arff_scanner.xrl", 17).
yyaction_10() ->
  skip_token .

-file("/usr/lib/erlang/lib/parsetools-2.0.6/include/leexinc.hrl", 282).
