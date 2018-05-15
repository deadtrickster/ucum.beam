-module(ucum_lexer).

-export([consume/1]).

%% ==================================================================
%% API
%% ==================================================================

consume(String) ->
  consume(String, <<>>, []).

%% ===================================================================
%% Private functions
%% ===================================================================

consume(<<>>, Acc, Tokens) ->
  lists:reverse([Acc | Tokens]);
consume(<<$/, Rest/binary>>, Acc, Tokens) ->
  consume(Rest, <<>>, [solidus, Acc | Tokens]);
consume(<<$., Rest/binary>>, Acc, Tokens) ->
  consume(Rest, <<>>, [mul, Acc | Tokens]);
consume(<<Char, Rest/binary>>, Acc, Tokens) ->
  consume(Rest, <<Acc/binary, Char>>, Tokens).

