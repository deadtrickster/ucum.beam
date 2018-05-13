-module(ucum_canonical).

-export([new/0,
         set_units/2,
         multiply_value/2,
         divide_value/2,
         update_units_exponents/2,
         apply_exponent/3]).

-include("model.hrl").

%% ===================================================================
%% API
%% ===================================================================

new() ->
  #canonical{}.

set_units(C, Units) ->
  C#canonical{units=Units}.

multiply_value(#canonical{value=Value}=C, M) ->
  C#canonical{value=decimal:multiply(Value, M)}.

divide_value(#canonical{value=Value}=C, M) ->
  C#canonical{value=decimal:divide(Value, M)}.

update_units_exponents(#canonical{units=Units}=C, Exp) ->
  C#canonical{units=[CU#canonical_unit{exp=CUExp * Exp} || #canonical_unit{exp=CUExp}=CU <- Units]}.

apply_exponent(C, V, E) when E > 0 ->
  apply_exponent_(C, fun decimal:multiply/2, V, E);
apply_exponent(C, V, E) when E < 0 ->
  apply_exponent_(C, fun decimal:divide/2, V, E);
apply_exponent(C, _V, 0) ->
  C.

%% ===================================================================
%% Private functions
%% ===================================================================

apply_exponent_(#canonical{value=Value}=C, Fun, V, E) ->
  C#canonical{value=times(fun(Acc) ->
                              Fun(Acc, V)
                          end, Value, E)}.

times(_Fun, Acc, 0) ->
  Acc;
times(Fun, Acc, Counter) ->
  times(Fun, Fun(Acc), Counter-1).
