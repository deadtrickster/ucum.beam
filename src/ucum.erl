-module(ucum).

-export([convert/3]).

-include("model.hrl").

convert(Value, Src, Dst) ->
  CSrc = ucum_normalizer:normalize(Src),
  CDst = ucum_normalizer:normalize(Dst),

  CanValue = decimal:multiply(Value, CSrc#canonical.value),

  decimal:divide(CanValue, CDst#canonical.value).
