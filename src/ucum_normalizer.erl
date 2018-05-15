-module(ucum_normalizer).

-export([normalize/1]).

-include("model.hrl").

%% ===================================================================
%% API
%% ===================================================================

normalize(#term{}) ->
  

normalize(#symbol{prefix=Prefix,
                  unit=Unit,
                  exp=Exp}) ->

  Result =
    case Unit of
      #base_unit{} ->
        #canonical{units=[#canonical_unit{base=Unit,
                                          exp=Exp}]};
      _ ->
        Expanded = ucum_canonical:update_units_exponents(expand_defined_unit(Unit), Exp),
        Result0 = #canonical{units=Expanded#canonical.units},
        ucum_canonical:apply_exponent(Result0, Expanded#canonical.value, Exp)
    end,

  case Prefix of
    undefined -> Result;
    #prefix{value=PValue} ->
      ucum_canonical:apply_exponent(Result, PValue, Exp)
  end.

normalize_term_comp(#term{}, Div) ->
  ;
normalize_term_comp(#factor{}, Div) ->
  ;
normalize_term_comp(#symbol{}, Div) ->
  



%% ===================================================================
%% Private functions
%% ===================================================================

expand_defined_unit(#defined_unit{value = Value} = DU) ->

  UnitCode = catch_special(DU),

  Term = ucum_parser:parse(UnitCode),

  Canonical = normalize(Term),

  ucum_canonical:multiply_value(Canonical, Value).

catch_special(#defined_unit{is_special=true}) ->
    erlang:error("Special units are not implemented yet");
     catch_special(#defined_unit{unit=Unit}) ->
         Unit.
