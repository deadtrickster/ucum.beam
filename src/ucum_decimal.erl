-module(ucum_decimal).

-export([new/1]).

-include("model.hrl").

%% ===================================================================
%% API
%% ===================================================================

new(Coef) ->
  #decimal{coef = Coef}.

%% ===================================================================
%% Private functions
%% ===================================================================

