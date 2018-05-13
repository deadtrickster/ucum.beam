-module(ucum_expression_composer).

-export([compose/2]).

-include("model.hrl").

%% ===================================================================
%% API
%% ===================================================================

compose(#canonical{}=C, Value) ->
  {ok, Fd} = ram_file:open("", [write, read, binary]),

  case Value of
    true -> file:write(Fd, decimal_conv:string(C#canonical.value));
    _ -> ok
  end,

  [CUnit | Rest] = C#canonical.units,

  write_canonical_unit(Fd, CUnit),

  [begin
     file:write(Fd, "."),
     write_canonical_unit(Fd, CU)
   end || CU <- Rest],

  {ok, Size} = ram_file:get_size(Fd),
  {ok, Str} = file:pread(Fd, 0, Size),
  ok = file:close(Fd),
  Str.

%% ===================================================================
%% Private functions
%% ===================================================================

write_canonical_unit(Fd, #canonical_unit{base=Base,
                                         exp=Exp}) ->

  file:write(Fd, Base#base_unit.code),

  case Exp of
    1 -> ok;
    _ -> file:write(Fd, integer_to_list(Exp))
  end.
