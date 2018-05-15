-module(ucum_parser).

-export([parse/1,
         symbol/1]).

-include("model.hrl").

-define(DECIMAL(Value), Value).

%% ===================================================================
%% API
%% ===================================================================

parse(String) ->
  Lexemes = ucum_lexer:consume(String),
  case Lexemes of
    [solidus | RLexemes] -> #term{op=solidus, term=parse_term(RLexemes)};
    _ -> parse_term(Lexemes)
  end.

parse_term([CompL]) ->
  #term{comp=parse_comp(CompL)};
parse_term([CompL, Op | RLexemes]) ->
  Comp = parse_comp(CompL),
  #term{comp=Comp, op=Op, term = parse_term(RLexemes)}.

parse_comp(CompL) ->
  case maybe_number(CompL) of
    false ->
      {S, <<>>} = symbol(CompL),
      S;
    {Number, <<>>} -> #factor{value=Number};
    {_Number, _Rest} ->
      {S, <<>>} = symbol(CompL),
      S
  end.

symbol(String) ->
  prefix(String, #symbol{}).

%% ===================================================================
%% Private functions
%% ===================================================================

prefix(String, S0) ->
  prefix_2(String, S0).


prefix_2(<<"da", String/binary>>=String0, S0) ->
  try
    S = S0#symbol{prefix=#prefix{
                            code= <<"da">>,
                            name= <<"deka">>,
                            value= decimal_conv:number("1e1")
                           }},
    unit(String, S)
  of
    OK -> OK
  catch
    error:{unknown_unit, String} -> prefix_1(String0, S0)
  end;

prefix_2(<<"Ti", String/binary>>=String0, S0) ->
  try
    S = S0#symbol{prefix=#prefix{
                            code= <<"Ti">>,
                            name= <<"tebi">>,
                            value= decimal_conv:number("1099511627776")
                           }},
    unit(String, S)
  of
    OK -> OK
  catch
    error:{unknown_unit, String} -> prefix_1(String0, S0)
  end;

prefix_2(<<"Mi", String/binary>>=String0, S0) ->
  try
    S = S0#symbol{prefix=#prefix{
                            code= <<"Mi">>,
                            name= <<"mebi">>,
                            value= decimal_conv:number("1048576")
                           }},
    unit(String, S)
  of
    OK -> OK
  catch
    error:{unknown_unit, String} -> prefix_1(String0, S0)
  end;

prefix_2(<<"Ki", String/binary>>=String0, S0) ->
  try
    S = S0#symbol{prefix=#prefix{
                            code= <<"Ki">>,
                            name= <<"kibi">>,
                            value= decimal_conv:number("1024")
                           }},
    unit(String, S)
  of
    OK -> OK
  catch
    error:{unknown_unit, String} -> prefix_1(String0, S0)
  end;

prefix_2(<<"Gi", String/binary>>=String0, S0) ->
  try
    S = S0#symbol{prefix=#prefix{
                            code= <<"Gi">>,
                            name= <<"gibi">>,
                            value= decimal_conv:number("1073741824")
                           }},
    unit(String, S)
  of
    OK -> OK
  catch
    error:{unknown_unit, String} -> prefix_1(String0, S0)
  end;

prefix_2(String, S0) ->
  prefix_1(String, S0).


prefix_1(<<"z", String/binary>>=String0, S0) ->
  try
    S = S0#symbol{prefix=#prefix{
                            code= <<"z">>,
                            name= <<"zepto">>,
                            value= decimal_conv:number("1e-21")
                           }},
    unit(String, S)
  of
    OK -> OK
  catch
    error:{unknown_unit, String} -> prefix_0(String0, S0)
  end;

prefix_1(<<"y", String/binary>>=String0, S0) ->
  try
    S = S0#symbol{prefix=#prefix{
                            code= <<"y">>,
                            name= <<"yocto">>,
                            value= decimal_conv:number("1e-24")
                           }},
    unit(String, S)
  of
    OK -> OK
  catch
    error:{unknown_unit, String} -> prefix_0(String0, S0)
  end;

prefix_1(<<"u", String/binary>>=String0, S0) ->
  try
    S = S0#symbol{prefix=#prefix{
                            code= <<"u">>,
                            name= <<"micro">>,
                            value= decimal_conv:number("1e-6")
                           }},
    unit(String, S)
  of
    OK -> OK
  catch
    error:{unknown_unit, String} -> prefix_0(String0, S0)
  end;

prefix_1(<<"p", String/binary>>=String0, S0) ->
  try
    S = S0#symbol{prefix=#prefix{
                            code= <<"p">>,
                            name= <<"pico">>,
                            value= decimal_conv:number("1e-12")
                           }},
    unit(String, S)
  of
    OK -> OK
  catch
    error:{unknown_unit, String} -> prefix_0(String0, S0)
  end;

prefix_1(<<"n", String/binary>>=String0, S0) ->
  try
    S = S0#symbol{prefix=#prefix{
                            code= <<"n">>,
                            name= <<"nano">>,
                            value= decimal_conv:number("1e-9")
                           }},
    unit(String, S)
  of
    OK -> OK
  catch
    error:{unknown_unit, String} -> prefix_0(String0, S0)
  end;

prefix_1(<<"m", String/binary>>=String0, S0) ->
  try
    S = S0#symbol{prefix=#prefix{
                            code= <<"m">>,
                            name= <<"milli">>,
                            value= decimal_conv:number("1e-3")
                           }},
    unit(String, S)
  of
    OK -> OK
  catch
    error:{unknown_unit, String} -> prefix_0(String0, S0)
  end;

prefix_1(<<"k", String/binary>>=String0, S0) ->
  try
    S = S0#symbol{prefix=#prefix{
                            code= <<"k">>,
                            name= <<"kilo">>,
                            value= decimal_conv:number("1e3")
                           }},
    unit(String, S)
  of
    OK -> OK
  catch
    error:{unknown_unit, String} -> prefix_0(String0, S0)
  end;

prefix_1(<<"h", String/binary>>=String0, S0) ->
  try
    S = S0#symbol{prefix=#prefix{
                            code= <<"h">>,
                            name= <<"hecto">>,
                            value= decimal_conv:number("1e2")
                           }},
    unit(String, S)
  of
    OK -> OK
  catch
    error:{unknown_unit, String} -> prefix_0(String0, S0)
  end;

prefix_1(<<"f", String/binary>>=String0, S0) ->
  try
    S = S0#symbol{prefix=#prefix{
                            code= <<"f">>,
                            name= <<"femto">>,
                            value= decimal_conv:number("1e-15")
                           }},
    unit(String, S)
  of
    OK -> OK
  catch
    error:{unknown_unit, String} -> prefix_0(String0, S0)
  end;

prefix_1(<<"d", String/binary>>=String0, S0) ->
  try
    S = S0#symbol{prefix=#prefix{
                            code= <<"d">>,
                            name= <<"deci">>,
                            value= decimal_conv:number("1e-1")
                           }},
    unit(String, S)
  of
    OK -> OK
  catch
    error:{unknown_unit, String} -> prefix_0(String0, S0)
  end;

prefix_1(<<"c", String/binary>>=String0, S0) ->
  try
    S = S0#symbol{prefix=#prefix{
                            code= <<"c">>,
                            name= <<"centi">>,
                            value= decimal_conv:number("1e-2")
                           }},
    unit(String, S)
  of
    OK -> OK
  catch
    error:{unknown_unit, String} -> prefix_0(String0, S0)
  end;

prefix_1(<<"a", String/binary>>=String0, S0) ->
  try
    S = S0#symbol{prefix=#prefix{
                            code= <<"a">>,
                            name= <<"atto">>,
                            value= decimal_conv:number("1e-18")
                           }},
    unit(String, S)
  of
    OK -> OK
  catch
    error:{unknown_unit, String} -> prefix_0(String0, S0)
  end;

prefix_1(<<"Z", String/binary>>=String0, S0) ->
  try
    S = S0#symbol{prefix=#prefix{
                            code= <<"Z">>,
                            name= <<"zetta">>,
                            value= decimal_conv:number("1e21")
                           }},
    unit(String, S)
  of
    OK -> OK
  catch
    error:{unknown_unit, String} -> prefix_0(String0, S0)
  end;

prefix_1(<<"Y", String/binary>>=String0, S0) ->
  try
    S = S0#symbol{prefix=#prefix{
                            code= <<"Y">>,
                            name= <<"yotta">>,
                            value= decimal_conv:number("1e24")
                           }},
    unit(String, S)
  of
    OK -> OK
  catch
    error:{unknown_unit, String} -> prefix_0(String0, S0)
  end;

prefix_1(<<"T", String/binary>>=String0, S0) ->
  try
    S = S0#symbol{prefix=#prefix{
                            code= <<"T">>,
                            name= <<"tera">>,
                            value= decimal_conv:number("1e12")
                           }},
    unit(String, S)
  of
    OK -> OK
  catch
    error:{unknown_unit, String} -> prefix_0(String0, S0)
  end;

prefix_1(<<"P", String/binary>>=String0, S0) ->
  try
    S = S0#symbol{prefix=#prefix{
                            code= <<"P">>,
                            name= <<"peta">>,
                            value= decimal_conv:number("1e15")
                           }},
    unit(String, S)
  of
    OK -> OK
  catch
    error:{unknown_unit, String} -> prefix_0(String0, S0)
  end;

prefix_1(<<"M", String/binary>>=String0, S0) ->
  try
    S = S0#symbol{prefix=#prefix{
                            code= <<"M">>,
                            name= <<"mega">>,
                            value= decimal_conv:number("1e6")
                           }},
    unit(String, S)
  of
    OK -> OK
  catch
    error:{unknown_unit, String} -> prefix_0(String0, S0)
  end;

prefix_1(<<"G", String/binary>>=String0, S0) ->
  try
    S = S0#symbol{prefix=#prefix{
                            code= <<"G">>,
                            name= <<"giga">>,
                            value= decimal_conv:number("1e9")
                           }},
    unit(String, S)
  of
    OK -> OK
  catch
    error:{unknown_unit, String} -> prefix_0(String0, S0)
  end;

prefix_1(<<"E", String/binary>>=String0, S0) ->
  try
    S = S0#symbol{prefix=#prefix{
                            code= <<"E">>,
                            name= <<"exa">>,
                            value= decimal_conv:number("1e18")
                           }},
    unit(String, S)
  of
    OK -> OK
  catch
    error:{unknown_unit, String} -> prefix_0(String0, S0)
  end;

prefix_1(String, S0) ->
  prefix_0(String, S0).

prefix_0(String, S0) ->
  unit(String, S0).


unit(<<"[m/s2/Hz^(1/2)]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[m/s2/Hz^(1/2)]">>,
                        name= <<"meter per square seconds per square root of hertz">>,
                        property= <<"amplitude spectral density">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= true,
                        class= <<"misc">>,
                        unit= <<"sqrt(1 m2/s4/Hz)">>,
                        value= <<"">>
                       }},

  exponent(String, S);

unit(<<"[Amb'a'1'U]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[Amb'a'1'U]">>,
                        name= <<"allergen unit for Ambrosia artemisiifolia">>,
                        property= <<"procedure defined amount of the major allergen of ragweed.">>,
                        is_metric= true,
                        is_arbitrary= true,
                        is_special= false,
                        class= <<"chemical">>,
                        unit= <<"1">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[anti'Xa'U]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[anti'Xa'U]">>,
                        name= <<"anti factor Xa unit">>,
                        property= <<"biologic activity of factor Xa inhibitor (heparin)">>,
                        is_metric= true,
                        is_arbitrary= true,
                        is_special= false,
                        class= <<"chemical">>,
                        unit= <<"1">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[in_i'H2O]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[in_i'H2O]">>,
                        name= <<"inch of water column">>,
                        property= <<"pressure">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"clinical">>,
                        unit= <<"m[H2O].[in_i]/m">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[stone_av]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[stone_av]">>,
                        name= <<"British stonestone">>,
                        property= <<"mass">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"avoirdupois">>,
                        unit= <<"[lb_av]">>,
                        value= <<"14">>
                       }},

  exponent(String, S);

unit(<<"[TCID_50]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[TCID_50]">>,
                        name= <<"50% tissue culture infectious dose">>,
                        property= <<"biologic activity (infectivity) of an infectious agent preparation">>,
                        is_metric= true,
                        is_arbitrary= true,
                        is_special= false,
                        class= <<"chemical">>,
                        unit= <<"1">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[CCID_50]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[CCID_50]">>,
                        name= <<"50% cell culture infectious dose">>,
                        property= <<"biologic activity (infectivity) of an infectious agent preparation">>,
                        is_metric= true,
                        is_arbitrary= true,
                        is_special= false,
                        class= <<"chemical">>,
                        unit= <<"1">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[in_i'Hg]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[in_i'Hg]">>,
                        name= <<"inch of mercury column">>,
                        property= <<"pressure">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"clinical">>,
                        unit= <<"m[Hg].[in_i]/m">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[lton_av]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[lton_av]">>,
                        name= <<"British tonlong ton">>,
                        property= <<"mass">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"avoirdupois">>,
                        unit= <<"[lcwt_av]">>,
                        value= <<"20">>
                       }},

  exponent(String, S);

unit(<<"[ston_av]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[ston_av]">>,
                        name= <<"U.S. tonshort ton">>,
                        property= <<"mass">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"avoirdupois">>,
                        unit= <<"[scwt_av]">>,
                        value= <<"20">>
                       }},

  exponent(String, S);

unit(<<"[lcwt_av]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[lcwt_av]">>,
                        name= <<"British hundredweightlong hunderdweight">>,
                        property= <<"mass">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"avoirdupois">>,
                        unit= <<"[lb_av]">>,
                        value= <<"112">>
                       }},

  exponent(String, S);

unit(<<"[scwt_av]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[scwt_av]">>,
                        name= <<"U.S. hundredweightshort hundredweight">>,
                        property= <<"mass">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"avoirdupois">>,
                        unit= <<"[lb_av]">>,
                        value= <<"100">>
                       }},

  exponent(String, S);

unit(<<"[car_Au]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[car_Au]">>,
                        name= <<"carat of gold alloys">>,
                        property= <<"mass fraction">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"misc">>,
                        unit= <<"/24">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"B[10.nV]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"B[10.nV]">>,
                        name= <<"bel 10 nanovolt">>,
                        property= <<"electric potential level">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= true,
                        class= <<"levels">>,
                        unit= <<"2lg(10 nV)">>,
                        value= <<"">>
                       }},

  exponent(String, S);

unit(<<"[D'ag'U]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[D'ag'U]">>,
                        name= <<"D-antigen unit">>,
                        property= <<"procedure defined amount of a poliomyelitis d-antigen substance">>,
                        is_metric= true,
                        is_arbitrary= true,
                        is_special= false,
                        class= <<"chemical">>,
                        unit= <<"1">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[EID_50]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[EID_50]">>,
                        name= <<"50% embryo infectious dose">>,
                        property= <<"biologic activity (infectivity) of an infectious agent preparation">>,
                        is_metric= true,
                        is_arbitrary= true,
                        is_special= false,
                        class= <<"chemical">>,
                        unit= <<"1">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[mclg'U]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[mclg'U]">>,
                        name= <<"Mac Lagan unit">>,
                        property= <<"arbitrary biologic activity">>,
                        is_metric= true,
                        is_arbitrary= true,
                        is_special= false,
                        class= <<"chemical">>,
                        unit= <<"1">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[bdsk'U]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[bdsk'U]">>,
                        name= <<"Bodansky unit">>,
                        property= <<"biologic activity of phosphatase">>,
                        is_metric= true,
                        is_arbitrary= true,
                        is_special= false,
                        class= <<"chemical">>,
                        unit= <<"1">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[smgy'U]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[smgy'U]">>,
                        name= <<"Somogyi unit">>,
                        property= <<"biologic activity of amylase">>,
                        is_metric= true,
                        is_arbitrary= true,
                        is_special= false,
                        class= <<"chemical">>,
                        unit= <<"1">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[todd'U]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[todd'U]">>,
                        name= <<"Todd unit">>,
                        property= <<"biologic activity antistreptolysin O">>,
                        is_metric= true,
                        is_arbitrary= true,
                        is_special= false,
                        class= <<"chemical">>,
                        unit= <<"1">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[beth'U]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[beth'U]">>,
                        name= <<"Bethesda unit">>,
                        property= <<"biologic activity of factor VIII inhibitor">>,
                        is_metric= true,
                        is_arbitrary= true,
                        is_special= false,
                        class= <<"chemical">>,
                        unit= <<"1">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[hnsf'U]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[hnsf'U]">>,
                        name= <<"Hounsfield unit">>,
                        property= <<"x-ray attenuation">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"clinical">>,
                        unit= <<"1">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[mesh_i]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[mesh_i]">>,
                        name= <<"mesh">>,
                        property= <<"lineic number">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"clinical">>,
                        unit= <<"/[in_i]">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"%[slope]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"%[slope]">>,
                        name= <<"percent of slope">>,
                        property= <<"slope">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= true,
                        class= <<"clinical">>,
                        unit= <<"100tan(1 rad)">>,
                        value= <<"">>
                       }},

  exponent(String, S);

unit(<<"[p'diop]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[p'diop]">>,
                        name= <<"prism diopter">>,
                        property= <<"refraction of a prism">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= true,
                        class= <<"clinical">>,
                        unit= <<"100tan(1 rad)">>,
                        value= <<"">>
                       }},

  exponent(String, S);

unit(<<"[wood'U]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[wood'U]">>,
                        name= <<"Wood unit">>,
                        property= <<"fluid resistance">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"clinical">>,
                        unit= <<"mm[Hg].min/L">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[Btu_th]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[Btu_th]">>,
                        name= <<"thermochemical British thermal unit">>,
                        property= <<"energy">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"heat">>,
                        unit= <<"kJ">>,
                        value= <<"1.054350">>
                       }},

  exponent(String, S);

unit(<<"[Btu_IT]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[Btu_IT]">>,
                        name= <<"international table British thermal unit">>,
                        property= <<"energy">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"heat">>,
                        unit= <<"kJ">>,
                        value= <<"1.05505585262">>
                       }},

  exponent(String, S);

unit(<<"[Btu_60]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[Btu_60]">>,
                        name= <<"British thermal unit at 60 °F">>,
                        property= <<"energy">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"heat">>,
                        unit= <<"kJ">>,
                        value= <<"1.05468">>
                       }},

  exponent(String, S);

unit(<<"[Btu_59]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[Btu_59]">>,
                        name= <<"British thermal unit at 59 °F">>,
                        property= <<"energy">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"heat">>,
                        unit= <<"kJ">>,
                        value= <<"1.05480">>
                       }},

  exponent(String, S);

unit(<<"[Btu_39]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[Btu_39]">>,
                        name= <<"British thermal unit at 39 °F">>,
                        property= <<"energy">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"heat">>,
                        unit= <<"kJ">>,
                        value= <<"1.05967">>
                       }},

  exponent(String, S);

unit(<<"cal_[20]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"cal_[20]">>,
                        name= <<"calorie at 20 °C">>,
                        property= <<"energy">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"heat">>,
                        unit= <<"J">>,
                        value= <<"4.18190">>
                       }},

  exponent(String, S);

unit(<<"cal_[15]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"cal_[15]">>,
                        name= <<"calorie at 15 °C">>,
                        property= <<"energy">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"heat">>,
                        unit= <<"J">>,
                        value= <<"4.18580">>
                       }},

  exponent(String, S);

unit(<<"[cicero]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[cicero]">>,
                        name= <<"Didot's picacicero">>,
                        property= <<"length">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"typeset">>,
                        unit= <<"[didot]">>,
                        value= <<"12">>
                       }},

  exponent(String, S);

unit(<<"[pca_pr]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[pca_pr]">>,
                        name= <<"Printer's pica">>,
                        property= <<"length">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"typeset">>,
                        unit= <<"[pnt_pr]">>,
                        value= <<"12">>
                       }},

  exponent(String, S);

unit(<<"[pnt_pr]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[pnt_pr]">>,
                        name= <<"Printer's point">>,
                        property= <<"length">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"typeset">>,
                        unit= <<"[in_i]">>,
                        value= <<"0.013837">>
                       }},

  exponent(String, S);

unit(<<"[pwt_tr]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[pwt_tr]">>,
                        name= <<"pennyweight">>,
                        property= <<"mass">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"troy">>,
                        unit= <<"[gr]">>,
                        value= <<"24">>
                       }},

  exponent(String, S);

unit(<<"[min_br]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[min_br]">>,
                        name= <<"minim">>,
                        property= <<"volume">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"brit-volumes">>,
                        unit= <<"[fdr_br]/60">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[fdr_br]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[fdr_br]">>,
                        name= <<"fluid dram">>,
                        property= <<"volume">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"brit-volumes">>,
                        unit= <<"[foz_br]/8">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[foz_br]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[foz_br]">>,
                        name= <<"fluid ounce">>,
                        property= <<"volume">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"brit-volumes">>,
                        unit= <<"[gil_br]/5">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[gil_br]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[gil_br]">>,
                        name= <<"gill">>,
                        property= <<"volume">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"brit-volumes">>,
                        unit= <<"[pt_br]/4">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[gal_br]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[gal_br]">>,
                        name= <<"gallon">>,
                        property= <<"volume">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"brit-volumes">>,
                        unit= <<"l">>,
                        value= <<"4.54609">>
                       }},

  exponent(String, S);

unit(<<"[cup_us]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[cup_us]">>,
                        name= <<"cup">>,
                        property= <<"volume">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"us-volumes">>,
                        unit= <<"[tbs_us]">>,
                        value= <<"16">>
                       }},

  exponent(String, S);

unit(<<"[tsp_us]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[tsp_us]">>,
                        name= <<"teaspoon">>,
                        property= <<"volume">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"us-volumes">>,
                        unit= <<"[tbs_us]/3">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[tbs_us]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[tbs_us]">>,
                        name= <<"tablespoon">>,
                        property= <<"volume">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"us-volumes">>,
                        unit= <<"[foz_us]/2">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[dpt_us]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[dpt_us]">>,
                        name= <<"dry pint">>,
                        property= <<"dry volume">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"us-volumes">>,
                        unit= <<"[dqt_us]/2">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[dqt_us]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[dqt_us]">>,
                        name= <<"dry quart">>,
                        property= <<"dry volume">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"us-volumes">>,
                        unit= <<"[pk_us]/8">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[gal_wi]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[gal_wi]">>,
                        name= <<"historical winchester gallon">>,
                        property= <<"dry volume">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"us-volumes">>,
                        unit= <<"[bu_us]/8">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[crd_us]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[crd_us]">>,
                        name= <<"cord">>,
                        property= <<"fluid volume">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"us-volumes">>,
                        unit= <<"[ft_i]3">>,
                        value= <<"128">>
                       }},

  exponent(String, S);

unit(<<"[min_us]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[min_us]">>,
                        name= <<"minim">>,
                        property= <<"fluid volume">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"us-volumes">>,
                        unit= <<"[fdr_us]/60">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[fdr_us]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[fdr_us]">>,
                        name= <<"fluid dram">>,
                        property= <<"fluid volume">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"us-volumes">>,
                        unit= <<"[foz_us]/8">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[foz_us]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[foz_us]">>,
                        name= <<"fluid ounce">>,
                        property= <<"fluid volume">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"us-volumes">>,
                        unit= <<"[gil_us]/4">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[gil_us]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[gil_us]">>,
                        name= <<"gill">>,
                        property= <<"fluid volume">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"us-volumes">>,
                        unit= <<"[pt_us]/4">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[bbl_us]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[bbl_us]">>,
                        name= <<"barrel">>,
                        property= <<"fluid volume">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"us-volumes">>,
                        unit= <<"[gal_us]">>,
                        value= <<"42">>
                       }},

  exponent(String, S);

unit(<<"[gal_us]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[gal_us]">>,
                        name= <<"Queen Anne's wine gallon">>,
                        property= <<"fluid volume">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"us-volumes">>,
                        unit= <<"[in_i]3">>,
                        value= <<"231">>
                       }},

  exponent(String, S);

unit(<<"[acr_br]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[acr_br]">>,
                        name= <<"acre">>,
                        property= <<"area">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"brit-length">>,
                        unit= <<"[yd_br]2">>,
                        value= <<"4840">>
                       }},

  exponent(String, S);

unit(<<"[nmi_br]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[nmi_br]">>,
                        name= <<"nautical mile">>,
                        property= <<"length">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"brit-length">>,
                        unit= <<"[ft_br]">>,
                        value= <<"6080">>
                       }},

  exponent(String, S);

unit(<<"[fth_br]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[fth_br]">>,
                        name= <<"fathom">>,
                        property= <<"length">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"brit-length">>,
                        unit= <<"[ft_br]">>,
                        value= <<"6">>
                       }},

  exponent(String, S);

unit(<<"[mil_us]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[mil_us]">>,
                        name= <<"mil">>,
                        property= <<"length">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"us-lengths">>,
                        unit= <<"[in_us]">>,
                        value= <<"1e-3">>
                       }},

  exponent(String, S);

unit(<<"[smi_us]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[smi_us]">>,
                        name= <<"square mile">>,
                        property= <<"area">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"us-lengths">>,
                        unit= <<"[mi_us]2">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[srd_us]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[srd_us]">>,
                        name= <<"square rod">>,
                        property= <<"area">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"us-lengths">>,
                        unit= <<"[rd_us]2">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[acr_us]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[acr_us]">>,
                        name= <<"acre">>,
                        property= <<"area">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"us-lengths">>,
                        unit= <<"[rd_us]2">>,
                        value= <<"160">>
                       }},

  exponent(String, S);

unit(<<"[fur_us]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[fur_us]">>,
                        name= <<"furlong">>,
                        property= <<"length">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"us-lengths">>,
                        unit= <<"[rd_us]">>,
                        value= <<"40">>
                       }},

  exponent(String, S);

unit(<<"[fth_us]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[fth_us]">>,
                        name= <<"fathom">>,
                        property= <<"length">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"us-lengths">>,
                        unit= <<"[ft_us]">>,
                        value= <<"6">>
                       }},

  exponent(String, S);

unit(<<"[rlk_us]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[rlk_us]">>,
                        name= <<"link for Ramden's chain">>,
                        property= <<"length">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"us-lengths">>,
                        unit= <<"[rch_us]/100">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[rch_us]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[rch_us]">>,
                        name= <<"Engineer's chainRamden's chain">>,
                        property= <<"length">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"us-lengths">>,
                        unit= <<"[ft_us]">>,
                        value= <<"100">>
                       }},

  exponent(String, S);

unit(<<"[lbf_av]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[lbf_av]">>,
                        name= <<"pound force">>,
                        property= <<"force">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"const">>,
                        unit= <<"[lb_av].[g]">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[smoot]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[smoot]">>,
                        name= <<"Smoot">>,
                        property= <<"length">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"misc">>,
                        unit= <<"[in_i]">>,
                        value= <<"67">>
                       }},

  exponent(String, S);

unit(<<"[car_m]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[car_m]">>,
                        name= <<"metric carat">>,
                        property= <<"mass">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"misc">>,
                        unit= <<"g">>,
                        value= <<"2e-1">>
                       }},

  exponent(String, S);

unit(<<"[knk'U]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[knk'U]">>,
                        name= <<"Kunkel unit">>,
                        property= <<"arbitrary biologic activity">>,
                        is_metric= true,
                        is_arbitrary= true,
                        is_special= false,
                        class= <<"chemical">>,
                        unit= <<"1">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[dye'U]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[dye'U]">>,
                        name= <<"Dye unit">>,
                        property= <<"biologic activity of amylase">>,
                        is_metric= true,
                        is_arbitrary= true,
                        is_special= false,
                        class= <<"chemical">>,
                        unit= <<"1">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[APL'U]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[APL'U]">>,
                        name= <<"APL unit">>,
                        property= <<"biologic activity of anticardiolipin IgA">>,
                        is_metric= true,
                        is_arbitrary= true,
                        is_special= false,
                        class= <<"chemical">>,
                        unit= <<"1">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[MPL'U]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[MPL'U]">>,
                        name= <<"MPL unit">>,
                        property= <<"biologic activity of anticardiolipin IgM">>,
                        is_metric= true,
                        is_arbitrary= true,
                        is_special= false,
                        class= <<"chemical">>,
                        unit= <<"1">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[GPL'U]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[GPL'U]">>,
                        name= <<"GPL unit">>,
                        property= <<"biologic activity of anticardiolipin IgG">>,
                        is_metric= true,
                        is_arbitrary= true,
                        is_special= false,
                        class= <<"chemical">>,
                        unit= <<"1">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[USP'U]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[USP'U]">>,
                        name= <<"United States Pharmacopeia unit">>,
                        property= <<"arbitrary">>,
                        is_metric= true,
                        is_arbitrary= true,
                        is_special= false,
                        class= <<"chemical">>,
                        unit= <<"1">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[arb'U]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[arb'U]">>,
                        name= <<"arbitary unit">>,
                        property= <<"arbitrary">>,
                        is_metric= true,
                        is_arbitrary= true,
                        is_special= false,
                        class= <<"chemical">>,
                        unit= <<"1">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[hp'_Q]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[hp'_Q]">>,
                        name= <<"homeopathic potency of quintamillesimal series (retired)">>,
                        property= <<"homeopathic potency (retired)">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= true,
                        class= <<"clinical">>,
                        unit= <<"hpQ(1 1)">>,
                        value= <<"">>
                       }},

  exponent(String, S);

unit(<<"[hp'_M]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[hp'_M]">>,
                        name= <<"homeopathic potency of millesimal series (retired)">>,
                        property= <<"homeopathic potency (retired)">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= true,
                        class= <<"clinical">>,
                        unit= <<"hpM(1 1)">>,
                        value= <<"">>
                       }},

  exponent(String, S);

unit(<<"[hp'_C]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[hp'_C]">>,
                        name= <<"homeopathic potency of centesimal series (retired)">>,
                        property= <<"homeopathic potency (retired)">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= true,
                        class= <<"clinical">>,
                        unit= <<"hpC(1 1)">>,
                        value= <<"">>
                       }},

  exponent(String, S);

unit(<<"[hp'_X]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[hp'_X]">>,
                        name= <<"homeopathic potency of decimal series (retired)">>,
                        property= <<"homeopathic potency (retired)">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= true,
                        class= <<"clinical">>,
                        unit= <<"hpX(1 1)">>,
                        value= <<"">>
                       }},

  exponent(String, S);

unit(<<"[Btu_m]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[Btu_m]">>,
                        name= <<"mean British thermal unit">>,
                        property= <<"energy">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"heat">>,
                        unit= <<"kJ">>,
                        value= <<"1.05587">>
                       }},

  exponent(String, S);

unit(<<"[degRe]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[degRe]">>,
                        name= <<"degree Réaumur">>,
                        property= <<"temperature">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= true,
                        class= <<"heat">>,
                        unit= <<"degre(5 K/4)">>,
                        value= <<"">>
                       }},

  exponent(String, S);

unit(<<"[didot]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[didot]">>,
                        name= <<"Didot's pointdidot">>,
                        property= <<"length">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"typeset">>,
                        unit= <<"[ligne]/6">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[ligne]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[ligne]">>,
                        name= <<"French lineligne">>,
                        property= <<"length">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"typeset">>,
                        unit= <<"[pouce]/12">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[pouce]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[pouce]">>,
                        name= <<"French inchpouce">>,
                        property= <<"length">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"typeset">>,
                        unit= <<"[pied]/12">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[lb_ap]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[lb_ap]">>,
                        name= <<"pound">>,
                        property= <<"mass">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"apoth">>,
                        unit= <<"[oz_ap]">>,
                        value= <<"12">>
                       }},

  exponent(String, S);

unit(<<"[oz_ap]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[oz_ap]">>,
                        name= <<"ounce">>,
                        property= <<"mass">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"apoth">>,
                        unit= <<"[dr_ap]">>,
                        value= <<"8">>
                       }},

  exponent(String, S);

unit(<<"[dr_ap]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[dr_ap]">>,
                        name= <<"drachmdram">>,
                        property= <<"mass">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"apoth">>,
                        unit= <<"[sc_ap]">>,
                        value= <<"3">>
                       }},

  exponent(String, S);

unit(<<"[sc_ap]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[sc_ap]">>,
                        name= <<"scruple">>,
                        property= <<"mass">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"apoth">>,
                        unit= <<"[gr]">>,
                        value= <<"20">>
                       }},

  exponent(String, S);

unit(<<"[lb_tr]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[lb_tr]">>,
                        name= <<"pound">>,
                        property= <<"mass">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"troy">>,
                        unit= <<"[oz_tr]">>,
                        value= <<"12">>
                       }},

  exponent(String, S);

unit(<<"[oz_tr]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[oz_tr]">>,
                        name= <<"ounce">>,
                        property= <<"mass">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"troy">>,
                        unit= <<"[pwt_tr]">>,
                        value= <<"20">>
                       }},

  exponent(String, S);

unit(<<"[dr_av]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[dr_av]">>,
                        name= <<"dram">>,
                        property= <<"mass">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"avoirdupois">>,
                        unit= <<"[oz_av]/16">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[oz_av]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[oz_av]">>,
                        name= <<"ounce">>,
                        property= <<"mass">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"avoirdupois">>,
                        unit= <<"[lb_av]/16">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[lb_av]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[lb_av]">>,
                        name= <<"pound">>,
                        property= <<"mass">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"avoirdupois">>,
                        unit= <<"[gr]">>,
                        value= <<"7000">>
                       }},

  exponent(String, S);

unit(<<"[pt_br]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[pt_br]">>,
                        name= <<"pint">>,
                        property= <<"volume">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"brit-volumes">>,
                        unit= <<"[qt_br]/2">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[qt_br]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[qt_br]">>,
                        name= <<"quart">>,
                        property= <<"volume">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"brit-volumes">>,
                        unit= <<"[gal_br]/4">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[bu_br]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[bu_br]">>,
                        name= <<"bushel">>,
                        property= <<"volume">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"brit-volumes">>,
                        unit= <<"[pk_br]">>,
                        value= <<"4">>
                       }},

  exponent(String, S);

unit(<<"[pk_br]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[pk_br]">>,
                        name= <<"peck">>,
                        property= <<"volume">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"brit-volumes">>,
                        unit= <<"[gal_br]">>,
                        value= <<"2">>
                       }},

  exponent(String, S);

unit(<<"[tbs_m]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[tbs_m]">>,
                        name= <<"metric tablespoon">>,
                        property= <<"volume">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"us-volumes">>,
                        unit= <<"mL">>,
                        value= <<"15">>
                       }},

  exponent(String, S);

unit(<<"[tsp_m]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[tsp_m]">>,
                        name= <<"metric teaspoon">>,
                        property= <<"volume">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"us-volumes">>,
                        unit= <<"mL">>,
                        value= <<"5">>
                       }},

  exponent(String, S);

unit(<<"[cup_m]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[cup_m]">>,
                        name= <<"metric cup">>,
                        property= <<"volume">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"us-volumes">>,
                        unit= <<"mL">>,
                        value= <<"240">>
                       }},

  exponent(String, S);

unit(<<"[foz_m]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[foz_m]">>,
                        name= <<"metric fluid ounce">>,
                        property= <<"fluid volume">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"us-volumes">>,
                        unit= <<"mL">>,
                        value= <<"30">>
                       }},

  exponent(String, S);

unit(<<"[pk_us]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[pk_us]">>,
                        name= <<"peck">>,
                        property= <<"dry volume">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"us-volumes">>,
                        unit= <<"[bu_us]/4">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[bu_us]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[bu_us]">>,
                        name= <<"bushel">>,
                        property= <<"dry volume">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"us-volumes">>,
                        unit= <<"[in_i]3">>,
                        value= <<"2150.42">>
                       }},

  exponent(String, S);

unit(<<"[pt_us]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[pt_us]">>,
                        name= <<"pint">>,
                        property= <<"fluid volume">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"us-volumes">>,
                        unit= <<"[qt_us]/2">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[qt_us]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[qt_us]">>,
                        name= <<"quart">>,
                        property= <<"fluid volume">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"us-volumes">>,
                        unit= <<"[gal_us]/4">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[kn_br]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[kn_br]">>,
                        name= <<"knot">>,
                        property= <<"velocity">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"brit-length">>,
                        unit= <<"[nmi_br]/h">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[mi_br]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[mi_br]">>,
                        name= <<"mile">>,
                        property= <<"length">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"brit-length">>,
                        unit= <<"[ft_br]">>,
                        value= <<"5280">>
                       }},

  exponent(String, S);

unit(<<"[yd_br]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[yd_br]">>,
                        name= <<"yard">>,
                        property= <<"length">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"brit-length">>,
                        unit= <<"[ft_br]">>,
                        value= <<"3">>
                       }},

  exponent(String, S);

unit(<<"[pc_br]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[pc_br]">>,
                        name= <<"pace">>,
                        property= <<"length">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"brit-length">>,
                        unit= <<"[ft_br]">>,
                        value= <<"2.5">>
                       }},

  exponent(String, S);

unit(<<"[lk_br]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[lk_br]">>,
                        name= <<"link for Gunter's chain">>,
                        property= <<"length">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"brit-length">>,
                        unit= <<"[ch_br]/100">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[ch_br]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[ch_br]">>,
                        name= <<"Gunter's chain">>,
                        property= <<"length">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"brit-length">>,
                        unit= <<"[rd_br]">>,
                        value= <<"4">>
                       }},

  exponent(String, S);

unit(<<"[rd_br]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[rd_br]">>,
                        name= <<"rod">>,
                        property= <<"length">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"brit-length">>,
                        unit= <<"[ft_br]">>,
                        value= <<"16.5">>
                       }},

  exponent(String, S);

unit(<<"[ft_br]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[ft_br]">>,
                        name= <<"foot">>,
                        property= <<"length">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"brit-length">>,
                        unit= <<"[in_br]">>,
                        value= <<"12">>
                       }},

  exponent(String, S);

unit(<<"[in_br]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[in_br]">>,
                        name= <<"inch">>,
                        property= <<"length">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"brit-length">>,
                        unit= <<"cm">>,
                        value= <<"2.539998">>
                       }},

  exponent(String, S);

unit(<<"[mi_us]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[mi_us]">>,
                        name= <<"mile">>,
                        property= <<"length">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"us-lengths">>,
                        unit= <<"[fur_us]">>,
                        value= <<"8">>
                       }},

  exponent(String, S);

unit(<<"[lk_us]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[lk_us]">>,
                        name= <<"link for Gunter's chain">>,
                        property= <<"length">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"us-lengths">>,
                        unit= <<"[ch_us]/100">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[ch_us]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[ch_us]">>,
                        name= <<"Surveyor's chainGunter's chain">>,
                        property= <<"length">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"us-lengths">>,
                        unit= <<"[rd_us]">>,
                        value= <<"4">>
                       }},

  exponent(String, S);

unit(<<"[rd_us]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[rd_us]">>,
                        name= <<"rod">>,
                        property= <<"length">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"us-lengths">>,
                        unit= <<"[ft_us]">>,
                        value= <<"16.5">>
                       }},

  exponent(String, S);

unit(<<"[in_us]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[in_us]">>,
                        name= <<"inch">>,
                        property= <<"length">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"us-lengths">>,
                        unit= <<"[ft_us]/12">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[yd_us]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[yd_us]">>,
                        name= <<"yard">>,
                        property= <<"length">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"us-lengths">>,
                        unit= <<"[ft_us]">>,
                        value= <<"3">>
                       }},

  exponent(String, S);

unit(<<"[ft_us]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[ft_us]">>,
                        name= <<"foot">>,
                        property= <<"length">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"us-lengths">>,
                        unit= <<"m/3937">>,
                        value= <<"1200">>
                       }},

  exponent(String, S);

unit(<<"[cml_i]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[cml_i]">>,
                        name= <<"circular mil">>,
                        property= <<"area">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"intcust">>,
                        unit= <<"[pi]/4.[mil_i]2">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[mil_i]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[mil_i]">>,
                        name= <<"mil">>,
                        property= <<"length">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"intcust">>,
                        unit= <<"[in_i]">>,
                        value= <<"1e-3">>
                       }},

  exponent(String, S);

unit(<<"[cyd_i]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[cyd_i]">>,
                        name= <<"cubic yard">>,
                        property= <<"volume">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"intcust">>,
                        unit= <<"[yd_i]3">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[cft_i]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[cft_i]">>,
                        name= <<"cubic foot">>,
                        property= <<"volume">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"intcust">>,
                        unit= <<"[ft_i]3">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[cin_i]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[cin_i]">>,
                        name= <<"cubic inch">>,
                        property= <<"volume">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"intcust">>,
                        unit= <<"[in_i]3">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[syd_i]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[syd_i]">>,
                        name= <<"square yard">>,
                        property= <<"area">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"intcust">>,
                        unit= <<"[yd_i]2">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[sft_i]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[sft_i]">>,
                        name= <<"square foot">>,
                        property= <<"area">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"intcust">>,
                        unit= <<"[ft_i]2">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[sin_i]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[sin_i]">>,
                        name= <<"square inch">>,
                        property= <<"area">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"intcust">>,
                        unit= <<"[in_i]2">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[nmi_i]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[nmi_i]">>,
                        name= <<"nautical mile">>,
                        property= <<"length">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"intcust">>,
                        unit= <<"m">>,
                        value= <<"1852">>
                       }},

  exponent(String, S);

unit(<<"[fth_i]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[fth_i]">>,
                        name= <<"fathom">>,
                        property= <<"depth of water">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"intcust">>,
                        unit= <<"[ft_i]">>,
                        value= <<"6">>
                       }},

  exponent(String, S);

unit(<<"[eps_0]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[eps_0]">>,
                        name= <<"permittivity of vacuum">>,
                        property= <<"electric permittivity">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"const">>,
                        unit= <<"F/m">>,
                        value= <<"8.854187817e-12">>
                       }},

  exponent(String, S);

unit(<<"B[SPL]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"B[SPL]">>,
                        name= <<"bel sound pressure">>,
                        property= <<"pressure level">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= true,
                        class= <<"levels">>,
                        unit= <<"2lg(2 10*-5.Pa)">>,
                        value= <<"">>
                       }},

  exponent(String, S);

unit(<<"[tb'U]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[tb'U]">>,
                        name= <<"tuberculin unit">>,
                        property= <<"biologic activity of tuberculin">>,
                        is_metric= true,
                        is_arbitrary= true,
                        is_special= false,
                        class= <<"chemical">>,
                        unit= <<"1">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[ka'U]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[ka'U]">>,
                        name= <<"King-Armstrong unit">>,
                        property= <<"biologic activity of phosphatase">>,
                        is_metric= true,
                        is_arbitrary= true,
                        is_special= false,
                        class= <<"chemical">>,
                        unit= <<"1">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[kp_Q]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[kp_Q]">>,
                        name= <<"homeopathic potency of quintamillesimal korsakovian series">>,
                        property= <<"homeopathic potency (Korsakov)">>,
                        is_metric= true,
                        is_arbitrary= true,
                        is_special= false,
                        class= <<"clinical">>,
                        unit= <<"1">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[kp_M]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[kp_M]">>,
                        name= <<"homeopathic potency of millesimal korsakovian series">>,
                        property= <<"homeopathic potency (Korsakov)">>,
                        is_metric= true,
                        is_arbitrary= true,
                        is_special= false,
                        class= <<"clinical">>,
                        unit= <<"1">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[kp_C]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[kp_C]">>,
                        name= <<"homeopathic potency of centesimal korsakovian series">>,
                        property= <<"homeopathic potency (Korsakov)">>,
                        is_metric= true,
                        is_arbitrary= true,
                        is_special= false,
                        class= <<"clinical">>,
                        unit= <<"1">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[kp_X]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[kp_X]">>,
                        name= <<"homeopathic potency of decimal korsakovian series">>,
                        property= <<"homeopathic potency (Korsakov)">>,
                        is_metric= true,
                        is_arbitrary= true,
                        is_special= false,
                        class= <<"clinical">>,
                        unit= <<"1">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[hp_Q]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[hp_Q]">>,
                        name= <<"homeopathic potency of quintamillesimal hahnemannian series">>,
                        property= <<"homeopathic potency (Hahnemann)">>,
                        is_metric= true,
                        is_arbitrary= true,
                        is_special= false,
                        class= <<"clinical">>,
                        unit= <<"1">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[hp_M]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[hp_M]">>,
                        name= <<"homeopathic potency of millesimal hahnemannian series">>,
                        property= <<"homeopathic potency (Hahnemann)">>,
                        is_metric= true,
                        is_arbitrary= true,
                        is_special= false,
                        class= <<"clinical">>,
                        unit= <<"1">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[hp_C]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[hp_C]">>,
                        name= <<"homeopathic potency of centesimal hahnemannian series">>,
                        property= <<"homeopathic potency (Hahnemann)">>,
                        is_metric= true,
                        is_arbitrary= true,
                        is_special= false,
                        class= <<"clinical">>,
                        unit= <<"1">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[hp_X]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[hp_X]">>,
                        name= <<"homeopathic potency of decimal hahnemannian series">>,
                        property= <<"homeopathic potency (Hahnemann)">>,
                        is_metric= true,
                        is_arbitrary= true,
                        is_special= false,
                        class= <<"clinical">>,
                        unit= <<"1">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[diop]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[diop]">>,
                        name= <<"diopter">>,
                        property= <<"refraction of a lens">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"clinical">>,
                        unit= <<"/m">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"m[H2O]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"m[H2O]">>,
                        name= <<"meter of water column">>,
                        property= <<"pressure">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"clinical">>,
                        unit= <<"kPa">>,
                        value= <<"980665e-5">>
                       }},

  exponent(String, S);

unit(<<"cal_th", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"cal_th">>,
                        name= <<"thermochemical calorie">>,
                        property= <<"energy">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"heat">>,
                        unit= <<"J">>,
                        value= <<"4.184">>
                       }},

  exponent(String, S);

unit(<<"cal_IT", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"cal_IT">>,
                        name= <<"international table calorie">>,
                        property= <<"energy">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"heat">>,
                        unit= <<"J">>,
                        value= <<"4.1868">>
                       }},

  exponent(String, S);

unit(<<"[degR]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[degR]">>,
                        name= <<"degree Rankine">>,
                        property= <<"temperature">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"heat">>,
                        unit= <<"K/9">>,
                        value= <<"5">>
                       }},

  exponent(String, S);

unit(<<"[degF]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[degF]">>,
                        name= <<"degree Fahrenheit">>,
                        property= <<"temperature">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= true,
                        class= <<"heat">>,
                        unit= <<"degf(5 K/9)">>,
                        value= <<"">>
                       }},

  exponent(String, S);

unit(<<"[pied]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[pied]">>,
                        name= <<"French footpied">>,
                        property= <<"length">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"typeset">>,
                        unit= <<"cm">>,
                        value= <<"32.48">>
                       }},

  exponent(String, S);

unit(<<"[oz_m]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[oz_m]">>,
                        name= <<"metric ounce">>,
                        property= <<"mass">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"apoth">>,
                        unit= <<"g">>,
                        value= <<"28">>
                       }},

  exponent(String, S);

unit(<<"[hd_i]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[hd_i]">>,
                        name= <<"hand">>,
                        property= <<"height of horses">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"intcust">>,
                        unit= <<"[in_i]">>,
                        value= <<"4">>
                       }},

  exponent(String, S);

unit(<<"[cr_i]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[cr_i]">>,
                        name= <<"cord">>,
                        property= <<"volume">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"intcust">>,
                        unit= <<"[ft_i]3">>,
                        value= <<"128">>
                       }},

  exponent(String, S);

unit(<<"[bf_i]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[bf_i]">>,
                        name= <<"board foot">>,
                        property= <<"volume">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"intcust">>,
                        unit= <<"[in_i]3">>,
                        value= <<"144">>
                       }},

  exponent(String, S);

unit(<<"[kn_i]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[kn_i]">>,
                        name= <<"knot">>,
                        property= <<"velocity">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"intcust">>,
                        unit= <<"[nmi_i]/h">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[mi_i]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[mi_i]">>,
                        name= <<"mile">>,
                        property= <<"length">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"intcust">>,
                        unit= <<"[ft_i]">>,
                        value= <<"5280">>
                       }},

  exponent(String, S);

unit(<<"[yd_i]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[yd_i]">>,
                        name= <<"yard">>,
                        property= <<"length">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"intcust">>,
                        unit= <<"[ft_i]">>,
                        value= <<"3">>
                       }},

  exponent(String, S);

unit(<<"[ft_i]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[ft_i]">>,
                        name= <<"foot">>,
                        property= <<"length">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"intcust">>,
                        unit= <<"[in_i]">>,
                        value= <<"12">>
                       }},

  exponent(String, S);

unit(<<"[in_i]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[in_i]">>,
                        name= <<"inch">>,
                        property= <<"length">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"intcust">>,
                        unit= <<"cm">>,
                        value= <<"254e-2">>
                       }},

  exponent(String, S);

unit(<<"[mu_0]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[mu_0]">>,
                        name= <<"permeability of vacuum">>,
                        property= <<"magnetic permeability">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"const">>,
                        unit= <<"4.[pi].10*-7.N/A2">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[pptr]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[pptr]">>,
                        name= <<"parts per trillion">>,
                        property= <<"fraction">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"dimless">>,
                        unit= <<"10*-12">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[ppth]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[ppth]">>,
                        name= <<"parts per thousand">>,
                        property= <<"fraction">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"dimless">>,
                        unit= <<"10*-3">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"bit_s", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"bit_s">>,
                        name= <<"bit">>,
                        property= <<"amount of information">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= true,
                        class= <<"infotech">>,
                        unit= <<"ld(1 1)">>,
                        value= <<"">>
                       }},

  exponent(String, S);

unit(<<"[psi]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[psi]">>,
                        name= <<"pound per sqare inch">>,
                        property= <<"pressure">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"misc">>,
                        unit= <<"[lbf_av]/[in_i]2">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"B[kW]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"B[kW]">>,
                        name= <<"bel kilowatt">>,
                        property= <<"power level">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= true,
                        class= <<"levels">>,
                        unit= <<"lg(1 kW)">>,
                        value= <<"">>
                       }},

  exponent(String, S);

unit(<<"B[uV]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"B[uV]">>,
                        name= <<"bel microvolt">>,
                        property= <<"electric potential level">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= true,
                        class= <<"levels">>,
                        unit= <<"2lg(1 uV)">>,
                        value= <<"">>
                       }},

  exponent(String, S);

unit(<<"B[mV]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"B[mV]">>,
                        name= <<"bel millivolt">>,
                        property= <<"electric potential level">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= true,
                        class= <<"levels">>,
                        unit= <<"2lg(1 mV)">>,
                        value= <<"">>
                       }},

  exponent(String, S);

unit(<<"[ELU]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[ELU]">>,
                        name= <<"ELISA unit">>,
                        property= <<"arbitrary ELISA unit">>,
                        is_metric= true,
                        is_arbitrary= true,
                        is_special= false,
                        class= <<"chemical">>,
                        unit= <<"1">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[FEU]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[FEU]">>,
                        name= <<"fibrinogen equivalent unit">>,
                        property= <<"amount of fibrinogen broken down into the measured d-dimers">>,
                        is_metric= true,
                        is_arbitrary= true,
                        is_special= false,
                        class= <<"chemical">>,
                        unit= <<"1">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[PNU]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[PNU]">>,
                        name= <<"protein nitrogen unit">>,
                        property= <<"procedure defined amount of a protein substance">>,
                        is_metric= true,
                        is_arbitrary= true,
                        is_special= false,
                        class= <<"chemical">>,
                        unit= <<"1">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[BAU]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[BAU]">>,
                        name= <<"bioequivalent allergen unit">>,
                        property= <<"amount of an allergen callibrated through in-vivo testing based on the ID50EAL method of (intradermal dilution for 50mm sum of erythema diameters">>,
                        is_metric= true,
                        is_arbitrary= true,
                        is_special= false,
                        class= <<"chemical">>,
                        unit= <<"1">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[CFU]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[CFU]">>,
                        name= <<"colony forming units">>,
                        property= <<"amount of a proliferating organism">>,
                        is_metric= true,
                        is_arbitrary= true,
                        is_special= false,
                        class= <<"chemical">>,
                        unit= <<"1">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[FFU]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[FFU]">>,
                        name= <<"focus forming units">>,
                        property= <<"amount of an infectious agent">>,
                        is_metric= true,
                        is_arbitrary= true,
                        is_special= false,
                        class= <<"chemical">>,
                        unit= <<"1">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[PFU]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[PFU]">>,
                        name= <<"plaque forming units">>,
                        property= <<"amount of an infectious agent">>,
                        is_metric= true,
                        is_arbitrary= true,
                        is_special= false,
                        class= <<"chemical">>,
                        unit= <<"1">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[LPF]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[LPF]">>,
                        name= <<"low power field">>,
                        property= <<"view area in microscope">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"chemical">>,
                        unit= <<"1">>,
                        value= <<"100">>
                       }},

  exponent(String, S);

unit(<<"[HPF]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[HPF]">>,
                        name= <<"high power field">>,
                        property= <<"view area in microscope">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"chemical">>,
                        unit= <<"1">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[MET]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[MET]">>,
                        name= <<"metabolic equivalent">>,
                        property= <<"metabolic cost of physical activity">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"clinical">>,
                        unit= <<"mL/min/kg">>,
                        value= <<"3.5">>
                       }},

  exponent(String, S);

unit(<<"[drp]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[drp]">>,
                        name= <<"drop">>,
                        property= <<"volume">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"clinical">>,
                        unit= <<"ml/20">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[PRU]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[PRU]">>,
                        name= <<"peripheral vascular resistance unit">>,
                        property= <<"fluid resistance">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"clinical">>,
                        unit= <<"mm[Hg].s/ml">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"m[Hg]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"m[Hg]">>,
                        name= <<"meter of mercury column">>,
                        property= <<"pressure">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"clinical">>,
                        unit= <<"kPa">>,
                        value= <<"133.3220">>
                       }},

  exponent(String, S);

unit(<<"[den]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[den]">>,
                        name= <<"Denier">>,
                        property= <<"linear mass density (of textile thread)">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"heat">>,
                        unit= <<"g/9/km">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[Btu]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[Btu]">>,
                        name= <<"British thermal unit">>,
                        property= <<"energy">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"heat">>,
                        unit= <<"[Btu_th]">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[Cal]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[Cal]">>,
                        name= <<"nutrition label Calories">>,
                        property= <<"energy">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"heat">>,
                        unit= <<"kcal_th">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"cal_m", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"cal_m">>,
                        name= <<"mean calorie">>,
                        property= <<"energy">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"heat">>,
                        unit= <<"J">>,
                        value= <<"4.19002">>
                       }},

  exponent(String, S);

unit(<<"[pca]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[pca]">>,
                        name= <<"pica">>,
                        property= <<"length">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"typeset">>,
                        unit= <<"[pnt]">>,
                        value= <<"12">>
                       }},

  exponent(String, S);

unit(<<"[pnt]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[pnt]">>,
                        name= <<"point">>,
                        property= <<"length">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"typeset">>,
                        unit= <<"[lne]/6">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[lne]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[lne]">>,
                        name= <<"line">>,
                        property= <<"length">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"typeset">>,
                        unit= <<"[in_i]/12">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[twp]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[twp]">>,
                        name= <<"township">>,
                        property= <<"area">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"us-lengths">>,
                        unit= <<"[sct]">>,
                        value= <<"36">>
                       }},

  exponent(String, S);

unit(<<"[sct]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[sct]">>,
                        name= <<"section">>,
                        property= <<"area">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"us-lengths">>,
                        unit= <<"[mi_us]2">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[m_p]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[m_p]">>,
                        name= <<"proton mass">>,
                        property= <<"mass">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"const">>,
                        unit= <<"g">>,
                        value= <<"1.6726231e-24">>
                       }},

  exponent(String, S);

unit(<<"[m_e]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[m_e]">>,
                        name= <<"electron mass">>,
                        property= <<"mass">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"const">>,
                        unit= <<"g">>,
                        value= <<"9.1093897e-28">>
                       }},

  exponent(String, S);

unit(<<"[ppb]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[ppb]">>,
                        name= <<"parts per billion">>,
                        property= <<"fraction">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"dimless">>,
                        unit= <<"10*-9">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[ppm]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[ppm]">>,
                        name= <<"parts per million">>,
                        property= <<"fraction">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"dimless">>,
                        unit= <<"10*-6">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"circ", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"circ">>,
                        name= <<"circle">>,
                        property= <<"plane angle">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"misc">>,
                        unit= <<"[pi].rad">>,
                        value= <<"2">>
                       }},

  exponent(String, S);

unit(<<"B[W]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"B[W]">>,
                        name= <<"bel watt">>,
                        property= <<"power level">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= true,
                        class= <<"levels">>,
                        unit= <<"lg(1 W)">>,
                        value= <<"">>
                       }},

  exponent(String, S);

unit(<<"B[V]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"B[V]">>,
                        name= <<"bel volt">>,
                        property= <<"electric potential level">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= true,
                        class= <<"levels">>,
                        unit= <<"2lg(1 V)">>,
                        value= <<"">>
                       }},

  exponent(String, S);

unit(<<"[EU]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[EU]">>,
                        name= <<"Ehrlich unit">>,
                        property= <<"Ehrlich unit">>,
                        is_metric= true,
                        is_arbitrary= true,
                        is_special= false,
                        class= <<"chemical">>,
                        unit= <<"1">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[Lf]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[Lf]">>,
                        name= <<"Limit of flocculation">>,
                        property= <<"procedure defined amount of an antigen substance">>,
                        is_metric= true,
                        is_arbitrary= true,
                        is_special= false,
                        class= <<"chemical">>,
                        unit= <<"1">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[AU]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[AU]">>,
                        name= <<"allergen unit">>,
                        property= <<"procedure defined amount of an allergen using some reference standard">>,
                        is_metric= true,
                        is_arbitrary= true,
                        is_special= false,
                        class= <<"chemical">>,
                        unit= <<"1">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[IR]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[IR]">>,
                        name= <<"index of reactivity">>,
                        property= <<"amount of an allergen callibrated through in-vivo testing using the Stallergenes® method.">>,
                        is_metric= true,
                        is_arbitrary= true,
                        is_special= false,
                        class= <<"chemical">>,
                        unit= <<"1">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[IU]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[IU]">>,
                        name= <<"international unit">>,
                        property= <<"arbitrary">>,
                        is_metric= true,
                        is_arbitrary= true,
                        is_special= false,
                        class= <<"chemical">>,
                        unit= <<"[iU]">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[iU]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[iU]">>,
                        name= <<"international unit">>,
                        property= <<"arbitrary">>,
                        is_metric= true,
                        is_arbitrary= true,
                        is_special= false,
                        class= <<"chemical">>,
                        unit= <<"1">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[pH]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[pH]">>,
                        name= <<"pH">>,
                        property= <<"acidity">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= true,
                        class= <<"chemical">>,
                        unit= <<"pH(1 mol/l)">>,
                        value= <<"">>
                       }},

  exponent(String, S);

unit(<<"[Ch]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[Ch]">>,
                        name= <<"frenchCharrière">>,
                        property= <<"gauge of catheters">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"clinical">>,
                        unit= <<"mm/3">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[HP]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[HP]">>,
                        name= <<"horsepower">>,
                        property= <<"power">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"heat">>,
                        unit= <<"[ft_i].[lbf_av]/s">>,
                        value= <<"550">>
                       }},

  exponent(String, S);

unit(<<"[gr]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[gr]">>,
                        name= <<"grain">>,
                        property= <<"mass">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"avoirdupois">>,
                        unit= <<"mg">>,
                        value= <<"64.79891">>
                       }},

  exponent(String, S);

unit(<<"[ly]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[ly]">>,
                        name= <<"light-year">>,
                        property= <<"length">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"const">>,
                        unit= <<"[c].a_j">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"mo_g", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"mo_g">>,
                        name= <<"mean Gregorian month">>,
                        property= <<"time">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"iso1000">>,
                        unit= <<"a_g/12">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"mo_j", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"mo_j">>,
                        name= <<"mean Julian month">>,
                        property= <<"time">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"iso1000">>,
                        unit= <<"a_j/12">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"mo_s", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"mo_s">>,
                        name= <<"synodal month">>,
                        property= <<"time">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"iso1000">>,
                        unit= <<"d">>,
                        value= <<"29.53059">>
                       }},

  exponent(String, S);

unit(<<"[pi]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[pi]">>,
                        name= <<"the number pi">>,
                        property= <<"number">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"dimless">>,
                        unit= <<"1">>,
                        value= <<"3.1415926535897932384626433832795028841971693993751058209749445923">>
                       }},

  exponent(String, S);

unit(<<"bit", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"bit">>,
                        name= <<"bit">>,
                        property= <<"amount of information">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"infotech">>,
                        unit= <<"1">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"sph", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"sph">>,
                        name= <<"spere">>,
                        property= <<"solid angle">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"misc">>,
                        unit= <<"[pi].sr">>,
                        value= <<"4">>
                       }},

  exponent(String, S);

unit(<<"mho", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"mho">>,
                        name= <<"mho">>,
                        property= <<"electric conductance">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"misc">>,
                        unit= <<"S">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"att", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"att">>,
                        name= <<"technical atmosphere">>,
                        property= <<"pressure">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"misc">>,
                        unit= <<"kgf/cm2">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"kat", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"kat">>,
                        name= <<"katal">>,
                        property= <<"catalytic activity">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"chemical">>,
                        unit= <<"mol/s">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"[S]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[S]">>,
                        name= <<"Svedberg unit">>,
                        property= <<"sedimentation coefficient">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"chemical">>,
                        unit= <<"10*-13.s">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"osm", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"osm">>,
                        name= <<"osmole">>,
                        property= <<"amount of substance (dissolved particles)">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"chemical">>,
                        unit= <<"mol">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"tex", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"tex">>,
                        name= <<"tex">>,
                        property= <<"linear mass density (of textile thread)">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"heat">>,
                        unit= <<"g/km">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"cal", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"cal">>,
                        name= <<"calorie">>,
                        property= <<"energy">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"heat">>,
                        unit= <<"cal_th">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"REM", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"REM">>,
                        name= <<"radiation equivalent man">>,
                        property= <<"dose equivalent">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"cgs">>,
                        unit= <<"RAD">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"RAD", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"RAD">>,
                        name= <<"radiation absorbed dose">>,
                        property= <<"energy dose">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"cgs">>,
                        unit= <<"erg/g">>,
                        value= <<"100">>
                       }},

  exponent(String, S);

unit(<<"req", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"req">>,
                        name= <<"request">>,
                        property= <<"number of requests">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"infotech">>,
                        unit= <<"1">>,
                        value= <<"1">>
                       }},
  
  exponent(String, S);

unit(<<"Lmb", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"Lmb">>,
                        name= <<"Lambert">>,
                        property= <<"brightness">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"cgs">>,
                        unit= <<"cd/cm2/[pi]">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"erg", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"erg">>,
                        name= <<"erg">>,
                        property= <<"energy">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"cgs">>,
                        unit= <<"dyn.cm">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"dyn", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"dyn">>,
                        name= <<"dyne">>,
                        property= <<"force">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"cgs">>,
                        unit= <<"g.cm/s2">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"Gal", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"Gal">>,
                        name= <<"Gal">>,
                        property= <<"acceleration">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"cgs">>,
                        unit= <<"cm/s2">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"atm", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"atm">>,
                        name= <<"standard atmosphere">>,
                        property= <<"pressure">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"const">>,
                        unit= <<"Pa">>,
                        value= <<"101325">>
                       }},

  exponent(String, S);

unit(<<"[g]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[g]">>,
                        name= <<"standard acceleration of free fall">>,
                        property= <<"acceleration">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"const">>,
                        unit= <<"m/s2">>,
                        value= <<"980665e-5">>
                       }},

  exponent(String, S);

unit(<<"[G]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[G]">>,
                        name= <<"Newtonian constant of gravitation">>,
                        property= <<"(unclassified)">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"const">>,
                        unit= <<"m3.kg-1.s-2">>,
                        value= <<"6.67259e-11">>
                       }},

  exponent(String, S);

unit(<<"[e]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[e]">>,
                        name= <<"elementary charge">>,
                        property= <<"electric charge">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"const">>,
                        unit= <<"C">>,
                        value= <<"1.60217733e-19">>
                       }},

  exponent(String, S);

unit(<<"[k]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[k]">>,
                        name= <<"Boltzmann constant">>,
                        property= <<"(unclassified)">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"const">>,
                        unit= <<"J/K">>,
                        value= <<"1.380658e-23">>
                       }},

  exponent(String, S);

unit(<<"[h]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[h]">>,
                        name= <<"Planck constant">>,
                        property= <<"action">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"const">>,
                        unit= <<"J.s">>,
                        value= <<"6.6260755e-34">>
                       }},

  exponent(String, S);

unit(<<"[c]", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"[c]">>,
                        name= <<"velocity of light">>,
                        property= <<"velocity">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"const">>,
                        unit= <<"m/s">>,
                        value= <<"299792458">>
                       }},

  exponent(String, S);

unit(<<"bar", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"bar">>,
                        name= <<"bar">>,
                        property= <<"pressure">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"iso1000">>,
                        unit= <<"Pa">>,
                        value= <<"1e5">>
                       }},

  exponent(String, S);

unit(<<"a_g", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"a_g">>,
                        name= <<"mean Gregorian year">>,
                        property= <<"time">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"iso1000">>,
                        unit= <<"d">>,
                        value= <<"365.2425">>
                       }},

  exponent(String, S);

unit(<<"a_j", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"a_j">>,
                        name= <<"mean Julian year">>,
                        property= <<"time">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"iso1000">>,
                        unit= <<"d">>,
                        value= <<"365.25">>
                       }},

  exponent(String, S);

unit(<<"a_t", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"a_t">>,
                        name= <<"tropical year">>,
                        property= <<"time">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"iso1000">>,
                        unit= <<"d">>,
                        value= <<"365.24219">>
                       }},

  exponent(String, S);

unit(<<"min", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"min">>,
                        name= <<"minute">>,
                        property= <<"time">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"iso1000">>,
                        unit= <<"s">>,
                        value= <<"60">>
                       }},

  exponent(String, S);

unit(<<"deg", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"deg">>,
                        name= <<"degree">>,
                        property= <<"plane angle">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"iso1000">>,
                        unit= <<"[pi].rad/360">>,
                        value= <<"2">>
                       }},

  exponent(String, S);

unit(<<"gon", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"gon">>,
                        name= <<"gradegon">>,
                        property= <<"plane angle">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"iso1000">>,
                        unit= <<"deg">>,
                        value= <<"0.9">>
                       }},

  exponent(String, S);

unit(<<"Cel", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"Cel">>,
                        name= <<"degree Celsius">>,
                        property= <<"temperature">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= true,
                        class= <<"si">>,
                        unit= <<"cel(1 K)">>,
                        value= <<"">>
                       }},

  exponent(String, S);

unit(<<"Ohm", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"Ohm">>,
                        name= <<"ohm">>,
                        property= <<"electric resistance">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"si">>,
                        unit= <<"V/A">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"mol", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"mol">>,
                        name= <<"mole">>,
                        property= <<"amount of substance">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"si">>,
                        unit= <<"10*23">>,
                        value= <<"6.0221367">>
                       }},

  exponent(String, S);

unit(<<"10^", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"10^">>,
                        name= <<"the number ten for arbitrary powers">>,
                        property= <<"number">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"dimless">>,
                        unit= <<"1">>,
                        value= <<"10">>
                       }},

  exponent(String, S);

unit(<<"10*", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"10*">>,
                        name= <<"the number ten for arbitrary powers">>,
                        property= <<"number">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"dimless">>,
                        unit= <<"1">>,
                        value= <<"10">>
                       }},

  exponent(String, S);

unit(<<"Bd", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"Bd">>,
                        name= <<"baud">>,
                        property= <<"signal transmission rate">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"infotech">>,
                        unit= <<"/s">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"By", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"By">>,
                        name= <<"byte">>,
                        property= <<"amount of information">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"infotech">>,
                        unit= <<"bit">>,
                        value= <<"8">>
                       }},

  exponent(String, S);

unit(<<"Ao", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"Ao">>,
                        name= <<"Ångström">>,
                        property= <<"length">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"misc">>,
                        unit= <<"nm">>,
                        value= <<"0.1">>
                       }},

  exponent(String, S);

unit(<<"st", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"st">>,
                        name= <<"stere">>,
                        property= <<"volume">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"misc">>,
                        unit= <<"m3">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"Np", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"Np">>,
                        name= <<"neper">>,
                        property= <<"level">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= true,
                        class= <<"levels">>,
                        unit= <<"ln(1 1)">>,
                        value= <<"">>
                       }},

  exponent(String, S);

unit(<<"g%", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"g%">>,
                        name= <<"gram percent">>,
                        property= <<"mass concentration">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"chemical">>,
                        unit= <<"g/dl">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"eq", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"eq">>,
                        name= <<"equivalents">>,
                        property= <<"amount of substance">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"chemical">>,
                        unit= <<"mol">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"Ci", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"Ci">>,
                        name= <<"Curie">>,
                        property= <<"radioactivity">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"cgs">>,
                        unit= <<"Bq">>,
                        value= <<"37e9">>
                       }},

  exponent(String, S);

unit(<<"ph", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"ph">>,
                        name= <<"phot">>,
                        property= <<"illuminance">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"cgs">>,
                        unit= <<"lx">>,
                        value= <<"1e-4">>
                       }},

  exponent(String, S);

unit(<<"sb", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"sb">>,
                        name= <<"stilb">>,
                        property= <<"lum. intensity density">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"cgs">>,
                        unit= <<"cd/cm2">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"Gb", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"Gb">>,
                        name= <<"Gilbert">>,
                        property= <<"magnetic tension">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"cgs">>,
                        unit= <<"Oe.cm">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"Oe", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"Oe">>,
                        name= <<"Oersted">>,
                        property= <<"magnetic field intensity">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"cgs">>,
                        unit= <<"/[pi].A/m">>,
                        value= <<"250">>
                       }},

  exponent(String, S);

unit(<<"Mx", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"Mx">>,
                        name= <<"Maxwell">>,
                        property= <<"flux of magnetic induction">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"cgs">>,
                        unit= <<"Wb">>,
                        value= <<"1e-8">>
                       }},

  exponent(String, S);

unit(<<"St", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"St">>,
                        name= <<"Stokes">>,
                        property= <<"kinematic viscosity">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"cgs">>,
                        unit= <<"cm2/s">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"Bi", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"Bi">>,
                        name= <<"Biot">>,
                        property= <<"electric current">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"cgs">>,
                        unit= <<"A">>,
                        value= <<"10">>
                       }},

  exponent(String, S);

unit(<<"Ky", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"Ky">>,
                        name= <<"Kayser">>,
                        property= <<"lineic number">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"cgs">>,
                        unit= <<"cm-1">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"gf", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"gf">>,
                        name= <<"gram-force">>,
                        property= <<"force">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"const">>,
                        unit= <<"g.[g]">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"pc", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"pc">>,
                        name= <<"parsec">>,
                        property= <<"length">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"iso1000">>,
                        unit= <<"m">>,
                        value= <<"3.085678e16">>
                       }},

  exponent(String, S);

unit(<<"AU", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"AU">>,
                        name= <<"astronomic unit">>,
                        property= <<"length">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"iso1000">>,
                        unit= <<"Mm">>,
                        value= <<"149597.870691">>
                       }},

  exponent(String, S);

unit(<<"eV", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"eV">>,
                        name= <<"electronvolt">>,
                        property= <<"energy">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"iso1000">>,
                        unit= <<"[e].V">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"mo", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"mo">>,
                        name= <<"month">>,
                        property= <<"time">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"iso1000">>,
                        unit= <<"mo_j">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"wk", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"wk">>,
                        name= <<"week">>,
                        property= <<"time">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"iso1000">>,
                        unit= <<"d">>,
                        value= <<"7">>
                       }},

  exponent(String, S);

unit(<<"ar", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"ar">>,
                        name= <<"are">>,
                        property= <<"area">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"iso1000">>,
                        unit= <<"m2">>,
                        value= <<"100">>
                       }},

  exponent(String, S);

unit(<<"''", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"''">>,
                        name= <<"second">>,
                        property= <<"plane angle">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"iso1000">>,
                        unit= <<"'/60">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"Sv", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"Sv">>,
                        name= <<"sievert">>,
                        property= <<"dose equivalent">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"si">>,
                        unit= <<"J/kg">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"Gy", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"Gy">>,
                        name= <<"gray">>,
                        property= <<"energy dose">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"si">>,
                        unit= <<"J/kg">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"Bq", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"Bq">>,
                        name= <<"becquerel">>,
                        property= <<"radioactivity">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"si">>,
                        unit= <<"s-1">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"lx", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"lx">>,
                        name= <<"lux">>,
                        property= <<"illuminance">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"si">>,
                        unit= <<"lm/m2">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"lm", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"lm">>,
                        name= <<"lumen">>,
                        property= <<"luminous flux">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"si">>,
                        unit= <<"cd.sr">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"Wb", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"Wb">>,
                        name= <<"weber">>,
                        property= <<"magnetic flux">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"si">>,
                        unit= <<"V.s">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"Pa", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"Pa">>,
                        name= <<"pascal">>,
                        property= <<"pressure">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"si">>,
                        unit= <<"N/m2">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"Hz", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"Hz">>,
                        name= <<"hertz">>,
                        property= <<"frequency">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"si">>,
                        unit= <<"s-1">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"sr", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"sr">>,
                        name= <<"steradian">>,
                        property= <<"solid angle">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"si">>,
                        unit= <<"rad2">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"b", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"b">>,
                        name= <<"barn">>,
                        property= <<"action area">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"misc">>,
                        unit= <<"fm2">>,
                        value= <<"100">>
                       }},

  exponent(String, S);

unit(<<"B", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"B">>,
                        name= <<"bel">>,
                        property= <<"level">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= true,
                        class= <<"levels">>,
                        unit= <<"lg(1 1)">>,
                        value= <<"">>
                       }},

  exponent(String, S);

unit(<<"U", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"U">>,
                        name= <<"Unit">>,
                        property= <<"catalytic activity">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"chemical">>,
                        unit= <<"umol/min">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"R", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"R">>,
                        name= <<"Roentgen">>,
                        property= <<"ion dose">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"cgs">>,
                        unit= <<"C/kg">>,
                        value= <<"2.58e-4">>
                       }},

  exponent(String, S);

unit(<<"G", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"G">>,
                        name= <<"Gauss">>,
                        property= <<"magnetic flux density">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"cgs">>,
                        unit= <<"T">>,
                        value= <<"1e-4">>
                       }},

  exponent(String, S);

unit(<<"P", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"P">>,
                        name= <<"Poise">>,
                        property= <<"dynamic viscosity">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"cgs">>,
                        unit= <<"dyn.s/cm2">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"u", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"u">>,
                        name= <<"unified atomic mass unit">>,
                        property= <<"mass">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"iso1000">>,
                        unit= <<"g">>,
                        value= <<"1.6605402e-24">>
                       }},

  exponent(String, S);

unit(<<"t", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"t">>,
                        name= <<"tonne">>,
                        property= <<"mass">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"iso1000">>,
                        unit= <<"kg">>,
                        value= <<"1e3">>
                       }},

  exponent(String, S);

unit(<<"a", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"a">>,
                        name= <<"year">>,
                        property= <<"time">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"iso1000">>,
                        unit= <<"a_j">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"d", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"d">>,
                        name= <<"day">>,
                        property= <<"time">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"iso1000">>,
                        unit= <<"h">>,
                        value= <<"24">>
                       }},

  exponent(String, S);

unit(<<"h", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"h">>,
                        name= <<"hour">>,
                        property= <<"time">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"iso1000">>,
                        unit= <<"min">>,
                        value= <<"60">>
                       }},

  exponent(String, S);

unit(<<"L", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"L">>,
                        name= <<"liter">>,
                        property= <<"volume">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"iso1000">>,
                        unit= <<"l">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"l", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"l">>,
                        name= <<"liter">>,
                        property= <<"volume">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"iso1000">>,
                        unit= <<"dm3">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"'", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"'">>,
                        name= <<"minute">>,
                        property= <<"plane angle">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"iso1000">>,
                        unit= <<"deg/60">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"H", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"H">>,
                        name= <<"henry">>,
                        property= <<"inductance">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"si">>,
                        unit= <<"Wb/A">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"T", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"T">>,
                        name= <<"tesla">>,
                        property= <<"magnetic flux density">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"si">>,
                        unit= <<"Wb/m2">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"S", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"S">>,
                        name= <<"siemens">>,
                        property= <<"electric conductance">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"si">>,
                        unit= <<"Ohm-1">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"F", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"F">>,
                        name= <<"farad">>,
                        property= <<"electric capacitance">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"si">>,
                        unit= <<"C/V">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"V", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"V">>,
                        name= <<"volt">>,
                        property= <<"electric potential">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"si">>,
                        unit= <<"J/C">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"A", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"A">>,
                        name= <<"ampère">>,
                        property= <<"electric current">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"si">>,
                        unit= <<"C/s">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"W", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"W">>,
                        name= <<"watt">>,
                        property= <<"power">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"si">>,
                        unit= <<"J/s">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"J", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"J">>,
                        name= <<"joule">>,
                        property= <<"energy">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"si">>,
                        unit= <<"N.m">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"N", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"N">>,
                        name= <<"newton">>,
                        property= <<"force">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"si">>,
                        unit= <<"kg.m/s2">>,
                        value= <<"1">>
                       }},

  exponent(String, S);

unit(<<"%", String/binary>>, S0) ->
  S = S0#symbol{unit=#defined_unit{
                        code= <<"%">>,
                        name= <<"percent">>,
                        property= <<"fraction">>,
                        is_metric= true,
                        is_arbitrary= false,
                        is_special= false,
                        class= <<"dimless">>,
                        unit= <<"10*-2">>,
                        value= <<"1">>
                       }},

  exponent(String, S);


unit(<<"cd", String/binary>>, S0) ->
  S = S0#symbol{unit=#base_unit{
                        code= <<"cd">>,
                        name= <<"candela">>,
                        property= <<"luminous intensity">>,
                        dim= <<"F">>
                       }},

  exponent(String, S);

unit(<<"C", String/binary>>, S0) ->
  S = S0#symbol{unit=#base_unit{
                        code= <<"C">>,
                        name= <<"coulomb">>,
                        property= <<"electric charge">>,
                        dim= <<"Q">>
                       }},

  exponent(String, S);

unit(<<"K", String/binary>>, S0) ->
  S = S0#symbol{unit=#base_unit{
                        code= <<"K">>,
                        name= <<"kelvin">>,
                        property= <<"temperature">>,
                        dim= <<"C">>
                       }},

  exponent(String, S);

unit(<<"rad", String/binary>>, S0) ->
  S = S0#symbol{unit=#base_unit{
                        code= <<"rad">>,
                        name= <<"radian">>,
                        property= <<"plane angle">>,
                        dim= <<"A">>
                       }},

  exponent(String, S);

unit(<<"g", String/binary>>, S0) ->
  S = S0#symbol{unit=#base_unit{
                        code= <<"g">>,
                        name= <<"gram">>,
                        property= <<"mass">>,
                        dim= <<"M">>
                       }},

  exponent(String, S);

unit(<<"s", String/binary>>, S0) ->
  S = S0#symbol{unit=#base_unit{
                        code= <<"s">>,
                        name= <<"second">>,
                        property= <<"time">>,
                        dim= <<"T">>
                       }},

  exponent(String, S);

unit(<<"m", String/binary>>, S0) ->
  S = S0#symbol{unit=#base_unit{
                        code= <<"m">>,
                        name= <<"meter">>,
                        property= <<"length">>,
                        dim= <<"L">>
                       }},

  exponent(String, S);

unit(String, _S0) ->
  erlang:error({unknown_unit, String}).

exponent(String, S0) ->
  case maybe_number(String) of
    false -> {S0, String};
    {Number, Rest} -> {S0#symbol{exp=Number},
                       Rest}
  end.

maybe_number(String) ->
  case String of
    <<$+, Rest/binary>> ->
      number_digits(1, Rest);
    <<$-, Rest/binary>> ->
      number_digits(-1, Rest);
    _ ->
      maybe_number_digit(String)
  end.

number_digits(Sign, String) ->
  StringL = binary_to_list(String),
  case string:to_integer(StringL) of
    {error, _} -> erlang:error({invalid_symbol_exponent, Sign, String});
    {Int, RestL} -> {Sign * Int, list_to_binary(RestL)}
  end.

maybe_number_digit(<<$0, _/binary>>=String) ->
  number_digits(1, String);

maybe_number_digit(<<$1, _/binary>>=String) ->
  number_digits(1, String);

maybe_number_digit(<<$2, _/binary>>=String) ->
  number_digits(1, String);

maybe_number_digit(<<$3, _/binary>>=String) ->
  number_digits(1, String);

maybe_number_digit(<<$4, _/binary>>=String) ->
  number_digits(1, String);

maybe_number_digit(<<$5, _/binary>>=String) ->
  number_digits(1, String);

maybe_number_digit(<<$6, _/binary>>=String) ->
  number_digits(1, String);

maybe_number_digit(<<$7, _/binary>>=String) ->
  number_digits(1, String);

maybe_number_digit(<<$8, _/binary>>=String) ->
  number_digits(1, String);

maybe_number_digit(<<$9, _/binary>>=String) ->
  number_digits(1, String);

maybe_number_digit(_String) ->
  false.
