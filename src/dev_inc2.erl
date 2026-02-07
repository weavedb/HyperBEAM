-module(dev_inc2).
-export([ compute/3, init/3, snapshot/3, normalize/3 ]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").
 
compute(Msg1, Msg2, Opts) ->
  Num = maps:get(<<"num">>, Msg1) + 1,
  {ok, hb_ao:set( 
    Msg1,
    #{ 
      <<"num">> => Num,
      <<"results">> => #{ 
        <<"1">> => #{ 
          <<"method">> => <<"PATCH">>, 
          <<"double">> => Num * 2,
          <<"square">> => Num * Num 
        }
      } 
    }, 
    Opts
  )}.

init(Msg, Msg2, Opts) -> 
  {ok, hb_ao:set(Msg, #{ <<"num">> => 0 }, Opts)}.
 
snapshot(Msg, _Msg2, _Opts) -> {ok, Msg}.
 
normalize(Msg, _Msg2, _Opts) -> {ok, Msg}.
