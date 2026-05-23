-module(dev_double).
-export([ compute/3, init/3, snapshot/3, normalize/3 ]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

compute(Msg1, Msg2, Opts) ->
  %% v0.9-FINAL: see dev_inc:compute for the link-resolution rationale.
  Num = hb_ao:get(<<"num">>, Msg1, 0, Opts),
  {ok, hb_ao:set( Msg1, #{ <<"num">> => Num * 2 }, Opts )}.

init(Msg, Msg2, Opts) -> 
  {ok, hb_ao:set(Msg, #{ <<"num">> => 0 }, Opts)}.
 
snapshot(Msg, _Msg2, _Opts) -> {ok, Msg}.
 
normalize(Msg, _Msg2, _Opts) -> {ok, Msg}.
