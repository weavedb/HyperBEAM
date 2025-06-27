-module(dev_double).
-export([ info/3, compute/3, init/3, snapshot/3, normalize/3 ]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

info(Msg, _, Opts) ->
    {ok, hb_ao:set(Msg, #{ <<"version">> => <<"1.0">> }, Opts)}.

compute(Msg1, Msg2, Opts) -> 
    Count = hb_ao:get(<<"count">>, Msg1, 0, Opts) * 2,
    {ok, hb_ao:set( Msg1, #{ <<"count">> => Count }, Opts )}.

init(Msg, Msg2, Opts) -> {ok, Msg}.

snapshot(Msg, _Msg2, _Opts) -> {ok, Msg}.

normalize(Msg, _Msg2, _Opts) -> {ok, Msg}.
