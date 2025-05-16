-module(dev_weavedb).
-export([ compute/3, init/3, snapshot/3]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

compute(Msg1, Msg2, Opts) ->
    Count = hb_ao:get(<<"count">>, Msg1, Opts),
    Plus = hb_ao:get([<<"body">>,<<"plus">>], Msg2, Opts),
    {ok,
        hb_ao:set( Msg1, #{ <<"count">> => Count + binary_to_integer(Plus) }, Opts )
    }.

init(Msg, _Msg2, Opts) ->
    {ok, hb_ao:set(Msg, #{ <<"count">> => 0 }, Opts)}.

snapshot(_Msg1, _Msg2, _Opts) ->
    {ok, #{}}.

