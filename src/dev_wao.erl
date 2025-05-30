-module(dev_wao).
-export([ info/3 ]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

info(Msg, _, Opts) ->
    {ok, hb_ao:set( Msg, #{ <<"version">> => <<"1.0">> }, Opts )}.
