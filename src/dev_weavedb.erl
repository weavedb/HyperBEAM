-module(dev_weavedb).
-export([ compute/3, init/3, snapshot/3 ]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

compute(Msg1, Msg2, Opts) ->
    case hb_ao:get([<<"body">>,<<"Action">>], Msg2, Opts) of
	<<"Query">> ->
	    case hb_ao:resolve(Msg1, {as, <<"delegated-compute@1.0">>, Msg2}, Opts) of
		{ok, Msg3} ->
		    {ok, Msg4} =
			hb_ao:resolve(
			  Msg3,
			  {
			   as,
			   <<"patch@1.0">>,
			   Msg2#{ <<"patch-from">> => <<"/results/outbox">> }
			  },
			  Opts
			 ),
		    {ok, Msg4};
		{error, Error} ->
		    {error, Error}
	    end;
	Other ->
	    ID = hb_ao:get(<<"db">>, Msg1, Opts),
	    ZKHash = hb_ao:get([<<"body">>,<<"zkhash">>], Msg2, Opts),
	    {ok, hb_ao:set( Msg1, #{ <<"db">> => ID, <<"zkhash">> => ZKHash }, Opts )}
    end.

init(Msg, Msg2, Opts) -> 
    DB = hb_ao:get([<<"process">>,<<"db">>], Msg, Opts),    
    {ok, hb_ao:set(Msg, #{ <<"db">> => DB, <<"zkhash">> => "0" }, Opts)}.

snapshot(_Msg1, _Msg2, _Opts) ->
    {ok, #{}}.
