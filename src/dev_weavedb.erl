-module(dev_weavedb).
-export([ compute/3, init/3, snapshot/3, normalize/3 , query/3 ]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

compute(Msg1, Msg2, Opts) ->
    case hb_ao:get([<<"body">>,<<"Action">>], Msg2, Opts) of
	<<"Query">> ->
	    Slot = hb_ao:get(<<"slot">>, Msg2, Opts),
	    ProcID = hb_ao:get(<<"process">>, Msg2, Opts),
	    {ok, AOS2 = #{ <<"body">> := Body }} =
		dev_scheduler_formats:assignments_to_aos2(
		  ProcID,
		  #{
		    Slot => Msg2
		   },
		  false,
		  Opts
		 ),
	    {ok, Res} = 
		hb_ao:resolve(
		  #{
		    <<"device">> => <<"relay@1.0">>,
		    <<"content-type">> => <<"application/json">>
		   },
		  AOS2#{
			<<"path">> => <<"call">>,
			<<"relay-method">> => <<"POST">>,
			<<"relay-body">> => Body,
			<<"relay-path">> =>
			    << "/result/", (hb_util:bin(Slot))/binary, "?process-id=", ProcID/binary >>,
			<<"content-type">> => <<"application/json">>
		       },
		  Opts#{
			hashpath => ignore,
			cache_control => [<<"no-store">>, <<"no-cache">>]
		       }
		 ),
	    ID = hb_ao:get(<<"db">>, Msg1, Opts),
	    ZKHash = hb_ao:get(<<"zkhash">>, Msg1, Opts),
	    Result = dev_codec_json:from(hb_ao:get(<<"body">>, Res, Opts)),
	    Data = hb_ao:get([<<"Output">>,<<"data">>], Result, Opts),
	    {ok, hb_ao:set( Msg1, #{ <<"db">> => ID, <<"zkhash">> => ZKHash, <<"results">> => #{ <<"data">> => Data } }, Opts )};
	Other ->
	    ID = hb_ao:get(<<"db">>, Msg1, Opts),
	    ZKHash = hb_ao:get([<<"body">>,<<"zkhash">>], Msg2, Opts),
	    {ok, hb_ao:set( Msg1, #{ <<"db">> => ID, <<"zkhash">> => ZKHash }, Opts )}
    end.

init(Msg, Msg2, Opts) -> 
    DB = hb_ao:get([<<"process">>,<<"db">>], Msg, Opts),    
    {ok, hb_ao:set(Msg, #{ <<"db">> => DB, <<"zkhash">> => "0" }, Opts)}.


snapshot(Msg, _Msg2, _Opts) -> {ok, Msg}.

normalize(Msg, _Msg2, _Opts) -> {ok, Msg}.

query(_M1, M2, _Opts) ->
    A = maps:get(<<"a">>, M2),
    B = maps:get(<<"b">>, M2),
    {ok, Sum} = dev_weavedb_nif:query(A, B),
    {ok, #{ <<"sum">> => Sum }}.

%%% Tests
resolve_add_test() ->
    M1 = #{ <<"device">> => <<"weavedb@1.0">> },
    M2 = #{ <<"path">> => <<"query">>, <<"a">> => 8, <<"b">> => 9 },
    {ok, #{ <<"sum">> := 17 }} = hb_ao:resolve(M1, M2, #{}).
