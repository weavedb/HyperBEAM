-module(dev_wao).
-export([ info/3, relay/3, compute/3, init/3, snapshot/3, normalize/3, cache_wasm_image/3 ]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

info(Msg, _, Opts) ->
    {ok, hb_ao:set(Msg, #{ <<"version">> => <<"1.0">> }, Opts)}.

relay(_Msg1, Msg2, Opts) ->
						% Extract the forwarding parameters
    Target = hb_ao:get(<<"forward-to">>, Msg2, undefined, Opts),
    Method = hb_ao:get(<<"forward-method">>, Msg2, <<"POST">>, Opts),
    Body = hb_ao:get(<<"forward-body">>, Msg2, <<>>, Opts),

    case Target of
        undefined ->
            {error, <<"Missing forward-to header">>};
        _ ->
						% Construct the relay message
            RelayMsg = #{
			 <<"path">> => <<"/~relay@1.0/call">>,
			 <<"method">> => <<"POST">>,
			 <<"relay-path">> => Target,
			 <<"relay-method">> => Method,
			 <<"relay-body">> => Body
			},

						% Forward to the relay device
            case hb_ao:resolve(RelayMsg, Opts) of
                {ok, Response} ->
                    {ok, Response};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

compute(Msg1, Msg2, Opts) ->
    case hb_ao:get([<<"body">>,<<"Action">>], Msg2, Opts) of
	Other ->
	    {ok, hb_ao:set( Msg1, #{  }, Opts )}
    end.

init(Msg, Msg2, Opts) -> 
    {ok, hb_ao:set(Msg, #{ }, Opts)}.


snapshot(Msg, _Msg2, _Opts) -> {ok, Msg}.

normalize(Msg, _Msg2, _Opts) -> {ok, Msg}.

cache_wasm_image(Msg1, _Msg2, Opts) ->
    Filename = hb_ao:get(<<"filename">>, Msg1, <<>>, Opts),
    case dev_wasm:cache_wasm_image(Filename, Opts) of
        #{ <<"image">> := ImageID } ->
            {ok, #{ <<"image">> => ImageID }};
        Error ->
            {ok, #{
		   <<"content-type">> => <<"application/json">>,
		   <<"body">> => dev_codec_json:to(#{
						     <<"error">> => <<"Failed to cache WASM image">>,
						     <<"details">> => iolist_to_binary(io_lib:format("~p", [Error]))
						    })
		  }}
    end.
