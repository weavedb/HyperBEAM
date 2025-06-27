-module(dev_wao).
-export([ info/3, compute/3, init/3, snapshot/3, normalize/3 ]).
-export([ relay/3, cache_module/3, httpsig_to_json/3, balance/3, topup/3, cron/3 ]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

cron(Msg1, Msg2, Opts) ->
    Target = hb_ao:get(<<"target">>, Msg1, not_found, Opts),
    Wallet = hb_opts:get(priv_wallet, not_found, Opts),
    Msg = #{
        <<"device">> => <<"process@1.0">>,
        <<"path">> => <<"schedule">>,
        <<"target">> => Target,
        <<"method">> => <<"POST">>,
        <<"body">> => hb_message:commit( #{ }, Wallet )
	 },
    hb_ao:resolve( Msg, Opts ).

info(Msg, _, Opts) ->
    {ok, hb_ao:set(Msg, #{ <<"version">> => <<"1.0">> }, Opts)}.

relay(_Msg1, Msg2, Opts) ->
    Target = hb_ao:get(<<"forward-to">>, Msg2, undefined, Opts),
    Method = hb_ao:get(<<"forward-method">>, Msg2, <<"POST">>, Opts),
    Body = hb_ao:get(<<"forward-body">>, Msg2, <<>>, Opts),

    case Target of
        undefined ->
            {error, <<"Missing forward-to header">>};
        _ ->
            RelayMsg = #{
			 <<"path">> => <<"/~relay@1.0/call">>,
			 <<"method">> => <<"POST">>,
			 <<"relay-path">> => Target,
			 <<"relay-method">> => Method,
			 <<"relay-body">> => Body
			},

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
	    Count = hb_ao:get(<<"count">>, Msg1, 0, Opts) + 1,
	    {ok, hb_ao:set( Msg1, #{ <<"results">> => #{ <<"1">> => #{ <<"method">> => <<"PATCH">>, <<"square">> => Count * Count, <<"double">> => Count * 2 } }, <<"count">> => Count }, Opts )}
    end.

init(Msg, Msg2, Opts) -> 
    {ok, hb_ao:set(Msg, #{ <<"count">> => 0 }, Opts)}.

snapshot(Msg, _Msg2, _Opts) -> {ok, Msg}.

normalize(Msg, _Msg2, _Opts) -> {ok, Msg}.

cache_module(Msg1, _Msg2, Opts) ->
    Binary = hb_ao:get(<<"data">>, Msg1, <<>>, Opts),
    Type = hb_ao:get(<<"type">>, Msg1, <<>>, Opts),
    ModuleMsg = #{ <<"content-type">> => Type, <<"body">> => Binary },
    case hb_cache:write(ModuleMsg, Opts) of
	{ok, BinaryID} ->
	    {ok, #{
		   <<"id">> => BinaryID,
		   <<"size">> => byte_size(Binary)
		  }};
	Error ->
	    {error, #{
		      <<"status">> => 500,
		      <<"body">> => <<"Failed to cache file">>
		     }}
    end.


is_operator(Req, NodeMsg) ->
    Signers = hb_message:signers(Req),
    OperatorAddr = hb_util:human_id(hb_opts:get(operator, undefined, NodeMsg)),
    lists:any(
      fun(Signer) ->
	      OperatorAddr =:= hb_util:human_id(Signer)
      end,
      Signers
     ).

topup(Msg, Msg2, Opts) -> 
    case is_operator(Msg2,Opts) of
	true ->
	    Wallet = hb_opts:get(priv_wallet, not_found, Opts),
	    Recipient = hb_ao:get(<<"recipient">>, Msg, <<>>, Opts),
	    Port = hb_opts:get(port, not_found, Opts),
	    Node = <<"http://localhost:", (integer_to_binary(Port))/binary, "/">>,
	    {ok, TopupRes} =
		hb_http:post(
		  Node,
		  hb_message:commit(
		    #{
		      <<"path">> => <<"/ledger~node-process@1.0/schedule">>,
		      <<"body">> =>
			  hb_message:commit(
                            #{
			      <<"path">> => <<"credit-notice">>,
			      <<"quantity">> => 100,
			      <<"recipient">> =>  Recipient
			     },
			    Wallet
			   )
		     },
		    Wallet
		   ),
		  #{}
		 ),
	    {ok, dev_codec_json:to(TopupRes)};	
	_ -> {error, dev_codec_json:to(#{ <<"error">> => <<"not operator">> })}
    end.

balance(Msg, Msg2, Opts) -> 
    Target = hb_ao:get(<<"target">>, Msg, <<>>, Opts),
    Port = hb_opts:get(port, not_found, Opts),
    Node = <<"http://localhost:", (integer_to_binary(Port))/binary, "/">>,
    {ok, Bal} =
        hb_http:get(
	  Node,
	  <<"/ledger~node-process@1.0/now/balance/", Target/binary>>,
	  #{}
	 ),
    {ok, dev_codec_json:to(#{ <<"balance">> => Bal})}.

httpsig_to_json(Msg, Msg2, Opts) -> {ok, dev_codec_json:to(Msg2)}.
