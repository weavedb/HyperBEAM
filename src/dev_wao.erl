-module(dev_wao).
-export([ info/3, compute/3, init/3, snapshot/3, normalize/3 ]).
-export([ cache_module/3, httpsig/3, cron/3 ]).
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

info(Msg1, Msg2_, Opts) ->
    JSON = dev_codec_json:to(#{ <<"version">> => <<"1.0">> }),
    {ok, JSON}.

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

httpsig(Msg, Msg2, Opts) -> 
    Msg3 = maps:without([
			 <<"commitments">>, 
			 <<"method">>, 
			 <<"path">>, 
			 <<"inline-body-key">>, 
			 <<"content-length">>, 
			 <<"content-type">>
			], Msg2),
    Msg4 = convert_binaries_to_base64(Msg3),
    Msg5 = dev_codec_json:to(Msg4),
    {ok, Msg5}.

%% Convert all binaries in a nested structure to base64
convert_binaries_to_base64(Value) when is_binary(Value) ->
    %% Check if the binary contains only ASCII printable characters
    case is_safe_for_json(Value) of
        false ->
            %% Not safe for JSON, encode as base64
            base64:encode(Value);
        true ->
            %% Safe ASCII, keep as is
            Value
    end;

convert_binaries_to_base64(Value) when is_map(Value) ->
    maps:fold(
      fun(K, V, Acc) ->
	      maps:put(K, convert_binaries_to_base64(V), Acc)
      end,
      #{},
      Value
     );

convert_binaries_to_base64(Value) when is_list(Value) ->
    %% Check if it's a string (list of integers in valid range)
    case io_lib:printable_list(Value) of
        true ->
            %% It's a string, keep as is
            Value;
        false ->
            %% It's a list, recursively convert elements
            lists:map(fun convert_binaries_to_base64/1, Value)
    end;

convert_binaries_to_base64(Value) when is_tuple(Value) ->
    %% Convert tuple to list, process, and convert back
    list_to_tuple(lists:map(fun convert_binaries_to_base64/1, tuple_to_list(Value)));

convert_binaries_to_base64(Value) ->
    %% For all other types (integers, atoms, floats, etc.), return as is
    Value. 

%% Alternative: More aggressive version that converts ALL binaries to base64
%% regardless of whether they're valid UTF-8
convert_all_binaries_to_base64(Value) when is_binary(Value) ->
    base64:encode(Value);

convert_all_binaries_to_base64(Value) when is_map(Value) ->
    maps:fold(
      fun(K, V, Acc) ->
	      maps:put(K, convert_all_binaries_to_base64(V), Acc)
      end,
      #{},
      Value
     );

convert_all_binaries_to_base64(Value) when is_list(Value) ->
    case io_lib:printable_list(Value) of
        true -> Value;
        false -> lists:map(fun convert_all_binaries_to_base64/1, Value)
    end;

convert_all_binaries_to_base64(Value) when is_tuple(Value) ->
    list_to_tuple(lists:map(fun convert_all_binaries_to_base64/1, tuple_to_list(Value)));

convert_all_binaries_to_base64(Value) -> Value.

%% Helper function to check if binary contains only ASCII printable characters
is_safe_for_json(Bin) when is_binary(Bin) ->
    try
        %% Check if all bytes are printable ASCII (32-126)
        lists:all(fun(Byte) -> Byte >= 32 andalso Byte =< 126 end, binary_to_list(Bin))
    catch
        _:_ -> false
    end.

%% Version that only converts binaries that would break JSON
convert_unsafe_binaries_to_base64(Value) when is_binary(Value) ->
    case is_safe_for_json(Value) of
        true -> Value;
        false -> base64:encode(Value)
    end;

convert_unsafe_binaries_to_base64(Value) when is_map(Value) ->
    maps:fold(
      fun(K, V, Acc) ->
	      maps:put(K, convert_unsafe_binaries_to_base64(V), Acc)
      end,
      #{},
      Value
     );

convert_unsafe_binaries_to_base64(Value) when is_list(Value) ->
    case io_lib:printable_list(Value) of
        true -> Value;
        false -> lists:map(fun convert_unsafe_binaries_to_base64/1, Value)
    end;

convert_unsafe_binaries_to_base64(Value) when is_tuple(Value) ->
    list_to_tuple(lists:map(fun convert_unsafe_binaries_to_base64/1, tuple_to_list(Value)));

convert_unsafe_binaries_to_base64(Value) ->
    Value.


