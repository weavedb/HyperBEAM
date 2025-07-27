-module(dev_hbsig).
-export([ json_to_erl/3, to_erl/1, to_str/1, structured_to/3, structured_from/3, httpsig_from/3, httpsig_to/3, msg2/3, flat_from/3, flat_to/3 ]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

to_erl(Msg) ->
    JSON = maps:get(<<"body">>, Msg),
    Data = dev_codec_json:from(JSON),
    process_json_data(Data).
    
%% Return both raw term and formatted string representation
to_str(Obj) -> 
    % For raw, use our own format that preserves string/binary distinction
    RawRepr = iolist_to_binary(format_term_raw(Obj)),
    
    % Format the term for string representation (for display/logging)
    % This converts all binaries to byte format for UTF-8 safety
    FormattedRepr = iolist_to_binary(format_term_utf8_safe(Obj)),
    
    % Return a structured response with both representations
    iolist_to_binary([
        <<"#erl_response{raw=">>,
        RawRepr,
        <<",formatted=">>,
        FormattedRepr,
        <<"}">>
    ]).

%% Format term for raw output, preserving string literals
format_term_raw(Map) when is_map(Map) ->
    Items = maps:fold(fun(K, V, Acc) ->
        FormattedK = format_term_raw(K),
        FormattedV = format_term_raw(V),
        [[FormattedK, " => ", FormattedV] | Acc]
    end, [], Map),
    ["#{", lists:join(",", lists:reverse(Items)), "}"];
    
format_term_raw(List) when is_list(List) ->
    Items = [format_term_raw(Item) || Item <- List],
    ["[", lists:join(",", Items), "]"];
    
format_term_raw(Bin) when is_binary(Bin) ->
    % Always format as string literal for raw output
    % This preserves the information that it's meant to be a string
    ["<<\"", escape_binary_string(Bin), "\">>"];
    
format_term_raw(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
    
format_term_raw(Int) when is_integer(Int) ->
    integer_to_list(Int);
    
format_term_raw(Float) when is_float(Float) ->
    io_lib:format("~p", [Float]);
    
format_term_raw(Other) ->
    io_lib:format("~p", [Other]).

%% Escape binary content for string representation
escape_binary_string(Bin) ->
    escape_binary_string(Bin, []).

escape_binary_string(<<>>, Acc) ->
    lists:reverse(Acc);
escape_binary_string(<<$", Rest/binary>>, Acc) ->
    escape_binary_string(Rest, [$", $\\ | Acc]);
escape_binary_string(<<$\\, Rest/binary>>, Acc) ->
    escape_binary_string(Rest, [$\\, $\\ | Acc]);
escape_binary_string(<<$\n, Rest/binary>>, Acc) ->
    escape_binary_string(Rest, [$n, $\\ | Acc]);
escape_binary_string(<<$\r, Rest/binary>>, Acc) ->
    escape_binary_string(Rest, [$r, $\\ | Acc]);
escape_binary_string(<<$\t, Rest/binary>>, Acc) ->
    escape_binary_string(Rest, [$t, $\\ | Acc]);
escape_binary_string(<<C, Rest/binary>>, Acc) when C >= 32, C =< 126 ->
    escape_binary_string(Rest, [C | Acc]);
escape_binary_string(<<C, Rest/binary>>, Acc) ->
    % Non-printable character, use byte representation
    Escaped = io_lib:format("\\~3.8.0B", [C]),
    escape_binary_string(Rest, lists:reverse(Escaped) ++ Acc).

json_to_erl(Msg1, _Msg2, _Opts) ->
    Data = to_erl(Msg1),
    io:format("After structured field decode: ~p~n", [Data]),
    
    % Use to_str to get both representations
    Result = to_str(Data),
    
    % Log the formatted part for debugging
    case binary:match(Result, <<"formatted=">>) of
        {Start, _} ->
            <<_:Start/binary, "formatted=", Rest/binary>> = Result,
            case binary:match(Rest, <<"}">>)  of
                {End, _} ->
                    <<Formatted:End/binary, _/binary>> = Rest,
                    io:format("Erlang string response: ~s~n", [Formatted]);
                _ -> ok
            end;
        _ -> ok
    end,
    
    {ok, Result}.

%% Format term with UTF-8 safe binary representation
%% This is only used for the formatted output, not the raw output
format_term_utf8_safe(Map) when is_map(Map) ->
    Items = maps:fold(fun(K, V, Acc) ->
        FormattedK = format_term_utf8_safe(K),
        FormattedV = format_term_utf8_safe(V),
        [[FormattedK, " => ", FormattedV] | Acc]
    end, [], Map),
    ["#{", lists:join(",", lists:reverse(Items)), "}"];
    
format_term_utf8_safe(List) when is_list(List) ->
    Items = [format_term_utf8_safe(Item) || Item <- List],
    ["[", lists:join(",", Items), "]"];
    
format_term_utf8_safe(Bin) when is_binary(Bin) ->
    % Always use byte representation to avoid UTF-8 issues
    % This is only for the formatted output
    ByteList = binary_to_list(Bin),
    ["<<", lists:join(",", [integer_to_list(B) || B <- ByteList]), ">>"];
    
format_term_utf8_safe(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
    
format_term_utf8_safe(Int) when is_integer(Int) ->
    integer_to_list(Int);
    
format_term_utf8_safe(Float) when is_float(Float) ->
    io_lib:format("~p", [Float]);
    
format_term_utf8_safe(Other) ->
    io_lib:format("~p", [Other]).

%% Process JSON data - transform structured fields and $empty
process_json_data(Map) when is_map(Map) ->
    % Check for $empty annotation
    case maps:get(<<"$empty">>, Map, undefined) of
        <<"binary">> -> <<>>;
        <<"list">> -> [];
        <<"map">> -> #{};
        undefined ->
            maps:map(fun(_K, V) -> process_json_data(V) end, Map);
        _Other ->
            % For any other value of $empty, just process the map normally
            % This handles cases like "should not be special" or "test"
            maps:map(fun(_K, V) -> process_json_data(V) end, Map)
    end;
    
process_json_data(List) when is_list(List) ->
    [process_json_data(Item) || Item <- List];
    
process_json_data(Value) when is_binary(Value) ->
    % Check for structured field formats
    case Value of
        <<$:, Rest/binary>> when byte_size(Rest) > 0 ->
            case binary:last(Value) of
                $: ->
                    % It's a binary structured field, decode the base64
                    Base64Len = byte_size(Value) - 2,
                    <<$:, Base64:Base64Len/binary, $:>> = Value,
                    % Handle empty binary special case
                    case Base64 of
                        <<>> -> <<>>;
                        _ ->
                            try
                                base64:decode(Base64)
                            catch
                                _:_ -> Value  % If decode fails, return original
                            end
                    end;
                _ -> Value
            end;
        <<$%, Rest/binary>> when byte_size(Rest) > 0 ->
            case binary:last(Value) of
                $% ->
                    % It's a token structured field (for atoms)
                    TokenLen = byte_size(Value) - 2,
                    <<$%, Token:TokenLen/binary, $%>> = Value,
                    % Convert to atom
                    binary_to_atom(Token, utf8);
                _ -> Value
            end;
        _ -> 
            % Regular string, leave as-is (already a binary)
            Value
    end;
    
process_json_data(Other) -> 
    % Everything else passes through unchanged
    Other.

%% Check if binary contains only safe ASCII characters (32-126)
is_safe_ascii(Bin) ->
    lists:all(fun(B) -> B >= 32 andalso B =< 126 end, binary_to_list(Bin)).


structured_from(Msg1, _Msg2, _Opts) ->
    Data = to_erl(Msg1),
    io:format("After structured field decode: ~p~n", [Data]),
    OBJ = dev_codec_structured:from(Data),
    io:format("OBJ: ~p~n", [OBJ]),    
    Result = to_str(OBJ),
    {ok, Result}.

structured_to(Msg1, _Msg2, _Opts) ->
    Data = to_erl(Msg1),
    io:format("After structured field decode: ~p~n", [Data]),
    OBJ = dev_codec_structured:to(Data),
    io:format("OBJ: ~p~n", [OBJ]),    
    Result = to_str(OBJ),
    {ok, Result}.

httpsig_from(Msg1, Msg2, Opts) ->
    Data = to_erl(Msg1),
    io:format("After structured field decode: ~p~n", [Data]),
    OBJ = dev_codec_httpsig:from(Data),
    io:format("httpsig:to: ~p~n", [OBJ]),
    Result = to_str(OBJ),
    {ok, Result}.

httpsig_to(Msg1, Msg2, Opts) ->
    Data = to_erl(Msg1),
    io:format("After structured field decode: ~p~n", [Data]),
    OBJ = dev_codec_httpsig:to(Data),
    io:format("httpsig:to: ~p~n", [OBJ]),
    Result = to_str(OBJ),
    {ok, Result}.

flat_from(Msg1, _Msg2, _Opts) ->
    Data = to_erl(Msg1),
    io:format("After structured field decode: ~p~n", [Data]),
    OBJ = dev_codec_flat:from(Data),
    io:format("OBJ: ~p~n", [OBJ]),    
    Result = to_str(OBJ),
    {ok, Result}.

flat_to(Msg1, _Msg2, _Opts) ->
    Data = to_erl(Msg1),
    io:format("After structured field decode: ~p~n", [Data]),
    OBJ = dev_codec_flat:to(Data),
    io:format("OBJ: ~p~n", [OBJ]),    
    Result = to_str(OBJ),
    {ok, Result}.

msg2(Msg, Msg2, Opts) -> 
    io:format("OBJ: ~p~n", [Msg2]),    
    Result = to_str(Msg2),
    {ok, Result}.
