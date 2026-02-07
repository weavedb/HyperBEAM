-module(dev_hbsig).
-export([ json_to_erl/3, to_erl/1, to_str/1, structured_to/3, structured_from/3, httpsig_from/3, httpsig_to/3, msg2/3, flat_from/3, flat_to/3 ]).
-include("include/hb.hrl").

to_erl(Msg) ->
    Body = maps:get(<<"body">>, Msg),
    case Body of
        JSON when is_binary(JSON) ->
            Decoded = json:decode(JSON),
            process_json_data(Decoded);
        AlreadyDecoded when is_map(AlreadyDecoded) ->
            AlreadyDecoded;
        Other ->
            Other
    end.

to_str(Obj) ->
    RawRepr = iolist_to_binary(format_term_raw(Obj)),
    FormattedRepr = iolist_to_binary(format_term_utf8_safe(Obj)),
    iolist_to_binary([
        <<"#erl_response{raw=">>,
        RawRepr,
        <<",formatted=">>,
        FormattedRepr,
        <<"}">>
    ]).

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
    ["<<\"", escape_binary_string(Bin), "\">>"];
format_term_raw(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
format_term_raw(Int) when is_integer(Int) ->
    integer_to_list(Int);
format_term_raw(Float) when is_float(Float) ->
    io_lib:format("~p", [Float]);
format_term_raw(Other) ->
    io_lib:format("~p", [Other]).

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
    Escaped = io_lib:format("\\~3.8.0B", [C]),
    escape_binary_string(Rest, lists:reverse(Escaped) ++ Acc).

json_to_erl(Msg1, _Msg2, _Opts) ->
    Data = to_erl(Msg1),
    Result = to_str(Data),
    {ok, Result}.

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

process_json_data(Map) when is_map(Map) ->
    case maps:get(<<"$empty">>, Map, undefined) of
        <<"binary">> -> <<>>;
        <<"list">> -> [];
        <<"map">> -> #{};
        undefined ->
            maps:map(fun(_K, V) -> process_json_data(V) end, Map);
        _Other ->
            maps:map(fun(_K, V) -> process_json_data(V) end, Map)
    end;
process_json_data(List) when is_list(List) ->
    [process_json_data(Item) || Item <- List];
process_json_data(Value) when is_binary(Value) ->
    case Value of
        <<$:, Rest/binary>> when byte_size(Rest) > 0 ->
            case binary:last(Value) of
                $: ->
                    Base64Len = byte_size(Value) - 2,
                    <<$:, Base64:Base64Len/binary, $:>> = Value,
                    case Base64 of
                        <<>> -> <<>>;
                        _ ->
                            try base64:decode(Base64)
                            catch _:_ -> Value
                            end
                    end;
                _ -> Value
            end;
        <<$%, Rest/binary>> when byte_size(Rest) > 0 ->
            case binary:last(Value) of
                $% ->
                    TokenLen = byte_size(Value) - 2,
                    <<$%, Token:TokenLen/binary, $%>> = Value,
                    binary_to_atom(Token, utf8);
                _ -> Value
            end;
        _ -> Value
    end;
process_json_data(Other) -> Other.

structured_from(Msg1, _Msg2, _Opts) ->
    Data = to_erl(Msg1),
    {ok, OBJ} = dev_codec_structured:from(Data, #{<<"bundle">> => true}, #{}),
    Result = to_str(OBJ),
    {ok, Result}.

structured_to(Msg1, _Msg2, _Opts) ->
    Data = to_erl(Msg1),
    {ok, OBJ} = dev_codec_structured:to(Data, #{<<"bundle">> => true}, #{}),
    Result = to_str(OBJ),
    {ok, Result}.

httpsig_from(Msg1, _Msg2, _Opts) ->
    Data = to_erl(Msg1),
    {ok, OBJ} = dev_codec_httpsig:from(Data, #{<<"bundle">> => true}, #{}),
    Result = to_str(OBJ),
    {ok, Result}.

httpsig_to(Msg1, _Msg2, _Opts) ->
    Data = to_erl(Msg1),
    PreparedData = preprocess_unsupported_types(Data),
    ensure_atoms_from_ao_types(PreparedData),
    {ok, OBJ} = dev_codec_httpsig:to(PreparedData, #{<<"bundle">> => true}, #{}),
    Result = to_str(OBJ),
    {ok, Result}.

preprocess_unsupported_types(Map) when is_map(Map) ->
    case maps:get(<<"ao-types">>, Map, undefined) of
        undefined ->
            maps:map(fun(_K, V) -> preprocess_unsupported_types(V) end, Map);
        AoTypes when is_binary(AoTypes) ->
            HasBoolean = binary:match(AoTypes, <<"boolean">>) =/= nomatch,
            HasEmpty = binary:match(AoTypes, <<"empty-">>) =/= nomatch,
            case HasBoolean orelse HasEmpty of
                false ->
                    maps:map(fun(_K, V) -> preprocess_unsupported_types(V) end, Map);
                true ->
                    {NewAoTypes, BoolKeys, EmptyEntries} = process_ao_types(AoTypes),
                    Map2 = convert_boolean_values(Map, BoolKeys),
                    Map3 = create_empty_values(Map2, EmptyEntries),
                    Map4 = case NewAoTypes of
                        <<>> -> maps:remove(<<"ao-types">>, Map3);
                        _ -> maps:put(<<"ao-types">>, NewAoTypes, Map3)
                    end,
                    maps:map(fun(_K, V) -> preprocess_unsupported_types(V) end, Map4)
            end;
        _ ->
            maps:map(fun(_K, V) -> preprocess_unsupported_types(V) end, Map)
    end;
preprocess_unsupported_types(Bin) when is_binary(Bin) ->
    case binary:match(Bin, <<"(ao-type-boolean)">>) of
        nomatch -> Bin;
        _ ->
            Bin2 = binary:replace(Bin, <<"\"(ao-type-boolean) ?1\"">>, <<"true">>, [global]),
            Bin3 = binary:replace(Bin2, <<"\"(ao-type-boolean) ?0\"">>, <<"false">>, [global]),
            Bin4 = binary:replace(Bin3, <<"(ao-type-boolean) ?1">>, <<"true">>, [global]),
            binary:replace(Bin4, <<"(ao-type-boolean) ?0">>, <<"false">>, [global])
    end;
preprocess_unsupported_types(List) when is_list(List) ->
    [preprocess_unsupported_types(Item) || Item <- List];
preprocess_unsupported_types(Other) ->
    Other.

process_ao_types(AoTypes) when is_binary(AoTypes) ->
    Parts = binary:split(AoTypes, <<", ">>, [global]),
    {NewParts, BoolKeys, EmptyEntries} = lists:foldl(fun(Part, {PAcc, BAcc, EAcc}) ->
        case extract_type_annotation(Part) of
            {ok, Key, <<"boolean">>} ->
                NewPart = <<Key/binary, "=\"atom\"">>,
                {[NewPart | PAcc], [Key | BAcc], EAcc};
            {ok, Key, <<"empty-", _/binary>> = EmptyType} ->
                {PAcc, BAcc, [{Key, EmptyType} | EAcc]};
            _ ->
                {[Part | PAcc], BAcc, EAcc}
        end
    end, {[], [], []}, Parts),
    {iolist_to_binary(lists:join(<<", ">>, lists:reverse(NewParts))), BoolKeys, EmptyEntries};
process_ao_types(Other) ->
    {Other, [], []}.

extract_type_annotation(Part) ->
    case binary:match(Part, <<"=\"">>) of
        {Pos, _} ->
            Key = binary:part(Part, 0, Pos),
            RestStart = Pos + 2,
            RestLen = byte_size(Part) - RestStart - 1,
            case RestLen > 0 of
                true ->
                    Type = binary:part(Part, RestStart, RestLen),
                    {ok, Key, Type};
                false ->
                    error
            end;
        nomatch ->
            error
    end.

convert_boolean_values(Map, []) -> Map;
convert_boolean_values(Map, [Key | Rest]) ->
    LowerKey = list_to_binary(string:lowercase(binary_to_list(Key))),
    Map2 = case maps:get(LowerKey, Map, undefined) of
        <<"?1">> -> maps:put(LowerKey, <<"true">>, Map);
        <<"?0">> -> maps:put(LowerKey, <<"false">>, Map);
        _ ->
            case maps:get(Key, Map, undefined) of
                <<"?1">> -> maps:put(Key, <<"true">>, Map);
                <<"?0">> -> maps:put(Key, <<"false">>, Map);
                _ -> Map
            end
    end,
    convert_boolean_values(Map2, Rest).

create_empty_values(Map, []) -> Map;
create_empty_values(Map, [{Key, EmptyType} | Rest]) ->
    LowerKey = list_to_binary(string:lowercase(binary_to_list(Key))),
    EmptyValue = case EmptyType of
        <<"empty-binary">> -> <<>>;
        <<"empty-list">> -> [];
        <<"empty-message">> -> #{}
    end,
    Map2 = case maps:is_key(LowerKey, Map) of
        true -> Map;
        false -> maps:put(LowerKey, EmptyValue, Map)
    end,
    create_empty_values(Map2, Rest).

ensure_atoms_from_ao_types(Map) when is_map(Map) ->
    case maps:get(<<"ao-types">>, Map, undefined) of
        AoTypes when is_binary(AoTypes) ->
            Pairs = binary:split(AoTypes, <<", ">>, [global]),
            lists:foreach(fun(Pair) ->
                case extract_type_annotation(Pair) of
                    {ok, Key, <<"atom">>} ->
                        LowerKey = list_to_binary(
                            string:lowercase(binary_to_list(Key))),
                        Value = case maps:get(Key, Map, undefined) of
                            undefined -> maps:get(LowerKey, Map, undefined);
                            V -> V
                        end,
                        case Value of
                            V2 when is_binary(V2) ->
                                binary_to_atom(V2, utf8);
                            _ -> ok
                        end;
                    _ -> ok
                end
            end, Pairs);
        _ -> ok
    end,
    maps:foreach(fun(_K, V) -> ensure_atoms_from_ao_types(V) end, Map);
ensure_atoms_from_ao_types(List) when is_list(List) ->
    lists:foreach(fun(Item) -> ensure_atoms_from_ao_types(Item) end, List);
ensure_atoms_from_ao_types(_) ->
    ok.

flat_from(Msg1, _Msg2, _Opts) ->
    Data = to_erl(Msg1),
    PreparedData = prepare_for_flat(Data),
    {ok, OBJ} = dev_codec_flat:from(PreparedData, #{}, #{}),
    Result = to_str(OBJ),
    {ok, Result}.

prepare_for_flat(Map) when is_map(Map) ->
    maps:map(fun(_K, V) -> prepare_for_flat(V) end, Map);
prepare_for_flat(List) when is_list(List) ->
    iolist_to_binary(io_lib:format("~p", [List]));
prepare_for_flat(Bin) when is_binary(Bin) ->
    Bin;
prepare_for_flat(Int) when is_integer(Int) ->
    integer_to_binary(Int);
prepare_for_flat(Float) when is_float(Float) ->
    float_to_binary(Float, [{decimals, 10}, compact]);
prepare_for_flat(true) ->
    <<"true">>;
prepare_for_flat(false) ->
    <<"false">>;
prepare_for_flat(null) ->
    <<"null">>;
prepare_for_flat(Atom) when is_atom(Atom) ->
    atom_to_binary(Atom, utf8);
prepare_for_flat(Other) ->
    iolist_to_binary(io_lib:format("~p", [Other])).

flat_to(Msg1, _Msg2, _Opts) ->
    Data = to_erl(Msg1),
    {ok, OBJ} = dev_codec_flat:to(Data, #{}, #{}),
    Result = to_str(OBJ),
    {ok, Result}.

msg2(_Msg, Msg2, _Opts) ->
    Msg3 = process_ao_types_empty_values(Msg2, Msg2),
    Result = to_str(Msg3),
    {ok, Result}.

process_ao_types_empty_values(Msg, Msg2) ->
    AoTypes = maps:get(<<"ao-types">>, Msg, <<>>),
    case AoTypes of
        <<>> -> Msg2;
        _ ->
            EmptyKeys = parse_empty_types(AoTypes),
            Msg3 = lists:foldl(
                fun({Key, Type}, Acc) ->
                    case Type of
                        <<"empty-binary">> ->
                            maps:put(Key, <<>>, Acc);
                        <<"empty-list">> ->
                            maps:put(Key, [], Acc);
                        <<"empty-message">> ->
                            maps:put(Key, #{}, Acc);
                        _ ->
                            Acc
                    end
                end,
                Msg2,
                EmptyKeys
            ),
            BodyInAoTypes = lists:any(
                fun({K, _}) -> K =:= <<"body">> end,
                EmptyKeys
            ),
            case maps:get(<<"body">>, Msg3, undefined) of
                <<>> when not BodyInAoTypes ->
                    maps:remove(<<"body">>, Msg3);
                _ ->
                    Msg3
            end
    end.

parse_empty_types(AoTypes) ->
    Pairs = binary:split(AoTypes, <<", ">>, [global]),
    lists:filtermap(
        fun(Pair) ->
            case re:run(Pair, <<"^(.+?)=\"(empty-[^\"]+)\"$">>, [{capture, [1, 2], binary}]) of
                {match, [Key, Type]} ->
                    {true, {Key, Type}};
                _ ->
                    false
            end
        end,
        Pairs
    ).
