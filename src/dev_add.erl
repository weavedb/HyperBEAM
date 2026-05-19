-module(dev_add).
-export([add/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

add(_M1, M2, _Opts) ->
    A = to_int(maps:get(<<"a">>, M2)),
    B = to_int(maps:get(<<"b">>, M2)),
    {ok, Sum} = dev_add_nif:add(A, B),
    {ok, #{ <<"sum">> => Sum }}.

to_int(N) when is_integer(N) -> N;
to_int(B) when is_binary(B) -> binary_to_integer(B);
to_int(L) when is_list(L) -> list_to_integer(L).

add_test() ->
    M1 = #{ <<"device">> => <<"add@1.0">> },
    M2 = #{ <<"path">> => <<"add">>, <<"a">> => 2, <<"b">> => 3 },
    {ok, #{ <<"sum">> := 5 }} = hb_ao:resolve(M1, M2, #{}).
