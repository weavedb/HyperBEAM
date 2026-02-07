-module(dev_add).
-export([add/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

add(_M1, M2, _Opts) ->
    A = maps:get(<<"a">>, M2),
    B = maps:get(<<"b">>, M2),
    {ok, Sum} = dev_add_nif:add(A, B),
    {ok, #{ <<"sum">> => Sum }}.

add_test() ->
    M1 = #{ <<"device">> => <<"add@1.0">> },
    M2 = #{ <<"path">> => <<"add">>, <<"a">> => 2, <<"b">> => 3 },
    {ok, #{ <<"sum">> := 5 }} = hb_ao:resolve(M1, M2, #{}).
