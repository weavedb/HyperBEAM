-module(dev_mul).
-export([mul/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

mul(_, M2, Opts) ->
    A = hb_ao:get(<<"a">>, M2, Opts),
    B = hb_ao:get(<<"b">>, M2, Opts),

    Product = dev_mul_nif:multiply(A, B),

    {ok, #{ <<"product">> => Product, <<"a">> => A, <<"b">> => B }}.

multiply_test() ->
    M1 = #{<<"device">> => <<"mul@1.0">>},
    M2 = #{
	   <<"path">> => <<"mul">>,
	   <<"a">> => 2,
	   <<"b">> => 3
	  },
    {ok, Product} = hb_ao:resolve(M1, M2, #{}),
    ?assertEqual(6, maps:get(<<"product">>, Product)).
