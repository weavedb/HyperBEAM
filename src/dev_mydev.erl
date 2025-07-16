-module(dev_mydev).
-export([ info/3, info_json/3, hello/3, forward/3 ]).
-export([ flat_to/3, flat_from/3, structured_to/3, structured_from/3, httpsig_to/3, httpsig_from/3 ]).
-export([ add/3, resolve/3, resolve2/3, resolve3/3 ]).
-export([ inc/3, double/3, square/3, calc/3, inc2/3, double2/3, square2/3 ]).

-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

info(Msg1, Msg2_, Opts) ->
    {ok, #{ <<"version">> => <<"1.0">> }}.

info_json(Msg1, Msg2_, Opts) ->
    JSON = dev_codec_json:to(#{ <<"version">> => <<"1.0">> }),
    {ok, JSON}.

hello(Msg1, Msg2_, Opts) ->
    Body = maps:get(<<"body">>, Msg1),
    OBJ = dev_codec_json:from(Body),
    Name = maps:get(<<"name">>, OBJ),
    Hello = <<<<"Hello, ">>/binary, Name/binary, <<"!">>/binary>>,
    JSON = dev_codec_json:to(#{ <<"hello">> => Hello }),
    {ok, JSON}.

forward(Msg1, Msg2, Opts) ->
  io:format("Msg1: ~p~n~nMsg2: ~p~n~nOpts: ~p~n", [Msg1, Msg2, Opts]),
  JSON = dev_codec_json:to(#{
    <<"msg1">> => Msg1,
    <<"msg2">> => Msg2,
    <<"opts">> => hb_private:reset(Opts)
  }),
  {ok, JSON}.

flat_to(Msg1, Msg2, Opts) ->
    Body = maps:get(<<"body">>, Msg1),
    OBJ = dev_codec_json:from(Body),
    FLAT = dev_codec_flat:to(OBJ),
    JSON = dev_codec_json:to(FLAT),
    {ok, JSON}.

flat_from(Msg1, Msg2, Opts) ->
    Body = maps:get(<<"body">>, Msg1),
    OBJ = dev_codec_json:from(Body),
    FLAT = dev_codec_flat:from(OBJ),
    JSON = dev_codec_json:to(FLAT),
    {ok, JSON}.

structured_to(Msg1, Msg2, Opts) ->
    Body = maps:get(<<"body">>, Msg1),
    OBJ = dev_codec_json:from(Body),
    TABM = dev_codec_structured:to(OBJ),
    JSON = dev_codec_json:to(TABM),
    {ok, JSON}.
 
structured_from(Msg1, Msg2, Opts) ->
    Body = maps:get(<<"body">>, Msg1),
    TABM = dev_codec_json:from(Body),
    OBJ = dev_codec_structured:from(TABM),
    JSON = dev_codec_json:to(OBJ),
    {ok, JSON}.

httpsig_to(Msg1, Msg2, Opts) ->
    Body = maps:get(<<"body">>, Msg1),
    TABM = dev_codec_json:from(Body),
    HTTPSIG = dev_codec_httpsig:to(TABM),
    JSON = dev_codec_json:to(HTTPSIG),
    {ok, JSON}.
 
httpsig_from(Msg1, Msg2, Opts) ->
    Body = maps:get(<<"body">>, Msg1),
    HTTPSIG = dev_codec_json:from(Body),
    TABM = dev_codec_httpsig:from(HTTPSIG),
    JSON = dev_codec_json:to(TABM),
    {ok, JSON}.

add(Msg1, Msg2, Opts)->
  Num = maps:get(<<"num">>, Msg1),
  Plus = maps:get(<<"plus">>, Msg2),
  {ok, #{ <<"device">> => <<"mydev@1.0">>, <<"num">> => Num + Plus }}.

resolve(_, _, Opts)->
  Msg1 = #{ <<"device">> => <<"mydev@1.0">>, <<"num">> => 0 },
  io:format("Msg1 ID: ~p~n", [hb_message:id(Msg1)]),
 
  Msg2 = #{ <<"path">> => <<"add">>, <<"plus">> => 1 },
  io:format("Msg2 ID: ~p~n", [hb_message:id(Msg2)]),
  {ok, Msg3} = hb_ao:resolve(Msg1, Msg2, Opts),
  io:format("Msg3: ~p~n", [Msg3]),
  io:format("Msg3 ID: ~p~n", [hb_message:id(Msg3)]),
 
  Msg4 = #{ <<"path">> => <<"add">>, <<"plus">> => 2 },
  io:format("Msg4 ID: ~p~n", [hb_message:id(Msg4)]),
 
  {ok, Msg5} = hb_ao:resolve(Msg3, Msg4, Opts),
  io:format("Msg5: ~p~n", [Msg5]),
  io:format("Msg5 ID: ~p~n", [hb_message:id(Msg5)]),
 
  Msg6 = #{ <<"path">> => <<"add">>, <<"plus">> => 3 },
  io:format("Msg6 ID: ~p~n", [Msg6]),
 
  {ok, Msg7} = hb_ao:resolve(Msg5, Msg6, Opts),
  io:format("Msg7: ~p~n", [Msg7]),
  io:format("Msg7 ID: ~p~n", [Msg7]),
 
  {ok, Msg7}.

resolve2(_, _, Opts)->
  Msg1 = #{ <<"device">> => <<"mydev@1.0">>, <<"num">> => 0 },
  io:format("Msg1 ID: ~p~n", [hb_message:id(Msg1)]),
 
  Msg2 = hb_message:commit(#{ <<"path">> => <<"add">>, <<"plus">> => 1 }, Opts),
  io:format("Msg2 ID: ~p~n", [hb_message:id(Msg2)]),
  
  {ok, Msg3} = hb_ao:resolve(Msg1, Msg2, Opts),
  io:format("Msg3: ~p~n", [Msg3]),
  io:format("Msg3 ID: ~p~n", [hb_message:id(Msg3)]),
 
  Msg4 = hb_message:commit(#{ <<"path">> => <<"add">>, <<"plus">> => 2 }, Opts),
  io:format("Msg4 ID: ~p~n", [hb_message:id(Msg4)]),
 
  {ok, Msg5} = hb_ao:resolve(Msg3, Msg4, Opts),
  io:format("Msg5: ~p~n", [Msg5]),
  io:format("Msg5 ID: ~p~n", [hb_message:id(Msg5)]),
 
  Msg6 = hb_message:commit(#{ <<"path">> => <<"add">>, <<"plus">> => 3 }, Opts),
  io:format("Msg6 ID: ~p~n", [Msg6]),
 
  {ok, Msg7} = hb_ao:resolve(Msg5, Msg6, Opts),
  io:format("Msg7: ~p~n", [Msg7]),
  io:format("Msg7 ID: ~p~n", [Msg7]),
 
  {ok, Msg7}.

resolve3(_, _, Opts)->
  Msg1 = #{ <<"device">> => <<"mydev@1.0">>, <<"num">> => 0 },
  io:format("Msg1 ID: ~p~n", [hb_message:id(Msg1)]),

  Msg2 = hb_message:commit(#{ <<"path">> => <<"add">>, <<"plus">> => 1 }, Opts),
  io:format("Msg2 ID: ~p~n", [hb_message:id(Msg2)]),
  
  {ok, Msg3} = hb_ao:resolve(Msg1, Msg2, Opts),
  io:format("Msg3: ~p~n", [Msg3]),
  io:format("Msg3 ID: ~p~n", [hb_message:id(Msg3)]),
  hb_cache:write_hashpath(Msg3, Opts),

  Msg4 = hb_message:commit(#{ <<"path">> => <<"add">>, <<"plus">> => 2 }, Opts),
  io:format("Msg4 ID: ~p~n", [hb_message:id(Msg4)]),

  {ok, Msg5} = hb_ao:resolve(Msg3, Msg4, Opts),
  io:format("Msg5: ~p~n", [Msg5]),
  io:format("Msg5 ID: ~p~n", [hb_message:id(Msg5)]),
  hb_cache:write_hashpath(Msg5, Opts),
  
  Msg6 = hb_message:commit(#{ <<"path">> => <<"add">>, <<"plus">> => 3 }, Opts),
  io:format("Msg6 ID: ~p~n", [Msg6]),

  {ok, Msg7} = hb_ao:resolve(Msg5, Msg6, Opts),
  io:format("Msg7: ~p~n", [Msg7]),
  io:format("Msg7 ID: ~p~n", [Msg7]),
  hb_cache:write_hashpath(Msg7, Opts),
  
  {ok, Msg7#{ 
    <<"hashpath_3">> => maps:get(<<"hashpath">>, maps:get(<<"priv">>, Msg3)),
    <<"hashpath_5">> => maps:get(<<"hashpath">>, maps:get(<<"priv">>, Msg5)),
    <<"hashpath_7">> => maps:get(<<"hashpath">>, maps:get(<<"priv">>, Msg7))
  }}.

inc(Msg1, Msg2, Opts)->
  Num = maps:get(<<"num">>, Msg1),
  {ok, #{ <<"num">> => Num + 1 }}.
 
double(Msg1, Msg2, Opts)->
  Num = maps:get(<<"num">>, Msg1),
  {ok, #{ <<"num">> => Num * 2 }}.
 
square(Msg1, Msg2, Opts)->
  Num = maps:get(<<"num">>, Msg1),
  {ok, #{ <<"num">> => Num * Num }}.

calc(Msg1, Msg2, Opts)->
  Num = maps:get(<<"init_num">>, Msg2),
  {ok, #{ <<"num">> => Num}}.

inc2(Msg1, Msg2, Opts)->
  Num = maps:get(<<"num">>, Msg1),
  {ok, #{ 
    <<"num">> => Num + 1, 
    <<"device-stack">> => maps:get(<<"device-stack">>, Msg1)
  }}.

double2(Msg1, Msg2, Opts)->
  Num = maps:get(<<"num">>, Msg1),
  {ok, #{ 
    <<"num">> => Num * 2,
    <<"device-stack">> => maps:get(<<"device-stack">>, Msg1)
  }}.

square2(Msg1, Msg2, Opts)->
  Num = maps:get(<<"num">>, Msg1),
  {ok, #{ 
    <<"num">> => Num * Num,
    <<"device-stack">> => maps:get(<<"device-stack">>, Msg1)
   }}.
