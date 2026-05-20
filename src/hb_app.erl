%%%-------------------------------------------------------------------
%% @doc The main HyperBEAM application module.
%% @end
%%%-------------------------------------------------------------------

-module(hb_app).

-behaviour(application).

-export([start/2, stop/1]).

-include("include/hb.hrl").

start(_StartType, _StartArgs) ->
    hb:init(),
    {ok, SupPid} = hb_sup:start_link(),
    ok = dev_scheduler_registry:start(),
    _TimestampServer = ar_timestamp:start(),
    %% When WAO_NO_DEFAULT_HTTP_SERVER is set (wao spawns >1 HyperBEAM
    %% instance), skip the default 8734-port HTTP server start so the
    %% second VM doesn't crash with eaddrinuse / case_clause when both
    %% nodes auto-start the hb application. wao calls
    %% hb_http_server:start_node/1 explicitly later with the right port.
    case os:getenv("WAO_NO_DEFAULT_HTTP_SERVER") of
        false ->
            {ok, _} = hb_http_server:start();
        _ ->
            ok
    end,
    {ok, SupPid}.

stop(_State) ->
    ok.