-module(dev_weavedb_nif).
-export([query/2]).
-on_load(init/0).

-include("include/cargo.hrl").
-include_lib("eunit/include/eunit.hrl").

init() ->
    ?load_nif_from_crate(dev_weavedb_nif, 0).

query(_, _) ->
    erlang:nif_error(nif_not_loaded).
