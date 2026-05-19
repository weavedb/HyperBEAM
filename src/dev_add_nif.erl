-module(dev_add_nif).
-export([add/2]).
-on_load(init/0).

-include("include/cargo.hrl").
-include_lib("eunit/include/eunit.hrl").

init() ->
    ?load_nif_from_crate(dev_add_nif, 0).

add(_, _) ->
    erlang:nif_error(nif_not_loaded).
