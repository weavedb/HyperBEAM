-module(dev_mul_nif).
-export([multiply/2]).

-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-on_load(init/0).

-define(NOT_LOADED, not_loaded(?LINE)).
not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).


init() ->
    PrivDir = code:priv_dir(hb),
    Path = filename:join(PrivDir, "dev_mul"),
    case erlang:load_nif(Path, 0) of
	ok -> ok;
	{error, Reason} -> exit({load_failed, Reason})
    end.

multiply(_A, _B) ->
    not_loaded(?LINE).
