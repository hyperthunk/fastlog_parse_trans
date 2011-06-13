-module(console).
-compile(export_all).
-include_lib("fastlog_parse_trans/include/fastlog.hrl").
-fastlog({parse_trans.verbose, true}).

main() ->
    fastlog:start(),
    fastlog:add_logger(?MODULE, debug),
    Logger = 'fastlog.special.logger',
    fastlog:add_logger(Logger, debug),
    hello("World"),
    goodbye("foo"),
    warn_number(33),
    info(),
    log(Logger),
    ok.

hello(World) ->
    ?DEBUG("Hello ~s~n", [World]),
    ok.

goodbye(Name) ->
    ?ERROR("Goodbye ~s~n", [Name]).

warn_number(N) ->
    ?WARN("Warning #~p~n", [N]).

info() ->
    ?INFO("INFO MSG ~p~n", [123]).

log(Logger) ->
    fastlog:info(Logger, "Log ~s~n", ["this"]).
