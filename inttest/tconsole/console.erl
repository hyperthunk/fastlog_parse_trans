-module(console).
-compile(export_all).
-include_lib("fastlog_parse_trans/include/fastlog.hrl").
-fastlog({parse_trans.verbose, true}).

main(_) ->
	%% = init:get_plain_arguments(),
	fastlog:start(),
	fastlog:add_logger(?MODULE),
	Logger = list_to_atom(io:get_line("logger? >>")),
	fastlog:add_logger(Logger),
	hello(io:get_line("hello? >>")),
	goodbye(io:get_line("goodbye? >>")),
	warn_number(list_to_integer(io:get_line("warn_number? >>"))),
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
