-module(example).
-compile({parse_transform, fastlog_parse_trans}).

-export([demo/0, demo2/0]).

-include_lib("fastlog/include/fastlog.hrl").

demo() ->
    fastlog:debug("hello world ~p~n", [{from, ?MODULE}]).

demo2() ->
    fastlog:debug(?MODULE, "This is madness ~s!~n", ["right here"]).
