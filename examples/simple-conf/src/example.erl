-module(example).
-compile({parse_transform, fastlog_parse_trans}).

-export([demo/0, demo1/0, demo2/0, demo3/0, demo4/0, demo5/0]).

-include_lib("fastlog/include/fastlog.hrl").

-fastlog({parse_trans.verbose, false}).	

demo() ->
    Msg = {from, ?MODULE},
    fastlog:debug("hello world ~p~n", [Msg]).

demo1() ->
    fastlog:debug(call_this_logger_please, 
                  "This is madness doctor, madness!~n").

demo2() ->
    fastlog:debug('com.acme.examples',
                  "This is madness ~s, ~p!~n", ["Doctor", madness]).
demo3() ->
    fastlog:debug(com.acme.examples, "This is madness doctor, madness!~n").

demo4() ->
    fastlog:debug(com.acme.more.examples, "This is still madness doctor!!~n").

demo5() ->
    fastlog:debug(#'fastlog.entry'{
                    dest=example,
                    message="[~p] [~p] [~p] [~p/~p] [line:~p]",
                    args=[{from, example}],
                    site=#'fastlog.callsite'{
                        node=node(),
                        pid=self(),
                        module=?MODULE,
                        function=demo3,
                        arity=0,
                        line=?LINE
                    }
                  }).
