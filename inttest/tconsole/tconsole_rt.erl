-module(tconsole_rt).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("hamcrest/include/hamcrest.hrl").

files() ->
	[{copy, "console.erl", "src/console.erl"},
	 {create, "ebin/console_app.app", app(console_app, [console])}].

run(Dir) ->
	retest_log:log(debug, "Dir: ~p~n", [Dir]),
	?assertMatch({ok, _}, retest:sh("rebar get-deps compile -v")),
	Ref = retest:sh("erl -pa ebin -s console main", [{async, true}]),
	{ok, Hello} = retest:sh_expect(Ref,
	                "\\[nonode@nohost\\] \\[<.*>\\] \\[warn\\] \\[console\\] "
	                "\\[hello\\] \\[line:19\\] Hello World",
	                [{capture, all, list}]),
    retest_log:log(debug, "[CAPTURED]: ~s~n", [Hello]),
	{ok, Goodbye} = retest:sh_expect(Ref,
	                "\\[nonode@nohost\\] \\[<.*>\\] \\[error\\] \\[console\\] "
	                "\\[goodbye\\] \\[line:23\\] Goodbye foo",
	                [{capture, all, list}]),
    retest_log:log(debug, "[CAPTURED]: ~s~n", [Goodbye]),
    {ok, Warning} = retest:sh_expect(Ref,
                    "\\[nonode@nohost\\] \\[<.*>\\] \\[warn\\] \\[console\\] "
                    "\\[warn_number\\] \\[line:26\\] Warning #33",
	                [{capture, all, list}]),
	retest_log:log(debug, "[CAPTURED]: ~s~n", [Warning]),
    {ok, Info} = retest:sh_expect(Ref,
	                "\\[nonode@nohost\\] \\[<.*>\\] \\[info\\] \\[console\\] "
	                "\\[info\\] \\[line:29\\] INFO MSG 123",
	                [{capture, all, list}]),
	retest_log:log(debug, "[CAPTURED]: ~s~n", [Info]),
    retest_sh:kill(Ref),
	ok.

%%
%% Generate the contents of a simple .app file
%%
app(Name, Modules) ->
    App = {application, Name,
           [{description, atom_to_list(Name)},
            {vsn, "1"},
            {modules, Modules},
            {registered, []},
            {applications, [kernel, stdlib]}]},
    io_lib:format("~p.\n", [App]).

