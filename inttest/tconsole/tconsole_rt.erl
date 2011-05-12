-module(tconsole_rt).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

files() ->
	[{copy, "console.erl", "src/console.erl"},
	 {create, "ebin/console_app.app", app(console_app, [console])}].

run(Dir) ->
	retest_log:log(debug, "Dir: ~p~n", [Dir]),
	?assertMatch({ok, _}, retest:sh("rebar get-deps compile -v")),
	Ref = retest:sh("erl -pa ebin -run console main", [{async,true}]),
	retest:sh_send(Ref, "World"),
	retest:sh_expect(Ref, ""),
	retest_log:log(debug, "STDOUT: ~p~n", StdOut),
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

