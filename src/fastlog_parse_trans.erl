%% -----------------------------------------------------------------------------
%%
%% Copyright (c) 2008-2010 Tim Watson (watson.timothy@gmail.com)
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%%
%% -----------------------------------------------------------------------------

-module(fastlog_parse_trans).

-compile(export_all).
%%-export([parse_transform/2]).

-include_lib("fastlog/include/fastlog.hrl").

parse_transform__bad(Forms, Options) ->
    {NewForms,_} =
        parse_trans:depth_first(fun xform_fun/4,
                parse_trans:initial_context(Forms, Options), Forms, Options),
    parse_trans:revert(NewForms).

parse_transform(Forms, Options) ->
    parse_trans:top(fun do_transform/2, Forms, Options).

do_transform(Forms, Context) ->
    {Forms2, _Acc2} =
        parse_trans:do_transform(fun xform_fun/4, [], Forms, Context),
    parse_trans:revert(Forms2).

xform_fun(application, Form, Ctx, Acc) ->
    case erl_syntax_lib:analyze_application(Form) of
        {fastlog, {LogFunc, 2}} ->
            Args = erl_syntax:application_arguments(Form),
            Mod = parse_trans:context(module, Ctx),
            Func = parse_trans:context(function, Ctx),
            Arity = parse_trans:context(arity, Ctx),
            Line = erl_syntax:get_pos(Form),
            io:format("[Function Application]  ~p~n[Args]  ~p~n[Extras]  ~p~n",
                        [Syn, Args, [Mod, Func, Arity, Line]]),

            %% TODO: generate a #'fastlog.entry'{} record and pass that....

            %% FunctionApplication =
            %%    erl_syntax:application(erl_syntax:atom(fastlog), )
            {[], Form, [], true, Acc};
        _ ->
            {[], Form, [], true, Acc}
    end;
xform_fun(Thing, Form, Ctx, Acc) ->
    io:format("[Thing]  ~p~n"
              "[Form]  ~p~n", [Thing, Form]),
    {[], Form, [], true, Acc}.

