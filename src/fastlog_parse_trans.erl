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
-export([parse_transform/2]).

-include_lib("fastlog/include/fastlog.hrl").

parse_transform(Forms, Options) ->
    parse_trans:top(fun do_transform/2, Forms, Options).

do_transform(Forms, Context) ->
    {Forms2, _Acc2} =
        parse_trans:do_transform(fun xform_fun/4, [], Forms, Context),
    parse_trans:revert(Forms2).

xform_fun(application, Form, Ctx, Conf) ->
    case erl_syntax_lib:analyze_application(Form) of
        {fastlog, {LogFunc, _ArgLength}} ->
            FuncApplic = transform_application(Form, LogFunc, Ctx, Conf),
            {[], FuncApplic, [], true, Conf};
        _ ->
            {[], Form, [], true, Conf}
    end;
xform_fun(attribute, {attribute,_LN,fastlog,{_,_}=Data}=Form, _Ctx, Conf) ->
    {[], Form, [], true, [Data|Conf]};
xform_fun(Thing, Form, _Ctx, Conf) ->
    progress_message(Conf,
              "[Thing]  ~p~n"
              "[Form]  ~p~n", [Thing, Form]),
    {[], Form, [], true, Conf}.

transform_application(Form, LogFunc, Ctx, Conf) ->
    Args = erl_syntax:application_arguments(Form),
    Mod = parse_trans:context(module, Ctx),
    Func = parse_trans:context(function, Ctx),
    Arity = parse_trans:context(arity, Ctx),
    L0 = erl_syntax:get_pos(Form),
    io:format("[Function Application]  ~p~n[Args]  ~p~n[Extras]  ~p~n",
                [LogFunc, Args, [Mod, Func, Arity, L0]]),
    case Args of
        [{var, _, _}=Var, {string, _, _Str}=Msg|[]] ->
			call_logger(LogFunc, Var, Msg, 
                        undef(L0), Form, Ctx, Conf);
        [{var, _, _}=Var, {string, _, _Str}=Msg, Params|[]] ->
			call_logger(LogFunc, Var, Msg, 
                        Params, Form, Ctx, Conf);
		[{string, _, _Msg}, Msg, Params|[]] ->
            call_logger(LogFunc, dest(Mod), Msg, 
                        Params, Form, Ctx, Conf);
        [{string, _, _Str}=Msg, Params|[]] ->
            call_logger(LogFunc, dest(Mod), Msg, 
                        Params, Form, Ctx, Conf);
        [{atom, _, Logger}, Msg|[]] ->
            call_logger(LogFunc, Logger, Msg,
                        undef(L0), Form, Ctx, Conf);
        [{atom, _, Logger}, Msg, Params|[]] ->
            call_logger(LogFunc, Logger, Msg,
                        Params, Form, Ctx, Conf);
        [{record_field, _, _, _}=QualName, Msg|[]] ->
            Dest = dest(QualName),
            call_logger(LogFunc, Dest, Msg, 
                        undef(L0), Form, Ctx, Conf);
        [{record_field, _, _, _}=QualName, Msg, Params|[]] ->
            Dest = dest(QualName),
            call_logger(LogFunc, Dest, Msg, 
                        Params, Form, Ctx, Conf);
        Other ->
            io:format("[OTHER]  ~p~n", [Other]),
            Form
    end.

call_logger(LogFunc, Dest, Msg, Args, Form, Ctx, _Conf) ->
    Mod = parse_trans:context(module, Ctx),
    Func = parse_trans:context(function, Ctx),
    Arity = parse_trans:context(arity, Ctx),
    L0 = erl_syntax:get_pos(Form),
    Logger = case Dest of 
		X when is_atom(X) ->
			{atom, L0, X};
		{var, _, _} ->
			Dest
	end,
	{call, L0,
        {remote, L0, {atom, L0, fastlog},
            {atom, L0, LogFunc}},
            [{record,L0,'fastlog.entry',
                [{record_field,L0,
                     {atom,L0,message},Msg},
                 {record_field,L0,
                     {atom,L0,args},Args},
                 {record_field,L0,
                     {atom,L0,dest},Logger},
                 {record_field,L0,
                     {atom,L0,site},
                 {record,L0,'fastlog.callsite',
                     [{record_field,L0,
                          {atom,L0,node},
                          {call,L0,{atom,L0,node},[]}},
                      {record_field,L0,
                          {atom,L0,pid},
                          {call,L0,{atom,L0,self},[]}},
                      {record_field,L0,{atom,L0,module},{atom,L0,Mod}},
                      {record_field,L0,{atom,L0,function},{atom,L0,Func}},
                      {record_field,L0,{atom,L0,arity},{integer,L0,Arity}},
                      {record_field,L0,{atom,L0,line},{integer,L0,L0}}]}}]}]}.

dest({record_field, _LN2, _, _}=QualName) ->
    Segments = erl_syntax:qualified_name_segments(QualName),
    Parts = [ atom_to_list(X) || {atom, _, X} <- Segments ],
    dest({tokens, Parts});
dest(Name) when is_atom(Name) ->
    dest(atom_to_list(Name));
dest(Name) when is_list(Name) ->
    dest({tokens, string:tokens(Name, "_")});
dest({tokens, Tokens}) ->
    list_to_atom(lists:foldl(
        fun(E, Acc) ->
            case Acc of
                [] ->
                    E;
                _ ->
                    string:join([E, Acc], ".")
            end
        end, "",
        lists:reverse(Tokens))).

undef(L0) ->
    {atom,L0,undefined}.

progress_message(Config, Msg, Args) ->
    case proplists:get_value('parse_trans.verbose', Config) of
        true ->
            io:format(Msg, Args);
        _ ->
            ok
    end.
