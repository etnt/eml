%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% Copyright (c) 2012 Torbjorn Tornkvist (etnt@redhoterlang.com)
%%
%% This file is based upon rebar_lfe_compiler.erl from the
%% Rebar project, which had the following notice:
%%
%% Copyright (c) 2009 Dave Smith (dizzyd@dizzyd.com),
%%                    Tim Dysinger (tim@dysinger.net)
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
%% -------------------------------------------------------------------

-module(rebar_eml_plugin).

-export([compile/2]).


%% ===================================================================
%% Public API
%% ===================================================================

compile(Config, _AppFile) ->
    FirstFiles = rebar_config:get_list(Config, lfe_first_files, []),
    rebar_base_compiler:run(Config, FirstFiles, "src", ".eml", "ebin", ".beam",
                            fun compile_eml/3).

%% ===================================================================
%% Internal functions
%% ===================================================================

compile_eml(Source, _Target, Config) ->
    case code:which(eml) of
        non_existing ->
            rebar_utils:abort(
              "~n"
              "*** MISSING EML COMPILER ***~n"
              "  You must do one of the following:~n"
              "    a) Install EML globally in your erl libs~n"
              "    b) Add EML as a dep for your project, eg:~n"
              "       {eml, \"0.*\",~n"
              "        {git, \"git://github.com/etnt/eml\",~n"
              "         \"HEAD\"}}~n"
              "~n"
              , []);
        _ ->
            Opts = [{i, "include"}, {outdir, "ebin"}, report]
                ++ rebar_config:get_list(Config, erl_opts, []),
            try {ok,_} = eml:compile_file(Source, Opts), ok
            catch Class:Error ->
                    rebar_utils:abort("~p: EML compilation failed: ~p:~p~n~p~n",
                                      [Source, Class, Error, erlang:get_stacktrace()])
            end
    end.
