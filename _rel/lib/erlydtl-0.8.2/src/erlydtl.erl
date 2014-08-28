%%%-------------------------------------------------------------------
%%% File:      erlydtl.erl
%%% @author    Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @author    Evan Miller <emmiller@gmail.com>
%%% @copyright 2008 Roberto Saccon, Evan Miller
%%% @doc
%%% Public interface for ErlyDTL
%%% @end
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2008 Roberto Saccon, Evan Miller
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
%%%
%%% @since 2007-11-11 by Roberto Saccon, Evan Miller
%%%-------------------------------------------------------------------
-module(erlydtl).
-author('rsaccon@gmail.com').
-author('emmiller@gmail.com').

%% API
-export([compile/2, compile/3]).
-export([compile_dir/2, compile_dir/3]).

-type error_info() :: {File::list(),
                       [{Line::integer() | none,
                         Module::atom(),
                         ErrorDesc::term()}]}.
-type errors() :: list(error_info()).
-type warnings() :: list(error_info()).
-type ok_ret() :: {ok, Module::atom()} | {ok, Module::atom(), warnings()}.
-type err_ret() :: error | {error, errors(), warnings()}.

-spec compile( list() | binary(), atom() ) -> {ok, Module::atom()} | error.
compile(FileOrBinary, Module) ->
    erlydtl_compiler:compile(FileOrBinary, Module).

-spec compile( list() | binary(), atom(), list() ) -> ok_ret() | err_ret().
compile(FileOrBinary, Module, Options) ->
    erlydtl_compiler:compile(FileOrBinary, Module, Options).

-spec compile_dir(list() | binary(), atom()) -> {ok, Module::atom()} | error.
compile_dir(DirectoryPath, Module) ->
    erlydtl_compiler:compile_dir(DirectoryPath, Module).

-spec compile_dir(list() | binary(), atom(), list()) -> ok_ret() | err_ret().
compile_dir(DirectoryPath, Module, Options) ->
    erlydtl_compiler:compile_dir(DirectoryPath, Module, Options).