%% ------------------------------------------------------------------
%% Copyright 2013 by Andrei Nesterov (ae.nesterov@gmail.com)
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to
%% deal in the Software without restriction, including without limitation the
%% rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
%% sell copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
%% IN THE SOFTWARE.
%% ------------------------------------------------------------------

-module(notfound_handler).
-behaviour(cowboy_http_handler).
-author('ae.nesterov@gmail.com').

%% Base handler callbacks
-export([
	init/3,
	handle/2,
	terminate/3
]).

%% ==================================================================
%% Base handler callbacks
%% ==================================================================

init({tcp, http}, Req, _Opts) ->
	{ok, Req, undefined_state}.

handle(Req, State) ->
	{ok, Req2} = cowboy_req:reply(
		303,
		[
			{<<"location">>, <<"http://manifest.github.io/fastprot-analyzer-backend/manual.html">>},
			{<<"content-type">>, <<"application/json">>}
		],
		fpas_json:encode_data(<<"Redirecting to documentation page.">>),
		Req
	),
	{ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
	ok.

