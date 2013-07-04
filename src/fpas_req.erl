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

-module(fpas_req).
-author('ae.nesterov@gmail.com').

% API
-export([
	options/2,
	onresponse/4
]).

%% ==================================================================
%% API 
%% ==================================================================

options(Req, State) ->
	Req2 = cowboy_req:set_resp_header(<<"access-control-allow-headers">>, <<"x-requested-with, content-type">>, Req),
	Req3 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"GET, OPTIONS">>, Req2),
	Req4 = cowboy_req:set_resp_header(<<"access-control-max-age">>, <<"1728000">>, Req3),
	{ok, Req4, State}.

onresponse(Status, Headers, Body, Req) ->
	{Origin, Req2} = cowboy_req:header(<<"origin">>, Req, <<"*">>),
	Headers2 = [
		{<<"access-control-allow-credentials">>, <<"true">>},
		{<<"access-control-allow-origin">>, Origin}
		| Headers
	],
	{ok, Req3} = cowboy_req:reply(Status, Headers2, Body, Req2),
	Req3.

