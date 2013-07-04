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

-module(fpas_tests).
-author('ae.nesterov@gmail.com').

-include_lib("eunit/include/eunit.hrl").

%% ==================================================================
%% Templates, /templates[/:template]
%% ==================================================================

templates(Config) ->
	{inparallel, [
		{"/api/templates", templates_list(Config)},
		{"/api/templates/:template", template(Config)}
	]}.

templates_list(Config) ->
	C   = proplists:get_value(client, Config),
	Url = proplists:get_value(url_builder, Config),
	{Status, Headers, Body, _} = th_http:request(<<"GET">>, Url(<<"/api/templates">>), C),
	[
		th_http:check_success_status_code(Status),
		th_http:check_content_json_list(Headers, Body),
		th_http:check_cors_accessible_response(Headers)
	].

template(Config) ->
	[
		{"template is exist",
			template(<<"/api/templates/rtsx">>, fun th_http:check_success_status_code/1, Config)},
		{"template doesn't exist",
			template(<<"/api/templates/qwerty">>, fun th_http:check_notfound_status_code/1, Config)}
	].

template(Path, CheckStatus, Config) ->
	[
		{"'accept' header didn't set",
			template(Path, [], CheckStatus, fun th_http:check_content_json/2, Config)},
		{"'accept' header is 'application/json'",
			template(Path, [{<<"accept">>, <<"application/json">>}], CheckStatus, fun th_http:check_content_json/2, Config)},
		{"'accept' headers is 'application/xml'",
			template(Path, [{<<"accept">>, <<"application/xml">>}], CheckStatus, fun th_http:check_content_xml/2, Config)}
	].

template(Path, ReqHeaders, CheckStatus, CheckContentType, Config) ->
	C   = proplists:get_value(client, Config),
	Url = proplists:get_value(url_builder, Config),
	{Status, Headers, Body, _} = th_http:request(<<"GET">>, Url(Path), ReqHeaders, C),
	[
		CheckStatus(Status),
		CheckContentType(Headers, Body),
		th_http:check_cors_accessible_response(Headers)
	].

%% ==================================================================
%% Messages, /templates/:template/decode
%% ==================================================================

messages(Config) ->
	ContentTypeHeader = {<<"content-type">>, <<"application/octet-stream">>},
	{ok, RightMsg} = th:read_data_file("fast-message-example"),
	{ok, WrongMsg} = th:read_data_file("fast-message-example-broken"),

	{"/api/templates/:template/decode", [
		{"Right payload", message([ContentTypeHeader], RightMsg, fun th_http:check_success_status_code/1,              Config)},
		{"Wrong payload", message([ContentTypeHeader], WrongMsg, fun th_http:check_unprocessable_entity_status_code/1, Config)},
		{"Empty payload", message([ContentTypeHeader], <<>>,     fun th_http:check_bad_request_status_code/1,          Config)}
	]}.

message(ReqHeaders, ReqBody, CheckStatus, Config) ->
	C   = proplists:get_value(client, Config),
	Url = proplists:get_value(url_builder, Config),
	{Status, Headers, Body, _} = th_http:request(<<"POST">>, Url(<<"/api/templates/rtsx/decode">>), ReqHeaders, ReqBody, C),
	[
		CheckStatus(Status),
		th_http:check_content_json(Headers, Body),
		th_http:check_cors_accessible_response(Headers)
	].

%% ==================================================================
%% Not Resource
%% ==================================================================

notresource(Config) ->
	{"Redirecting to documentation page ", 
		[{lists:flatten(io_lib:format("For path: ~s", [Path])), notresource(Path, Config)} || Path <- [
			<<"/qwerty">>,
			<<"/api">>,
			<<"/api/qwerty">>
		]]}.

notresource(Path, Config) ->
	C   = proplists:get_value(client, Config),
	Url = proplists:get_value(url_builder, Config),
	{Status, Headers, Body, _} = th_http:request(<<"GET">>, Url(Path), C),
	[
		th_http:check_see_other_status_code(Status),
		th_http:check_header(<<"location">>, Headers),
		th_http:check_content_json(Headers, Body),
		th_http:check_cors_accessible_response(Headers)
	].

%% ==================================================================
%% Test suite setup
%% ==================================================================

suite_main(Config) ->
	{"REST API", {inparallel, [
		{"Templates", templates(Config)},
		{"Messages",  messages(Config)},
		{"Not resource", notresource(Config)}
	]}}.

suite_begin() ->
%	ok = application:start(crypto),
	ok = application:start(ranch),
	ok = application:start(cowboy),
	ok = application:start(jsx),
	ok = application:start(fastprot),
	ok = application:start(fpas),

	{ok, Client} = cowboy_client:init([]),
	Url = fun(Url) -> th_http:build_url(<<"http">>, <<"localhost">>, ranch:get_port(http), Url) end,
	
	[{client, Client}, {url_builder, Url}].

suite_end(_Config) ->
	application:stop(fpas),
	application:stop(fastprot),
	application:stop(cowboy),
	application:stop(jsx),
	application:stop(ranch),
%	application:stop(crypto),
	ok.

%% ==================================================================
%% Test suite run function
%% ==================================================================

fpas_test_() ->
	{setup,
		fun suite_begin/0,
		fun suite_end/1,
		fun suite_main/1
	}.

