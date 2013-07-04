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

-module(th_http).
-author('ae.nesterov@gmail.com').

-include_lib("eunit/include/eunit.hrl").

%% Helpers
-export([
	build_url/4,
	request/3,
	request/4,
	request/5
]).

%% Asserts
-export([
	check_success_status_code/1,
	check_bad_request_status_code/1,
	check_notfound_status_code/1,
	check_unprocessable_entity_status_code/1,
	check_see_other_status_code/1,
	check_header/2,
	check_header/3,
	check_body/1,
	check_body_json/1,
	check_body_xml/1,
	check_content_json/2,
	check_content_json_list/2,
	check_content_xml/2,
	check_cors_accessible_response/1
]).

%% ==================================================================
%% Build Url helper
%% ==================================================================

build_url(Scheme, Host, Port, Path) when is_list(Scheme) ->
	SchemeBin = erlang:list_to_binary(Scheme),
	build_url(SchemeBin, Host, Port, Path);

build_url(Scheme, Host, Port, Path) when is_list(Host) ->
	HostBin = erlang:list_to_binary(Host),
	build_url(Scheme, HostBin, Port, Path);

build_url(Scheme, Host, Port, Path) when is_integer(Port) ->
	PortStr = erlang:integer_to_list(Port),
	build_url(Scheme, Host, PortStr, Path);

build_url(Scheme, Host, Port, Path) when is_list(Port) ->
	PortBin = erlang:list_to_binary(Port),
	build_url(Scheme, Host, PortBin, Path);

build_url(Scheme, Host, Port, Path) when is_list(Path) ->
	PathBin = erlang:list_to_binary(Path),
	build_url(Scheme, Host, Port, PathBin);

build_url(Scheme, Host, Port, Path) ->
	<< Scheme/binary, "://", Host/binary, ":", Port/binary, Path/binary >>.

%% ==================================================================
%% Request helper
%% ==================================================================

request(Method, Url, Client) ->
	request(Method, Url, [], Client).

request(Method, Url, Headers, Client) ->
	request(Method, Url, Headers, <<>>, Client).

request(Method, Url, Headers, Body, Client) ->
	{ok, C} = cowboy_client:request(Method, Url, Headers, Body, Client),
	{ok, RespStatus, RespHeaders, C2} = cowboy_client:response(C),
	case proplists:get_value(<<"content-length">>, RespHeaders) of
		L when is_binary(L) andalso L =/= <<"0">> -> 
			{ok, RespBody, C3} = cowboy_client:response_body(C2),
			{RespStatus, RespHeaders, RespBody, C3};
		_L ->
			{RespStatus, RespHeaders, <<>>, C2}
	end.

%% ==================================================================
%% Repeated test cases
%% ==================================================================

check_success_status_code(Status) ->
	{"Success status", 
		?_assertEqual(200, Status)}.

check_bad_request_status_code(Status) ->
	{"Bad Request status", 
		?_assertEqual(400, Status)}.

check_notfound_status_code(Status) ->
	{"Not Found status", 
		?_assertEqual(404, Status)}.

check_unprocessable_entity_status_code(Status) ->
	{"Unprocessable Entity status", 
		?_assertEqual(422, Status)}.
		
check_see_other_status_code(Status) ->
	{"See Other status", 
		?_assertEqual(303, Status)}.

check_header(Header, Headers) ->
	{lists:flatten(io_lib:format("Has '~s' header", [Header])),
		?_assertNotEqual(undefined, proplists:get_value(Header, Headers))}.

check_header(Header, Value, Headers) ->
	{lists:flatten(io_lib:format("Has '~s' header, and its value is '~s'", [Header, Value])),
		?_assertEqual(Value, proplists:get_value(Header, Headers))}.

check_body(Body) ->
	{"Has body",
		?_assert(byte_size(Body) > 0)}.

check_body_json(Body) ->
	check_body(Body),
	{"Body in json format",
		?_assert(fpas_json:is_json(Body))}.

check_body_xml(Body) ->
	check_body(Body),
	{"Body in xml format",
		?_assert(fpas_xml:is_xml(Body))}.

check_content_json(Headers, Body) ->
	[
		check_header(<<"content-type">>, <<"application/json">>, Headers),
		check_body_json(Body)
	].

check_content_xml(Headers, Body) ->
	[
		check_header(<<"content-type">>, <<"application/xml">>, Headers),
		check_body_xml(Body)
	].

check_content_json_list(Headers, Body) ->
	[
		check_content_json(Headers, Body),
		{"Body is json list",
			?_assert(erlang:is_list(jsx:decode(Body)))}
	].
	
check_cors_accessible_response(Headers) ->
	[
		{"Has 'access-control-allow-credentials' header",
			?_assertNotEqual(undefined, proplists:get_value(<<"access-control-allow-credentials">>, Headers))},
		{"Has 'access-control-allow-origin' header",
			?_assertNotEqual(undefined, proplists:get_value(<<"access-control-allow-origin">>, Headers))}
	].

