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

-module(messages_handler).
-author('ae.nesterov@gmail.com').

%% API
-export([
	message_from_blob/2,
	message_from_multipart_data/2,
	message_response/3,
	consume_multipart_data/1,
	consume_multipart_data/2
]).

%% REST handler callbacks
-export([
	allowed_methods/2, 
	resource_exists/2, 
	content_types_provided/2,
	content_types_accepted/2,
	malformed_request/2,
	options/2,
	rest_init/2
]).

%% Base handler callbacks
-export([init/3]).

%% State
-record(state, {
	template
}).

%% ==================================================================
%% API 
%% ==================================================================

consume_multipart_data(Disposition) ->
	consume_multipart_data(Disposition, <<>>).

consume_multipart_data({headers, _Headers, Req}, Acc) ->
	consume_multipart_data(cowboy_req:multipart_data(Req), Acc);

consume_multipart_data({body, Body, Req}, Acc) ->
	consume_multipart_data(cowboy_req:multipart_data(Req), <<Acc/binary, Body/binary>>);

consume_multipart_data({end_of_part, Req}, Acc) ->
	consume_multipart_data(cowboy_req:multipart_data(Req), Acc);

consume_multipart_data({eof, Req}, Acc) ->
	{Acc, Req}.

message_from_multipart_data(Req, State) ->
	{EncData, Req2} = consume_multipart_data(cowboy_req:multipart_data(Req)),
	message_response(EncData, Req2, State).

message_from_blob(Req, State) ->
	{ok, EncData, Req2} = cowboy_req:body(Req),
	message_response(EncData, Req2, State).
	
message_response(EncData, Req, State) ->
	{ok, Dec} = fastprot:decoder(State#state.template, [json_ex]),
	
	{Status, Json} = case fastprot:decode(Dec, EncData) of
		{ok,    DecData} -> {true,  DecData};
		{error, Reason}  -> {false, fpas_json:encode_error(Reason)}
	end,

	Req2 = cowboy_req:set_resp_body(Json, Req),
	{Status, Req2, State}.

%% ==================================================================
%% REST handler callbacks
%% ==================================================================

allowed_methods(Req, State) ->
	{[<<"POST">>, <<"OPTIONS">>], Req, State}.

% POST must have data payload
malformed_request(Req, State) ->
	{Method, Req2} = cowboy_req:method(Req),
	case Method of
		<<"POST">> ->
			case cowboy_req:has_body(Req2) of
				false ->
					Req3 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Req2),
					Json = fpas_json:encode_error(<<"Request does not contain data.">>),
					{true, cowboy_req:set_resp_body(Json, Req3), State};
				_Else ->
					{false, Req2, State}
			end;
		_Else -> {false, Req2, State}
	end.

resource_exists(Req, State) ->
	{Method, Req2} = cowboy_req:method(Req),
	{case Method of
		<<"POST">> -> false;
		_Else      -> true
	end, Req2, State}.

content_types_accepted(Req, State) ->
	{[
		{{<<"application">>, <<"octet-stream">>, '*'}, message_from_blob},
		{{  <<"multipart">>,    <<"form-data">>, '*'}, message_from_multipart_data}
	], Req, State}.

content_types_provided(Req, State) ->
	{[
		{{<<"application">>, <<"json">>, '*'}, none}
	], Req, State}.

options(Req, State) ->
	fpas_req:options(Req, State).

rest_init(Req, Opts) ->
	[TemplateBody] = Opts,
	{ok, Req, #state{template = TemplateBody}}.

%% ==================================================================
%% Base handler callbacks
%% ==================================================================

init(_Transport, _Req, _Opts) ->
	{upgrade, protocol, cowboy_rest}.

