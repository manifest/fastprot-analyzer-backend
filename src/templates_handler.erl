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

-module(templates_handler).
-author('ae.nesterov@gmail.com').

%% API
-export([
	template_to_xml/2,
	template_to_json/2,
	template_list_to_json/2
]).

%% REST handler callbacks
-export([
	allowed_methods/2, 
	resource_exists/2, 
	content_types_provided/2,
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

template_to_xml(Req, State) ->
	Xml = State#state.template,
	{Xml, Req, State}.

template_to_json(Req, State) ->
	Xml = State#state.template,
	{fpas_json:encode_data(Xml), Req, State}.

template_list_to_json(Req, State) ->
	Json = fpas_json:encode([<<"rtsx">>]),
	{Json, Req, State}.

%% ==================================================================
%% REST handler callbacks
%% ==================================================================

allowed_methods(Req, State) ->
	{[<<"GET">>, <<"OPTIONS">>], Req, State}.

content_types_provided(Req, State) ->
	{TemplateName, Req2} = cowboy_req:binding(template, Req),
	Types = case TemplateName of
		undefined -> [
				{{<<"application">>, <<"json">>, '*'}, template_list_to_json}
			];
		_Else -> [
				{{<<"application">>,  <<"json">>, '*'}, template_to_json},
				{{<<"application">>,  <<"xml">>, '*'}, template_to_xml}
			]
	end,
	{Types, Req2, State}.

resource_exists(Req, State) ->
	{ContentType, Req2}  = cowboy_req:header(<<"accept">>, Req),
	{TemplateName, Req3} = cowboy_req:binding(template, Req2),
	case TemplateName of
		undefined  -> {true, Req3, State};
		<<"rtsx">> -> {true, Req3, State};
		_Else      ->
			Reason = <<"Requested template does not exist.">>,
			Error = case ContentType of
				<<"application/xml">> -> fpas_xml:encode_error(Reason);
				_Else2                -> fpas_json:encode_error(Reason)
			end,
			{false, cowboy_req:set_resp_body(Error, Req3), State}
	end.

options(Req, State) ->
	fpas:options(Req, State).

rest_init(Req, Opts) ->
	[TemplateBody] = Opts,
	{ok, Req, #state{template = TemplateBody}}.

%% ==================================================================
%% Base handler callbacks
%% ==================================================================

init(_Transport, _Req, _Opts) ->
	{upgrade, protocol, cowboy_rest}.

