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

-module(fpas_xml).
-author('ae.nesterov@gmail.com').

-include_lib("xmerl/include/xmerl.hrl").

% API
-export([
	encode_error/1,
	is_xml/1
]).

%% ==================================================================
%% API 
%% ==================================================================

encode_error(Reason) ->
	<<"<error>", Reason/binary, "</error>">>.

is_xml(Xml) ->
	try
		{Doc, _} = xmerl_scan:string(erlang:binary_to_list(Xml), [{document, true}]),
		erlang:is_record(Doc, xmlDocument)
	catch
		_:_ -> false
	end.

