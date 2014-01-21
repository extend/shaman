%% Copyright (c) 2014, Loïc Hoguin <essen@ninenines.eu>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(ws_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

init(_, _, _) ->
	shaman:sub(),
	true = gproc:reg({p, l, shaman_clients}),
	{upgrade, protocol, cowboy_websocket}.

websocket_init(_, Req, _) ->
	shaman_nodes:update(),
	{ok, Req, undefined}.

websocket_handle({text, JSON}, Req, State) ->
	Array = jsx:decode(JSON),
	{_, Type} = lists:keyfind(<<"t">>, 1, Array),
	case handle(Type, Array) of
		noreply ->
			{ok, Req, State};
		Resp ->
			{reply, {text, jsx:encode(Resp)}, Req, State}
	end.

%% Events coming from shaman itself.
websocket_info({shaman, Name, Data}, Req, State) ->
	{reply, {text, jsx:encode([
		[
			{<<"t">>, <<"data">>},
			{<<"n">>, atom_to_binary(Name, utf8)},
			{<<"d">>, Data}
		]
	])}, Req, State};
%% Events coming from alien probes.
websocket_info({shaman, Node, Name, Data}, Req, State) ->
	{reply, {text, jsx:encode([
		[
			{<<"t">>, <<"data">>},
			{<<"n">>, atom_to_binary(Name, utf8)},
			{<<"d">>, [
				{<<"node">>, atom_to_binary(Node, utf8)},
				{<<"data">>, Data}
			]}
		]
	])}, Req, State};
websocket_info(Info, Req, State) ->
	io:format("~p~n", [Info]),
	{noreply, Req, State}.

websocket_terminate(_, _, _) ->
	ok.

%% Requests handler.

handle(<<"nodes.connect">>, Array) ->
	{_, Name} = lists:keyfind(<<"name">>, 1, Array),
	{_, Cookie} = lists:keyfind(<<"cookie">>, 1, Array),
	Name2 = binary_to_atom(Name, utf8),
	Cookie2 = binary_to_atom(Cookie, utf8),
	erlang:set_cookie(Name2, Cookie2),
	case net_adm:ping(Name2) of
		pong ->
			shaman_nodes:update(),
			noreply;
		pang ->
			error_reply(<<"nodes">>, <<"Failed to connect to node.">>)
	end;
handle(Type, Array) ->
	io:format("~p ~p~n", [Type, Array]),
	noreply.

%% @todo
error_reply(_, _) ->
	noreply.
