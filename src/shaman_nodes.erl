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

-module(shaman_nodes).
-behaviour(gen_server).

-export([start_link/0]).
-export([update/0]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
	interval :: non_neg_integer()
}).

%% API.

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

update() ->
	gen_server:cast(?MODULE, update).

%% gen_server.

init([]) ->
	Interval = 5000, %% @todo Config.
	_ = erlang:send_after(Interval, self(), update),
	{ok, #state{interval=Interval}}.

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast(update, State) ->
	shaman:pub(nodes, all()),
	{noreply, State};
handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(update, State=#state{interval=Interval}) ->
	shaman:pub(nodes, all()),
	_ = erlang:send_after(Interval, self(), update),
	{noreply, State};
handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% Internal.

all() ->
	[local_node()|[remote_node(N) || N <- nodes()]].

local_node() ->
	IsAlienStarted = is_alien_started(application:which_applications()),
	IsShamanEnabled = case IsAlienStarted of
		false ->
			false;
		true ->
			%% @todo alien:list_probes/0
			false
	end,
	[
		{name, atom_to_binary(node(), utf8)},
		{release, list_to_binary(erlang:system_info(otp_release))},
		{alien_started, IsAlienStarted},
		{shaman_enabled, IsShamanEnabled}
	].

remote_node(Node) ->
	IsAlienStarted = is_alien_started(rpc:call(Node,
		application, which_applications, [])),
	IsShamanEnabled = case IsAlienStarted of
		false ->
			false;
		true ->
			%% @todo alien:list_probes/0
			false
	end,
	[
		{name, atom_to_binary(Node, utf8)},
		{release, list_to_binary(
			rpc:call(Node, erlang, system_info, [otp_release]))},
		{alien_started, IsAlienStarted},
		{shaman_enabled, IsShamanEnabled}
	].

is_alien_started(Apps) when is_list(Apps) ->
	false =/= lists:keyfind(alien, 1, Apps);
is_alien_started(_) ->
	false.
