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

-module(shaman_processes).
-behaviour(alien_process_probe).
-behaviour(gen_server).

-export([start_link/2]).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
	ref :: alien:ref(),
	interval :: pos_integer()
}).

start_link(Ref, []) ->
	gen_server:start_link(?MODULE, [Ref], []).

init([Ref]) ->
	Interval = 1000, %% @todo Config.
	_ = erlang:send_after(Interval, self(), update),
	{ok, #state{ref=Ref, interval=Interval}}.

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

%% @todo messages links dictionary error_handler
%% group_leader total_heap_size heap_size stack_size
%% suspending?
handle_info(update, State=#state{ref=Ref, interval=Interval}) ->
	alien:event(Ref, [begin
		Info = process_info(P),
		{_, {CurMod, CurFun, CurAri}}
			= lists:keyfind(current_function, 1, Info),
		{_, {InitMod, InitFun, InitAri}}
			= lists:keyfind(initial_call, 1, Info),
		{_, MsgQ} = lists:keyfind(message_queue_len, 1, Info),
		{_, Priority} = lists:keyfind(priority, 1, Info),
		{_, Reductions} = lists:keyfind(reductions, 1, Info),
		{_, Status} = lists:keyfind(status, 1, Info),
		{_, TrapExit} = lists:keyfind(trap_exit, 1, Info),
		%% GC.
		{_, GC} = lists:keyfind(garbage_collection, 1, Info),
		%% @todo what about min_bin_vheap_size and minor_gcs
		{_, GCMinHeapSize} = lists:keyfind(min_heap_size, 1, GC),
		{_, GCFullsweepAfter} = lists:keyfind(fullsweep_after, 1, GC),
		%% --
		{_, Memory} = process_info(P, memory),
		RegName = case process_info(P, registered_name) of
			[] -> <<>>;
			{_, AtomName} -> a(AtomName)
		end,
		[
			{<<"pid">>, list_to_binary(pid_to_list(P))},
			{<<"current">>, << (a(CurMod))/binary,
				":", (a(CurFun))/binary,
				"/", (integer_to_binary(CurAri))/binary >>},
			{<<"gc_min_heap_size">>, GCMinHeapSize},
			{<<"gc_fullsweep_after">>, GCFullsweepAfter},
			{<<"init">>, << (a(InitMod))/binary,
				":", (a(InitFun))/binary,
				"/", (integer_to_binary(InitAri))/binary >>},
			{<<"mem">>, Memory},
			{<<"msgq">>, MsgQ},
			{<<"priority">>, a(Priority)},
			{<<"reds">>, Reductions},
			{<<"reg_name">>, RegName},
			{<<"status">>, a(Status)},
			{<<"trap_exit">>, a(TrapExit)}
		]
	end || P <- processes(), is_process_alive(P)]),
	_ = erlang:send_after(Interval, self(), update),
	{noreply, State};
handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% Internal.

a(Atom) ->
	atom_to_binary(Atom, utf8).
