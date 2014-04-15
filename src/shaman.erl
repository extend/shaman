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

%% @todo Use gproc for this.
-module(shaman).

-export([pub/2]).
-export([pub/3]).

-export([sub/0]).
-export([sub/1]).

pub(Name, Data) ->
	pub(node(), Name, Data).

pub(Node, Name, Data) ->
	_ = [Pid ! {shaman, Node, Name, Data}
		|| {Pid, sub} <- ets:tab2list(shaman_subs)],
	ok.

sub() ->
	sub(self()).

sub(Pid) ->
	true = ets:insert(shaman_subs, {Pid, sub}),
	ok.
