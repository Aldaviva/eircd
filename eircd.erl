-module(eircd).
-export([start/0, start/1]).
-include("constants.hrl").

start() ->
	start(6667).
start(Port) ->
	create_users_table(),
	create_channels_table(),
	start_listener(Port),
	ok.

create_users_table() ->
	ets:new(users, [
		public,
		named_table,
		{read_concurrency, true}
	]).

create_channels_table() ->
	ets:new(channels, [
		public,
		named_table,
		{read_concurrency, true}
	]).

start_listener(Port) ->
	spawn_link(listener, listen, [Port]).