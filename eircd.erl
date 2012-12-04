-module(eircd).
-export([start/0, start/1]).
-include("common.hrl").

start() ->
	start("6667"). %cli args are always strings
start(PortStr) ->
	create_users_table(),
	create_channels_table(),
	start_listener(list_to_integer(PortStr)),
	receive
		quit ->
			ok
	end.

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
	spawn_link(eircd_listener, listen, [Port]).