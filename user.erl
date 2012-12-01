-module(user).
-export([create/2, find/1, save/1, delete/1]).
-include("constants.hrl").

create(Socket, Nickname) ->
	%make a new user record and set some fields, but don't persist it, just return it.
	%starts the user's actor process, registering PID in table and listening for messages
	unimplemented.

find(Socket) ->
	%look up and return user based on the primary key (socket)
	unimplemented.

save(User) ->
	%write user into state, don't return it
	unimplemented.

delete(User) ->
	%stop process and remote PID from table
	unimplemented.