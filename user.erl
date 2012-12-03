-module(user).
-export([create/2, find/1, save/1, delete/1]).
-include("constants.hrl").

create(Socket, Nickname) ->
	Process = spawn(fun()-> run(empty) end),
	State = #user{
		socket = Socket,
		nickname = Nickname,
		process = Process
	},
	save(State),
	ets:insert(users, {Socket, Process}),
	State.

find(Socket) ->
	Matches = ets:lookup(users, Socket),
	case Matches of
		[Process] -> 
			Process ! {self(), read},
			receive
				User ->
					User
			end;
		[] ->
			none
	end.

save(User) ->
	Process = get_user_process(User),
	Process ! {self(), save, User},
	receive ok -> ok end.

delete(User) ->
	Process = get_user_process(User),
	Process ! {self(), delete},
	ets:delete(users, State#user.socket),
	receive ok -> ok end.

run(State) ->
	receive
		{Invoker, read} ->
			Invoker ! State,
			run(State);
		{Invoker, save, NewState} ->
			Invoker ! ok,
			run(NewState);
		{Invoker, delete} ->
			Invoker ! ok; %don't recur
	end.

get_user_process(User) ->
	User#user.process.