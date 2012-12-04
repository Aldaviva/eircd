-module(eircd_users).
-export([create/2, find_by_socket/1, find_by_nickname/1, save/1, delete/1]).
-include("common.hrl").

create(Socket, Nickname) ->
	Process = spawn(fun()-> run(empty) end),
	State = #user{
		socket = Socket,
		nickname = Nickname,
		process = Process
	},
	save(State),
	ets:insert(users, [{Socket, Process, Nickname}]),
	State.

find_by_socket(Socket) ->
	Matches = ets:lookup(users, Socket),
	case Matches of
		[{Socket, Process, _}] -> 
			Process ! {self(), read},
			receive
				User ->
					User
			end;
		[] ->
			none
	end.

find_by_nickname(Nickname) ->
	Processes = ets:match(users, {'_', '$0', Nickname}),
	case Processes of
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
	ets:delete(users, User#user.socket),
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
			Invoker ! ok
	end.

get_user_process(User) ->
	User#user.process.