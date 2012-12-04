-module(eircd_channels).
-export([create/1, find/1, save/1, delete/1, add_user/2, filter/1]).
-include("common.hrl").

create(Name) ->
	case find(Name) of
		none ->
			Process = spawn(fun()-> run(empty) end),
			State = #channel{
				name = Name,
				process = Process
			},
			save(State),
			ets:insert(channels, [{Name, Process}]),
			State;
		State ->
			State
	end.

find(Name) ->
	Matches = ets:lookup(channels, Name),
	case Matches of
		[{Name, Process}] ->
			read(Process);
		[] ->
			none
	end.

save(Channel) ->
	Process = get_channel_process(Channel),
	Process ! {self(), save, Channel},
	receive ok -> ok end.

delete(Channel) ->
	UserCount = sets:size(Channel#channel.users),
	case UserCount of
		0 ->
			Process = get_channel_process(Channel),
			Process ! {self(), delete},
			receive
				ok -> 
					ets:delete(channels, Channel#channel.name),
					ok
			end;
		_ ->
			not_empty
	end.

add_user(Channel1, User) ->
	UsersInChannel1 = Channel1#channel.users,
	UsersInChannel2 = sets:add_element(User#user.socket, UsersInChannel1),
	Channel2 = Channel1#channel{users=UsersInChannel2},
	save(Channel2).

filter(Predicate) ->
	%sequential scan, not efficient
	AllProcesses = ets:match(channels, {'_', '$1'}),
	All = lists:map(fun read/1, AllProcesses),
	lists:filter(Predicate, All).

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

read(Process) ->
	Process ! {self(), read},
	receive
		Channel ->
			Channel
	end.

get_channel_process(Channel) ->
	Channel#channel.process.