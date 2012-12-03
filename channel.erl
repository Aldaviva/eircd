-module(channel).
-export([create/1, find/1, save/1, delete/1, add_user/2]).
-include("constants.hrl").

create(Name) ->
	Process = spawn(fun()-> run(empty) end),
	State = #channel{
		name = Name,
		creation_date = calendar:universal_time(),
		topic = "",
		topic_date = none,
		users = sets:new(),
		modes = sets:new(),
		process = Process
	},
	save(State),
	ets:insert(channels, {Name, Process}),
	State.

find(Name) ->
	Matches = ets:lookup(channels, Name),
	case Matches of
		[Process] ->
			Process ! {self(), read},
			receive
				Channel ->
					Channel
			end;
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
			Process ! {self(), delete};
			receive
				ok -> 
					ets:delete(channels, Channel#channel.name),
					ok
			end.
		More ->
			not_empty
	end.

add_user(Channel1, User) ->
	UsersInChannel1 = Channel#channel.users,
	UsersInChannel2 = sets:add_element(User, UsersInChannel1),
	Channel2 = Channel1#channel{users=UsersInChannel2},
	save(Channel2).

run(State) ->
	receive
		{Invoker, read} ->
			Invoker ! State,
			run(State);
		{Invoker, save, NewState} ->
			Invoker ! ok,
			run(NewState);
		{Invoker, delete} ->
			Invoker ! ok;
	end.

get_channel_process(Channel) ->
	Channel#channel.process.