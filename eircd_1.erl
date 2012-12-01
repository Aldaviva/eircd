-module (eircd_1).
-compile ([debug_info]).
-export ([start/0, dump_users/1]).

-define (SERVER_HOSTNAME, "127.0.0.1").
-define (SERVER_VERSION, "eircd0.0.1").
-define (CRLF, [13,10]).

-record (context, {user_manager, channel_manager}).

start() ->
	start(6667).
start(Port) ->
	Context = #context{
		user_manager = spawn(fun() -> start_user_manager() end),
		channel_manager = spawn(fun() -> start_channel_manager() end)
	},
	io:format("Started User Manager with PID ~p~n", [Context#context.user_manager]),
	io:format("Started Channel Manager with PID ~p~n", [Context#context.channel_manager]),
	listen(Port, Context).


listen(Port, Context) ->
	case gen_tcp:listen(Port, [binary, { active, false }, { packet, 0 }]) of
		{ ok, ServerSocket } ->
			io:format("Listening on port ~p~n", [Port]),
			accept(ServerSocket, Context);
		Other ->
			io:format("Can't listen to socket ~p~n", [Other])
	end.

accept(ServerSocket, Context) ->
	{ ok, ClientSocket } = gen_tcp:accept(ServerSocket),
	HandlerPid = spawn(fun() -> handle_request(ClientSocket, Context) end),
	ok = gen_tcp:controlling_process(ClientSocket, HandlerPid),
	accept(ServerSocket, Context).


send(Socket, Message) ->
	gen_tcp:send(Socket, Message++?CRLF).
send_server_message(User, Code, Message) ->
	send(get(User, socket), ":"++?SERVER_HOSTNAME++" "++string:right(integer_to_list(Code), 3, $0)++" "++get(User, nickname)++" "++Message).
send_user_message(User, Command, Message) ->
	send(get(User, socket), ":"++get(User, nickname)++"!"++get(User, username)++"@"++get(User, hostname)++" "++string:to_upper(Command)++" "++Message).


handle_request(Socket, Context) ->
	inet:setopts(Socket, [{active, once}]),
	receive
		{ tcp, Socket, Data } ->
			RequestLines = string:tokens(bitstring_to_list(Data), "\r\n"),
			[ (fun(L) ->
				Command = string:to_upper(string:sub_word(L, 1)),
				io:format("socket: ~p, command to handle: ~p, raw: ~p~n", [Socket, Command, L]),
				User = case user_by_socket(Context#context.user_manager, Socket) of
					none -> Socket;
					U -> U
				end,
				Args = case string:words(L) of
					1 -> "";
					N when N > 1 -> string:substr(L, string:len(Command) + 2)
				end,
				handle_command(Command, Context, User, Args)
			end)(Line) || Line <- RequestLines],
			handle_request(Socket, Context);
		{ tcp_closed, Socket } ->
			io:format("Socket ~p closed~n", [Socket]);
		{ tcp_error, Socket, Reason } ->
			io:format("Error on socket ~p caused by ~p~n", [Socket, Reason])
	end.


handle_command("NICK", Context, Socket, ArgStr) ->
	[Nickname|_] = split(ArgStr),
	user_create(Context#context.user_manager, Nickname, Socket);

handle_command("USER", Context, User, ArgStr) ->
	[Username, Hostname, ServerName, [$:|Realname]] = split(ArgStr, 4),
	% Nickname = get(User, nickname),
	User2 = user_update(Context#context.user_manager, User, [{username, Username}, {hostname, Hostname}, {servername, ServerName}, {realname, Realname}]),
	Nickname = get(User2, nickname),
	io:format("User connected with username = ~p, hostname = ~p, server = ~p, realname = ~p~n", [Username, Hostname, ServerName, Realname]),
	send_server_message(User2, 1, ":Welcome to the Internet Relay Network "++Nickname++"!"++Username++"@"++Hostname),
	send_server_message(User2, 2, ":Your host is "++?SERVER_HOSTNAME++", running version "++?SERVER_VERSION),
	BuildTime = "Wed Nov 28 23:04:16 2012",
	send_server_message(User2, 3, ":This server was created "++BuildTime),
	send_server_message(User2, 4, ?SERVER_HOSTNAME++" "++(?SERVER_VERSION)++" i n"),
	handle_command(Context, User2, "MOTD", "");

handle_command("QUIT", Context, User, _) ->
	io:format("User quitting.~n"),
	Nickname = get(User, nickname),
	Socket = get(User, socket),
	user_delete(Context#context.user_manager, Nickname),
	gen_tcp:close(Socket);

handle_command(_, User, "MOTD", _) ->
	send_server_message(User, 375, ":- "++?SERVER_HOSTNAME++" Message of the Day -"),
	send_server_message(User, 372, ":-  Hello"),
	send_server_message(User, 376, ":End of /MOTD command.");

handle_command("JOIN", Context, User, ArgStr) ->
	[ (fun(ChannelName) ->
		io:format("User wants to join channel ~p~n", [ChannelName]),
		ok = join(Context#context.channel_manager, Context#context.user_manager, ChannelName, User),
		send_user_message(User, "JOIN", [$:|[$#|ChannelName]])
	end)(channel_strip_name(C)) || C <- split(ArgStr)];

handle_command("MODE", Context, User, ArgStr) ->
	case split(ArgStr) of
		[[$#|ChannelName]] ->
			Channel = channel_by_name(Context#context.channel_manager, ChannelName),
			Modes = get(Channel, modes),
			ModeString = [$+|lists:concat(sets:to_list(Modes))],
			send_server_message(User, 324, "#"++ChannelName++" "++ModeString),
			CreationDateSeconds = seconds_since_epoch(get(Channel, creation_date)),
			send_server_message(User, 329, "#"++ChannelName++" "++integer_to_list(CreationDateSeconds));
		[[$#|ChannelName]|ModesToSet] ->
			send_server_message(User, 421, ":Unimplemented: changing channel modes");
		[Nickname] ->
			ArgUser = user_by_nickname(Context#context.user_manager, Nickname),
			Modes = get(ArgUser, modes),
			ModeString = [$+|lists:concat(sets:to_list(Modes))],
			send_server_message(User, 324, ModeString);
		[Nickname|ModesToSet] ->
			send_server_message(User, 421, ":Unimplemented: changing user modes")
	end;

handle_command("WHO", Context, RequestUser, ArgStr) ->
	case split(ArgStr) of 
		[] -> 
			% this variant is bullshit. it lists every user, but users in multiple channels are only listed under one of them
			send_server_message(RequestUser, 421, ":Unimplemented: global /WHO. Try adding a channel");
		[[$#|ChannelName]] ->
			% for each user in the channel, send a server message to the requesting user with some disgusting string concatenation
			Channel = channel_by_name(Context#context.channel_manager, ChannelName),
			Users = sets:to_list(get(Channel, users)),
			[(fun() ->
				Message = "#"++ChannelName++" "++get(User, username)++" "++get(User, hostname)++" "++get(User, servername)++" "++get(User, nickname)++" H :0 "++get(User, realname),
				send_server_message(RequestUser, 352, Message)
			end)() || User <- Users],
			send_server_message(RequestUser, 315, ":End of /WHO list.");
		_ ->
			send_server_message(RequestUser, 421, ":Supported invocation: /WHO #channelname")
	end;

handle_command("TIME", Context, User, _) ->
	send_server_message(User, 391, ?SERVER_HOSTNAME++" "++seconds_since_epoch());

handle_command(UnknownCommand, _, User, _) ->
	send_server_message(User, 421, UnknownCommand++" :Unknown command").


user_create(UserManager, Nickname, Socket) ->
	UserManager ! {self(), create, Nickname, Socket},
	receive ok -> ok end.

user_by_nickname(UserManager, Nickname) ->
	UserManager ! {self(), filter, fun(Key, Value) when Key==nickname -> Value==Nickname; (_, _)->true end},
	receive
		{ok, [User|[]]} ->
			User
	end.

user_by_socket(UserManager, Socket) ->
	UserManager ! {self(), find, Socket},
	receive
		{ok, User} ->
			User;
		error ->
			io:format("No users found with socket ~p~n", [Socket]),
			none
	end.

user_filter(UserManager, Predicate) ->
	UserManager ! {self(), filter, Predicate},
	receive
		{ok, Users} ->
			Users
	end.

user_update(UserManager, User, Changes) ->
	UserManager ! {self(), update, get(User, socket), Changes},
	receive
		{ok, NewUser} ->
			NewUser;
		Other -> 
			io:format("Failed to update user ~p~n", [Other]),
			Other
	end.

user_delete(UserManager, Nickname) ->
	UserManager ! {self(), delete, Nickname},
	receive ok -> ok end.

dump_users(UserManager) ->
	UserManager ! dump.

start_user_manager() ->
	user_manager(dict:new()).
user_manager(Users) ->
	receive
		{Invoker, create, Nickname, Socket} ->
			false = dict:is_key(Socket, Users),
			User = dict:from_list([
				{nickname, Nickname},
				{socket, Socket},
				{modes, sets:new()}]),
			NewUsers = dict:store(Socket, User, Users),
			Invoker ! ok,
			user_manager(NewUsers);

		{Invoker, find, Socket} ->
			Invoker ! dict:find(Socket, Users),
			user_manager(Users);

		{Invoker, filter, Predicate} ->
			FilteredUsers = dict:filter(Predicate, Users),
			Invoker ! {ok, dict:fold(fun(_, V, AccIn) -> [V|AccIn] end, [], FilteredUsers)},
			user_manager(Users);

		{Invoker, update, Socket, Changes} ->
			OldUser = dict:fetch(Socket, Users),
			NewUser = dict:merge(fun(_, _, NewValue) ->
				NewValue
			end, OldUser, dict:from_list(Changes)),
			UsersWithUpdates = dict:store(Socket, NewUser, Users),
			Invoker ! {ok, NewUser},
			user_manager(UsersWithUpdates);

		{Invoker, delete, Socket} ->
			NewUsers = dict:erase(Socket, Users),
			Invoker ! ok,
			user_manager(NewUsers);

		{Invoker, add_to_channel, _, _} ->
			%noop for now, unless we need to change the User when it's added to a channel
			Invoker ! ok,
			user_manager(Users);

		dump ->
			io:format("Users: ~p~n", [dict:to_list(Users)]),
			user_manager(Users)
	end.


channel_by_name(ChannelManager, Name) ->
	ChannelManager ! {self(), find, Name},
	receive
		{ok, Channel} ->
			Channel;
		error ->
			none
	end.

channel_create(ChannelManager, Name) ->
	ChannelManager ! {self(), create, Name},
	receive {_, Channel} -> Channel end.

channel_delete_if_empty(ChannelManager, Name) ->
	ChannelManager ! {self(), delete_if_empty, Name},
	receive Response -> Response end.

join(ChannelManager, UserManager, ChannelName, User) ->
	Channel = channel_create(ChannelManager, ChannelName),
	ChannelManager ! {self(), add_user, Channel, User},
	ok = receive {added, ChannelWithUser} -> ok end,
	UserManager ! {self(), add_to_channel, User, ChannelWithUser},
	ok = receive Response -> Response end,
	ok.

start_channel_manager() ->
	channel_manager(dict:new()).
channel_manager(Channels) ->
	receive
		{Invoker, create, Name} ->
			{Response, NewChannel, NewChannels} = case dict:find(Name, Channels) of
				{ok, Channel} ->
					{already_created, Channel, Channels};
				error ->
					NewChannel_ = dict:from_list([
						{name, Name},
						{creation_date, calendar:universal_time()},
						{topic, ""},
						{topic_date, none},
						{users, sets:new()},
						{modes, sets:new()}
					]),
					NewChannels_ = set(Channels, Name, NewChannel_),
					{created, NewChannel_, NewChannels_}
			end,
			Invoker ! {Response, NewChannel},
			channel_manager(NewChannels);

		{Invoker, find, Name} ->
			Invoker ! dict:find(channel_strip_name(Name), Channels),
			channel_manager(Channels);

		{Invoker, delete_if_empty, Name} ->
			{Response, NewChannels} = case dict:find(Name, Channels) of
				{ok, Channel} ->
					Users = get(Channel, users),
					case sets:size(Users) of
						0 -> {deleted, dict:erase(Name, Channels)};
						_ -> {not_empty, Channels}
					end;
				error -> {already_deleted, Channels}
			end,
			Invoker ! Response,
			channel_manager(NewChannels);

		{Invoker, add_user, Channel, User} ->
			io:format("Channel Manager adding user ~p to channel ~p~n", [get(User, nickname), get(Channel, name)]),
			% Channel = dict:fetch(Name, Channels),
			UsersInChannel = get(Channel, users),
			NewUsersInChannel = sets:add_element(User, UsersInChannel),
			NewChannel = set(Channel, users, NewUsersInChannel),
			NewChannels = set(Channels, get(Channel, name), NewChannel),
			Invoker ! {added, NewChannel},
			channel_manager(NewChannels);

		dump ->
			io:format("Channels: ~p~n", [dict:to_list(Channels)]),
			channel_manager(Channels)
	end.

get(Object, Field) ->
	dict:fetch(Field, Object).
set(Object, Field, Value) ->
	dict:store(Field, Value, Object).

split(String, Limit) ->
	re:split(String, "\\s+", [{return, list}, {parts, Limit}]).
split(String) ->
	split(String, infinity).

channel_strip_name(Name) ->
	case Name of
		[$#|Tail] -> Tail;
		Other -> Other
	end.

seconds_since_epoch() ->
	seconds_since_epoch(calendar:universal_time()).
seconds_since_epoch(Datetime) ->
	calendar:datetime_to_gregorian_seconds(Datetime) - calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}).
