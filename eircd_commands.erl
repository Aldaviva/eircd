-module(eircd_commands).
-export([handle_command/3]).
-include("common.hrl").

handle_command("NICK", Socket, ArgStr) ->
	[Nickname|_] = eircd_helpers:split(ArgStr),
	User = eircd_users:create(Socket, Nickname),
	eircd_users:save(User);

handle_command("USER", User, ArgStr) ->
	[Username, Hostname, Servername, [$:|Realname]] = eircd_helpers:split(ArgStr, 4),
	Nickname = User#user.nickname,
	User2 = User#user{
		username = Username,
		hostname = Hostname,
		servername = Servername,
		realname = Realname},
	eircd_users:save(User2),
	eircd_connector:send_server_message(User2, ?IRC_WELCOME_NAME, ":Welcome to the Internet Relay Network "++Nickname++"!"++Username++"@"++Hostname),
	eircd_connector:send_server_message(User2, ?IRC_WELCOME_SERVER, ":Your host is "++?SERVER_HOSTNAME++", running version "++?SERVER_VERSION),
	eircd_connector:send_server_message(User2, ?IRC_WELCOME_SERVER_CREATION_DATE, ":This server was created "++?SERVER_BUILD_TIME),
	eircd_connector:send_server_message(User2, ?IRC_WELCOME_CAPABILITIES, ?SERVER_HOSTNAME++" "++(?SERVER_VERSION)++" i n"),
	handle_command("MOTD", User, "");

handle_command("QUIT", User, _) ->
	Socket = User#user.socket,
	eircd_users:delete(User),
	gen_tcp:close(Socket);

handle_command("MOTD", User, _) ->
	eircd_connector:send_server_message(User, ?IRC_MOTD_START, ":- "++?SERVER_HOSTNAME++" Message of the Day -"),
	eircd_connector:send_server_message(User, ?IRC_MOTD_BODY, ":-  Hello"),
	eircd_connector:send_server_message(User, ?IRC_MOTD_END, ":End of /MOTD command.");

handle_command("LIST", User, QueriesArg) ->
	QueryStrs = eircd_helpers:split(QueriesArg, ","),
	Predicates = lists:map(fun(QueryStr) ->
		case QueryStr of
			["<"|MaxStr] ->
				fun(Channel) ->
					Max = list_to_integer(MaxStr),
					Users = Channel#channel.users,
					sets:size(Users) < Max
				end;
			[">"|MinStr] ->
				fun(Channel) ->
					Min = list_to_integer(MinStr),
					Users = Channel#channel.users,
					sets:size(Users) > Min
				end;
			["!"|NotNamePattern] ->
				fun(Channel) ->
					ChannelName = Channel#channel.name,
					Pattern = "^"++re:replace(NotNamePattern, "\\*", ".*", [global, {return, list}])++"$",
					case re:run(ChannelName, Pattern) of
						{match, _} ->
							false;
						nomatch ->
							true
					end
				end;
			NamePattern ->
				fun(Channel) ->
					ChannelName = Channel#channel.name,
					Pattern = "^"++re:replace(NamePattern, "\\*", ".*", [global, {return, list}])++"$",
					case re:run(ChannelName, Pattern) of
						{match, _} ->
							true;
						nomatch ->
							false
					end
				end
		end
	end, QueryStrs),
	MatchedChannels = eircd_channels:filter(fun(Channel) ->
		lists:all(fun(Predicate) ->
			Predicate(Channel)
		end, Predicates)
	end),
	eircd_connector:send_server_message(User, ?IRC_CHANNEL_LIST_START, "Channel :Users  Name"),
	lists:foreach(fun(MatchedChannel) ->
		Topic = case MatchedChannel#channel.topic of
			undefined -> " ";
			Defined -> Defined
		end,
		Message = "#"++MatchedChannel#channel.name++" "++integer_to_list(sets:size(MatchedChannel#channel.users))++" :"++Topic,
		eircd_connector:send_server_message(User, ?IRC_CHANNEL_LIST_BODY, Message)
	end, MatchedChannels),
	eircd_connector:send_server_message(User, ?IRC_CHANNEL_LIST_END, ":End of /LIST");

handle_command("JOIN", User, ArgStr) ->
	ChannelNames = lists:map(fun eircd_helpers:channel_strip_name/1, eircd_helpers:split(ArgStr)),
	lists:foreach(fun(ChannelName) ->
		Channel = eircd_channels:create(ChannelName),
		eircd_channels:add_user(Channel, User),
		eircd_connector:send_user_message(User, "JOIN", ":#"++ChannelName), %TODO this should actually go to every user in the channel, not just the one that joined
		handle_command("NAMES", User, [$#|ChannelName])
	end, ChannelNames);

handle_command("MODE", User, ArgStr) ->
	case eircd_helpers:split(ArgStr) of
		[[$#|ChannelName]] ->
			Channel = eircd_channels:find(ChannelName),
			Modes = Channel#channel.modes,
			ModeString = [$+|lists:concat(sets:to_list(Modes))],
			eircd_connector:send_server_message(User, ?IRC_CHANNEL_MODES, "#"++ChannelName++" "++ModeString),
			CreationDateSeconds = eircd_helpers:seconds_since_epoch(Channel#channel.creation_date),
			eircd_connector:send_server_message(User, ?IRC_CHANNEL_CREATION_DATE, "#"++ChannelName++" "++integer_to_list(CreationDateSeconds));
		[Nickname] ->
			ArgUser = eircd_users:find_by_nickname(Nickname),
			Modes = ArgUser#user.modes,
			ModeString = [$+|lists:concat(sets:to_list(Modes))],
			eircd_connector:send_server_message(User, ?IRC_USER_MODES, ModeString);
		_ ->
			eircd_connector:send_server_message(User, ?IRC_ERROR, ":Unimplemented (changing modes")
	end;

handle_command("WHO", User, ArgStr) ->
	case eircd_helpers:split(ArgStr) of
		[] ->
			eircd_connector:send_server_message(User, 421, ":Unimplemented (global who)");
		[[$#|ChannelName]] ->
			Channel = eircd_channels:find(ChannelName),
			UserSockets = sets:to_list(Channel#channel.users),
			lists:foreach(fun(UserSocket) ->
				ChannelUser = eircd_users:find_by_socket(UserSocket),
				Message = "#"++ChannelName++" "++ChannelUser#user.username++" "++ChannelUser#user.hostname++" "++ChannelUser#user.servername++" "++ChannelUser#user.nickname++" H :0 "++ChannelUser#user.realname,
				eircd_connector:send_server_message(User, ?IRC_WHO_BODY, Message)
			end, UserSockets),
			eircd_connector:send_server_message(User, ?IRC_WHO_END, "#"++ChannelName++" :End of /WHO list.");
		_ ->
			eircd_connector:send_server_message(User, 421, ":Unimplemented (filtered who or user who)")
	end;

handle_command("NAMES", User, ArgStr) ->
	case eircd_helpers:split(ArgStr) of
		[[$#|ChannelName]] ->
			Channel = eircd_channels:find(ChannelName),
			UserSockets = sets:to_list(Channel#channel.users),
			UserNames = lists:map(fun(UserSocket) ->
				ChannelUser = eircd_users:find_by_socket(UserSocket),
				[$\s|ChannelUser#user.nickname]
			end, UserSockets),
			[$\s|UserNamesStr] = lists:concat(UserNames),
			eircd_connector:send_server_message(User, ?IRC_NAMES_BODY, "= #"++ChannelName++" :"++UserNamesStr),
			eircd_connector:send_server_message(User, ?IRC_NAMES_END, "#"++ChannelName++" :End of /NAMES list.")
	end;

handle_command("PRIVMSG", User, ArgStr) ->
	case eircd_helpers:split(ArgStr, 2) of
		[[$#|ChannelName], [$:|Body]] ->
			Channel = eircd_channels:find(ChannelName),
			UserSockets = sets:to_list(sets:del_element(User#user.socket, Channel#channel.users)),
			lists:foreach(fun(UserSocket) ->
				Recipient = eircd_users:find_by_socket(UserSocket),
				eircd_connector:send_user_message(Recipient, "PRIVMSG", "#"++ChannelName++" :"++Body)
			end, UserSockets);
		[Nickname,Body] ->
			Recipient = eircd_users:find_by_nickname(Nickname),
			eircd_connector:send_user_message(Recipient, "PRIVMSG", User#user.nickname++" :"++Body)
	end;

handle_command(UnknownCommand, User, _) ->
	eircd_connector:send_server_message(User, ?IRC_ERROR, UnknownCommand++" :Unknown command").