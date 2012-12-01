-module(commands).
-export([handle_command/3]).
-include("constants.hrl").

handle_command("NICK", Socket, ArgStr) ->
	[Nickname|_] = split(ArgStr),
	User = user:create(Socket, Nickname),
	user:save(User);

handle_command("USER", User, ArgStr) ->
	[Username, Hostname, Servername, [$:|Realname]] = split(ArgStr, 4),
	Nickname = User#user.nickname,
	User2 = User#user{
		username = Username,
		hostname = Hostname,
		servername = Servername,
		realname = Realname},
	user:save(User2),
	connector:send_server_message(User2, ?IRC_WELCOME_NAME, ":Welcome to the Internet Relay Network "++Nickname++"!"++Username++"@"++Hostname),
	connector:send_server_message(User2, ?IRC_WELCOME_SERVER, ":Your host is "++?SERVER_HOSTNAME++", running version "++?SERVER_VERSION),
	connector:send_server_message(User2, ?IRC_WELCOME_SERVER_CREATION_DATE, ":This server was created "++?SERVER_BUILD_TIME),
	connector:send_server_message(User2, ?IRC_WELCOME_CAPABILITIES, ?SERVER_HOSTNAME++" "++(?SERVER_VERSION)++" i n"),
	handle_command("MOTD", User, "");

handle_command("QUIT", User, _) ->
	Socket = User#user.socket,
	user:delete(User),
	gen_tcp:close(Socket);

handle_command("MOTD", User, _) ->
	connector:send_server_message(User, ?IRC_MOTD_START, ":- "++?SERVER_HOSTNAME++" Message of the Day -"),
	connector:send_server_message(User, ?IRC_MOTD_BODY, ":-  Hello"),
	connector:send_server_message(User, ?IRC_MOTD_END, ":End of /MOTD command.");

%add support for more commands (MODE, WHO, JOIN)